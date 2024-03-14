package dotty.tools.dotc
package typer

import dotty.tools.dotc.ast.*
import dotty.tools.dotc.config.Feature.*
import dotty.tools.dotc.config.SourceVersion.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Annotations.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.PatMatGivenVarName
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.inlines.PrepareInlineable
import dotty.tools.dotc.quoted.QuotePatterns
import dotty.tools.dotc.staging.StagingLevel.*

import dotty.tools.dotc.typer.ErrorReporting.errorTree
import dotty.tools.dotc.typer.Implicits.*
import dotty.tools.dotc.typer.Inferencing.*
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.util.Spans.*
import dotty.tools.dotc.util.Stats.record
import dotty.tools.dotc.reporting.IllegalVariableInPatternAlternative
import scala.collection.mutable
import scala.collection.SeqMap

/** Type quotes `'{ ... }` and splices `${ ... }` */
trait QuotesAndSplices {
  self: Typer =>

  import tpd.*
  import QuotesAndSplices.*

  /** Translate `'{ e }` into `scala.quoted.Expr.apply(e)` and `'[T]` into `scala.quoted.Type.apply[T]`
   *  while tracking the quotation level in the context.
   */
  def typedQuote(tree: untpd.Quote, pt: Type)(using Context): Tree = {
    record("typedQuote")
    tree.body match {
      case _: untpd.Splice if tree.isTerm && !ctx.mode.is(Mode.Pattern) =>
        report.warning("Canceled splice directly inside a quote. '{ ${ XYZ } } is equivalent to XYZ.", tree.srcPos)
      case _ =>
    }
    val quotes = inferImplicitArg(defn.QuotesClass.typeRef, tree.span)

    if quotes.tpe.isInstanceOf[SearchFailureType] then
      report.error(missingArgMsg(quotes, defn.QuotesClass.typeRef, ""), ctx.source.atSpan(tree.span))
    else if !quotes.tpe.isStable then
      report.error(em"Quotes require stable Quotes, but found non stable $quotes", quotes.srcPos)

    if ctx.mode.is(Mode.Pattern) then
      PrepareInlineable.makeInlineable(typedQuotePattern(tree, pt, quotes).withSpan(tree.span))
    else if tree.isTypeQuote then
      val msg = em"""Quoted types `'[..]` can only be used in patterns.
                    |
                    |Hint: To get a scala.quoted.Type[T] use scala.quoted.Type.of[T] instead.
                    |"""
      report.error(msg, tree.srcPos)
      EmptyTree
    else
      // TODO typecheck directly (without `exprQuote`)
      val exprQuoteTree = untpd.Apply(untpd.ref(defn.QuotedRuntime_exprQuote.termRef), tree.body)
      val quotedExpr = typedApply(exprQuoteTree, pt)(using quoteContext) match
        case Apply(TypeApply(fn, tpt :: Nil), quotedExpr :: Nil) => untpd.Quote(quotedExpr, Nil).withBodyType(tpt.tpe)
      makeInlineable(quotedExpr.select(nme.apply).appliedTo(quotes).withSpan(tree.span))
  }

  private def makeInlineable(tree: Tree)(using Context): Tree =
    inContext(ctx.withOwner(ctx.owner.skipLocalOwners)) {
      PrepareInlineable.makeInlineable(tree)
    }

  /** Translate `${ t: Expr[T] }` into expression `t.splice` while tracking the quotation level in the context */
  def typedSplice(tree: untpd.Splice, pt: Type)(using Context): Tree = {
    record("typedSplice")
    checkSpliceOutsideQuote(tree)
    assert(!ctx.mode.isQuotedPattern)
    tree.expr match {
      case untpd.Quote(innerExpr, Nil) if innerExpr.isTerm =>
        report.warning("Canceled quote directly inside a splice. ${ '{ XYZ } } is equivalent to XYZ.", tree.srcPos)
        return typed(innerExpr, pt)
      case _ =>
    }
    if (level == 0) {
      // Mark the first inline method from the context as a macro
      def markAsMacro(c: Context): Unit =
        if (c.owner eq c.outer.owner) markAsMacro(c.outer)
        else if (c.owner.isInlineMethod) c.owner.setFlag(Macro)
        else if (!c.outer.owner.is(Package)) markAsMacro(c.outer)
        else assert(ctx.reporter.hasErrors) // Did not find inline def to mark as macro
      markAsMacro(ctx)
    }

    // TODO typecheck directly (without `exprSplice`)
    val internalSplice =
      untpd.Apply(untpd.ref(defn.QuotedRuntime_exprSplice.termRef), tree.expr)
    typedApply(internalSplice, pt)(using spliceContext).withSpan(tree.span) match
      case tree @ Apply(TypeApply(_, tpt :: Nil), spliced :: Nil) if tree.symbol == defn.QuotedRuntime_exprSplice =>
        cpy.Splice(tree)(spliced)
      case tree => tree
  }

  def typedQuotePattern(tree: untpd.QuotePattern, pt: Type)(using Context): Tree =
    throw new UnsupportedOperationException("cannot type check a Hole node")

  def typedSplicePattern(tree: untpd.SplicePattern, pt: Type)(using Context): Tree = {
    record("typedSplicePattern")
    if isFullyDefined(pt, ForceDegree.flipBottom) then
      val typedArgs = withMode(Mode.InQuotePatternHoasArgs) {
        tree.args.map {
          case arg: untpd.Ident =>
            typedExpr(arg)
          case arg =>
            report.error("Open pattern expected an identifier", arg.srcPos)
            EmptyTree
        }
      }
      for arg <- typedArgs if arg.symbol.is(Mutable) do // TODO support these patterns. Possibly using scala.quoted.util.Var
        report.error("References to `var`s cannot be used in higher-order pattern", arg.srcPos)
      val argTypes = typedArgs.map(_.tpe.widenTermRefExpr)
      val patType = if tree.args.isEmpty then pt else defn.FunctionNOf(argTypes, pt)
      val pat = typedPattern(tree.body, defn.QuotedExprClass.typeRef.appliedTo(patType))(using quotePatternSpliceContext)
      val baseType = pat.tpe.baseType(defn.QuotedExprClass)
      val argType = if baseType.exists then baseType.argTypesHi.head else defn.NothingType
      untpd.cpy.SplicePattern(tree)(pat, typedArgs).withType(pt)
    else
      errorTree(tree, em"Type must be fully defined.\nConsider annotating the splice using a type ascription:\n  ($tree: XYZ).", tree.body.srcPos)
  }

  def typedHole(tree: untpd.Hole, pt: Type)(using Context): Tree =
    throw new UnsupportedOperationException("cannot type check a Hole node")

  /** Types a splice applied to some arguments `$f(arg1, ..., argn)` in a quote pattern.
   *
   *  The tree is desugared into `$f.apply(arg1, ..., argn)` where the expression `$f`
   *  is expected to type as a function type `(T1, ..., Tn) => R`.
   *  `Ti` is the type of the argument `argi` and R if the type of the prototype.
   *  The prototype must be fully defined to be able to infer the type of `R`.
   */
  def typedAppliedSplice(tree: untpd.Apply, pt: Type)(using Context): Tree = {
    assert(ctx.mode.isQuotedPattern)
    val untpd.Apply(splice: untpd.SplicePattern, args) = tree: @unchecked
    def isInBraces: Boolean = splice.span.end != splice.body.span.end
    if isInBraces then // ${x}(...) match an application
      val typedArgs = args.map(arg => typedExpr(arg))
      val argTypes = typedArgs.map(_.tpe.widenTermRefExpr)
      val splice1 = typedSplicePattern(splice, defn.FunctionNOf(argTypes, pt))
      untpd.cpy.Apply(tree)(splice1.select(nme.apply), typedArgs).withType(pt)
    else // $x(...) higher-order quasipattern
      if args.isEmpty then
         report.error("Missing arguments for open pattern", tree.srcPos)
      typedSplicePattern(untpd.cpy.SplicePattern(tree)(splice.body, args), pt)
  }

  /** Type check a type binding reference in a quoted pattern.
   *
   *  If no binding exists with that name, this becomes the definition of a new type binding.
   */
  def typedQuotedTypeVar(tree: untpd.Ident, pt: Type)(using Context): Tree =
    val typeSymInfo = pt match
      case pt: TypeBounds => pt
      case _ => TypeBounds.empty

    def warnOnInferredBounds(typeSym: Symbol) =
      if !(typeSymInfo =:= TypeBounds.empty) && !(typeSym.info <:< typeSymInfo) then
        val (openQuote, closeQuote) = if ctx.mode.is(Mode.QuotedExprPattern) then ("'{", "}") else ("'[", "]")
        report.warning(em"Ignored bound$typeSymInfo\n\nConsider defining bounds explicitly:\n  $openQuote $typeSym${typeSym.info & typeSymInfo}; ... $closeQuote", tree.srcPos)

    getQuotedPatternTypeVariable(tree.name.asTypeName) match
      case Some(typeSym) =>
        warnOnInferredBounds(typeSym)
        ref(typeSym)
      case None =>
        if ctx.mode.is(Mode.InPatternAlternative) then
          report.error(IllegalVariableInPatternAlternative(tree.name), tree.srcPos)
        val typeSym = inContext(quotePatternOuterContext(ctx)) {
          newSymbol(ctx.owner, tree.name.toTypeName, Case, typeSymInfo, NoSymbol, tree.span)
        }
        addQuotedPatternTypeVariable(typeSym)
        Bind(typeSym, untpd.Ident(nme.WILDCARD).withType(typeSymInfo)).withSpan(tree.span)

  private def checkSpliceOutsideQuote(tree: untpd.Tree)(using Context): Unit =
    if (level == 0 && !ctx.owner.ownersIterator.exists(_.isInlineMethod))
      report.error("Splice ${...} outside quotes '{...} or inline method", tree.srcPos)
    else if (level < 0)
      report.error(
        em"""Splice $${...} at level $level.
            |
            |Inline method may contain a splice at level 0 but the contents of this splice cannot have a splice.
            |""", tree.srcPos
      )

  /** Type a quote pattern `case '{ <pattern> } =>` given the a current prototype. Typing the pattern
   *  will create a QuotePattern tree.
   *
   *  Code directly inside the quote is typed as an expression using Mode.QuotedPattern. Splices
   *  within the quotes become patterns again and typed accordingly.
   */
  private def typedQuotePattern(tree: untpd.Quote, pt: Type, quotes: Tree)(using Context): Tree = {
    val quoted = tree.body
    if quoted.isTerm && !pt.derivesFrom(defn.QuotedExprClass) then
      report.error("Quote pattern can only match scrutinees of type scala.quoted.Expr", tree.srcPos)
    else if quoted.isType && !pt.derivesFrom(defn.QuotedTypeClass) then
      report.error("Quote pattern can only match scrutinees of type scala.quoted.Type", tree.srcPos)

    val exprPt = pt.baseType(if quoted.isType then defn.QuotedTypeClass else defn.QuotedExprClass)
    val quotedPt = exprPt.argInfos.headOption match {
      case Some(argPt: ValueType) => argPt // excludes TypeBounds
      case _ => defn.AnyType
    }
    val (untpdTypeVariables, quoted0) = desugar.quotedPatternTypeVariables(desugar.quotedPattern(quoted, untpd.TypedSplice(TypeTree(quotedPt))))

    for tdef @ untpd.TypeDef(_, rhs) <- untpdTypeVariables do rhs match
      case _: TypeBoundsTree => // ok
      case LambdaTypeTree(_, body: TypeBoundsTree) => // ok
      case _ => report.error("Quote type variable definition cannot be an alias", tdef.srcPos)

    if ctx.mode.is(Mode.InPatternAlternative) then
      for tpVar <- untpdTypeVariables do
        report.error(IllegalVariableInPatternAlternative(tpVar.name), tpVar.srcPos)

    val (typeTypeVariables, patternBlockCtx) =
      val quoteCtx = quotePatternContext(quoted.isType)
      if untpdTypeVariables.isEmpty then (Nil, quoteCtx)
      else typedBlockStats(untpdTypeVariables)(using quoteCtx)
    val patternCtx = patternBlockCtx.addMode(if quoted.isType then Mode.QuotedTypePattern else Mode.QuotedExprPattern)

    val allTypeBindings = List.newBuilder[Bind]
    for tpVar <- typeTypeVariables do
      val sym = tpVar.symbol
      allTypeBindings += Bind(sym, untpd.Ident(nme.WILDCARD).withType(sym.info)).withSpan(tpVar.span)

    val body1 = inContext(patternCtx) {
      for typeVariable <- typeTypeVariables do
        addQuotedPatternTypeVariable(typeVariable.symbol)

      if quoted.isType then typedType(quoted0, WildcardType)
      else typedExpr(quoted0, WildcardType)
    }

    val extractTypeBindings = new TreeMapWithVariance {
      override def transform(tree: Tree)(using Context) = tree match
        case pat: Bind if pat.isType =>
          if inContravariantPosition then
            pat.symbol.addAnnotation(Annotation(New(ref(defn.QuotedRuntimePatterns_fromAboveAnnot.typeRef)).withSpan(pat.span)))
          allTypeBindings += pat
          TypeTree(pat.symbol.typeRef).withSpan(pat.span)
        case _: SplicePattern =>
          tree
        case _ =>
          super.transform(tree)
    }
    val body2 = extractTypeBindings.transform(body1)

    val quoteClass = if quoted.isTerm then defn.QuotedExprClass else defn.QuotedTypeClass
    val pt1 = quoteClass.typeRef.appliedTo(body2.tpe)

    val quotePattern = QuotePattern(allTypeBindings.result(), body2, quotes, pt1)
    QuotePatterns.checkPattern(quotePattern)
    quotePattern
  }
}

object QuotesAndSplices {
  import tpd.*

  /** Key for mapping from quoted pattern type variable names into their symbol */
  private val TypeVariableKey = new Property.Key[collection.mutable.Map[TypeName, Symbol]]

  /** Get the symbol for the quoted pattern type variable if it exists */
  def getQuotedPatternTypeVariable(name: TypeName)(using Context): Option[Symbol] =
    ctx.property(TypeVariableKey).get.get(name)

  /** Get the symbol for the quoted pattern type variable if it exists */
  def addQuotedPatternTypeVariable(sym: Symbol)(using Context): Unit =
    ctx.property(TypeVariableKey).get.update(sym.name.asTypeName, sym)

  /** Context used to type the contents of a quote pattern */
  def quotePatternContext(isTypePattern: Boolean)(using Context): Context =
    quoteContext.fresh.setNewScope
      .retractMode(Mode.Pattern)
      .setProperty(TypeVariableKey, collection.mutable.Map.empty)

  /** Context used to type the contents of a quote pattern splice */
  def quotePatternSpliceContext(using Context): Context =
    spliceContext
      .retractMode(Mode.QuotedPatternBits)
      .addMode(Mode.Pattern)
      .withOwner(quotePatternOuterContext(ctx).owner)

  /** First outer context that is outside of a quoted pattern. */
  def quotePatternOuterContext(ctx: Context): Context =
    if ctx.mode.isQuotedPattern then quotePatternOuterContext(ctx.outer) else ctx

  private[QuotesAndSplices] class TreeMapWithVariance extends TreeMap:
      private var variance: Int = 1

      def inContravariantPosition: Boolean = variance == -1

      inline private def atVariance[T](v: Int)(op: => T): T = {
        val saved = variance
        variance = v
        val res = op
        variance = saved
        res
      }

      override def transform(tree: Tree)(using Context) = tree match
        // TODO: handle TypeBoundsTree, LambdaTypeTree as well as method parameters in DefTrees?
        case tree @ AppliedTypeTree(tpt, args) =>
          val args1: List[Tree] = args.zipWithConserve(tpt.tpe.typeParams.map(_.paramVarianceSign)) { (arg, v) =>
            arg.tpe match {
              case _: TypeBounds => transform(arg)
              case _ => atVariance(v * variance)(transform(arg))
            }
          }
          cpy.AppliedTypeTree(tree)(transform(tpt), args1)
        case _ =>
          super.transform(tree)
    end TreeMapWithVariance
}
