package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Annotations.BodyAnnotation
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.util.SrcPos
import dotty.tools.dotc.util.Spans._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.typer.Checking
import dotty.tools.dotc.typer.Implicits.SearchFailureType
import dotty.tools.dotc.core.Annotations._

import dotty.tools.dotc.util.Property

import scala.annotation.constructorOnly

/** Checks that the Phase Consistency Principle (PCP) holds and heals types.
 *
 *  Local term references are phase consistent if and only if they are used at the same level as their definition.
 *
 *  Local type references can be used at the level of their definition or lower. If used used at a higher level,
 *  it will be healed if possible, otherwise it is inconsistent.
 *
 *  Type healing consists in transforming a phase inconsistent type `T` into `summon[Type[T]].Underlying`.
 *
 *  As references to types do not necessarily have an associated tree it is not always possible to replace the types directly.
 *  Instead we always generate a type alias for it and place it at the start of the surrounding quote. This also avoids duplication.
 *  For example:
 *    '{
 *      val x: List[T] = List[T]()
 *      ()
 *    }
 *
 *  is transformed to
 *
 *    '{
 *      type t$1 = summon[Type[T]].Underlying
 *      val x: List[t$1] = List[t$1]();
 *      ()
 *     }
 *
 */
class PCPCheckAndHeal(@constructorOnly ictx: Context) extends TreeMapWithStages(ictx) with Checking {
  import tpd._

  private val InAnnotation = Property.Key[Unit]()

  override def transform(tree: Tree)(using Context): Tree =
    if (tree.source != ctx.source && tree.source.exists)
      transform(tree)(using ctx.withSource(tree.source))
    else if !isInQuoteOrSplice then
      checkAnnotations(tree)
      super.transform(tree)
    else tree match {

      case _: TypeTree | _: RefTree if tree.isType  =>
        val healedType = healType(tree.srcPos)(tree.tpe)
        if healedType == tree.tpe then tree
        else TypeTree(healedType).withSpan(tree.span)
      case _: AppliedTypeTree =>
        super.transform(tree) match
          case tree1: AppliedTypeTree if tree1 ne tree =>
            // propagate healed types
            tree1.withType(tree1.tpt.tpe.appliedTo(tree1.args.map(_.tpe)))
          case tree1 => tree1

      case _: Ident | _: This =>
        tree.withType(healTypeOfTerm(tree.srcPos)(tree.tpe))

      // Remove inline defs in quoted code. Already fully inlined.
      case tree: DefDef if tree.symbol.is(Inline) && level > 0 =>
        EmptyTree

      case tree: ValOrDefDef =>
        checkAnnotations(tree)
        healInfo(tree, tree.tpt.srcPos)
        super.transform(tree)
      case tree: Bind =>
        checkAnnotations(tree)
        healInfo(tree, tree.srcPos)
        super.transform(tree)
      case tree: UnApply =>
        super.transform(tree).withType(healTypeOfTerm(tree.srcPos)(tree.tpe))
      case tree: TypeDef if tree.symbol.is(Case) && level > 0 =>
        report.error(reporting.CaseClassInInlinedCode(tree), tree)
        super.transform(tree)
      case _ =>
        super.transform(tree)
    }

  /** Transform quoted trees while maintaining phase correctness */
  override protected def transformQuotation(body: Tree, quote: Apply)(using Context): Tree = {
    val taggedTypes = new PCPCheckAndHeal.QuoteTypeTags(quote.span)

    if (ctx.property(InAnnotation).isDefined)
      report.error("Cannot have a quote in an annotation", quote.srcPos)

    val contextWithQuote =
      if level == 0 then contextWithQuoteTypeTags(taggedTypes)(using quoteContext)
      else quoteContext
    val body1 = transform(body)(using contextWithQuote)
    val body2 =
      taggedTypes.getTypeTags match
        case Nil  => body1
        case tags => tpd.Block(tags, body1).withSpan(body.span)

    if body.isTerm then
      // `quoted.runtime.Expr.quote[T](<body>)`  --> `quoted.runtime.Expr.quote[T2](<body2>)`
      val TypeApply(fun, targs) = quote.fun: @unchecked
      val targs2 = targs.map(targ => TypeTree(healTypeOfTerm(quote.fun.srcPos)(targ.tpe)))
      cpy.Apply(quote)(cpy.TypeApply(quote.fun)(fun, targs2), body2 :: Nil)
    else
      val quotes = quote.args.mapConserve(transform)
      body.tpe match
        case tp @ TypeRef(x: TermRef, _) if tp.symbol == defn.QuotedType_splice =>
          // Optimization: `quoted.Type.of[x.Underlying](quotes)`  -->  `x`
          ref(x)
        case _ =>
          // `quoted.Type.of[<body>](quotes)`  --> `quoted.Type.of[<body2>](quotes)`
          val TypeApply(fun, _) = quote.fun: @unchecked
          cpy.Apply(quote)(cpy.TypeApply(quote.fun)(fun, body2 :: Nil), quotes)
  }

  /** Transform splice
   *  - If inside a quote, transform the contents of the splice.
   *  - If inside inlined code, expand the macro code.
   *  - If inside of a macro definition, check the validity of the macro.
   */
  protected def transformSplice(body: Tree, splice: Apply)(using Context): Tree = {
    val body1 = transform(body)(using spliceContext)
    splice.fun match {
      case fun @ TypeApply(_, _ :: Nil) =>
        // Type of the splice itself must also be healed
        // `quoted.runtime.Expr.quote[F[T]](... T ...)`  -->  `internal.Quoted.expr[F[$t]](... T ...)`
        val tp = healType(splice.srcPos)(splice.tpe.widenTermRefExpr)
        cpy.Apply(splice)(cpy.TypeApply(fun)(fun.fun, tpd.TypeTree(tp) :: Nil), body1 :: Nil)
      case f @ Apply(fun @ TypeApply(_, _), qctx :: Nil) =>
        // Type of the splice itself must also be healed
        // `quoted.runtime.Expr.quote[F[T]](... T ...)`  -->  `internal.Quoted.expr[F[$t]](... T ...)`
        val tp = healType(splice.srcPos)(splice.tpe.widenTermRefExpr)
        cpy.Apply(splice)(cpy.Apply(f)(cpy.TypeApply(fun)(fun.fun, tpd.TypeTree(tp) :: Nil), qctx :: Nil), body1 :: Nil)
    }
  }

  protected def transformSpliceType(body: Tree, splice: Select)(using Context): Tree = {
    val body1 = transform(body)(using spliceContext)
    if ctx.reporter.hasErrors then
      splice
    else
      val tagRef = getQuoteTypeTags.getTagRef(splice.qualifier.tpe.asInstanceOf[TermRef])
      ref(tagRef).withSpan(splice.span)
  }

  /** Check that annotations do not contain quotes and and that splices are valid */
  private def checkAnnotations(tree: Tree)(using Context): Unit =
    tree match
      case tree: DefTree =>
        lazy val annotCtx = ctx.fresh.setProperty(InAnnotation, true).withOwner(tree.symbol)
        for (annot <- tree.symbol.annotations) annot match
          case annot: BodyAnnotation => annot // already checked in PrepareInlineable before the creation of the BodyAnnotation
          case annot => transform(annot.tree)(using annotCtx)
      case _ =>

  /** Heal types in the info of the given tree */
  private def healInfo(tree: Tree, pos: SrcPos)(using Context): Unit =
    tree.symbol.info = healType(pos)(tree.symbol.info)

  /** If the type refers to a locally defined symbol (either directly, or in a pickled type),
   *  check that its staging level matches the current level.
   *  - Static types and term are allowed at any level.
   *  - If a type reference is used a higher level, then it is inconsistent. Will attempt to heal before failing.
   *  - If a term reference is used a different level, then it is inconsistent.
   *
   *  If `T` is a reference to a type at the wrong level, try to heal it by replacing it with
   *  a type tag of type `quoted.Type[T]`.
   *  The tag is generated by an instance of `QuoteTypeTags` directly if the splice is explicit
   *  or indirectly by `tryHeal`.
   */
  private def healType(pos: SrcPos)(using Context) = new TypeMap {
    def apply(tp: Type): Type =
      tp match
        case tp: TypeRef =>
          tp.prefix match
            case NoPrefix if level > levelOf(tp.symbol) && !tp.typeSymbol.hasAnnotation(defn.QuotedRuntime_SplicedTypeAnnot) =>
              val tp1 = tp.dealias
              if tp1 != tp then apply(tp1)
              else tryHeal(tp.symbol, tp, pos)
            case prefix: ThisType if !tp.symbol.isStatic && level > levelOf(prefix.cls) =>
              tryHeal(tp.symbol, tp, pos)
            case prefix: TermRef if tp.symbol.isTypeSplice =>
              prefix.symbol.info.argInfos match
                case (tb: TypeBounds) :: _ =>
                  report.error(em"Cannot splice $tp because it is a wildcard type", pos)
                case _ =>
              // Heal explicit type splice in the code
              if level > 0 then getQuoteTypeTags.getTagRef(prefix) else tp
            case prefix: TermRef if !prefix.symbol.isStatic && level > levelOf(prefix.symbol) =>
              tryHeal(prefix.symbol, tp, pos)
            case _ =>
              mapOver(tp)
        case tp: ThisType if level != -1 && level != levelOf(tp.cls) =>
          levelError(tp.cls, tp, pos)
        case tp: AnnotatedType =>
          val newAnnotTree = transform(tp.annot.tree)
          derivedAnnotatedType(tp, apply(tp.parent), tp.annot.derivedAnnotation(newAnnotTree))
        case _ =>
          mapOver(tp)
  }

  /** Check phase consistency of terms and heal inconsistent type references. */
  private def healTypeOfTerm(pos: SrcPos)(using Context) = new TypeMap {
    def apply(tp: Type): Type =
      tp match
        case tp @ TypeRef(NoPrefix, _) if level > levelOf(tp.symbol) =>
          tryHeal(tp.symbol, tp, pos)
        case tp @ TermRef(NoPrefix, _) if !tp.symbol.isStatic && level != levelOf(tp.symbol) =>
          levelError(tp.symbol, tp, pos)
        case tp: ThisType if level != -1 && level != levelOf(tp.cls) =>
          levelError(tp.cls, tp, pos)
        case tp: AnnotatedType =>
          derivedAnnotatedType(tp, apply(tp.parent), tp.annot)
        case _ =>
          if tp.typeSymbol.is(Package) then tp
          else mapOver(tp)
  }

  /** Try to heal reference to type `T` used in a higher level than its definition.
   *  Returns a reference to a type tag generated by `QuoteTypeTags` that contains a
   *  reference to a type alias containing the equivalent of `${summon[quoted.Type[T]]}`.
   *  Emits and error if `T` cannot be healed and returns `T`.
   */
  protected def tryHeal(sym: Symbol, tp: TypeRef, pos: SrcPos)(using Context): TypeRef = {
    val reqType = defn.QuotedTypeClass.typeRef.appliedTo(tp)
    val tag = ctx.typer.inferImplicitArg(reqType, pos.span)
    tag.tpe match

      case tp: TermRef =>
        checkStable(tp, pos, "type witness")
        getQuoteTypeTags.getTagRef(tp)
      case _: SearchFailureType =>
        val tpStr = tp.show
        val reqTypeStr = reqType.show
        report.error(
          ctx.typer.missingArgMsg(tag, reqType, "")
            .prepend(s"Reference to $tpStr within quotes requires a given $reqTypeStr in scope.\n")
            .append("\n"),
            pos)
        tp
      case _ =>
        report.error(em"""Reference to $tp within quotes requires a given $reqType in scope.
                     |
                     |""", pos)
        tp
  }

  private def levelError(sym: Symbol, tp: Type, pos: SrcPos)(using Context): tp.type = {
    def symStr =
      if (!tp.isInstanceOf[ThisType]) sym.show
      else if (sym.is(ModuleClass)) sym.sourceModule.show
      else i"${sym.name}.this"
    report.error(
      em"""access to $symStr from wrong staging level:
          | - the definition is at level ${levelOf(sym)},
          | - but the access is at level $level.""", pos)
    tp
  }

}

object PCPCheckAndHeal {
  import tpd._

  class QuoteTypeTags(span: Span)(using Context) {

    private val tags = collection.mutable.LinkedHashMap.empty[Symbol, TypeDef]

    def getTagRef(spliced: TermRef): TypeRef = {
      val typeDef = tags.getOrElseUpdate(spliced.symbol, mkTagSymbolAndAssignType(spliced))
      typeDef.symbol.typeRef
    }

    def getTypeTags: List[TypeDef] = tags.valuesIterator.toList

    private def mkTagSymbolAndAssignType(spliced: TermRef): TypeDef = {
      val splicedTree = tpd.ref(spliced).withSpan(span)
      val rhs = splicedTree.select(tpnme.Underlying).withSpan(span)
      val alias = ctx.typeAssigner.assignType(untpd.TypeBoundsTree(rhs, rhs), rhs, rhs, EmptyTree)
      val local = newSymbol(
        owner = ctx.owner,
        name = UniqueName.fresh((splicedTree.symbol.name.toString + "$_").toTermName).toTypeName,
        flags = Synthetic,
        info = TypeAlias(splicedTree.tpe.select(tpnme.Underlying)),
        coord = span).asType
      local.addAnnotation(Annotation(defn.QuotedRuntime_SplicedTypeAnnot))
      ctx.typeAssigner.assignType(untpd.TypeDef(local.name, alias), local)
    }

  }

}
