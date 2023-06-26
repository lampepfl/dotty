package dotty.tools
package dotc
package inlines

import ast.*, core.*
import Flags.*, Symbols.*, Types.*, Decorators.*, Constants.*, Contexts.*, TypeOps.*
import Names.Name
import StdNames.{str, nme, tpnme}
import transform.SymUtils._
import typer.*
import NameKinds.BodyRetainerName
import SymDenotations.SymDenotation
import config.Printers.inlining
import ErrorReporting.errorTree
import dotty.tools.dotc.util.{SourceFile, SourcePosition, SrcPos}
import parsing.Parsers.Parser
import transform.{PostTyper, Inlining, CrossVersionChecks}
import staging.StagingLevel

import collection.mutable
import reporting.{NotConstant, trace}
import util.Spans.{Span, spanCoord}
import NameOps.expandedName

/** Support for querying inlineable methods and for inlining calls to such methods */
object Inlines:
  import tpd._

  /** An exception signalling that an inline info cannot be computed due to a
   *  cyclic reference. i14772.scala shows a case where this happens.
   */
  private[dotc] class MissingInlineInfo extends Exception

  /** `sym` is an inline method with a known body to inline.
   */
  def hasBodyToInline(sym: SymDenotation)(using Context): Boolean =
    (sym.isInlineMethod || sym.isInlineTrait) && sym.hasAnnotation(defn.BodyAnnot)

  /** The body to inline for method `sym`, or `EmptyTree` if none exists.
   *  @pre  hasBodyToInline(sym)
   */
  def bodyToInline(sym: SymDenotation)(using Context): Tree =
    if hasBodyToInline(sym) then
      sym.getAnnotation(defn.BodyAnnot).get.tree
    else
      EmptyTree

  def defsToInline(traitSym: SymDenotation)(using Context): List[Tree] =
    bodyToInline(traitSym) match
      case Block(defs, _) if traitSym.isInlineTrait => defs
      case _ => Nil

  /** Are we in an inline method body? */
  def inInlineMethod(using Context): Boolean =
    ctx.owner.ownersIterator.exists(_.isInlineMethod)

  def inInlineContext(using Context): Boolean =
    ctx.owner.ownersIterator.exists(sym => sym.isInlineMethod || sym.isInlineTrait)

  /** Can a call to method `meth` be inlined? */
  def isInlineable(meth: Symbol)(using Context): Boolean =
    meth.isInlineMethod && meth.hasAnnotation(defn.BodyAnnot) && !inInlineMethod

  def isInlineableFromInlineTrait(inlinedTraitSym: ClassSymbol, member: tpd.Tree)(using Context): Boolean =
    !(member.isInstanceOf[tpd.TypeDef] && inlinedTraitSym.typeParams.contains(member.symbol))
    && !member.symbol.isAllOf(Inline)

  /** Should call be inlined in this context? */
  def needsInlining(tree: Tree)(using Context): Boolean =
    def isInlineableInCtx =
      StagingLevel.level == 0
      && (
        ctx.phase == Phases.inliningPhase
        || (ctx.phase == Phases.typerPhase && needsTransparentInlining(tree))
        || (ctx.phase == Phases.specializeInlineTraitsPhase && !tree.symbol.is(Macro))
      )
      && !ctx.typer.hasInliningErrors
      && !ctx.base.stopInlining
      && !ctx.owner.ownersIterator.exists(_.isInlineTrait)

    tree match
      case Block(_, expr) =>
        needsInlining(expr)
      case tdef @ TypeDef(_, impl: Template) =>
        impl.parents.map(symbolFromParent).exists(_.isInlineTrait) && isInlineableInCtx
      case _ =>
        isInlineable(tree.symbol) && !tree.tpe.widenTermRefExpr.isInstanceOf[MethodOrPoly] && isInlineableInCtx

  private[dotc] def symbolFromParent(parent: Tree)(using Context): Symbol =
    if parent.symbol.isConstructor then parent.symbol.owner else parent.symbol

  private def inlineTraitAncestors(cls: TypeDef)(using Context): List[Tree] = cls match {
    case tpd.TypeDef(_, tmpl: Template) =>
      val parentTrees: Map[Symbol, Tree] = tmpl.parents.map(par => symbolFromParent(par) -> par).toMap.filter(_._1.isInlineTrait)
      val ancestors: List[ClassSymbol] = cls.tpe.baseClasses.filter(sym => sym.isInlineTrait && sym != cls.symbol)
      ancestors.flatMap(ancestor =>
        def baseTree =
          cls.tpe.baseType(ancestor) match
            case AppliedType(tycon, targs) =>
              Some(AppliedTypeTree(TypeTree(tycon), targs.map(TypeTree(_))))
            case tref: TypeRef =>
              Some(Ident(tref))
            case baseTpe =>
              report.error(s"unknown base type ${baseTpe.show} for ancestor ${ancestor.show} of ${cls.symbol.show}")
              None
        parentTrees.get(ancestor).orElse(baseTree.map(_.withSpan(cls.span)))
      )
    case _ =>
      Nil
  }

  private def needsTransparentInlining(tree: Tree)(using Context): Boolean =
    tree.symbol.is(Transparent)
    || ctx.mode.is(Mode.ForceInline)
    || ctx.settings.YforceInlineWhileTyping.value

  /** Try to inline a call to an inline method. Fail with error if the maximal
   *  inline depth is exceeded.
   *
   *  @param tree   The call to inline
   *  @param pt     The expected type of the call.
   *  @return   An `Inlined` node that refers to the original call and the inlined bindings
   *            and body that replace it.
   */
  def inlineCall(tree: Tree)(using Context): Tree =
    if tree.symbol.denot != SymDenotations.NoDenotation
      && tree.symbol.effectiveOwner == defn.CompiletimeTestingPackage.moduleClass
    then
      if (tree.symbol == defn.CompiletimeTesting_typeChecks) return Intrinsics.typeChecks(tree)
      if (tree.symbol == defn.CompiletimeTesting_typeCheckErrors) return Intrinsics.typeCheckErrors(tree)

    if ctx.isAfterTyper then
      // During typer we wait with cross version checks until PostTyper, in order
      // not to provoke cyclic references. See i16116 for a test case.
      CrossVersionChecks.checkExperimentalRef(tree.symbol, tree.srcPos)

    if tree.symbol.isConstructor then return tree // error already reported for the inline constructor definition

    /** Set the position of all trees logically contained in the expansion of
     *  inlined call `call` to the position of `call`. This transform is necessary
     *  when lifting bindings from the expansion to the outside of the call.
     */
    def liftFromInlined(call: Tree) = new TreeMap:
      override def transform(t: Tree)(using Context) =
        if call.span.exists then
          t match
            case Inlined(t, Nil, expr) if t.isEmpty => expr
            case _ if t.isEmpty => t
            case _ => super.transform(t.withSpan(call.span))
        else t
    end liftFromInlined

    val bindings = new mutable.ListBuffer[Tree]

    /** Lift bindings around inline call or in its function part to
     *  the `bindings` buffer. This is done as an optimization to keep
     *  inline call expansions smaller.
     */
    def liftBindings(tree: Tree, liftPos: Tree => Tree): Tree = tree match {
      case Block(stats, expr) =>
        bindings ++= stats.map(liftPos)
        liftBindings(expr, liftPos)
      case Inlined(call, stats, expr) =>
        bindings ++= stats.map(liftPos)
        val lifter = liftFromInlined(call)
        cpy.Inlined(tree)(call, Nil, liftBindings(expr, liftFromInlined(call).transform(_)))
      case Apply(fn, args) =>
        cpy.Apply(tree)(liftBindings(fn, liftPos), args)
      case TypeApply(fn, args) =>
        fn.tpe.widenTermRefExpr match
          case tp: PolyType =>
            val targBounds = tp.instantiateParamInfos(args.map(_.tpe))
            for case (arg, bounds: TypeBounds) <- args.zip(targBounds) if !bounds.contains(arg.tpe) do
              val boundsStr =
                if bounds == TypeBounds.empty then " <: Any. Note that this type is higher-kinded."
                else bounds.show
              report.error(em"${arg.tpe} does not conform to bound$boundsStr", arg)
        cpy.TypeApply(tree)(liftBindings(fn, liftPos), args)
      case Select(qual, name) =>
        cpy.Select(tree)(liftBindings(qual, liftPos), name)
      case _ =>
        tree
    }

    // assertAllPositioned(tree)   // debug
    val tree1 = liftBindings(tree, identity)
    val tree2  =
      if bindings.nonEmpty then
        cpy.Block(tree)(bindings.toList, inlineCall(tree1))
      else if enclosingInlineds.length < ctx.settings.XmaxInlines.value && !reachedInlinedTreesLimit then
        val body =
          try bodyToInline(tree.symbol) // can typecheck the tree and thereby produce errors
          catch case _: MissingInlineInfo =>
            throw CyclicReference(ctx.owner)
        new InlineCall(tree).expand(body)
      else
        ctx.base.stopInlining = true
        val (reason, setting) =
          if reachedInlinedTreesLimit then ("inlined trees", ctx.settings.XmaxInlinedTrees)
          else ("successive inlines", ctx.settings.XmaxInlines)
        errorTree(
          tree,
          em"""|Maximal number of $reason (${setting.value}) exceeded,
               |Maybe this is caused by a recursive inline method?
               |You can use ${setting.name} to change the limit.""",
          (tree :: enclosingInlineds).last.srcPos
        )
    if ctx.base.stopInlining && enclosingInlineds.isEmpty then
      ctx.base.stopInlining = false
        // we have completely backed out of the call that overflowed;
        // reset so that further inline calls can be expanded
    tree2
  end inlineCall

  def inlineParentInlineTraits(cls: Tree)(using Context): Tree =
    cls match {
      case cls @ tpd.TypeDef(_, impl: Template) =>
        val clsOverriddenSyms = cls.symbol.info.decls.toList.flatMap(_.allOverriddenSymbols).toSet
        val newDefs = inContext(ctx.withOwner(cls.symbol)) {
          inlineTraitAncestors(cls).foldLeft((List.empty[Tree], impl.body)){
            case ((inlineDefs, childDefs), parent) =>
              val parentTraitInliner = InlineParentTrait(parent)
              val overriddenSymbols = clsOverriddenSyms ++ inlineDefs.flatMap(_.symbol.allOverriddenSymbols)
              val inlinedDefs1 = inlineDefs ::: parentTraitInliner.expandDefs(overriddenSymbols)
              val childDefs1 = parentTraitInliner.adaptDefs(childDefs)  // TODO do this outside of inlining: we need to adapt ALL references to inlined stuff
              (inlinedDefs1, childDefs1)
          }
        }
        val impl1 = cpy.Template(impl)(body = newDefs._1 ::: newDefs._2)
        cpy.TypeDef(cls)(rhs = impl1)
      case _ =>
        cls
    }

  /** Try to inline a pattern with an inline unapply method. Fail with error if the maximal
   *  inline depth is exceeded.
   *
   *  @param unapp   The tree of the pattern to inline
   *  @return   An `Unapply` with a `fun` containing the inlined call to the unapply
   */
  def inlinedUnapply(unapp: tpd.UnApply)(using Context): Tree =
    // We cannot inline the unapply directly, since the pattern matcher relies on unapply applications
    // as signposts what to do. On the other hand, we can do the inlining only in typer, not afterwards.
    // So the trick is to create a "wrapper" unapply in an anonymous class that has the inlined unapply
    // as its right hand side. The call to the wrapper unapply serves as the signpost for pattern matching.
    // After pattern matching, the anonymous class is removed in phase InlinePatterns with a beta reduction step.
    //
    // An inline unapply `P.unapply` in a pattern `P[...](using ...)(x1,x2,...)(using t1: T1, t2: T2, ...)` is transformed into
    // `{ class $anon { def unapply(s: S)(using t1: T1, t2: T2, ...): R = P.unapply[...](using ...)(s)(using t1, t2, ...) }; new $anon }.unapply(using y1,y2,...)`
    // and the call `P.unapply[...](using ...)(x1, x2, ...)(using t1, t2, ...)` is inlined.
    // This serves as a placeholder for the inlined body until the `patternMatcher` phase. After pattern matcher
    // transforms the patterns into terms, the `inlinePatterns` phase removes this anonymous class by β-reducing
    // the call to the `unapply`.

    val UnApply(fun, trailingImplicits, patterns) = unapp

    val sym = unapp.symbol

    var unapplySym1: Symbol = NoSymbol // created from within AnonClass() and used afterwards

    val newUnapply = AnonClass(ctx.owner, List(defn.ObjectType), sym.coord) { cls =>
      // `fun` is a partially applied method that contains all type applications of the method.
      // The methodic type `fun.tpe.widen` is the type of the function starting from the scrutinee argument
      // and its type parameters are instantiated.
      val unapplySym = newSymbol(cls, sym.name.toTermName, Synthetic | Method, fun.tpe.widen, coord = sym.coord).entered
      val unapply = DefDef(unapplySym.asTerm, argss =>
        val body = fun.appliedToArgss(argss).withSpan(unapp.span)
        if body.symbol.is(Transparent) then inlineCall(body)(using ctx.withOwner(unapplySym))
        else body
      )
      unapplySym1 = unapplySym
      List(unapply)
    }

    val newFun = newUnapply.select(unapplySym1).withSpan(unapp.span)
    cpy.UnApply(unapp)(newFun, trailingImplicits, patterns)
  end inlinedUnapply

  /** For a retained inline method, another method that keeps track of
   *  the body that is kept at runtime. For instance, an inline method
   *
   *      inline override def f(x: T) = b
   *
   *  is complemented by the body retainer method
   *
   *      private def f$retainedBody(x: T) = f(x)
   *
   *  where the call `f(x)` is inline-expanded. This body is then transferred
   *  back to `f` at erasure, using method addRetainedInlineBodies.
   */
  def bodyRetainer(mdef: DefDef)(using Context): DefDef =
    val meth = mdef.symbol.asTerm

    val retainer = meth.copy(
      name = BodyRetainerName(meth.name),
      flags = (meth.flags &~ (Inline | Macro | Override | AbsOverride)) | Private,
      coord = mdef.rhs.span.startPos).asTerm.entered
    retainer.deriveTargetNameAnnotation(meth, name => BodyRetainerName(name.asTermName))
    DefDef(retainer, prefss =>
      inlineCall(
        ref(meth).appliedToArgss(prefss).withSpan(mdef.rhs.span.startPos))(
        using ctx.withOwner(retainer)))
    .showing(i"retainer for $meth: $result", inlining)

  /** Replace `Inlined` node by a block that contains its bindings and expansion */
  def dropInlined(inlined: Inlined)(using Context): Tree =
    val tree1 =
      if inlined.bindings.isEmpty then inlined.expansion
      else cpy.Block(inlined)(inlined.bindings, inlined.expansion)
    // Reposition in the outer most inlined call
    if (enclosingInlineds.nonEmpty) tree1 else reposition(tree1, inlined.span)

  def reposition(tree: Tree, callSpan: Span)(using Context): Tree =
    // Reference test tests/run/i4947b

    val curSource = ctx.compilationUnit.source

    // Tree copier that changes the source of all trees to `curSource`
    val cpyWithNewSource = new TypedTreeCopier {
      override protected def sourceFile(tree: tpd.Tree): SourceFile = curSource
      override protected val untpdCpy: untpd.UntypedTreeCopier = new untpd.UntypedTreeCopier {
        override protected def sourceFile(tree: untpd.Tree): SourceFile = curSource
      }
    }

    /** Removes all Inlined trees, replacing them with blocks.
     *  Repositions all trees directly inside an inlined expansion of a non empty call to the position of the call.
     *  Any tree directly inside an empty call (inlined in the inlined code) retains their position.
     *
     *  Until we implement JSR-45, we cannot represent in output positions in other source files.
     *  So, reposition inlined code from other files with the call position.
     */
    class Reposition extends TreeMap(cpyWithNewSource) {

      override def transform(tree: Tree)(using Context): Tree = {
        def fixSpan[T <: untpd.Tree](copied: T): T =
          copied.withSpan(if tree.source == curSource then tree.span else callSpan)
        def finalize(copied: untpd.Tree) =
          fixSpan(copied).withAttachmentsFrom(tree).withTypeUnchecked(tree.tpe)

        inContext(ctx.withSource(curSource)) {
          tree match
            case tree: Ident => finalize(untpd.Ident(tree.name)(curSource))
            case tree: Literal => finalize(untpd.Literal(tree.const)(curSource))
            case tree: This => finalize(untpd.This(tree.qual)(curSource))
            case tree: JavaSeqLiteral => finalize(untpd.JavaSeqLiteral(transform(tree.elems), transform(tree.elemtpt))(curSource))
            case tree: SeqLiteral => finalize(untpd.SeqLiteral(transform(tree.elems), transform(tree.elemtpt))(curSource))
            case tree: Bind => finalize(untpd.Bind(tree.name, transform(tree.body))(curSource))
            case tree: TypeTree => finalize(tpd.TypeTree(tree.tpe))
            case tree: DefTree => super.transform(tree).setDefTree
            case EmptyTree => tree
            case _ => fixSpan(super.transform(tree))
        }
      }
    }

    (new Reposition).transform(tree)
  end reposition

  /** Leave only a call trace consisting of
   *  - a reference to the top-level class from which the call was inlined,
   *  - the call's position
   *  in the call field of an Inlined node.
   *  The trace has enough info to completely reconstruct positions.
   *  Note: For macros it returns a Select and for other inline methods it returns an Ident (this distinction is only temporary to be able to run YCheckPositions)
   */
  def inlineCallTrace(callSym: Symbol, pos: SourcePosition)(using Context): Tree = {
    assert(ctx.source == pos.source)
    val topLevelCls = callSym.topLevelClass
    if (callSym.is(Macro)) ref(topLevelCls.owner).select(topLevelCls.name)(using ctx.withOwner(topLevelCls.owner)).withSpan(pos.span)
    else Ident(topLevelCls.typeRef).withSpan(pos.span)
  }

  private object Intrinsics:
    import dotty.tools.dotc.reporting.Diagnostic.Error
    private enum ErrorKind:
      case Parser, Typer

    private def compileForErrors(tree: Tree)(using Context): List[(ErrorKind, Error)] =
      assert(tree.symbol == defn.CompiletimeTesting_typeChecks || tree.symbol == defn.CompiletimeTesting_typeCheckErrors)
      def stripTyped(t: Tree): Tree = t match {
        case Typed(t2, _) => stripTyped(t2)
        case Block(Nil, t2) => stripTyped(t2)
        case Inlined(_, Nil, t2) => stripTyped(t2)
        case _ => t
      }

      val Apply(_, codeArg :: Nil) = tree: @unchecked
      val codeArg1 = stripTyped(codeArg.underlying)
      val underlyingCodeArg =
        if Inlines.isInlineable(codeArg1.symbol) then stripTyped(Inlines.inlineCall(codeArg1))
        else codeArg1

      ConstFold(underlyingCodeArg).tpe.widenTermRefExpr match {
        case ConstantType(Constant(code: String)) =>
          val source2 = SourceFile.virtual("tasty-reflect", code)
          inContext(ctx.fresh.setNewTyperState().setTyper(new Typer(ctx.nestingLevel + 1)).setSource(source2)) {
            val tree2 = new Parser(source2).block()
            if ctx.reporter.allErrors.nonEmpty then
              ctx.reporter.allErrors.map((ErrorKind.Parser, _))
            else
              val tree3 = ctx.typer.typed(tree2)
              ctx.base.postTyperPhase match
                case postTyper: PostTyper if ctx.reporter.allErrors.isEmpty =>
                  val tree4 = atPhase(postTyper) { postTyper.newTransformer.transform(tree3) }
                  ctx.base.inliningPhase match
                    case inlining: Inlining if ctx.reporter.allErrors.isEmpty =>
                      atPhase(inlining) { inlining.newTransformer.transform(tree4) }
                    case _ =>
                case _ =>
              ctx.reporter.allErrors.map((ErrorKind.Typer, _))
          }
        case t =>
          report.error(em"argument to compileError must be a statically known String but was: $codeArg", codeArg1.srcPos)
          Nil
      }

    private def packError(kind: ErrorKind, error: Error)(using Context): Tree =
      def lit(x: Any) = Literal(Constant(x))
      val constructor: Tree = ref(defn.CompiletimeTesting_Error_apply)
      val parserErrorKind: Tree = ref(defn.CompiletimeTesting_ErrorKind_Parser)
      val typerErrorKind: Tree = ref(defn.CompiletimeTesting_ErrorKind_Typer)

      constructor.appliedTo(
        lit(error.message),
        lit(error.pos.lineContent.reverse.dropWhile("\n ".contains).reverse),
        lit(error.pos.column),
        if kind == ErrorKind.Parser then parserErrorKind else typerErrorKind)

    private def packErrors(errors: List[(ErrorKind, Error)], pos: SrcPos)(using Context): Tree =
      val individualErrors: List[Tree] = errors.map(packError)
      val errorTpt = ref(defn.CompiletimeTesting_ErrorClass).withSpan(pos.span)
      mkList(individualErrors, errorTpt)

    /** Expand call to scala.compiletime.testing.typeChecks */
    def typeChecks(tree: Tree)(using Context): Tree =
      val errors = compileForErrors(tree)
      Literal(Constant(errors.isEmpty)).withSpan(tree.span)

    /** Expand call to scala.compiletime.testing.typeCheckErrors */
    def typeCheckErrors(tree: Tree)(using Context): Tree =
      val errors = compileForErrors(tree)
      packErrors(errors, tree)

    /** Expand call to scala.compiletime.codeOf */
    def codeOf(arg: Tree, pos: SrcPos)(using Context): Tree =
      Literal(Constant(arg.show(using ctx.withoutColors))).withSpan(pos.span)
  end Intrinsics

  /** Produces an inlined version of `call` via its `inlined` method.
   *
   *  @param  call         the original call to an inlineable method
   *  @param  rhsToInline  the body of the inlineable method that replaces the call.
   */
  private class InlineCall(call: tpd.Tree)(using Context) extends Inliner(call):
    import tpd._
    import Inlines.*

    /** The Inlined node representing the inlined call */
    def expand(rhsToInline: Tree): Tree =

      // Special handling of `requireConst` and `codeOf`
      callValueArgss match
        case (arg :: Nil) :: Nil =>
          if inlinedMethod == defn.Compiletime_requireConst then
            arg match
              case ConstantValue(_) | Inlined(_, Nil, Typed(ConstantValue(_), _)) => // ok
              case _ => report.error(em"expected a constant value but found: $arg", arg.srcPos)
            return Literal(Constant(())).withSpan(call.span)
          else if inlinedMethod == defn.Compiletime_codeOf then
            return Intrinsics.codeOf(arg, call.srcPos)
        case _ =>

      // Special handling of `constValue[T]`, `constValueOpt[T], and summonInline[T]`
      if callTypeArgs.length == 1 then
        if (inlinedMethod == defn.Compiletime_constValue) {
          val constVal = tryConstValue
          if constVal.isEmpty then
            val msg = NotConstant("cannot take constValue", callTypeArgs.head.tpe)
            return ref(defn.Predef_undefined).withSpan(call.span).withType(ErrorType(msg))
          else
            return constVal
        }
        else if (inlinedMethod == defn.Compiletime_constValueOpt) {
          val constVal = tryConstValue
          return (
            if (constVal.isEmpty) ref(defn.NoneModule.termRef)
            else New(defn.SomeClass.typeRef.appliedTo(constVal.tpe), constVal :: Nil)
          )
        }
        else if (inlinedMethod == defn.Compiletime_summonInline) {
          def searchImplicit(tpt: Tree) =
            val evTyper = new Typer(ctx.nestingLevel + 1)
            val evCtx = ctx.fresh.setTyper(evTyper)
            inContext(evCtx) {
              val evidence = evTyper.inferImplicitArg(tpt.tpe, tpt.span)
              evidence.tpe match
                case fail: Implicits.SearchFailureType =>
                  errorTree(call, evTyper.missingArgMsg(evidence, tpt.tpe, ""))
                case _ =>
                  evidence
            }
          return searchImplicit(callTypeArgs.head)
        }
      end if

      val (bindings, expansion) = super.inlined(rhsToInline)

      // Take care that only argument bindings go into `bindings`, since positions are
      // different for bindings from arguments and bindings from body.
      val res = tpd.Inlined(call, bindings, expansion)

      if !hasOpaqueProxies then res
      else
        val target =
          if inlinedMethod.is(Transparent) then call.tpe & res.tpe
          else call.tpe
        res.ensureConforms(target)
          // Make sure that the sealing with the declared type
          // is type correct. Without it we might get problems since the
          // expression's type is the opaque alias but the call's type is
          // the opaque type itself. An example is in pos/opaque-inline1.scala.
    end expand
  end InlineCall

  private class InlineParentTrait(parent: tpd.Tree)(using Context) extends Inliner(parent):
    import tpd._
    import Inlines.*

    private val parentSym = symbolFromParent(parent)
    private val paramAccessorsMapper = ParamAccessorsMapper()
    private val innerClassNewSyms: mutable.LinkedHashMap[Symbol, Symbol] = mutable.LinkedHashMap.empty

    private val childThisType = ctx.owner.thisType
    private val childThisTree = This(ctx.owner.asClass).withSpan(parent.span)

    def expandDefs(overriddenDecls: Set[Symbol]): List[Tree] =
      paramAccessorsMapper.registerParamValuesOf(parent)
      val stats = Inlines.defsToInline(parentSym).filterNot(stat => overriddenDecls.contains(stat.symbol))
      stats.map{
        case member: MemberDef => Left((member, inlinedSym(member.symbol))) // Private symbols must be entered before the RHSs are inlined
        case stat => Right(stat)
      }.map{
        case Left((tree, inlinedSym)) => expandStat(tree, inlinedSym)
        case Right(tree) => inlinedRhs(tree)
      }
    end expandDefs

    def adaptDefs(definitions: List[Tree]): List[Tree] = definitions.mapconserve(defsAdapter(_))

    protected class InlineTraitTypeMap extends InlinerTypeMap {
      override def apply(t: Type) = super.apply(t) match {
        case t: ThisType if t.cls == parentSym => childThisType
        case t => mapOver(t)
      }
    }

    protected class InlineTraitTreeMap extends InlinerTreeMap {
      override def apply(tree: Tree) = super.apply(tree) match {
        case tree: This if tree.symbol == parentSym =>
          Inlined(EmptyTree, Nil, childThisTree).withSpan(parent.span)
        case tree: This =>
          tree.tpe match {
            case thisTpe: ThisType if thisTpe.cls.isInlineTrait =>
              integrate(This(ctx.owner.asClass).withSpan(parent.span), thisTpe.cls)
            case _ =>
              tree
          }
        case Select(qual, name) =>
          paramAccessorsMapper.getParamAccessorName(qual.symbol, name) match {
            case Some(newName) => Select(this(qual), newName).withSpan(parent.span)
            case None => Select(this(qual), name)
          }
        case tree =>
          tree
      }
    }

    override protected val inlinerTypeMap: InlinerTypeMap = InlineTraitTypeMap()
    override protected val inlinerTreeMap: InlinerTreeMap = InlineTraitTreeMap()

    override protected def substFrom: List[Symbol] = innerClassNewSyms.keys.toList
    override protected def substTo: List[Symbol] = innerClassNewSyms.values.toList
    override protected def inlineCopier: tpd.TreeCopier = new TypedTreeCopier() {
      // FIXME it feels weird... Is this correct?
      override def Apply(tree: Tree)(fun: Tree, args: List[Tree])(using Context): Apply =
        untpd.cpy.Apply(tree)(fun, args).withTypeUnchecked(tree.tpe)
    }

    override protected def computeThisBindings(): Unit = ()
    override protected def canElideThis(tpe: ThisType): Boolean = true

    override protected def inlineCtx(inlineTyper: InlineTyper)(using Context): Context =
      ctx.fresh.setTyper(inlineTyper).setNewScope

    extension (sym: Symbol)
      private def isTermParamAccessor: Boolean = !sym.isType && sym.is(ParamAccessor)

    private def expandStat(stat: tpd.Tree, inlinedSym: Symbol)(using Context): tpd.Tree = stat match
      case stat: ValDef =>
        inlinedValDef(stat, inlinedSym)
      case stat: DefDef =>
        inlinedDefDef(stat, inlinedSym)
      case stat @ TypeDef(_, _: Template) =>
        inlinedClassDef(stat, inlinedSym.asClass)
      case stat: TypeDef =>
        inlinedTypeDef(stat, inlinedSym)

    private def inlinedSym(sym: Symbol, withoutFlags: FlagSet = EmptyFlags)(using Context): Symbol =
      if sym.isClass then inlinedClassSym(sym.asClass, withoutFlags) else inlinedMemberSym(sym, withoutFlags)

    private def inlinedClassSym(sym: ClassSymbol, withoutFlags: FlagSet = EmptyFlags)(using Context): ClassSymbol =
      sym.info match {
        case clsInfo: ClassInfo =>
          val typeParams: List[Type] = sym.primaryConstructor.info match {
            case poly: PolyType => poly.paramRefs
            case _ => Nil
          }
          // Extend inner class from inline trait to preserve typing
          val newParent = ctx.owner.thisType.select(sym).appliedTo(typeParams)
          val inlinedSym = newClassSymbol(
            ctx.owner,
            sym.name,
            (sym.flags | Synthetic) &~ withoutFlags,
            newCls => {
              val ClassInfo(prefix, _, parents, _, selfInfo) = inlinerTypeMap.mapClassInfo(clsInfo)
              ClassInfo(prefix, newCls, parents :+ newParent, Scopes.newScope, selfInfo) // TODO fix selfInfo (what to use?)
            },
            sym.privateWithin,
            spanCoord(parent.span)
          )
          inlinedSym.setTargetName(sym.name ++ str.NAME_JOIN ++ ctx.owner.name)
          innerClassNewSyms.put(sym, inlinedSym)
          inlinedSym.entered
        case _ =>
          report.error(s"Class symbol ${sym.show} does not have class info")
          sym
      }

    private def inlinedMemberSym(sym: Symbol, withoutFlags: FlagSet = EmptyFlags)(using Context): Symbol =
      var name = sym.name
      var flags = sym.flags | Synthetic
      if sym.isType || !sym.is(Private) then flags |= Override
      if sym.isTermParamAccessor then
        flags &~= ParamAccessor
        if sym.is(Local) && sym.owner.isInlineTrait then
          name = paramAccessorsMapper.registerNewName(sym)
      sym.copy(
        owner = ctx.owner,
        name = name,
        flags = flags &~ withoutFlags,
        info = inlinerTypeMap(sym.info),
        coord = spanCoord(parent.span)).entered

    private def inlinedValDef(vdef: ValDef, inlinedSym: Symbol)(using Context): ValDef =
      val rhs =
        paramAccessorsMapper
          .getParamAccessorRhs(vdef.symbol.owner, vdef.symbol.name)
          .getOrElse(inlinedRhs(vdef, inlinedSym))
      tpd.ValDef(inlinedSym.asTerm, rhs).withSpan(parent.span)

    private def inlinedDefDef(ddef: DefDef, inlinedSym: Symbol)(using Context): DefDef =
      val rhsFun: List[List[Tree]] => Tree =
        if ddef.symbol.isSetter then
          _ => unitLiteral
        else
          paramss =>
            val oldParamSyms = ddef.paramss.flatten.map(_.symbol)
            val newParamSyms = paramss.flatten.map(_.symbol)
            val ddef1 = cpy.DefDef(ddef)(rhs = ddef.rhs.subst(oldParamSyms, newParamSyms))
            inlinedRhs(ddef1, inlinedSym)
      tpd.DefDef(inlinedSym.asTerm, rhsFun).withSpan(parent.span)

    private def inlinedPrimaryConstructorDefDef(ddef: DefDef)(using Context): DefDef =
      // TODO check if symbol must be copied
      val inlinedSym = inlinedMemberSym(ddef.symbol, withoutFlags = Override)
      val constr = inlinedDefDef(ddef, inlinedSym)
      cpy.DefDef(constr)(tpt = TypeTree(defn.UnitType), rhs = EmptyTree)

    private def inlinedClassDef(clsDef: TypeDef, inlinedCls: ClassSymbol)(using Context): Tree =
      val TypeDef(_, tmpl: Template) = clsDef: @unchecked
      val (constr, body) = inContext(ctx.withOwner(inlinedCls)) {
        val inlinedConstr = inlinedPrimaryConstructorDefDef(tmpl.constr)
        val inlinedTmpl = tmpl.body.map {
          case stat: TypeDef if stat.symbol.isAllOf(PrivateLocal | Param) =>
            expandStat(stat, inlinedSym(stat.symbol, withoutFlags = Override))
          case stat =>
            expandStat(stat, inlinedSym(stat.symbol))
        }
        (inlinedConstr, inlinedTmpl)
      }
      val clsDef1 = tpd.ClassDefWithParents(inlinedCls, constr, tmpl.parents, body) // TODO add correct parent tree
      inlined(clsDef1)._2.withSpan(clsDef.span)

    private def inlinedTypeDef(tdef: TypeDef, inlinedSym: Symbol)(using Context): TypeDef =
      tpd.TypeDef(inlinedSym.asType).withSpan(parent.span)

    private def inlinedRhs(vddef: ValOrDefDef, inlinedSym: Symbol)(using Context): Tree =
      val rhs = vddef.rhs.changeOwner(vddef.symbol, inlinedSym)
      inlinedRhs(rhs)(using ctx.withOwner(inlinedSym))

    private def inlinedRhs(rhs: Tree)(using Context): Tree =
      if rhs.isEmpty then
        rhs
      else
        // TODO make version of inlined that does not return bindings?
        Inlined(tpd.ref(parentSym), Nil, inlined(rhs)._2).withSpan(parent.span)

    private val defsAdapter =
      val typeMap = new DeepTypeMap {
        override def apply(tp: Type): Type = tp match {
          case TypeRef(_, sym: Symbol) if innerClassNewSyms.contains(sym) =>
            TypeRef(childThisType, innerClassNewSyms(sym))
          case _ =>
            mapOver(tp)
        }
      }
      def treeMap(tree: Tree) = tree match {
        case ident: Ident if innerClassNewSyms.contains(ident.symbol) =>
          Ident(innerClassNewSyms(ident.symbol).namedType)
        case tdef: TypeDef if tdef.symbol.isClass =>
          tdef.symbol.info = typeMap(tdef.symbol.info)
          tdef
        case tree =>
          tree
      }
      new TreeTypeMap(
        typeMap = typeMap,
        treeMap = treeMap,
        substFrom = substFrom,
        substTo = substTo,
      )
    end defsAdapter

    private class ParamAccessorsMapper:
      private val paramAccessorsTrees: mutable.Map[Symbol, Map[Name, Tree]] = mutable.Map.empty
      private val paramAccessorsNewNames: mutable.Map[(Symbol, Name), Name] = mutable.Map.empty

      def registerParamValuesOf(parent: Tree): Unit =
        def allArgs(tree: Tree, acc: Vector[List[Tree]]): List[List[Tree]] = tree match
          case Apply(fun, args) => allArgs(fun, acc :+ args)
          case TypeApply(fun, _) => allArgs(fun, acc)
          case _ => acc.toList
        def allParams(info: Type, acc: List[List[Name]]): List[List[Name]] = info match
          case mt: MethodType => allParams(mt.resultType, mt.paramNames :: acc)
          case pt: PolyType => allParams(pt.resultType, acc)
          case _ => acc
        val info =
          if parent.symbol.isClass then parent.symbol.primaryConstructor.info
          else parent.symbol.info
        val paramAccessors = allParams(info, Nil).flatten.zip(allArgs(parent, Vector.empty).flatten).toMap
        paramAccessorsTrees.put(symbolFromParent(parent), paramAccessors)

      def registerNewName(paramAccessorSym: Symbol): paramAccessorSym.ThisName =
        val oldName = paramAccessorSym.name
        val newName = oldName.expandedName(parentSym)
        paramAccessorsNewNames.put((paramAccessorSym.owner, oldName), newName)
        newName

      def getParamAccessorRhs(parent: Symbol, paramAccessorName: Name): Option[Tree] =
        paramAccessorsTrees.get(parent).flatMap(_.get(paramAccessorName))

      def getParamAccessorName(parent: Symbol, paramAccessorName: Name): Option[Name] =
        paramAccessorsNewNames.get(parent, paramAccessorName)
    end ParamAccessorsMapper
  end InlineParentTrait
end Inlines
