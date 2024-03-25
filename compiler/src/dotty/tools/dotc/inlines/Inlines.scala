package dotty.tools
package dotc
package inlines

import ast.*, core.*
import Flags.*, Symbols.*, Types.*, Decorators.*, Constants.*, Contexts.*
import StdNames.{tpnme, nme}
import NameOps.*
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
import util.Spans.Span

/** Support for querying inlineable methods and for inlining calls to such methods */
object Inlines:
  import tpd.*

  /** An exception signalling that an inline info cannot be computed due to a
   *  cyclic reference. i14772.scala shows a case where this happens.
   */
  private[dotc] class MissingInlineInfo extends Exception

  /** `sym` is an inline method with a known body to inline.
   */
  def hasBodyToInline(sym: SymDenotation)(using Context): Boolean =
    sym.isInlineMethod && sym.hasAnnotation(defn.BodyAnnot)

  /** The body to inline for method `sym`, or `EmptyTree` if none exists.
   *  @pre  hasBodyToInline(sym)
   */
  def bodyToInline(sym: SymDenotation)(using Context): Tree =
    if hasBodyToInline(sym) then
      sym.getAnnotation(defn.BodyAnnot).get.tree
    else
      EmptyTree

  /** Are we in an inline method body? */
  def inInlineMethod(using Context): Boolean =
    ctx.owner.ownersIterator.exists(_.isInlineMethod)

  /** Can a call to method `meth` be inlined? */
  def isInlineable(meth: Symbol)(using Context): Boolean =
    meth.is(Inline) && meth.hasAnnotation(defn.BodyAnnot) && !inInlineMethod

  /** Should call be inlined in this context? */
  def needsInlining(tree: Tree)(using Context): Boolean = tree match {
    case Block(_, expr) => needsInlining(expr)
    case _ =>
      def isUnapplyExpressionWithDummy: Boolean =
        // The first step of typing an `unapply` consists in typing the call
        // with a dummy argument (see Applications.typedUnApply). We delay the
        // inlining of this call.
        def rec(tree: Tree): Boolean = tree match
          case Apply(_, ProtoTypes.dummyTreeOfType(_) :: Nil) => true
          case Apply(fn, _) => rec(fn)
          case _ => false
        tree.symbol.name.isUnapplyName && rec(tree)

      isInlineable(tree.symbol)
      && !tree.tpe.widenTermRefExpr.isInstanceOf[MethodOrPoly]
      && StagingLevel.level == 0
      && (
        ctx.phase == Phases.inliningPhase
        || (ctx.phase == Phases.typerPhase && needsTransparentInlining(tree))
      )
      && !ctx.typer.hasInliningErrors
      && !ctx.base.stopInlining
      && !ctx.mode.is(Mode.NoInline)
      && !isUnapplyExpressionWithDummy
  }

  private def needsTransparentInlining(tree: Tree)(using Context): Boolean =
    tree.symbol.is(Transparent)
    || ctx.mode.is(Mode.ForceInline)

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
      CrossVersionChecks.checkRef(tree.symbol, tree.srcPos)

    if tree.symbol.isConstructor then return tree // error already reported for the inline constructor definition

    /** Set the position of all trees logically contained in the expansion of
     *  inlined call `call` to the position of `call`. This transform is necessary
     *  when lifting bindings from the expansion to the outside of the call.
     */
    def liftFromInlined(call: Tree) = new TreeMap:
      override def transform(t: Tree)(using Context) =
        if call.span.exists then
          t match
            case t @ Inlined(_, Nil, expr) if t.inlinedFromOuterScope => expr
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
      case tree @ Inlined(call, stats, expr) =>
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

  /** Try to inline a pattern with an inline unapply method. Fail with error if the maximal
   *  inline depth is exceeded.
   *
   *  @param fun The function of an Unapply node
   *  @return   An `Unapply` with a `fun` containing the inlined call to the unapply
   */
  def inlinedUnapplyFun(fun: tpd.Tree)(using Context): Tree =
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

    val sym = fun.symbol

    val newUnapply = AnonClass(ctx.owner, List(defn.ObjectType), sym.coord) { cls =>
      // `fun` is a partially applied method that contains all type applications of the method.
      // The methodic type `fun.tpe.widen` is the type of the function starting from the scrutinee argument
      // and its type parameters are instantiated.
      val unapplyInfo = fun.tpe.widen
      val unapplySym = newSymbol(cls, sym.name.toTermName, Synthetic | Method, unapplyInfo, coord = sym.coord).entered

      val unapply = DefDef(unapplySym.asTerm, argss => fun.appliedToArgss(argss).withSpan(fun.span))

      if sym.is(Transparent) then
        // Inline the body and refine the type of the unapply method
        val inlinedBody = inlineCall(unapply.rhs)(using ctx.withOwner(unapplySym))
        val refinedResultType = inlinedBody.tpe.widen
        def refinedResult(info: Type): Type = info match
          case info: LambdaType => info.newLikeThis(info.paramNames, info.paramInfos, refinedResult(info.resultType))
          case _ => refinedResultType
        unapplySym.info = refinedResult(unapplyInfo)
        List(cpy.DefDef(unapply)(tpt = TypeTree(refinedResultType), rhs = inlinedBody))
      else
        List(unapply)
    }

    newUnapply.select(sym.name).withSpan(fun.span)
  end inlinedUnapplyFun

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
    import tpd.*
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
            return unitLiteral.withSpan(call.span)
          else if inlinedMethod == defn.Compiletime_codeOf then
            return Intrinsics.codeOf(arg, call.srcPos)
        case _ =>

      // Special handling of `constValue[T]`, `constValueOpt[T]`, `constValueTuple[T]`, `summonInline[T]` and `summonAll[T]`
      if callTypeArgs.length == 1 then

        def constValueOrError(tpe: Type): Tree =
          val constVal = tryConstValue(tpe)
          if constVal.isEmpty then
            val msg = NotConstant("cannot take constValue", tpe)
            ref(defn.Predef_undefined).withSpan(callTypeArgs.head.span).withType(ErrorType(msg))
          else
            constVal

        def searchImplicitOrError(tpe: Type): Tree =
          val evTyper = new Typer(ctx.nestingLevel + 1)
          val evCtx = ctx.fresh.setTyper(evTyper)
          inContext(evCtx) {
            val evidence = evTyper.inferImplicitArg(tpe, callTypeArgs.head.span)
            evidence.tpe match
              case fail: Implicits.SearchFailureType =>
                errorTree(call, evTyper.missingArgMsg(evidence, tpe, ""))
              case _ =>
                evidence
          }

        def unrollTupleTypes(tpe: Type): Option[List[Type]] = tpe.dealias match
          case AppliedType(tycon, args) if defn.isTupleClass(tycon.typeSymbol) =>
            Some(args)
          case AppliedType(tycon, head :: tail :: Nil) if tycon.isRef(defn.PairClass) =>
            unrollTupleTypes(tail).map(head :: _)
          case tpe: TermRef if tpe.symbol == defn.EmptyTupleModule =>
            Some(Nil)
          case tpe: AppliedType if tpe.isMatchAlias =>
            unrollTupleTypes(tpe.tryNormalize)
          case _ =>
            None

        if (inlinedMethod == defn.Compiletime_constValue) {
          return constValueOrError(callTypeArgs.head.tpe)
        }
        else if (inlinedMethod == defn.Compiletime_constValueOpt) {
          val constVal = tryConstValue(callTypeArgs.head.tpe)
          return (
            if (constVal.isEmpty) ref(defn.NoneModule.termRef)
            else New(defn.SomeClass.typeRef.appliedTo(constVal.tpe), constVal :: Nil)
          )
        }
        else if (inlinedMethod == defn.Compiletime_constValueTuple) {
          unrollTupleTypes(callTypeArgs.head.tpe) match
            case Some(types) =>
              val constants = types.map(constValueOrError)
              return Typed(tpd.tupleTree(constants), TypeTree(callTypeArgs.head.tpe)).withSpan(call.span)
            case _ =>
              return errorTree(call, em"Tuple element types must be known at compile time")
        }
        else if (inlinedMethod == defn.Compiletime_summonInline) {
          return searchImplicitOrError(callTypeArgs.head.tpe)
        }
        else if (inlinedMethod == defn.Compiletime_summonAll) {
          unrollTupleTypes(callTypeArgs.head.tpe) match
            case Some(types) =>
              val implicits = types.map(searchImplicitOrError)
              return Typed(tpd.tupleTree(implicits), TypeTree(callTypeArgs.head.tpe)).withSpan(call.span)
            case _ =>
              return errorTree(call, em"Tuple element types must be known at compile time")
        }
      end if

      val (bindings, expansion) = super.inlined(rhsToInline)

      // Take care that only argument bindings go into `bindings`, since positions are
      // different for bindings from arguments and bindings from body.
      val inlined = tpd.Inlined(call, bindings, expansion)

      if !hasOpaqueProxies then inlined
      else
        val target =
          if inlinedMethod.is(Transparent) then call.tpe & inlined.tpe
          else call.tpe
        inlined.ensureConforms(target)
          // Make sure that the sealing with the declared type
          // is type correct. Without it we might get problems since the
          // expression's type is the opaque alias but the call's type is
          // the opaque type itself. An example is in pos/opaque-inline1.scala.
    end expand
  end InlineCall
end Inlines
