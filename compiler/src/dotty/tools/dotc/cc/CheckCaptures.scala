package dotty.tools
package dotc
package cc

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*, Denotations.*
import config.Printers.{capt, recheckr, noPrinter}
import config.{Config, Feature}
import ast.{tpd, untpd, Trees}
import Trees.*
import typer.RefChecks.{checkAllOverrides, checkSelfAgainstParents, OverridingPairsChecker}
import typer.Checking.{checkBounds, checkAppliedTypesIn}
import typer.ErrorReporting.{Addenda, err}
import typer.ProtoTypes.{AnySelectionProto, LhsProto}
import util.{SimpleIdentitySet, EqHashMap, EqHashSet, SrcPos, Property}
import transform.{Recheck, PreRecheck, CapturedVars}
import Recheck.*
import scala.collection.mutable
import CaptureSet.{withCaptureSetsExplained, IdempotentCaptRefMap, CompareResult}
import StdNames.nme
import NameKinds.{DefaultGetterName, WildcardParamName}
import reporting.trace

/** The capture checker */
object CheckCaptures:
  import ast.tpd.*

  enum EnvKind:
    case Regular        // normal case
    case NestedInOwner  // environment is a temporary one nested in the owner's environment,
                        // and does not have a different actual owner symbol
                        // (this happens when doing box adaptation).
    case ClosureResult  // environment is for the result of a closure
    case Boxed          // environment is inside a box (in which case references are not counted)

  /** A class describing environments.
   *  @param owner     the current owner
   *  @param kind      the environment's kind
   *  @param captured  the caputure set containing all references to tracked free variables outside of boxes
   *  @param outer0    the next enclosing environment
   */
  case class Env(
      owner: Symbol,
      kind: EnvKind,
      captured: CaptureSet,
      outer0: Env | Null):

    def outer = outer0.nn

    def isOutermost = outer0 == null

    /** If an environment is open it tracks free references */
    def isOpen = !captured.isAlwaysEmpty && kind != EnvKind.Boxed

    def outersIterator: Iterator[Env] = new:
      private var cur = Env.this
      def hasNext = !cur.isOutermost
      def next(): Env =
        val res = cur
        cur = cur.outer
        res
  end Env

  /** Similar normal substParams, but this is an approximating type map that
   *  maps parameters in contravariant capture sets to the empty set.
   */
  final class SubstParamsMap(from: BindingType, to: List[Type])(using Context)
  extends ApproximatingTypeMap, IdempotentCaptRefMap:
    def apply(tp: Type): Type =
      tp match
        case tp: ParamRef =>
          if tp.binder == from then to(tp.paramNum) else tp
        case tp: NamedType =>
          if tp.prefix `eq` NoPrefix then tp
          else tp.derivedSelect(apply(tp.prefix))
        case _: ThisType =>
          tp
        case _ =>
          mapOver(tp)
  end SubstParamsMap

  final class SubstParamsBiMap(from: LambdaType, to: List[Type])(using Context)
  extends BiTypeMap:
    thisMap =>

    def apply(tp: Type): Type = tp match
      case tp: ParamRef =>
        if tp.binder == from then to(tp.paramNum) else tp
      case tp: NamedType =>
        if tp.prefix `eq` NoPrefix then tp
        else tp.derivedSelect(apply(tp.prefix))
      case _: ThisType =>
        tp
      case _ =>
        mapOver(tp)

    lazy val inverse = new BiTypeMap:
      def apply(tp: Type): Type = tp match
        case tp: NamedType =>
          var idx = 0
          var to1 = to
          while idx < to.length && (tp ne to(idx)) do
            idx += 1
            to1 = to1.tail
          if idx < to.length then from.paramRefs(idx)
          else if tp.prefix `eq` NoPrefix then tp
          else tp.derivedSelect(apply(tp.prefix))
        case _: ThisType =>
          tp
        case _ =>
          mapOver(tp)
      def inverse = thisMap
  end SubstParamsBiMap

  /** Check that a @retains annotation only mentions references that can be tracked.
   *  This check is performed at Typer.
   */
  def checkWellformed(parent: Tree, ann: Tree)(using Context): Unit =
    parent.tpe match
      case _: SingletonType =>
        report.error(em"Singleton type $parent cannot have capture set", parent.srcPos)
      case _ =>
    for elem <- ann.retainedElems do
      val elem1 = elem match
        case ReachCapabilityApply(arg) => arg
        case _ => elem
      elem1.tpe match
        case ref: CaptureRef =>
          if !ref.isTrackableRef then
            report.error(em"$elem cannot be tracked since it is not a parameter or local value", elem.srcPos)
        case tpe =>
          report.error(em"$elem: $tpe is not a legal element of a capture set", elem.srcPos)

  /** Report an error if some part of `tp` contains the root capability in its capture set
   *  or if it refers to an unsealed type parameter that could possibly be instantiated with
   *  cap in a way that's visible at the type.
   */
  private def disallowRootCapabilitiesIn(tp: Type, carrier: Symbol, what: String, have: String, addendum: String, pos: SrcPos)(using Context) =
    val check = new TypeTraverser:

      private val seen = new EqHashSet[TypeRef]

      def traverse(t: Type) =
        t.dealiasKeepAnnots match
          case t: TypeRef =>
            if !seen.contains(t) then
              seen += t
              traverseChildren(t)

              // Check the lower bound of path dependent types.
              // See issue #19330.
              val isMember = t.prefix ne NoPrefix
              t.info match
                case TypeBounds(lo, _) if isMember => traverse(lo)
                case _ =>
          case AnnotatedType(_, ann) if ann.symbol == defn.UncheckedCapturesAnnot =>
            ()
          case t =>
            if variance >= 0 then
              t.captureSet.disallowRootCapability: () =>
                def part = if t eq tp then "" else i"the part $t of "
                report.error(
                  em"""$what cannot $have $tp since
                      |${part}that type captures the root capability `cap`.
                      |$addendum""",
                  pos)
            traverseChildren(t)
    check.traverse(tp)
  end disallowRootCapabilitiesIn

  /** Attachment key for bodies of closures, provided they are values */
  val ClosureBodyValue = Property.Key[Unit]

class CheckCaptures extends Recheck, SymTransformer:
  thisPhase =>

  import ast.tpd.*
  import CheckCaptures.*

  def phaseName: String = "cc"

  override def isRunnable(using Context) = super.isRunnable && Feature.ccEnabledSomewhere

  def newRechecker()(using Context) = CaptureChecker(ctx)

  override def run(using Context): Unit =
    if Feature.ccEnabled then
      super.run

  val ccState = new CCState

  class CaptureChecker(ictx: Context) extends Rechecker(ictx):

    override def keepType(tree: Tree) =
      super.keepType(tree)
      || tree.isInstanceOf[Try]  // type of `try` needs tp be checked for * escapes

    /** Instantiate capture set variables appearing contra-variantly to their
     *  upper approximation.
     */
    private def interpolator(startingVariance: Int = 1)(using Context) = new TypeTraverser:
      variance = startingVariance
      override def traverse(t: Type) = t match
        case t @ CapturingType(parent, refs) =>
          refs match
            case refs: CaptureSet.Var if variance < 0 => refs.solve()
            case _ =>
          traverse(parent)
        case t @ defn.RefinedFunctionOf(rinfo) =>
          traverse(rinfo)
        case _ =>
          traverseChildren(t)

    /** If `tpt` is an inferred type, interpolate capture set variables appearing contra-
     *  variantly in it.
     */
    private def interpolateVarsIn(tpt: Tree)(using Context): Unit =
      if tpt.isInstanceOf[InferredTypeTree] then
        interpolator().traverse(tpt.knownType)
          .showing(i"solved vars in ${tpt.knownType}", capt)

    /** Assert subcapturing `cs1 <: cs2` */
    def assertSub(cs1: CaptureSet, cs2: CaptureSet)(using Context) =
      assert(cs1.subCaptures(cs2, frozen = false).isOK, i"$cs1 is not a subset of $cs2")

    def checkOK(res: CompareResult, prefix: => String, pos: SrcPos, provenance: => String = "")(using Context): Unit =
      if !res.isOK then
        def toAdd: String = CaptureSet.levelErrors.toAdd.mkString
        def descr: String =
          val d = res.blocking.description
          if d.isEmpty then provenance else ""
        report.error(em"$prefix included in the allowed capture set ${res.blocking}$descr$toAdd", pos)

    /** Check subcapturing `{elem} <: cs`, report error on failure */
    def checkElem(elem: CaptureRef, cs: CaptureSet, pos: SrcPos, provenance: => String = "")(using Context) =
      checkOK(
          elem.singletonCaptureSet.subCaptures(cs, frozen = false),
          i"$elem cannot be referenced here; it is not",
          pos, provenance)

    /** Check subcapturing `cs1 <: cs2`, report error on failure */
    def checkSubset(cs1: CaptureSet, cs2: CaptureSet, pos: SrcPos,
        provenance: => String = "", cs1description: String = "")(using Context) =
      checkOK(
          cs1.subCaptures(cs2, frozen = false),
          if cs1.elems.size == 1 then i"reference ${cs1.elems.toList.head}$cs1description is not"
          else i"references $cs1$cs1description are not all",
          pos, provenance)

    /** The current environment */
    private val rootEnv: Env = inContext(ictx):
      Env(defn.RootClass, EnvKind.Regular, CaptureSet.empty, null)
    private var curEnv = rootEnv

    /** Currently checked closures and their expected types, used for error reporting */
    private var openClosures: List[(Symbol, Type)] = Nil

    private val myCapturedVars: util.EqHashMap[Symbol, CaptureSet] = EqHashMap()

    /** A list of actions to perform at postCheck. The reason to defer these actions
     *  is that it is sometimes better for type inference to not constrain too early
     *  with a checkConformsExpr.
     */
    private var todoAtPostCheck = new mutable.ListBuffer[() => Unit]

    /** If `sym` is a class or method nested inside a term, a capture set variable representing
     *  the captured variables of the environment associated with `sym`.
     */
    def capturedVars(sym: Symbol)(using Context): CaptureSet =
      myCapturedVars.getOrElseUpdate(sym,
        if sym.ownersIterator.exists(_.isTerm)
        then CaptureSet.Var(sym.owner)
        else CaptureSet.empty)

    /** For all nested environments up to `limit` or a closed environment perform `op`,
     *  but skip environmenrts directly enclosing environments of kind ClosureResult.
     */
    def forallOuterEnvsUpTo(limit: Symbol)(op: Env => Unit)(using Context): Unit =
      def recur(env: Env, skip: Boolean): Unit =
        if env.isOpen && env.owner != limit then
          if !skip then op(env)
          if !env.isOutermost then
            var nextEnv = env.outer
            if env.owner.isConstructor then
              if nextEnv.owner != limit && !nextEnv.isOutermost then
                nextEnv = nextEnv.outer
            recur(nextEnv, skip = env.kind == EnvKind.ClosureResult)
      recur(curEnv, skip = false)

    /** A description where this environment comes from */
    private def provenance(env: Env)(using Context): String =
      val owner = env.owner
      if owner.isAnonymousFunction then
        val expected = openClosures
          .find(_._1 == owner)
          .map(_._2)
          .getOrElse(owner.info.toFunctionType())
        i"\nof an enclosing function literal with expected type $expected"
      else
        i"\nof the enclosing ${owner.showLocated}"


    /** Include `sym` in the capture sets of all enclosing environments nested in the
     *  the environment in which `sym` is defined.
     */
    def markFree(sym: Symbol, pos: SrcPos)(using Context): Unit =
      if sym.exists then
        val ref = sym.termRef
        if ref.isTracked then
          forallOuterEnvsUpTo(sym.enclosure): env =>
            capt.println(i"Mark $sym with cs ${ref.captureSet} free in ${env.owner}")
            checkElem(ref, env.captured, pos, provenance(env))

    /** Make sure (projected) `cs` is a subset of the capture sets of all enclosing
     *  environments. At each stage, only include references from `cs` that are outside
     *  the environment's owner
     */
    def markFree(cs: CaptureSet, pos: SrcPos)(using Context): Unit =
      if !cs.isAlwaysEmpty then
        forallOuterEnvsUpTo(ctx.owner.topLevelClass): env =>
          def isVisibleFromEnv(sym: Symbol) =
            (env.kind == EnvKind.NestedInOwner || env.owner != sym)
            && env.owner.isContainedIn(sym)
          val included = cs.filter:
            case ref: TermRef => isVisibleFromEnv(ref.symbol.owner)
            case ref: ThisType => isVisibleFromEnv(ref.cls)
            case _ => false
          capt.println(i"Include call capture $included in ${env.owner}")
          checkSubset(included, env.captured, pos, provenance(env))

    /** Include references captured by the called method in the current environment stack */
    def includeCallCaptures(sym: Symbol, pos: SrcPos)(using Context): Unit =
      if sym.exists && curEnv.isOpen then markFree(capturedVars(sym), pos)

    override def recheckIdent(tree: Ident, pt: Type)(using Context): Type =
      if tree.symbol.is(Method) then
        if tree.symbol.info.isParameterless then
          // there won't be an apply; need to include call captures now
          includeCallCaptures(tree.symbol, tree.srcPos)
      else
        markFree(tree.symbol, tree.srcPos)
      super.recheckIdent(tree, pt)

    /** A specialized implementation of the selection rule.
     *
     *  E |- f: f{ m: Cr R }^Cf
     *  -----------------------
     *  E |- f.m: R^C
     *
     *  The implementation picks as `C` one of `{f}` or `Cr`, depending on the
     *  outcome of a `mightSubcapture` test. It picks `{f}` if this might subcapture Cr
     *  and Cr otherwise.
     */
    override def recheckSelection(tree: Select, qualType: Type, name: Name, pt: Type)(using Context) = {
      def disambiguate(denot: Denotation): Denotation = denot match
        case MultiDenotation(denot1, denot2) =>
          // This case can arise when we try to merge multiple types that have different
          // capture sets on some part. For instance an asSeenFrom might produce
          // a bi-mapped capture set arising from a substition. Applying the same substitution
          // to the same type twice will nevertheless produce different capture sets which can
          // lead to a failure in disambiguation since neither alternative is better than the
          // other in a frozen constraint. An example test case is disambiguate-select.scala.
          // We address the problem by disambiguating while ignoring all capture sets as a fallback.
          withMode(Mode.IgnoreCaptures) {
            disambiguate(denot1).meet(disambiguate(denot2), qualType)
          }
        case _ => denot

      val selType = recheckSelection(tree, qualType, name, disambiguate)
      val selCs = selType.widen.captureSet
      if selCs.isAlwaysEmpty
          || selType.widen.isBoxedCapturing
          || qualType.isBoxedCapturing
          || pt == LhsProto
      then
        selType
      else
        val qualCs = qualType.captureSet
        capt.println(i"pick one of $qualType, ${selType.widen}, $qualCs, $selCs in $tree")
        if qualCs.mightSubcapture(selCs)
            && !selCs.mightSubcapture(qualCs)
            && !pt.stripCapturing.isInstanceOf[SingletonType]
        then
          selType.widen.stripCapturing.capturing(qualCs)
            .showing(i"alternate type for select $tree: $selType --> $result, $qualCs / $selCs", capt)
        else
          selType
    }//.showing(i"recheck sel $tree, $qualType = $result")

    /** A specialized implementation of the apply rule.
     *
     *  E |- f: Ra ->Cf Rr^Cr
     *  E |- a: Ra^Ca
     *  ---------------------
     *  E |- f a: Rr^C
     *
     *  The implementation picks as `C` one of `{f, a}` or `Cr`, depending on the
     *  outcome of a `mightSubcapture` test. It picks `{f, a}` if this might subcapture Cr
     *  and Cr otherwise.
     */
    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val meth = tree.fun.symbol
      includeCallCaptures(meth, tree.srcPos)

      // Unsafe box/unbox handlng, only for versions < 3.3
      def mapArgUsing(f: Type => Type) =
        val arg :: Nil = tree.args: @unchecked
        val argType0 = f(recheckStart(arg, pt))
        val argType = super.recheckFinish(argType0, arg, pt)
        super.recheckFinish(argType, tree, pt)

      if meth == defn.Caps_unsafeAssumePure then
        val arg :: Nil = tree.args: @unchecked
        val argType0 = recheck(arg, pt.capturing(CaptureSet.universal))
        val argType =
          if argType0.captureSet.isAlwaysEmpty then argType0
          else argType0.widen.stripCapturing
        capt.println(i"rechecking $arg with $pt: $argType")
        super.recheckFinish(argType, tree, pt)
      else if meth == defn.Caps_unsafeBox then
        mapArgUsing(_.forceBoxStatus(true))
      else if meth == defn.Caps_unsafeUnbox then
        mapArgUsing(_.forceBoxStatus(false))
      else if meth == defn.Caps_unsafeBoxFunArg then
        def forceBox(tp: Type): Type = tp.strippedDealias match
          case defn.FunctionOf(paramtpe :: Nil, restpe, isContextual) =>
            defn.FunctionOf(paramtpe.forceBoxStatus(true) :: Nil, restpe, isContextual)
          case tp @ RefinedType(parent, rname, rinfo: MethodType) =>
            tp.derivedRefinedType(parent, rname,
              rinfo.derivedLambdaType(
                paramInfos = rinfo.paramInfos.map(_.forceBoxStatus(true))))
          case tp @ CapturingType(parent, refs) =>
            tp.derivedCapturingType(forceBox(parent), refs)
        mapArgUsing(forceBox)
      else
        super.recheckApply(tree, pt) match
          case appType @ CapturingType(appType1, refs) =>
            tree.fun match
              case Select(qual, _)
              if !tree.fun.symbol.isConstructor
                  && !qual.tpe.isBoxedCapturing
                  && !tree.args.exists(_.tpe.isBoxedCapturing)
                  && qual.tpe.captureSet.mightSubcapture(refs)
                  && tree.args.forall(_.tpe.captureSet.mightSubcapture(refs))
              =>
                val callCaptures = tree.args.foldLeft(qual.tpe.captureSet): (cs, arg) =>
                  cs ++ arg.tpe.captureSet
                appType.derivedCapturingType(appType1, callCaptures)
                  .showing(i"narrow $tree: $appType, refs = $refs, qual = ${qual.tpe.captureSet} --> $result", capt)
              case _ => appType
          case appType => appType
    end recheckApply

    private def isDistinct(xs: List[Type]): Boolean = xs match
      case x :: xs1 => xs1.isEmpty || !xs1.contains(x) && isDistinct(xs1)
      case Nil => true

    /** Handle an application of method `sym` with type `mt` to arguments of types `argTypes`.
     *  This means:
     *   - Instantiate result type with actual arguments
     *   - If call is to a constructor:
     *      - remember types of arguments corresponding to tracked
     *        parameters in refinements.
     *      - add capture set of instantiated class to capture set of result type.
     *  If all argument types are mutually different trackable capture references, use a BiTypeMap,
     *  since that is more precise. Otherwise use a normal idempotent map, which might lose information
     *  in the case where the result type contains captureset variables that are further
     *  constrained afterwards.
     */
    override def instantiate(mt: MethodType, argTypes: List[Type], sym: Symbol)(using Context): Type =
      val ownType =
        if !mt.isResultDependent then
          mt.resType
        else if argTypes.forall(_.isTrackableRef) && isDistinct(argTypes) then
          SubstParamsBiMap(mt, argTypes)(mt.resType)
        else
          SubstParamsMap(mt, argTypes)(mt.resType)

      if sym.isConstructor then
        val cls = sym.owner.asClass

        /** First half of result pair:
         *  Refine the type of a constructor call `new C(t_1, ..., t_n)`
         *  to C{val x_1: T_1, ..., x_m: T_m} where x_1, ..., x_m are the tracked
         *  parameters of C and T_1, ..., T_m are the types of the corresponding arguments.
         *
         *  Second half: union of all capture sets of arguments to tracked parameters.
         */
        def addParamArgRefinements(core: Type, initCs: CaptureSet): (Type, CaptureSet) =
          var refined: Type = core
          var allCaptures: CaptureSet = if core.hasUniversalCapability
            then CaptureSet.universal else initCs
          for (getterName, argType) <- mt.paramNames.lazyZip(argTypes) do
            val getter = cls.info.member(getterName).suchThat(_.is(ParamAccessor)).symbol
            if getter.termRef.isTracked && !getter.is(Private) then
              refined = RefinedType(refined, getterName, argType)
              allCaptures ++= argType.captureSet
          (refined, allCaptures)

        /** Augment result type of constructor with refinements and captures.
         *  @param  core   The result type of the constructor
         *  @param  initCs The initial capture set to add, not yet counting capture sets from arguments
         */
        def augmentConstructorType(core: Type, initCs: CaptureSet): Type = core match
          case core: MethodType =>
            // more parameters to follow; augment result type
            core.derivedLambdaType(resType = augmentConstructorType(core.resType, initCs))
          case CapturingType(parent, refs) =>
            // can happen for curried constructors if instantiate of a previous step
            // added capture set to result.
            augmentConstructorType(parent, initCs ++ refs)
          case _ =>
            val (refined, cs) = addParamArgRefinements(core, initCs)
            refined.capturing(cs)

        augmentConstructorType(ownType, capturedVars(cls) ++ capturedVars(sym))
          .showing(i"constr type $mt with $argTypes%, % in $cls = $result", capt)
      else ownType
    end instantiate

    override def recheckTypeApply(tree: TypeApply, pt: Type)(using Context): Type =
      if ccConfig.allowUniversalInBoxed then
        val TypeApply(fn, args) = tree
        val polyType = atPhase(thisPhase.prev):
          fn.tpe.widen.asInstanceOf[TypeLambda]
        for case (arg: TypeTree, formal, pname) <- args.lazyZip(polyType.paramRefs).lazyZip((polyType.paramNames)) do
          if !tree.symbol.isTypeTestOrCast then
            def where = if fn.symbol.exists then i" in an argument of ${fn.symbol}" else ""
            disallowRootCapabilitiesIn(arg.knownType, NoSymbol,
              i"Sealed type variable $pname", "be instantiated to",
              i"This is often caused by a local capability$where\nleaking as part of its result.",
              tree.srcPos)
      super.recheckTypeApply(tree, pt)

    override def recheckClosure(tree: Closure, pt: Type, forceDependent: Boolean)(using Context): Type =
      val cs = capturedVars(tree.meth.symbol)
      capt.println(i"typing closure $tree with cvs $cs")
      super.recheckClosure(tree, pt, forceDependent).capturing(cs)
        .showing(i"rechecked closure $tree / $pt = $result", capt)

    override def recheckClosureBlock(mdef: DefDef, expr: Closure, pt: Type)(using Context): Type =
      mdef.rhs match
        case rhs @ closure(_, _, _) =>
          // In a curried closure `x => y => e` don't leak capabilities retained by
          // the second closure `y => e` into the first one. This is an approximation
          // of the CC rule which says that a closure contributes captures to its
          // environment only if a let-bound reference to the closure is used.
          mdef.rhs.putAttachment(ClosureBodyValue, ())
        case _ =>

      // Constrain closure's parameters and result from the expected type before
      // rechecking the body.
      openClosures = (mdef.symbol, pt) :: openClosures
      try
        val res = recheckClosure(expr, pt, forceDependent = true)
        if !isEtaExpansion(mdef) then
          // If closure is an eta expanded method reference it's better to not constrain
          // its internals early since that would give error messages in generated code
          // which are less intelligible.
          // Example is the line `a = x` in neg-custom-args/captures/vars.scala.
          // For all other closures, early constraints are preferred since they
          // give more localized error messages.
          checkConformsExpr(res, pt, expr)
        recheckDef(mdef, mdef.symbol)
        res
      finally
        openClosures = openClosures.tail
    end recheckClosureBlock

    /** Maps mutable variables to the symbols that capture them (in the
     *  CheckCaptures sense, i.e. symbol is referred to from a different method
     *  than the one it is defined in).
     */
    private val capturedBy = util.HashMap[Symbol, Symbol]()

    /** Maps anonymous functions appearing as function arguments to
     *  the function that is called.
     */
    private val anonFunCallee = util.HashMap[Symbol, Symbol]()

    /** Populates `capturedBy` and `anonFunCallee`. Called by `checkUnit`.
     */
    private def collectCapturedMutVars(using Context) = new TreeTraverser:
      def traverse(tree: Tree)(using Context) = tree match
        case id: Ident =>
          val sym = id.symbol
          if sym.is(Mutable, butNot = Method) && sym.owner.isTerm then
            val enclMeth = ctx.owner.enclosingMethod
            if sym.enclosingMethod != enclMeth then
              capturedBy(sym) = enclMeth
        case Apply(fn, args) =>
          for case closureDef(mdef) <- args do
            anonFunCallee(mdef.symbol) = fn.symbol
          traverseChildren(tree)
        case Inlined(_, bindings, expansion) =>
          traverse(bindings)
          traverse(expansion)
        case mdef: DefDef =>
          if !mdef.symbol.isInlineMethod then traverseChildren(tree)
        case _ =>
          traverseChildren(tree)

    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Type =
      try
        if sym.is(Module) then sym.info // Modules are checked by checking the module class
        else
          if sym.is(Mutable) && !sym.hasAnnotation(defn.UncheckedCapturesAnnot) then
            val (carrier, addendum) = capturedBy.get(sym) match
              case Some(encl) =>
                val enclStr =
                  if encl.isAnonymousFunction then
                    val location = anonFunCallee.get(encl) match
                      case Some(meth) if meth.exists => i" argument in a call to $meth"
                      case _ => ""
                    s"an anonymous function$location"
                  else encl.show
                (NoSymbol, i"\nNote that $sym does not count as local since it is captured by $enclStr")
              case _ =>
                (sym, "")
            disallowRootCapabilitiesIn(
              tree.tpt.knownType, carrier, i"Mutable $sym", "have type", addendum, sym.srcPos)
          checkInferredResult(super.recheckValDef(tree, sym), tree)
      finally
        if !sym.is(Param) then
          // Parameters with inferred types belong to anonymous methods. We need to wait
          // for more info from the context, so we cannot interpolate. Note that we cannot
          // expect to have all necessary info available at the point where the anonymous
          // function is compiled since we do not propagate expected types into blocks.
          interpolateVarsIn(tree.tpt)

    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Type =
      if Synthetics.isExcluded(sym) then sym.info
      else
        val saved = curEnv
        val localSet = capturedVars(sym)
        if !localSet.isAlwaysEmpty then
          curEnv = Env(sym, EnvKind.Regular, localSet, curEnv)
        try checkInferredResult(super.recheckDefDef(tree, sym), tree)
        finally
          if !sym.isAnonymousFunction then
            // Anonymous functions propagate their type to the enclosing environment
            // so it is not in general sound to interpolate their types.
            interpolateVarsIn(tree.tpt)
          curEnv = saved

    /** If val or def definition with inferred (result) type is visible
     *  in other compilation units, check that the actual inferred type
     *  conforms to the expected type where all inferred capture sets are dropped.
     *  This ensures that if files compile separately, they will also compile
     *  in a joint compilation.
     */
    def checkInferredResult(tp: Type, tree: ValOrDefDef)(using Context): Type =
      val sym = tree.symbol

      def canUseInferred =    // If canUseInferred is false, all capturing types in the type of `sym` need to be given explicitly
        sym.is(Private)                   // private symbols can always have inferred types
        || sym.name.is(DefaultGetterName) // default getters are exempted since otherwise it would be
                                          // too annoying. This is a hole since a defualt getter's result type
                                          // might leak into a type variable.
        ||                                // non-local symbols cannot have inferred types since external capture types are not inferred
          sym.isLocalToCompilationUnit    // local symbols still need explicit types if
          && !sym.owner.is(Trait)         // they are defined in a trait, since we do OverridingPairs checking before capture inference

      def addenda(expected: Type) = new Addenda:
        override def toAdd(using Context) =
          def result = if tree.isInstanceOf[ValDef] then"" else " result"
          i"""
           |
           |Note that the expected type $expected
           |is the previously inferred$result type of $sym
           |which is also the type seen in separately compiled sources.
           |The new inferred type $tp
           |must conform to this type.""" :: Nil

      tree.tpt match
        case tpt: InferredTypeTree if !canUseInferred =>
          val expected = tpt.tpe.dropAllRetains
          todoAtPostCheck += (() => checkConformsExpr(tp, expected, tree.rhs, addenda(expected)))
        case _ =>
      tp
    end checkInferredResult

    /** Class-specific capture set relations:
     *   1. The capture set of a class includes the capture sets of its parents.
     *   2. The capture set of the self type of a class includes the capture set of the class.
     *   3. The capture set of the self type of a class includes the capture set of every class parameter,
     *      unless the parameter is marked @constructorOnly.
     *   4. If the class extends a pure base class, the capture set of the self type must be empty.
     */
    override def recheckClassDef(tree: TypeDef, impl: Template, cls: ClassSymbol)(using Context): Type =
      val saved = curEnv
      val localSet = capturedVars(cls)
      for parent <- impl.parents do // (1)
        checkSubset(capturedVars(parent.tpe.classSymbol), localSet, parent.srcPos,
          i"\nof the references allowed to be captured by $cls")
      if !localSet.isAlwaysEmpty then
        curEnv = Env(cls, EnvKind.Regular, localSet, curEnv)
      try
        val thisSet = cls.classInfo.selfType.captureSet.withDescription(i"of the self type of $cls")
        checkSubset(localSet, thisSet, tree.srcPos) // (2)
        for param <- cls.paramGetters do
          if !param.hasAnnotation(defn.ConstructorOnlyAnnot) then
            checkSubset(param.termRef.captureSet, thisSet, param.srcPos) // (3)
        for pureBase <- cls.pureBaseClass do // (4)
          def selfType = impl.body
            .collect:
              case TypeDef(tpnme.SELF, rhs) => rhs
            .headOption
            .getOrElse(tree)
            .orElse(tree)
          checkSubset(thisSet,
            CaptureSet.empty.withDescription(i"of pure base class $pureBase"),
            selfType.srcPos, cs1description = " captured by this self type")
        super.recheckClassDef(tree, impl, cls)
      finally
        curEnv = saved

    /** If type is of the form `T @requiresCapability(x)`,
     *  mark `x` as free in the current environment. This is used to require the
     *  correct `CanThrow` capability when encountering a `throw`.
     */
    override def recheckTyped(tree: Typed)(using Context): Type =
      tree.tpt.tpe match
        case AnnotatedType(_, annot) if annot.symbol == defn.RequiresCapabilityAnnot =>
          annot.tree match
            case Apply(_, cap :: Nil) =>
              markFree(cap.symbol, tree.srcPos)
            case _ =>
        case _ =>
      super.recheckTyped(tree)

    override def recheckTry(tree: Try, pt: Type)(using Context): Type =
      val tp = super.recheckTry(tree, pt)
      if ccConfig.allowUniversalInBoxed && Feature.enabled(Feature.saferExceptions) then
        disallowRootCapabilitiesIn(tp, ctx.owner,
          "result of `try`", "have type",
          "This is often caused by a locally generated exception capability leaking as part of its result.",
          tree.srcPos)
      tp

    /* Currently not needed, since capture checking takes place after ElimByName.
     * Keep around in case we need to get back to it
    def recheckByNameArg(tree: Tree, pt: Type)(using Context): Type =
      val closureDef(mdef) = tree: @unchecked
      val arg = mdef.rhs
      val localSet = CaptureSet.Var()
      curEnv = Env(mdef.symbol, localSet, isBoxed = false, curEnv)
      val result =
        try
          inContext(ctx.withOwner(mdef.symbol)) {
            recheckStart(arg, pt).capturing(localSet)
          }
        finally curEnv = curEnv.outer
      recheckFinish(result, arg, pt)
    */

    /** If expected type `pt` is boxed and the tree is a function or a reference,
     *  don't propagate free variables.
     *  Otherwise, if the result type is boxed, simulate an unboxing by
     *  adding all references in the boxed capture set to the current environment.
     */
    override def recheck(tree: Tree, pt: Type = WildcardType)(using Context): Type =
      val saved = curEnv
      tree match
        case _: RefTree | closureDef(_) if pt.isBoxedCapturing =>
          curEnv = Env(curEnv.owner, EnvKind.Boxed, CaptureSet.Var(curEnv.owner), curEnv)
        case _ if tree.hasAttachment(ClosureBodyValue) =>
          curEnv = Env(curEnv.owner, EnvKind.ClosureResult, CaptureSet.Var(curEnv.owner), curEnv)
        case _ =>
      val res =
        try
          if capt eq noPrinter then
            super.recheck(tree, pt)
          else
            trace.force(i"rechecking $tree with pt = $pt", recheckr, show = true):
              super.recheck(tree, pt)
        catch case ex: NoCommonRoot =>
          report.error(ex.getMessage.nn)
          tree.tpe
        finally curEnv = saved
      if tree.isTerm && !pt.isBoxedCapturing then
        markFree(res.boxedCaptureSet, tree.srcPos)
      res

    override def recheckFinish(tpe: Type, tree: Tree, pt: Type)(using Context): Type =
      def needsUniversalCheck = tree match
        case _: RefTree | _: Apply | _: TypeApply => tree.symbol.unboxesResult
        case _: Try => true
        case _ => false
      def checkNotUniversal(tp: Type): Unit = tp.widenDealias match
        case wtp @ CapturingType(parent, refs) =>
          refs.disallowRootCapability { () =>
            report.error(
              em"""The expression's type $wtp is not allowed to capture the root capability `cap`.
                  |This usually means that a capability persists longer than its allowed lifetime.""",
              tree.srcPos)
          }
          checkNotUniversal(parent)
        case _ =>
      if !ccConfig.allowUniversalInBoxed && needsUniversalCheck then
        checkNotUniversal(tpe)
      super.recheckFinish(tpe, tree, pt)
    end recheckFinish

    // ------------------ Adaptation -------------------------------------
    //
    // Adaptations before checking conformance of actual vs expected:
    //
    //   - Convert function to dependent function if expected type is a dependent function type
    //     (c.f. alignDependentFunction).
    //   - Relax expected capture set containing `this.type`s by adding references only
    //     accessible through those types (c.f. addOuterRefs, also #14930 for a discussion).
    //   - Adapt box status and environment capture sets by simulating box/unbox operations.
    //   - Instantiate covariant occurrenves of `cap` in actual to reach capabilities.

    private inline val debugSuccesses = false

    /** Massage `actual` and `expected` types before checking conformance.
     *  Massaging is done by the methods following this one:
     *   - align dependent function types and add outer references in the expected type
     *   - adapt boxing in the actual type
     *  If the resulting types are not compatible, try again with an actual type
     *  where local capture roots are instantiated to root variables.
     */
    override def checkConformsExpr(actual: Type, expected: Type, tree: Tree, addenda: Addenda)(using Context): Type =
      var expected1 = alignDependentFunction(expected, actual.stripCapturing)
      val actualBoxed = adaptBoxed(actual, expected1, tree.srcPos)
      //println(i"check conforms $actualBoxed <<< $expected1")

      if actualBoxed eq actual then
        // Only `addOuterRefs` when there is no box adaptation
        expected1 = addOuterRefs(expected1, actual)
      if isCompatible(actualBoxed, expected1) then
        if debugSuccesses then tree match
            case Ident(_) =>
              println(i"SUCCESS $tree:\n${TypeComparer.explained(_.isSubType(actual, expected))}")
            case _ =>
        actualBoxed
      else
        capt.println(i"conforms failed for ${tree}: $actual vs $expected")
        err.typeMismatch(tree.withType(actualBoxed), expected1, addenda ++ CaptureSet.levelErrors)
        actual
    end checkConformsExpr

    /** Turn `expected` into a dependent function when `actual` is dependent. */
    private def alignDependentFunction(expected: Type, actual: Type)(using Context): Type =
      def recur(expected: Type): Type = expected.dealias match
        case expected0 @ CapturingType(eparent, refs) =>
          val eparent1 = recur(eparent)
          if eparent1 eq eparent then expected
          else CapturingType(eparent1, refs, boxed = expected0.isBoxed)
        case expected @ defn.FunctionOf(args, resultType, isContextual)
        if defn.isNonRefinedFunction(expected) =>
          actual match
            case RefinedType(parent, nme.apply, rinfo: MethodType)
            if defn.isFunctionNType(actual) =>
              depFun(args, resultType, isContextual, rinfo.paramNames)
            case _ => expected
        case _ => expected
      recur(expected)

    /** For the expected type, implement the rule outlined in #14390:
     *   - when checking an expression `a: Ta^Ca` against an expected type `Te^Ce`,
     *   - where the capture set `Ce` contains Cls.this,
     *   - and where all method definitions enclosing `a` inside class `Cls`
     *     have only pure parameters,
     *   - add to `Ce` all references to variables or this-references in `Ca`
     *     that are outside `Cls`. These are all accessed through `Cls.this`,
     *     so we can assume they are already accounted for by `Ce` and adding
     *     them explicitly to `Ce` changes nothing.
     */
    private def addOuterRefs(expected: Type, actual: Type)(using Context): Type =

      def isPure(info: Type): Boolean = info match
        case info: PolyType => isPure(info.resType)
        case info: MethodType => info.paramInfos.forall(_.captureSet.isAlwaysEmpty) && isPure(info.resType)
        case _ => true

      def isPureContext(owner: Symbol, limit: Symbol): Boolean =
        if owner == limit then true
        else if !owner.exists then false
        else isPure(owner.info) && isPureContext(owner.owner, limit)

      // Augment expeced capture set `erefs` by all references in actual capture
      // set `arefs` that are outside some `this.type` reference in `erefs`
      def augment(erefs: CaptureSet, arefs: CaptureSet): CaptureSet =
        (erefs /: erefs.elems): (erefs, eref) =>
          eref match
            case eref: ThisType if isPureContext(ctx.owner, eref.cls) =>
              erefs ++ arefs.filter {
                case aref: TermRef => eref.cls.isProperlyContainedIn(aref.symbol.owner)
                case aref: ThisType => eref.cls.isProperlyContainedIn(aref.cls)
                case _ => false
              }
            case _ =>
              erefs

      expected match
        case CapturingType(ecore, erefs) =>
          val erefs1 = augment(erefs, actual.captureSet)
          if erefs1 ne erefs then
            capt.println(i"augmented $expected from ${actual.captureSet} --> $erefs1")
          expected.derivedCapturingType(ecore, erefs1)
        case _ =>
          expected
    end addOuterRefs


    /** Adapt `actual` type to `expected` type by inserting boxing and unboxing conversions
     *
     *  @param alwaysConst  always make capture set variables constant after adaptation
     */
    def adaptBoxed(actual: Type, expected: Type, pos: SrcPos, alwaysConst: Boolean = false)(using Context): Type =

      inline def inNestedEnv[T](boxed: Boolean)(op: => T): T =
        val saved = curEnv
        curEnv = Env(curEnv.owner, EnvKind.NestedInOwner, CaptureSet.Var(curEnv.owner), if boxed then null else curEnv)
        try op
        finally curEnv = saved

      /** Adapt function type `actual`, which is `aargs -> ares` (possibly with dependencies)
       *  to `expected` type.
       *  It returns the adapted type along with a capture set consisting of the references
       *  that were additionally captured during adaptation.
       *   @param reconstruct  how to rebuild the adapted function type
       */
      def adaptFun(actual: Type, aargs: List[Type], ares: Type, expected: Type,
          covariant: Boolean, boxed: Boolean,
          reconstruct: (List[Type], Type) => Type): (Type, CaptureSet) =
        inNestedEnv(boxed):
          val (eargs, eres) = expected.dealias.stripCapturing match
            case defn.FunctionOf(eargs, eres, _) => (eargs, eres)
            case expected: MethodType => (expected.paramInfos, expected.resType)
            case expected @ RefinedType(_, _, rinfo: MethodType) if defn.isFunctionNType(expected) => (rinfo.paramInfos, rinfo.resType)
            case _ => (aargs.map(_ => WildcardType), WildcardType)
          val aargs1 = aargs.zipWithConserve(eargs) { (aarg, earg) => adapt(aarg, earg, !covariant) }
          val ares1 = adapt(ares, eres, covariant)

          val resTp =
            if (ares1 eq ares) && (aargs1 eq aargs) then actual
            else reconstruct(aargs1, ares1)

          (resTp, curEnv.captured)
      end adaptFun

      /** Adapt type function type `actual` to the expected type.
       *  @see [[adaptFun]]
       */
      def adaptTypeFun(
          actual: Type, ares: Type, expected: Type,
          covariant: Boolean, boxed: Boolean,
          reconstruct: Type => Type): (Type, CaptureSet) =
        inNestedEnv(boxed):
          val eres = expected.dealias.stripCapturing match
            case defn.PolyFunctionOf(rinfo: PolyType) => rinfo.resType
            case expected: PolyType => expected.resType
            case _ => WildcardType

          val ares1 = adapt(ares, eres, covariant)

          val resTp =
            if ares1 eq ares then actual
            else reconstruct(ares1)

          (resTp, curEnv.captured)
      end adaptTypeFun

      def adaptInfo(actual: Type, expected: Type, covariant: Boolean): String =
        val arrow = if covariant then "~~>" else "<~~"
        i"adapting $actual $arrow $expected"

      def adapt(actual: Type, expected: Type, covariant: Boolean): Type = trace(adaptInfo(actual, expected, covariant), recheckr, show = true) {
        if expected.isInstanceOf[WildcardType] then actual
        else
          // Decompose the actual type into the inner shape type, the capture set and the box status
          val styp = if actual.isFromJavaObject then actual else actual.stripCapturing
          val cs = actual.captureSet
          val boxed = actual.isBoxedCapturing

          // A box/unbox should be inserted, if the actual box status mismatches with the expectation
          val needsAdaptation = boxed != expected.isBoxedCapturing
          // Whether to insert a box or an unbox?
          val insertBox = needsAdaptation && covariant != boxed

          // Adapt the inner shape type: get the adapted shape type, and the capture set leaked during adaptation
          val (styp1, leaked) = styp match {
            case actual @ AppliedType(tycon, args) if defn.isNonRefinedFunction(actual) =>
              adaptFun(actual, args.init, args.last, expected, covariant, insertBox,
                  (aargs1, ares1) => actual.derivedAppliedType(tycon, aargs1 :+ ares1))
            case actual @ defn.RefinedFunctionOf(rinfo: MethodType) =>
              // TODO Find a way to combine handling of generic and dependent function types (here and elsewhere)
              adaptFun(actual, rinfo.paramInfos, rinfo.resType, expected, covariant, insertBox,
                (aargs1, ares1) =>
                  rinfo.derivedLambdaType(paramInfos = aargs1, resType = ares1)
                    .toFunctionType(alwaysDependent = true))
            case actual: MethodType =>
              adaptFun(actual, actual.paramInfos, actual.resType, expected, covariant, insertBox,
                (aargs1, ares1) =>
                  actual.derivedLambdaType(paramInfos = aargs1, resType = ares1))
            case actual @ defn.RefinedFunctionOf(rinfo: PolyType) =>
              adaptTypeFun(actual, rinfo.resType, expected, covariant, insertBox,
                ares1 =>
                  val rinfo1 = rinfo.derivedLambdaType(rinfo.paramNames, rinfo.paramInfos, ares1)
                  val actual1 = actual.derivedRefinedType(refinedInfo = rinfo1)
                  actual1
              )
            case _ =>
              (styp, CaptureSet())
          }

          // Capture set of the term after adaptation
          val cs1 =
            if covariant then cs ++ leaked
            else
              if !leaked.subCaptures(cs, frozen = false).isOK then
                report.error(
                  em"""$expected cannot be box-converted to $actual
                      |since the additional capture set $leaked resulted from box conversion is not allowed in $actual""", pos)
              cs

          // Compute the adapted type
          def adaptedType(resultBoxed: Boolean) =
            if (styp1 eq styp) && leaked.isAlwaysEmpty && boxed == resultBoxed then actual
            else styp1.capturing(if alwaysConst then CaptureSet(cs1.elems) else cs1).forceBoxStatus(resultBoxed)

          if needsAdaptation then
            val criticalSet =          // the set which is not allowed to have `cap`
              if covariant then cs1    // can't box with `cap`
              else expected.captureSet // can't unbox with `cap`
            if criticalSet.isUniversal && expected.isValueType && !ccConfig.allowUniversalInBoxed then
              // We can't box/unbox the universal capability. Leave `actual` as it is
              // so we get an error in checkConforms. This tends to give better error
              // messages than disallowing the root capability in `criticalSet`.
              if ctx.settings.YccDebug.value then
                println(i"cannot box/unbox $actual vs $expected")
              actual
            else
              if !ccConfig.allowUniversalInBoxed then
                // Disallow future addition of `cap` to `criticalSet`.
                criticalSet.disallowRootCapability { () =>
                  report.error(
                    em"""$actual cannot be box-converted to $expected
                        |since one of their capture sets contains the root capability `cap`""",
                  pos)
                }
              if !insertBox then  // unboxing
                markFree(criticalSet, pos)
              adaptedType(!boxed)
          else
            adaptedType(boxed)
      }

      if expected == LhsProto || expected.isSingleton && actual.isSingleton then
        actual
      else
        var actualw = actual.widenDealias
        actual match
          case ref: CaptureRef if ref.isTracked =>
            actualw match
              case CapturingType(p, refs) if ref.singletonCaptureSet.mightSubcapture(refs) =>
                actualw = actualw.derivedCapturingType(p, ref.singletonCaptureSet)
                  .showing(i"improve $actualw to $result", capt)
                  // given `a: T^C`, improve `T^C` to `T^{a}`
              case _ =>
          case _ =>
        val adapted = adapt(actualw.withReachCaptures(actual), expected, covariant = true)
        if adapted ne actualw then
          capt.println(i"adapt boxed $actual vs $expected ===> $adapted")
          adapted
        else actual
    end adaptBoxed

    /** Check overrides again, taking capture sets into account.
    *  TODO: Can we avoid doing overrides checks twice?
    *  We need to do them here since only at this phase CaptureTypes are relevant
    *  But maybe we can then elide the check during the RefChecks phase under captureChecking?
    */
    def checkOverrides = new TreeTraverser:
      class OverridingPairsCheckerCC(clazz: ClassSymbol, self: Type, srcPos: SrcPos)(using Context) extends OverridingPairsChecker(clazz, self):
        /** Check subtype with box adaptation.
        *  This function is passed to RefChecks to check the compatibility of overriding pairs.
        *  @param sym  symbol of the field definition that is being checked
        */
        override def checkSubType(actual: Type, expected: Type)(using Context): Boolean =
          val expected1 = alignDependentFunction(addOuterRefs(expected, actual), actual.stripCapturing)
          val actual1 =
            val saved = curEnv
            try
              curEnv = Env(clazz, EnvKind.NestedInOwner, capturedVars(clazz), outer0 = curEnv)
              val adapted = adaptBoxed(actual, expected1, srcPos, alwaysConst = true)
              actual match
                case _: MethodType =>
                  // We remove the capture set resulted from box adaptation for method types,
                  // since class methods are always treated as pure, and their captured variables
                  // are charged to the capture set of the class (which is already done during
                  // box adaptation).
                  adapted.stripCapturing
                case _ => adapted
            finally curEnv = saved
          actual1 frozen_<:< expected1

        override def needsCheck(overriding: Symbol, overridden: Symbol)(using Context): Boolean =
          !setup.isPreCC(overriding) && !setup.isPreCC(overridden)

        override def checkInheritedTraitParameters: Boolean = false
      end OverridingPairsCheckerCC

      def traverse(t: Tree)(using Context) =
        t match
          case t: Template =>
            checkAllOverrides(ctx.owner.asClass, OverridingPairsCheckerCC(_, _, t))
          case _ =>
        traverseChildren(t)

    /** Check a ValDef or DefDef as an action performed in a completer. Since
     *  these checks can appear out of order, we need to firsty create the correct
     *  environment for checking the definition.
     */
    def completeDef(tree: ValOrDefDef, sym: Symbol)(using Context): Type =
      val saved = curEnv
      try
        // Setup environment to reflect the new owner.
        val envForOwner: Map[Symbol, Env] = curEnv.outersIterator
          .takeWhile(e => !capturedVars(e.owner).isAlwaysEmpty) // no refs can leak beyind this point
          .map(e => (e.owner, e))
          .toMap
        def restoreEnvFor(sym: Symbol): Env =
          val localSet = capturedVars(sym)
          if localSet.isAlwaysEmpty then rootEnv
          else envForOwner.get(sym) match
            case Some(e) => e
            case None => Env(sym, EnvKind.Regular, localSet, restoreEnvFor(sym.owner))
        curEnv = restoreEnvFor(sym.owner)
        capt.println(i"Complete $sym in ${curEnv.outersIterator.toList.map(_.owner)}")
        recheckDef(tree, sym)
      finally
        curEnv = saved

    private val setup: SetupAPI = thisPhase.prev.asInstanceOf[Setup]

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      setup.setupUnit(unit.tpdTree, completeDef)
      collectCapturedMutVars.traverse(unit.tpdTree)

      if ctx.settings.YccPrintSetup.value then
        val echoHeader = "[[syntax tree at end of cc setup]]"
        val treeString = show(unit.tpdTree)
        report.echo(s"$echoHeader\n$treeString\n")

      withCaptureSetsExplained:
        super.checkUnit(unit)
        checkOverrides.traverse(unit.tpdTree)
        checkSelfTypes(unit.tpdTree)
        postCheck(unit.tpdTree)
        if ctx.settings.YccDebug.value then
          show(unit.tpdTree) // this does not print tree, but makes its variables visible for dependency printing

    /** Check that self types of subclasses conform to self types of super classes.
     *  (See comment below how this is achieved). The check assumes that classes
     *  without an explicit self type have the universal capture set `{cap}` on the
     *  self type. If a class without explicit self type is not `effectivelySealed`
     *  it is checked that the inferred self type is universal, in order to assure
     *  that joint and separate compilation give the same result.
     */
    def checkSelfTypes(unit: tpd.Tree)(using Context): Unit =
      val parentTrees = mutable.HashMap[Symbol, List[Tree]]()
      unit.foreachSubTree {
        case cdef @ TypeDef(_, impl: Template) => parentTrees(cdef.symbol) = impl.parents
        case _ =>
      }
      // Perform self type checking. The problem here is that `checkParents` compares a
      // self type of a subclass with the result of an asSeenFrom of the self type of the
      // superclass. That's no good. We need to constrain the original superclass self type
      // capture set, not the set mapped by asSeenFrom.
      //
      // Instead, we proceed from parent classes to child classes. For every class
      // we first check its parents, and then interpolate the self type to an
      // upper approximation that satisfies all constraints on its capture set.
      // That means all capture sets of parent self types are constants, so mapping
      // them with asSeenFrom is OK.
      while parentTrees.nonEmpty do
        val roots = parentTrees.keysIterator.filter {
          cls => !parentTrees(cls).exists(ptree => parentTrees.contains(ptree.tpe.classSymbol))
        }
        assert(roots.nonEmpty)
        for case root: ClassSymbol <- roots do
          inContext(ctx.fresh.setOwner(root)):
            checkSelfAgainstParents(root, root.baseClasses)
            val selfType = root.asClass.classInfo.selfType
            interpolator(startingVariance = -1).traverse(selfType)
            selfType match
              case CapturingType(_, refs: CaptureSet.Var)
              if !root.isEffectivelySealed
                  && !refs.elems.exists(_.isRootCapability)
                  && !root.matchesExplicitRefsInBaseClass(refs)
              =>
                // Forbid inferred self types unless they are already implied by an explicit
                // self type in a parent.
                report.error(
                  em"""$root needs an explicitly declared self type since its
                      |inferred self type $selfType
                      |is not visible in other compilation units that define subclasses.""",
                  root.srcPos)
              case _ =>
            parentTrees -= root
            capt.println(i"checked $root with $selfType")
    end checkSelfTypes

    /** Heal ill-formed capture sets in the type parameter.
     *
     *  We can push parameter refs into a capture set in type parameters
     *  that this type parameter can't see.
     *  For example, when capture checking the following expression:
     *
     *    def usingLogFile[T](op: File^ => T): T = ...
     *
     *    usingLogFile[box ?1 () -> Unit] { (f: File^) => () => { f.write(0) } }
     *
     *  We may propagate `f` into ?1, making ?1 ill-formed.
     *  This also causes soundness issues, since `f` in ?1 should be widened to `cap`,
     *  giving rise to an error that `cap` cannot be included in a boxed capture set.
     *
     *  To solve this, we still allow ?1 to capture parameter refs like `f`, but
     *  compensate this by pushing the widened capture set of `f` into ?1.
     *  This solves the soundness issue caused by the ill-formness of ?1.
     */
    private def healTypeParam(tree: Tree, paramName: TypeName, meth: Symbol)(using Context): Unit =
      val checker = new TypeTraverser:
        private var allowed: SimpleIdentitySet[TermParamRef] = SimpleIdentitySet.empty

        private def isAllowed(ref: CaptureRef): Boolean = ref match
          case ref: TermParamRef => allowed.contains(ref)
          case _ => true

        private def healCaptureSet(cs: CaptureSet): Unit =
          cs.ensureWellformed: elem =>
            ctx ?=>
              var seen = new util.HashSet[CaptureRef]
              def recur(ref: CaptureRef): Unit = ref.stripReach match
                case ref: TermParamRef
                if !allowed.contains(ref) && !seen.contains(ref) =>
                  seen += ref
                  if ref.underlying.isRef(defn.Caps_Cap) then
                    report.error(i"escaping local reference $ref", tree.srcPos)
                  else
                    val widened = ref.captureSetOfInfo
                    val added = widened.filter(isAllowed(_))
                    capt.println(i"heal $ref in $cs by widening to $added")
                    if !added.subCaptures(cs, frozen = false).isOK then
                      val location = if meth.exists then i" of $meth" else ""
                      val debugSetInfo = if ctx.settings.YccDebug.value then i" $cs" else ""
                      report.error(
                        i"local reference ${ref.paramName} leaks into outer capture set$debugSetInfo of type parameter $paramName$location",
                        tree.srcPos)
                    else
                      widened.elems.foreach(recur)
                case _ =>
              recur(elem)

        def traverse(tp: Type) =
          tp match
            case CapturingType(parent, refs) =>
              healCaptureSet(refs)
              traverse(parent)
            case defn.RefinedFunctionOf(rinfo: MethodType) =>
              traverse(rinfo)
            case tp: TermLambda =>
              val saved = allowed
              try
                tp.paramRefs.foreach(allowed += _)
                traverseChildren(tp)
              finally allowed = saved
            case _ =>
              traverseChildren(tp)

      if tree.isInstanceOf[InferredTypeTree] then
        checker.traverse(tree.knownType)
    end healTypeParam

    def checkArraysAreSealedIn(tp: Type, pos: SrcPos)(using Context): Unit =
      val check = new TypeTraverser:
        def traverse(t: Type): Unit =
          t match
            case AppliedType(tycon, arg :: Nil) if tycon.typeSymbol == defn.ArrayClass =>
              if !(pos.span.isSynthetic && ctx.reporter.errorsReported)
                && !arg.typeSymbol.name.is(WildcardParamName)
              then
                CheckCaptures.disallowRootCapabilitiesIn(arg, NoSymbol,
                  "Array", "have element type",
                  "Since arrays are mutable, they have to be treated like variables,\nso their element type must be sealed.",
                  pos)
              traverseChildren(t)
            case defn.RefinedFunctionOf(rinfo: MethodType) =>
              traverse(rinfo)
            case _ =>
              traverseChildren(t)
      check.traverse(tp)

    /** Perform the following kinds of checks
     *   - Check all explicitly written capturing types for well-formedness using `checkWellFormedPost`.
     *   - Check that arguments of TypeApplys and AppliedTypes conform to their bounds.
     *   - Heal ill-formed capture sets of type parameters. See `healTypeParam`.
     */
    def postCheck(unit: tpd.Tree)(using Context): Unit =
      val checker = new TreeTraverser:
        def traverse(tree: Tree)(using Context): Unit =
          val lctx = tree match
            case _: DefTree | _: TypeDef if tree.symbol.exists => ctx.withOwner(tree.symbol)
            case _ => ctx
          trace(i"post check $tree"):
            traverseChildren(tree)(using lctx)
            check(tree)
        def check(tree: Tree)(using Context) = tree match
          case TypeApply(fun, args) =>
            fun.knownType.widen match
              case tl: PolyType =>
                val normArgs = args.lazyZip(tl.paramInfos).map: (arg, bounds) =>
                  arg.withType(arg.knownType.forceBoxStatus(
                    bounds.hi.isBoxedCapturing | bounds.lo.isBoxedCapturing))
                checkBounds(normArgs, tl)
                args.lazyZip(tl.paramNames).foreach(healTypeParam(_, _, fun.symbol))
              case _ =>
          case tree: TypeTree =>
            checkArraysAreSealedIn(tree.tpe, tree.srcPos)
          case _ =>
        end check
      end checker
      checker.traverse(unit)(using ctx.withOwner(defn.RootClass))
      for chk <- todoAtPostCheck do chk()
      setup.postCheck()

      if !ctx.reporter.errorsReported then
        // We dont report errors here if previous errors were reported, because other
        // errors often result in bad applied types, but flagging these bad types gives
        // often worse error messages than the original errors.
        val checkApplied = new TreeTraverser:
          def traverse(t: Tree)(using Context) = t match
            case tree: InferredTypeTree =>
            case tree: New =>
            case tree: TypeTree => checkAppliedTypesIn(tree.withKnownType)
            case _ => traverseChildren(t)
        checkApplied.traverse(unit)
  end CaptureChecker
end CheckCaptures
