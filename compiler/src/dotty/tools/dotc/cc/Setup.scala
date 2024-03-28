package dotty.tools
package dotc
package cc

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*
import Annotations.Annotation
import config.Feature
import config.Printers.{capt, captDebug}
import ast.tpd, tpd.*
import transform.{PreRecheck, Recheck}, Recheck.*
import CaptureSet.{IdentityCaptRefMap, IdempotentCaptRefMap}
import Synthetics.isExcluded
import util.Property
import printing.{Printer, Texts}, Texts.{Text, Str}
import collection.mutable

/** Operations accessed from CheckCaptures */
trait SetupAPI:
  type DefRecheck = (tpd.ValOrDefDef, Symbol) => Context ?=> Type
  def setupUnit(tree: Tree, recheckDef: DefRecheck)(using Context): Unit
  def isPreCC(sym: Symbol)(using Context): Boolean
  def postCheck()(using Context): Unit

object Setup:

  /** Recognizer for `res $throws exc`, returning `(res, exc)` in case of success */
  object throwsAlias:
    def unapply(tp: Type)(using Context): Option[(Type, Type)] = tp match
      case AppliedType(tycon, res :: exc :: Nil) if tycon.typeSymbol == defn.throwsAlias =>
        Some((res, exc))
      case _ =>
        None
end Setup
import Setup.*

/** A tree traverser that prepares a compilation unit to be capture checked.
 *  It does the following:
 *    - For every inferred type, drop any retains annotations,
 *      add capture sets to all its parts, add refinements to class types and function types.
 *      (c.f. mapInferred)
 *    - For explicit capturing types, expand throws aliases to the underlying (pure) function,
 *      and add some implied capture sets to curried functions (c.f. expandThrowsAlias, expandAbbreviations).
 *    - Add capture sets to self types of classes and objects, unless the self type was written explicitly.
 *    - Box the types of mutable variables and type arguments to methods (type arguments of types
 *      are boxed on access).
 *    - Link the external types of val and def symbols with the inferred types based on their parameter symbols.
 */
class Setup extends PreRecheck, SymTransformer, SetupAPI:
  thisPhase =>

  override def isRunnable(using Context) =
    super.isRunnable && Feature.ccEnabledSomewhere

  private val toBeUpdated = new mutable.HashSet[Symbol]

  private def newFlagsFor(symd: SymDenotation)(using Context): FlagSet =
    if symd.isAllOf(PrivateParamAccessor) && symd.owner.is(CaptureChecked) && !symd.hasAnnotation(defn.ConstructorOnlyAnnot)
    then symd.flags &~ Private | Recheck.ResetPrivate
    else symd.flags

  def isPreCC(sym: Symbol)(using Context): Boolean =
    sym.isTerm && sym.maybeOwner.isClass
    && !sym.is(Module)
    && !sym.owner.is(CaptureChecked)
    && !defn.isFunctionSymbol(sym.owner)

  private def fluidify(using Context) = new TypeMap with IdempotentCaptRefMap:
    def apply(t: Type): Type = t match
      case t: MethodType =>
        mapOver(t)
      case t: TypeLambda =>
        t.derivedLambdaType(resType = this(t.resType))
      case CapturingType(_, _) =>
        t
      case _ =>
        val t1  = t match
          case t @ defn.RefinedFunctionOf(rinfo: MethodType) =>
            t.derivedRefinedType(t.parent, t.refinedName, this(rinfo))
          case _ =>
            mapOver(t)
        if variance > 0 then t1
        else decorate(t1, addedSet = Function.const(CaptureSet.Fluid))

  /**  - Reset `private` flags of parameter accessors so that we can refine them
   *     in Setup if they have non-empty capture sets.
   *   - Special handling of some symbols defined for case classes.
   *  Enabled only until recheck is finished, and provided some compilation unit
   *  is CC-enabled.
   */
  def transformSym(symd: SymDenotation)(using Context): SymDenotation =
    if !pastRecheck && Feature.ccEnabledSomewhere then
      val sym = symd.symbol
      def mappedInfo =
        if toBeUpdated.contains(sym) then symd.info
        else transformExplicitType(symd.info)
      if Synthetics.needsTransform(symd) then
        Synthetics.transform(symd, mappedInfo)
      else if isPreCC(sym) then
        symd.copySymDenotation(info = fluidify(sym.info))
      else if symd.owner.isTerm || symd.is(CaptureChecked) || symd.owner.is(CaptureChecked) then
        val newFlags = newFlagsFor(symd)
        val newInfo = mappedInfo
        if sym.isClass then
          sym.thisType.asInstanceOf[ThisType].invalidateCaches()
        if newFlags != symd.flags || (newInfo ne sym.info)
        then symd.copySymDenotation(initFlags = newFlags, info = newInfo)
        else symd
      else symd
    else symd
  end transformSym

  /** If `tp` is an unboxed capturing type or a function returning an unboxed capturing type,
   *  convert it to be boxed.
   */
  private def box(tp: Type)(using Context): Type =
    def recur(tp: Type): Type = tp.dealiasKeepAnnotsAndOpaques match
      case tp @ CapturingType(parent, refs) =>
        if tp.isBoxed then tp else tp.boxed
      case tp @ AnnotatedType(parent, ann) =>
        if ann.symbol.isRetains
        then CapturingType(parent, ann.tree.toCaptureSet, boxed = true)
        else tp.derivedAnnotatedType(box(parent), ann)
      case tp1 @ AppliedType(tycon, args) if defn.isNonRefinedFunction(tp1) =>
        val res = args.last
        val boxedRes = recur(res)
        if boxedRes eq res then tp
        else tp1.derivedAppliedType(tycon, args.init :+ boxedRes)
      case tp1 @ defn.RefinedFunctionOf(rinfo: MethodType) =>
        val boxedRinfo = recur(rinfo)
        if boxedRinfo eq rinfo then tp
        else boxedRinfo.toFunctionType(alwaysDependent = true)
      case tp1: MethodOrPoly =>
        val res = tp1.resType
        val boxedRes = recur(res)
        if boxedRes eq res then tp
        else tp1.derivedLambdaType(resType = boxedRes)
      case _ => tp
    tp match
      case tp: MethodOrPoly => tp // don't box results of methods outside refinements
      case _ => recur(tp)

  /** Perform the following transformation steps everywhere in a type:
    *  1. Drop retains annotations
    *  2. Turn plain function types into dependent function types, so that
    *     we can refer to their parameters in capture sets. Currently this is
    *     only done at the toplevel, i.e. for function types that are not
    *     themselves argument types of other function types. Without this restriction
    *     pos.../lists.scala and pos/...curried-shorthands.scala fail.
    *     Need to figure out why.
    *  3. Refine other class types C by adding capture set variables to their parameter getters
    *     (see addCaptureRefinements), provided `refine` is true.
    *  4. Add capture set variables to all types that can be tracked
    *
    *  Polytype bounds are only cleaned using step 1, but not otherwise transformed.
    */
  private def transformInferredType(tp: Type)(using Context): Type =
    def mapInferred(refine: Boolean): TypeMap = new TypeMap:
      override def toString = "map inferred"

      /** Refine a possibly applied class type C where the class has tracked parameters
       *  x_1: T_1, ..., x_n: T_n to C { val x_1: CV_1 T_1, ..., val x_n: CV_n T_n }
       *  where CV_1, ..., CV_n are fresh capture sets.
       */
      def addCaptureRefinements(tp: Type): Type = tp match
        case _: TypeRef | _: AppliedType if refine && tp.typeParams.isEmpty =>
          tp.typeSymbol match
            case cls: ClassSymbol
            if !defn.isFunctionClass(cls) && cls.is(CaptureChecked) =>
              cls.paramGetters.foldLeft(tp) { (core, getter) =>
                if atPhase(thisPhase.next)(getter.termRef.isTracked) then
                  val getterType =
                    mapInferred(refine = false)(tp.memberInfo(getter)).strippedDealias
                  RefinedType(core, getter.name,
                      CapturingType(getterType, CaptureSet.RefiningVar(ctx.owner)))
                    .showing(i"add capture refinement $tp --> $result", capt)
                else
                  core
              }
            case _ => tp
        case _ => tp

      private var isTopLevel = true

      private def mapNested(ts: List[Type]): List[Type] =
        val saved = isTopLevel
        isTopLevel = false
        try ts.mapConserve(this)
        finally isTopLevel = saved

      def apply(tp: Type) =
        val tp1 = tp match
          case AnnotatedType(parent, annot) if annot.symbol.isRetains =>
            // Drop explicit retains annotations
            apply(parent)
          case tp @ AppliedType(tycon, args) =>
            val tycon1 = this(tycon)
            if defn.isNonRefinedFunction(tp) then
              // Convert toplevel generic function types to dependent functions
              if !defn.isFunctionSymbol(tp.typeSymbol) && (tp.dealias ne tp) then
                // This type is a function after dealiasing, so we dealias and recurse.
                // See #15925.
                this(tp.dealias)
              else
                val args0 = args.init
                var res0 = args.last
                val args1 = mapNested(args0)
                val res1 = this(res0)
                if isTopLevel then
                  depFun(args1, res1,
                      isContextual = defn.isContextFunctionClass(tycon1.classSymbol))
                    .showing(i"add function refinement $tp ($tycon1, $args1, $res1) (${tp.dealias}) --> $result", capt)
                else if (tycon1 eq tycon) && (args1 eq args0) && (res1 eq res0) then
                  tp
                else
                  tp.derivedAppliedType(tycon1, args1 :+ res1)
            else
              tp.derivedAppliedType(tycon1, args.mapConserve(arg => box(this(arg))))
          case defn.RefinedFunctionOf(rinfo: MethodType) =>
            val rinfo1 = apply(rinfo)
            if rinfo1 ne rinfo then rinfo1.toFunctionType(alwaysDependent = true)
            else tp
          case tp: MethodType =>
            tp.derivedLambdaType(
              paramInfos = mapNested(tp.paramInfos),
              resType = this(tp.resType))
          case tp: TypeLambda =>
            // Don't recurse into parameter bounds, just cleanup any stray retains annotations
            tp.derivedLambdaType(
              paramInfos = tp.paramInfos.mapConserve(_.dropAllRetains.bounds),
              resType = this(tp.resType))
          case _ =>
            mapOver(tp)
        addVar(addCaptureRefinements(normalizeCaptures(tp1)), ctx.owner)
      end apply
    end mapInferred

    mapInferred(refine = true)(tp)
  end transformInferredType

  private def transformExplicitType(tp: Type, tptToCheck: Option[Tree] = None)(using Context): Type =
    val expandAliases = new DeepTypeMap:
      override def toString = "expand aliases"

      /** Expand $throws aliases. This is hard-coded here since $throws aliases in stdlib
        * are defined with `?=>` rather than `?->`.
        * We also have to add a capture set to the last expanded throws alias. I.e.
        *       T $throws E1 $throws E2
        * expands to
        *       (erased x$0: CanThrow[E1]) ?-> (erased x$1: CanThrow[E1]) ?->{x$0} T
        */
      private def expandThrowsAlias(res: Type, exc: Type, encl: List[MethodType]): Type =
        val paramType = AnnotatedType(
            defn.CanThrowClass.typeRef.appliedTo(exc),
            Annotation(defn.ErasedParamAnnot, defn.CanThrowClass.span))
        val resDecomposed = throwsAlias.unapply(res)
        val paramName = nme.syntheticParamName(encl.length)
        val mt = ContextualMethodType(paramName :: Nil)(
            _ => paramType :: Nil,
            mt => resDecomposed match
              case Some((res1, exc1)) => expandThrowsAlias(res1, exc1, mt :: encl)
              case _ => res
          )
        val fntpe = defn.PolyFunctionOf(mt)
        if !encl.isEmpty && resDecomposed.isEmpty then
          val cs = CaptureSet(encl.map(_.paramRefs.head)*)
          CapturingType(fntpe, cs, boxed = false)
        else fntpe

      private def recur(t: Type): Type = normalizeCaptures(mapOver(t))

      def apply(t: Type) =
        t match
          case t @ CapturingType(parent, refs) =>
            t.derivedCapturingType(this(parent), refs)
          case t @ AnnotatedType(parent, ann) =>
            val parent1 = this(parent)
            if ann.symbol.isRetains then
              for tpt <- tptToCheck do
                checkWellformedLater(parent1, ann.tree, tpt)
              CapturingType(parent1, ann.tree.toCaptureSet)
            else
              t.derivedAnnotatedType(parent1, ann)
          case throwsAlias(res, exc) =>
            this(expandThrowsAlias(res, exc, Nil))
          case t: LazyRef =>
            val t1 = this(t.ref)
            if t1 ne t.ref then t1 else t
          case t: TypeVar =>
            this(t.underlying)
          case t =>
            // Map references to capability classes C to C^
            if t.hasUniversalCapability
            then CapturingType(t, defn.expandedUniversalSet, boxed = false)
            else recur(t)
    end expandAliases

    val tp1 = expandAliases(tp) // TODO: Do we still need to follow aliases?
    if tp1 ne tp then capt.println(i"expanded in ${ctx.owner}: $tp  -->  $tp1")
    tp1
  end transformExplicitType

  /** Transform type of type tree, and remember the transformed type as the type the tree */
  private def transformTT(tree: TypeTree, boxed: Boolean, exact: Boolean)(using Context): Unit =
    if !tree.hasRememberedType then
      val transformed =
        if tree.isInstanceOf[InferredTypeTree] && !exact
        then transformInferredType(tree.tpe)
        else transformExplicitType(tree.tpe, tptToCheck = Some(tree))
      tree.rememberType(if boxed then box(transformed) else transformed)

  /** Substitute parameter symbols in `from` to paramRefs in corresponding
   *  method or poly types `to`. We use a single BiTypeMap to do everything.
   *  @param from  a list of lists of type or term parameter symbols of a curried method
   *  @param to    a list of method or poly types corresponding one-to-one to the parameter lists
   */
  private class SubstParams(from: List[List[Symbol]], to: List[LambdaType])(using Context)
  extends DeepTypeMap, BiTypeMap:

    def apply(t: Type): Type = t match
      case t: NamedType =>
        if t.prefix == NoPrefix then
          val sym = t.symbol
          def outer(froms: List[List[Symbol]], tos: List[LambdaType]): Type =
            def inner(from: List[Symbol], to: List[ParamRef]): Type =
              if from.isEmpty then outer(froms.tail, tos.tail)
              else if sym eq from.head then to.head
              else inner(from.tail, to.tail)
            if tos.isEmpty then t
            else inner(froms.head, tos.head.paramRefs)
          outer(from, to)
        else t.derivedSelect(apply(t.prefix))
      case _ =>
        mapOver(t)

    lazy val inverse = new BiTypeMap:
      override def toString = "SubstParams.inverse"
      def apply(t: Type): Type = t match
        case t: ParamRef =>
          def recur(from: List[LambdaType], to: List[List[Symbol]]): Type =
            if from.isEmpty then t
            else if t.binder eq from.head then to.head(t.paramNum).namedType
            else recur(from.tail, to.tail)
          recur(to, from)
        case _ =>
          mapOver(t)
      def inverse = SubstParams.this
  end SubstParams

  /** Update info of `sym` for CheckCaptures phase only */
  private def updateInfo(sym: Symbol, info: Type)(using Context) =
    toBeUpdated += sym
    sym.updateInfo(thisPhase, info, newFlagsFor(sym))
    toBeUpdated -= sym
    sym.namedType match
      case ref: CaptureRef => ref.invalidateCaches() // TODO: needed?
      case _ =>

  extension (sym: Symbol) def nextInfo(using Context): Type =
    atPhase(thisPhase.next)(sym.info)

  def setupTraverser(recheckDef: DefRecheck) = new TreeTraverserWithPreciseImportContexts:

    def transformResultType(tpt: TypeTree, sym: Symbol)(using Context): Unit =
      transformTT(tpt,
          boxed = !ccConfig.allowUniversalInBoxed && sym.is(Mutable, butNot = Method),
            // types of mutable variables are boxed in pre 3.3 codee
          exact = sym.allOverriddenSymbols.hasNext,
            // types of symbols that override a parent don't get a capture set TODO drop
        )
      val addDescription = new TypeTraverser:
        def traverse(tp: Type) = tp match
          case tp @ CapturingType(parent, refs) =>
            if !refs.isConst then refs.withDescription(i"of $sym")
            traverse(parent)
          case _ =>
            traverseChildren(tp)
      addDescription.traverse(tpt.knownType)

    def traverse(tree: Tree)(using Context): Unit =
      tree match
        case tree @ DefDef(_, paramss, tpt: TypeTree, _) =>
          val meth = tree.symbol
          if isExcluded(meth) then
            return

          inContext(ctx.withOwner(meth)):
            paramss.foreach(traverse)
            transformResultType(tpt, meth)
            traverse(tree.rhs)
            //println(i"TYPE of ${tree.symbol.showLocated} = ${tpt.knownType}")

        case tree @ ValDef(_, tpt: TypeTree, _) =>
          val sym = tree.symbol
          val defCtx = if sym.isOneOf(TermParamOrAccessor) then ctx else ctx.withOwner(sym)
          inContext(defCtx):
            transformResultType(tpt, sym)
            capt.println(i"mapped $tree = ${tpt.knownType}")
            traverse(tree.rhs)

        case tree @ TypeApply(fn, args) =>
          traverse(fn)
          for case arg: TypeTree <- args do
            transformTT(arg, boxed = true, exact = false) // type arguments in type applications are boxed

        case tree: TypeDef if tree.symbol.isClass =>
          inContext(ctx.withOwner(tree.symbol)):
            traverseChildren(tree)

        case tree @ SeqLiteral(elems, tpt: TypeTree) =>
          traverse(elems)
          tpt.rememberType(box(transformInferredType(tpt.tpe)))

        case _ =>
          traverseChildren(tree)
      postProcess(tree)
    end traverse

    def postProcess(tree: Tree)(using Context): Unit = tree match
      case tree: TypeTree =>
        transformTT(tree, boxed = false, exact = false)
      case tree: ValOrDefDef =>
        val sym = tree.symbol

        /** The return type of a constructor instantiated with local type and value
         *  parameters. Constructors have `unit` result type, that's why we can't
         *  get this type by reading the result type tree, and have to construct it
         *  explicitly.
         */
        def constrReturnType(info: Type, psymss: List[List[Symbol]]): Type = info match
          case info: MethodOrPoly =>
            constrReturnType(info.instantiate(psymss.head.map(_.namedType)), psymss.tail)
          case _ =>
            info

        /** The local result type, which is the known type of the result type tree,
         *  with special treatment for constructors.
         */
        def localReturnType =
          if sym.isConstructor then constrReturnType(sym.info, sym.paramSymss)
          else tree.tpt.knownType

        def paramSignatureChanges = tree.match
          case tree: DefDef => tree.paramss.nestedExists:
            case param: ValDef => param.tpt.hasRememberedType
            case param: TypeDef => param.rhs.hasRememberedType
          case _ => false

        def signatureChanges =
          tree.tpt.hasRememberedType && !sym.isConstructor || paramSignatureChanges

        // Replace an existing symbol info with inferred types where capture sets of
        // TypeParamRefs and TermParamRefs put in correspondence by BiTypeMaps with the
        // capture sets of the types of the method's parameter symbols and result type.
        def integrateRT(
            info: Type,                     // symbol info to replace
            psymss: List[List[Symbol]],     // the local (type and term) parameter symbols corresponding to `info`
            resType: Type,                  // the locally computed return type
            prevPsymss: List[List[Symbol]], // the local parameter symbols seen previously in reverse order
            prevLambdas: List[LambdaType]   // the outer method and polytypes generated previously in reverse order
          ): Type =
          info match
            case mt: MethodOrPoly =>
              val psyms = psymss.head
              mt.companion(mt.paramNames)(
                mt1 =>
                  if !paramSignatureChanges && !mt.isParamDependent && prevLambdas.isEmpty then
                    mt.paramInfos
                  else
                    val subst = SubstParams(psyms :: prevPsymss, mt1 :: prevLambdas)
                    psyms.map(psym => subst(psym.nextInfo).asInstanceOf[mt.PInfo]),
                mt1 =>
                  integrateRT(mt.resType, psymss.tail, resType, psyms :: prevPsymss, mt1 :: prevLambdas)
              )
            case info: ExprType =>
              info.derivedExprType(resType =
                integrateRT(info.resType, psymss, resType, prevPsymss, prevLambdas))
            case info =>
              if prevLambdas.isEmpty then resType
              else SubstParams(prevPsymss, prevLambdas)(resType)

        if sym.exists && signatureChanges then
          val newInfo = integrateRT(sym.info, sym.paramSymss, localReturnType, Nil, Nil)
            .showing(i"update info $sym: ${sym.info} = $result", capt)
          if newInfo ne sym.info then
            val updatedInfo =
              if sym.isAnonymousFunction
                  || sym.is(Param)
                  || sym.is(ParamAccessor)
                  || sym.isPrimaryConstructor
              then
                // closures are handled specially; the newInfo is constrained from
                // the expected type and only afterwards we recheck the definition
                newInfo
              else new LazyType:
                def complete(denot: SymDenotation)(using Context) =
                  // infos of other methods are determined from their definitions which
                  // are checked on demand
                  assert(ctx.phase == thisPhase.next, i"$sym")
                  capt.println(i"forcing $sym, printing = ${ctx.mode.is(Mode.Printing)}")
                  //if ctx.mode.is(Mode.Printing) then new Error().printStackTrace()
                  denot.info = newInfo
                  recheckDef(tree, sym)
            updateInfo(sym, updatedInfo)

      case tree: Bind =>
        val sym = tree.symbol
        updateInfo(sym, transformInferredType(sym.info))
      case tree: TypeDef =>
        tree.symbol match
          case cls: ClassSymbol =>
            val cinfo @ ClassInfo(prefix, _, ps, decls, selfInfo) = cls.classInfo
            def innerModule = cls.is(ModuleClass) && !cls.isStatic
            val selfInfo1 =
              if (selfInfo ne NoType) && !innerModule then
                // if selfInfo is explicitly given then use that one, except if
                // self info applies to non-static modules, these still need to be inferred
                selfInfo
              else if cls.isPureClass then
                // is cls is known to be pure, nothing needs to be added to self type
                selfInfo
              else if !cls.isEffectivelySealed && !cls.baseClassHasExplicitSelfType then
                // assume {cap} for completely unconstrained self types of publicly extensible classes
                CapturingType(cinfo.selfType, CaptureSet.universal)
              else
                // Infer the self type for the rest, which is all classes without explicit
                // self types (to which we also add nested module classes), provided they are
                // neither pure, nor are publicily extensible with an unconstrained self type.
                CapturingType(cinfo.selfType, CaptureSet.Var(cls))
            val ps1 = inContext(ctx.withOwner(cls)):
              ps.mapConserve(transformExplicitType(_))
            if (selfInfo1 ne selfInfo) || (ps1 ne ps) then
              val newInfo = ClassInfo(prefix, cls, ps1, decls, selfInfo1)
              updateInfo(cls, newInfo)
              capt.println(i"update class info of $cls with parents $ps selfinfo $selfInfo to $newInfo")
              cls.thisType.asInstanceOf[ThisType].invalidateCaches()
              if cls.is(ModuleClass) then
                // if it's a module, the capture set of the module reference is the capture set of the self type
                val modul = cls.sourceModule
                updateInfo(modul, CapturingType(modul.info, selfInfo1.asInstanceOf[Type].captureSet))
                modul.termRef.invalidateCaches()
          case _ =>
      case _ =>
    end postProcess
  end setupTraverser

  /** Checks whether an abstract type could be impure. See also: [[needsVariable]]. */
  private def instanceCanBeImpure(tp: Type)(using Context): Boolean = {
    tp.dealiasKeepAnnots match
      case CapturingType(_, refs) =>
        !refs.isAlwaysEmpty
      case RetainingType(parent, refs) =>
        !refs.isEmpty
      case tp: (TypeRef | AppliedType) =>
        val sym = tp.typeSymbol
        if sym.isClass then
          !sym.isPureClass
        else
          sym != defn.Caps_Cap && instanceCanBeImpure(tp.superType)
      case tp: (RefinedOrRecType | MatchType) =>
        instanceCanBeImpure(tp.underlying)
      case tp: AndType =>
        instanceCanBeImpure(tp.tp1) || instanceCanBeImpure(tp.tp2)
      case tp: OrType =>
        instanceCanBeImpure(tp.tp1) && instanceCanBeImpure(tp.tp2)
      case _ =>
        false
  }.showing(i"instance can be impure $tp = $result", capt)

  /** Should a capture set variable be added on type `tp`? */
  def needsVariable(tp: Type)(using Context): Boolean = {
    tp.typeParams.isEmpty && tp.match
      case tp: (TypeRef | AppliedType) =>
        val sym = tp.typeSymbol
        if sym.isClass then
          !sym.isPureClass && sym != defn.AnyClass
        else
          val tp1 = tp.dealiasKeepAnnotsAndOpaques
          if tp1 ne tp then needsVariable(tp1)
          else instanceCanBeImpure(tp1)
      case tp: (RefinedOrRecType | MatchType) =>
        needsVariable(tp.underlying)
      case tp: AndType =>
        needsVariable(tp.tp1) && needsVariable(tp.tp2)
      case tp: OrType =>
        needsVariable(tp.tp1) || needsVariable(tp.tp2)
      case CapturingType(parent, refs) =>
        needsVariable(parent)
        && refs.isConst       // if refs is a variable, no need to add another
        && !refs.isUniversal  // if refs is {cap}, an added variable would not change anything
      case RetainingType(parent, refs) =>
        needsVariable(parent)
        && !refs.tpes.exists:
            case ref: TermRef => ref.isRootCapability
            case _ => false
      case AnnotatedType(parent, _) =>
        needsVariable(parent)
      case _ =>
        false
  }.showing(i"can have inferred capture $tp = $result", captDebug)

  /** Pull out an embedded capture set from a part of `tp` */
  def normalizeCaptures(tp: Type)(using Context): Type = tp match
    case tp @ RefinedType(parent @ CapturingType(parent1, refs), rname, rinfo) =>
      CapturingType(tp.derivedRefinedType(parent1, rname, rinfo), refs, parent.isBoxed)
    case tp: RecType =>
      tp.parent match
        case parent @ CapturingType(parent1, refs) =>
          CapturingType(tp.derivedRecType(parent1), refs, parent.isBoxed)
        case _ =>
          tp // can return `tp` here since unlike RefinedTypes, RecTypes are never created
              // by `mapInferred`. Hence if the underlying type admits capture variables
              // a variable was already added, and the first case above would apply.
    case AndType(tp1 @ CapturingType(parent1, refs1), tp2 @ CapturingType(parent2, refs2)) =>
      assert(tp1.isBoxed == tp2.isBoxed)
      CapturingType(AndType(parent1, parent2), refs1 ** refs2, tp1.isBoxed)
    case tp @ OrType(tp1 @ CapturingType(parent1, refs1), tp2 @ CapturingType(parent2, refs2)) =>
      assert(tp1.isBoxed == tp2.isBoxed)
      CapturingType(OrType(parent1, parent2, tp.isSoft), refs1 ++ refs2, tp1.isBoxed)
    case tp @ OrType(tp1 @ CapturingType(parent1, refs1), tp2) =>
      CapturingType(OrType(parent1, tp2, tp.isSoft), refs1, tp1.isBoxed)
    case tp @ OrType(tp1, tp2 @ CapturingType(parent2, refs2)) =>
      CapturingType(OrType(tp1, parent2, tp.isSoft), refs2, tp2.isBoxed)
    case tp @ AppliedType(tycon, args) if !defn.isFunctionClass(tp.dealias.typeSymbol) =>
      tp.derivedAppliedType(tycon, args.mapConserve(box))
    case tp: RealTypeBounds =>
      tp.derivedTypeBounds(tp.lo, box(tp.hi))
    case tp: LazyRef =>
      normalizeCaptures(tp.ref)
    case _ =>
      tp

  /** Add a capture set variable to `tp` if necessary, or maybe pull out
   *  an embedded capture set variable from a part of `tp`.
   */
  def decorate(tp: Type, addedSet: Type => CaptureSet)(using Context): Type =
    if tp.typeSymbol == defn.FromJavaObjectSymbol then
      // For capture checking, we assume Object from Java is the same as Any
      tp
    else
      def maybeAdd(target: Type, fallback: Type) =
        if needsVariable(target) then CapturingType(target, addedSet(target))
        else fallback
      val dealiased = tp.dealiasKeepAnnotsAndOpaques
      if dealiased ne tp then
        val transformed = transformInferredType(dealiased)
        maybeAdd(transformed, if transformed ne dealiased then transformed else tp)
      else maybeAdd(tp, tp)

  /** Add a capture set variable to `tp` if necessary, or maybe pull out
   *  an embedded capture set variable from a part of `tp`.
   */
  def addVar(tp: Type, owner: Symbol)(using Context): Type =
    decorate(tp,
      addedSet = _.dealias.match
        case CapturingType(_, refs) => CaptureSet.Var(owner, refs.elems)
        case _ => CaptureSet.Var(owner))

  def setupUnit(tree: Tree, recheckDef: DefRecheck)(using Context): Unit =
    setupTraverser(recheckDef).traverse(tree)(using ctx.withPhase(thisPhase))

  // ------ Checks to run after main capture checking --------------------------

  /** A list of actions to perform at postCheck */
  private val todoAtPostCheck = new mutable.ListBuffer[Context => Unit]

  /** If `tp` is a capturing type, check that all references it mentions have non-empty
   *  capture sets.
   *  Also: warn about redundant capture annotations.
   *  This check is performed after capture sets are computed in phase cc.
   *  Note: We need to perform the check on the original annotation rather than its
   *  capture set since the conversion to a capture set already eliminates redundant elements.
   */
  private def checkWellformedPost(parent: Type, ann: Tree, tpt: Tree)(using Context): Unit =
    capt.println(i"checkWF post $parent ${ann.retainedElems} in $tpt")
    var retained = ann.retainedElems.toArray
    for i <- 0 until retained.length do
      val refTree = retained(i)
      val ref = refTree.toCaptureRef

      def pos =
        if refTree.span.exists then refTree.srcPos
        else if ann.span.exists then ann.srcPos
        else tpt.srcPos

      def check(others: CaptureSet, dom: Type | CaptureSet): Unit =
        if others.accountsFor(ref) then
          report.warning(em"redundant capture: $dom already accounts for $ref", pos)

      if ref.captureSetOfInfo.elems.isEmpty then
        report.error(em"$ref cannot be tracked since its capture set is empty", pos)
      if parent.captureSet ne defn.expandedUniversalSet then
        check(parent.captureSet, parent)

      val others =
        for j <- 0 until retained.length if j != i yield retained(j).toCaptureRef
      val remaining = CaptureSet(others*)
      check(remaining, remaining)
    end for
  end checkWellformedPost

  /** Check well formed at post check time */
  private def checkWellformedLater(parent: Type, ann: Tree, tpt: Tree)(using Context): Unit =
    if !tpt.span.isZeroExtent then
      todoAtPostCheck += (ctx1 =>
        checkWellformedPost(parent, ann, tpt)(using ctx1.withOwner(ctx.owner)))

  def postCheck()(using Context): Unit =
    for chk <- todoAtPostCheck do chk(ctx)
    todoAtPostCheck.clear()
end Setup
