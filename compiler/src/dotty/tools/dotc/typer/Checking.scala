package dotty.tools
package dotc
package typer

import core.*
import ast.*
import Contexts.*
import Types.*
import Flags.*
import Names.*
import StdNames.*
import Symbols.*
import Trees.*
import ProtoTypes.*
import Scopes.*
import CheckRealizable.*
import ErrorReporting.errorTree
import util.Spans.Span
import Phases.refchecksPhase
import Constants.Constant

import util.SrcPos
import util.Spans.Span
import rewrites.Rewrites.patch
import inlines.Inlines
import Decorators.*
import ErrorReporting.{err, errorType}
import config.Printers.{typr, patmatch}
import NameKinds.DefaultGetterName
import NameOps.*
import SymDenotations.{NoCompleter, NoDenotation}
import Applications.unapplyArgs
import Inferencing.isFullyDefined
import transform.patmat.SpaceEngine.{isIrrefutable, isIrrefutableQuotePattern}
import transform.ValueClasses.underlyingOfValueClass
import config.Feature
import config.Feature.sourceVersion
import config.SourceVersion.*
import config.MigrationVersion
import printing.Formatting.hlAsKeyword
import cc.{isCaptureChecking, isRetainsLike}

import collection.mutable
import reporting.*

object Checking {
  import tpd.*

  /** Add further information for error messages involving applied types if the
   *  type is inferred:
   *   1. the full inferred type is a TypeTree node
   *   2. the applied type causing the error, if different from (1)
   */
  private def showInferred(msg: Message, app: Type, tpt: Tree)(using Context): Message =
    if tpt.isInstanceOf[TypeTree] then
      def subPart = if app eq tpt.tpe then "" else i" subpart $app of"
      msg.append(i" in$subPart inferred type ${tpt}")
        .appendExplanation("\n\nTo fix the problem, provide an explicit type.")
    else msg

  /** A general checkBounds method that can be used for TypeApply nodes as
   *  well as for AppliedTypeTree nodes. Also checks that type arguments to
   *  *-type parameters are fully applied.
   *  @param tpt  If bounds are checked for an AppliedType, the type tree representing
   *              or (in case it is inferred) containing the type.
   *  See TypeOps.boundsViolations for an explanation of the first four parameters.
   */
  def checkBounds(args: List[tpd.Tree], boundss: List[TypeBounds],
    instantiate: (Type, List[Type]) => Type, app: Type = NoType, tpt: Tree = EmptyTree)(using Context): Unit =
    if !isCaptureChecking then
      args.lazyZip(boundss).foreach { (arg, bound) =>
        if !bound.isLambdaSub && !arg.tpe.hasSimpleKind then
          errorTree(arg,
            showInferred(MissingTypeParameterInTypeApp(arg.tpe), app, tpt))
      }
    for (arg, which, bound) <- TypeOps.boundsViolations(args, boundss, instantiate, app) do
      report.error(
        showInferred(DoesNotConformToBound(arg.tpe, which, bound), app, tpt),
        arg.srcPos.focus)

  /** Check that type arguments `args` conform to corresponding bounds in `tl`
   *  Note: This does not check the bounds of AppliedTypeTrees. These
   *  are handled by method checkAppliedType below.
   */
  def checkBounds(args: List[tpd.Tree], tl: TypeLambda)(using Context): Unit =
    checkBounds(args, tl.paramInfos, _.substParams(tl, _))

  def checkGoodBounds(sym: Symbol)(using Context): Boolean =
    val bad = findBadBounds(sym.typeRef)
    if bad.exists then
      report.error(em"$sym has possibly conflicting bounds $bad", sym.srcPos)
    !bad.exists

  /** If `tp` dealiases to a typebounds L..H where not L <:< H
   *  return the potentially conflicting bounds, otherwise return NoType.
   */
  private def findBadBounds(tp: Type)(using Context): Type = tp.dealias match
    case tp: TypeRef => findBadBounds(tp.info)
    case tp @ TypeBounds(lo, hi) if !(lo <:< hi) => tp
    case _ => NoType

  /** Check applied type trees for well-formedness. This means
   *   - all arguments are within their corresponding bounds
   *   - if type is a higher-kinded application with wildcard arguments,
   *     check that it or one of its supertypes can be reduced to a normal application.
   *     Unreducible applications correspond to general existentials, and we
   *     cannot handle those.
   *  @param tree The applied type tree to check
   *  @param tpt  If `tree` is synthesized from a type in a TypeTree,
   *              the original TypeTree, or EmptyTree otherwise.
   */
  def checkAppliedType(tree: AppliedTypeTree, tpt: Tree = EmptyTree)(using Context): Unit = {
    val AppliedTypeTree(tycon, args) = tree
    // If `args` is a list of named arguments, return corresponding type parameters,
    // otherwise return type parameters unchanged
    val tparams = tycon.tpe.typeParams
    val bounds = tparams.map(_.paramInfoAsSeenFrom(tree.tpe).bounds)
    def instantiate(bound: Type, args: List[Type]) =
      tparams match
        case LambdaParam(lam, _) :: _ =>
          HKTypeLambda.fromParams(tparams, bound).appliedTo(args)
        case _ =>
          bound // paramInfoAsSeenFrom already took care of instantiation in this case
    if !ctx.mode.is(Mode.Pattern)           // no bounds checking in patterns
       && tycon.symbol != defn.TypeBoxClass // TypeBox types are generated for capture
                                            // conversion, may contain AnyKind as arguments
    then
      checkBounds(args, bounds, instantiate, tree.tpe, tpt)

    def checkWildcardApply(tp: Type): Unit = tp match {
      case tp @ AppliedType(tycon, _) =>
        if tp.isUnreducibleWild then
          report.errorOrMigrationWarning(
            showInferred(UnreducibleApplication(tycon), tp, tpt),
            tree.srcPos, MigrationVersion.Scala2to3)
      case _ =>
    }
    def checkValidIfApply(using Context): Unit =
      checkWildcardApply(tycon.tpe.appliedTo(args.map(_.tpe)))
    withMode(Mode.AllowLambdaWildcardApply)(checkValidIfApply)
  }

  /** Check all applied type trees in inferred type `tpt` for well-formedness */
  def checkAppliedTypesIn(tpt: TypeTree)(using Context): Unit =
    val checker = new TypeTraverser:
      def traverse(tp: Type) =
        tp match
          case AppliedType(tycon, argTypes)
          if !(tycon.typeSymbol.is(JavaDefined) && ctx.compilationUnit.isJava)
            // Don't check bounds in Java units that refer to Java type constructors.
            // Scala is not obliged to do Java type checking and in fact i17763 goes wrong
            // if we attempt to check bounds of F-bounded mutually recursive Java interfaces.
            // Do check all bounds in Scala units and those bounds in Java units that
            // occur in applications of Scala type constructors.
            && !isCaptureChecking || tycon.typeSymbol.is(CaptureChecked)
            // Don't check bounds when capture checking type constructors that were not
            // themselves capture checked. Since the type constructor could not foresee
            // possible capture sets, it's better to be lenient for backwards compatibility.
          =>
            checkAppliedType(
              untpd.AppliedTypeTree(TypeTree(tycon), argTypes.map(TypeTree(_)))
                .withType(tp).withSpan(tpt.span.toSynthetic),
              tpt)
          case _ =>
        traverseChildren(tp)
    checker.traverse(tpt.tpe)

  def checkNoWildcard(tree: Tree)(using Context): Tree = tree.tpe match {
    case tpe: TypeBounds => errorTree(tree, em"no wildcard type allowed here")
    case _ => tree
  }

  /** Check that kind of `arg` has the same outline as the kind of paramBounds.
   *  E.g. if `paramBounds` has kind * -> *, `arg` must have that kind as well,
   *  and analogously for all other kinds. This kind checking does not take into account
   *  variances or bounds. The more detailed kind checking is done as part of checkBounds in PostTyper.
   *  The purpose of preCheckKind is to do a rough test earlier in Typer,
   *  in order to prevent scenarios that lead to self application of
   *  types. Self application needs to be avoided since it can lead to stack overflows.
   *  Test cases are neg/i2771.scala and neg/i2771b.scala.
   *  A NoType paramBounds is used as a sign that checking should be suppressed.
   */
  def preCheckKind(arg: Tree, paramBounds: Type)(using Context): Tree =
    if (arg.tpe.widen.isRef(defn.NothingClass) ||
        !paramBounds.exists ||
        arg.tpe.hasSameKindAs(paramBounds.bounds.hi)) arg
    else errorTree(arg, em"Type argument ${arg.tpe} does not have the same kind as its bound $paramBounds")

  def preCheckKinds(args: List[Tree], paramBoundss: List[Type])(using Context): List[Tree] = {
    val args1 = args.zipWithConserve(paramBoundss)(preCheckKind)
    args1 ++ args.drop(paramBoundss.length)
      // add any arguments that do not correspond to a parameter back,
      // so the wrong number of parameters is reported afterwards.
  }

  /** Check that `tp` refers to a nonAbstract class
   *  and that the instance conforms to the self type of the created class.
   */
  def checkInstantiable(tp: Type, srcTp: Type, pos: SrcPos)(using Context): Unit =
    tp.underlyingClassRef(refinementOK = false) match
      case tref: TypeRef =>
        val cls = tref.symbol
        if (cls.isOneOf(AbstractOrTrait)) {
          val srcCls = srcTp.underlyingClassRef(refinementOK = false).typeSymbol
          report.error(CantInstantiateAbstractClassOrTrait(srcCls, isTrait = srcCls.is(Trait)), pos)
        }
        if !cls.is(Module) then
          // Create a synthetic singleton type instance, and check whether
          // it conforms to the self type of the class as seen from that instance.
          val stp = SkolemType(tp)
          val selfType = cls.declaredSelfTypeAsSeenFrom(stp)
          if selfType.exists && !(stp <:< selfType) then
            report.error(DoesNotConformToSelfTypeCantBeInstantiated(tp, selfType), pos)
      case _ =>

  /** Check that type `tp` is realizable. */
  def checkRealizable(tp: Type, pos: SrcPos, what: String = "path")(using Context): Unit = {
    val rstatus = realizability(tp)
    if (rstatus ne Realizable)
      report.errorOrMigrationWarning(
        em"$tp is not a legal $what\nsince it${rstatus.msg}", pos, MigrationVersion.Scala2to3)
  }

  /** Given a parent `parent` of a class `cls`, if `parent` is a trait check that
   *  the superclass of `cls` derived from the superclass of `parent`.
   *
   *  An exception is made if `cls` extends `Any`, and `parent` is `java.io.Serializable`
   *  or `java.lang.Comparable`. These two classes are treated by Scala as universal
   *  traits. E.g. the following is OK:
   *
   *      ... extends Any with java.io.Serializable
   *
   *  The standard library relies on this idiom.
   */
  def checkTraitInheritance(parent: Symbol, cls: ClassSymbol, pos: SrcPos)(using Context): Unit =
    parent match {
      case parent: ClassSymbol if parent.is(Trait) =>
        val psuper = parent.superClass
        val csuper = cls.superClass
        val ok = csuper.derivesFrom(psuper) ||
          parent.is(JavaDefined) && csuper == defn.AnyClass &&
          (parent == defn.JavaSerializableClass || parent == defn.ComparableClass)
        if (!ok)
          report.error(em"illegal trait inheritance: super$csuper does not derive from $parent's super$psuper", pos)
      case _ =>
    }

  /** A type map which checks that the only cycles in a type are F-bounds
   *  and that protects all F-bounded references by LazyRefs.
   */
  class CheckNonCyclicMap(sym: Symbol, reportErrors: Boolean)(using Context) extends TypeMap {

    /** Set of type references whose info is currently checked */
    private val locked = mutable.Set[TypeRef]()

    /** Are cycles allowed within nested refinedInfos of currently checked type? */
    private var nestedCycleOK = false

    /** Are cycles allowed within currently checked type? */
    private var cycleOK = false

    /** A diagnostic output string that indicates the position of the last
     *  part of a type bounds checked by checkInfo. Possible choices:
     *  alias, lower bound, upper bound.
     */
    var where: String = ""

    /** The last type top-level type checked when a CyclicReference occurs. */
    var lastChecked: Type = NoType

    private def checkPart(tp: Type, w: String) =
      try apply(tp)
      finally {
        where = w
        lastChecked = tp
      }

    private def checkUpper(tp: Type, w: String) = {
      val saved = nestedCycleOK
      nestedCycleOK = true
      try checkPart(tp, w)
      finally nestedCycleOK = saved
    }

    /** Check info `tp` for cycles. Throw CyclicReference for illegal cycles,
     *  break direct cycle with a LazyRef for legal, F-bounded cycles.
     */
    def checkInfo(tp: Type): Type = tp match {
      case tp @ TypeAlias(alias) =>
        tp.derivedAlias(checkPart(alias, "alias"))
      case tp @ MatchAlias(alias) =>
        tp.derivedAlias(checkUpper(alias, "match"))
      case tp @ TypeBounds(lo, hi) =>
        tp.derivedTypeBounds(checkPart(lo, "lower bound"), checkUpper(hi, "upper bound"))
      case _ =>
        tp
    }

    private def apply(tp: Type, cycleOK: Boolean, nestedCycleOK: Boolean): Type = {
      val savedCycleOK = this.cycleOK
      val savedNestedCycleOK = this.nestedCycleOK
      this.cycleOK = cycleOK
      this.nestedCycleOK = nestedCycleOK
      try apply(tp)
      finally {
        this.cycleOK = savedCycleOK
        this.nestedCycleOK = savedNestedCycleOK
      }
    }

    def apply(tp: Type): Type = tp match {
      case tp: TermRef =>
        this(tp.info)
        mapOver(tp)
      case tp @ AppliedType(tycon, args) =>
        tp.derivedAppliedType(this(tycon), args.mapConserve(this(_, nestedCycleOK, nestedCycleOK)))
      case tp @ RefinedType(parent, name, rinfo) =>
        tp.derivedRefinedType(this(parent), name, this(rinfo, nestedCycleOK, nestedCycleOK))
      case tp: RecType =>
        tp.rebind(this(tp.parent))
      case tp @ TypeRef(pre, _) =>
        try {
          // A prefix is interesting if it might contain (transitively) a reference
          // to symbol `sym` itself. We only check references with interesting
          // prefixes for cycles. This pruning is done in order not to force
          // global symbols when doing the cyclicity check.
          def isInteresting(prefix: Type): Boolean = prefix.stripTypeVar match {
            case NoPrefix => true
            case prefix: ThisType =>
              sym.owner.isClass && (
                prefix.cls.isContainedIn(sym.owner)    // sym reachable through outer references
                || sym.owner.isContainedIn(prefix.cls) // sym reachable through member references
              )
            case prefix: NamedType =>
              !sym.is(Private) && prefix.derivesFrom(sym.owner)
              || {
                val pcls = prefix.symbol.moduleClass
                if pcls.isStaticOwner then
                  pcls.span.exists && pcls.defRunId == ctx.runId // cheaper approximation to isDefinedInCurrentRun
                else
                  isInteresting(prefix.prefix)
              }
            case SuperType(thistp, _) => isInteresting(thistp)
            case AndType(tp1, tp2) => isInteresting(tp1) || isInteresting(tp2)
            case OrType(tp1, tp2) => isInteresting(tp1) && isInteresting(tp2)
            case _: RefinedOrRecType | _: AppliedType => true
            case tp: AnnotatedType => isInteresting(tp.parent)
            case _ => false
          }

          if isInteresting(pre) then
            val traceCycles = CyclicReference.isTraced
            try
              if traceCycles then
                CyclicReference.pushTrace("explore ", tp.symbol, " for cyclic references")
              val pre1 = this(pre, false, false)
              if locked.contains(tp)
                  || tp.symbol.infoOrCompleter.isInstanceOf[NoCompleter]
              then
                throw CyclicReference(tp.symbol)
              locked += tp
              try
                if tp.symbol.isOpaqueAlias then
                  checkInfo(TypeAlias(tp.translucentSuperType))
                else if !tp.symbol.isClass then
                  checkInfo(tp.info)
              finally
                locked -= tp
              tp.withPrefix(pre1)
            finally
              if traceCycles then CyclicReference.popTrace()
          else tp
        }
        catch {
          case ex: CyclicReference =>
            report.debuglog(i"cycle detected for $tp, $nestedCycleOK, $cycleOK")
            if (cycleOK) LazyRef.of(tp)
            else if (reportErrors) throw ex
            else tp
        }
      case _ => mapOver(tp)
    }
  }

  /** Under -Yrequire-targetName, if `sym` has an operator name, check that it has a
   *  @targetName annotation.
   */
  def checkValidOperator(sym: Symbol)(using Context): Unit =
    if ctx.settings.YrequireTargetName.value then
      sym.name.toTermName match
        case name: SimpleName
        if name.isOperatorName
          && !name.isSetterName
          && !name.isConstructorName
          && !sym.getAnnotation(defn.TargetNameAnnot).isDefined
          && !sym.is(Synthetic) =>
          report.warning(
            i"$sym has an operator name; it should come with an @targetName annotation", sym.srcPos)
        case _ =>

  /** Check that `info` of symbol `sym` is not cyclic.
   *  @pre     sym is not yet initialized (i.e. its type is a Completer).
   *  @return  `info` where every legal F-bounded reference is protected
   *                  by a `LazyRef`, or `ErrorType` if a cycle was detected and reported.
   */
  def checkNonCyclic(sym: Symbol, info: Type, reportErrors: Boolean)(using Context): Type = {
    val checker = withMode(Mode.CheckCyclic)(new CheckNonCyclicMap(sym, reportErrors))
    try
      val toCheck = info match
        case info: RealTypeBounds if sym.isOpaqueAlias => TypeAlias(sym.opaqueAlias)
        case _ => info
      checker.checkInfo(toCheck)
    catch {
      case ex: CyclicReference =>
        if (reportErrors)
          errorType(IllegalCyclicTypeReference(ex, sym, checker.where, checker.lastChecked), sym.srcPos)
        else info
    }
  }

  /** Check that refinement satisfies the following two conditions
   *  1. No part of it refers to a symbol that's defined in the same refinement
   *     at a textually later point.
   *  2. All references to the refinement itself via `this` are followed by
   *     selections.
   *  Note: It's not yet clear what exactly we want to allow and what we want to rule out.
   *  This depends also on firming up the DOT calculus. For the moment we only issue
   *  deprecated warnings, not errors.
   */
  def checkRefinementNonCyclic(refinement: Tree, refineCls: ClassSymbol, seen: mutable.Set[Symbol])
    (using Context): Unit = {
    def flag(what: String, tree: Tree) =
      report.warning(i"$what reference in refinement is deprecated", tree.srcPos)
    def forwardRef(tree: Tree) = flag("forward", tree)
    def selfRef(tree: Tree) = flag("self", tree)
    val checkTree = new TreeAccumulator[Unit] {
      def checkRef(tree: Tree, sym: Symbol) =
        if (sym.maybeOwner == refineCls && !seen(sym)) forwardRef(tree)
      def apply(x: Unit, tree: Tree)(using Context) = tree match {
        case tree: MemberDef =>
          foldOver(x, tree)
          seen += tree.symbol
        case tree @ Select(This(_), _) =>
          checkRef(tree, tree.symbol)
        case tree: RefTree =>
          checkRef(tree, tree.symbol)
          foldOver(x, tree)
        case tree: This if tree.tpe.classSymbol == refineCls =>
          selfRef(tree)
        case tree: TypeTree =>
          val checkType = new TypeAccumulator[Unit] {
            def apply(x: Unit, tp: Type): Unit = tp match {
              case tp: NamedType =>
                checkRef(tree, tp.symbol)
                tp.prefix match {
                  case pre: ThisType =>
                  case pre => foldOver(x, pre)
                }
              case tp: ThisType if tp.cls == refineCls =>
                selfRef(tree)
              case _ =>
                foldOver(x, tp)
            }
          }
          checkType((), tree.tpe)
        case _ =>
          foldOver(x, tree)
      }
    }
    checkTree((), refinement)
  }

  /** Check type members inherited from different `parents` of `joint` type for cycles,
   *  unless a type with the same name already appears in `decls`.
   *  @return    true iff no cycles were detected
   */
  def checkNonCyclicInherited(joint: Type, parents: List[Type], decls: Scope, pos: SrcPos)(using Context): Unit = {
    // If we don't have more than one parent, then there's nothing to check
    if (parents.lengthCompare(1) <= 0)
      return

    def qualifies(sym: Symbol) = sym.name.isTypeName && !sym.is(Private)
    withMode(Mode.CheckCyclic) {
      val abstractTypeNames =
        for (parent <- parents; mbr <- parent.abstractTypeMembers if qualifies(mbr.symbol))
        yield mbr.name.asTypeName

      for name <- abstractTypeNames do
        try
          val mbr = joint.member(name)
          mbr.info match
            case bounds: TypeBounds =>
              !checkNonCyclic(mbr.symbol, bounds, reportErrors = true).isError
            case _ =>
              true
        catch case _: RecursionOverflow | _: CyclicReference =>
          report.error(em"cyclic reference involving type $name", pos)
          false
    }
  }

  /** Check that symbol's definition is well-formed. */
  def checkWellFormed(sym: Symbol)(using Context): Unit = {
    def fail(msg: Message) = report.error(msg, sym.srcPos)
    def warn(msg: Message) = report.warning(msg, sym.srcPos)

    def checkWithDeferred(flag: FlagSet) =
      if (sym.isOneOf(flag))
        fail(AbstractMemberMayNotHaveModifier(sym, flag))
    def checkNoConflict(flag1: FlagSet, flag2: FlagSet, msg: Message) =
      if (sym.isAllOf(flag1 | flag2)) fail(msg)
    def checkCombination(flag1: FlagSet, flag2: FlagSet) =
      if sym.isAllOf(flag1 | flag2) then
        fail(em"illegal combination of modifiers: `${flag1.flagsString}` and `${flag2.flagsString}` for: $sym")
    def checkApplicable(flag: Flag, ok: Boolean) =
      if sym.is(flag, butNot = Synthetic) && !ok then
        fail(ModifierNotAllowedForDefinition(flag))
        sym.resetFlag(flag)

    if (sym.is(Inline) &&
          (  sym.is(ParamAccessor) && sym.owner.isClass
          || sym.is(TermParam) && !sym.owner.isInlineMethod
          ))
      fail(ParamsNoInline(sym.owner))
    if sym.isInlineMethod && !sym.is(Deferred) && sym.allOverriddenSymbols.nonEmpty then
      checkInlineOverrideParameters(sym)
    if (sym.is(Implicit)) {
      assert(!sym.owner.is(Package), s"top-level implicit $sym should be wrapped by a package after typer")
      if sym.isType && (!sym.isClass || sym.is(Trait)) then
        fail(TypesAndTraitsCantBeImplicit())
    }
    if sym.is(Transparent) then
      if sym.isType then
        if !sym.isExtensibleClass then fail(em"`transparent` can only be used for extensible classes and traits")
      else
        if !sym.isInlineMethod then fail(em"`transparent` can only be used for inline methods")
    if (!sym.isClass && sym.is(Abstract))
      fail(OnlyClassesCanBeAbstract(sym))
        // note: this is not covered by the next test since terms can be abstract (which is a dual-mode flag)
        // but they can never be one of ClassOnlyFlags
    if !sym.isClass && sym.isOneOf(ClassOnlyFlags) then
      val illegal = sym.flags & ClassOnlyFlags
      fail(em"only classes can be ${illegal.flagsString}")
    if (sym.is(AbsOverride) && !sym.owner.is(Trait))
      fail(AbstractOverrideOnlyInTraits(sym))
    if sym.is(Trait) then
      if sym.is(Final) then
        fail(TraitsMayNotBeFinal(sym))
      else if sym.is(Open) then
        warn(RedundantModifier(Open))
    if sym.isAllOf(Abstract | Open) then
      warn(RedundantModifier(Open))
    if sym.is(Open) && sym.isLocal then
      warn(RedundantModifier(Open))
    // Skip ModuleVal since the annotation will also be on the ModuleClass
    if sym.hasAnnotation(defn.TailrecAnnot) then
      if !sym.isOneOf(Method | ModuleVal) then
        fail(TailrecNotApplicable(sym))
      else if sym.is(Inline) then
        fail(em"Inline methods cannot be @tailrec")
    if sym.hasAnnotation(defn.TargetNameAnnot) && sym.isClass && sym.isTopLevelClass then
      fail(TargetNameOnTopLevelClass(sym))
    if sym.hasAnnotation(defn.PublicInBinaryAnnot) then
      if sym.is(Enum) then fail(em"@publicInBinary cannot be used on enum definitions")
      else if sym.isType && !sym.is(Module) && !(sym.is(Given) || sym.companionModule.is(Given)) then fail(em"@publicInBinary cannot be used on ${sym.showKind} definitions")
      else if !sym.owner.isClass && !(sym.is(Param) && sym.owner.isConstructor) then fail(em"@publicInBinary cannot be used on local definitions")
      else if sym.is(ParamAccessor) && sym.is(Private) then fail(em"@publicInBinary cannot be non `val` constructor parameters")
      else if sym.is(Private) && !sym.privateWithin.exists && !sym.isConstructor then fail(em"@publicInBinary cannot be used on private definitions\n\nConsider using `private[${sym.owner.name}]` or `protected` instead")
    if (sym.hasAnnotation(defn.NativeAnnot)) {
      if (!sym.is(Deferred))
        fail(NativeMembersMayNotHaveImplementation(sym))
      else if(sym.owner.is(Trait))
        fail(TraitMayNotDefineNativeMethod(sym))
    }
    else if (sym.is(Deferred, butNot = Param) && !sym.isType && !sym.isSelfSym) {
      if (!sym.owner.isClass || sym.owner.is(Module) || sym.owner.isAnonymousClass)
        fail(OnlyClassesCanHaveDeclaredButUndefinedMembers(sym))
      checkWithDeferred(Private)
      checkWithDeferred(Final)
    }
    if (sym.isValueClass && sym.is(Trait) && !sym.isRefinementClass)
      fail(CannotExtendAnyVal(sym))
    if (sym.isConstructor && !sym.isPrimaryConstructor && sym.owner.is(Trait, butNot = JavaDefined))
      val addendum = if ctx.settings.Ydebug.value then s" ${sym.owner.flagsString}" else ""
      fail(em"Traits cannot have secondary constructors$addendum")
    checkApplicable(Inline, sym.isTerm && !sym.isOneOf(Mutable | Module))
    checkApplicable(Lazy, !sym.isOneOf(Method | Mutable))
    if (sym.isType && !sym.isOneOf(Deferred | JavaDefined))
      for (cls <- sym.allOverriddenSymbols.filter(_.isClass)) {
        fail(CannotHaveSameNameAs(sym, cls, CannotHaveSameNameAs.CannotBeOverridden))
        sym.setFlag(Private) // break the overriding relationship by making sym Private
      }
    if sym.isWrappedToplevelDef && !sym.isType && sym.flags.is(Infix, butNot = Extension) then
      fail(ModifierNotAllowedForDefinition(Flags.Infix, s"A top-level ${sym.showKind} cannot be infix."))
    checkApplicable(Erased,
      !sym.isOneOf(MutableOrLazy, butNot = Given) && !sym.isType || sym.isClass)
    checkCombination(Final, Open)
    checkCombination(Sealed, Open)
    checkCombination(Final, Sealed)
    checkCombination(Private, Protected)
    checkCombination(Abstract, Override)
    checkCombination(Private, Override)
    if sym.isType && !sym.isClass then checkCombination(Private, Opaque)
    checkCombination(Lazy, Inline)
    // The issue with `erased inline` is that the erased semantics get lost
    // as the code is inlined and the reference is removed before the erased usage check.
    checkCombination(Erased, Inline)
    checkNoConflict(Lazy, ParamAccessor, em"parameter may not be `lazy`")
  }

  /** Check for illegal or redundant modifiers on modules. This is done separately
   *  from checkWellformed, since the original module modifiers don't surivive desugaring
   */
  def checkWellFormedModule(mdef: untpd.ModuleDef)(using Context) =
    val mods = mdef.mods
    def flagSourcePos(flag: FlagSet) =
      mods.mods.find(_.flags == flag).getOrElse(mdef).srcPos
    if mods.is(Abstract) then
      report.error(ModifierNotAllowedForDefinition(Abstract), flagSourcePos(Abstract))
    if mods.is(Sealed) then
      report.error(ModifierNotAllowedForDefinition(Sealed), flagSourcePos(Sealed))
    if mods.is(Final, butNot = Synthetic) then
      report.warning(RedundantModifier(Final), flagSourcePos(Final))
    if mods.is(Infix) then
      report.error(ModifierNotAllowedForDefinition(Infix), flagSourcePos(Infix))

  /** Check the type signature of the symbol `M` defined by `tree` does not refer
   *  to a private type or value which is invisible at a point where `M` is still
   *  visible.
   *
   *  As an exception, we allow references to type aliases if the underlying
   *  type of the alias is not a leak, and if `sym` is not a type. The rationale
   *  for this is that the inferred type of a term symbol might contain leaky
   *  aliases which should be removed (see leak-inferred.scala for an example),
   *  but a type symbol definition will not contain leaky aliases unless the
   *  user wrote them, so we can ask the user to change his definition. The more
   *  practical reason for not transforming types is that `checkNoPrivateLeaks`
   *  can force a lot of denotations, and this restriction means that we never
   *  need to run `TypeAssigner#avoidPrivateLeaks` on type symbols when
   *  unpickling, which avoids some issues related to forcing order.
   *
   *  See i997.scala for negative tests, and i1130.scala for a case where it
   *  matters that we transform leaky aliases away.
   *
   *  @return The `info` of `sym`, with problematic aliases expanded away.
   */
  def checkNoPrivateLeaks(sym: Symbol)(using Context): Type = {
    class NotPrivate extends TypeMap {
      var errors: List[Message] = Nil
      private var inCaptureSet: Boolean = false

      def accessBoundary(sym: Symbol): Symbol =
        if (sym.is(Private) || !sym.owner.isClass) sym.owner
        else if (sym.privateWithin.exists) sym.privateWithin
        else if (sym.is(Package)) sym
        else accessBoundary(sym.owner)

      val symBoundary = accessBoundary(sym)

      /** Is `other` leaked outside its access boundary ?
       *  @pre  The signature of `sym` refers to `other`
       */
      def isLeaked(other: Symbol) =
        other.is(Private, butNot = TypeParam)
        && {
          val otherBoundary = other.owner
          val otherLinkedBoundary = otherBoundary.linkedClass
          !(symBoundary.isContainedIn(otherBoundary) ||
            otherLinkedBoundary.exists && symBoundary.isContainedIn(otherLinkedBoundary))
        }
        && !(inCaptureSet && other.isAllOf(LocalParamAccessor))
            // class parameters in capture sets are not treated as leaked since in
            // phase CheckCaptures these are treated as normal vals.

      def apply(tp: Type): Type = tp match {
        case tp: NamedType =>
          val prevErrors = errors
          var tp1 =
            if (isLeaked(tp.symbol)) {
              errors =
                em"non-private ${sym.showLocated} refers to private ${tp.symbol}\nin its type signature ${sym.info}"
                :: errors
              tp
            }
            else mapOver(tp)
          if ((errors ne prevErrors) && tp.info.isTypeAlias) {
            // try to dealias to avoid a leak error
            val savedErrors = errors
            errors = prevErrors
            val tp2 = apply(tp.superType)
            if (errors eq prevErrors) tp1 = tp2
            else errors = savedErrors
          }
          tp1
        case tp: ClassInfo =>
          def transformedParent(tp: Type): Type = tp match {
            case ref: TypeRef => ref
            case ref: AppliedType => ref
            case AnnotatedType(parent, annot) =>
              AnnotatedType(transformedParent(parent), annot)
            case _ => defn.ObjectType // can happen if class files are missing
          }
          tp.derivedClassInfo(
            prefix = apply(tp.prefix),
            declaredParents =
              tp.declaredParents.map(p => transformedParent(apply(p)))
            )
        case tp @ AnnotatedType(underlying, annot) if annot.symbol.isRetainsLike =>
          val underlying1 = this(underlying)
          val saved = inCaptureSet
          inCaptureSet = true
          val annot1 = annot.mapWith(this)
          inCaptureSet = saved
          derivedAnnotatedType(tp, underlying1, annot1)
        case _ =>
          mapOver(tp)
      }
    }
    val notPrivate = new NotPrivate
    val info = notPrivate(sym.info)
    notPrivate.errors.foreach(report.errorOrMigrationWarning(_, sym.srcPos, MigrationVersion.Scala2to3))
    info
  }

  /** Verify classes extending AnyVal meet the requirements */
  def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(using Context): Unit = {
    def checkValueClassMember(stat: Tree) = stat match {
      case _: TypeDef if stat.symbol.isClass =>
        report.error(ValueClassesMayNotDefineInner(clazz, stat.symbol), stat.srcPos)
      case _: ValDef if !stat.symbol.is(ParamAccessor) =>
        report.error(ValueClassesMayNotDefineNonParameterField(clazz, stat.symbol), stat.srcPos)
      case _: DefDef if stat.symbol.isConstructor =>
        report.error(ValueClassesMayNotDefineASecondaryConstructor(clazz, stat.symbol), stat.srcPos)
      case _: MemberDef | _: Import | EmptyTree =>
      // ok
      case _ =>
        report.error(ValueClassesMayNotContainInitalization(clazz), stat.srcPos)
    }
    if (clazz.isDerivedValueClass) {
      if (clazz.is(Trait))
        report.error(CannotExtendAnyVal(clazz), clazz.srcPos)
      if clazz.is(Module) then
        report.error(CannotExtendAnyVal(clazz), clazz.srcPos)
      if (clazz.is(Abstract))
        report.error(ValueClassesMayNotBeAbstract(clazz), clazz.srcPos)
      if (!clazz.isStatic)
        report.error(ValueClassesMayNotBeContainted(clazz), clazz.srcPos)
      if (isDerivedValueClass(underlyingOfValueClass(clazz.asClass).classSymbol))
        report.error(ValueClassesMayNotWrapAnotherValueClass(clazz), clazz.srcPos)
      else {
        val clParamAccessors = clazz.asClass.paramAccessors.filter { param =>
          param.isTerm && !param.is(Flags.Accessor)
        }
        clParamAccessors match {
          case param :: params =>
            if (param.is(Mutable))
              report.error(ValueClassParameterMayNotBeAVar(clazz, param), param.srcPos)
            if (param.info.isInstanceOf[ExprType])
              report.error(ValueClassParameterMayNotBeCallByName(clazz, param), param.srcPos)
            if (param.is(Erased))
              report.error("value class first parameter cannot be `erased`", param.srcPos)
            else
              for (p <- params if !p.is(Erased))
                report.error("value class can only have one non `erased` parameter", p.srcPos)
          case Nil =>
            report.error(ValueClassNeedsOneValParam(clazz), clazz.srcPos)
        }
      }
      stats.foreach(checkValueClassMember)
    }
  }

  /** Check the inline override methods only use inline parameters if they override an inline parameter. */
  def checkInlineOverrideParameters(sym: Symbol)(using Context): Unit =
    lazy val params = sym.paramSymss.flatten
    for
      sym2 <- sym.allOverriddenSymbols
      (p1, p2) <- sym.paramSymss.flatten.lazyZip(sym2.paramSymss.flatten)
      if p1.is(Inline) != p2.is(Inline)
    do
      report.error(
          if p2.is(Inline) then "Cannot override inline parameter with a non-inline parameter"
          else "Cannot override non-inline parameter with an inline parameter",
          p1.srcPos)

  def checkValue(tree: Tree)(using Context): Unit =
    val sym = tree.tpe.termSymbol
    if sym.isNoValue && !ctx.isJava then
      report.error(JavaSymbolIsNotAValue(sym), tree.srcPos)

  /** Check that `tree` refers to a value, unless `tree` is selected or applied
   *  (singleton types x.type don't count as selections).
   */
  def checkValue(tree: Tree, proto: Type)(using Context): tree.type =
    tree match
      case tree: RefTree if tree.name.isTermName =>
        proto match
          case _: SelectionProto if proto ne SingletonTypeProto => // no value check
          case _: FunOrPolyProto => // no value check
          case _ => checkValue(tree)
      case _ =>
    tree

  /** Check that experimental language imports in `trees`
   *  are done only in experimental scopes, or in a top-level
   *  scope with only @experimental definitions.
   */
  def checkExperimentalImports(trees: List[Tree])(using Context): Unit =

    def nonExperimentalStat(trees: List[Tree]): Tree = trees match
      case (_: Import | EmptyTree) :: rest =>
        nonExperimentalStat(rest)
      case (tree @ TypeDef(_, impl: Template)) :: rest if tree.symbol.isPackageObject =>
        nonExperimentalStat(impl.body).orElse(nonExperimentalStat(rest))
      case (tree: PackageDef) :: rest =>
        nonExperimentalStat(tree.stats).orElse(nonExperimentalStat(rest))
      case (tree: MemberDef) :: rest =>
        if tree.symbol.isExperimental || tree.symbol.is(Synthetic) then
          nonExperimentalStat(rest)
        else
          tree
      case tree :: rest =>
        tree
      case Nil =>
        EmptyTree

    for case imp @ Import(qual, selectors) <- trees do
      def isAllowedImport(sel: untpd.ImportSelector) =
        val name = Feature.experimental(sel.name)
        name == Feature.scala2macros
        || name == Feature.erasedDefinitions
        || name == Feature.captureChecking

      languageImport(qual) match
        case Some(nme.experimental)
        if !ctx.owner.isInExperimentalScope && !selectors.forall(isAllowedImport) =>
          def check(stable: => String) =
            Feature.checkExperimentalFeature("features", imp.srcPos,
              s"\n\nNote: the scope enclosing the import is not considered experimental because it contains the\nnon-experimental $stable")
          if ctx.owner.is(Package) then
            // allow top-level experimental imports if all definitions are @experimental
            nonExperimentalStat(trees) match
              case EmptyTree =>
              case tree: MemberDef => check(i"${tree.symbol}")
              case tree => check(i"expression ${tree}")
          else Feature.checkExperimentalFeature("features", imp.srcPos)
        case _ =>
  end checkExperimentalImports

  /** Checks that PolyFunction only have valid refinements.
   *
   *  It only supports `apply` methods with one parameter list and optional type arguments.
   */
  def checkPolyFunctionType(tree: Tree)(using Context): Unit = new TreeTraverser {
    def traverse(tree: Tree)(using Context): Unit = tree match
      case tree: RefinedTypeTree if tree.tpe.derivesFrom(defn.PolyFunctionClass) =>
        if tree.refinements.isEmpty then
          reportNoRefinements(tree.srcPos)
        tree.refinements.foreach {
          case refinement: DefDef if refinement.name != nme.apply =>
            report.error("PolyFunction only supports apply method refinements", refinement.srcPos)
          case refinement: DefDef if !defn.PolyFunctionOf.isValidPolyFunctionInfo(refinement.tpe.widen) =>
            report.error("Implementation restriction: PolyFunction apply must have exactly one parameter list and optionally type arguments. No by-name nor varags are allowed.", refinement.srcPos)
          case _ =>
        }
      case _: RefTree if tree.symbol == defn.PolyFunctionClass =>
        reportNoRefinements(tree.srcPos)
      case _ =>
        traverseChildren(tree)

    def reportNoRefinements(pos: SrcPos) =
      report.error("PolyFunction subtypes must refine the apply method", pos)
  }.traverse(tree)

  /** Check that users do not extend the `PolyFunction` trait.
   *  We only allow compiler generated `PolyFunction`s.
   */
  def checkPolyFunctionExtension(templ: Template)(using Context): Unit =
    templ.parents.find(_.tpe.derivesFrom(defn.PolyFunctionClass)) match
      case Some(parent) => report.error(s"`PolyFunction` marker trait is reserved for compiler generated refinements", parent.srcPos)
      case None =>
}

trait Checking {

  import tpd.*

  def checkNonCyclic(sym: Symbol, info: TypeBounds, reportErrors: Boolean)(using Context): Type =
    Checking.checkNonCyclic(sym, info, reportErrors)

  def checkNonCyclicInherited(joint: Type, parents: List[Type], decls: Scope, pos: SrcPos)(using Context): Unit =
    Checking.checkNonCyclicInherited(joint, parents, decls, pos)

  /** Check that type `tp` is stable. */
  def checkStable(tp: Type, pos: SrcPos, kind: String)(using Context): Unit =
    if !tp.isStable && !tp.isErroneous then report.error(NotAPath(tp, kind), pos)

  /** Check that all type members of `tp` have realizable bounds */
  def checkRealizableBounds(cls: Symbol, pos: SrcPos)(using Context): Unit = {
    val rstatus = boundsRealizability(cls.thisType)
    if (rstatus ne Realizable)
      report.error(em"$cls cannot be instantiated since it${rstatus.msg}", pos)
  }

  /** Check that pattern `pat` is irrefutable for scrutinee type `sel.tpe`.
   *  This means `sel` is either marked @unchecked or `sel.tpe` conforms to the
   *  pattern's type. If pattern is an UnApply, also check that the extractor is
   *  irrefutable, and do the check recursively.
   */
  def checkIrrefutable(sel: Tree, pat: Tree, isPatDef: Boolean)(using Context): Boolean = {
    val pt = sel.tpe

    enum Reason:
      case NonConforming, RefutableExtractor

    def fail(pat: Tree, pt: Type, reason: Reason): Boolean = {
      import Reason.*
      val message = reason match
        case NonConforming =>
          var reportedPt = pt.dropAnnot(defn.UncheckedAnnot)
          if !pat.tpe.isSingleton then reportedPt = reportedPt.widen
          val problem = if pat.tpe <:< reportedPt then "is more specialized than" else "does not match"
          em"pattern's type ${pat.tpe} $problem the right hand side expression's type $reportedPt"
        case RefutableExtractor =>
          val extractor = pat match
            case UnApply(fn, _, _) =>
              tpd.funPart(fn) match
                case Select(id, _) if !id.isEmpty => id.show
                case _ => ""
            case QuotePattern(_, body, _) =>
              if body.isTerm then "'{...}" else "'[...]"
          if extractor.isEmpty then em"pattern binding uses refutable extractor"
          else em"pattern binding uses refutable extractor `$extractor`"

      val fix =
        if isPatDef then "adding `: @unchecked` after the expression"
        else "adding the `case` keyword before the full pattern"
      val addendum =
        if isPatDef then "may result in a MatchError at runtime"
        else "will result in a filtering for expression (using `withFilter`)"
      val usage = reason match
        case NonConforming => "the narrowing"
        case RefutableExtractor => "this usage"
      val pos =
        if isPatDef then reason match
          case NonConforming => sel.srcPos
          case RefutableExtractor => pat.source.atSpan(pat.span union sel.span)
        else pat.srcPos
      def rewriteMsg = Message.rewriteNotice("This patch", `3.2-migration`)
      report.errorOrMigrationWarning(
        message.append(
          i"""|
              |
              |If $usage is intentional, this can be communicated by $fix,
              |which $addendum.$rewriteMsg"""),
        pos,
        // we tighten for-comprehension without `case` to error in 3.4,
        // but we keep pat-defs as warnings for now ("@unchecked"),
        // until we propose an alternative way to assert exhaustivity to the typechecker.
        if isPatDef then MigrationVersion.ForComprehensionUncheckedPathDefs
        else MigrationVersion.ForComprehensionPatternWithoutCase
      )
      false
    }

    def check(pat: Tree, pt: Type): Boolean =
      pt.isTupleXXLExtract(pat.tpe) // See isTupleXXLExtract, fixes TupleXXL parameter type
      || pt <:< pat.tpe
      || fail(pat, pt, Reason.NonConforming)

    def recur(pat: Tree, pt: Type): Boolean =
      !sourceVersion.isAtLeast(`3.2`)
      || pt.hasAnnotation(defn.UncheckedAnnot)
      || {
        patmatch.println(i"check irrefutable $pat: ${pat.tpe} against $pt")
        pat match
          case Bind(_, pat1) =>
            recur(pat1, pt)
          case UnApply(fn, implicits, pats) =>
            check(pat, pt) &&
            (isIrrefutable(fn, pats.length) || fail(pat, pt, Reason.RefutableExtractor)) && {
              val argPts = unapplyArgs(fn.tpe.widen.finalResultType, fn, pats, pat.srcPos)
              pats.corresponds(argPts)(recur)
            }
          case Alternative(pats) =>
            pats.forall(recur(_, pt))
          case Typed(arg, tpt) =>
            check(pat, pt) && recur(arg, pt)
          case Ident(nme.WILDCARD) =>
            true
          case pat: QuotePattern =>
            isIrrefutableQuotePattern(pat, pt) || fail(pat, pt, Reason.RefutableExtractor)
          case _ =>
            check(pat, pt)
        }

    recur(pat, pt)
  }

  private def checkLegalImportOrExportPath(path: Tree, kind: String)(using Context): Unit = {
    checkStable(path.tpe, path.srcPos, kind)
    if (!ctx.isAfterTyper) Checking.checkRealizable(path.tpe, path.srcPos)
    if !isIdempotentExpr(path) && !path.tpe.isErroneous then
      report.error(em"import prefix is not a pure expression", path.srcPos)
  }

  /** Check that `path` is a legal prefix for an import clause */
  def checkLegalImportPath(path: Tree)(using Context): Unit =
    checkLegalImportOrExportPath(path, "import prefix")
    languageImport(path) match
      case Some(prefix) =>
        val required =
          if prefix == nme.experimental then defn.LanguageExperimentalModule
          else if prefix == nme.deprecated then defn.LanguageDeprecatedModule
          else defn.LanguageModule
        if path.symbol != required then
          report.error(em"import looks like a language import, but refers to something else: ${path.symbol.showLocated}", path.srcPos)
      case None =>
        val foundClasses = path.tpe.classSymbols
        if foundClasses.contains(defn.LanguageModule.moduleClass)
           || foundClasses.contains(defn.LanguageExperimentalModule.moduleClass)
        then
          report.error(em"no aliases can be used to refer to a language import", path.srcPos)

  /** Check that `path` is a legal prefix for an export clause */
  def checkLegalExportPath(path: Tree, selectors: List[untpd.ImportSelector])(using Context): Unit =
    checkLegalImportOrExportPath(path, "export prefix")
    if
      selectors.exists(_.isWildcard)
      && path.tpe.classSymbol.is(PackageClass)
    then
      // we restrict wildcard export from package as incremental compilation does not yet
      // register a dependency on "all members of a package" - see https://github.com/sbt/zinc/issues/226
      report.error(
        em"Implementation restriction: ${path.tpe.classSymbol} is not a valid prefix for a wildcard export, as it is a package",
        path.srcPos)

  /** Check that the definition name isn't root. */
  def checkNonRootName(name: Name, nameSpan: Span)(using Context): Unit =
    if name == nme.ROOTPKG then
      report.error(em"Illegal use of root package name.", ctx.source.atSpan(nameSpan))

  /** Check that module `sym` does not clash with a class of the same name
   *  that is concurrently compiled in another source file.
   */
  def checkNoModuleClash(sym: Symbol)(using Context): Unit =
    val effectiveOwner = sym.effectiveOwner
    if effectiveOwner.is(Package)
       && effectiveOwner.info.member(sym.name.moduleClassName).symbol.isAbsent()
    then
      val conflicting = effectiveOwner.info.member(sym.name.toTypeName).symbol
      if conflicting.exists then
        report.error(AlreadyDefined(sym.name, effectiveOwner, conflicting), sym.srcPos)

 /**  Check that `tp` is a class type.
  *   Also, if `traitReq` is true, check that `tp` is a trait.
  *   Also, if `stablePrefixReq` is true and phase is not after RefChecks,
  *   check that class prefix is stable.
   *  @return  `tp` itself if it is a class or trait ref, ObjectType if not.
   */
  def checkClassType(tp: Type, pos: SrcPos, traitReq: Boolean, stablePrefixReq: Boolean)(using Context): Type =
    tp.underlyingClassRef(refinementOK = false) match {
      case tref: TypeRef =>
        if (traitReq && !tref.symbol.is(Trait)) report.error(TraitIsExpected(tref.symbol), pos)
        if (stablePrefixReq && ctx.phase <= refchecksPhase) checkStable(tref.prefix, pos, "class prefix")
        tp
      case _ =>
        report.error(NotClassType(tp), pos)
        defn.ObjectType
    }

  /** If `sym` is an old-style implicit conversion, check that implicit conversions are enabled.
   *  @pre  sym.is(GivenOrImplicit)
   */
  def checkImplicitConversionDefOK(sym: Symbol)(using Context): Unit =
    if sym.isOldStyleImplicitConversion(directOnly = true) then
      checkFeature(
        nme.implicitConversions,
        i"Definition of implicit conversion $sym",
        ctx.owner.topLevelClass,
        sym.srcPos)

  /** If `tree` is an application of a new-style implicit conversion (using the apply
   *  method of a `scala.Conversion` instance), check that the expected type is
   *  annotated with @$into or that implicit conversions are enabled.
   */
  def checkImplicitConversionUseOK(tree: Tree, expected: Type)(using Context): Unit =
    val sym = tree.symbol
    if sym.name == nme.apply
       && sym.owner.derivesFrom(defn.ConversionClass)
       && !sym.info.isErroneous
       && !expected.isInto
    then
      def conv = methPart(tree) match
        case Select(qual, _) => qual.symbol.orElse(sym.owner)
        case _ => sym.owner
      checkFeature(nme.implicitConversions,
        i"Use of implicit conversion ${conv.showLocated}", NoSymbol, tree.srcPos)

  private def infixOKSinceFollowedBy(tree: untpd.Tree): Boolean = tree match {
    case _: untpd.Block | _: untpd.Match => true
    case _ => false
  }

  /** Check that `tree` is a valid infix operation. That is, if the
   *  operator is alphanumeric, it must be declared `infix`.
   */
  def checkValidInfix(tree: untpd.InfixOp, meth: Symbol)(using Context): Unit = {
    tree.op match {
      case id @ Ident(name: Name) =>
        def methCompiledBeforeDeprecation =
          meth.tastyInfo match
            case Some(info) => info.version.major == 28 && info.version.minor < 4 // compiled before 3.4
            case _ => false // compiled with the current compiler
        name.toTermName match {
          case name: SimpleName
          if !untpd.isBackquoted(id) &&
             !name.isOperatorName &&
             !meth.isDeclaredInfix &&
             !meth.maybeOwner.is(Scala2x) &&
             !methCompiledBeforeDeprecation &&
             !infixOKSinceFollowedBy(tree.right) =>
            val (kind, alternative) =
              if (ctx.mode.is(Mode.Type))
                ("type", (n: Name) => s"prefix syntax $n[...]")
              else if (ctx.mode.is(Mode.Pattern))
                ("extractor", (n: Name) => s"prefix syntax $n(...)")
              else
                ("method", (n: Name) => s"method syntax .$n(...)")
            def rewriteMsg = Message.rewriteNotice("The latter", version = `3.4-migration`)
            report.errorOrMigrationWarning(
              em"""Alphanumeric $kind $name is not declared ${hlAsKeyword("infix")}; it should not be used as infix operator.
                  |Instead, use ${alternative(name)} or backticked identifier `$name`.$rewriteMsg""",
              tree.op.srcPos,
              MigrationVersion.AlphanumericInfix)
            if MigrationVersion.AlphanumericInfix.needsPatch then
              patch(Span(tree.op.span.start, tree.op.span.start), "`")
              patch(Span(tree.op.span.end, tree.op.span.end), "`")
          case _ =>
        }
    }
  }

  /** Issue a feature warning if feature is not enabled */
  def checkFeature(name: TermName,
                   description: => String,
                   featureUseSite: Symbol,
                   pos: SrcPos)(using Context): Unit =
    if !Feature.enabled(name) then
      report.featureWarning(name.toString, description, featureUseSite, required = false, pos)

  /** Check that `tp` is a class type and that any top-level type arguments in this type
   *  are feasible, i.e. that their lower bound conforms to their upper bound. If a type
   *  argument is infeasible, issue and error and continue with upper bound.
   */
  def checkFeasibleParent(tp: Type, pos: SrcPos, where: => String = "")(using Context): Type = {
    def checkGoodBounds(tp: Type) = tp match {
      case tp @ TypeBounds(lo, hi) if !(lo <:< hi) =>
        report.error(em"no type exists between low bound $lo and high bound $hi$where", pos)
        TypeBounds(hi, hi)
      case _ =>
        tp
    }
    tp match {
      case tp @ AndType(tp1, tp2) =>
        report.error(em"conflicting type arguments$where", pos)
        tp1
      case tp @ AppliedType(tycon, args) =>
        tp.derivedAppliedType(tycon, args.mapConserve(checkGoodBounds))
      case tp: RefinedType =>
        tp.derivedRefinedType(refinedInfo = checkGoodBounds(tp.refinedInfo))
      case _ =>
        tp
    }
  }

  /** A hook to exclude selected symbols from double declaration check */
  def excludeFromDoubleDeclCheck(sym: Symbol)(using Context): Boolean = false

  /** Check that class does not declare same symbol twice */
  def checkNoDoubleDeclaration(cls: Symbol)(using Context): Unit = {
    val seen = new mutable.HashMap[Name, List[Symbol]].withDefaultValue(Nil)
    typr.println(i"check no double declarations $cls")

    def checkDecl(decl: Symbol): Unit = {
      for (other <- seen(decl.name) if !decl.isAbsent() && !other.isAbsent()) {
        typr.println(i"conflict? $decl $other")
        def javaFieldMethodPair =
          decl.is(JavaDefined) && other.is(JavaDefined) &&
          decl.is(Method) != other.is(Method)
        def staticNonStaticPair = decl.isScalaStatic != other.isScalaStatic
        if (decl.matches(other) && !javaFieldMethodPair && !staticNonStaticPair) {
          def doubleDefError(decl: Symbol, other: Symbol): Unit =
            if (!decl.info.isErroneous && !other.info.isErroneous)
              report.error(DoubleDefinition(decl, other, cls), decl.srcPos)
          if decl.name.is(DefaultGetterName) && ctx.reporter.errorsReported then
            () // do nothing; we already have reported an error that overloaded variants cannot have default arguments
          else if (decl is Synthetic) doubleDefError(other, decl)
          else doubleDefError(decl, other)
        }
        if decl.hasDefaultParams && other.hasDefaultParams then
          report.error(em"two or more overloaded variants of $decl have default arguments", decl.srcPos)
          decl.resetFlag(HasDefaultParams)
      }
      if (!excludeFromDoubleDeclCheck(decl))
        seen(decl.name) = decl :: seen(decl.name)
    }

    cls.info.decls.foreach(checkDecl)
    cls.info match {
      case ClassInfo(_, _, _, _, selfSym: Symbol) => checkDecl(selfSym)
      case _ =>
    }
  }

  def checkParentCall(call: Tree, caller: ClassSymbol)(using Context): Unit =
    if (!ctx.isAfterTyper) {
      val called = call.tpe.classSymbol
      if (called.is(JavaAnnotation))
        report.error(em"${called.name} must appear without any argument to be a valid class parent because it is a Java annotation", call.srcPos)
      if (caller.is(Trait))
        report.error(em"$caller may not call constructor of $called", call.srcPos)
      else if (called.is(Trait) && !caller.mixins.contains(called))
        report.error(em"""$called is already implemented by super${caller.superClass},
                   |its constructor cannot be called again""", call.srcPos)

      // Check that constructor call is of the form _.<init>(args1)...(argsN).
      // This guards against calls resulting from inserted implicits or applies.
      def checkLegalConstructorCall(tree: Tree, encl: Tree, kind: String): Unit = tree match {
        case Apply(fn, _) => checkLegalConstructorCall(fn, tree, "")
        case TypeApply(fn, _) => checkLegalConstructorCall(fn, tree, "type ")
        case Select(_, nme.CONSTRUCTOR) => // ok
        case _ => report.error(em"too many ${kind}arguments in parent constructor", encl.srcPos)
      }
      call match {
        case Apply(fn, _) => checkLegalConstructorCall(fn, call, "")
        case _ =>
      }
    }

  /** Check that `tpt` does not define a higher-kinded type */
  def checkSimpleKinded(tpt: Tree)(using Context): Tree =
    if (!tpt.tpe.hasSimpleKind && !ctx.isJava)
        // be more lenient with missing type params in Java,
        // needed to make pos/java-interop/t1196 work.
      errorTree(tpt, MissingTypeParameterFor(tpt.tpe))
    else tpt

  /** Verify classes extending AnyVal meet the requirements */
  def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(using Context): Unit =
    Checking.checkDerivedValueClass(clazz, stats)

  /** Check that case classes are not inherited by case classes.
   */
  def checkCaseInheritance(parent: Symbol, caseCls: ClassSymbol, pos: SrcPos)(using Context): Unit =
    parent match {
      case parent: ClassSymbol =>
        if (parent.is(Case))
          report.error(em"""case $caseCls has case ancestor $parent, but case-to-case inheritance is prohibited.
                        |To overcome this limitation, use extractors to pattern match on non-leaf nodes.""", pos)
        else checkCaseInheritance(parent.superClass, caseCls, pos)
      case _ =>
    }

  /** Check that method parameter types do not reference their own parameter
   *  or later parameters in the same parameter section.
   */
  def checkNoForwardDependencies(vparams: List[ValDef])(using Context): Unit = vparams match {
    case vparam :: vparams1 =>
      vparam.tpt.foreachSubTree {
        case id: Ident if vparams.exists(_.symbol == id.symbol) =>
          report.error(em"illegal forward reference to method parameter", id.srcPos)
        case _ =>
      }
      checkNoForwardDependencies(vparams1)
    case Nil =>
  }

  /** Check that all named types that form part of this type have a denotation.
   *  Called on inferred (result) types of ValDefs and DefDefs.
   *  This could fail for types where the member was originally available as part
   *  of the self type, yet is no longer visible once the `this` has been replaced
   *  by some other prefix. See neg/i3083.scala
   */
  def checkMembersOK(tp: Type, pos: SrcPos)(using Context): Type = {
    var ok = true
    val check: Type => Unit = {
      case ref: NamedType =>
        val d = try ref.denot catch { case ex: TypeError => NoDenotation }
        if (!d.exists) {
          report.error(em"$ref is not defined in inferred type $tp", pos)
          ok = false
        }
      case _ =>
    }
    tp.foreachPart(check, StopAt.Static)
    if (ok) tp else UnspecifiedErrorType
  }

  /** Check that all non-synthetic references of the form `<ident>` or
   *  `this.<ident>` in `tree` that refer to a member of `badOwner` are
   *  `allowed`. Also check that there are no other explicit `this` references
   *  to `badOwner`.
   */
  def checkRefsLegal(tree: tpd.Tree, badOwner: Symbol, allowed: (Name, Symbol) => Boolean, where: String)(using Context): Unit = {
    val checker = new TreeTraverser {
      def traverse(t: Tree)(using Context) = {
        def check(owner: Symbol, checkedSym: Symbol) =
          if (t.span.isSourceDerived && owner == badOwner)
            t match {
              case t: RefTree if allowed(t.name, checkedSym) =>
              case _ => report.error(em"illegal reference to $checkedSym from $where", t.srcPos)
            }
        val sym = t.symbol
        t match {
          case Ident(_) | Select(This(_), _) => check(sym.maybeOwner, sym)
          case This(_) => check(sym, sym)
          case _ => traverseChildren(t)
        }
      }
    }
    checker.traverse(tree)
  }

  /** Check that user-defined (result) type is fully applied */
  def checkFullyAppliedType(tree: Tree)(using Context): Unit = tree match
    case TypeBoundsTree(lo, hi, alias) =>
      checkFullyAppliedType(lo)
      checkFullyAppliedType(hi)
      checkFullyAppliedType(alias)
    case Annotated(arg, annot) =>
      checkFullyAppliedType(arg)
    case LambdaTypeTree(_, body) =>
      checkFullyAppliedType(body)
    case _: TypeTree =>
    case _ =>
      if tree.tpe.typeParams.nonEmpty then
        val what = if tree.symbol.exists then tree.symbol.show else i"type $tree"
        report.error(em"$what takes type parameters", tree.srcPos)

  /** Check that we are in an inline context (inside an inline method or in inline code) */
  def checkInInlineContext(what: String, pos: SrcPos)(using Context): Unit =
    if !Inlines.inInlineMethod && !ctx.isInlineContext then
      report.error(em"$what can only be used in an inline method", pos)

  /** Check that the class corresponding to this tree is either a Scala or Java annotation.
   *
   *  @return The original tree or an error tree in case `tree` isn't a valid
   *          annotation or already an error tree.
   */
  def checkAnnotClass(tree: Tree)(using Context): Tree =
    if tree.tpe.isError then
      return tree
    val cls = Annotations.annotClass(tree)
    if cls.is(JavaDefined) then
      if !cls.is(JavaAnnotation) then
        errorTree(tree, em"$cls is not a valid Java annotation: it was not declared with `@interface`")
      else tree
    else if !cls.derivesFrom(defn.AnnotationClass) then
      errorTree(tree, em"$cls is not a valid Scala annotation: it does not extend `scala.annotation.Annotation`")
    else tree

  /** Check arguments of compiler-defined annotations */
  def checkAnnotArgs(tree: Tree)(using Context): tree.type =
    val cls = Annotations.annotClass(tree)
    tree match
      case Apply(tycon, arg :: Nil) if cls == defn.TargetNameAnnot =>
        arg match
          case Literal(Constant("")) =>
            report.error(em"target name cannot be empty", arg.srcPos)
          case Literal(_) => // ok
          case _ =>
            report.error(em"@${cls.name} needs a string literal as argument", arg.srcPos)
      case _ =>
    tree

  /** 1. Check that all case classes that extend `scala.reflect.Enum` are `enum` cases
   *  2. Check that parameterised `enum` cases do not extend java.lang.Enum.
   *  3. Check that only a static `enum` base class can extend java.lang.Enum.
   *  4. Check that user does not implement an `ordinal` method in the body of an enum class.
   */
  def checkEnum(cdef: untpd.TypeDef, cls: Symbol, firstParent: Symbol)(using Context): Unit = {
    def existingDef(sym: Symbol, clazz: ClassSymbol)(using Context): Symbol = // adapted from SyntheticMembers
      val existing = sym.matchingMember(clazz.thisType)
      if existing != sym && !existing.is(Deferred) then existing else NoSymbol
    def checkExistingOrdinal(using Context) =
      val decl = existingDef(defn.Enum_ordinal, cls.asClass)
      if decl.exists then
        if decl.owner == cls then
          report.error(em"the ordinal method of enum $cls can not be defined by the user", decl.srcPos)
        else
          report.error(em"enum $cls can not inherit the concrete ordinal method of ${decl.owner}", cdef.srcPos)
    def isEnumAnonCls =
      cls.isAnonymousClass
      && cls.owner.isTerm
      && (cls.owner.flagsUNSAFE.isAllOf(EnumCase)
        || ((cls.owner.name eq nme.DOLLAR_NEW) && cls.owner.flagsUNSAFE.isAllOf(Private | Synthetic)))
    val isJavaEnum = cls.derivesFrom(defn.JavaEnumClass)
    if isJavaEnum && cdef.mods.isEnumClass && !cls.isStatic then
      report.error(em"An enum extending java.lang.Enum must be declared in a static scope", cdef.srcPos)
    if !isEnumAnonCls then
      if cdef.mods.isEnumCase then
        if isJavaEnum then
          report.error(em"paramerized case is not allowed in an enum that extends java.lang.Enum", cdef.srcPos)
      else if cls.is(Case) || firstParent.is(Enum) then
        // Since enums are classes and Namer checks that classes don't extend multiple classes, we only check the class
        // parent.
        //
        // this test allows inheriting from `Enum` by hand;
        // see enum-List-control.scala.
        report.error(ClassCannotExtendEnum(cls, firstParent), cdef.srcPos)
    if cls.isEnumClass && !isJavaEnum then
      checkExistingOrdinal
  }

  /** Check that the firstParent for an enum case derives from the declaring enum class, if not, adds it as a parent
   *  after emitting an error.
   *
   *  This check will have no effect on simple enum cases as their parents are inferred by the compiler.
   */
  def checkEnumParent(cls: Symbol, firstParent: Symbol)(using Context): Unit =

    extension (sym: Symbol) def typeRefApplied(using Context): Type =
      sym.typeRef.appliedTo(sym.typeParams.map(_.info.loBound))

    def ensureParentDerivesFrom(enumCase: Symbol)(using Context) =
      val enumCls = enumCase.owner.linkedClass
      if !firstParent.derivesFrom(enumCls) then
        report.error(em"enum case does not extend its enum $enumCls", enumCase.srcPos)
        cls.info match
          case info: ClassInfo =>
            cls.info = info.derivedClassInfo(declaredParents = enumCls.typeRefApplied :: info.declaredParents)
          case _ =>

    val enumCase =
      if cls.flagsUNSAFE.isAllOf(EnumCase) then cls
      else if cls.isAnonymousClass && cls.owner.flagsUNSAFE.isAllOf(EnumCase) then cls.owner
      else NoSymbol
    if enumCase.exists then
      ensureParentDerivesFrom(enumCase)

  end checkEnumParent


  /** Check that all references coming from enum cases in an enum companion object
   *  are legal.
   *  @param  cdef     the enum companion object class
   *  @param  enumCtx  the context immediately enclosing the corresponding enum
   */
  def checkEnumCaseRefsLegal(cdef: TypeDef, enumCtx: Context)(using Context): Unit = {

    def checkEnumCaseOrDefault(stat: Tree, caseCtx: Context) = {

      def check(tree: Tree) = {
        // allow access to `sym` if a typedIdent just outside the enclosing enum
        // would have produced the same symbol without errors
        def allowAccess(name: Name, sym: Symbol): Boolean = {
          val testCtx = caseCtx.fresh.setNewTyperState()
          val ref = ctx.typer.typedIdent(untpd.Ident(name).withSpan(stat.span), WildcardType)(using testCtx)
          ref.symbol == sym && !testCtx.reporter.hasErrors
        }
        checkRefsLegal(tree, cdef.symbol, allowAccess, "enum case")
      }

      if (stat.symbol.isAllOf(EnumCase))
        stat match {
          case TypeDef(_, impl @ Template(DefDef(_, paramss, _, _), _, _, _)) =>
            paramss.foreach(_.foreach(check))
            impl.parents.foreach(check)
          case vdef: ValDef =>
            vdef.rhs match {
              case Block((clsDef @ TypeDef(_, impl: Template)) :: Nil, _)
              if clsDef.symbol.isAnonymousClass =>
                impl.parents.foreach(check)
              case _ =>
            }
          case _ =>
        }
      else if (stat.symbol.is(Module) && stat.symbol.linkedClass.isAllOf(EnumCase))
        stat match {
          case TypeDef(_, impl: Template) =>
            for (case (defaultGetter @
                  DefDef(DefaultGetterName(nme.CONSTRUCTOR, _), _, _, _)) <- impl.body)
              check(defaultGetter.rhs)
          case _ =>
        }
    }

    cdef.rhs match {
      case impl: Template =>
        def isEnumCase(stat: Tree) = stat match {
          case _: ValDef | _: TypeDef => stat.symbol.isAllOf(EnumCase)
          case _ => false
        }
        val cases =
          for (stat <- impl.body if isEnumCase(stat))
          yield untpd.ImportSelector(untpd.Ident(stat.symbol.name.toTermName))
        val caseImport: Import = Import(ref(cdef.symbol), cases)
        val caseCtx = enumCtx.importContext(caseImport, caseImport.symbol)
        for (stat <- impl.body) checkEnumCaseOrDefault(stat, caseCtx)
      case _ =>
    }
  }

  /** check that annotation `annot` is applicable to symbol `sym` */
  def checkAnnotApplicable(annot: Tree, sym: Symbol)(using Context): Boolean =
    !ctx.reporter.reportsErrorsFor {
      val annotCls = Annotations.annotClass(annot)
      val concreteAnnot = Annotations.ConcreteAnnotation(annot)
      val pos = annot.srcPos
      if (annotCls == defn.MainAnnot) {
        if (!sym.isRealMethod)
          report.error(em"main annotation cannot be applied to $sym", pos)
        if (!sym.owner.is(Module) || !sym.owner.isStatic)
          report.error(em"$sym cannot be a main method since it cannot be accessed statically", pos)
      }
      // TODO: Add more checks here
    }

  /** Check that symbol's external name does not clash with symbols defined in the same scope */
  def checkNoTargetNameConflict(stats: List[Tree])(using Context): Unit =
    var seen = Set[Name]()
    for stat <- stats do
      val sym = stat.symbol
      val tname = sym.targetName
      if tname != sym.name then
        val preExisting = ctx.effectiveScope.lookup(tname)
        if preExisting.exists || seen.contains(tname) then
          report.error(em"@targetName annotation ${'"'}$tname${'"'} clashes with other definition in same scope", stat.srcPos)
        if stat.isDef then seen += tname

  def checkMatchable(tp: Type, pos: SrcPos, pattern: Boolean)(using Context): Unit =
    if !tp.derivesFrom(defn.MatchableClass) && sourceVersion.isAtLeast(`future-migration`) then
      report.warning(MatchableWarning(tp, pattern), pos)

  /** Check that there is an implicit capability to throw a checked exception
   *  if the saferExceptions feature is turned on. Return that capability is it exists,
   *  EmptyTree otherwise.
   */
  def checkCanThrow(tp: Type, span: Span)(using Context): Tree =
    if Feature.enabled(Feature.saferExceptions) && tp.isCheckedException then
      ctx.typer.implicitArgTree(defn.CanThrowClass.typeRef.appliedTo(tp), span)
    else
      EmptyTree

  /** Check that catch can generate a good CanThrow exception */
  def checkCatch(pat: Tree, guard: Tree)(using Context): Unit = pat match
    case Typed(_: Ident, tpt) if isFullyDefined(tpt.tpe, ForceDegree.none) && guard.isEmpty =>
      // OK
    case Bind(_, pat1) =>
      checkCatch(pat1, guard)
    case _ =>
      val req =
        if guard.isEmpty then "for cases of the form `ex: T` where `T` is fully defined"
        else "if no pattern guard is given"
      report.error(
        em"""Implementation restriction: cannot generate CanThrow capability for this kind of catch.
            |CanThrow capabilities can only be generated $req.""",
        pat.srcPos)

  /** Check that tree does not define a context function type */
  def checkNoContextFunctionType(tree: Tree)(using Context): Unit =
    def recur(tp: Type): Unit = tp.dealias match
      case tp: HKTypeLambda => recur(tp.resType)
      case tp if defn.isContextFunctionType(tp) =>
        report.error(em"context function type cannot have opaque aliases", tree.srcPos)
      case _ =>
    recur(tree.tpe)

  /** (1) Check that every named import selector refers to a type or value member of the
   *  qualifier type.
   *  (2) Check that no import selector is renamed more than once.
   */
  def checkImportSelectors(qualType: Type, selectors: List[untpd.ImportSelector])(using Context): Unit =
    val originals = mutable.Set.empty[Name]
    val targets = mutable.Set.empty[Name]

    def checkIdent(sel: untpd.ImportSelector): Unit =
      if sel.name != nme.ERROR
          && !qualType.member(sel.name).exists
          && !qualType.member(sel.name.toTypeName).exists
      then
        report.error(NotAMember(qualType, sel.name, "value", WildcardType), sel.imported.srcPos)
      if sel.isUnimport then
        if originals.contains(sel.name) then
          report.error(UnimportedAndImported(sel.name, targets.contains(sel.name)), sel.imported.srcPos)
      else
        if targets.contains(sel.rename) then
          report.error(ImportedTwice(sel.rename), sel.renamed.orElse(sel.imported).srcPos)
        targets += sel.rename
      originals += sel.name

    if !ctx.compilationUnit.isJava then
      for sel <- selectors do
        if !sel.isWildcard then checkIdent(sel)
  end checkImportSelectors
}

trait ReChecking extends Checking {
  import tpd.*
  override def checkEnumParent(cls: Symbol, firstParent: Symbol)(using Context): Unit = ()
  override def checkEnum(cdef: untpd.TypeDef, cls: Symbol, firstParent: Symbol)(using Context): Unit = ()
  override def checkRefsLegal(tree: tpd.Tree, badOwner: Symbol, allowed: (Name, Symbol) => Boolean, where: String)(using Context): Unit = ()
  override def checkFullyAppliedType(tree: Tree)(using Context): Unit = ()
  override def checkEnumCaseRefsLegal(cdef: TypeDef, enumCtx: Context)(using Context): Unit = ()
  override def checkAnnotApplicable(annot: Tree, sym: Symbol)(using Context): Boolean = true
  override def checkMatchable(tp: Type, pos: SrcPos, pattern: Boolean)(using Context): Unit = ()
  override def checkNoModuleClash(sym: Symbol)(using Context) = ()
  override def checkCanThrow(tp: Type, span: Span)(using Context): Tree = EmptyTree
  override def checkCatch(pat: Tree, guard: Tree)(using Context): Unit = ()
  override def checkNoContextFunctionType(tree: Tree)(using Context): Unit = ()
  override def checkFeature(name: TermName, description: => String, featureUseSite: Symbol, pos: SrcPos)(using Context): Unit = ()
}

trait NoChecking extends ReChecking {
  import tpd.*
  override def checkNonCyclic(sym: Symbol, info: TypeBounds, reportErrors: Boolean)(using Context): Type = info
  override def checkNonCyclicInherited(joint: Type, parents: List[Type], decls: Scope, pos: SrcPos)(using Context): Unit = ()
  override def checkStable(tp: Type, pos: SrcPos, kind: String)(using Context): Unit = ()
  override def checkClassType(tp: Type, pos: SrcPos, traitReq: Boolean, stablePrefixReq: Boolean)(using Context): Type = tp
  override def checkImplicitConversionDefOK(sym: Symbol)(using Context): Unit = ()
  override def checkImplicitConversionUseOK(tree: Tree, expected: Type)(using Context): Unit = ()
  override def checkFeasibleParent(tp: Type, pos: SrcPos, where: => String = "")(using Context): Type = tp
  override def checkAnnotArgs(tree: Tree)(using Context): tree.type = tree
  override def checkNoTargetNameConflict(stats: List[Tree])(using Context): Unit = ()
  override def checkParentCall(call: Tree, caller: ClassSymbol)(using Context): Unit = ()
  override def checkSimpleKinded(tpt: Tree)(using Context): Tree = tpt
  override def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(using Context): Unit = ()
  override def checkCaseInheritance(parentSym: Symbol, caseCls: ClassSymbol, pos: SrcPos)(using Context): Unit = ()
  override def checkNoForwardDependencies(vparams: List[ValDef])(using Context): Unit = ()
  override def checkMembersOK(tp: Type, pos: SrcPos)(using Context): Type = tp
  override def checkInInlineContext(what: String, pos: SrcPos)(using Context): Unit = ()
  override def checkValidInfix(tree: untpd.InfixOp, meth: Symbol)(using Context): Unit = ()
  override def checkImportSelectors(qualType: Type, selectors: List[untpd.ImportSelector])(using Context): Unit = ()
}
