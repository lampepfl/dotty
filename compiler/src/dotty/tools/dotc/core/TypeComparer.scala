package dotty.tools
package dotc
package core

import Types.*, Contexts.*, Symbols.*, Flags.*, Names.*, NameOps.*, Denotations.*
import Decorators.*
import Phases.{gettersPhase, elimByNamePhase}
import StdNames.nme
import TypeOps.refineUsingParent
import collection.mutable
import util.{Stats, NoSourcePosition, EqHashMap}
import config.Config
import config.Feature.{migrateTo3, sourceVersion}
import config.Printers.{subtyping, gadts, matchTypes, noPrinter}
import config.SourceVersion
import TypeErasure.{erasedLub, erasedGlb}
import TypeApplications.*
import Variances.{Variance, variancesConform}
import Constants.Constant
import scala.util.control.NonFatal
import typer.ProtoTypes.constrained
import typer.Applications.productSelectorTypes
import reporting.trace
import annotation.constructorOnly
import cc.*
import NameKinds.WildcardParamName

/** Provides methods to compare types.
 */
class TypeComparer(@constructorOnly initctx: Context) extends ConstraintHandling, PatternTypeConstrainer {
  import TypeComparer.*
  Stats.record("TypeComparer")

  private var myContext: Context = initctx
  def comparerContext: Context = myContext

  protected given [DummySoItsADef]: Context = myContext

  protected var state: TyperState = compiletime.uninitialized
  def constraint: Constraint = state.constraint
  def constraint_=(c: Constraint): Unit = state.constraint = c

  def init(c: Context): Unit =
    myContext = c
    state = c.typerState
    monitored = false
    GADTused = false
    opaquesUsed = false
    recCount = 0
    needsGc = false
    if Config.checkTypeComparerReset then checkReset()

  private var pendingSubTypes: util.MutableSet[(Type, Type)] | Null = null
  private var recCount = 0
  private var monitored = false

  private var needsGc = false

  private var canCompareAtoms: Boolean = true // used for internal consistency checking

  /** Indicates whether the subtype check used GADT bounds */
  private var GADTused: Boolean = false

  /** Indicates whether the subtype check used opaque types */
  private var opaquesUsed: Boolean = false

  private var myInstance: TypeComparer = this
  def currentInstance: TypeComparer = myInstance

  /** Is a subtype check in progress? In that case we may not
   *  permanently instantiate type variables, because the corresponding
   *  constraint might still be retracted and the instantiation should
   *  then be reversed.
   */
  def subtypeCheckInProgress: Boolean = {
    val result = recCount > 0
    if (result) {
      constr.println("*** needsGC ***")
      needsGc = true
    }
    result
  }

  /** For statistics: count how many isSubTypes are part of successful comparisons */
  private var successCount = 0
  private var totalCount = 0

  protected val AnyClass     = defn.AnyClass
  protected val AnyKindClass = defn.AnyKindClass
  protected val NothingClass = defn.NothingClass
  protected val NullClass    = defn.NullClass
  protected val ObjectClass  = defn.ObjectClass
  protected val AnyType      = AnyClass.typeRef
  protected val AnyKindType  = AnyKindClass.typeRef
  protected val NothingType  = NothingClass.typeRef

  override def checkReset() =
    super.checkReset()
    assert(pendingSubTypes == null || pendingSubTypes.uncheckedNN.isEmpty)
    assert(canCompareAtoms == true)
    assert(successCount == 0)
    assert(totalCount == 0)
    assert(approx == ApproxState.Fresh)
    assert(leftRoot == null)
    assert(frozenGadt == false)

  /** Record that GADT bounds of `sym` were used in a subtype check.
   *  But exclude constructor type parameters, as these are aliased
   *  to the corresponding class parameters, which does not constitute
   *  a true usage of a GADT symbol.
   */
  private def GADTusage(sym: Symbol): true = recordGadtUsageIf(!sym.owner.isConstructor)

  private def recordGadtUsageIf(cond: Boolean): true = {
    if cond then
      GADTused = true
    true
  }

  private def isBottom(tp: Type) = tp.widen.isRef(NothingClass)

  protected def gadtBounds(sym: Symbol)(using Context) = ctx.gadt.bounds(sym)
  protected def gadtAddBound(sym: Symbol, b: Type, isUpper: Boolean): Boolean = ctx.gadtState.addBound(sym, b, isUpper)

  protected def typeVarInstance(tvar: TypeVar)(using Context): Type = tvar.underlying

  // Subtype testing `<:<`

  def topLevelSubType(tp1: Type, tp2: Type): Boolean = {
    if (tp2 eq NoType) return false
    if ((tp2 eq tp1) || (tp2 eq WildcardType)) return true
    try isSubType(tp1, tp2)
    finally {
      monitored = false
      if (Config.checkConstraintsSatisfiable)
        assert(isSatisfiable, constraint.show)
    }
  }

  def necessarySubType(tp1: Type, tp2: Type): Boolean =
    val saved = myNecessaryConstraintsOnly
    myNecessaryConstraintsOnly = true
    try topLevelSubType(tp1, tp2)
    finally myNecessaryConstraintsOnly = saved

  def testSubType(tp1: Type, tp2: Type): CompareResult =
    GADTused = false
    opaquesUsed = false
    if !topLevelSubType(tp1, tp2) then CompareResult.Fail
    else if GADTused then CompareResult.OKwithGADTUsed
    else if opaquesUsed then CompareResult.OKwithOpaquesUsed // we cast on GADTused, so handles if both are used
    else CompareResult.OK

  /** The current approximation state. See `ApproxState`. */
  private var approx: ApproxState = ApproxState.Fresh
  protected def approxState: ApproxState = approx

  /** The original left-hand type of the comparison. Gets reset
   *  every time we compare components of the previous pair of types.
   *  This type is used for capture conversion in `isSubArgs`.
   */
  private var leftRoot: Type | Null = null

  /** Are we forbidden from recording GADT constraints? */
  private var frozenGadt = false
  private inline def inFrozenGadt[T](inline op: T): T =
    inFrozenGadtIf(true)(op)

  /** A flag to prevent recursive joins when comparing AndTypes on the left */
  private var joined = false

  /** A variable to keep track of number of outstanding isSameType tests */
  private var sameLevel = 0

  /** A map that records successful isSameType comparisons.
   *  Used together with `sameLevel` to avoid exponential blowUp of isSameType
   *  comparisons for deeply nested invariant applied types.
   */
  private var sames: util.EqHashMap[Type, Type] | Null = null

  /** The `sameLevel` nesting depth from which on we want to keep track
   *  of isSameTypes suucesses using `sames`
   */
  val startSameTypeTrackingLevel = 3

  private inline def inFrozenGadtIf[T](cond: Boolean)(inline op: T): T = {
    val savedFrozenGadt = frozenGadt
    frozenGadt ||= cond
    try op finally frozenGadt = savedFrozenGadt
  }

  private inline def inFrozenGadtAndConstraint[T](inline op: T): T =
    inFrozenGadtIf(true)(inFrozenConstraint(op))

  extension (sym: Symbol)
    private inline def onGadtBounds(inline op: TypeBounds => Boolean): Boolean =
      val bounds = gadtBounds(sym)
      bounds != null && op(bounds)

  private inline def comparingTypeLambdas(tl1: TypeLambda, tl2: TypeLambda)(op: => Boolean): Boolean =
    val saved = comparedTypeLambdas
    comparedTypeLambdas += tl1
    comparedTypeLambdas += tl2
    try op finally comparedTypeLambdas = saved

  protected def isSubType(tp1: Type, tp2: Type, a: ApproxState): Boolean = {
    val savedApprox = approx
    val savedLeftRoot = leftRoot
    if (a == ApproxState.Fresh) {
      this.approx = ApproxState.None
      this.leftRoot = tp1
    }
    else this.approx = a
    try recur(tp1, tp2)
    catch {
      case ex: Throwable => handleRecursive("subtype", i"$tp1 <:< $tp2", ex, weight = 2)
    }
    finally {
      this.approx = savedApprox
      this.leftRoot = savedLeftRoot
    }
  }

  def isSubType(tp1: Type, tp2: Type): Boolean = isSubType(tp1, tp2, ApproxState.Fresh)

  override protected def isSub(tp1: Type, tp2: Type)(using Context): Boolean = isSubType(tp1, tp2)

  /** The inner loop of the isSubType comparison.
   *  Recursive calls from recur should go to recur directly if the two types
   *  compared in the callee are essentially the same as the types compared in the
   *  caller. "The same" means: represent essentially the same sets of values.
   * `recur` should not be used to compare components of types. In this case
   *  one should use `isSubType(_, _)`.
   *  `recur` should also not be used to compare approximated versions of the original
   *  types (as when we go from an abstract type to one of its bounds). In that case
   *  one should use `isSubType(_, _, a)` where `a` defines the kind of approximation.
   *
   *  Note: Logically, `recur` could be nested in `isSubType`, which would avoid
   *  the instance state consisting `approx` and `leftRoot`. But then the implemented
   *  code would have two extra parameters for each of the many calls that go from
   *  one sub-part of isSubType to another.
   */
  protected def recur(tp1: Type, tp2: Type): Boolean = trace(s"isSubType ${traceInfo(tp1, tp2)}${approx.show}", subtyping) {

    def monitoredIsSubType = {
      if (pendingSubTypes == null) {
        pendingSubTypes = util.HashSet[(Type, Type)]()
        report.log(s"!!! deep subtype recursion involving ${tp1.show} <:< ${tp2.show}, constraint = ${state.constraint.show}")
        report.log(s"!!! constraint = ${constraint.show}")
        //if (ctx.settings.YnoDeepSubtypes.value) {
        //  new Error("deep subtype").printStackTrace()
        //}
        assert(!ctx.settings.YnoDeepSubtypes.value)
        if (Config.traceDeepSubTypeRecursions && !this.isInstanceOf[ExplainingTypeComparer])
          report.log(explained(_.isSubType(tp1, tp2, approx), short = false))
      }
      // Eliminate LazyRefs before checking whether we have seen a type before
      val normalize = new TypeMap with CaptureSet.IdempotentCaptRefMap {
        val DerefLimit = 10
        var derefCount = 0
        def apply(t: Type) = t match {
          case t: LazyRef =>
            // Dereference a lazyref to detect underlying matching types, but
            // be careful not to get into an infinite recursion. If recursion count
            // exceeds `DerefLimit`, approximate with `t` instead.
            derefCount += 1
            if t.evaluating || derefCount >= DerefLimit then t
            else try mapOver(t.ref) finally derefCount -= 1
          case tp: TypeVar =>
            tp
          case _ =>
            mapOver(t)
        }
      }
      val p = (normalize(tp1), normalize(tp2))
      !pendingSubTypes.nn.contains(p) && {
        try {
          pendingSubTypes.nn += p
          firstTry
        }
        finally
          pendingSubTypes.nn -= p
      }
    }

    def firstTry: Boolean = tp2 match {
      case tp2: NamedType =>
        def compareNamed(tp1: Type, tp2: NamedType): Boolean =
          val ctx = comparerContext
          given Context = ctx // optimization for performance
          val info2 = tp2.info

          /** Does `tp2` have a stable prefix?
           *  If that's not the case, following an alias via asSeenFrom could be lossy
           *  so we should not conclude `false` if comparing aliases fails.
           *  See pos/i17064.scala for a test case
           */
          def hasStablePrefix(tp: NamedType) =
            tp.prefix.isStable

          info2 match
            case info2: TypeAlias =>
              if recur(tp1, info2.alias) then return true
              if tp2.asInstanceOf[TypeRef].canDropAlias && hasStablePrefix(tp2) then
                return false
            case _ =>
          tp1 match
            case tp1: NamedType =>
              tp1.info match {
                case info1: TypeAlias =>
                  if recur(info1.alias, tp2) then return true
                  if tp1.asInstanceOf[TypeRef].canDropAlias && hasStablePrefix(tp2) then
                    return false
                case _ =>
              }
              val sym2 = tp2.symbol
              var sym1 = tp1.symbol
              if (sym1.is(ModuleClass) && sym2.is(ModuleVal))
                // For convenience we want X$ <:< X.type
                // This is safe because X$ self-type is X.type
                sym1 = sym1.companionModule
              if (sym1 ne NoSymbol) && (sym1 eq sym2) then
                ctx.erasedTypes ||
                sym1.isStaticOwner ||
                isSubPrefix(tp1.prefix, tp2.prefix) ||
                thirdTryNamed(tp2)
              else
                (  (tp1.name eq tp2.name)
                && !sym1.is(Private)
                && tp2.isPrefixDependentMemberRef
                && isSubPrefix(tp1.prefix, tp2.prefix)
                && tp1.signature == tp2.signature
                && !(sym1.isClass && sym2.isClass)  // class types don't subtype each other
                ) ||
                thirdTryNamed(tp2)
            case _ =>
              secondTry
        end compareNamed
        // See the documentation of `FromJavaObjectSymbol`
        if !ctx.erasedTypes && tp2.isFromJavaObject then
          recur(tp1, defn.AnyType)
        else
          compareNamed(tp1, tp2)
      case tp2: ProtoType =>
        isMatchedByProto(tp2, tp1)
      case tp2: BoundType =>
        tp2 == tp1 || secondTry
      case tp2: TypeVar =>
        recur(tp1, typeVarInstance(tp2))
      case tp2: WildcardType =>
        def compareWild = tp2.optBounds match {
          case TypeBounds(_, hi) => recur(tp1, hi)
          case NoType => true
        }
        compareWild
      case tp2: LazyRef =>
        isBottom(tp1) || !tp2.evaluating && recur(tp1, tp2.ref)
      case CapturingType(_, _) =>
        secondTry
      case tp2: AnnotatedType if !tp2.isRefining =>
        recur(tp1, tp2.parent)
      case tp2: ThisType =>
        def compareThis = {
          val cls2 = tp2.cls
          tp1 match {
            case tp1: ThisType =>
              tp1.cls eq cls2
            case tp1: NamedType if cls2.is(Module) && cls2.eq(tp1.typeSymbol) =>
              cls2.isStaticOwner ||
              recur(tp1.prefix, cls2.owner.thisType) ||
              secondTry
            case _ =>
              secondTry
          }
        }
        compareThis
      case tp2: SuperType =>
        def compareSuper = tp1 match {
          case tp1: SuperType =>
            recur(tp1.thistpe, tp2.thistpe) &&
            isSameType(tp1.supertpe, tp2.supertpe)
          case _ =>
            secondTry
        }
        compareSuper
      case AndType(tp21, tp22) =>
        recur(tp1, tp21) && recur(tp1, tp22)
      case OrType(tp21, tp22) =>
        if (tp21.stripTypeVar eq tp22.stripTypeVar) recur(tp1, tp21)
        else secondTry
      case TypeErasure.ErasedValueType(tycon1, underlying2) =>
        def compareErasedValueType = tp1 match {
          case TypeErasure.ErasedValueType(tycon2, underlying1) =>
            (tycon1.symbol eq tycon2.symbol) && isSubType(underlying1, underlying2)
          case _ =>
            secondTry
        }
        compareErasedValueType
      case ConstantType(v2) =>
        tp1 match {
          case ConstantType(v1) => v1.value == v2.value && recur(v1.tpe, v2.tpe)
          case _ => secondTry
        }
      case tp2: AnyConstantType =>
        if (tp2.tpe.exists) recur(tp1, tp2.tpe)
        else tp1 match {
          case tp1: ConstantType =>
            tp2.tpe = tp1
            true
          case _ =>
            secondTry
        }
      case _: FlexType =>
        true
      case _ =>
        secondTry
    }

    def secondTry: Boolean = tp1 match {
      case tp1: NamedType =>
        tp1.info match {
          case info1: TypeAlias =>
            if (recur(info1.alias, tp2)) return true
            if (tp1.prefix.isStable) return tryLiftedToThis1
          case _ =>
            if (tp1 eq NothingType) || isBottom(tp1) then return true
        }
        thirdTry
      case tp1: TypeParamRef =>
        def flagNothingBound = {
          if (!frozenConstraint && isBottom(tp2) && state.isGlobalCommittable) {
            def msg = s"!!! instantiated to Nothing: $tp1, constraint = ${constraint.show}"
            if (Config.failOnInstantiationToNothing) assert(false, msg)
            else report.log(msg)
          }
          true
        }
        def compareTypeParamRef =
          assumedTrue(tp1)
          || tp2.dealias.match
              case tp2a: TypeParamRef => constraint.isLess(tp1, tp2a)
              case tp2a: AndType => recur(tp1, tp2a)
              case _ => false
          || isSubTypeWhenFrozen(bounds(tp1).hi.boxed, tp2)
          || (if canConstrain(tp1) && !approx.high then
                addConstraint(tp1, tp2, fromBelow = false) && flagNothingBound
              else thirdTry)

        compareTypeParamRef
      case tp1: ThisType =>
        val cls1 = tp1.cls
        tp2 match {
          case tp2: TermRef if cls1.is(Module) && cls1.eq(tp2.typeSymbol) =>
            cls1.isStaticOwner ||
            recur(cls1.owner.thisType, tp2.prefix) ||
            thirdTry
          case _ =>
            thirdTry
        }
      case tp1: SkolemType =>
        tp2 match {
          case tp2: SkolemType if !ctx.phase.isTyper && recur(tp1.info, tp2.info) => true
          case _ => thirdTry
        }
      case tp1: TypeVar =>
        recur(typeVarInstance(tp1), tp2)
      case tp1: WildcardType =>
        def compareWild = tp1.optBounds match {
          case bounds: TypeBounds => recur(bounds.lo, tp2)
          case _ => true
        }
        compareWild
      case tp1: LazyRef =>
        // If `tp1` is in train of being evaluated, don't force it
        // because that would cause an assertionError. Return false instead.
        // See i859.scala for an example where we hit this case.
        tp2.isRef(AnyClass, skipRefined = false)
        || !tp1.evaluating && recur(tp1.ref, tp2)
      case AndType(tp11, tp12) =>
        if tp11.stripTypeVar eq tp12.stripTypeVar then recur(tp11, tp2)
        else thirdTry
      case tp1 @ OrType(tp11, tp12) =>
        compareAtoms(tp1, tp2) match
          case Some(b) => return b
          case None =>

        def widenOK =
          (tp2.widenSingletons eq tp2)
          && (tp1.widenSingletons ne tp1)
          && inFrozenGadtAndConstraint(recur(tp1.widenSingletons(), tp2))

        def joinOK = tp2.dealiasKeepRefiningAnnots match {
          case tp2: AppliedType if !tp2.tycon.typeSymbol.isClass =>
            // If we apply the default algorithm for `A[X] | B[Y] <: C[Z]` where `C` is a
            // type parameter, we will instantiate `C` to `A` and then fail when comparing
            // with `B[Y]`. To do the right thing, we need to instantiate `C` to the
            // common superclass of `A` and `B`.
            inFrozenGadtAndConstraint(recur(tp1.join, tp2))
          case _ =>
            false
        }

        val res = widenOK || joinOK
          || recur(tp11, tp2) && recur(tp12, tp2)
          || containsAnd(tp1)
              && !joined
              && {
                joined = true
                try inFrozenGadt(recur(tp1.join, tp2))
                finally joined = false
              }
              // An & on the left side loses information. We compensate by also trying the join.
              // This is less ad-hoc than it looks since we produce joins in type inference,
              // and then need to check that they are indeed supertypes of the original types
              // under -Ycheck. Test case is i7965.scala.
              // On the other hand, we could get a combinatorial explosion by applying such joins
              // recursively, so we do it only once. See i14870.scala as a test case, which would
              // loop for a very long time without the recursion brake.

        if res && !tp1.isSoft && state.isCommittable then
          // We use a heuristic here where every toplevel type variable on the right hand side
          // is marked so that it converts all soft unions in its lower bound to hard unions
          // before it is instantiated. The reason is that the variable's instance type will
          // be a supertype of (decomposed and reconstituted) `tp1`.
          constraint = constraint.hardenTypeVars(tp2)

        res

      case tp1 @ CapturingType(parent1, refs1) =>
        def compareCapturing =
          if tp2.isAny then true
          else if subCaptures(refs1, tp2.captureSet, frozenConstraint).isOK && sameBoxed(tp1, tp2, refs1)
            || !ctx.mode.is(Mode.CheckBoundsOrSelfType) && tp1.isAlwaysPure
          then
            val tp2a =
              if tp1.isBoxedCapturing && !parent1.isBoxedCapturing
              then tp2.unboxed
              else tp2
            recur(parent1, tp2a)
          else thirdTry
        compareCapturing
      case tp1: AnnotatedType if !tp1.isRefining =>
        recur(tp1.parent, tp2)
      case tp1: MatchType =>
        val reduced = tp1.reduced
        if reduced.exists then
          recur(reduced, tp2) && recordGadtUsageIf { MatchType.thatReducesUsingGadt(tp1) }
        else thirdTry
      case _: FlexType =>
        true
      case _ =>
        thirdTry
    }

    def thirdTryNamed(tp2: NamedType): Boolean = tp2.info match {
      case info2: TypeBounds =>
        def compareGADT: Boolean =
          tp2.symbol.onGadtBounds(gbounds2 =>
            isSubTypeWhenFrozen(tp1, gbounds2.lo)
            || tp1.match
                case tp1: NamedType if ctx.gadt.contains(tp1.symbol) =>
                  // Note: since we approximate constrained types only with their non-param bounds,
                  // we need to manually handle the case when we're comparing two constrained types,
                  // one of which is constrained to be a subtype of another.
                  // We do not need similar code in fourthTry, since we only need to care about
                  // comparing two constrained types, and that case will be handled here first.
                  ctx.gadt.isLess(tp1.symbol, tp2.symbol) && GADTusage(tp1.symbol) && GADTusage(tp2.symbol)
                case _ => false
            || narrowGADTBounds(tp2, tp1, approx, isUpper = false))
          && (isBottom(tp1) || GADTusage(tp2.symbol))

        isSubApproxHi(tp1, info2.lo) && (trustBounds || isSubApproxHi(tp1, info2.hi))
        || compareGADT
        || tryLiftedToThis2
        || fourthTry

      case _ =>
        val cls2 = tp2.symbol
        if (cls2.isClass)
          if (cls2.typeParams.isEmpty) {
            if (cls2 eq AnyKindClass) return true
            if (isBottom(tp1)) return true
            if (tp1.isLambdaSub) return false
              // Note: We would like to replace this by `if (tp1.hasHigherKind)`
              // but right now we cannot since some parts of the standard library rely on the
              // idiom that e.g. `List <: Any`. We have to bootstrap without scalac first.
            if cls2 eq AnyClass then return true
            if cls2 == defn.SingletonClass && tp1.isStable then return true
            return tryBaseType(cls2)
          }
          else if (cls2.is(JavaDefined)) {
            // If `cls2` is parameterized, we are seeing a raw type, so we need to compare only the symbol
            val base = nonExprBaseType(tp1, cls2)
            if (base.typeSymbol == cls2) return true
          }
          else if tp1.typeParams.nonEmpty && !tp1.isAnyKind then
            return recur(tp1, tp2.etaExpand)
        fourthTry
    }

    def compareTypeParamRef(tp2: TypeParamRef): Boolean =
      assumedTrue(tp2)
      || {
        val alwaysTrue =
          // The following condition is carefully formulated to catch all cases
          // where the subtype relation is true without needing to add a constraint
          // It's tricky because we might need to either approximate tp2 by its
          // lower bound or else widen tp1 and check that the result is a subtype of tp2.
          // So if the constraint is not yet frozen, we do the same comparison again
          // with a frozen constraint, which means that we get a chance to do the
          // widening in `fourthTry` before adding to the constraint.
          if (frozenConstraint) recur(tp1, bounds(tp2).lo.boxed)
          else isSubTypeWhenFrozen(tp1, tp2)
        alwaysTrue
        || tp1.dealias.match
            case tp1a: OrType => recur(tp1a, tp2)
            case _ => false
        || (if canConstrain(tp2) && !approx.low then
              addConstraint(tp2, tp1.widenExpr, fromBelow = true)
            else fourthTry)
      }

    def thirdTry: Boolean = tp2 match {
      case tp2 @ AppliedType(tycon2, args2) =>
        compareAppliedType2(tp2, tycon2, args2)
      case tp2: NamedType =>
        thirdTryNamed(tp2)
      case tp2: TypeParamRef =>
        compareTypeParamRef(tp2)
      case tp2: RefinedType =>
        def compareRefinedSlow: Boolean =
          val name2 = tp2.refinedName
          recur(tp1, tp2.parent)
          && (name2 == nme.WILDCARD || hasMatchingMember(name2, tp1, tp2))

        def compareRefined: Boolean =
          val tp1w = tp1.widen

          if isCaptureCheckingOrSetup then

            // A relaxed version of subtyping for dependent functions where method types
            // are treated as contravariant.
            // TODO: Merge with isSubInfo in hasMatchingMember. Currently, we can't since
            // the isSubinfo of hasMatchingMember has problems dealing with PolyTypes
            // (---> orphan params during pickling)
            def isSubInfo(info1: Type, info2: Type): Boolean = (info1, info2) match
              case (info1: PolyType, info2: PolyType) =>
                info1.paramNames.hasSameLengthAs(info2.paramNames)
                && isSubInfo(info1.resultType, info2.resultType.subst(info2, info1))
              case (info1: MethodType, info2: MethodType) =>
                matchingMethodParams(info1, info2, precise = false)
                && isSubInfo(info1.resultType, info2.resultType.subst(info2, info1))
              case (info1 @ CapturingType(parent1, refs1), info2: Type)
              if info2.stripCapturing.isInstanceOf[MethodOrPoly] =>
                subCaptures(refs1, info2.captureSet, frozenConstraint).isOK && sameBoxed(info1, info2, refs1)
                  && isSubInfo(parent1, info2)
              case (info1: Type, CapturingType(parent2, refs2))
              if info1.stripCapturing.isInstanceOf[MethodOrPoly] =>
                val refs1 = info1.captureSet
                (refs1.isAlwaysEmpty || subCaptures(refs1, refs2, frozenConstraint).isOK) && sameBoxed(info1, info2, refs1)
                  && isSubInfo(info1, parent2)
              case _ =>
                isSubType(info1, info2)

            if defn.isFunctionType(tp2) then
              if tp2.derivesFrom(defn.PolyFunctionClass) then
                return isSubInfo(tp1.member(nme.apply).info, tp2.refinedInfo)
              else
                tp1w.widenDealias match
                  case tp1: RefinedType =>
                    return isSubInfo(tp1.refinedInfo, tp2.refinedInfo)
                  case _ =>
          end if

          val skipped2 = skipMatching(tp1w, tp2)
          if (skipped2 eq tp2) || !Config.fastPathForRefinedSubtype then
            if containsAnd(tp1) then
              tp2.parent match
                case _: RefinedType | _: AndType =>
                  // maximally decompose RHS to limit the bad effects of the `either` that is necessary
                  // since LHS contains an AndType
                  recur(tp1, decomposeRefinements(tp2, Nil))
                case _ =>
                  // Delay calling `compareRefinedSlow` because looking up a member
                  // of an `AndType` can lead to a cascade of subtyping checks
                  // This twist is needed to make collection/generic/ParFactory.scala compile
                  fourthTry || compareRefinedSlow
            else if tp1.isInstanceOf[HKTypeLambda] then
              // HKTypeLambdas do not have members.
              fourthTry
            else
              compareRefinedSlow || fourthTry
          else // fast path, in particular for refinements resulting from parameterization.
            isSubRefinements(tp1w.asInstanceOf[RefinedType], tp2, skipped2) &&
            recur(tp1, skipped2)
        end compareRefined

        compareRefined
      case tp2: RecType =>
        def compareRec = tp1.safeDealias match {
          case tp1: RecType =>
            val rthis1 = tp1.recThis
            recur(tp1.parent, tp2.parent.substRecThis(tp2, rthis1))
          case NoType => false
          case _ =>
            val tp1stable = ensureStableSingleton(tp1)
            recur(fixRecs(tp1stable, tp1stable.widenExpr), tp2.parent.substRecThis(tp2, tp1stable))
        }
        compareRec
      case tp2: HKTypeLambda =>
        def compareTypeLambda: Boolean = tp1.stripTypeVar match {
          case tp1: HKTypeLambda =>
           /* Don't compare bounds of lambdas under language:Scala2, or t2994 will fail.
            * The issue is that, logically, bounds should compare contravariantly,
            * but that would invalidate a pattern exploited in t2994:
            *
            *    [X0 <: Number] -> Number   <:<    [X0] -> Any
            *
            * Under the new scheme, `[X0] -> Any` is NOT a kind that subsumes
            * all other bounds. You'd have to write `[X0 >: Any <: Nothing] -> Any` instead.
            * This might look weird, but is the only logically correct way to do it.
            *
            * Note: it would be nice if this could trigger a migration warning, but I
            * am not sure how, since the code is buried so deep in subtyping logic.
            */
            def boundsOK =
              migrateTo3 ||
              tp1.typeParams.corresponds(tp2.typeParams)((tparam1, tparam2) =>
                isSubType(tparam2.paramInfo.subst(tp2, tp1), tparam1.paramInfo))
            comparingTypeLambdas(tp1, tp2) {
              val variancesOK = variancesConform(tp1.typeParams, tp2.typeParams)
              variancesOK && boundsOK && isSubType(tp1.resType, tp2.resType.subst(tp2, tp1))
            }
          case _ =>
            val tparams1 = tp1.typeParams
            if (tparams1.nonEmpty)
              return recur(tp1.etaExpand, tp2) || fourthTry
            tp2 match {
              case EtaExpansion(tycon2: TypeRef) if tycon2.symbol.isClass && tycon2.symbol.is(JavaDefined) =>
                recur(tp1, tycon2) || fourthTry
              case _ =>
                fourthTry
            }
        }
        compareTypeLambda
      case tp2 @ OrType(tp21, tp22) =>
        compareAtoms(tp1, tp2) match
          case Some(b) => return b
          case _ =>

        // The next clause handles a situation like the one encountered in i2745.scala.
        // We have:
        //
        //   x: A | B, x.type <:< A | X   where X is a type variable
        //
        // We should instantiate X to B instead of x.type or A | B. To do this, we widen
        // the LHS to A | B and recur *without indicating that this is a lowApprox*. The
        // latter point is important since otherwise we would not get to instantiate X.
        // If that succeeds, fine. If not we continue and hit the `either` below.
        // That second path is important to handle comparisons with unions of singletons,
        // as in `1 <:< 1 | 2`.
        val tp1w = tp1.widen
        if ((tp1w ne tp1) && recur(tp1w, tp2))
          return true

        val tp1a = tp1.dealiasKeepRefiningAnnots
        if (tp1a ne tp1)
          // Follow the alias; this might lead to an OrType on the left which needs to be split
          return recur(tp1a, tp2)

        // Rewrite T1 <: (T211 & T212) | T22 to T1 <: (T211 | T22) and T1 <: (T212 | T22)
        // and analogously for T1 <: T21 | (T221 & T222)
        // `|' types to the right of <: are problematic, because
        // we have to choose one constraint set or another, which might cut off
        // solutions. The rewriting delays the point where we have to choose.
        tp21 match {
          case AndType(tp211, tp212) =>
            return recur(tp1, OrType(tp211, tp22, tp2.isSoft)) && recur(tp1, OrType(tp212, tp22, tp2.isSoft))
          case _ =>
        }
        tp22 match {
          case AndType(tp221, tp222) =>
            return recur(tp1, OrType(tp21, tp221, tp2.isSoft)) && recur(tp1, OrType(tp21, tp222, tp2.isSoft))
          case _ =>
        }
        either(recur(tp1, tp21), recur(tp1, tp22)) || fourthTry
      case tp2: MatchType =>
        val reduced = tp2.reduced
        if reduced.exists then
          recur(tp1, reduced) && recordGadtUsageIf { MatchType.thatReducesUsingGadt(tp2) }
        else
          fourthTry
      case tp2: MethodType =>
        def compareMethod = tp1 match {
          case tp1: MethodType =>
            (tp1.signature consistentParams tp2.signature) &&
            matchingMethodParams(tp1, tp2) &&
            (!tp2.isImplicitMethod || tp1.isImplicitMethod) &&
            isSubType(tp1.resultType, tp2.resultType.subst(tp2, tp1))
          case _ => false
        }
        compareMethod
      case tp2: PolyType =>
        def comparePoly = tp1 match {
          case tp1: PolyType =>
            comparingTypeLambdas(tp1, tp2) {
              (tp1.signature consistentParams tp2.signature)
              && matchingPolyParams(tp1, tp2)
              && isSubType(tp1.resultType, tp2.resultType.subst(tp2, tp1))
            }
          case _ => false
        }
        comparePoly
      case tp2 @ ExprType(restpe2) =>
        def compareExpr = tp1 match {
          // We allow ()T to be a subtype of => T.
          // We need some subtype relationship between them so that e.g.
          // def toString   and   def toString()   don't clash when seen
          // as members of the same type. And it seems most logical to take
          // ()T <:< => T, since everything one can do with a => T one can
          // also do with a ()T by automatic () insertion.
          case tp1 @ MethodType(Nil) => isSubType(tp1.resultType, restpe2)
          case tp1 @ ExprType(restpe1) => isSubType(restpe1, restpe2)
          case _ => fourthTry
        }
        compareExpr
      case tp2 @ TypeBounds(lo2, hi2) =>
        def compareTypeBounds = tp1 match {
          case tp1 @ TypeBounds(lo1, hi1) =>
            ((lo2 eq NothingType) || isSubType(lo2, lo1)) &&
            ((hi2 eq AnyType) && !hi1.isLambdaSub
             || (hi2 eq AnyKindType)
             || isSubType(hi1, hi2))
          case tp1: ClassInfo =>
            tp2 contains tp1
          case _ =>
            false
        }
        compareTypeBounds
      case CapturingType(parent2, refs2) =>
        def compareCapturing =
          val refs1 = tp1.captureSet
          try
            if refs1.isAlwaysEmpty then recur(tp1, parent2)
            else subCaptures(refs1, refs2, frozenConstraint).isOK
              && sameBoxed(tp1, tp2, refs1)
              && (recur(tp1.widen.stripCapturing, parent2)
                 || tp1.isInstanceOf[SingletonType] && recur(tp1, parent2)
                    // this alternative is needed in case the right hand side is a
                    // capturing type that contains the lhs as an alternative of a union type.
                 )
          catch case ex: AssertionError =>
            println(i"assertion failed while compare captured $tp1 <:< $tp2")
            throw ex
        compareCapturing || fourthTry
      case tp2: AnnotatedType if tp2.isRefining =>
        (tp1.derivesAnnotWith(tp2.annot.sameAnnotation) || tp1.isBottomType) &&
        recur(tp1, tp2.parent)
      case ClassInfo(pre2, cls2, _, _, _) =>
        def compareClassInfo = tp1 match {
          case ClassInfo(pre1, cls1, _, _, _) =>
            (cls1 eq cls2) && isSubType(pre1, pre2)
          case _ =>
            false
        }
        compareClassInfo
      case _ =>
        fourthTry
    }

    /** Can we widen an abstract type when comparing with `tp`?
    *   This is the case with the following cases:
    *    - if `canWidenAbstract` is true.
     *
     *  Secondly, if `tp` is a type parameter, we can widen if:
     *    - if `tp` is not a type parameter of the matched-against case lambda
     *    - if `tp` is an invariant or wildcard type parameter
     *    - finally, allow widening, but record the type parameter in `poisoned`,
     *      so that can be accounted for during the reduction step
     */
    def widenAbstractOKFor(tp: Type): Boolean =
      val acc = new TypeAccumulator[Boolean]:
        override def apply(x: Boolean, t: Type) =
          x && t.match
            case t: TypeParamRef =>
              variance == 0
              || (t.binder ne caseLambda)
              || t.paramName.is(WildcardParamName)
              || { poisoned += t; true }
            case _ =>
              foldOver(x, t)

      canWidenAbstract && acc(true, tp)

    def tryBaseType(cls2: Symbol) =
      val base = nonExprBaseType(tp1, cls2)
      if base.exists && (base ne tp1)
          && (!caseLambda.exists
              || widenAbstractOKFor(tp2)
              || tp1.widen.underlyingClassRef(refinementOK = true).exists)
      then
        def checkBase =
          isSubType(base, tp2, if tp1.isRef(cls2) then approx else approx.addLow)
          && recordGadtUsageIf { MatchType.thatReducesUsingGadt(tp1) }
        if tp1.widenDealias.isInstanceOf[AndType] || base.isInstanceOf[OrType] then
          // If tp1 is a intersection, it could be that one of the original
          // branches of the AndType tp1 conforms to tp2, but its base type does
          // not, or else that its base type for cls2 does not exist, in which case
          // it would not show up in `base`. In either case, we need to also fall back
          // to fourthTry. Test cases are i18266.scala and i18226a.scala.
          // If base is a disjunction, this might have come from a tp1 type that
          // expands to a match type. In this case, we should try to reduce the type
          // and compare the redux. This is done in fourthTry
          either(checkBase, fourthTry)
        else
          checkBase
      else fourthTry

    def fourthTry: Boolean = tp1 match {
      case tp1: TypeRef =>
        tp1.info match {
          case info1 @ TypeBounds(lo1, hi1) =>
            def compareGADT =
              tp1.symbol.onGadtBounds(gbounds1 =>
                isSubTypeWhenFrozen(gbounds1.hi, tp2)
                || narrowGADTBounds(tp1, tp2, approx, isUpper = true))
              && (tp2.isAny || GADTusage(tp1.symbol))

            (!caseLambda.exists || widenAbstractOKFor(tp2))
              && isSubType(hi1, tp2, approx.addLow) && (trustBounds || isSubType(lo1, tp2, approx.addLow))
            || compareGADT
            || tryLiftedToThis1
          case _ =>
            // `Mode.RelaxedOverriding` is only enabled when checking Java overriding
            // in explicit nulls, and `Null` becomes a bottom type, which allows
            // `T | Null` being a subtype of `T`.
            // A type variable `T` from Java is translated to `T >: Nothing <: Any`.
            // However, `null` can always be a value of `T` for Java side.
            // So the best solution here is to let `Null` be a subtype of non-primitive
            // value types temporarily.
            def isNullable(tp: Type): Boolean = tp.dealias match
              case tp: TypeRef =>
                val tpSym = tp.symbol
                ctx.mode.is(Mode.RelaxedOverriding) && !tpSym.isPrimitiveValueClass ||
                tpSym.isNullableClass
              case tp: TermRef =>
                // https://scala-lang.org/files/archive/spec/2.13/03-types.html#singleton-types
                // A singleton type is of the form p.type. Where p is a path pointing to a value which conforms to
                // scala.AnyRef [Scala 3: which scala.Null conforms to], the type denotes the set of values consisting
                // of null and the value denoted by p (i.e., the value v for which v eq p). [Otherwise,] the type
                // denotes the set consisting of only the value denoted by p.
                !ctx.explicitNulls && isNullable(tp.underlying) && tp.isStable
              case tp: ThisType =>
                // Same as above; this.type is also a singleton type in spec language
                !ctx.explicitNulls && isNullable(tp.underlying)
              case tp: RefinedOrRecType => isNullable(tp.parent)
              case tp: AppliedType => isNullable(tp.tycon)
              case AndType(tp1, tp2) => isNullable(tp1) && isNullable(tp2)
              case OrType(tp1, tp2) => isNullable(tp1) || isNullable(tp2)
              case AnnotatedType(tp1, _) => isNullable(tp1)
              case ConstantType(c) => c.tag == Constants.NullTag
              case _ => false
            val sym1 = tp1.symbol
            (sym1 eq NothingClass) && tp2.isValueTypeOrLambda ||
            (sym1 eq NullClass) && isNullable(tp2)
        }
      case tp1 @ AppliedType(tycon1, args1) =>
        compareAppliedType1(tp1, tycon1, args1)
      case tp1: SingletonType =>
        def comparePaths = tp2 match
          case tp2: TermRef =>
            compareAtoms(tp1, tp2, knownSingletons = true).getOrElse(false)
            || { // needed to make from-tasty work. test cases: pos/i1753.scala, pos/t839.scala
              tp2.info.widenExpr.dealias match
                case tp2i: SingletonType => recur(tp1, tp2i)
                case _ => false
            }
          case _ => false

        def tp1widened =
          val tp1w = tp1.underlying.widenExpr
          if isCaptureCheckingOrSetup then
            tp1
              .match
                case tp1: CaptureRef if tp1.isTracked =>
                  CapturingType(tp1w.stripCapturing, tp1.singletonCaptureSet)
                case _ =>
                  tp1w
              .withReachCaptures(tp1)
          else tp1w

        comparePaths || isSubType(tp1widened, tp2, approx.addLow)
      case tp1: RefinedType =>
        isNewSubType(tp1.parent)
      case tp1: RecType =>
        isNewSubType(tp1.parent)
      case tp1: HKTypeLambda =>
        def compareHKLambda = tp1 match {
          case EtaExpansion(tycon1: TypeRef) if tycon1.symbol.isClass && tycon1.symbol.is(JavaDefined) =>
            // It's a raw type that was mistakenly eta-expanded to a hk-type.
            // This can happen because we do not cook types coming from Java sources
            recur(tycon1, tp2)
          case _ => tp2 match {
            case tp2: HKTypeLambda => false // this case was covered in thirdTry
            case _ => tp2.typeParams.hasSameLengthAs(tp1.paramRefs) && isSubType(tp1.resultType, tp2.appliedTo(tp1.paramRefs))
          }
        }
        compareHKLambda
      case tp1 @ AndType(tp11, tp12) =>
        val tp2a = tp2.dealiasKeepRefiningAnnots
        if (tp2a ne tp2) // Follow the alias; this might avoid truncating the search space in the either below
          return recur(tp1, tp2a)

        // Rewrite (T111 | T112) & T12 <: T2 to (T111 & T12) <: T2 and (T112 & T12) <: T2
        // and analogously for T11 & (T121 | T122) & T12 <: T2
        // `&' types to the left of <: are problematic, because
        // we have to choose one constraint set or another, which might cut off
        // solutions. The rewriting delays the point where we have to choose.
        tp11 match {
          case OrType(tp111, tp112) =>
            return recur(AndType(tp111, tp12), tp2) && recur(AndType(tp112, tp12), tp2)
          case _ =>
        }
        tp12 match {
          case OrType(tp121, tp122) =>
            return recur(AndType(tp11, tp121), tp2) && recur(AndType(tp11, tp122), tp2)
          case _ =>
        }
        val tp1norm = trySimplify(tp1)
        if tp1 ne tp1norm then recur(tp1norm, tp2)
        else either(recur(tp11, tp2), recur(tp12, tp2))
      case tp1: MatchType =>
        def compareMatch = tp2 match {
          case tp2: MatchType =>
            // we allow a small number of scrutinee types to be widened:
            // * skolems, which may appear from type avoidance, but are widened in the inferred result type
            // * inline proxies, which is inlining's solution to the same problem
            def widenScrutinee(scrutinee1: Type) = scrutinee1 match
                case tp: TermRef if tp.symbol.is(InlineProxy) => tp.info
                case tp                                       => tp.widenSkolem
            def checkScrutinee(scrutinee1: Type): Boolean =
              isSameType(scrutinee1, tp2.scrutinee) || {
                val widenScrutinee1 = widenScrutinee(scrutinee1)
                (widenScrutinee1 ne scrutinee1) && checkScrutinee(widenScrutinee1)
              }
            checkScrutinee(tp1.scrutinee) &&
            tp1.cases.corresponds(tp2.cases)(isSubType)
          case _ => false
        }
        (!caseLambda.exists || canWidenAbstract) && recur(tp1.underlying, tp2) || compareMatch
      case tp1: AnnotatedType if tp1.isRefining =>
        isNewSubType(tp1.parent)
      case JavaArrayType(elem1) =>
        def compareJavaArray = tp2 match {
          case JavaArrayType(elem2) => isSubType(elem1, elem2)
          case _ => tp2.isAnyRef
        }
        compareJavaArray
      case tp1: ExprType if ctx.phaseId > gettersPhase.id =>
        // getters might have converted T to => T, need to compensate.
        recur(tp1.widenExpr, tp2)
      case _ =>
        false
    }

    /** When called from `pre1.A <:< pre2.A` does `pre1` relate to `pre2` so that
     *  the subtype test is true? This is the case if
     *
     *    1. `pre1 <:< pre2`, or
     *    2. One of `pre1` and `pre2` refers to a package and the other to a
     *       package object in that package, or
     *    3. `pre1` and `pre2` are both this-types of related classes.
     *
     *  Here, two classes are related if each of them has a self type that derives from the other.
     *  The third criterion is a bit dubious. I.e. in the test
     *
     *      A.this.T <:< B.this.T
     *
     *  where `T` is the same type, what relationship must exist between A and B
     *  for the test to be guaranteed true? The problem is we can't tell without additional
     *  info. One could be an outer this at the point where we do the test, but that
     *  location is unknown to us.
     *
     *  The conservative choice would be to require A == B, but then some tests involving
     *  self types fail. Specifically, t360, t361 and pat_iuli fail the pickling test, and
     *  Namer fails to compile. At line 203, we get
     *
     *    val Deriver         : Property.Key[typer.Deriver]       = new Property.Key
     *        ^
     *  value Deriver in class Namer is not a legal implementation of `Deriver` in class Namer.
     *  its type             dotty.tools.dotc.util.Property.Key[Namer.this.Deriver]
    |*  does not conform to  dotty.tools.dotc.util.Property.Key[Typer.this.Deriver & Namer.this.Deriver]
     */
    def isSubPrefix(pre1: Type, pre2: Type): Boolean =
      def samePkg(sym1: Symbol, sym2: Symbol) =
           sym2.is(Package) && sym1.isPackageObject && sym1.owner == sym2.moduleClass
        || sym1.is(Package) && sym2.isPackageObject && sym2.owner == sym1.moduleClass
      pre1 match
        case pre1: ThisType =>
          pre2 match
            case pre2: ThisType =>
              if samePkg(pre1.cls, pre2.cls) then return true
              if pre1.cls.classInfo.selfType.derivesFrom(pre2.cls)
                 && pre2.cls.classInfo.selfType.derivesFrom(pre1.cls)
              then
                subtyping.println(i"assume equal prefixes $pre1 $pre2")
                return true
            case pre2: TermRef =>
              if samePkg(pre1.cls, pre2.symbol) then return true
            case _ =>
        case pre1: TermRef =>
          pre2 match
            case pre2: TermRef =>
              if samePkg(pre1.symbol, pre2.symbol) then return true
            case pre2: ThisType =>
              if samePkg(pre1.symbol, pre2.cls) then return true
            case _ =>
        case _ =>
      isSubType(pre1, pre2)
    end isSubPrefix

    /** Compare `tycon[args]` with `other := otherTycon[otherArgs]`, via `>:>` if fromBelow is true, `<:<` otherwise
     *  (we call this relationship `~:~` in the rest of this comment).
     *
     *  This method works by:
     *
     *  1. Choosing an appropriate type constructor `adaptedTycon`
     *  2. Constraining `tycon` such that `tycon ~:~ adaptedTycon`
     *  3. Recursing on `adaptedTycon[args] ~:~ other`
     *
     *  So, how do we pick `adaptedTycon`? When `args` and `otherArgs` have the
     *  same length the answer is simply:
     *
     *    adaptedTycon := otherTycon
     *
     *  But we also handle having `args.length < otherArgs.length`, in which
     *  case we need to make up a type constructor of the right kind. For
     *  example, if `fromBelow = false` and we're comparing:
     *
     *    ?F[A] <:< Either[String, B] where `?F <: [X] =>> Any`
     *
     *  we will choose:
     *
     *    adaptedTycon := [X] =>> Either[String, X]
     *
     *  this allows us to constrain:
     *
     *    ?F <: adaptedTycon
     *
     *  and then recurse on:
     *
     *    adaptedTycon[A] <:< Either[String, B]
     *
     *  In general, given:
     *
     *  - k := args.length
     *  - d := otherArgs.length - k
     *  - T_0, ..., T_k-1 fresh type parameters
     *  - bodyArgs := otherArgs.take(d), T_0, ..., T_k-1
     *
     *  Then,
     *
     *    adaptedTycon := [T_0, ..., T_k-1] =>> otherTycon[bodyArgs]
     *
     *  where the bounds of `T_i` are set based on the bounds of `otherTycon.typeParams(d+i)`
     *  after substituting type parameter references by the corresponding argument
     *  in `bodyArgs` (see `adaptedBounds` in the implementation).
     *
     *  Historical note: this strategy is known in Scala as "partial unification"
     *  (even though the type constructor variable isn't actually unified but only
     *  has one of its bounds constrained), for background see:
     *  - The infamous SI-2712: https://github.com/scala/bug/issues/2712
     *  - The PR against Scala 2.12 implementing -Ypartial-unification: https://github.com/scala/scala/pull/5102
     *  - Some explanations on how this impacts API design: https://gist.github.com/djspiewak/7a81a395c461fd3a09a6941d4cd040f2
     */
    def compareAppliedTypeParamRef(tycon: TypeParamRef, args: List[Type], other: AppliedType, fromBelow: Boolean): Boolean =
      def directionalIsSubType(tp1: Type, tp2: Type): Boolean =
        if fromBelow then isSubType(tp2, tp1) else isSubType(tp1, tp2)
      def directionalRecur(tp1: Type, tp2: Type): Boolean =
        if fromBelow then recur(tp2, tp1) else recur(tp1, tp2)

      val otherTycon = other.tycon
      val otherArgs = other.args

      val d = otherArgs.length - args.length
      d >= 0 && {
        val tparams = tycon.typeParams
        val remainingTparams = otherTycon.typeParams.drop(d)
        variancesConform(remainingTparams, tparams) && {
          val adaptedTycon =
            if d > 0 then
              val initialArgs = otherArgs.take(d)
              /** The arguments passed to `otherTycon` in the body of `tl` */
              def bodyArgs(tl: HKTypeLambda) = initialArgs ++ tl.paramRefs
              /** The bounds of the type parameters of `tl` */
              def adaptedBounds(tl: HKTypeLambda) =
                val bodyArgsComputed = bodyArgs(tl)
                remainingTparams.map(_.paramInfo)
                  .mapconserve(_.substTypeParams(otherTycon, bodyArgsComputed).bounds)

              HKTypeLambda(remainingTparams.map(_.paramName))(
                adaptedBounds,
                tl => otherTycon.appliedTo(bodyArgs(tl)))
            else
              otherTycon
          (assumedTrue(tycon) || directionalIsSubType(tycon, adaptedTycon)) &&
          directionalRecur(adaptedTycon.appliedTo(args), other)
        }
      }
    end compareAppliedTypeParamRef

    /** Subtype test for the hk application `tp2 = tycon2[args2]`.
     */
    def compareAppliedType2(tp2: AppliedType, tycon2: Type, args2: List[Type]): Boolean = {
      val tparams = tycon2.typeParams
      if (tparams.isEmpty) return false // can happen for ill-typed programs, e.g. neg/tcpoly_overloaded.scala

      /** True if `tp1` and `tp2` have compatible type constructors and their
       *  corresponding arguments are subtypes relative to their variance (see `isSubArgs`).
       */
      def isMatchingApply(tp1: Type): Boolean = tp1.widen match {
        case tp1 @ AppliedType(tycon1, args1) =>
          // We intentionally do not automatically dealias `tycon1` or `tycon2` here.
          // `TypeApplications#appliedTo` already takes care of dealiasing type
          // constructors when this can be done without affecting type
          // inference, doing it here would not only prevent code from compiling
          // but could also result in the wrong thing being inferred later, for example
          // in `tests/run/hk-alias-unification.scala` we end up checking:
          //
          //   Foo[?F, ?T] <:< Foo[[X] =>> (X, String), Int]
          //
          // where
          //
          //   type Foo[F[_], T] = ErasedFoo[F[T]]
          //
          // Naturally, we'd like to infer:
          //
          //   ?F := [X] => (X, String)
          //
          // but if we dealias `Foo` then we'll end up trying to check:
          //
          //   ErasedFoo[?F[?T]] <:< ErasedFoo[(Int, String)]
          //
          // Because of partial unification, this will succeed, but will produce the constraint:
          //
          //   ?F := [X] =>> (Int, X)
          //
          // Which is not what we wanted!
          // On the other hand, we are not allowed to always stop at the present arguments either.
          // An example is i10129.scala. Here, we have the following situation:
          //
          //    type Lifted[A] = Err | A
          //    def point[O](o: O): Lifted[O] = o
          //    extension [O, U](o: Lifted[O]) def map(f: O => U): Lifted[U] = ???
          //    point("a").map(_ => if true then 1 else error)
          //
          // This leads to the constraint Lifted[U] <: Lifted[Int]. If we just
          // check the arguments this gives `U <: Int`. But this is wrong. Dealiasing
          // `Lifted` gives `Err | U <: Err | Int`, hence it should be `U <: Err | Int`.
          //
          // So it's a conundrum. We need to check the immediate arguments for hk type inference,
          // but this could narrow the constraint too much. The solution is to also
          // check the constraint arising from the dealiased subtype test
          // in the case where isSubArgs adds a constraint. If that second constraint
          // is weaker than the first, we keep it in place of the first.
          // Note that if the isSubArgs test fails, we will proceed anyway by
          // dealising by doing a compareLower.
          def loop(tycon1: Type, args1: List[Type]): Boolean = tycon1 match {
            case tycon1: TypeParamRef =>
              (tycon1 == tycon2 ||
               canConstrain(tycon1) && isSubType(tycon1, tycon2)) &&
              isSubArgs(args1, args2, tp1, tparams)
            case tycon1: TypeRef =>
              tycon2 match {
                case tycon2: TypeRef =>
                  val tycon1sym = tycon1.symbol
                  val tycon2sym = tycon2.symbol

                  var touchedGADTs = false
                  var gadtIsInstantiated = false

                  extension (sym: Symbol)
                    inline def byGadtBounds(inline op: TypeBounds => Boolean): Boolean =
                      touchedGADTs = true
                      sym.onGadtBounds(
                        b => op(b) && { gadtIsInstantiated = b.isInstanceOf[TypeAlias]; true })

                  def byGadtOrdering: Boolean =
                    ctx.gadt.contains(tycon1sym)
                    && ctx.gadt.contains(tycon2sym)
                    && ctx.gadt.isLess(tycon1sym, tycon2sym)

                  val res = (
                    tycon1sym == tycon2sym && isSubPrefix(tycon1.prefix, tycon2.prefix)
                    || tycon1sym.byGadtBounds(b => isSubTypeWhenFrozen(b.hi, tycon2))
                    || tycon2sym.byGadtBounds(b => isSubTypeWhenFrozen(tycon1, b.lo))
                    || byGadtOrdering
                  ) && {
                    // There are two cases in which we can assume injectivity.
                    // First we check if either sym is a class.
                    // Then:
                    // 1) if we didn't touch GADTs, then both symbols are the same
                    //    (b/c of an earlier condition) and both are the same class
                    // 2) if we touched GADTs, then the _other_ symbol (class syms
                    //    cannot have GADT constraints), the one w/ GADT cstrs,
                    //    must be instantiated, making the two tycons equal
                    val tyconIsInjective =
                      (tycon1sym.isClass || tycon2sym.isClass)
                      && (!touchedGADTs || gadtIsInstantiated)

                    inFrozenGadtIf(!tyconIsInjective) {
                      if tycon1sym == tycon2sym && tycon1sym.isAliasType then
                        val preConstraint = constraint
                        isSubArgs(args1, args2, tp1, tparams)
                        && tryAlso(preConstraint, recur(tp1.superTypeNormalized, tp2.superTypeNormalized))
                      else
                        isSubArgs(args1, args2, tp1, tparams)
                    }
                  }
                  res && recordGadtUsageIf(touchedGADTs)
                case _ =>
                  false
              }
            case tycon1: TypeVar =>
              loop(tycon1.underlying, args1)
            case tycon1: AnnotatedType if !tycon1.isRefining =>
              loop(tycon1.underlying, args1)
            case _ =>
              false
          }
          loop(tycon1, args1)
        case _ =>
          false
      }

      /** `param2` can be instantiated to a type application prefix of the LHS
       *  or to a type application prefix of one of the LHS base class instances
       *  and the resulting type application is a supertype of `tp1`.
       */
      def canInstantiate(tycon2: TypeParamRef): Boolean = {
        def appOK(tp1base: Type) = tp1base match {
          case tp1base: AppliedType =>
            compareAppliedTypeParamRef(tycon2, args2, tp1base, fromBelow = true)
          case _ => false
        }

        val tp1w = tp1.widen
        appOK(tp1w) || tp1w.typeSymbol.isClass && {
          val classBounds = tycon2.classSymbols
          def liftToBase(bcs: List[ClassSymbol]): Boolean = bcs match {
            case bc :: bcs1 =>
              classBounds.exists(bc.derivesFrom) && appOK(nonExprBaseType(tp1, bc))
              || liftToBase(bcs1)
            case _ =>
              false
          }
          liftToBase(tp1w.baseClasses)
        }
      }

      /** Fall back to comparing either with `fourthTry` or against the lower
       *  approximation of the rhs.
       *  @param   tyconLo   The type constructor's lower approximation.
       */
      def fallback(tyconLo: Type) =
        either(fourthTry, isSubApproxHi(tp1, tyconLo.applyIfParameterized(args2)))

      /** Let `tycon2bounds` be the bounds of the RHS type constructor `tycon2`.
       *  Let `app2 = tp2` where the type constructor of `tp2` is replaced by
       *  `tycon2bounds.lo`.
       *  If both bounds are the same, continue with `tp1 <:< app2`.
       *  otherwise continue with either
       *
       *    tp1 <:< tp2    using fourthTry (this might instantiate params in tp1)
       *    tp1 <:< app2   using isSubType (this might instantiate params in tp2)
       */
      def compareLower(tycon2bounds: TypeBounds, tyconIsTypeRef: Boolean): Boolean =
        if ((tycon2bounds.lo `eq` tycon2bounds.hi) && !tycon2bounds.isInstanceOf[MatchAlias])
          if (tyconIsTypeRef) recur(tp1, tp2.superTypeNormalized) && recordGadtUsageIf(MatchType.thatReducesUsingGadt(tp2))
          else isSubApproxHi(tp1, tycon2bounds.lo.applyIfParameterized(args2))
        else
          fallback(tycon2bounds.lo)

      def byGadtBounds: Boolean =
        {
          tycon2 match
            case tycon2: TypeRef =>
              val tycon2sym = tycon2.symbol
              tycon2sym.onGadtBounds { bounds2 =>
                inFrozenGadt { compareLower(bounds2, tyconIsTypeRef = false) }
              }
            case _ => false
        } && recordGadtUsageIf(true)

      tycon2 match {
        case param2: TypeParamRef =>
          isMatchingApply(tp1) ||
          canConstrain(param2) && canInstantiate(param2) ||
          compareLower(bounds(param2), tyconIsTypeRef = false)
        case tycon2: TypeRef =>
          isMatchingApply(tp1) ||
          byGadtBounds ||
          defn.isCompiletimeAppliedType(tycon2.symbol) && compareCompiletimeAppliedType(tp2, tp1, fromBelow = true) || {
            tycon2.info match {
              case info2: TypeBounds =>
                compareLower(info2, tyconIsTypeRef = true)
              case info2: ClassInfo =>
                tycon2.name.startsWith("Tuple") &&
                  defn.isTupleNType(tp2) && recur(tp1, tp2.toNestedPairs) ||
                tryBaseType(info2.cls)
              case _ =>
                fourthTry
            }
          } || tryLiftedToThis2

        case tv: TypeVar =>
          if tv.isInstantiated then
            recur(tp1, tp2.superType)
          else
            compareAppliedType2(tp2, tv.origin, args2)
        case tycon2: AnnotatedType if !tycon2.isRefining =>
          recur(tp1, tp2.superType)
        case tycon2: AppliedType =>
          fallback(tycon2.lowerBound)
        case _ =>
          false
      }
    }

    /** Subtype test for the application `tp1 = tycon1[args1]`.
     */
    def compareAppliedType1(tp1: AppliedType, tycon1: Type, args1: List[Type]): Boolean =
      tycon1 match {
        case param1: TypeParamRef =>
          def canInstantiate = tp2 match {
            case tp2base: AppliedType =>
              compareAppliedTypeParamRef(param1, args1, tp2base, fromBelow = false)
            case _ =>
              false
          }
          canConstrain(param1) && canInstantiate ||
            isSubType(bounds(param1).hi.applyIfParameterized(args1), tp2, approx.addLow)
        case tycon1: TypeRef =>
          val sym = tycon1.symbol

          def byGadtBounds: Boolean =
            sym.onGadtBounds { bounds1 =>
              inFrozenGadt { isSubType(bounds1.hi.applyIfParameterized(args1), tp2, approx.addLow) }
            } && recordGadtUsageIf(true)


          !sym.isClass && {
            defn.isCompiletimeAppliedType(sym) && compareCompiletimeAppliedType(tp1, tp2, fromBelow = false) ||
            { recur(tp1.superTypeNormalized, tp2) && recordGadtUsageIf(MatchType.thatReducesUsingGadt(tp1)) } ||
            tryLiftedToThis1
          } || byGadtBounds
        case tycon1: TypeProxy =>
          recur(tp1.superTypeNormalized, tp2)
        case _ =>
          false
      }

    /** Compare `tp` of form `S[arg]` with `other`, via ">:>" if fromBelow is true, "<:<" otherwise.
     *  If `arg` is a Nat constant `n`, proceed with comparing `n + 1` and `other`.
     *  Otherwise, if `other` is a Nat constant `n`, proceed with comparing `arg` and `n - 1`.
     */
    def compareS(tp: AppliedType, other: Type, fromBelow: Boolean): Boolean = tp.args match {
      case arg :: Nil =>
        natValue(arg) match {
          case Some(n) if n != Int.MaxValue =>
            val succ = ConstantType(Constant(n + 1))
            if (fromBelow) recur(other, succ) else recur(succ, other)
          case none =>
            natValue(other) match {
              case Some(n) if n > 0 =>
                val pred = ConstantType(Constant(n - 1))
                if (fromBelow) recur(pred, arg) else recur(arg, pred)
              case none =>
                false
            }
        }
      case _ => false
    }

    /** Compare `tp` of form `tycon[...args]`, where `tycon` is a scala.compiletime type,
     *  with `other` via ">:>" if fromBelow is true, "<:<" otherwise.
     *  Delegates to compareS if `tycon` is scala.compiletime.S. Otherwise, constant folds if possible.
     */
    def compareCompiletimeAppliedType(tp: AppliedType, other: Type, fromBelow: Boolean): Boolean = {
      if (defn.isCompiletime_S(tp.tycon.typeSymbol)) compareS(tp, other, fromBelow)
      else {
        val folded = tp.tryCompiletimeConstantFold
        if (fromBelow) recur(other, folded) else recur(folded, other)
      }
    }

    /** Like tp1 <:< tp2, but returns false immediately if we know that
     *  the case was covered previously during subtyping.
     *
     *  A type has been covered previously in subtype checking if it
     *  is some combination of TypeRefs that point to classes, where the
     *  combiners are AppliedTypes, RefinedTypes, RecTypes, And/Or-Types or AnnotatedTypes.
     *
     *  The exception is that if both sides contain OrTypes, the check hasn't been covered.
     *  See #17465.
     */
    def isNewSubType(tp1: Type): Boolean =
      def isCovered(tp: Type): CoveredStatus =
        tp.dealiasKeepRefiningAnnots.stripTypeVar match
          case tp: TypeRef =>
            if tp.symbol.isClass && tp.symbol != NothingClass && tp.symbol != NullClass
            then CoveredStatus.Covered
            else CoveredStatus.Uncovered
          case tp: AppliedType => isCovered(tp.tycon)
          case tp: RefinedOrRecType => isCovered(tp.parent)
          case tp: AndType => isCovered(tp.tp1) min isCovered(tp.tp2)
          case tp: OrType  => isCovered(tp.tp1) min isCovered(tp.tp2) min CoveredStatus.CoveredWithOr
          case _ => CoveredStatus.Uncovered

      val covered1 = isCovered(tp1)
      val covered2 = isCovered(tp2)
      if (covered1 min covered2) >= CoveredStatus.CoveredWithOr && (covered1 max covered2) == CoveredStatus.Covered then
        //println(s"useless subtype: $tp1 <:< $tp2")
        false
      else isSubType(tp1, tp2, approx.addLow)

    def isSubApproxHi(tp1: Type, tp2: Type): Boolean =
      tp1.eq(tp2) || tp2.ne(NothingType) && isSubType(tp1, tp2, approx.addHigh)

    def tryLiftedToThis1: Boolean = {
      val tp1a = liftToThis(tp1)
      (tp1a ne tp1) && recur(tp1a, tp2) && { opaquesUsed = true; true }
    }

    def tryLiftedToThis2: Boolean = {
      val tp2a = liftToThis(tp2)
      (tp2a ne tp2) && recur(tp1, tp2a) && { opaquesUsed = true; true }
    }

    // begin recur
    if tp2 eq NoType then false
    else if tp1 eq tp2 then true
    else
      val savedCstr = constraint
      val savedGadt = ctx.gadt
      inline def restore() =
        state.constraint = savedCstr
        ctx.gadtState.restore(savedGadt)
      val savedSuccessCount = successCount
      try
        recCount += 1
        if recCount >= Config.LogPendingSubTypesThreshold then monitored = true
        val result = if monitored then monitoredIsSubType else firstTry
        recCount -= 1
        if !result then restore()
        else if recCount == 0 && needsGc then
          state.gc()
          needsGc = false
        if (Stats.monitored) recordStatistics(result, savedSuccessCount)
        result
      catch case NonFatal(ex) =>
        if ex.isInstanceOf[AssertionError] then showGoal(tp1, tp2)
        recCount -= 1
        restore()
        successCount = savedSuccessCount
        throw ex
  }

  private def nonExprBaseType(tp: Type, cls: Symbol)(using Context): Type =
    if tp.isInstanceOf[ExprType] then NoType
    else tp.baseType(cls)

  /** If `tp` is an external reference to an enclosing module M that contains opaque types,
   *  convert to M.this.
   *  Note: It would be legal to do the lifting also if M does not contain opaque types,
   *  but in this case the retries in tryLiftedToThis would be redundant.
   */
  private def liftToThis(tp: Type): Type = {

    def findEnclosingThis(moduleClass: Symbol, from: Symbol): Type =
      if ((from.owner eq moduleClass) && from.isPackageObject && from.is(Opaque)) from.thisType
      else if (from.is(Package)) tp
      else if ((from eq moduleClass) && from.is(Opaque)) from.thisType
      else if (from eq NoSymbol) tp
      else findEnclosingThis(moduleClass, from.owner)

    tp match {
      case tp: TermRef if tp.symbol.is(Module) =>
        findEnclosingThis(tp.symbol.moduleClass, ctx.owner)
      case tp: TypeRef =>
        val pre1 = liftToThis(tp.prefix)
        if ((pre1 ne tp.prefix) && pre1.exists) tp.withPrefix(pre1) else tp
      case tp: ThisType if tp.cls.is(Package) =>
        findEnclosingThis(tp.cls, ctx.owner)
      case tp: AppliedType =>
        val tycon1 = liftToThis(tp.tycon)
        if (tycon1 ne tp.tycon) tp.derivedAppliedType(tycon1, tp.args) else tp
      case tp: TypeVar if tp.isInstantiated =>
        liftToThis(tp.inst)
      case tp: AnnotatedType =>
        val parent1 = liftToThis(tp.parent)
        if (parent1 ne tp.parent) tp.derivedAnnotatedType(parent1, tp.annot) else tp
      case _ =>
        tp
    }
  }

  /** Optionally, the `n` such that `tp <:< ConstantType(Constant(n: Int))` */
  def natValue(tp: Type): Option[Int] = constValue(tp) match {
    case Some(Constant(n: Int)) if n >= 0 => Some(n)
    case _ => None
  }

  /** Optionally, the constant `c` such that `tp <:< ConstantType(c)` */
  def constValue(tp: Type): Option[Constant] = {
    val ct = new AnyConstantType
    if (isSubTypeWhenFrozen(tp, ct))
      ct.tpe match {
        case ConstantType(c) => Some(c)
        case _ => None
      }
    else None
  }

  /** If both `tp1` and `tp2` have atoms information, compare the atoms
   *  in a Some, otherwise None.
   *  @param knownSingletons  If true, we are coming from a comparison of two singleton types
   *                          This influences the comparison as shown below:
   *
   *  Say you have singleton types p.type and q.type the atoms of p.type are `{p.type}..{p.type}`,
   *  and the atoms of `q.type` are `{}..{p.type}`. Normally the atom comparison between p's
   *  atoms and q's atoms gives false. But in this case we know that `q.type` is an alias of `p.type`
   *  so we are still allowed to conclude that `p.type <:< q.type`. A situation where this happens
   *  is in i6635.scala. Here,
   *
   *     p: A, q: B & p.type   and we want to conclude that p.type <: q.type.
   */
  def compareAtoms(tp1: Type, tp2: Type, knownSingletons: Boolean = false): Option[Boolean] =

    /** Check whether we can compare the given set of atoms with another to determine
     *  a subtype test between OrTypes. There is one situation where this is not
     *  the case, which has to do with SkolemTypes. TreeChecker sometimes expects two
     *  types to be equal that have different skolems. To account for this, we identify
     *  two different skolems in all phases `p`, where `p.isTyper` is false.
     *  But in that case comparing two sets of atoms that contain skolems
     *  for equality would give the wrong result, so we should not use the sets
     *  for comparisons.
     */
    def canCompare(ts: Set[Type]) =
      ctx.phase.isTyper
      || !ts.exists(_.existsPart(_.isInstanceOf[SkolemType], StopAt.Static))

    def verified(result: Boolean): Boolean =
      if Config.checkAtomsComparisons then
        try
          canCompareAtoms = false
          val regular = recur(tp1, tp2)
          assert(result == regular,
            i"""Atoms inconsistency for $tp1 <:< $tp2
              |atoms predicted $result
              |atoms1 = ${tp1.atoms}
              |atoms2 = ${tp2.atoms}""")
        finally canCompareAtoms = true
      result

    tp2.atoms match
      case Atoms.Range(lo2, hi2) if canCompareAtoms && canCompare(hi2) =>
        tp1.atoms match
          case Atoms.Range(lo1, hi1) =>
            if hi1.subsetOf(lo2) || knownSingletons && hi2.size == 1 && hi1 == hi2 then
              Some(verified(true))
            else if !lo1.subsetOf(hi2) then
              Some(verified(false))
            else
              None
          case _ => Some(verified(recur(tp1, NothingType)))
      case _ => None

  /** Subtype test for corresponding arguments in `args1`, `args2` according to
   *  variances in type parameters `tparams2`.
   *
   *  @param  tp1       The applied type containing `args1`
   *  @param  tparams2  The type parameters of the type constructor applied to `args2`
   */
  def isSubArgs(args1: List[Type], args2: List[Type], tp1: Type, tparams2: List[ParamInfo]): Boolean = {

    /** The bounds of parameter `tparam`, where all references to type paramneters
     *  are replaced by corresponding arguments (or their approximations in the case of
     *  wildcard arguments).
     */
    def paramBounds(tparam: Symbol): TypeBounds =
      tparam.info.substApprox(tparams2.asInstanceOf[List[Symbol]], args2).bounds

    /** Test all arguments. Incomplete argument tests (according to isIncomplete) are deferred in
     *  the first run and picked up in the second.
     */
    def recurArgs(args1: List[Type], args2: List[Type], tparams2: List[ParamInfo],
                  canDefer: Boolean,
                  deferred1: List[Type], deferred2: List[Type], deferredTparams2: List[ParamInfo]): Boolean =
      if args1.isEmpty then
        args2.isEmpty
        && (deferred1.isEmpty
            || recurArgs(
                  deferred1.reverse, deferred2.reverse, deferredTparams2.reverse,
                  canDefer = false, Nil, Nil, Nil))
      else args2.nonEmpty && tparams2.nonEmpty && {
        val tparam = tparams2.head
        val v = tparam.paramVarianceSign

        /** An argument test is incomplete if it implies a comparison A <: B where
         *  A is an AndType or B is an OrType. In these cases we need to run an
         *  either, which can lose solutions if there are type variables involved.
         *  So we defer such tests to run last, on the chance that some other argument
         *  comparison will instantiate or constrain type variables first.
         */
        def isIncomplete(arg1: Type, arg2: Type): Boolean =
          val arg1d = arg1.strippedDealias
          val arg2d = arg2.strippedDealias
          (v >= 0) && (arg1d.isInstanceOf[AndType] || arg2d.isInstanceOf[OrType])
          ||
          (v <= 0) && (arg1d.isInstanceOf[OrType] || arg2d.isInstanceOf[AndType])

        /** Try a capture conversion:
         *  If the original left-hand type `leftRoot` is a path `p.type`,
         *  and the current widened left type is an application with wildcard arguments
         *  such as `C[?]`, where `X` is `C`'s type parameter corresponding to the `_` argument,
         *  compare with `C[p.X]` instead. Otherwise approximate based on variance.
         *  Also do a capture conversion in either of the following cases:
         *
         *   - If we are after typer. We generally relax soundness requirements then.
         *     We need the relaxed condition to correctly compute overriding relationships.
         *     Missing this case led to AbstractMethod errors in the bootstrap.
         *
         *   - If we are in mode TypevarsMissContext, which means we test implicits
         *     for eligibility. In this case, we can be more permissive, since it's
         *     just a pre-check. This relaxation is needed since the full
         *     implicit typing might perform an adaptation that skolemizes the
         *     type of a synthesized tree before comparing it with an expected type.
         *     But no such adaptation is applied for implicit eligibility
         *     testing, so we have to compensate.
         *
         *  Note: Doing the capture conversion on path types is actually not necessary
         *  since we can already deal with the situation through skolemization in Typer#captureWildcards.
         *  But performance tests indicate that it's better to do it, since we avoid
         *  skolemizations, which are more expensive . And, besides, capture conversion on
         *  paths is less intrusive than skolemization.
         */
        def compareCaptured(arg1: TypeBounds, arg2: Type) = tparam match {
          case tparam: Symbol =>
            val leftr = leftRoot.nn
            if (leftr.isStable || ctx.isAfterTyper || ctx.mode.is(Mode.TypevarsMissContext))
                && leftr.isValueType
                && leftr.member(tparam.name).exists
            then
              val captured = TypeRef(leftr, tparam)
              try isSubArg(captured, arg2)
              catch case ex: TypeError =>
                // The captured reference could be illegal and cause a
                // TypeError to be thrown in argDenot
                false
            else if (v > 0)
              isSubType(paramBounds(tparam).hi, arg2)
            else if (v < 0)
              isSubType(arg2, paramBounds(tparam).lo)
            else
              false
          case _ =>
            false
        }

        def isSubArg(arg1: Type, arg2: Type): Boolean = arg2 match
          case arg2: TypeBounds =>
            val arg1norm = arg1 match {
              case arg1: TypeBounds =>
                tparam match {
                  case tparam: Symbol => arg1 & paramBounds(tparam)
                  case _ => arg1 // This case can only arise when a hk-type is illegally instantiated with a wildcard
                }
              case _ => arg1
            }
            arg2.contains(arg1norm)
          case ExprType(arg2res)
          if ctx.phaseId > elimByNamePhase.id && !ctx.erasedTypes
               && defn.isByNameFunction(arg1.dealias) =>
            // ElimByName maps `=> T` to `()? => T`, but only in method parameters. It leaves
            // embedded `=> T` arguments alone. This clause needs to compensate for that.
            isSubArg(arg1.dealias.argInfos.head, arg2res)
          case _ =>
            arg1 match
              case arg1: TypeBounds =>
                CaptureSet.subCapturesRange(arg1, arg2)
                  // subCapturesRange is important for invariant arguments that get expanded
                  // to TypeBounds where each bound is obtained by adding a captureset variable
                  // to the argument type. If subCapturesRange returns true we know that arg1's'
                  // capture set can be unified with arg2's capture set, so it only remains to
                  // check the underlying types with `isSubArg`.
                  && isSubArg(arg1.hi.stripCapturing, arg2.stripCapturing)
                || compareCaptured(arg1, arg2)
              case ExprType(arg1res)
              if ctx.phaseId > elimByNamePhase.id && !ctx.erasedTypes
                   && defn.isByNameFunction(arg2.dealias) =>
                 isSubArg(arg1res, arg2.argInfos.head)
              case _ =>
                if v < 0 then isSubType(arg2, arg1)
                else if v > 0 then isSubType(arg1, arg2)
                else isSameType(arg2, arg1)

        val arg1 = args1.head
        val arg2 = args2.head
        val rest1 = args1.tail
        if !canDefer
            || rest1.isEmpty && deferred1.isEmpty
                // skip the incompleteness test if this is the last argument and no previous argument tests were incomplete
            || !isIncomplete(arg1, arg2)
        then
          isSubArg(arg1, arg2)
          && recurArgs(
                rest1, args2.tail, tparams2.tail, canDefer,
                deferred1, deferred2, deferredTparams2)
        else
          recurArgs(
            rest1, args2.tail, tparams2.tail, canDefer,
            arg1 :: deferred1, arg2 :: deferred2, tparams2.head :: deferredTparams2)
      }

    recurArgs(args1, args2, tparams2, canDefer = true, Nil, Nil, Nil)

  }

  /** Test whether `tp1` has a base type of the form `B[T1, ..., Tn]` where
   *   - `B` derives from one of the class symbols of `tp2`,
   *   - the type parameters of `B` match one-by-one the variances of `tparams`,
   *   - `B` satisfies predicate `p`.
   */
  private def testLifted(tp1: Type, tp2: Type, tparams: List[TypeParamInfo], p: Type => Boolean): Boolean = {
    val classBounds = tp2.classSymbols
    def recur(bcs: List[ClassSymbol]): Boolean = bcs match {
      case bc :: bcs1 =>
        (classBounds.exists(bc.derivesFrom) &&
          variancesConform(bc.typeParams, tparams) &&
          p(nonExprBaseType(tp1, bc))
        ||
        recur(bcs1))
      case nil =>
        false
    }
    recur(tp1.baseClasses)
  }

  /** Replace any top-level recursive type `{ z => T }` in `tp` with
   *  `[z := anchor]T`.
   */
  private def fixRecs(anchor: SingletonType, tp: Type): Type = {
    def fix(tp: Type): Type = tp.stripTypeVar match {
      case tp: RecType => fix(tp.parent).substRecThis(tp, anchor)
      case tp: RefinedType => tp.derivedRefinedType(parent = fix(tp.parent))
      case tp: TypeParamRef => fixOrElse(bounds(tp).hi, tp)
      case tp: TypeProxy => fixOrElse(tp.superType, tp)
      case tp: AndType => tp.derivedAndType(fix(tp.tp1), fix(tp.tp2))
      case tp: OrType  => tp.derivedOrType (fix(tp.tp1), fix(tp.tp2))
      case tp => tp
    }
    def fixOrElse(tp: Type, fallback: Type) = {
      val tp1 = fix(tp)
      if (tp1 ne tp) tp1 else fallback
    }
    fix(tp)
  }

  /** Returns true iff the result of evaluating either `op1` or `op2` is true and approximates resulting constraints.
   *
   *  If `necessaryConstraintsOnly` is true, we keep the smaller constraint if
   *  any, or no constraint at all.
   *
   *  Otherwise, we infer _sufficient_ constraints: we try to keep the smaller of
   *  the two constraints, but if neither is smaller than the other, we just pick
   *  the first one.
   */
  protected def either(op1: => Boolean, op2: => Boolean): Boolean =
    Stats.record("TypeComparer.either")
    if necessaryConstraintsOnly then
      necessaryEither(op1, op2)
    else
      sufficientEither(op1, op2)

  /** Returns true iff the result of evaluating either `op1` or `op2` is true,
   *  trying at the same time to keep the constraint as wide as possible.
   *  E.g, if
   *
   *    tp11 <:< tp12 = true   with post-constraint c1
   *    tp12 <:< tp22 = true   with post-constraint c2
   *
   *  and c1 subsumes c2, then c2 is kept as the post-constraint of the result,
   *  otherwise c1 is kept.
   *
   *  This method is used to approximate a solution in one of the following cases
   *
   *     T1 & T2 <:< T3
   *     T1 <:< T2 | T3
   *
   *  In the first case (the second one is analogous), we have a choice whether we
   *  want to establish the subtyping judgement using
   *
   *     T1 <:< T3   or    T2 <:< T3
   *
   *  as a precondition. Either precondition might constrain type variables.
   *  The purpose of this method is to pick the precondition that constrains less.
   *  The method is not complete, because sometimes there is no best solution. Example:
   *
   *     A? & B?  <:  T
   *
   *  Here, each precondition leads to a different constraint, and neither of
   *  the two post-constraints subsumes the other.
   *
   *  Note that to be complete when it comes to typechecking, we would instead need to backtrack
   *  and attempt to typecheck with the other constraint.
   *
   *  Method name comes from the notion that we are keeping a constraint which is sufficient to satisfy
   *  one of subtyping relationships.
   */
  private def sufficientEither(op1: => Boolean, op2: => Boolean): Boolean =
    val preConstraint = constraint
    if op1 then tryAlso(preConstraint, op2) else op2

  /** Check whether `op` generates a weaker constraint than the
   *  current constraint if we run it starting with `preConstraint`.
   *  If that's the case, replace the current constraint with the
   *  constraint generated by `op`.
   */
  private def tryAlso(preConstraint: Constraint, op: => Boolean): true =
    if constraint ne preConstraint then
      // check whether `op2` generates a weaker constraint than `op1`
      val leftConstraint = constraint
      constraint = preConstraint
      if !(op && subsumes(leftConstraint, constraint, preConstraint)) then
        if constr != noPrinter && !subsumes(constraint, leftConstraint, preConstraint) then
          constr.println(i"CUT - prefer $leftConstraint over $constraint")
        constraint = leftConstraint
    true

  /** Returns true iff the result of evaluating either `op1` or `op2` is true, keeping the smaller constraint if any.
   *  E.g., if
   *
   *    tp11 <:< tp12 = true   with constraint c1 and GADT constraint g1
   *    tp12 <:< tp22 = true   with constraint c2 and GADT constraint g2
   *
   *  We keep:
   *    - (c1, g1) if c2 subsumes c1 and g2 subsumes g1
   *    - (c2, g2) if c1 subsumes c2 and g1 subsumes g2
   *    - neither constraint pair otherwise.
   *
   *  Like [[sufficientEither]], this method is used to approximate a solution in one of the following cases:
   *
   *     T1 & T2 <:< T3
   *     T1 <:< T2 | T3
   *
   *  But this method is used when `useNecessaryEither` is true, like when we are attempting
   *  to infer GADT constraints that necessarily follow from the subtyping relationship. For instance, if we have
   *
   *     enum Expr[T] {
   *       case IntExpr(i: Int) extends Expr[Int]
   *       case StrExpr(s: String) extends Expr[String]
   *     }
   *
   *  and `A` is an abstract type and we know that
   *
   *     Expr[A] <: IntExpr | StrExpr
   *
   *  (the case with &-type is analogous) then this may follow either from
   *
   *     Expr[A] <: IntExpr    or    Expr[A] <: StrExpr
   *
   *  Since we don't know which branch is true, we need to give up and not keep either constraint. OTOH, if one
   *  constraint pair is subsumed by the other, we know that it is necessary for both cases and therefore we can
   *  keep it.
   *
   *  Like [[sufficientEither]], this method is not complete because sometimes, the necessary constraint
   *  is neither of the pairs. For instance, if
   *
   *     g1 = { A = Int, B = String }
   *     g2 = { A = Int, B = Int }
   *
   *  then the necessary constraint is { A = Int }, but correctly inferring that is, as far as we know, too expensive.
   *
   *  This method is also used in ConstrainResult mode
   *  to avoid inference getting stuck due to lack of backtracking,
   *  see or-inf.scala and and-inf.scala for examples.
   *
   *  Method name comes from the notion that we are keeping the constraint which is necessary to satisfy both
   *  subtyping relationships.
   */
  private def necessaryEither(op1: => Boolean, op2: => Boolean): Boolean =
    val preConstraint = constraint
    val preGadt = ctx.gadt

    def allSubsumes(leftGadt: GadtConstraint, rightGadt: GadtConstraint, left: Constraint, right: Constraint): Boolean =
      subsumes(left, right, preConstraint)
      && subsumes(leftGadt.constraint, rightGadt.constraint, preGadt.constraint)

    if op1 then
      val op1Constraint = constraint
      val op1Gadt = ctx.gadt
      constraint = preConstraint
      ctx.gadtState.restore(preGadt)
      if op2 then
        if allSubsumes(op1Gadt, ctx.gadt, op1Constraint, constraint) then
          gadts.println(i"GADT CUT - prefer ${ctx.gadt} over $op1Gadt")
          constr.println(i"CUT - prefer $constraint over $op1Constraint")
        else if allSubsumes(ctx.gadt, op1Gadt, constraint, op1Constraint) then
          gadts.println(i"GADT CUT - prefer $op1Gadt over ${ctx.gadt}")
          constr.println(i"CUT - prefer $op1Constraint over $constraint")
          constraint = op1Constraint
          ctx.gadtState.restore(op1Gadt)
        else
          gadts.println(i"GADT CUT - no constraint is preferable, reverting to $preGadt")
          constr.println(i"CUT - no constraint is preferable, reverting to $preConstraint")
          constraint = preConstraint
          ctx.gadtState.restore(preGadt)
      else
        constraint = op1Constraint
        ctx.gadtState.restore(op1Gadt)
      true
    else op2
  end necessaryEither

  inline def rollbackConstraintsUnless(inline op: Boolean): Boolean =
    val saved = constraint
    var result = false
    try result = ctx.gadtState.rollbackGadtUnless(op)
    finally if !result then constraint = saved
    result

  /** Decompose into conjunction of types each of which has only a single refinement */
  def decomposeRefinements(tp: Type, refines: List[(Name, Type)]): Type = tp match
    case RefinedType(parent, rname, rinfo) =>
      decomposeRefinements(parent, (rname, rinfo) :: refines)
    case AndType(tp1, tp2) =>
      AndType(decomposeRefinements(tp1, refines), decomposeRefinements(tp2, refines))
    case _ =>
      refines.map(RefinedType(tp, _, _): Type).reduce(AndType(_, _))

  /** Can comparing this type on the left lead to an either? This is the case if
   *  the type is and AndType or contains embedded occurrences of AndTypes
   */
  def containsAnd(tp: Type): Boolean = tp match
    case tp: AndType => true
    case OrType(tp1, tp2) => containsAnd(tp1) || containsAnd(tp2)
    case tp: TypeParamRef => containsAnd(bounds(tp).hi)
    case tp: TypeRef => containsAnd(tp.info.hiBound) || tp.symbol.onGadtBounds(gbounds => containsAnd(gbounds.hi))
    case tp: TypeProxy => containsAnd(tp.superType)
    case _ => false

  /** Does type `tp1` have a member with name `name` whose normalized type is a subtype of
   *  the normalized type of the refinement of `tp2`?
   *  Normalization is as follows: If `tp2` contains a skolem to its refinement type,
   *  rebase both itself and the member info of `tp` on a freshly created skolem type.
   */
  def hasMatchingMember(name: Name, tp1: Type, tp2: RefinedType): Boolean =
    trace(i"hasMatchingMember($tp1 . $name :? ${tp2.refinedInfo}), mbr: ${tp1.member(name).info}", subtyping) {

      // If the member is an abstract type and the prefix is a path, compare the member itself
      // instead of its bounds. This case is needed situations like:
      //
      //    class C { type T }
      //    val foo: C
      //    foo.type <: C { type T {= , <: , >:} foo.T }
      //
      // or like:
      //
      //    class C[T]
      //    C[?] <: C[TV]
      //
      // where TV is a type variable. See i2397.scala for an example of the latter.
      def matchAbstractTypeMember(info1: Type): Boolean = info1 match {
        case TypeBounds(lo, hi) if lo ne hi =>
          tp2.refinedInfo match {
            case rinfo2: TypeBounds if tp1.isStable =>
              val ref1 = tp1.widenExpr.select(name)
              isSubType(rinfo2.lo, ref1) && isSubType(ref1, rinfo2.hi)
            case _ =>
              false
          }
        case _ => false
      }

      // An additional check for type member matching: If the refinement of the
      // supertype `tp2` does not refer to a member symbol defined in the parent of `tp2`.
      // then the symbol referred to in the subtype must have a signature that coincides
      // in its parameters with the refinement's signature. The reason for the check
      // is that if the refinement does not refer to a member symbol, we will have to
      // resort to reflection to invoke the member. And Java reflection needs to know exact
      // erased parameter types. See neg/i12211.scala. Other reflection algorithms could
      // conceivably dispatch without knowing precise parameter signatures. One can signal
      // this by inheriting from the `scala.reflect.SignatureCanBeImprecise` marker trait,
      // in which case the signature test is elided.
      def sigsOK(symInfo: Type, info2: Type) =
        tp2.underlyingClassRef(refinementOK = true).member(name).exists
        || tp2.derivesFrom(defn.WithoutPreciseParameterTypesClass)
        || symInfo.isInstanceOf[MethodType]
            && symInfo.signature.consistentParams(info2.signature)

      def tp1IsSingleton: Boolean = tp1.isInstanceOf[SingletonType]

      // A relaxed version of isSubType, which compares method types
      // under the standard arrow rule which is contravariant in the parameter types,
      // but under the condition that signatures might have to match (see sigsOK)
      // This relaxed version is needed to correctly compare dependent function types.
      // See pos/i12211.scala.
      def isSubInfo(info1: Type, info2: Type, symInfo: Type): Boolean =
        info2 match
          case info2: MethodType =>
            info1 match
              case info1: MethodType =>
                val symInfo1 = symInfo.stripPoly
                matchingMethodParams(info1, info2, precise = false)
                && isSubInfo(info1.resultType, info2.resultType.subst(info2, info1), symInfo1.resultType)
                && sigsOK(symInfo1, info2)
              case _ => inFrozenGadtIf(tp1IsSingleton) { isSubType(info1, info2) }
          case _ => inFrozenGadtIf(tp1IsSingleton) { isSubType(info1, info2) }

      def qualifies(m: SingleDenotation): Boolean =
        val info2 = tp2.refinedInfo
        val isExpr2 = info2.isInstanceOf[ExprType]
        var info1 = m.info match
          case info1: ValueType if isExpr2 || m.symbol.is(Mutable) =>
            // OK: { val x: T } <: { def x: T }
            // OK: { var x: T } <: { def x: T }
            // NO: { var x: T } <: { val x: T }
            ExprType(info1)
          case info1 @ MethodType(Nil) if isExpr2 && m.symbol.is(JavaDefined) =>
            // OK{ { def x(): T } <: { def x: T} // if x is Java defined
            ExprType(info1.resType)
          case info1 => info1

        m.symbol.hasTargetName(m.symbol.name) && (
          isSubInfo(info1, info2, m.symbol.info.orElse(info1))
          || matchAbstractTypeMember(m.info)
          || (tp1.isStable && m.symbol.isStableMember && isSubType(TermRef(tp1, m.symbol), tp2.refinedInfo)))
      end qualifies

      tp1.member(name).hasAltWithInline(qualifies)
    }

  final def ensureStableSingleton(tp: Type): SingletonType = tp.stripTypeVar match {
    case tp: SingletonType if tp.isStable => tp
    case tp: ValueType => SkolemType(tp)
    case tp: TypeProxy => ensureStableSingleton(tp.superType)
    case tp => assert(ctx.reporter.errorsReported); SkolemType(tp)
  }

  /** Skip refinements in `tp2` which match corresponding refinements in `tp1`.
   *  "Match" means:
   *   - they appear in the same order,
   *   - they refine the same names,
   *   - the refinement in `tp1` is an alias type, and
   *   - neither refinement refers back to the refined type via a refined this.
   *  @return  The parent type of `tp2` after skipping the matching refinements.
   */
  private def skipMatching(tp1: Type, tp2: RefinedType): Type = tp1 match {
    case tp1 @ RefinedType(parent1, name1, rinfo1: TypeAlias) if name1 == tp2.refinedName =>
      tp2.parent match {
        case parent2: RefinedType => skipMatching(parent1, parent2)
        case parent2 => parent2
      }
    case _ => tp2
  }

  /** Are refinements in `tp1` pairwise subtypes of the refinements of `tp2`
   *  up to parent type `limit`?
   *  @pre `tp1` has the necessary number of refinements, they are type aliases,
   *       and their names match the corresponding refinements in `tp2`.
   *       Further, no refinement refers back to the refined type via a refined this.
   *  The precondition is established by `skipMatching`.
   */
  private def isSubRefinements(tp1: RefinedType, tp2: RefinedType, limit: Type): Boolean =
    isSubType(tp1.refinedInfo, tp2.refinedInfo)
    && ((tp2.parent eq limit)
       || isSubRefinements(
            tp1.parent.asInstanceOf[RefinedType],
            tp2.parent.asInstanceOf[RefinedType], limit))

  /** Defer constraining type variables when compared against prototypes */
  def isMatchedByProto(proto: ProtoType, tp: Type): Boolean = tp.stripTypeVar match {
    case tp: TypeParamRef if constraint contains tp => true
    case _ => proto.isMatchedBy(tp, keepConstraint = true)
  }

  /** Narrow gadt.bounds for the type parameter referenced by `tr` to include
   *  `bound` as an upper or lower bound (which depends on `isUpper`).
   *  Test that the resulting bounds are still satisfiable.
   */
  private def narrowGADTBounds(tr: NamedType, bound: Type, approx: ApproxState, isUpper: Boolean): Boolean = {
    val boundImprecise = approx.high || approx.low
    ctx.mode.is(Mode.GadtConstraintInference) && !frozenGadt && !frozenConstraint && !boundImprecise && {
      val tparam = tr.symbol
      gadts.println(i"narrow gadt bound of $tparam: ${tparam.info} from ${if (isUpper) "above" else "below"} to $bound ${bound.toString} ${bound.isRef(tparam)}")
      if (bound.isRef(tparam)) false
      else
        ctx.gadtState.rollbackGadtUnless(gadtAddBound(tparam, bound, isUpper))
    }
  }

  // Tests around `matches`

  /** A function implementing `tp1` matches `tp2`. */
  final def matchesType(tp1: Type, tp2: Type, relaxed: Boolean): Boolean = tp1.widen match {
    case tp1: MethodType =>
      tp2.widen match {
        case tp2: MethodType =>
          // implicitness is ignored when matching
          matchingMethodParams(tp1, tp2) &&
          matchesType(tp1.resultType, tp2.resultType.subst(tp2, tp1), relaxed)
        case tp2 =>
          relaxed && tp1.paramNames.isEmpty &&
            matchesType(tp1.resultType, tp2, relaxed)
      }
    case tp1: PolyType =>
      tp2.widen match {
        case tp2: PolyType =>
          tp1.paramNames.hasSameLengthAs(tp2.paramNames) &&
          matchesType(tp1.resultType, tp2.resultType.subst(tp2, tp1), relaxed)
        case _ =>
          false
      }
    case _ =>
      tp2.widen match {
        case _: PolyType =>
          false
        case tp2: MethodType =>
          relaxed && tp2.paramNames.isEmpty &&
            matchesType(tp1, tp2.resultType, relaxed)
        case tp2 =>
          relaxed || isSameType(tp1, tp2)
      }
  }

  /** Do the parameter types of `tp1` and `tp2` match in a way that allows `tp1`
   *  to override `tp2` ? Two modes: precise or not.
   *  If `precise` is set (which is the default) this is the case if they're pairwise `=:=`.
   *  Otherwise parameters in `tp2` must be subtypes of corresponding parameters in `tp1`.
   */
  def matchingMethodParams(tp1: MethodType, tp2: MethodType, precise: Boolean = true): Boolean = {
    def loop(formals1: List[Type], formals2: List[Type]): Boolean = formals1 match {
      case formal1 :: rest1 =>
        formals2 match {
          case formal2 :: rest2 =>
            val formal2a = if (tp2.isParamDependent) formal2.subst(tp2, tp1) else formal2
            val paramsMatch =
              if precise then
                isSameTypeWhenFrozen(formal1, formal2a)
              else if isCaptureCheckingOrSetup then
                // allow to constrain capture set variables
                isSubType(formal2a, formal1)
              else
                isSubTypeWhenFrozen(formal2a, formal1)
            paramsMatch && loop(rest1, rest2)
          case nil =>
            false
        }
      case nil =>
        formals2.isEmpty
    }
    // If methods have erased parameters, then the erased parameters must match
    val erasedValid = (!tp1.hasErasedParams && !tp2.hasErasedParams) || (tp1.erasedParams == tp2.erasedParams)

    erasedValid && loop(tp1.paramInfos, tp2.paramInfos)
  }

  /** Do the parameter types of `tp1` and `tp2` match in a way that allows `tp1`
   *  to override `tp2` ? This is the case if they're pairwise >:>.
   */
  def matchingPolyParams(tp1: PolyType, tp2: PolyType): Boolean = {
    def loop(formals1: List[Type], formals2: List[Type]): Boolean = formals1 match {
      case formal1 :: rest1 =>
        formals2 match {
          case formal2 :: rest2 =>
            val formal2a = formal2.subst(tp2, tp1)
            isSubTypeWhenFrozen(formal2a, formal1) &&
            loop(rest1, rest2)
          case nil =>
            false
        }
      case nil =>
        formals2.isEmpty
    }
    loop(tp1.paramInfos, tp2.paramInfos)
  }

  // Type equality =:=

  /** Two types are the same if they are mutual subtypes of each other.
   *  To avoid exponential blowup for deeply nested invariant applied types,
   *  we cache successes once the stack of outstanding isSameTypes reaches
   *  depth `startSameTypeTrackingLevel`. See pos/i15525.scala, where this matters.
   */
  def isSameType(tp1: Type, tp2: Type): Boolean =
    if tp1 eq NoType then false
    else if tp1 eq tp2 then true
    else if sames != null && (sames.nn.lookup(tp1) eq tp2) then true
    else
      val savedSames = sames
      sameLevel += 1
      if sameLevel >= startSameTypeTrackingLevel then
        Stats.record("cache same type")
        sames = new util.EqHashMap()
      val res =
        try isSubType(tp1, tp2) && isSubType(tp2, tp1)
        finally
          sameLevel -= 1
          sames = savedSames
      if res && sames != null then sames.nn(tp2) = tp1
      res

  override protected def isSame(tp1: Type, tp2: Type)(using Context): Boolean = isSameType(tp1, tp2)

  /** Same as `isSameType` but also can be applied to overloaded TermRefs, where
   *  two overloaded refs are the same if they have pairwise equal alternatives
   */
  def isSameRef(tp1: Type, tp2: Type): Boolean = trace(s"isSameRef($tp1, $tp2") {
    def isSubRef(tp1: Type, tp2: Type): Boolean = tp1 match {
      case tp1: TermRef if tp1.isOverloaded =>
        tp1.alternatives forall (isSubRef(_, tp2))
      case _ =>
        tp2 match {
          case tp2: TermRef if tp2.isOverloaded =>
            tp2.alternatives exists (isSubRef(tp1, _))
          case _ =>
            isSubType(tp1, tp2)
        }
    }
    isSubRef(tp1, tp2) && isSubRef(tp2, tp1)
  }

  /** If the range `tp1..tp2` consist of a single type, that type, otherwise NoType`.
   *  This is the case if `tp1 =:= tp2`, but also if `tp1 <:< tp2`, `tp1` is a singleton type,
   *  and `tp2` derives from `scala.Singleton` (or vice-versa). Examples of the latter case:
   *
   *     "name".type .. Singleton
   *     "name".type .. String & Singleton
   *     Singleton .. "name".type
   *     String & Singleton .. "name".type
   *
   *  All consist of the single type `"name".type`.
   */
  def singletonInterval(tp1: Type, tp2: Type): Type = {
    def isSingletonBounds(lo: Type, hi: Type) =
      lo.isSingleton && hi.derivesFrom(defn.SingletonClass) && isSubTypeWhenFrozen(lo, hi)
    if (isSameTypeWhenFrozen(tp1, tp2)) tp1
    else if (isSingletonBounds(tp1, tp2)) tp1
    else if (isSingletonBounds(tp2, tp1)) tp2
    else NoType
  }

  /** The greatest lower bound of two types */
  def glb(tp1: Type, tp2: Type): Type = // trace(s"glb(${tp1.show}, ${tp2.show})", subtyping, show = true):
    if tp1 eq tp2 then tp1
    else if !tp1.exists || (tp1 eq WildcardType) then tp2
    else if !tp2.exists || (tp2 eq WildcardType) then tp1
    else if tp1.isAny && !tp2.isLambdaSub || tp1.isAnyKind || isBottom(tp2) then tp2
    else if tp2.isAny && !tp1.isLambdaSub || tp2.isAnyKind || isBottom(tp1) then tp1
    else
      def mergedGlb(tp1: Type, tp2: Type): Type =
        val tp1a = dropIfSuper(tp1, tp2)
        if tp1a ne tp1 then glb(tp1a, tp2)
        else
          val tp2a = dropIfSuper(tp2, tp1)
          if tp2a ne tp2 then glb(tp1, tp2a)
          else tp2 match // normalize to disjunctive normal form if possible.
            case tp2 @ OrType(tp2L, tp2R) =>
              lub(tp1 & tp2L, tp1 & tp2R, isSoft = tp2.isSoft)
            case _ =>
              tp1 match
                case tp1 @ OrType(tp1L, tp1R) =>
                  lub(tp1L & tp2, tp1R & tp2, isSoft = tp1.isSoft)
                case tp1: ConstantType =>
                  tp2 match
                    case tp2: ConstantType =>
                      // Make use of the fact that the intersection of two constant types
                      // types which are not subtypes of each other is known to be empty.
                      // Note: The same does not apply to singleton types in general.
                      // E.g. we could have a pattern match against `x.type & y.type`
                      // which might succeed if `x` and `y` happen to be the same ref
                      // at run time. It would not work to replace that with `Nothing`.
                      // However, maybe we can still apply the replacement to
                      // types which are not explicitly written.
                      NothingType
                    case _ => andType(tp1, tp2)
                case _ => andType(tp1, tp2)
      end mergedGlb

      mergedGlb(dropExpr(tp1.stripLazyRef), dropExpr(tp2.stripLazyRef))
  end glb

  def widenInUnions(using Context): Boolean =
    migrateTo3 || ctx.erasedTypes

  /** The least upper bound of two types
   *  @param canConstrain  If true, new constraints might be added to simplify the lub.
   *  @param isSoft        If the lub is a union, this determines whether it's a soft union.
   */
  def lub(tp1: Type, tp2: Type, canConstrain: Boolean = false, isSoft: Boolean = true): Type = // trace(s"lub(${tp1.show}, ${tp2.show}, canConstrain=$canConstrain, isSoft=$isSoft)", subtyping, show = true):
    if tp1 eq tp2 then tp1
    else if !tp1.exists || (tp2 eq WildcardType) then tp1
    else if !tp2.exists || (tp1 eq WildcardType) then tp2
    else if tp1.isAny && !tp2.isLambdaSub || tp1.isAnyKind || isBottom(tp2) then tp1
    else if tp2.isAny && !tp1.isLambdaSub || tp2.isAnyKind || isBottom(tp1) then tp2
    else
      def mergedLub(tp1: Type, tp2: Type): Type =
        // First, if tp1 and tp2 are the same singleton type, return one of them.
        if tp1.isSingleton && isSubType(tp1, tp2, whenFrozen = !canConstrain) then
          return tp2
        if tp2.isSingleton && isSubType(tp2, tp1, whenFrozen = !canConstrain) then
          return tp1

        // Second, handle special cases when tp1 and tp2 are disjunctions of
        // singleton types. This saves time otherwise spent in
        // costly subtype comparisons performed in dropIfSub below.
        tp1.atoms match
          case Atoms.Range(lo1, hi1) if !widenInUnions =>
            tp2.atoms match
              case Atoms.Range(lo2, hi2) =>
                if hi1.subsetOf(lo2) then return tp2
                if hi2.subsetOf(lo1) then return tp1
                if (hi1 & hi2).isEmpty then return orType(tp1, tp2, isSoft = isSoft)
              case none =>
          case none =>

        // Third, try to simplify after widening as follows:
        //  1. Drop all or-factors in tp2 that are subtypes of an or-factor
        //     in tp1, yielding tp2Final.
        //  2. Drop all or-factors in tp1 that are subtypes of an or-factor
        //     in tp2Final, yielding tp1Final.
        //  3. Combine the two final types in an OrType
        def widen(tp: Type) =
          if widenInUnions then tp.widen else tp.widenIfUnstable
        val tp1w = widen(tp1)
        val tp2w = widen(tp2)
        val tp2Final = dropIfSub(tp2w, tp1w, canConstrain)
        val tp1Final = dropIfSub(tp1w, tp2Final, canConstrain)
        recombine(tp1Final, tp2Final, orType(_, _, isSoft = isSoft))
      end mergedLub

      mergedLub(dropExpr(tp1.stripLazyRef), dropExpr(tp2.stripLazyRef))
  end lub

  /** Try to produce joint arguments for a lub `A[T_1, ..., T_n] | A[T_1', ..., T_n']` using
   *  the following strategies:
   *
   *    - if arguments are the same, that argument.
   *    - if corresponding parameter variance is co/contra-variant, the lub/glb.
   *    - otherwise a TypeBounds containing both arguments
   */
  def lubArgs(args1: List[Type], args2: List[Type], tparams: List[TypeParamInfo], canConstrain: Boolean = false): List[Type] =
    tparams match {
      case tparam :: tparamsRest =>
        val arg1 :: args1Rest = args1: @unchecked
        val arg2 :: args2Rest = args2: @unchecked
        val common = singletonInterval(arg1, arg2)
        val v = tparam.paramVarianceSign
        val lubArg =
          if (common.exists) common
          else if (v > 0) lub(arg1.hiBound, arg2.hiBound, canConstrain)
          else if (v < 0) glb(arg1.loBound, arg2.loBound)
          else TypeBounds(glb(arg1.loBound, arg2.loBound),
                          lub(arg1.hiBound, arg2.hiBound, canConstrain))
        lubArg :: lubArgs(args1Rest, args2Rest, tparamsRest, canConstrain)
      case nil =>
        Nil
    }

  /** Try to produce joint arguments for a glb `A[T_1, ..., T_n] & A[T_1', ..., T_n']` using
   *  the following strategies:
   *
   *    - if arguments are the same, that argument.
   *    - if corresponding parameter variance is co/contra-variant, the glb/lub.
   *    - if at least one of the arguments if a TypeBounds, the union of
   *      the bounds.
   *    - if homogenizeArgs is set, and arguments can be unified by instantiating
   *      type parameters, the unified argument.
   *    - otherwise NoType
   *
   *  The unification rule is contentious because it cuts the constraint set.
   *  Therefore it is subject to Config option `alignArgsInAnd`.
   */
  def glbArgs(args1: List[Type], args2: List[Type], tparams: List[TypeParamInfo]): List[Type] =
    tparams match {
      case tparam :: tparamsRest =>
        val arg1 :: args1Rest = args1: @unchecked
        val arg2 :: args2Rest = args2: @unchecked
        val common = singletonInterval(arg1, arg2)
        val v = tparam.paramVarianceSign
        val glbArg =
          if (common.exists) common
          else if (v > 0) glb(arg1.hiBound, arg2.hiBound)
          else if (v < 0) lub(arg1.loBound, arg2.loBound)
          else if (isBounds(arg1) || isBounds(arg2))
            TypeBounds(lub(arg1.loBound, arg2.loBound),
                       glb(arg1.hiBound, arg2.hiBound))
          else if (homogenizeArgs && !frozenConstraint && isSameType(arg1, arg2)) arg1
          else NoType
        glbArg :: glbArgs(args1Rest, args2Rest, tparamsRest)
      case nil =>
        Nil
    }

  private def recombine(tp1: Type, tp2: Type, rebuild: (Type, Type) => Type): Type =
    if !tp1.exists then tp2
    else if !tp2.exists then tp1
    else rebuild(tp1, tp2)

  private def recombine(tp1: Type, tp2: Type, tp: AndOrType): Type =
    recombine(tp1, tp2, tp.derivedAndOrType)

  /** If some (&-operand of) `tp` is a supertype of `sub` replace it with `NoType`.
   */
  private def dropIfSuper(tp: Type, sub: Type): Type =

    def isSuperOf(sub: Type): Boolean = sub match
      case AndType(sub1, sub2) => isSuperOf(sub1) || isSuperOf(sub2)
      case sub: TypeVar if sub.isInstantiated => isSuperOf(sub.inst)
      case _ => isSubTypeWhenFrozen(sub, tp)

    tp match
      case tp @ AndType(tp1, tp2) =>
        recombine(dropIfSuper(tp1, sub), dropIfSuper(tp2, sub), tp)
      case tp: TypeVar if tp.isInstantiated =>
        dropIfSuper(tp.inst, sub)
      case _ =>
        if isSuperOf(sub) then NoType else tp
  end dropIfSuper

  /** If some (|-operand of) `tp` is a subtype of `sup` replace it with `NoType`. */
  private def dropIfSub(tp: Type, sup: Type, canConstrain: Boolean): Type =

    def isSubOf(sup: Type): Boolean = sup match
      case OrType(sup1, sup2) => isSubOf(sup1) || isSubOf(sup2)
      case sup: TypeVar if sup.isInstantiated => isSubOf(sup.inst)
      case _ => isSubType(tp, sup, whenFrozen = !canConstrain)

    tp match
      case tp @ OrType(tp1, tp2) =>
        recombine(dropIfSub(tp1, sup, canConstrain), dropIfSub(tp2, sup, canConstrain), tp)
      case tp: TypeVar if tp.isInstantiated =>
        dropIfSub(tp.inst, sup, canConstrain)
      case _ =>
        if isSubOf(sup) then NoType else tp
  end dropIfSub

  /** There's a window of vulnerability between ElimByName and Erasure where some
   *  ExprTypes `=> T` that appear as parameters of function types are not yet converted
   *  to by-name functions `() ?=> T`. These would cause an assertion violation when
   *  used as operands of glb or lub. We fix this on the fly here. As explained in
   *  ElimByName, we can't fix it beforehand by mapping all occurrences of `=> T` to
   *  `() ?=> T` since that could lead to cycles.
   */
  private def dropExpr(tp: Type): Type = tp match
    case ExprType(rt) if (Phases.elimByNamePhase <= ctx.phase) && !ctx.erasedTypes =>
      defn.ByNameFunction(rt)
    case _ =>
      tp

  private def andTypeGen(tp1: Type, tp2: Type, op: (Type, Type) => Type,
      original: (Type, Type) => Type = _ & _, isErased: Boolean = ctx.erasedTypes): Type = trace(s"andTypeGen(${tp1.show}, ${tp2.show})", subtyping, show = true) {
    val t1 = distributeAnd(tp1, tp2)
    if (t1.exists) t1
    else {
      val t2 = distributeAnd(tp2, tp1)
      if (t2.exists) t2
      else if (isErased) erasedGlb(tp1, tp2)
      else liftIfHK(tp1, tp2, op, original, _ | _)
        // The ` | ` on variances is needed since variances are associated with bounds
        // not lambdas. Example:
        //
        //    trait A { def F[-X] }
        //    trait B { def F[+X] }
        //    object O extends A, B { ... }
        //
        // Here, `F` is treated as bivariant in `O`. That is, only bivariant implementation
        // of `F` are allowed. See neg/hk-variance2s.scala test.
    }
  }

  /** Form a normalized conjunction of two types.
   *  Note: For certain types, `&` is distributed inside the type. This holds for
   *  all types which are not value types (e.g. TypeBounds, ClassInfo,
   *  ExprType, LambdaType). Also, when forming an `&`,
   *  instantiated TypeVars are dereferenced and annotations are stripped.
   *  Finally, refined types with the same refined name are
   *  opportunistically merged.
   */
  final def andType(tp1: Type, tp2: Type, isErased: Boolean = ctx.erasedTypes): Type =
    andTypeGen(tp1, tp2, AndType.balanced(_, _), isErased = isErased)

  /** Try to simplify AndType, or return the type itself if no simplifiying opportunities exist. */
  private def trySimplify(tp: AndType): Type =
    andTypeGen(tp.tp1, tp.tp2, (_, _) => tp)

  /** Form a normalized conjunction of two types.
   *  Note: For certain types, `|` is distributed inside the type. This holds for
   *  all types which are not value types (e.g. TypeBounds, ClassInfo,
   *  ExprType, LambdaType). Also, when forming an `|`,
   *  instantiated TypeVars are dereferenced and annotations are stripped.
   *
   *  @param isSoft   If the result is a union, this determines whether it's a soft union.
   *  @param isErased Apply erasure semantics. If erased is true, instead of creating
   *                  an OrType, the lub will be computed using TypeCreator#erasedLub.
   */
  final def orType(tp1: Type, tp2: Type, isSoft: Boolean = true, isErased: Boolean = ctx.erasedTypes): Type = {
    val t1 = distributeOr(tp1, tp2, isSoft)
    if (t1.exists) t1
    else {
      val t2 = distributeOr(tp2, tp1, isSoft)
      if (t2.exists) t2
      else if (isErased) erasedLub(tp1, tp2)
      else liftIfHK(tp1, tp2, OrType.balanced(_, _, soft = isSoft), _ | _, _ & _)
    }
  }

  /** `op(tp1, tp2)` unless `tp1` and `tp2` are type-constructors.
   *  In the latter case, combine `tp1` and `tp2` under a type lambda like this:
   *
   *    [X1, ..., Xn] -> op(tp1[X1, ..., Xn], tp2[X1, ..., Xn])
   */
  def liftIfHK(tp1: Type, tp2: Type,
      op: (Type, Type) => Type, original: (Type, Type) => Type, combineVariance: (Variance, Variance) => Variance) = {
    val tparams1 = tp1.typeParams
    val tparams2 = tp2.typeParams
    def applied(tp: Type) = tp.appliedTo(tp.typeParams.map(_.paramInfoAsSeenFrom(tp)))
    if (tparams1.isEmpty)
      if (tparams2.isEmpty) op(tp1, tp2)
      else original(tp1, applied(tp2))
    else if (tparams2.isEmpty)
      original(applied(tp1), tp2)
    else if (tparams1.hasSameLengthAs(tparams2))
      HKTypeLambda(
        paramNames = HKTypeLambda.syntheticParamNames(tparams1.length),
        variances =
          if tp1.isDeclaredVarianceLambda && tp2.isDeclaredVarianceLambda then
            tparams1.lazyZip(tparams2).map((p1, p2) => combineVariance(p1.paramVariance, p2.paramVariance))
          else Nil
      )(
        paramInfosExp = tl => tparams1.lazyZip(tparams2).map((tparam1, tparam2) =>
          tl.integrate(tparams1, tparam1.paramInfoAsSeenFrom(tp1)).bounds &
          tl.integrate(tparams2, tparam2.paramInfoAsSeenFrom(tp2)).bounds),
        resultTypeExp = tl =>
          original(tp1.appliedTo(tl.paramRefs), tp2.appliedTo(tl.paramRefs)))
    else original(applied(tp1), applied(tp2))
  }

  /** Try to distribute `&` inside type, detect and handle conflicts
   *  @pre !(tp1 <: tp2) && !(tp2 <:< tp1) -- these cases were handled before
   */
  private def distributeAnd(tp1: Type, tp2: Type): Type = tp1 match {
    case tp1 @ AppliedType(tycon1, args1) =>
      tp2 match {
        case AppliedType(tycon2, args2)
        if tycon1.typeSymbol == tycon2.typeSymbol && tycon1 =:= tycon2 =>
          val jointArgs = glbArgs(args1, args2, tycon1.typeParams)
          if (jointArgs.forall(_.exists)) (tycon1 & tycon2).appliedTo(jointArgs)
          else NoType
        case _ =>
          NoType
      }
    case tp1: RefinedType =>
      // opportunistically merge same-named refinements
      // this does not change anything semantically (i.e. merging or not merging
      // gives =:= types), but it keeps the type smaller.
      tp2 match {
        case tp2: RefinedType if tp1.refinedName == tp2.refinedName =>
          val jointInfo = Denotations.infoMeet(tp1.refinedInfo, tp2.refinedInfo, safeIntersection = false)
          if jointInfo.exists then
            tp1.derivedRefinedType(tp1.parent & tp2.parent, tp1.refinedName, jointInfo)
          else
            NoType
        case _ =>
          NoType
      }
    case tp1: RecType =>
      tp1.rebind(distributeAnd(tp1.parent, tp2))
    case ExprType(rt1) =>
      tp2 match {
        case ExprType(rt2) =>
          ExprType(rt1 & rt2)
        case _ =>
          NoType
      }
    case tp1: TypeVar if tp1.isInstantiated =>
      tp1.underlying & tp2
    case CapturingType(parent1, refs1) =>
      val refs2 = tp2.captureSet
      if subCaptures(refs2, refs1, frozen = true).isOK
        && tp1.isBoxedCapturing == tp2.isBoxedCapturing
      then (parent1 & tp2).capturing(refs2)
      else tp1.derivedCapturingType(parent1 & tp2, refs1)
    case tp1: AnnotatedType if !tp1.isRefining =>
      tp1.underlying & tp2
    case _ =>
      NoType
  }

  /** Try to distribute `|` inside type, detect and handle conflicts
   *  Note that, unlike for `&`, a disjunction cannot be pushed into
   *  a refined or applied type. Example:
   *
   *     List[T] | List[U] is not the same as List[T | U].
   *
   *  The rhs is a proper supertype of the lhs.
   */
  private def distributeOr(tp1: Type, tp2: Type, isSoft: Boolean = true): Type = tp1 match {
    case ExprType(rt1) =>
      tp2 match {
        case ExprType(rt2) =>
          ExprType(lub(rt1, rt2, isSoft = isSoft))
        case _ =>
          NoType
      }
    case tp1: TypeVar if tp1.isInstantiated =>
      lub(tp1.underlying, tp2, isSoft = isSoft)
    case tp1: AnnotatedType if !tp1.isRefining =>
      lub(tp1.underlying, tp2, isSoft = isSoft)
    case _ =>
      NoType
  }

  /** A comparison function to pick a winner in case of a merge conflict */
  private def isAsGood(tp1: Type, tp2: Type): Boolean = tp1 match {
    case tp1: ClassInfo =>
      tp2 match {
        case tp2: ClassInfo =>
          isSubTypeWhenFrozen(tp1.prefix, tp2.prefix) || (tp1.cls.owner derivesFrom tp2.cls.owner)
        case _ =>
          false
      }
    case tp1: PolyType =>
      tp2 match {
        case tp2: PolyType =>
          tp1.typeParams.length == tp2.typeParams.length &&
          isAsGood(tp1.resultType, tp2.resultType.subst(tp2, tp1))
        case _ =>
          false
      }
    case tp1: MethodType =>
      tp2 match {
        case tp2: MethodType =>
          def asGoodParams(formals1: List[Type], formals2: List[Type]) =
            (formals2 corresponds formals1)(isSubTypeWhenFrozen)
          asGoodParams(tp1.paramInfos, tp2.paramInfos) &&
          (!asGoodParams(tp2.paramInfos, tp1.paramInfos) ||
           isAsGood(tp1.resultType, tp2.resultType))
        case _ =>
          false
      }
    case _ =>
      false
  }

  protected def subCaptures(refs1: CaptureSet, refs2: CaptureSet, frozen: Boolean)(using Context): CaptureSet.CompareResult =
    refs1.subCaptures(refs2, frozen)

  /** Is the boxing status of tp1 and tp2 the same, or alternatively, is
   *  the capture sets `refs1` of `tp1` a subcapture of the empty set?
   *  In the latter case, boxing status does not matter.
   */
  protected def sameBoxed(tp1: Type, tp2: Type, refs1: CaptureSet)(using Context): Boolean =
    (tp1.isBoxedCapturing == tp2.isBoxedCapturing)
    || refs1.subCaptures(CaptureSet.empty, frozenConstraint).isOK

  // ----------- Diagnostics --------------------------------------------------

  /** A hook for showing subtype traces. Overridden in ExplainingTypeComparer */
  def traceIndented[T](str: String)(op: => T): T = op

  private def traceInfo(tp1: Type, tp2: Type) =
    s"${tp1.show} <:< ${tp2.show}" + {
      if (ctx.settings.verbose.value || Config.verboseExplainSubtype)
        s" ${tp1.getClass}, ${tp2.getClass}" +
        (if (frozenConstraint) " frozen" else "") +
        (if (ctx.mode is Mode.TypevarsMissContext) " tvars-miss-ctx" else "")
      else ""
    }

  /** Show subtype goal that led to an assertion failure */
  def showGoal(tp1: Type, tp2: Type)(using Context): Unit =
    try
      report.echo(i"assertion failure for ${show(tp1)} <:< ${show(tp2)}, frozen = $frozenConstraint")
      def explainPoly(tp: Type) = tp match {
        case tp: TypeParamRef => report.echo(s"TypeParamRef ${tp.show} found in ${tp.binder.show}")
        case tp: TypeRef if tp.symbol.exists => report.echo(s"typeref ${tp.show} found in ${tp.symbol.owner.show}")
        case tp: TypeVar => report.echo(s"typevar ${tp.show}, origin = ${tp.origin}")
        case _ => report.echo(s"${tp.show} is a ${tp.getClass}")
      }
      if (Config.verboseExplainSubtype) {
        explainPoly(tp1)
        explainPoly(tp2)
      }
    catch case NonFatal(ex) =>
      report.echo(s"assertion failure [[cannot display since $ex was thrown]]")

  /** Record statistics about the total number of subtype checks
   *  and the number of "successful" subtype checks, i.e. checks
   *  that form part of a subtype derivation tree that's ultimately successful.
   */
  def recordStatistics(result: Boolean, prevSuccessCount: Int): Unit = {
    // Stats.record(s"isSubType ${tp1.show} <:< ${tp2.show}")
    totalCount += 1
    if (result) successCount += 1 else successCount = prevSuccessCount
    if (recCount == 0) {
      Stats.record("successful subType", successCount)
      Stats.record("total subType", totalCount)
      successCount = 0
      totalCount = 0
    }
  }

  /** Does `tycon` have a field with type `tparam`? Special cased for `scala.*:`
   *  as that type is artificially added to tuples. */
  private def typeparamCorrespondsToField(tycon: Type, tparam: TypeParamInfo): Boolean =
    productSelectorTypes(tycon, NoSourcePosition).exists {
      case tp: TypeRef =>
        tp.designator.eq(tparam) // Bingo!
      case _ =>
        false
    } || tycon.derivesFrom(defn.PairClass)

  /** Are `tp1` and `tp2` provablyDisjoint types, i.e., is `tp1 ⋔ tp2` true?
   *
   *  Proofs rely on the following properties of Scala types:
   *
   *  1. Single inheritance of classes
   *  2. Final classes cannot be extended
   *  3. ConstantTypes with distinct values are non intersecting
   *  4. TermRefs with distinct values are non intersecting
   *  5. There is no value of type Nothing
   *
   *  Note on soundness: the correctness of match types relies on on the
   *  property that in all possible contexts, the same match type expression
   *  is either stuck or reduces to the same case.
   *
   *  This method must adhere to the specification of disjointness in SIP-56:
   *  https://docs.scala-lang.org/sips/match-types-spec.html#disjointness
   *
   *  The pattern matcher reachability test uses it for its own purposes, so we
   *  generalize it to *also* handle type variables and their GADT bounds.
   *  This is fine because match type reduction always operates under frozen
   *  GADT constraints.
   *
   *  Other than that generalization, `provablyDisjoint` must not depart from
   *  the specified "provably disjoint" relation. In particular, it is not
   *  allowed to reply `false` instead of "I don't know". It must say `true`
   *  iff the spec says `true` and must say `false` iff the spec says `false`.
   */
  def provablyDisjoint(tp1: Type, tp2: Type)(using Context): Boolean =
    provablyDisjoint(tp1, tp2, null)

  private def provablyDisjoint(tp1: Type, tp2: Type, pending: util.HashSet[(Type, Type)] | Null)(
      using Context): Boolean = trace(i"provable disjoint $tp1, $tp2", matchTypes) {
    // println(s"provablyDisjoint(${tp1.show}, ${tp2.show})")

    // Computes ⌈tp⌉ (see the spec), generalized to handle GADT bounds
    @scala.annotation.tailrec
    def disjointnessBoundary(tp: Type): Type = tp match
      case tp: TypeRef =>
        tp.symbol match
          case cls: ClassSymbol =>
            if cls == defn.SingletonClass then defn.AnyType
            else if cls.typeParams.nonEmpty then tp.etaExpand
            else tp
          case sym =>
            if !ctx.erasedTypes && sym == defn.FromJavaObjectSymbol then defn.AnyType
            else
              val optGadtBounds = gadtBounds(sym)
              if optGadtBounds != null then disjointnessBoundary(optGadtBounds.hi)
              else disjointnessBoundary(tp.superTypeNormalized)
      case tp @ AppliedType(tycon: TypeRef, targs) if tycon.symbol.isClass =>
        /* The theory says we should just return `tp` here. However, due to how
         * baseType works (called from `isBaseTypeWithDisjointArguments`),
         * it can create infinitely growing towers of `AnnotatedType`s. This
         * defeats the infinite recursion detection with the `pending` set.
         * Therefore, we eagerly remove all non-refining annotations. We are
         * allowed to do that because they don't affect subtyping (so cannot
         * create an ill-kinded `AppliedType`) and would anyway be stripped
         * later on by the recursive calls to `provablyDisjoint`, though
         * `disjointnessBoundary`).
         * See tests/pos/provably-disjoint-infinite-recursion-1.scala for an example.
         */
        tp.derivedAppliedType(
          tycon,
          targs.mapConserve(_.stripAnnots(keep = _.symbol.derivesFrom(defn.RefiningAnnotationClass)))
        )
      case tp: TermRef =>
        val isEnumValue = tp.termSymbol.isAllOf(EnumCase, butNot = JavaDefined)
        if isEnumValue then tp
        else
          val optGadtBounds = gadtBounds(tp.symbol)
          if optGadtBounds != null then disjointnessBoundary(optGadtBounds.hi)
          else disjointnessBoundary(tp.superTypeNormalized)
      case tp: AndOrType =>
        tp
      case tp: ConstantType =>
        tp
      case tp: HKTypeLambda =>
        tp
      case tp: ParamRef =>
        val st = tp.superTypeNormalized
        if st.exists then
          disjointnessBoundary(st)
        else
          // workaround for when ParamRef#underlying returns NoType
          defn.AnyType
      case tp: TypeProxy =>
        disjointnessBoundary(tp.superTypeNormalized)
      case tp: WildcardType =>
        disjointnessBoundary(tp.effectiveBounds.hi)
      case tp: ErrorType =>
        defn.AnyType
    end disjointnessBoundary

    (disjointnessBoundary(tp1), disjointnessBoundary(tp2)) match
      // Infinite recursion detection
      case pair if pending != null && pending.contains(pair) =>
        false

      // Cases where there is an intersection or union on the right
      case (tp1, tp2: OrType) =>
        provablyDisjoint(tp1, tp2.tp1, pending) && provablyDisjoint(tp1, tp2.tp2, pending)
      case (tp1, tp2: AndType) =>
        provablyDisjoint(tp1, tp2.tp1, pending) || provablyDisjoint(tp1, tp2.tp2, pending)

      // Cases where there is an intersection or union on the left but not on the right
      case (tp1: OrType, tp2) =>
        provablyDisjoint(tp1.tp1, tp2, pending) && provablyDisjoint(tp1.tp2, tp2, pending)
      case (tp1: AndType, tp2) =>
        provablyDisjoint(tp1.tp1, tp2, pending) || provablyDisjoint(tp1.tp2, tp2, pending)

      // Cases involving type lambdas
      case (tp1: HKTypeLambda, tp2: HKTypeLambda) =>
        tp1.paramNames.sizeCompare(tp2.paramNames) != 0
          || provablyDisjoint(tp1.resultType, tp2.resultType, pending)
      case (tp1: HKTypeLambda, tp2) =>
        !tp2.isDirectRef(defn.AnyKindClass)
      case (tp1, tp2: HKTypeLambda) =>
        !tp1.isDirectRef(defn.AnyKindClass)

      /* Cases where both are unique values (enum cases or constant types)
       *
       * When both are TermRef's, we look at the symbols. We do not try to
       * prove disjointness based on the prefixes.
       *
       * Otherwise, we know everything there is to know about them our two types.
       * Therefore, a direct subtype test is enough to decide disjointness.
       */
      case (tp1: TermRef, tp2: TermRef) =>
        tp1.symbol != tp2.symbol
      case (tp1: ConstantType, tp2: ConstantType) =>
        tp1.value != tp2.value
      case (tp1: SingletonType, tp2: SingletonType) =>
        true // a TermRef and a ConstantType, in either direction

      /* Cases where one is a unique value and the other a possibly-parameterized
       * class type. Again, we do not look at prefixes, so we test whether the
       * unique value derives from the class.
       */
      case (tp1: SingletonType, tp2) =>
        !tp1.derivesFrom(tp2.classSymbol)
      case (tp1, tp2: SingletonType) =>
        !tp2.derivesFrom(tp1.classSymbol)

      /* Now both sides are possibly-parameterized class types `p.C[Ts]` and `q.D[Us]`.
       *
       * First, we try to show that C and D are entirely disjoint, independently
       * of the type arguments, based on their `final` status and `class` status.
       *
       * Otherwise, we look at all the common baseClasses of tp1 and tp2, and
       * try to find one common base class `E` such that `baseType(tp1, E)` and
       * `baseType(tp2, E)` can be proven disjoint based on the type arguments.
       *
       * Regardless, we do not look at prefixes.
       */
      case tpPair @ (tp1, tp2) =>
        val cls1 = tp1.classSymbol.asClass
        val cls2 = tp2.classSymbol.asClass

        def isBaseTypeWithDisjointArguments(baseClass: ClassSymbol, pending: util.HashSet[(Type, Type)]): Boolean =
          if baseClass.typeParams.isEmpty then
            // A common mono base class can never be disjoint thanks to type params
            false
          else
            (tp1.baseType(baseClass), tp2.baseType(baseClass)) match
              case (AppliedType(tycon1, args1), AppliedType(tycon2, args2)) =>
                provablyDisjointTypeArgs(baseClass, args1, args2, pending)
              case _ =>
                false
        end isBaseTypeWithDisjointArguments

        def typeArgsMatch(tp: Type, cls: ClassSymbol): Boolean =
          val typeArgs = tp match
            case tp: TypeRef          => Nil
            case AppliedType(_, args) => args
          cls.typeParams.sizeCompare(typeArgs) == 0

        def existsCommonBaseTypeWithDisjointArguments: Boolean =
          if !typeArgsMatch(tp1, cls1) || !typeArgsMatch(tp2, cls2) then
            /* We have an unapplied polymorphic class type or otherwise not star-kinded one.
             * This does not happen with match types, but happens when coming from the Space engine.
             * In that case, we cannot prove disjointness based on type arguments.
             */
            false
          else
            /* We search among the common base classes of `cls1` and `cls2`.
             * We exclude any base class that is an ancestor of one of the other base classes:
             * they are useless, since anything discovered at their level would also be discovered at
             * the level of the descendant common base class.
             */
            val innerPending =
              if pending != null then pending
              else util.HashSet[(Type, Type)]()
            innerPending += tpPair

            val cls2BaseClassSet = SymDenotations.BaseClassSet(cls2.classDenot.baseClasses)
            val commonBaseClasses = cls1.classDenot.baseClasses.filter(cls2BaseClassSet.contains(_))
            def isAncestorOfOtherBaseClass(cls: ClassSymbol): Boolean =
              commonBaseClasses.exists(other => (other ne cls) && other.derivesFrom(cls))
            val result = commonBaseClasses.exists { baseClass =>
              !isAncestorOfOtherBaseClass(baseClass) && isBaseTypeWithDisjointArguments(baseClass, innerPending)
            }

            innerPending -= tpPair
            result
        end existsCommonBaseTypeWithDisjointArguments

        provablyDisjointClasses(cls1, cls2)
          || existsCommonBaseTypeWithDisjointArguments
    end match
  }

  private def provablyDisjointClasses(cls1: Symbol, cls2: Symbol)(using Context): Boolean =
    def isDecomposable(cls: Symbol): Boolean =
      cls.is(Sealed) && !cls.hasAnonymousChild

    def decompose(cls: Symbol): List[Symbol] =
      cls.children.map { child =>
        if child.isTerm then child.info.classSymbol
        else child
      }.filter(child => child.exists && child != cls)

    // TODO? Special-case for Nothing and Null? We probably need Nothing/Null disjoint from Nothing/Null
    def eitherDerivesFromOther(cls1: Symbol, cls2: Symbol): Boolean =
      cls1.derivesFrom(cls2) || cls2.derivesFrom(cls1)

    def smallestNonTraitBase(cls: Symbol): Symbol =
      cls.asClass.baseClasses.find(!_.is(Trait)).get

    if cls1 == defn.AnyKindClass || cls2 == defn.AnyKindClass then
      // For some reason, A.derivesFrom(AnyKind) returns false, so we have to handle it specially
      false
    else if (eitherDerivesFromOther(cls1, cls2))
      false
    else
      if (cls1.is(Final) || cls2.is(Final))
        // One of these types is final and they are not mutually
        // subtype, so they must be unrelated.
        true
      else if (!eitherDerivesFromOther(smallestNonTraitBase(cls1), smallestNonTraitBase(cls2))) then
        // The traits extend a pair of non-trait classes that are not mutually subtypes,
        // so they must be unrelated by single inheritance of classes.
        true
      else if (isDecomposable(cls1))
        // At this point, !cls1.derivesFrom(cls2): we know that direct
        // instantiations of `cls1` (terms of the form `new cls1`) are not
        // of type `tp2`. Therefore, we can safely decompose `cls1` using
        // `.children`, even if `cls1` is non abstract.
        decompose(cls1).forall(x => provablyDisjointClasses(x, cls2))
      else if (isDecomposable(cls2))
        decompose(cls2).forall(x => provablyDisjointClasses(cls1, x))
      else
        false
  end provablyDisjointClasses

  private def provablyDisjointTypeArgs(cls: ClassSymbol, args1: List[Type], args2: List[Type], pending: util.HashSet[(Type, Type)])(using Context): Boolean =
    // It is possible to conclude that two types applied are disjoint by
    // looking at covariant type parameters if the said type parameters
    // are disjoint and correspond to fields.
    // (Type parameter disjointness is not enough by itself as it could
    // lead to incorrect conclusions for phantom type parameters).
    def covariantDisjoint(tp1: Type, tp2: Type, tparam: TypeParamInfo): Boolean =
      provablyDisjoint(tp1, tp2, pending) && typeparamCorrespondsToField(cls.appliedRef, tparam)

    // In the invariant case, direct type parameter disjointness is enough.
    def invariantDisjoint(tp1: Type, tp2: Type, tparam: TypeParamInfo): Boolean =
      provablyDisjoint(tp1, tp2, pending)

    args1.lazyZip(args2).lazyZip(cls.typeParams).exists {
      (arg1, arg2, tparam) =>
        val v = tparam.paramVarianceSign
        if (v > 0)
          covariantDisjoint(arg1, arg2, tparam)
        else if (v < 0)
          // Contravariant case: a value where this type parameter is
          // instantiated to `Any` belongs to both types.
          false
        else
          invariantDisjoint(arg1, arg2, tparam)
    }
  end provablyDisjointTypeArgs

  protected def explainingTypeComparer(short: Boolean) = ExplainingTypeComparer(comparerContext, short)
  protected def matchReducer = MatchReducer(comparerContext)

  private def inSubComparer[T, Cmp <: TypeComparer](comparer: Cmp)(op: Cmp => T): T =
    val saved = myInstance
    myInstance = comparer
    try op(comparer)
    finally myInstance = saved

  /** The trace of comparison operations when performing `op` */
  def explained[T](op: ExplainingTypeComparer => T, header: String = "Subtype trace:", short: Boolean)(using Context): String =
    val cmp = explainingTypeComparer(short)
    inSubComparer(cmp)(op)
    cmp.lastTrace(header)

  def reduceMatchWith[T](op: MatchReducer => T)(using Context): T =
    inSubComparer(matchReducer)(op)
}

object TypeComparer {

  enum CompareResult:
    case OK, Fail, OKwithGADTUsed, OKwithOpaquesUsed

  /** Class for unification variables used in `natValue`. */
  private class AnyConstantType extends UncachedGroundType with ValueType {
    var tpe: Type = NoType
  }

  private[core] def show(res: Any)(using Context): String =
    if ctx.settings.YexplainLowlevel.value then String.valueOf(res).nn
    else res match
      case ClassInfo(_, cls, _, _, _) => cls.showLocated
      case bounds: TypeBounds => i"type bounds [$bounds]"
      case CaptureSet.CompareResult.OK => "OK"
      case res: printing.Showable => res.show
      case _ => String.valueOf(res).nn

  /** The approximation state indicates how the pair of types currently compared
   *  relates to the types compared originally.
   *   - `None`    : They are still the same types
   *   - `LoApprox`: The left type is approximated (i.e widened)"
   *   - `HiApprox`: The right type is approximated (i.e narrowed)"
   */
  object ApproxState:
    opaque type Repr = Int

    val None: Repr = 0
    private val LoApprox = 1
    private val HiApprox = 2

    /** A special approximation state to indicate that this is the first time we
     *  compare (approximations of) this pair of types. It's converted to `None`
     *  in `isSubType`, but also leads to `leftRoot` being set there.
     */
    val Fresh: Repr = 4

    object Repr:
      extension (approx: Repr)
        def low: Boolean = (approx & LoApprox) != 0
        def high: Boolean = (approx & HiApprox) != 0
        def addLow: Repr = approx | LoApprox
        def addHigh: Repr = approx | HiApprox
        def show: String =
          val lo = if low then " (left is approximated)" else ""
          val hi = if high then " (right is approximated)" else ""
          lo ++ hi
  end ApproxState
  type ApproxState = ApproxState.Repr

  /** Result of `isCovered` check. */
  private object CoveredStatus:
    type Repr = Int

    val Uncovered:     Repr = 1   // The type is not covered
    val CoveredWithOr: Repr = 2   // The type is covered and contains OrTypes
    val Covered:       Repr = 3   // The type is covered and free from OrTypes
  end CoveredStatus
  type CoveredStatus = CoveredStatus.Repr

  def topLevelSubType(tp1: Type, tp2: Type)(using Context): Boolean =
    comparing(_.topLevelSubType(tp1, tp2))

  def necessarySubType(tp1: Type, tp2: Type)(using Context): Boolean =
    comparing(_.necessarySubType(tp1, tp2))

  def isSubType(tp1: Type, tp2: Type)(using Context): Boolean =
    comparing(_.isSubType(tp1, tp2))

  def isSameType(tp1: Type, tp2: Type)(using Context): Boolean =
    comparing(_.isSameType(tp1, tp2))

  def isSubTypeWhenFrozen(tp1: Type, tp2: Type)(using Context): Boolean =
    comparing(_.isSubTypeWhenFrozen(tp1, tp2))

  def testSubType(tp1: Type, tp2: Type)(using Context): CompareResult =
    comparing(_.testSubType(tp1, tp2))

  def isSameTypeWhenFrozen(tp1: Type, tp2: Type)(using Context): Boolean =
    comparing(_.isSameTypeWhenFrozen(tp1, tp2))

  def isSameRef(tp1: Type, tp2: Type)(using Context): Boolean =
    comparing(_.isSameRef(tp1, tp2))

  def matchesType(tp1: Type, tp2: Type, relaxed: Boolean)(using Context): Boolean =
    comparing(_.matchesType(tp1, tp2, relaxed))

  def hasMatchingMember(name: Name, tp1: Type, tp2: RefinedType)(using Context): Boolean =
    comparing(_.hasMatchingMember(name, tp1, tp2))

  def matchingMethodParams(tp1: MethodType, tp2: MethodType)(using Context): Boolean =
    comparing(_.matchingMethodParams(tp1, tp2))

  def lub(tp1: Type, tp2: Type, canConstrain: Boolean = false, isSoft: Boolean = true)(using Context): Type =
    comparing(_.lub(tp1, tp2, canConstrain = canConstrain, isSoft = isSoft))

  /** The least upper bound of a list of types */
  final def lub(tps: List[Type])(using Context): Type =
    tps.foldLeft(defn.NothingType: Type)(lub(_,_))

  def lubArgs(args1: List[Type], args2: List[Type], tparams: List[TypeParamInfo], canConstrain: Boolean = false)(using Context): List[Type] =
    comparing(_.lubArgs(args1, args2, tparams, canConstrain))

  def glb(tp1: Type, tp2: Type)(using Context): Type =
    comparing(_.glb(tp1, tp2))

  /** The greatest lower bound of a list types */
  def glb(tps: List[Type])(using Context): Type =
    tps.foldLeft(defn.AnyType: Type)(glb)

  def orType(using Context)(tp1: Type, tp2: Type, isSoft: Boolean = true, isErased: Boolean = ctx.erasedTypes): Type =
    comparing(_.orType(tp1, tp2, isSoft = isSoft, isErased = isErased))

  def andType(using Context)(tp1: Type, tp2: Type, isErased: Boolean = ctx.erasedTypes): Type =
    comparing(_.andType(tp1, tp2, isErased))

  def provablyDisjoint(tp1: Type, tp2: Type)(using Context): Boolean =
    comparing(_.provablyDisjoint(tp1, tp2))

  def liftIfHK(tp1: Type, tp2: Type,
      op: (Type, Type) => Type, original: (Type, Type) => Type,
      combineVariance: (Variance, Variance) => Variance)(using Context): Type =
    comparing(_.liftIfHK(tp1, tp2, op, original, combineVariance))

  def constValue(tp: Type)(using Context): Option[Constant] =
    comparing(_.constValue(tp))

  def subtypeCheckInProgress(using Context): Boolean =
    comparing(_.subtypeCheckInProgress)

  def instanceType(param: TypeParamRef, fromBelow: Boolean, widenUnions: Boolean, maxLevel: Int = Int.MaxValue)(using Context): Type =
    comparing(_.instanceType(param, fromBelow, widenUnions, maxLevel))

  def approximation(param: TypeParamRef, fromBelow: Boolean, maxLevel: Int = Int.MaxValue)(using Context): Type =
    comparing(_.approximation(param, fromBelow, maxLevel))

  def bounds(param: TypeParamRef)(using Context): TypeBounds =
    comparing(_.bounds(param))

  def fullBounds(param: TypeParamRef)(using Context): TypeBounds =
    comparing(_.fullBounds(param))

  def fullLowerBound(param: TypeParamRef)(using Context): Type =
    comparing(_.fullLowerBound(param))

  def fullUpperBound(param: TypeParamRef)(using Context): Type =
    comparing(_.fullUpperBound(param))

  def addToConstraint(tl: TypeLambda, tvars: List[TypeVar])(using Context): Boolean =
    comparing(_.addToConstraint(tl, tvars))

  def widenInferred(inst: Type, bound: Type, widenUnions: Boolean)(using Context): Type =
    comparing(_.widenInferred(inst, bound, widenUnions))

  def dropTransparentTraits(tp: Type, bound: Type)(using Context): Type =
    comparing(_.dropTransparentTraits(tp, bound))

  def constrainPatternType(pat: Type, scrut: Type, forceInvariantRefinement: Boolean = false)(using Context): Boolean =
    comparing(_.constrainPatternType(pat, scrut, forceInvariantRefinement))

  def explained[T](op: ExplainingTypeComparer => T, header: String = "Subtype trace:", short: Boolean = false)(using Context): String =
    comparing(_.explained(op, header, short))

  def reduceMatchWith[T](op: MatchReducer => T)(using Context): T =
    comparing(_.reduceMatchWith(op))

  def subCaptures(refs1: CaptureSet, refs2: CaptureSet, frozen: Boolean)(using Context): CaptureSet.CompareResult =
    comparing(_.subCaptures(refs1, refs2, frozen))
}

object MatchReducer:
  import printing.*, Texts.*
  enum MatchResult extends Showable:
    case Reduced(tp: Type)
    case Disjoint
    case ReducedAndDisjoint
    case Stuck
    case NoInstance(fails: List[(Name, TypeBounds)])

    def toText(p: Printer): Text = this match
      case Reduced(tp)        => "Reduced(" ~ p.toText(tp) ~ ")"
      case Disjoint           => "Disjoint"
      case ReducedAndDisjoint => "ReducedAndDisjoint"
      case Stuck              => "Stuck"
      case NoInstance(fails)  => "NoInstance(" ~ Text(fails.map(p.toText(_) ~ p.toText(_)), ", ") ~ ")"

/** A type comparer for reducing match types.
 *  TODO: Not sure this needs to be a type comparer. Can we make it a
 *  separate class?
 */
class MatchReducer(initctx: Context) extends TypeComparer(initctx) {
  import MatchReducer.*

  init(initctx)

  override def matchReducer = this

  def matchCases(scrut: Type, cases: List[MatchTypeCaseSpec])(using Context): Type = {
    // a reference for the type parameters poisoned during matching
    // for use during the reduction step
    var poisoned: Set[TypeParamRef] = Set.empty

    def paramInstances(canApprox: Boolean) = new TypeAccumulator[Array[Type]]:
      def apply(insts: Array[Type], t: Type) = t match
        case param @ TypeParamRef(b, n) if b eq caseLambda =>
          insts(n) =
            if canApprox then
              approximation(param, fromBelow = variance >= 0, Int.MaxValue).simplified
            else constraint.entry(param) match
              case entry: TypeBounds =>
                val lo = fullLowerBound(param)
                val hi = fullUpperBound(param)
                if !poisoned(param) && isSubType(hi, lo) then lo.simplified else Range(lo, hi)
              case inst =>
                assert(inst.exists, i"param = $param\nconstraint = $constraint")
                if !poisoned(param) then inst.simplified else Range(inst, inst)
          insts
        case _ =>
          foldOver(insts, t)

    def instantiateParams(insts: Array[Type]) = new ApproximatingTypeMap {
      variance = 0

      override def range(lo: Type, hi: Type): Type =
        if variance == 0 && (lo eq hi) then
          // override the default `lo eq hi` test, which removes the Range
          // which leads to a Reduced result, instead of NoInstance
          Range(lower(lo), upper(hi))
        else super.range(lo, hi)

      def apply(t: Type) = t match {
        case t @ TypeParamRef(b, n) if b `eq` caseLambda => insts(n)
        case t: LazyRef => apply(t.ref)
        case _ => mapOver(t)
      }
    }

    def instantiateParamsSpec(insts: Array[Type], caseLambda: HKTypeLambda) = new TypeMap {
      variance = 0

      def apply(t: Type) = t match {
        case t @ TypeParamRef(b, n) if b `eq` caseLambda => insts(n)
        case t: LazyRef => apply(t.ref)
        case _ => mapOver(t)
      }
    }

    /** Match a single case. */
    def matchCase(cas: MatchTypeCaseSpec): MatchResult = trace(i"$scrut match ${MatchTypeTrace.caseText(cas)}", matchTypes, show = true) {
      cas match
        case cas: MatchTypeCaseSpec.SubTypeTest     => matchSubTypeTest(cas)
        case cas: MatchTypeCaseSpec.SpeccedPatMat   => matchSpeccedPatMat(cas)
        case cas: MatchTypeCaseSpec.LegacyPatMat    => matchLegacyPatMat(cas)
        case cas: MatchTypeCaseSpec.MissingCaptures => matchMissingCaptures(cas)
    }

    def matchSubTypeTest(spec: MatchTypeCaseSpec.SubTypeTest): MatchResult =
      val disjoint = provablyDisjoint(scrut, spec.pattern)
      if necessarySubType(scrut, spec.pattern) then
        if disjoint then
          MatchResult.ReducedAndDisjoint
        else
          MatchResult.Reduced(spec.body)
      else if disjoint then
        MatchResult.Disjoint
      else
        MatchResult.Stuck
    end matchSubTypeTest

    // See https://docs.scala-lang.org/sips/match-types-spec.html#matching
    def matchSpeccedPatMat(spec: MatchTypeCaseSpec.SpeccedPatMat): MatchResult =
      /* Concreteness checking
       *
       * When following a baseType and reaching a non-wildcard, in-variant-pos type capture,
       * we have to make sure that the scrutinee is concrete enough to uniquely determine
       * the values of the captures. This comes down to checking that we do not follow any
       * upper bound of an abstract type.
       *
       * See notably neg/wildcard-match.scala for examples of this.
       *
       * See neg/i13780.scala and neg/i13780-1.scala for ClassCastException
       * reproducers if we disable this check.
       */

      def followEverythingConcrete(tp: Type): Type =
        val widenedTp = tp.widenDealias
        val tp1 = widenedTp.normalized

        def followTp1: Type =
          // If both widenDealias and normalized did something, start again
          if (tp1 ne widenedTp) && (widenedTp ne tp) then followEverythingConcrete(tp1)
          else tp1

        tp1 match
          case tp1: TypeRef =>
            tp1.info match
              case TypeAlias(tl: HKTypeLambda)  => tl
              case MatchAlias(tl: HKTypeLambda) => tl
              case _                            => followTp1
          case tp1 @ AppliedType(tycon, args) =>
            val concreteTycon = followEverythingConcrete(tycon)
            if concreteTycon eq tycon then followTp1
            else followEverythingConcrete(concreteTycon.applyIfParameterized(args))
          case _ =>
            followTp1
      end followEverythingConcrete

      def isConcrete(tp: Type): Boolean =
        followEverythingConcrete(tp) match
          case tp1: AndOrType => isConcrete(tp1.tp1) && isConcrete(tp1.tp2)
          case tp1            => tp1.underlyingClassRef(refinementOK = true).exists

      // Actual matching logic

      val instances = Array.fill[Type](spec.captureCount)(NoType)
      val noInstances = mutable.ListBuffer.empty[(TypeName, TypeBounds)]

      def rec(pattern: MatchTypeCasePattern, scrut: Type, variance: Int, scrutIsWidenedAbstract: Boolean): Boolean =
        pattern match
          case MatchTypeCasePattern.Capture(num, /* isWildcard = */ true) =>
            // instantiate the wildcard in a way that the subtype test always succeeds
            instances(num) = variance match
              case 1  => scrut.hiBound // actually important if we are not in a class type constructor
              case -1 => scrut.loBound
              case 0  => scrut
            !instances(num).isError

          case MatchTypeCasePattern.Capture(num, /* isWildcard = */ false) =>
            def failNotSpecific(bounds: TypeBounds): TypeBounds =
              noInstances += spec.origMatchCase.paramNames(num) -> bounds
              bounds

            instances(num) = scrut match
              case scrut: TypeBounds =>
                if scrutIsWidenedAbstract then
                  failNotSpecific(scrut)
                else
                  variance match
                    case 1  => scrut.hi
                    case -1 => scrut.lo
                    case 0  => failNotSpecific(scrut)
              case _ =>
                if scrutIsWidenedAbstract && variance != 0 then
                  // fail as not specific
                  // the Nothing and Any bounds are used so that they are not displayed; not for themselves in particular
                  if variance > 0 then failNotSpecific(TypeBounds(defn.NothingType, scrut))
                  else failNotSpecific(TypeBounds(scrut, defn.AnyType))
                else
                  scrut
            !instances(num).isError

          case MatchTypeCasePattern.TypeTest(tpe) =>
            // The actual type test is handled by `scrut <:< instantiatedPat`
            true

          case MatchTypeCasePattern.BaseTypeTest(classType, argPatterns, needsConcreteScrut) =>
            val cls = classType.classSymbol.asClass
            scrut.baseType(cls) match
              case base @ AppliedType(baseTycon, baseArgs) =>
                // #19445 Don't check the prefix of baseTycon here; it is handled by `scrut <:< instantiatedPat`.
                val innerScrutIsWidenedAbstract =
                  scrutIsWidenedAbstract
                    || (needsConcreteScrut && !isConcrete(scrut)) // no point in checking concreteness if it does not need to be concrete
                matchArgs(argPatterns, baseArgs, classType.typeParams, innerScrutIsWidenedAbstract)
              case _ =>
                false

          case MatchTypeCasePattern.AbstractTypeConstructor(tycon, argPatterns) =>
            scrut.dealias match
              case scrutDealias @ AppliedType(scrutTycon, args) if scrutTycon =:= tycon =>
                matchArgs(argPatterns, args, tycon.typeParams, scrutIsWidenedAbstract)
              case _ =>
                false

          case MatchTypeCasePattern.CompileTimeS(argPattern) =>
            natValue(scrut) match
              case Some(scrutValue) if scrutValue > 0 =>
                rec(argPattern, ConstantType(Constant(scrutValue - 1)), variance, scrutIsWidenedAbstract)
              case _ =>
                false

          case MatchTypeCasePattern.TypeMemberExtractor(typeMemberName, capture) =>
            val stableScrut: SingletonType = scrut match
              case scrut: SingletonType => scrut
              case _                    => SkolemType(scrut)
            stableScrut.member(typeMemberName) match
              case denot: SingleDenotation if denot.exists =>
                val info = denot.info match
                  case TypeAlias(alias)                => alias              // Extract the alias
                  case ClassInfo(prefix, cls, _, _, _) => prefix.select(cls) // Re-select the class from the prefix
                  case info => info // Notably, RealTypeBounds, which will eventually give a MatchResult.NoInstances
                val infoRefersToSkolem = stableScrut.isInstanceOf[SkolemType] && stableScrut.occursIn(info)
                val info1 = info match
                  case info: TypeBounds        => info                       // Will already trigger a MatchResult.NoInstances
                  case _ if infoRefersToSkolem => RealTypeBounds(info, info) // Explicitly trigger a MatchResult.NoInstances
                  case _                       => info                       // We have a match
                rec(capture, info1, variance = 0, scrutIsWidenedAbstract)
              case _ =>
                false
      end rec

      def matchArgs(argPatterns: List[MatchTypeCasePattern], args: List[Type], tparams: List[TypeParamInfo], scrutIsWidenedAbstract: Boolean): Boolean =
        if argPatterns.isEmpty then
          true
        else
          rec(argPatterns.head, args.head, tparams.head.paramVarianceSign, scrutIsWidenedAbstract)
            && matchArgs(argPatterns.tail, args.tail, tparams.tail, scrutIsWidenedAbstract)

      // This might not be needed
      val constrainedCaseLambda = constrained(spec.origMatchCase, ast.tpd.EmptyTree)._1.asInstanceOf[HKTypeLambda]

      val disjoint =
        val defn.MatchCase(origPattern, _) = constrainedCaseLambda.resultType: @unchecked
        provablyDisjoint(scrut, origPattern)

      def tryDisjoint: MatchResult =
        if disjoint then
          MatchResult.Disjoint
        else
          MatchResult.Stuck

      if rec(spec.pattern, scrut, variance = 1, scrutIsWidenedAbstract = false) then
        if noInstances.nonEmpty then
          MatchResult.NoInstance(noInstances.toList)
        else
          val defn.MatchCase(instantiatedPat, reduced) =
            instantiateParamsSpec(instances, constrainedCaseLambda)(constrainedCaseLambda.resultType): @unchecked
          if scrut <:< instantiatedPat then
            if disjoint then
              MatchResult.ReducedAndDisjoint
            else
              MatchResult.Reduced(reduced)
          else
            tryDisjoint
      else
        tryDisjoint
    end matchSpeccedPatMat

    def matchLegacyPatMat(spec: MatchTypeCaseSpec.LegacyPatMat): MatchResult =
      val caseLambda = constrained(spec.origMatchCase, ast.tpd.EmptyTree)._1.asInstanceOf[HKTypeLambda]
      this.caseLambda = caseLambda

      val defn.MatchCase(pat, body) = caseLambda.resultType: @unchecked

      def matches(canWidenAbstract: Boolean): Boolean =
        val saved = this.canWidenAbstract
        val savedPoisoned = this.poisoned
        this.canWidenAbstract = canWidenAbstract
        this.poisoned = Set.empty
        try necessarySubType(scrut, pat)
        finally
          poisoned = this.poisoned
          this.poisoned = savedPoisoned
          this.canWidenAbstract = saved

      val disjoint = provablyDisjoint(scrut, pat)

      def redux(canApprox: Boolean): MatchResult =
        val instances = paramInstances(canApprox)(Array.fill(caseLambda.paramNames.length)(NoType), pat)
        instantiateParams(instances)(body) match
          case Range(lo, hi) =>
            MatchResult.NoInstance {
              caseLambda.paramNames.zip(instances).collect {
                case (name, Range(lo, hi)) => (name, TypeBounds(lo, hi))
              }
            }
          case redux =>
            if disjoint then
              MatchResult.ReducedAndDisjoint
            else
              MatchResult.Reduced(redux)

      if matches(canWidenAbstract = false) then
        redux(canApprox = true)
      else if matches(canWidenAbstract = true) then
        redux(canApprox = false)
      else if (disjoint)
        // We found a proof that `scrut` and `pat` are incompatible.
        // The search continues.
        MatchResult.Disjoint
      else
        MatchResult.Stuck
    end matchLegacyPatMat

    def matchMissingCaptures(spec: MatchTypeCaseSpec.MissingCaptures): MatchResult =
      MatchResult.Stuck

    def recur(remaining: List[MatchTypeCaseSpec]): Type = remaining match
      case (cas: MatchTypeCaseSpec.LegacyPatMat) :: _ if sourceVersion.isAtLeast(SourceVersion.`3.4`) =>
        val errorText = MatchTypeTrace.illegalPatternText(scrut, cas)
        ErrorType(reporting.MatchTypeLegacyPattern(errorText))
      case cas :: remaining1 =>
        matchCase(cas) match
          case MatchResult.Disjoint =>
            recur(remaining1)
          case MatchResult.Stuck =>
            MatchTypeTrace.stuck(scrut, cas, remaining1)
            NoType
          case MatchResult.NoInstance(fails) =>
            MatchTypeTrace.noInstance(scrut, cas, fails)
            NoType
          case MatchResult.Reduced(tp) =>
            tp.simplified
          case MatchResult.ReducedAndDisjoint =>
            // Empty types break the basic assumption that if a scrutinee and a
            // pattern are disjoint it's OK to reduce passed that pattern. Indeed,
            // empty types viewed as a set of value is always a subset of any other
            // types. As a result, if a scrutinee both matches a pattern and is
            // probably disjoint from it, we prevent reduction.
            // See `tests/neg/6570.scala` and `6570-1.scala` for examples that
            // exploit emptiness to break match type soundness.
            MatchTypeTrace.emptyScrutinee(scrut)
            NoType
      case Nil =>
        /* TODO warn ? then re-enable warn/12974.scala:26
        val noCasesText = MatchTypeTrace.noMatchesText(scrut, cases)
        report.warning(reporting.MatchTypeNoCases(noCasesText), pos = ???)
        */
        MatchTypeTrace.noMatches(scrut, cases)
        NoType

    inFrozenConstraint(recur(cases))
  }
}

/** A type comparer that can record traces of subtype operations
 *  @param short  if true print only failing forward traces; never print succesful
 *                subtraces; never print backtraces starting with `<==`.
 */
class ExplainingTypeComparer(initctx: Context, short: Boolean) extends TypeComparer(initctx) {
  import TypeComparer.*

  init(initctx)

  override def explainingTypeComparer(short: Boolean) =
    if short == this.short then this
    else ExplainingTypeComparer(comparerContext, short)

  private var indent = 0
  private val b = new StringBuilder
  private var lastForwardGoal: String | Null = null

  override def traceIndented[T](str: String)(op: => T): T =
    val str1 = str.replace('\n', ' ')
    if short && str1 == lastForwardGoal then
      op // repeated goal, skip for clarity
    else
      lastForwardGoal = str1
      val curLength = b.length
      indent += 2
      b.append("\n").append(" " * indent).append("==> ").append(str1)
      val res = op
      if short then
        if res == false then
          if lastForwardGoal != null then  // last was deepest goal that failed
            b.append("  = false")
            lastForwardGoal = null
        else
          b.length = curLength // don't show successful subtraces
      else
        b.append("\n").append(" " * indent).append("<== ").append(str1).append(" = ").append(show(res))
      indent -= 2
      res

  private def traceIndentedIfNotShort[T](str: String)(op: => T): T =
    if short then op else traceIndented(str)(op)

  private def frozenNotice: String =
    if frozenConstraint then " in frozen constraint" else ""

  override def recur(tp1: Type, tp2: Type): Boolean =
    def moreInfo =
      if Config.verboseExplainSubtype || ctx.settings.verbose.value
      then s" ${tp1.getClass} ${tp2.getClass}"
      else ""
    val approx = approxState
    def approxStr = if short then "" else approx.show
    traceIndented(s"${show(tp1)}  <:  ${show(tp2)}$moreInfo${approxStr}$frozenNotice") {
      super.recur(tp1, tp2)
    }

  override def hasMatchingMember(name: Name, tp1: Type, tp2: RefinedType): Boolean =
    traceIndented(s"hasMatchingMember(${show(tp1)} . $name, ${show(tp2.refinedInfo)}), member = ${show(tp1.member(name).info)}") {
      super.hasMatchingMember(name, tp1, tp2)
    }

  override def lub(tp1: Type, tp2: Type, canConstrain: Boolean, isSoft: Boolean): Type =
    traceIndentedIfNotShort(s"lub(${show(tp1)}, ${show(tp2)}, canConstrain=$canConstrain, isSoft=$isSoft)") {
      super.lub(tp1, tp2, canConstrain, isSoft)
    }

  override def glb(tp1: Type, tp2: Type): Type =
    traceIndentedIfNotShort(s"glb(${show(tp1)}, ${show(tp2)})") {
      super.glb(tp1, tp2)
    }

  override def addConstraint(param: TypeParamRef, bound: Type, fromBelow: Boolean)(using Context): Boolean =
    traceIndented(s"add constraint ${show(param)} ${if (fromBelow) ">:" else "<:"} ${show(bound)} $frozenNotice, constraint = ${show(ctx.typerState.constraint)}") {
      super.addConstraint(param, bound, fromBelow)
    }

  override def gadtAddBound(sym: Symbol, b: Type, isUpper: Boolean): Boolean =
    traceIndented(s"add GADT constraint ${show(sym)} ${if isUpper then "<:" else ">:"} ${show(b)} $frozenNotice, GADT constraint = ${show(ctx.gadt)}") {
      super.gadtAddBound(sym, b, isUpper)
    }

  override def subCaptures(refs1: CaptureSet, refs2: CaptureSet, frozen: Boolean)(using Context): CaptureSet.CompareResult =
    traceIndented(i"subcaptures $refs1 <:< $refs2 ${if frozen then "frozen" else ""}") {
      super.subCaptures(refs1, refs2, frozen)
    }

  def lastTrace(header: String): String = header + { try b.toString finally b.clear() }
}
