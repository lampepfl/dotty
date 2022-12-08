package dotty.tools
package dotc
package core

import Types._
import Contexts._
import Symbols._
import Decorators._
import Flags._
import config.Config
import config.Printers.typr
import typer.ProtoTypes.{newTypeVar, representedParamRef}
import UnificationDirection.*
import NameKinds.AvoidNameKind
import util.SimpleIdentitySet
import NullOpsDecorator.stripNull

/** Methods for adding constraints and solving them.
 *
 * What goes into a Constraint as opposed to a ConstrainHandler?
 *
 * Constraint code is purely functional: Operations get constraints and produce new ones.
 * Constraint code does not have access to a type-comparer. Anything regarding lubs and glbs has to be done
 * elsewhere.
 *
 * By comparison: Constraint handlers are parts of type comparers and can use their functionality.
 * Constraint handlers update the current constraint as a side effect.
 */
trait ConstraintHandling {

  def constr: config.Printers.Printer = config.Printers.constr

  protected def isSub(tp1: Type, tp2: Type)(using Context): Boolean
  protected def isSame(tp1: Type, tp2: Type)(using Context): Boolean

  protected def constraint: Constraint
  protected def constraint_=(c: Constraint): Unit

  private var addConstraintInvocations = 0

  /** If the constraint is frozen we cannot add new bounds to the constraint. */
  protected var frozenConstraint: Boolean = false

  /** Potentially a type lambda that is still instantiatable, even though the constraint
   *  is generally frozen.
   */
  protected var caseLambda: Type = NoType

  /** If set, align arguments `S1`, `S2`when taking the glb
   *  `T1 { X = S1 } & T2 { X = S2 }` of a constraint upper bound for some type parameter.
   *  Aligning means computing `S1 =:= S2` which may change the current constraint.
   *  See note in TypeComparer#distributeAnd.
   */
  protected var homogenizeArgs: Boolean = false

  /** We are currently comparing type lambdas. Used as a flag for
   *  optimization: when `false`, no need to do an expensive `pruneLambdaParams`
   */
  protected var comparedTypeLambdas: Set[TypeLambda] = Set.empty

  /** Used for match type reduction: If false, we don't recognize an abstract type
   *  to be a subtype type of any of its base classes. This is in place only at the
   *  toplevel; it is turned on again when we add parts of the scrutinee to the constraint.
   */
  protected var canWidenAbstract: Boolean = true

  protected var myNecessaryConstraintsOnly = false
  /** When collecting the constraints needed for a particular subtyping
   *  judgment to be true, we sometimes need to approximate the constraint
   *  set (see `TypeComparer#either` for example).
   *
   *  Normally, this means adding extra constraints which may not be necessary
   *  for the subtyping judgment to be true, but if this variable is set to true
   *  we will instead under-approximate and keep only the constraints that must
   *  always be present for the subtyping judgment to hold.
   *
   *  This is needed for GADT bounds inference to be sound, but it is also used
   *  when constraining a method call based on its expected type to avoid adding
   *  constraints that would later prevent us from typechecking method
   *  arguments, see or-inf.scala and and-inf.scala for examples.
   */
  protected def necessaryConstraintsOnly(using Context): Boolean =
    ctx.mode.is(Mode.GadtConstraintInference) || myNecessaryConstraintsOnly

  /** If `trustBounds = false` we perform comparisons in a pessimistic way as follows:
   *  Given an abstract type `A >: L <: H`, a subtype comparison of any type
   *  with `A` will compare against both `L` and `H`. E.g.
   *
   *     T <:< A   if T <:< L and T <:< H
   *     A <:< T   if L <:< T and H <:< T
   *
   *  This restricted form makes sure we don't "forget"  types when forming
   *  unions and intersections with abstract types that have bad bounds. E.g.
   *  the following example from neg/i8900.scala that @smarter came up with:
   *  We have a type variable X with constraints
   *
   *     X >: 1, X >: x.M
   *
   *  where `x` is a locally nested variable and `x.M` has bad bounds
   *
   *     x.M >: Int | String <: Int & String
   *
   *  If we trust bounds, then the lower bound of `X` is `x.M` since `x.M >: 1`.
   *  Then even if we correct levels on instantiation to eliminate the local `x`,
   *  it is alreay too late, we'd get `Int & String` as instance, which does not
   *  satisfy the original constraint `X >: 1`.
   *
   *  But if `trustBounds` is false, we do not conclude the `x.M >: 1` since
   *  we compare both bounds and the upper bound `Int & String` is not a supertype
   *  of `1`. So the lower bound is `1 | x.M` and when we level-avoid that we
   *  get `1 | Int & String`, which simplifies to `Int`.
   */
  private var myTrustBounds = true

  inline def withUntrustedBounds(op: => Type): Type =
    val saved = myTrustBounds
    myTrustBounds = false
    try op finally myTrustBounds = saved

  def trustBounds: Boolean =
    !Config.checkLevelsOnInstantiation || myTrustBounds

  def checkReset() =
    assert(addConstraintInvocations == 0)
    assert(frozenConstraint == false)
    assert(caseLambda == NoType)
    assert(homogenizeArgs == false)
    assert(comparedTypeLambdas == Set.empty)

  def nestingLevel(param: TypeParamRef)(using Context) = constraint.typeVarOfParam(param) match
    case tv: TypeVar => tv.nestingLevel
    case _ =>
      // This should only happen when reducing match types (in
      // TrackingTypeComparer#matchCases) or in uncommitable TyperStates (as
      // asserted in ProtoTypes.constrained) and is special-cased in `levelOK`
      // below.
      Int.MaxValue

  /** Is `level` <= `maxLevel` or legal in the current context? */
  def levelOK(level: Int, maxLevel: Int)(using Context): Boolean =
    level <= maxLevel
    || ctx.isAfterTyper || !ctx.typerState.isCommittable // Leaks in these cases shouldn't break soundness
    || level == Int.MaxValue // See `nestingLevel` above.
    || !Config.checkLevelsOnConstraints

  /** If `param` is nested deeper than `maxLevel`, try to instantiate it to a
   *  fresh type variable of level `maxLevel` and return the new variable.
   *  If this isn't possible, throw a TypeError.
   */
  def atLevel(maxLevel: Int, param: TypeParamRef)(using Context): TypeParamRef =
    if levelOK(nestingLevel(param), maxLevel) then
      return param
    LevelAvoidMap(0, maxLevel)(param) match
      case freshVar: TypeVar => freshVar.origin
      case _ => throw TypeError(
        em"Could not decrease the nesting level of ${param} from ${nestingLevel(param)} to $maxLevel in $constraint")

  def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds = constraint.nonParamBounds(param)

  /** The full lower bound of `param` includes both the `nonParamBounds` and the
   *  params in the constraint known to be `<: param`, except that
   *  params with a `nestingLevel` higher than `param` will be instantiated
   *  to a fresh param at a legal level. See the documentation of `TypeVar`
   *  for details.
   */
  def fullLowerBound(param: TypeParamRef)(using Context): Type =
    val maxLevel = nestingLevel(param)
    var loParams = constraint.minLower(param)
    if maxLevel != Int.MaxValue then
      loParams = loParams.mapConserve(atLevel(maxLevel, _))
    loParams.foldLeft(nonParamBounds(param).lo)(_ | _)

  /** The full upper bound of `param`, see the documentation of `fullLowerBounds` above. */
  def fullUpperBound(param: TypeParamRef)(using Context): Type =
    val maxLevel = nestingLevel(param)
    var hiParams = constraint.minUpper(param)
    if maxLevel != Int.MaxValue then
      hiParams = hiParams.mapConserve(atLevel(maxLevel, _))
    hiParams.foldLeft(nonParamBounds(param).hi)(_ & _)

  /** Full bounds of `param`, including other lower/upper params.
    *
    * Note that underlying operations perform subtype checks - for this reason, recursing on `fullBounds`
    * of some param when comparing types might lead to infinite recursion. Consider `bounds` instead.
    */
  def fullBounds(param: TypeParamRef)(using Context): TypeBounds =
    nonParamBounds(param).derivedTypeBounds(fullLowerBound(param), fullUpperBound(param))

  /** An approximating map that prevents types nested deeper than maxLevel as
   *  well as WildcardTypes from leaking into the constraint.
   */
  class LevelAvoidMap(topLevelVariance: Int, maxLevel: Int)(using Context) extends TypeOps.AvoidMap:
    variance = topLevelVariance

    def toAvoid(tp: NamedType): Boolean =
      tp.prefix == NoPrefix && !tp.symbol.isStatic && !levelOK(tp.symbol.nestingLevel, maxLevel)

    /** Return a (possibly fresh) type variable of a level no greater than `maxLevel` which is:
     *  - lower-bounded by `tp` if variance >= 0
     *  - upper-bounded by `tp` if variance <= 0
     *  If this isn't possible, return the empty range.
     */
    def legalVar(tp: TypeVar): Type =
      val oldParam = tp.origin
      val nameKind =
        if variance > 0 then AvoidNameKind.UpperBound
        else if variance < 0 then AvoidNameKind.LowerBound
        else AvoidNameKind.BothBounds

      /** If it exists, return the first param in the list created in a previous call to `legalVar(tp)`
       *  with the appropriate level and variance.
       */
      def findParam(params: List[TypeParamRef]): Option[TypeParamRef] =
        params.find(p =>
          nestingLevel(p) <= maxLevel && representedParamRef(p) == oldParam &&
          (p.paramName.is(AvoidNameKind.BothBounds) ||
           variance != 0 && p.paramName.is(nameKind)))

      // First, check if we can reuse an existing parameter, this is more than an optimization
      // since it avoids an infinite loop in tests/pos/i8900-cycle.scala
      findParam(constraint.lower(oldParam)).orElse(findParam(constraint.upper(oldParam))) match
        case Some(param) =>
          constraint.typeVarOfParam(param)
        case _ =>
          // Otherwise, try to return a fresh type variable at `maxLevel` with
          // the appropriate constraints.
          val name = nameKind(oldParam.paramName.toTermName).toTypeName
          val freshVar = newTypeVar(TypeBounds.upper(tp.topType), name,
            nestingLevel = maxLevel, represents = oldParam)
          val ok =
            if variance < 0 then
              addLess(freshVar.origin, oldParam)
            else if variance > 0 then
              addLess(oldParam, freshVar.origin)
            else
              unify(freshVar.origin, oldParam)
          if ok then freshVar else emptyRange
    end legalVar

    override def apply(tp: Type): Type = tp match
      case tp: TypeVar if !tp.isInstantiated && !levelOK(tp.nestingLevel, maxLevel) =>
        legalVar(tp)
      // TypeParamRef can occur in tl bounds
      case tp: TypeParamRef =>
        constraint.typeVarOfParam(tp) match
          case tvar: TypeVar =>
            apply(tvar)
          case _ => super.apply(tp)
      case _ =>
        super.apply(tp)

    override def mapWild(t: WildcardType) =
      if ctx.mode.is(Mode.TypevarsMissContext) then super.mapWild(t)
      else
        val tvar = newTypeVar(apply(t.effectiveBounds).toBounds, nestingLevel = maxLevel)
        tvar
  end LevelAvoidMap

  /** Approximate `rawBound` if needed to make it a legal bound of `param` by
   *  avoiding cycles, wildcards and types with a level strictly greater than its
   *  `nestingLevel`.
   *
   *  Note that level-checking must be performed here and cannot be delayed
   *  until instantiation because if we allow level-incorrect bounds, then we
   *  might end up reasoning with bad bounds outside of the scope where they are
   *  defined. This can lead to level-correct but unsound instantiations as
   *  demonstrated by tests/neg/i8900.scala.
   */
  protected def legalBound(param: TypeParamRef, rawBound: Type, isUpper: Boolean)(using Context): Type =
    // Over-approximate for soundness.
    var variance = if isUpper then -1 else 1
    // ...unless we can only infer necessary constraints, in which case we
    // flip the variance to under-approximate.
    if necessaryConstraintsOnly then variance = -variance

    val approx = new LevelAvoidMap(variance, nestingLevel(param)):
      override def legalVar(tp: TypeVar): Type =
        // `legalVar` will create a type variable whose bounds depend on
        // `variance`, but whether the variance is positive or negative,
        // we can still infer necessary constraints since just creating a
        // type variable doesn't reduce the set of possible solutions.
        // Therefore, we can safely "unflip" the variance flipped above.
        // This is necessary for i8900-unflip.scala to typecheck.
        val v = if necessaryConstraintsOnly then -this.variance else this.variance
        atVariance(v)(super.legalVar(tp))
    constraint.validBoundFor(param, approx(rawBound), isUpper)
  end legalBound

  protected def addOneBound(param: TypeParamRef, rawBound: Type, isUpper: Boolean)(using Context): Boolean =
    if !constraint.contains(param) then true
    else if !isUpper && param.occursIn(rawBound) then
      // We don't allow recursive lower bounds when defining a type,
      // so we shouldn't allow them as constraints either.
      false
    else
      val bound = legalBound(param, rawBound, isUpper)
      val oldBounds @ TypeBounds(lo, hi) = constraint.nonParamBounds(param)
      val equalBounds = (if isUpper then lo else hi) eq bound
      if equalBounds && !bound.existsPart(_ eq param, StopAt.Static) then
        // The narrowed bounds are equal and not recursive,
        // so we can remove `param` from the constraint.
        constraint = constraint.replace(param, bound)
        true
      else
        // Narrow one of the bounds of type parameter `param`
        // If `isUpper` is true, ensure that `param <: `bound`, otherwise ensure
        // that `param >: bound`.
        val narrowedBounds =
          val saved = homogenizeArgs
          homogenizeArgs = Config.alignArgsInAnd
          try
            withUntrustedBounds(
              if isUpper then oldBounds.derivedTypeBounds(lo, hi & bound)
              else oldBounds.derivedTypeBounds(lo | bound, hi))
          finally
            homogenizeArgs = saved
        //println(i"narrow bounds for $param from $oldBounds to $narrowedBounds")
        val c1 = constraint.updateEntry(param, narrowedBounds)
        (c1 eq constraint)
        || {
          constraint = c1
          val TypeBounds(lo, hi) = constraint.entry(param): @unchecked
          isSub(lo, hi)
        }
  end addOneBound

  protected def addBoundTransitively(param: TypeParamRef, rawBound: Type, isUpper: Boolean)(using Context): Boolean =

    /** Adjust the bound `tp` in the following ways:
     *
     *   1. Toplevel occurrences of TypeRefs that are instantiated in the current
     *      constraint are also dereferenced.
     *   2. Toplevel occurrences of ExprTypes lead to a `NoType` return, which
     *      causes the addOneBound operation to fail.
     *
     *   An occurrence is toplevel if it is the bound itself, or a term in some
     *   combination of `&` or `|` types.
     */
    def adjust(tp: Type): Type = tp match
      case tp: AndOrType =>
        val p1 = adjust(tp.tp1)
        val p2 = adjust(tp.tp2)
        if p1.exists && p2.exists then tp.derivedAndOrType(p1, p2) else NoType
      case tp: TypeVar if constraint.contains(tp.origin) =>
        adjust(tp.underlying)
      case tp: ExprType =>
        // ExprTypes are not value types, so type parameters should not
        // be instantiated to ExprTypes. A scenario where such an attempted
        // instantiation can happen is if we unify (=> T) => () with A => ()
        // where A is a TypeParamRef. See the comment on EtaExpansion.etaExpand
        // why types such as (=> T) => () can be constructed and i7969.scala
        // as a test where this happens.
        // Note that scalac by contrast allows such instantiations. But letting
        // type variables be ExprTypes has its own problems (e.g. you can't write
        // the resulting types down) and is largely unknown terrain.
        NoType
      case _ =>
        tp

    def description = i"constraint $param ${if isUpper then "<:" else ":>"} $rawBound to\n$constraint"
    constr.println(i"adding $description$location")
    if isUpper && rawBound.isRef(defn.NothingClass) && ctx.typerState.isGlobalCommittable then
      def msg = i"!!! instantiated to Nothing: $param, constraint = $constraint"
      if Config.failOnInstantiationToNothing
      then assert(false, msg)
      else report.log(msg)
    def others = if isUpper then constraint.lower(param) else constraint.upper(param)
    val bound = adjust(rawBound)
    bound.exists
    && addOneBound(param, bound, isUpper) && others.forall(addOneBound(_, bound, isUpper))
        .showing(i"added $description = $result$location", constr)
  end addBoundTransitively

  protected def addLess(p1: TypeParamRef, p2: TypeParamRef)(using Context): Boolean = {
    def description = i"ordering $p1 <: $p2 to\n$constraint"
    val res =
      if (constraint.isLess(p2, p1)) unify(p2, p1)
      else {
        val down1 = p1 :: constraint.exclusiveLower(p1, p2)
        val up2 = p2 :: constraint.exclusiveUpper(p2, p1)
        val lo1 = constraint.nonParamBounds(p1).lo
        val hi2 = constraint.nonParamBounds(p2).hi
        constr.println(i"adding $description down1 = $down1, up2 = $up2$location")
        constraint = constraint.addLess(p1, p2)
        down1.forall(addOneBound(_, hi2, isUpper = true)) &&
        up2.forall(addOneBound(_, lo1, isUpper = false))
      }
    constr.println(i"added $description = $res$location")
    res
  }

  def location(using Context) = "" // i"in ${ctx.typerState.stateChainStr}" // use for debugging

  /** Unify p1 with p2: one parameter will be kept in the constraint, the
   *  other will be removed and its bounds transferred to the remaining one.
   *
   *  If p1 and p2 have different `nestingLevel`, the parameter with the lowest
   *  level will be kept and the transferred bounds from the other parameter
   *  will be adjusted for level-correctness.
   */
  private def unify(p1: TypeParamRef, p2: TypeParamRef)(using Context): Boolean = {
    constr.println(s"unifying $p1 $p2")
    if !constraint.isLess(p1, p2) then
      constraint = constraint.addLess(p1, p2)

    val level1 = nestingLevel(p1)
    val level2 = nestingLevel(p2)
    val pKept    = if level1 <= level2 then p1 else p2
    val pRemoved = if level1 <= level2 then p2 else p1

    val down = constraint.exclusiveLower(p2, p1)
    val up = constraint.exclusiveUpper(p1, p2)

    constraint = constraint.addLess(p2, p1, direction = if pKept eq p1 then KeepParam2 else KeepParam1)

    val boundKept    = constraint.validBoundsFor(pKept,
      constraint.nonParamBounds(   pKept).substParam(pRemoved, pKept).bounds)
    var boundRemoved = constraint.validBoundsFor(pKept,
      constraint.nonParamBounds(pRemoved).substParam(pRemoved, pKept).bounds)

    if level1 != level2 then
      boundRemoved = LevelAvoidMap(-1, math.min(level1, level2))(boundRemoved)
      val TypeBounds(lo, hi) = boundRemoved: @unchecked
      // After avoidance, the interval might be empty, e.g. in
      // tests/pos/i8900-promote.scala:
      //     >: x.type <: Singleton
      // becomes:
      //     >: Int <: Singleton
      // In that case, we can still get a legal constraint
      // by replacing the lower-bound to get:
      //     >: Int & Singleton <: Singleton
      if !isSub(lo, hi) then
        boundRemoved = TypeBounds(lo & hi, hi)

    val newBounds = (boundKept & boundRemoved).bounds
    constraint = constraint.updateEntry(pKept, newBounds).replace(pRemoved, pKept)

    val lo = newBounds.lo
    val hi = newBounds.hi
    isSub(lo, hi) &&
    down.forall(addOneBound(_, hi, isUpper = true)) &&
    up.forall(addOneBound(_, lo, isUpper = false))
  }

  protected def isSubType(tp1: Type, tp2: Type, whenFrozen: Boolean)(using Context): Boolean =
    if (whenFrozen)
      isSubTypeWhenFrozen(tp1, tp2)
    else
      isSub(tp1, tp2)

  inline final def inFrozenConstraint[T](op: => T): T = {
    val savedFrozen = frozenConstraint
    val savedLambda = caseLambda
    frozenConstraint = true
    caseLambda = NoType
    try op
    finally {
      frozenConstraint = savedFrozen
      caseLambda = savedLambda
    }
  }

  final def isSubTypeWhenFrozen(tp1: Type, tp2: Type)(using Context): Boolean = inFrozenConstraint(isSub(tp1, tp2))
  final def isSameTypeWhenFrozen(tp1: Type, tp2: Type)(using Context): Boolean = inFrozenConstraint(isSame(tp1, tp2))

  /** Test whether the lower bounds of all parameters in this
   *  constraint are a solution to the constraint.
   */
  protected final def isSatisfiable(using Context): Boolean =
    constraint.forallParams { param =>
      val TypeBounds(lo, hi) = constraint.entry(param): @unchecked
      isSub(lo, hi) || {
        report.log(i"sub fail $lo <:< $hi")
        false
      }
    }

  /** Fix instance type `tp` by avoidance  so that it does not contain references
   *  to types at level > `maxLevel`.
   *  @param tp         the type to be fixed
   *  @param fromBelow  whether type was obtained from lower bound
   *  @param maxLevel   the maximum level of references allowed
   *  @param param      the parameter that was instantiated
   */
  private def fixLevels(tp: Type, fromBelow: Boolean, maxLevel: Int, param: TypeParamRef)(using Context) =

    def needsFix(tp: NamedType)(using Context) =
      (tp.prefix eq NoPrefix) && tp.symbol.nestingLevel > maxLevel

    /** An accumulator that determines whether levels need to be fixed
     *  and computes on the side sets of nested type variables that need
     *  to be instantiated.
     */
    def needsLeveling = new TypeAccumulator[Boolean]:
      if !fromBelow then variance = -1

      def apply(need: Boolean, tp: Type) =
        need || tp.match
          case tp: NamedType =>
            needsFix(tp)
            || !stopBecauseStaticOrLocal(tp) && apply(need, tp.prefix)
          case tp: TypeVar =>
            val inst = tp.instanceOpt
            if inst.exists then apply(need, inst)
            else if tp.nestingLevel > maxLevel then
              // Change the nesting level of inner type variable to `maxLevel`.
              // This means that the type variable will be instantiated later to a
              // less nested type. If there are other references to the same type variable
              // that do not come from the type undergoing `fixLevels`, this could lead
              // to coarser types than intended. An alternative is to instantiate the
              // type variable right away, but this also loses information. See
              // i15934.scala for a test where the current strategey works but an early instantiation
              // of `tp` would fail.
              constr.println(i"widening nesting level of type variable $tp from ${tp.nestingLevel} to $maxLevel")
              ctx.typerState.setNestingLevel(tp, maxLevel)
              true
            else false
          case _ =>
            foldOver(need, tp)
    end needsLeveling

    def levelAvoid = new TypeOps.AvoidMap:
      if !fromBelow then variance = -1
      def toAvoid(tp: NamedType) = needsFix(tp)

    if Config.checkLevelsOnInstantiation && !ctx.isAfterTyper && needsLeveling(false, tp) then
      typr.println(i"instance $tp for $param needs leveling to $maxLevel")
      levelAvoid(tp)
    else tp
  end fixLevels

  /** Solve constraint set for given type parameter `param`.
   *  If `fromBelow` is true the parameter is approximated by its lower bound,
   *  otherwise it is approximated by its upper bound, unless the upper bound
   *  contains a reference to the parameter itself (such occurrences can arise
   *  for F-bounded types, `addOneBound` ensures that they never occur in the
   *  lower bound).
   *  The solved type is not allowed to contain references to types nested deeper
   *  than `maxLevel`.
   *  Wildcard types in bounds are approximated by their upper or lower bounds.
   *  The constraint is left unchanged.
   *  @return the instantiating type
   *  @pre `param` is in the constraint's domain.
   */
  final def approximation(param: TypeParamRef, fromBelow: Boolean, maxLevel: Int)(using Context): Type =
    constraint.entry(param) match
      case entry: TypeBounds =>
        val useLowerBound = fromBelow || param.occursIn(entry.hi)
        val rawInst = withUntrustedBounds(
          if useLowerBound then fullLowerBound(param) else fullUpperBound(param))
        val levelInst = fixLevels(rawInst, fromBelow, maxLevel, param)
        if levelInst ne rawInst then
          typr.println(i"level avoid for $maxLevel: $rawInst --> $levelInst")
        typr.println(i"approx $param, from below = $fromBelow, inst = $levelInst")
        levelInst
      case inst =>
        assert(inst.exists, i"param = $param\nconstraint = $constraint")
        inst
  end approximation

  private def isTransparent(tp: Type, traitOnly: Boolean)(using Context): Boolean = tp match
    case AndType(tp1, tp2) =>
      isTransparent(tp1, traitOnly) && isTransparent(tp2, traitOnly)
    case _ =>
      val cls = tp.underlyingClassRef(refinementOK = false).typeSymbol
      cls.isTransparentClass && (!traitOnly || cls.is(Trait))

  /** If `tp` is an intersection such that some operands are transparent trait instances
   *  and others are not, replace as many transparent trait instances as possible with Any
   *  as long as the result is still a subtype of `bound`. But fall back to the
   *  original type if the resulting widened type is a supertype of all dropped
   *  types (since in this case the type was not a true intersection of transparent traits
   *  and other types to start with).
   */
  def dropTransparentTraits(tp: Type, bound: Type)(using Context): Type =
    var kept: Set[Type] = Set()      // types to keep since otherwise bound would not fit
    var dropped: List[Type] = List() // the types dropped so far, last one on top

    def dropOneTransparentTrait(tp: Type): Type =
      if isTransparent(tp, traitOnly = true) && !kept.contains(tp) then
        dropped = tp :: dropped
        defn.AnyType
      else tp match
        case AndType(tp1, tp2) =>
          val tp1w = dropOneTransparentTrait(tp1)
          if tp1w ne tp1 then tp1w & tp2
          else
            val tp2w = dropOneTransparentTrait(tp2)
            if tp2w ne tp2 then tp1 & tp2w
            else tp
        case _ =>
          tp

    def recur(tp: Type): Type =
      val tpw = dropOneTransparentTrait(tp)
      if tpw eq tp then tp
      else if tpw <:< bound then recur(tpw)
      else
        kept += dropped.head
        dropped = dropped.tail
        recur(tp)

    val saved = ctx.typerState.snapshot()
    val tpw = recur(tp)
    if (tpw eq tp) || dropped.forall(_ frozen_<:< tpw) then
      // Rollback any constraint change that would lead to `tp` no longer
      // being a valid solution.
      ctx.typerState.resetTo(saved)
      tp
    else
      tpw
  end dropTransparentTraits

  /** If `tp` is an applied match type alias which is also an unreducible application
   *  of a higher-kinded type to a wildcard argument, widen to the match type's bound,
   *  in order to avoid an unreducible application of higher-kinded type ... in inferred type"
   *  error in PostTyper. Fixes #11246.
   */
  def widenIrreducible(tp: Type)(using Context): Type = tp match
    case tp @ AppliedType(tycon, _) if tycon.isLambdaSub && tp.hasWildcardArg =>
      tp.superType match
        case MatchType(bound, _, _) => bound
        case _ => tp
    case _ =>
      tp

  /** Widen inferred type `inst` with upper `bound`, according to the following rules:
   *   1. If `inst` is a singleton type, or a union containing some singleton types,
   *      widen (all) the singleton type(s), provided the result is a subtype of `bound`
   *      (i.e. `inst.widenSingletons <:< bound` succeeds with satisfiable constraint) and
   *      is not transparent according to `isTransparent`.
   *   2a. If `inst` is a union type and `widenUnions` is true, approximate the union type
   *      from above by an intersection of all common base types, provided the result
   *      is a subtype of `bound`.
   *   2b. If `inst` is a union type and `widenUnions` is false, turn it into a hard
   *      union type (except for unions | Null, which are kept in the state they were).
   *   3. Widen some irreducible applications of higher-kinded types to wildcard arguments
   *      (see @widenIrreducible).
   *   4. Drop transparent traits from intersections (see @dropTransparentTraits).
   *
   *  Don't do these widenings if `bound` is a subtype of `scala.Singleton`.
   *  Also, if the result of these widenings is a TypeRef to a module class,
   *  and this type ref is different from `inst`, replace by a TermRef to
   *  its source module instead.
   *
   * At this point we also drop the @Repeated annotation to avoid inferring type arguments with it,
   * as those could leak the annotation to users (see run/inferred-repeated-result).
   */
  def widenInferred(inst: Type, bound: Type, widenUnions: Boolean)(using Context): Type =
    def widenOr(tp: Type) =
      if widenUnions then
        val tpw = tp.widenUnion
        if (tpw ne tp) && !isTransparent(tpw, traitOnly = false) && (tpw <:< bound) then tpw else tp
      else tp.hardenUnions

    def widenSingle(tp: Type) =
      val tpw = tp.widenSingletons
      if (tpw ne tp) && (tpw <:< bound) then tpw else tp

    def isSingleton(tp: Type): Boolean = tp match
      case WildcardType(optBounds) => optBounds.exists && isSingleton(optBounds.bounds.hi)
      case _ => isSubTypeWhenFrozen(tp, defn.SingletonType)

    val wideInst =
      if isSingleton(bound) then inst
      else
        val widenedFromSingle = widenSingle(inst)
        val widenedFromUnion = widenOr(widenedFromSingle)
        val widened = dropTransparentTraits(widenedFromUnion, bound)
        widenIrreducible(widened)

    wideInst match
      case wideInst: TypeRef if wideInst.symbol.is(Module) =>
        TermRef(wideInst.prefix, wideInst.symbol.sourceModule)
      case _ =>
        wideInst.dropRepeatedAnnot
  end widenInferred

  /** Convert all toplevel union types in `tp` to hard unions */
  extension (tp: Type) private def hardenUnions(using Context): Type = tp.widen match
    case tp: AndType =>
      tp.derivedAndType(tp.tp1.hardenUnions, tp.tp2.hardenUnions)
    case tp: RefinedType =>
      tp.derivedRefinedType(tp.parent.hardenUnions, tp.refinedName, tp.refinedInfo)
    case tp: RecType =>
      tp.rebind(tp.parent.hardenUnions)
    case tp: HKTypeLambda =>
      tp.derivedLambdaType(resType = tp.resType.hardenUnions)
    case tp: OrType =>
      val tp1 = tp.stripNull
      if tp1 ne tp then tp.derivedOrType(tp1.hardenUnions, defn.NullType)
      else tp.derivedOrType(tp.tp1.hardenUnions, tp.tp2.hardenUnions, soft = false)
    case _ =>
      tp

  /** The instance type of `param` in the current constraint (which contains `param`).
   *  If `fromBelow` is true, the instance type is the lub of the parameter's
   *  lower bounds; otherwise it is the glb of its upper bounds. However,
   *  a lower bound instantiation can be a singleton type only if the upper bound
   *  is also a singleton type.
   *  The instance type is not allowed to contain references to types nested deeper
   *  than `maxLevel`.
   */
  def instanceType(param: TypeParamRef, fromBelow: Boolean, widenUnions: Boolean, maxLevel: Int)(using Context): Type = {
    val approx = approximation(param, fromBelow, maxLevel).simplified
    if fromBelow then
      val widened = widenInferred(approx, param, widenUnions)
      // Widening can add extra constraints, in particular the widened type might
      // be a type variable which is now instantiated to `param`, and therefore
      // cannot be used as an instantiation of `param` without creating a loop.
      // If that happens, we run `instanceType` again to find a new instantation.
      // (we do not check for non-toplevel occurences: those should never occur
      // since `addOneBound` disallows recursive lower bounds).
      if constraint.occursAtToplevel(param, widened) then
        instanceType(param, fromBelow, widenUnions, maxLevel)
      else
        widened
    else
      approx
  }

  /** Constraint `c1` subsumes constraint `c2`, if under `c2` as constraint we have
   *  for all poly params `p` defined in `c2` as `p >: L2 <: U2`:
   *
   *     c1 defines p with bounds p >: L1 <: U1, and
   *     L2 <: L1, and
   *     U1 <: U2
   *
   *  Both `c1` and `c2` are required to derive from constraint `pre`, without adding
   *  any new type variables but possibly narrowing already registered ones with further bounds.
   */
  protected final def subsumes(c1: Constraint, c2: Constraint, pre: Constraint)(using Context): Boolean =
    if (c2 eq pre) true
    else if (c1 eq pre) false
    else {
      val saved = constraint
      try
        // We iterate over params of `pre`, instead of `c2` as the documentation may suggest.
        // As neither `c1` nor `c2` can have more params than `pre`, this only matters in one edge case.
        // Constraint#forallParams only iterates over params that can be directly constrained.
        // If `c2` has, compared to `pre`, instantiated a param and we iterated over params of `c2`,
        // we could miss that param being instantiated to an incompatible type in `c1`.
        pre.forallParams(p =>
          c1.entry(p).exists
          && c2.upper(p).forall(c1.isLess(p, _))
          && isSubTypeWhenFrozen(c1.nonParamBounds(p), c2.nonParamBounds(p))
        )
      finally constraint = saved
    }

  /** The current bounds of type parameter `param` */
  def bounds(param: TypeParamRef)(using Context): TypeBounds = {
    val e = constraint.entry(param)
    if (e.exists) e.bounds
    else {
      // TODO: should we change the type of paramInfos to nullable?
      val pinfos: List[param.binder.PInfo] | Null = param.binder.paramInfos
      if (pinfos != null) pinfos(param.paramNum) // pinfos == null happens in pos/i536.scala
      else TypeBounds.empty
    }
  }

  /** Add type lambda `tl`, possibly with type variables `tvars`, to current constraint
   *  and propagate all bounds.
   *  @param tvars   See Constraint#add
   */
  def addToConstraint(tl: TypeLambda, tvars: List[TypeVar])(using Context): Boolean =
    checkPropagated(i"initialized $tl") {
      constraint = constraint.add(tl, tvars)
      tl.paramRefs.forall { param =>
        val lower = constraint.lower(param)
        val upper = constraint.upper(param)
        constraint.entry(param) match {
          case bounds: TypeBounds =>
            if lower.nonEmpty && !bounds.lo.isRef(defn.NothingClass)
               || upper.nonEmpty && !bounds.hi.isAny
            then constr.println(i"INIT*** $tl")
            lower.forall(addOneBound(_, bounds.hi, isUpper = true)) &&
              upper.forall(addOneBound(_, bounds.lo, isUpper = false))
          case x =>
            // Happens if param was already solved while processing earlier params of the same TypeLambda.
            // See #4720.
            true
        }
      }
    }

  /** Can `param` be constrained with new bounds? */
  final def canConstrain(param: TypeParamRef): Boolean =
    (!frozenConstraint || (caseLambda `eq` param.binder)) && constraint.contains(param)

  /** Is `param` assumed to be a sub- and super-type of any other type?
   *  This holds if `TypeVarsMissContext` is set unless `param` is a part
   *  of a MatchType that is currently normalized.
   */
  final def assumedTrue(param: TypeParamRef)(using Context): Boolean =
    ctx.mode.is(Mode.TypevarsMissContext) && (caseLambda `ne` param.binder)

  /** Add constraint `param <: bound` if `fromBelow` is false, `param >: bound` otherwise.
   *  `bound` is assumed to be in normalized form, as specified in `firstTry` and
   *  `secondTry` of `TypeComparer`. In particular, it should not be an alias type,
   *  lazy ref, typevar, wildcard type, error type. In addition, upper bounds may
   *  not be AndTypes and lower bounds may not be OrTypes. This is assured by the
   *  way isSubType is organized.
   */
  protected def addConstraint(param: TypeParamRef, bound: Type, fromBelow: Boolean)(using Context): Boolean =
    if !bound.isValueTypeOrLambda then return false

    /** When comparing lambdas we might get constraints such as
     *  `A <: X0` or `A = List[X0]` where `A` is a constrained parameter
     *  and `X0` is a lambda parameter. The constraint for `A` is not allowed
     *  to refer to such a lambda parameter because the lambda parameter is
     *  not visible where `A` is defined. Consequently, we need to
     *  approximate the bound so that the lambda parameter does not appear in it.
     *  If `tp` is an upper bound, we need to approximate with something smaller,
     *  otherwise something larger.
     *  Test case in pos/i94-nada.scala. This test crashes with an illegal instance
     *  error in Test2 when the rest of the SI-2712 fix is applied but `pruneLambdaParams` is
     *  missing.
     */
    def avoidLambdaParams(tp: Type) =
      if comparedTypeLambdas.nonEmpty then
        val approx = new ApproximatingTypeMap {
          if (!fromBelow) variance = -1
          def apply(t: Type): Type = t match {
            case t @ TypeParamRef(tl: TypeLambda, n) if comparedTypeLambdas contains tl =>
              val bounds = tl.paramInfos(n)
              range(bounds.lo, bounds.hi)
            case tl: TypeLambda =>
              val saved = comparedTypeLambdas
              comparedTypeLambdas -= tl
              try mapOver(tl)
              finally comparedTypeLambdas = saved
            case _ =>
              mapOver(t)
          }
        }
        approx(tp)
      else tp

    def addParamBound(bound: TypeParamRef) =
      constraint.entry(param) match {
        case _: TypeBounds =>
          if (fromBelow) addLess(bound, param) else addLess(param, bound)
        case tp =>
          if (fromBelow) isSub(bound, tp) else isSub(tp, bound)
      }

    def kindCompatible(tp1: Type, tp2: Type): Boolean =
      val tparams1 = tp1.typeParams
      val tparams2 = tp2.typeParams
      tparams1.corresponds(tparams2)((p1, p2) => kindCompatible(p1.paramInfo, p2.paramInfo))
      && (tparams1.isEmpty || kindCompatible(tp1.hkResult, tp2.hkResult))
      || tp1.hasAnyKind
      || tp2.hasAnyKind

    def description = i"constr $param ${if (fromBelow) ">:" else "<:"} $bound:\n$constraint"

    //checkPropagated(s"adding $description")(true) // DEBUG in case following fails
    checkPropagated(s"added $description") {
      addConstraintInvocations += 1
      val saved = canWidenAbstract
      canWidenAbstract = true
      try bound match
        case bound: TypeParamRef if constraint contains bound =>
          addParamBound(bound)
        case _ =>
          val pbound = avoidLambdaParams(bound)
          kindCompatible(param, pbound) && addBoundTransitively(param, pbound, !fromBelow)
      finally
        canWidenAbstract = saved
        addConstraintInvocations -= 1
    }
  end addConstraint

  /** Check that constraint is fully propagated. See comment in Config.checkConstraintsPropagated */
  def checkPropagated(msg: => String)(result: Boolean)(using Context): Boolean = {
    if (Config.checkConstraintsPropagated && result && addConstraintInvocations == 0)
      inFrozenConstraint {
        for (p <- constraint.domainParams) {
          def check(cond: => Boolean, q: TypeParamRef, ordering: String, explanation: String): Unit =
            assert(cond, i"propagation failure for $p $ordering $q: $explanation\n$msg")
          for (u <- constraint.upper(p))
            check(bounds(p).hi <:< bounds(u).hi, u, "<:", "upper bound not propagated")
          for (l <- constraint.lower(p)) {
            check(bounds(l).lo <:< bounds(p).hi, l, ">:", "lower bound not propagated")
            check(constraint.isLess(l, p), l, ">:", "reverse ordering (<:) missing")
          }
        }
      }
    result
  }
}
