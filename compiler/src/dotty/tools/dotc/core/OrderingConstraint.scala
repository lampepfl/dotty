package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Decorators._, TypeApplications._
import util.SimpleIdentityMap
import collection.mutable
import printing.Printer
import printing.Texts._
import config.Config
import config.Printers.constr
import reflect.ClassTag
import annotation.tailrec
import annotation.internal.sharable

object OrderingConstraint {

  type ArrayValuedMap[T] = SimpleIdentityMap[TypeLambda, Array[T]]

  /** The type of `OrderingConstraint#boundsMap` */
  type ParamBounds = ArrayValuedMap[Type]

  /** The type of `OrderingConstraint#lowerMap`, `OrderingConstraint#upperMap` */
  type ParamOrdering = ArrayValuedMap[List[TypeParamRef]]

  /** A new constraint with given maps */
  private def newConstraint(boundsMap: ParamBounds, lowerMap: ParamOrdering, upperMap: ParamOrdering)(using Context) : OrderingConstraint =
    if boundsMap.isEmpty && lowerMap.isEmpty && upperMap.isEmpty then
      empty
    else
      val result = new OrderingConstraint(boundsMap, lowerMap, upperMap)
      ctx.run.nn.recordConstraintSize(result, result.boundsMap.size)
      result

  /** A lens for updating a single entry array in one of the three constraint maps */
  abstract class ConstraintLens[T <: AnyRef: ClassTag] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[T] | Null
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[T])(using Context): OrderingConstraint
    def initial: T

    def apply(c: OrderingConstraint, poly: TypeLambda, idx: Int): T = {
      val es = entries(c, poly)
      if (es == null) initial else es(idx)
    }

    /** The `current` constraint but with the entry for `param` updated to `entry`.
     *  `current` is used linearly. If it is different from `prev` it is
     *  known to be dead after the call. Hence it is OK to update destructively
     *  parts of `current` which are not shared by `prev`.
     */
    def update(prev: OrderingConstraint, current: OrderingConstraint,
        poly: TypeLambda, idx: Int, entry: T)(using Context): OrderingConstraint = {
      var es = entries(current, poly)
      // TODO: investigate why flow typing is not working on `es`
      if (es != null && (es.nn(idx) eq entry)) current
      else {
        val result =
          if (es == null) {
            es = Array.fill(poly.paramNames.length)(initial)
            updateEntries(current, poly, es.nn)
          }
          else {
            val prev_es = entries(prev, poly)
            if (prev_es == null || (es.nn ne prev_es.nn))
              current // can re-use existing entries array.
            else {
              es = es.nn.clone
              updateEntries(current, poly, es.nn)
            }
          }
        es.nn(idx) = entry
        result
      }
    }

    def update(prev: OrderingConstraint, current: OrderingConstraint,
        param: TypeParamRef, entry: T)(using Context): OrderingConstraint =
      update(prev, current, param.binder, param.paramNum, entry)

    def map(prev: OrderingConstraint, current: OrderingConstraint,
        poly: TypeLambda, idx: Int, f: T => T)(using Context): OrderingConstraint =
     update(prev, current, poly, idx, f(apply(current, poly, idx)))

    def map(prev: OrderingConstraint, current: OrderingConstraint,
        param: TypeParamRef, f: T => T)(using Context): OrderingConstraint =
      map(prev, current, param.binder, param.paramNum, f)
  }

  val boundsLens: ConstraintLens[Type] = new ConstraintLens[Type] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[Type] | Null =
      c.boundsMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[Type])(using Context): OrderingConstraint =
      newConstraint(c.boundsMap.updated(poly, entries), c.lowerMap, c.upperMap)
    def initial = NoType
  }

  val lowerLens: ConstraintLens[List[TypeParamRef]] = new ConstraintLens[List[TypeParamRef]] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[List[TypeParamRef]] | Null =
      c.lowerMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[List[TypeParamRef]])(using Context): OrderingConstraint =
      newConstraint(c.boundsMap, c.lowerMap.updated(poly, entries), c.upperMap)
    def initial = Nil
  }

  val upperLens: ConstraintLens[List[TypeParamRef]] = new ConstraintLens[List[TypeParamRef]] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[List[TypeParamRef]] | Null =
      c.upperMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[List[TypeParamRef]])(using Context): OrderingConstraint =
      newConstraint(c.boundsMap, c.lowerMap, c.upperMap.updated(poly, entries))
    def initial = Nil
  }

  @sharable
  val empty = new OrderingConstraint(SimpleIdentityMap.empty, SimpleIdentityMap.empty, SimpleIdentityMap.empty)
}

import OrderingConstraint._

/** Constraint over undetermined type parameters that keeps separate maps to
 *  reflect parameter orderings.
 *  @param boundsMap a map from TypeLambda to arrays.
 *               Each array contains twice the number of entries as there a type parameters
 *               in the TypeLambda. The first half of the array contains the type bounds that constrain the
 *               lambda's type parameters. The second half might contain type variables that
 *               track the corresponding parameters, or is left empty (filled with nulls).
 *               An instantiated type parameter is represented by having its instance type in
 *               the corresponding array entry. The dual use of arrays for poly params
 *               and typevars is to save space and hopefully gain some speed.
 *
 *  @param lowerMap a map from TypeLambdas to arrays. Each array entry corresponds
 *               to a parameter P of the type lambda; it contains all constrained parameters
 *               Q that are known to be smaller than P, i.e. Q <: P.
 *  @param upperMap a map from TypeLambdas to arrays. Each array entry corresponds
 *               to a parameter P of the type lambda; it contains all constrained parameters
 *               Q that are known to be greater than P, i.e. P <: Q.
 */
class OrderingConstraint(private val boundsMap: ParamBounds,
                         private val lowerMap : ParamOrdering,
                         private val upperMap : ParamOrdering) extends Constraint {

  import UnificationDirection.*

  type This = OrderingConstraint

// ----------- Basic indices --------------------------------------------------

  /** The number of type parameters in the given entry array */
  private def paramCount(entries: Array[Type]) = entries.length >> 1

  /** The type variable corresponding to parameter numbered `n`, null if none was created */
  private def typeVar(entries: Array[Type], n: Int): Type | Null =
    entries(paramCount(entries) + n)

  /** The `boundsMap` entry corresponding to `param` */
  def entry(param: TypeParamRef): Type = {
    val entries = boundsMap(param.binder)
    if (entries == null) NoType
    else entries(param.paramNum)
  }

// ----------- Contains tests --------------------------------------------------

  def contains(pt: TypeLambda): Boolean = boundsMap(pt) != null

  def contains(param: TypeParamRef): Boolean = {
    val entries = boundsMap(param.binder)
    entries != null && isBounds(entries(param.paramNum))
  }

  def contains(tvar: TypeVar): Boolean = {
    val origin = tvar.origin
    val entries = boundsMap(origin.binder)
    val pnum = origin.paramNum
    entries != null && isBounds(entries(pnum)) && (typeVar(entries, pnum) eq tvar)
  }

// ---------- Dependency handling ----------------------------------------------

  def lower(param: TypeParamRef): List[TypeParamRef] = lowerLens(this, param.binder, param.paramNum)
  def upper(param: TypeParamRef): List[TypeParamRef] = upperLens(this, param.binder, param.paramNum)

  def minLower(param: TypeParamRef): List[TypeParamRef] = {
    val all = lower(param)
    all.filterNot(p => all.exists(isLess(p, _)))
  }

  def minUpper(param: TypeParamRef): List[TypeParamRef] = {
    val all = upper(param)
    all.filterNot(p => all.exists(isLess(_, p)))
  }

  def exclusiveLower(param: TypeParamRef, butNot: TypeParamRef): List[TypeParamRef] =
    lower(param).filterNot(isLess(_, butNot))

  def exclusiveUpper(param: TypeParamRef, butNot: TypeParamRef): List[TypeParamRef] =
    upper(param).filterNot(isLess(butNot, _))

// ---------- Info related to TypeParamRefs -------------------------------------------

  def isLess(param1: TypeParamRef, param2: TypeParamRef): Boolean =
    upper(param1).contains(param2)

  def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds =
    entry(param).bounds

  def typeVarOfParam(param: TypeParamRef): Type =
    val entries = boundsMap(param.binder)
    if entries == null then NoType
    else
      val tvar = typeVar(entries, param.paramNum)
      if tvar == null then NoType
      else tvar

// ---------- Adding TypeLambdas --------------------------------------------------

  /** The bound type `tp` without constrained parameters which are clearly
   *  dependent. A parameter in an upper bound is clearly dependent if it appears
   *  in a hole of a context H given by:
   *
   *      H = []
   *          H & T
   *          T & H
   *
   *  (the idea is that a parameter P in a H context is guaranteed to be a supertype of the
   *   bounded parameter.)
   *  Analogously, a parameter in a lower bound is clearly dependent if it appears
   *  in a hole of a context H given by:
   *
   *      L = []
   *          L | T
   *          T | L
   *
   *  "Clearly dependent" is not synonymous with "dependent" in the sense
   *  it is defined in `dependentParams`. Dependent parameters are handled
   *  in `updateEntry`. The idea of stripping off clearly dependent parameters
   *  and to handle them separately is for efficiency, so that type expressions
   *  used as bounds become smaller.
   *
   *  TODO: try to do without stripping? It would mean it is more efficient
   *  to pull out full bounds from a constraint.
   *
   *  @param isUpper   If true, `bound` is an upper bound, else a lower bound.
   */
  private def stripParams(
      tp: Type,
      todos: mutable.ListBuffer[(OrderingConstraint, TypeParamRef) => OrderingConstraint],
      isUpper: Boolean)(using Context): Type = tp match {
    case param: TypeParamRef if contains(param) =>
      todos += (if isUpper then order(_, _, param) else order(_, param, _))
      NoType
    case tp: TypeBounds =>
      val lo1 = stripParams(tp.lo, todos, !isUpper).orElse(defn.NothingType)
      val hi1 = stripParams(tp.hi, todos, isUpper).orElse(tp.topType)
      tp.derivedTypeBounds(lo1, hi1)
    case tp: AndType if isUpper =>
      val tp1 = stripParams(tp.tp1, todos, isUpper)
      val tp2 = stripParams(tp.tp2, todos, isUpper)
      if (tp1.exists)
        if (tp2.exists) tp.derivedAndType(tp1, tp2)
        else tp1
      else tp2
    case tp: OrType if !isUpper =>
      val tp1 = stripParams(tp.tp1, todos, isUpper)
      val tp2 = stripParams(tp.tp2, todos, isUpper)
      if (tp1.exists)
        if (tp2.exists) tp.derivedOrType(tp1, tp2)
        else tp1
      else tp2
    case _ =>
      tp
  }

  def add(poly: TypeLambda, tvars: List[TypeVar])(using Context): This = {
    assert(!contains(poly))
    val nparams = poly.paramNames.length
    val entries1 = new Array[Type](nparams * 2)
    poly.paramInfos.copyToArray(entries1, 0)
    tvars.copyToArray(entries1, nparams)
    newConstraint(boundsMap.updated(poly, entries1), lowerMap, upperMap).init(poly)
  }

  /** Split dependent parameters off the bounds for parameters in `poly`.
   *  Update all bounds to be normalized and update ordering to account for
   *  dependent parameters.
   */
  private def init(poly: TypeLambda)(using Context): This = {
    var current = this
    val todos = new mutable.ListBuffer[(OrderingConstraint, TypeParamRef) => OrderingConstraint]
    var i = 0
    val dropWildcards = AvoidWildcardsMap()
    while (i < poly.paramNames.length) {
      val param = poly.paramRefs(i)
      val bounds = dropWildcards(nonParamBounds(param))
      val stripped = stripParams(bounds, todos, isUpper = true)
      current = updateEntry(current, param, stripped)
      while todos.nonEmpty do
        current = todos.head(current, param)
        todos.dropInPlace(1)
      i += 1
    }
    current.checkNonCyclic()
  }

// ---------- Updates ------------------------------------------------------------

  /** If `inst` is a TypeBounds, make sure it does not contain toplevel
   *  references to `param` (see `Constraint#occursAtToplevel` for a definition
   *  of "toplevel").
   *  Any such references are replaced by `Nothing` in the lower bound and `Any`
   *  in the upper bound.
   *  References can be direct or indirect through instantiations of other
   *  parameters in the constraint.
   */
  private def ensureNonCyclic(param: TypeParamRef, inst: Type)(using Context): Type =

    def recur(tp: Type, fromBelow: Boolean): Type = tp match
      case tp: AndOrType =>
        val r1 = recur(tp.tp1, fromBelow)
        val r2 = recur(tp.tp2, fromBelow)
        if (r1 eq tp.tp1) && (r2 eq tp.tp2) then tp
        else tp.match
          case tp: OrType =>
            TypeComparer.lub(r1, r2, isSoft = tp.isSoft)
          case _ =>
            r1 & r2
      case tp: TypeParamRef =>
        if tp eq param then
          if fromBelow then defn.NothingType else defn.AnyType
        else entry(tp) match
          case NoType => tp
          case TypeBounds(lo, hi) => if lo eq hi then recur(lo, fromBelow) else tp
          case inst => recur(inst, fromBelow)
      case tp: TypeVar =>
        val underlying1 = recur(tp.underlying, fromBelow)
        if underlying1 ne tp.underlying then underlying1 else tp
      case tp: AnnotatedType =>
        val parent1 = recur(tp.parent, fromBelow)
        if parent1 ne tp.parent then tp.derivedAnnotatedType(parent1, tp.annot) else tp
      case _ =>
        val tp1 = tp.dealiasKeepAnnots
        if tp1 ne tp then
          val tp2 = recur(tp1, fromBelow)
          if tp2 ne tp1 then tp2 else tp
        else tp

    inst match
      case bounds: TypeBounds =>
        bounds.derivedTypeBounds(
          recur(bounds.lo, fromBelow = true),
          recur(bounds.hi, fromBelow = false))
      case _ =>
        inst
  end ensureNonCyclic

  /** Add the fact `param1 <: param2` to the constraint `current` and propagate
   *  `<:<` relationships between parameters ("edges") but not bounds.
   */
  def order(current: This, param1: TypeParamRef, param2: TypeParamRef, direction: UnificationDirection = NoUnification)(using Context): This =
    // /!\ Careful here: we're adding constraints on `current`, not `this`, so
    // think twice when using an instance method! We only need to pass `this` as
    // the `prev` argument in methods on `ConstraintLens`.
    // TODO: Refactor this code to take `prev` as a parameter and add
    // constraints on `this` instead?
    if param1 == param2 || current.isLess(param1, param2) then current
    else
      assert(current.contains(param1), i"$param1")
      assert(current.contains(param2), i"$param2")
      val unifying = direction != NoUnification
      val newUpper = {
        val up = current.exclusiveUpper(param2, param1)
        if unifying then
          // Since param2 <:< param1 already holds now, filter out param1 to avoid adding
          //   duplicated orderings.
          val filtered = up.filterNot(_ eq param1)
          // Only add bounds for param2 if it will be kept in the constraint after unification.
          if direction == KeepParam2 then
            param2 :: filtered
          else
            filtered
        else
          param2 :: up
      }
      val newLower = {
        val lower = current.exclusiveLower(param1, param2)
        if unifying then
          // Similarly, filter out param2 from lowerly-ordered parameters
          //   to avoid duplicated orderings.
          val filtered = lower.filterNot(_ eq param2)
          // Only add bounds for param1 if it will be kept in the constraint after unification.
          if direction == KeepParam1 then
            param1 :: filtered
          else
            filtered
        else
          param1 :: lower
      }
      val current1 = newLower.foldLeft(current)(upperLens.map(this, _, _, newUpper ::: _))
      val current2 = newUpper.foldLeft(current1)(lowerLens.map(this, _, _, newLower ::: _))
      current2
    end if
  end order

  /** The list of parameters P such that, for a fresh type parameter Q:
   *
   *    Q <: tp  implies  Q <: P      and isUpper = true, or
   *    tp <: Q  implies  P <: Q      and isUpper = false
   */
  private def dependentParams(tp: Type, isUpper: Boolean)(using Context): List[TypeParamRef] = tp match
    case param: TypeParamRef if contains(param) =>
      param :: (if (isUpper) upper(param) else lower(param))
    case tp: AndType if isUpper  =>
      dependentParams(tp.tp1, isUpper) | (dependentParams(tp.tp2, isUpper))
    case tp: OrType if !isUpper =>
      dependentParams(tp.tp1, isUpper).intersect(dependentParams(tp.tp2, isUpper))
    case EtaExpansion(tycon) =>
      dependentParams(tycon, isUpper)
    case _ =>
      Nil

  private def updateEntry(current: This, param: TypeParamRef, tp: Type)(using Context): This = {
    if Config.checkNoWildcardsInConstraint then assert(!tp.containsWildcardTypes)
    var current1 = boundsLens.update(this, current, param, tp)
    tp match {
      case TypeBounds(lo, hi) =>
        for p <- dependentParams(lo, isUpper = false) do
          current1 = order(current1, p, param)
        for p <- dependentParams(hi, isUpper = true) do
          current1 = order(current1, param, p)
      case _ =>
    }
    current1
  }

  /** The public version of `updateEntry`. Guarantees that there are no cycles */
  def updateEntry(param: TypeParamRef, tp: Type)(using Context): This =
    updateEntry(this, param, ensureNonCyclic(param, tp)).checkNonCyclic()

  def addLess(param1: TypeParamRef, param2: TypeParamRef, direction: UnificationDirection)(using Context): This =
    order(this, param1, param2, direction).checkNonCyclic()

// ---------- Replacements and Removals -------------------------------------

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain and replacing all top-level occurrences
   *  of the parameter elsewhere in the constraint by type `tp`.
   */
  def replace(param: TypeParamRef, tp: Type)(using Context): OrderingConstraint =
    val replacement = tp.stripTypeVar
    if param == replacement then this.checkNonCyclic()
    else
      assert(replacement.isValueTypeOrLambda)
      var current =
        if isRemovable(param.binder) then remove(param.binder)
        else updateEntry(this, param, replacement)

      def removeParam(ps: List[TypeParamRef]) = ps.filterConserve(param ne _)

      def replaceParam(tp: Type, atPoly: TypeLambda, atIdx: Int): Type =
        current.ensureNonCyclic(atPoly.paramRefs(atIdx), tp.substParam(param, replacement))

      current.foreachParam { (p, i) =>
        current = boundsLens.map(this, current, p, i, replaceParam(_, p, i))
        current = lowerLens.map(this, current, p, i, removeParam)
        current = upperLens.map(this, current, p, i, removeParam)
      }
      current.checkNonCyclic()
  end replace

  def remove(pt: TypeLambda)(using Context): This = {
    def removeFromOrdering(po: ParamOrdering) = {
      def removeFromBoundss(key: TypeLambda, bndss: Array[List[TypeParamRef]]): Array[List[TypeParamRef]] = {
        val bndss1 = bndss.map(_.filterConserve(_.binder ne pt))
        if (bndss.corresponds(bndss1)(_ eq _)) bndss else bndss1
      }
      po.remove(pt).mapValuesNow(removeFromBoundss)
    }
    newConstraint(boundsMap.remove(pt), removeFromOrdering(lowerMap), removeFromOrdering(upperMap))
      .checkNonCyclic()
  }

  def isRemovable(pt: TypeLambda): Boolean = {
    val entries = boundsMap(pt).nn
    @tailrec def allRemovable(last: Int): Boolean =
      if (last < 0) true
      else typeVar(entries, last) match {
        case tv: TypeVar => tv.inst.exists && allRemovable(last - 1)
        case _ => false
      }
    allRemovable(paramCount(entries) - 1)
  }

// ----------- Joins -----------------------------------------------------

  def hasConflictingTypeVarsFor(tl: TypeLambda, that: Constraint): Boolean =
    contains(tl) && that.contains(tl) &&
    // Since TypeVars are allocated in bulk for each type lambda, we only have
    // to check the first one to find out if some of them are different.
    (this.typeVarOfParam(tl.paramRefs(0)) ne that.typeVarOfParam(tl.paramRefs(0)))

  def subst(from: TypeLambda, to: TypeLambda)(using Context): OrderingConstraint =
    def swapKey[T](m: ArrayValuedMap[T]) = m.remove(from).updated(to, m(from).nn)
    var current = newConstraint(swapKey(boundsMap), swapKey(lowerMap), swapKey(upperMap))
    def subst[T <: Type](x: T): T = x.subst(from, to).asInstanceOf[T]
    current.foreachParam {(p, i) =>
      current = boundsLens.map(this, current, p, i, subst)
      current = lowerLens.map(this, current, p, i, _.map(subst))
      current = upperLens.map(this, current, p, i, _.map(subst))
    }
    constr.println(i"renamed $this to $current")
    current.checkNonCyclic()

  def instType(tvar: TypeVar): Type = entry(tvar.origin) match
    case _: TypeBounds => NoType
    case tp: TypeParamRef => typeVarOfParam(tp).orElse(tp)
    case tp => tp

  def ensureFresh(tl: TypeLambda)(using Context): TypeLambda =
    if (contains(tl)) {
      var paramInfos = tl.paramInfos
      if (tl.isInstanceOf[HKLambda]) {
        // HKLambdas are hash-consed, need to create an artificial difference by adding
        // a LazyRef to a bound.
        val TypeBounds(lo, hi) :: pinfos1 = tl.paramInfos: @unchecked
        paramInfos = TypeBounds(lo, LazyRef.of(hi)) :: pinfos1
      }
      ensureFresh(tl.newLikeThis(tl.paramNames, paramInfos, tl.resultType))
    }
    else tl

  def checkConsistentVars()(using Context): Unit =
    for param <- domainParams do
      typeVarOfParam(param) match
        case tvar: TypeVar =>
          assert(tvar.origin == param, i"mismatch $tvar, $param")
        case _ =>

// ---------- Exploration --------------------------------------------------------

  def domainLambdas: List[TypeLambda] = boundsMap.keys

  def domainParams: List[TypeParamRef] =
    for {
      (poly, entries) <- boundsMap.toList
      n <- 0 until paramCount(entries)
      if entries(n).exists
    }
    yield poly.paramRefs(n)

  def forallParams(p: TypeParamRef => Boolean): Boolean =
    boundsMap.forallBinding { (poly, entries) =>
      !0.until(paramCount(entries)).exists(i => isBounds(entries(i)) && !p(poly.paramRefs(i)))
    }

  def foreachParam(p: (TypeLambda, Int) => Unit): Unit =
    boundsMap.foreachBinding { (poly, entries) =>
      0.until(poly.paramNames.length).foreach(p(poly, _))
    }

  def foreachTypeVar(op: TypeVar => Unit): Unit =
    boundsMap.foreachBinding { (poly, entries) =>
      var i = 0
      val limit = paramCount(entries)
      while i < limit do
        typeVar(entries, i) match
          case tv: TypeVar if !tv.inst.exists => op(tv)
          case _ =>
        i += 1
    }

  private var myUninstVars: mutable.ArrayBuffer[TypeVar] | Null = _

  /** The uninstantiated typevars of this constraint */
  def uninstVars: collection.Seq[TypeVar] = {
    if (myUninstVars == null || myUninstVars.uncheckedNN.exists(_.inst.exists)) {
      myUninstVars = new mutable.ArrayBuffer[TypeVar]
      boundsMap.foreachBinding { (poly, entries) =>
        for (i <- 0 until paramCount(entries))
          typeVar(entries, i) match {
            case tv: TypeVar if !tv.inst.exists && isBounds(entries(i)) => myUninstVars.uncheckedNN += tv
            case _ =>
          }
      }
    }
    myUninstVars.uncheckedNN
  }

// ---------- Checking -----------------------------------------------

  def checkNonCyclic()(using Context): this.type =
    if Config.checkConstraintsNonCyclic then
      domainParams.foreach { param =>
        val inst = entry(param)
        assert(!isLess(param, param),
          s"cyclic ordering involving $param in ${this.show}, upper = $inst")
        assert(!occursAtToplevel(param, inst),
          s"cyclic bound for $param: ${inst.show} in ${this.show}")
      }
    this

  def occursAtToplevel(param: TypeParamRef, inst: Type)(using Context): Boolean =

    def occurs(tp: Type)(using Context): Boolean = tp match
      case tp: AndOrType =>
        occurs(tp.tp1) || occurs(tp.tp2)
      case tp: TypeParamRef =>
        (tp eq param) || entry(tp).match
          case NoType => false
          case TypeBounds(lo, hi) => (lo eq hi) && occurs(lo)
          case inst => occurs(inst)
      case tp: TypeVar =>
        occurs(tp.underlying)
      case TypeBounds(lo, hi) =>
        occurs(lo) || occurs(hi)
      case _ =>
        val tp1 = tp.dealias
        (tp1 ne tp) && occurs(tp1)

    occurs(inst)
  end occursAtToplevel

  override def checkClosed()(using Context): Unit =

    def isFreeTypeParamRef(tp: Type) = tp match
      case TypeParamRef(binder: TypeLambda, _) => !contains(binder)
      case _ => false

    def checkClosedType(tp: Type | Null, where: String) =
      if tp != null then
        assert(!tp.existsPart(isFreeTypeParamRef), i"unclosed constraint: $this refers to $tp in $where")

    boundsMap.foreachBinding((_, tps) => tps.foreach(checkClosedType(_, "bounds")))
    lowerMap.foreachBinding((_, paramss) => paramss.foreach(_.foreach(checkClosedType(_, "lower"))))
    upperMap.foreachBinding((_, paramss) => paramss.foreach(_.foreach(checkClosedType(_, "upper"))))
  end checkClosed

// ---------- Printing -----------------------------------------------------

  override def toText(printer: Printer): Text =
    printer.toText(this)

  override def toString: String = {
    def entryText(tp: Type): String = tp match {
      case tp: TypeBounds => tp.toString
      case _ => " := " + tp
    }
    val constrainedText =
      " constrained types = " + domainLambdas.mkString("\n")
    val boundsText =
      " bounds = " + {
        val assocs =
          for (param <- domainParams)
          yield
            s"${param.binder.paramNames(param.paramNum)}: ${entryText(entry(param))}"
        assocs.mkString("\n")
    }
    constrainedText + "\n" + boundsText
  }
}
