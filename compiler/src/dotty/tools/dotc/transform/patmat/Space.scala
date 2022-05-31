package dotty.tools
package dotc
package transform
package patmat

import core._
import Types._
import TypeUtils._
import Contexts._
import Flags._
import ast._
import Decorators._
import Symbols._
import StdNames._
import NameOps._
import Constants._
import typer._
import Applications._
import Inferencing._
import ProtoTypes._
import transform.SymUtils._
import reporting._
import config.Printers.{exhaustivity => debug}
import util.{SrcPos, NoSourcePosition}
import collection.mutable

/** Space logic for checking exhaustivity and unreachability of pattern matching
 *
 *  Space can be thought of as a set of possible values. A type or a pattern
 *  both refer to spaces. The space of a type is the values that inhabit the
 *  type. The space of a pattern is the values that can be covered by the
 *  pattern.
 *
 *  Space is recursively defined as follows:
 *
 *      1. `Empty` is a space
 *      2. For a type T, `Typ(T)` is a space
 *      3. A union of spaces `S1 | S2 | ...` is a space
 *      4. `Prod(S1, S2, ..., Sn)` is a product space.
 *
 *  For the problem of exhaustivity check, its formulation in terms of space is as follows:
 *
 *      Is the space Typ(T) a subspace of the union of space covered by all the patterns?
 *
 *  The problem of unreachable patterns can be formulated as follows:
 *
 *      Is the space covered by a pattern a subspace of the space covered by previous patterns?
 *
 *  Assumption:
 *    (1) One case class cannot be inherited directly or indirectly by another
 *        case class.
 *    (2) Inheritance of a case class cannot be well handled by the algorithm.
 *
 */


/** space definition */
sealed trait Space

/** Empty space */
case object Empty extends Space

/** Space representing the set of all values of a type
 *
 * @param tp: the type this space represents
 * @param decomposed: does the space result from decomposition? Used for pretty print
 *
 */
case class Typ(tp: Type, decomposed: Boolean = true) extends Space

/** Space representing an extractor pattern */
case class Prod(tp: Type, unappTp: TermRef, params: List[Space]) extends Space

/** Union of spaces */
case class Or(spaces: Seq[Space]) extends Space

/** abstract space logic */
trait SpaceLogic {
  /** Is `tp1` a subtype of `tp2`? */
  def isSubType(tp1: Type, tp2: Type): Boolean

  /** True if we can assume that the two unapply methods are the same.
   *  That is, given the same parameter, they return the same result.
   *
   *  We assume that unapply methods are pure, but the same method may
   *  be called with different prefixes, thus behaving differently.
   */
  def isSameUnapply(tp1: TermRef, tp2: TermRef): Boolean

  /** Return a space containing the values of both types.
   *
   * The types should be atomic (non-decomposable) and unrelated (neither
   * should be a subtype of the other).
   */
  def intersectUnrelatedAtomicTypes(tp1: Type, tp2: Type): Space

  /** Is the type `tp` decomposable? i.e. all values of the type can be covered
   *  by its decomposed types.
   *
   * Abstract sealed class, OrType, Boolean and Java enums can be decomposed.
   */
  def canDecompose(tp: Type): Boolean

  /** Return term parameter types of the extractor `unapp` */
  def signature(unapp: TermRef, scrutineeTp: Type, argLen: Int): List[Type]

  /** Get components of decomposable types */
  def decompose(tp: Type): List[Typ]

  /** Whether the extractor covers the given type */
  def covers(unapp: TermRef, scrutineeTp: Type, argLen: Int): Boolean

  /** Display space in string format */
  def show(sp: Space): String

  /** Simplify space such that a space equal to `Empty` becomes `Empty` */
  def simplify(space: Space)(using Context): Space = trace(s"simplify ${show(space)} --> ", debug, show)(space match {
    case Prod(tp, fun, spaces) =>
      val sps = spaces.map(simplify(_))
      if (sps.contains(Empty)) Empty
      else if (canDecompose(tp) && decompose(tp).isEmpty) Empty
      else Prod(tp, fun, sps)
    case Or(spaces) =>
      val spaces2 = spaces.map(simplify(_)).filter(_ != Empty)
      if spaces2.isEmpty then Empty
      else if spaces2.lengthCompare(1) == 0 then spaces2.head
      else Or(spaces2)
    case Typ(tp, _) =>
      if (canDecompose(tp) && decompose(tp).isEmpty) Empty
      else space
    case _ => space
  })

  /** Remove a space if it's a subspace of remaining spaces
   *
   *  Note: `dedup` will return the same result if the sequence >= 10
   */
  def dedup(spaces: Seq[Space])(using Context): Seq[Space] =
    if (spaces.lengthCompare(1) <= 0 || spaces.lengthCompare(10) >= 0) spaces
    else {
      val res = spaces.map(sp => (sp, spaces.filter(_ ne sp))).find {
        case (sp, sps) => isSubspace(sp, Or(LazyList(sps: _*)))
      }
      if (res.isEmpty) spaces
      else res.get._2
    }

  /** Flatten space to get rid of `Or` for pretty print */
  def flatten(space: Space)(using Context): Seq[Space] = space match {
    case Prod(tp, fun, spaces) =>
      val ss = LazyList(spaces: _*).map(flatten)

      ss.foldLeft(LazyList(Nil : List[Space])) { (acc, flat) =>
        for { sps <- acc; s <- flat }
        yield sps :+ s
      }.map { sps =>
        Prod(tp, fun, sps)
      }

    case Or(spaces) =>
      LazyList(spaces: _*).flatMap(flatten)

    case _ =>
      List(space)
  }

  /** Is `a` a subspace of `b`? Equivalent to `a - b == Empty`, but faster */
  def isSubspace(a: Space, b: Space)(using Context): Boolean = trace(s"isSubspace(${show(a)}, ${show(b)})", debug) {
    def tryDecompose1(tp: Type) = canDecompose(tp) && isSubspace(Or(decompose(tp)), b)
    def tryDecompose2(tp: Type) = canDecompose(tp) && isSubspace(a, Or(decompose(tp)))

    (simplify(a), simplify(b)) match {
      case (Empty, _) => true
      case (_, Empty) => false
      case (Or(ss), _) =>
        ss.forall(isSubspace(_, b))
      case (Typ(tp1, _), Typ(tp2, _)) =>
        isSubType(tp1, tp2)
        || canDecompose(tp1) && tryDecompose1(tp1)
        || canDecompose(tp2) && tryDecompose2(tp2)
      case (Typ(tp1, _), Or(ss)) =>  // optimization: don't go to subtraction too early
        ss.exists(isSubspace(a, _)) || tryDecompose1(tp1)
      case (_, Or(_)) =>
        simplify(minus(a, b)) == Empty
      case (Prod(tp1, _, _), Typ(tp2, _)) =>
        isSubType(tp1, tp2)
      case (Typ(tp1, _), Prod(tp2, fun, ss)) =>
        isSubType(tp1, tp2)
        && covers(fun, tp1, ss.length)
        && isSubspace(Prod(tp2, fun, signature(fun, tp2, ss.length).map(Typ(_, false))), b)
      case (Prod(_, fun1, ss1), Prod(_, fun2, ss2)) =>
        isSameUnapply(fun1, fun2) && ss1.zip(ss2).forall((isSubspace _).tupled)
    }
  }

  /** Intersection of two spaces  */
  def intersect(a: Space, b: Space)(using Context): Space = trace(s"${show(a)} & ${show(b)}", debug, show) {
    def tryDecompose1(tp: Type) = intersect(Or(decompose(tp)), b)
    def tryDecompose2(tp: Type) = intersect(a, Or(decompose(tp)))

    (a, b) match {
      case (Empty, _) | (_, Empty) => Empty
      case (_, Or(ss)) => Or(ss.map(intersect(a, _)).filter(_ ne Empty))
      case (Or(ss), _) => Or(ss.map(intersect(_, b)).filter(_ ne Empty))
      case (Typ(tp1, _), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) a
        else if (isSubType(tp2, tp1)) b
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else intersectUnrelatedAtomicTypes(tp1, tp2)
      case (Typ(tp1, _), Prod(tp2, fun, ss)) =>
        if (isSubType(tp2, tp1)) b
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else if (isSubType(tp1, tp2)) a // problematic corner case: inheriting a case class
        else intersectUnrelatedAtomicTypes(tp1, tp2) match
          case Typ(tp, _) => Prod(tp, fun, ss)
          case sp         => sp
      case (Prod(tp1, fun, ss), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) a
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else if (isSubType(tp2, tp1)) a  // problematic corner case: inheriting a case class
        else intersectUnrelatedAtomicTypes(tp1, tp2) match
          case Typ(tp, _) => Prod(tp, fun, ss)
          case sp         => sp
      case (Prod(tp1, fun1, ss1), Prod(tp2, fun2, ss2)) =>
        if (!isSameUnapply(fun1, fun2)) intersectUnrelatedAtomicTypes(tp1, tp2) match
          case Typ(tp, _) => Prod(tp, fun1, ss1)
          case sp         => sp
        else if (ss1.zip(ss2).exists(p => simplify(intersect(p._1, p._2)) == Empty)) Empty
        else Prod(tp1, fun1, ss1.zip(ss2).map((intersect _).tupled))
    }
  }

  /** The space of a not covered by b */
  def minus(a: Space, b: Space)(using Context): Space = trace(s"${show(a)} - ${show(b)}", debug, show) {
    def tryDecompose1(tp: Type) = minus(Or(decompose(tp)), b)
    def tryDecompose2(tp: Type) = minus(a, Or(decompose(tp)))

    (a, b) match {
      case (Empty, _) => Empty
      case (_, Empty) => a
      case (Typ(tp1, _), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) Empty
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else a
      case (Typ(tp1, _), Prod(tp2, fun, ss)) =>
        // rationale: every instance of `tp1` is covered by `tp2(_)`
        if isSubType(tp1, tp2) && covers(fun, tp1, ss.length) then
          minus(Prod(tp1, fun, signature(fun, tp1, ss.length).map(Typ(_, false))), b)
        else if canDecompose(tp1) then
          tryDecompose1(tp1)
        else
          a
      case (Or(ss), _) =>
        Or(ss.map(minus(_, b)))
      case (_, Or(ss)) =>
        ss.foldLeft(a)(minus)
      case (Prod(tp1, fun, ss), Typ(tp2, _)) =>
        // uncovered corner case: tp2 :< tp1, may happen when inheriting case class
        if (isSubType(tp1, tp2))
          Empty
        else if (simplify(a) == Empty)
           Empty
        else if (canDecompose(tp2))
          tryDecompose2(tp2)
        else
          a
      case (Prod(tp1, fun1, ss1), Prod(tp2, fun2, ss2)) =>
        if (!isSameUnapply(fun1, fun2)) return a
        if (fun1.symbol.name == nme.unapply && ss1.length != ss2.length) return a

        val range = (0 until ss1.size).toList
        val cache = Array.fill[Space | Null](ss2.length)(null)
        def sub(i: Int) =
          if cache(i) == null then
            cache(i) = minus(ss1(i), ss2(i))
          cache(i).nn
        end sub

        if range.exists(i => isSubspace(ss1(i), sub(i))) then a
        else if cache.forall(sub => isSubspace(sub.nn, Empty)) then Empty
        else
          // `(_, _, _) - (Some, None, _)` becomes `(None, _, _) | (_, Some, _) | (_, _, Empty)`
          val spaces = LazyList(range: _*).flatMap { i =>
            flatten(sub(i)).map(s => Prod(tp1, fun1, ss1.updated(i, s)))
          }
          Or(spaces)
    }
  }
}

object SpaceEngine {

  /** Is the unapply or unapplySeq irrefutable?
   *  @param  unapp   The unapply function reference
   */
  def isIrrefutable(unapp: TermRef, argLen: Int)(using Context): Boolean = {
    val unappResult = unapp.widen.finalResultType
    unappResult.isRef(defn.SomeClass)
    || unappResult <:< ConstantType(Constant(true)) // only for unapply
    || (unapp.symbol.is(Synthetic) && unapp.symbol.owner.linkedClass.is(Case))  // scala2 compatibility
    || unapplySeqTypeElemTp(unappResult).exists // only for unapplySeq
    || isProductMatch(unappResult, argLen)
    || {
      val isEmptyTp = extractorMemberType(unappResult, nme.isEmpty, NoSourcePosition)
      isEmptyTp <:< ConstantType(Constant(false))
    }
  }

  /** Is the unapply or unapplySeq irrefutable?
   *  @param  unapp   The unapply function tree
   */
  def isIrrefutable(unapp: tpd.Tree, argLen: Int)(using Context): Boolean = {
    tpd.funPart(unapp).tpe match
      case funRef: TermRef => isIrrefutable(funRef, argLen)
      case _: ErrorType => false
  }
}

/** Scala implementation of space logic */
class SpaceEngine(using Context) extends SpaceLogic {
  import tpd._

  private val scalaSeqFactoryClass = defn.SeqFactoryClass
  private val scalaListType        = defn.ListClass.typeRef
  private val scalaNilType         = defn.NilModule.termRef
  private val scalaConsType        = defn.ConsClass.typeRef

  private val constantNullType     = ConstantType(Constant(null))

  override def intersectUnrelatedAtomicTypes(tp1: Type, tp2: Type): Space = trace(s"atomic intersection: ${AndType(tp1, tp2).show}", debug) {
    // Precondition: !isSubType(tp1, tp2) && !isSubType(tp2, tp1).
    if !ctx.mode.is(Mode.SafeNulls) && (tp1.isNullType || tp2.isNullType) then
      // Since projections of types don't include null, intersection with null is empty.
      Empty
    else
      val intersection = Typ(AndType(tp1, tp2), decomposed = false)
      // unrelated numeric value classes can equal each other, so let's not consider type space intersection empty
      if tp1.classSymbol.isNumericValueClass && tp2.classSymbol.isNumericValueClass then intersection
      else if isPrimToBox(tp1, tp2) || isPrimToBox(tp2, tp1) then intersection
      else if TypeComparer.provablyDisjoint(tp1, tp2) then Empty
      else intersection
  }

  /** Return the space that represents the pattern `pat` */
  def project(pat: Tree): Space = pat match {
    case Literal(c) =>
      if (c.value.isInstanceOf[Symbol])
        Typ(c.value.asInstanceOf[Symbol].termRef, decomposed = false)
      else
        Typ(ConstantType(c), decomposed = false)

    case pat: Ident if isBackquoted(pat) =>
      Typ(pat.tpe, decomposed = false)

    case Ident(_) | Select(_, _) =>
      Typ(erase(pat.tpe.stripAnnots.widenSkolem, isValue = true), decomposed = false)

    case Alternative(trees) =>
      Or(trees.map(project(_)))

    case Bind(_, pat) =>
      project(pat)

    case SeqLiteral(pats, _) =>
      projectSeq(pats)

    case UnApply(fun, _, pats) =>
      val fun1 = funPart(fun)
      val funRef = fun1.tpe.asInstanceOf[TermRef]
      if (fun.symbol.name == nme.unapplySeq)
        val (arity, elemTp, resultTp) = unapplySeqInfo(fun.tpe.widen.finalResultType, fun.srcPos)
        if (fun.symbol.owner == scalaSeqFactoryClass && scalaListType.appliedTo(elemTp) <:< pat.tpe)
          // The exhaustivity and reachability logic already handles decomposing sum types (into its subclasses)
          // and product types (into its components).  To get better counter-examples for patterns that are of type
          // List (or a super-type of list, like LinearSeq) we project them into spaces that use `::` and Nil.
          // Doing so with a pattern of `case Seq() =>` with a scrutinee of type `Vector()` doesn't work because the
          // space is then discarded leading to a false positive reachability warning, see #13931.
          projectSeq(pats)
        else {
          if (elemTp.exists)
            Prod(erase(pat.tpe.stripAnnots, isValue = false), funRef, projectSeq(pats) :: Nil)
          else
            Prod(erase(pat.tpe.stripAnnots, isValue = false), funRef, pats.take(arity - 1).map(project) :+ projectSeq(pats.drop(arity - 1)))
        }
      else
        Prod(erase(pat.tpe.stripAnnots, isValue = false), funRef, pats.map(project))

    case Typed(pat @ UnApply(_, _, _), _) =>
      project(pat)

    case Typed(_, tpt) =>
      Typ(erase(tpt.tpe.stripAnnots, isValue = true), decomposed = false)

    case This(_) =>
      Typ(pat.tpe.stripAnnots, decomposed = false)

    case EmptyTree =>         // default rethrow clause of try/catch, check tests/patmat/try2.scala
      Typ(WildcardType, decomposed = false)

    case Block(Nil, expr) =>
      project(expr)

    case _ =>
      // Pattern is an arbitrary expression; assume a skolem (i.e. an unknown value) of the pattern type
      Typ(pat.tpe.narrow, decomposed = false)
  }

  private def project(tp: Type): Space = tp match {
    case OrType(tp1, tp2) => Or(project(tp1) :: project(tp2) :: Nil)
    case tp => Typ(tp, decomposed = true)
  }

  private def unapplySeqInfo(resTp: Type, pos: SrcPos): (Int, Type, Type) = {
    var resultTp = resTp
    var elemTp = unapplySeqTypeElemTp(resultTp)
    var arity = productArity(resultTp, pos)
    if (!elemTp.exists && arity <= 0) {
      resultTp = resTp.select(nme.get).finalResultType
      elemTp = unapplySeqTypeElemTp(resultTp.widen)
      arity = productSelectorTypes(resultTp, pos).size
    }
    (arity, elemTp, resultTp)
  }

  /** Erase pattern bound types with WildcardType
   *
   *  For example, the type `C[T$1]` should match any `C[?]`, thus
   *  `v` should be `WildcardType` instead of `T$1`:
   *
   *     sealed trait B
   *     case class C[T](v: T) extends B
   *     (b: B) match {
   *        case C(v) =>      //    case C.unapply[T$1 @ T$1](v @ _):C[T$1]
   *     }
   *
   *  However, we cannot use WildcardType for Array[?], due to that
   *  `Array[WildcardType] <: Array[Array[WildcardType]]`, which may
   *  cause false unreachable warnings. See tests/patmat/t2425.scala
   *
   *  We cannot use type erasure here, as it would lose the constraints
   *  involving GADTs. For example, in the following code, type
   *  erasure would loose the constraint that `x` and `y` must be
   *  the same type, resulting in false inexhaustive warnings:
   *
   *     sealed trait Expr[T]
   *     case class IntExpr(x: Int) extends Expr[Int]
   *     case class BooleanExpr(b: Boolean) extends Expr[Boolean]
   *
   *     def foo[T](x: Expr[T], y: Expr[T]) = (x, y) match {
   *       case (IntExpr(_), IntExpr(_)) =>
   *       case (BooleanExpr(_), BooleanExpr(_)) =>
   *     }
   *
   *  @param inArray whether `tp` is a type argument to `Array`
   *  @param isValue whether `tp` is the type which match against values
   *
   *  If `isValue` is true, then pattern-bound symbols are erased to its upper bound.
   *  This is needed to avoid spurious unreachable warnings. See tests/patmat/i6197.scala.
   */
  private def erase(tp: Type, inArray: Boolean = false, isValue: Boolean = false): Type = trace(i"$tp erased to", debug) {

    tp match {
      case tp @ AppliedType(tycon, args) =>
        if tycon.typeSymbol.isPatternBound then return WildcardType

        val args2 =
          if (tycon.isRef(defn.ArrayClass)) args.map(arg => erase(arg, inArray = true, isValue = false))
          else args.map(arg => erase(arg, inArray = false, isValue = false))
        tp.derivedAppliedType(erase(tycon, inArray, isValue = false), args2)

      case tp @ OrType(tp1, tp2) =>
        OrType(erase(tp1, inArray, isValue), erase(tp2, inArray, isValue), tp.isSoft)

      case AndType(tp1, tp2) =>
        AndType(erase(tp1, inArray, isValue), erase(tp2, inArray, isValue))

      case tp @ RefinedType(parent, _, _) =>
        erase(parent, inArray, isValue)

      case tref: TypeRef if tref.symbol.isPatternBound =>
        if inArray then tref.underlying
        else if isValue then tref.superType
        else WildcardType

      case _ => tp
    }
  }

  /** Space of the pattern: unapplySeq(a, b, c: _*)
   */
  def projectSeq(pats: List[Tree]): Space = {
    if (pats.isEmpty) return Typ(scalaNilType, false)

    val (items, zero) = if (isWildcardStarArg(pats.last))
      (pats.init, Typ(scalaListType.appliedTo(pats.last.tpe.elemType), false))
    else
      (pats, Typ(scalaNilType, false))

    val unapplyTp = scalaConsType.classSymbol.companionModule.termRef.select(nme.unapply)
    items.foldRight[Space](zero) { (pat, acc) =>
      val consTp = scalaConsType.appliedTo(pats.head.tpe.widen)
      Prod(consTp, unapplyTp, project(pat) :: acc :: Nil)
    }
  }

  def isPrimToBox(tp: Type, pt: Type): Boolean =
    tp.isPrimitiveValueType && (defn.boxedType(tp).classSymbol eq pt.classSymbol)

  private val isSubspaceCache = mutable.HashMap.empty[(Space, Space, Context), Boolean]

  override def isSubspace(a: Space, b: Space)(using Context): Boolean =
    isSubspaceCache.getOrElseUpdate((a, b, ctx), super.isSubspace(a, b))

  /** Is `tp1` a subtype of `tp2`?  */
  def isSubType(tp1: Type, tp2: Type): Boolean = trace(i"$tp1 <:< $tp2", debug, show = true) {
    if tp1 == constantNullType && !ctx.mode.is(Mode.SafeNulls)
    then tp2 == constantNullType
    else tp1 <:< tp2
  }

  def isSameUnapply(tp1: TermRef, tp2: TermRef): Boolean =
    // always assume two TypeTest[S, T].unapply are the same if they are equal in types
    (tp1.prefix.isStable && tp2.prefix.isStable || tp1.symbol == defn.TypeTest_unapply)
    && tp1 =:= tp2

  /** Parameter types of the case class type `tp`. Adapted from `unapplyPlan` in patternMatcher  */
  def signature(unapp: TermRef, scrutineeTp: Type, argLen: Int): List[Type] = {
    val unappSym = unapp.symbol

    // println("scrutineeTp = " + scrutineeTp.show)

    val mt: MethodType = unapp.widen match {
      case mt: MethodType => mt
      case pt: PolyType   =>
        inContext(ctx.fresh.setExploreTyperState()) {
          val tvars = pt.paramInfos.map(newTypeVar(_))
          val mt = pt.instantiate(tvars).asInstanceOf[MethodType]
          scrutineeTp <:< mt.paramInfos(0)
          // force type inference to infer a narrower type: could be singleton
          // see tests/patmat/i4227.scala
          mt.paramInfos(0) <:< scrutineeTp
          isFullyDefined(mt, ForceDegree.all)
          mt
        }
    }

    // Case unapply:
    // 1. return types of constructor fields if the extractor is synthesized for Scala2 case classes & length match
    // 2. return Nil if unapply returns Boolean  (boolean pattern)
    // 3. return product selector types if unapply returns a product type (product pattern)
    // 4. return product selectors of `T` where `def get: T` is a member of the return type of unapply & length match (named-based pattern)
    // 5. otherwise, return `T` where `def get: T` is a member of the return type of unapply
    //
    // Case unapplySeq:
    // 1. return the type `List[T]` where `T` is the element type of the unapplySeq return type `Seq[T]`

    val resTp = ctx.typeAssigner.safeSubstMethodParams(mt, scrutineeTp :: Nil).finalResultType

    val sig =
      if (resTp.isRef(defn.BooleanClass))
        List()
      else {
        val isUnapplySeq = unappSym.name == nme.unapplySeq

        if (isUnapplySeq) {
          val (arity, elemTp, resultTp) = unapplySeqInfo(resTp, unappSym.srcPos)
          if (elemTp.exists) scalaListType.appliedTo(elemTp) :: Nil
          else {
            val sels = productSeqSelectors(resultTp, arity, unappSym.srcPos)
            sels.init :+ scalaListType.appliedTo(sels.last)
          }
        }
        else {
          val arity = productArity(resTp, unappSym.srcPos)
          if (arity > 0)
            productSelectorTypes(resTp, unappSym.srcPos)
          else {
            val getTp = resTp.select(nme.get).finalResultType.widenTermRefExpr
            if (argLen == 1) getTp :: Nil
            else productSelectorTypes(getTp, unappSym.srcPos)
          }
        }
      }

    debug.println(s"signature of ${unappSym.showFullName} ----> ${sig.map(_.show).mkString(", ")}")

    sig.map(_.annotatedToRepeated)
  }

  /** Whether the extractor covers the given type */
  def covers(unapp: TermRef, scrutineeTp: Type, argLen: Int): Boolean =
    SpaceEngine.isIrrefutable(unapp, argLen) || unapp.symbol == defn.TypeTest_unapply && {
      val AppliedType(_, _ :: tp :: Nil) = unapp.prefix.widen.dealias: @unchecked
      scrutineeTp <:< tp
    }

  /** Decompose a type into subspaces -- assume the type can be decomposed */
  def decompose(tp: Type): List[Typ] =
    tp.dealias match {
      case AndType(tp1, tp2) =>
        def decomposeComponent(tpA: Type, tpB: Type): List[Typ] =
          decompose(tpA).flatMap {
            case Typ(tp, _) =>
              if tp <:< tpB then
                Typ(tp, decomposed = true) :: Nil
              else if tpB <:< tp then
                Typ(tpB, decomposed = true) :: Nil
              else if TypeComparer.provablyDisjoint(tp, tpB) then
                Nil
              else
                Typ(AndType(tp, tpB), decomposed = true) :: Nil
          }

        if canDecompose(tp1) then
          decomposeComponent(tp1, tp2)
        else
          decomposeComponent(tp2, tp1)

      case OrType(tp1, tp2) => List(Typ(tp1, true), Typ(tp2, true))
      case tp if tp.isRef(defn.BooleanClass) =>
        List(
          Typ(ConstantType(Constant(true)), true),
          Typ(ConstantType(Constant(false)), true)
        )
      case tp if tp.isRef(defn.UnitClass) =>
        Typ(ConstantType(Constant(())), true) :: Nil
      case tp if tp.classSymbol.isAllOf(JavaEnumTrait) =>
        tp.classSymbol.children.map(sym => Typ(sym.termRef, true))
      case tp =>
        def getChildren(sym: Symbol): List[Symbol] =
          sym.children.flatMap { child =>
            if child eq sym then Nil // i3145: sealed trait Baz, val x = new Baz {}, Baz.children returns Baz...
            else if tp.classSymbol == defn.TupleClass || tp.classSymbol == defn.NonEmptyTupleClass then
              List(child) // TupleN and TupleXXL classes are used for Tuple, but they aren't Tuple's children
            else if (child.is(Private) || child.is(Sealed)) && child.isOneOf(AbstractOrTrait) then getChildren(child)
            else List(child)
          }
        val children = getChildren(tp.classSymbol)
        debug.println(s"candidates for ${tp.show} : [${children.map(_.show).mkString(", ")}]")

        val parts = children.map { sym =>
          val sym1 = if (sym.is(ModuleClass)) sym.sourceModule else sym
          val refined = TypeOps.refineUsingParent(tp, sym1)

          debug.println(sym1.show + " refined to " + refined.show)

          def inhabited(tp: Type): Boolean =
            tp.dealias match {
              case AndType(tp1, tp2) => !TypeComparer.provablyDisjoint(tp1, tp2)
              case OrType(tp1, tp2) => inhabited(tp1) || inhabited(tp2)
              case tp: RefinedType => inhabited(tp.parent)
              case tp: TypeRef => inhabited(tp.prefix)
              case _ => true
            }

          if (inhabited(refined)) refined
          else NoType
        } filter(_.exists)

        debug.println(s"${tp.show} decomposes to [${parts.map(_.show).mkString(", ")}]")

        parts.map(Typ(_, true))
    }

  /** Abstract sealed types, or-types, Boolean and Java enums can be decomposed */
  def canDecompose(tp: Type): Boolean =
    val underlying = tp.dealias
    val res = underlying match
      case _: SingletonType => false
      case _: OrType => true
      case and: AndType => canDecompose(and.tp1) || canDecompose(and.tp2)
      case _ =>
        val cls = underlying.classSymbol
        cls.is(Sealed)
        && cls.isOneOf(AbstractOrTrait)
        && !cls.hasAnonymousChild
        && cls.children.nonEmpty
        || cls.isAllOf(JavaEnumTrait)
        || underlying.isRef(defn.BooleanClass)
        || underlying.isRef(defn.UnitClass)
    //debug.println(s"decomposable: ${tp.show} = $res")
    res

  /** Show friendly type name with current scope in mind
   *
   *  E.g.    C.this.B     -->  B     if current owner is C
   *          C.this.x.T   -->  x.T   if current owner is C
   *          X[T]         -->  X
   *          C            -->  C     if current owner is C !!!
   *
   */
  def showType(tp: Type, showTypeArgs: Boolean = false): String = {
    val enclosingCls = ctx.owner.enclosingClass

    def isOmittable(sym: Symbol) =
      sym.isEffectiveRoot || sym.isAnonymousClass || sym.name.isReplWrapperName ||
        ctx.definitions.unqualifiedOwnerTypes.exists(_.symbol == sym) ||
        sym.showFullName.startsWith("scala.") ||
        sym == enclosingCls || sym == enclosingCls.sourceModule

    def refinePrefix(tp: Type): String = tp match {
      case NoPrefix => ""
      case tp: NamedType if isOmittable(tp.symbol) => ""
      case tp: ThisType => refinePrefix(tp.tref)
      case tp: RefinedType => refinePrefix(tp.parent)
      case tp: NamedType => tp.name.show.stripSuffix("$")
      case tp: TypeVar => refinePrefix(tp.instanceOpt)
      case _ => tp.show
    }

    def refine(tp: Type): String = tp.stripped match {
      case tp: RefinedType => refine(tp.parent)
      case tp: AppliedType =>
        refine(tp.typeConstructor) + (
          if (showTypeArgs) tp.argInfos.map(refine).mkString("[", ",", "]")
          else ""
        )
      case tp: ThisType => refine(tp.tref)
      case tp: NamedType =>
        val pre = refinePrefix(tp.prefix)
        if (tp.name == tpnme.higherKinds) pre
        else if (pre.isEmpty) tp.name.show.stripSuffix("$")
        else pre + "." + tp.name.show.stripSuffix("$")
      case tp: OrType => refine(tp.tp1) + " | " + refine(tp.tp2)
      case _: TypeBounds => "_"
      case _ => tp.show.stripSuffix("$")
    }

    refine(tp)
  }

  /** Whether the counterexample is satisfiable. The space is flattened and non-empty. */
  def satisfiable(sp: Space): Boolean = {
    def impossible: Nothing = throw new AssertionError("`satisfiable` only accepts flattened space.")

    def genConstraint(space: Space): List[(Type, Type)] = space match {
      case Prod(tp, unappTp, ss) =>
        val tps = signature(unappTp, tp, ss.length)
        ss.zip(tps).flatMap {
          case (sp : Prod, tp) => sp.tp -> tp :: genConstraint(sp)
          case (Typ(tp1, _), tp2) => tp1 -> tp2 :: Nil
          case _ => impossible
        }
      case Typ(_, _) => Nil
      case _ => impossible
    }

    def checkConstraint(constrs: List[(Type, Type)])(using Context): Boolean = {
      val tvarMap = collection.mutable.Map.empty[Symbol, TypeVar]
      val typeParamMap = new TypeMap() {
        override def apply(tp: Type): Type = tp match {
          case tref: TypeRef if tref.symbol.is(TypeParam) =>
            tvarMap.getOrElseUpdate(tref.symbol, newTypeVar(tref.underlying.bounds))
          case tp => mapOver(tp)
        }
      }

      constrs.forall { case (tp1, tp2) => typeParamMap(tp1) <:< typeParamMap(tp2) }
    }

    checkConstraint(genConstraint(sp))(using ctx.fresh.setNewTyperState())
  }

  def show(ss: Seq[Space]): String = ss.map(show).mkString(", ")

  /** Display spaces */
  def show(s: Space): String = {
    def params(tp: Type): List[Type] = tp.classSymbol.primaryConstructor.info.firstParamTypes

    /** does the companion object of the given symbol have custom unapply */
    def hasCustomUnapply(sym: Symbol): Boolean = {
      val companion = sym.companionModule
      companion.findMember(nme.unapply, NoPrefix, required = EmptyFlags, excluded = Synthetic).exists ||
        companion.findMember(nme.unapplySeq, NoPrefix, required = EmptyFlags, excluded = Synthetic).exists
    }

    def doShow(s: Space, flattenList: Boolean = false): String = s match {
      case Empty => "empty"
      case Typ(c: ConstantType, _) => "" + c.value.value
      case Typ(tp: TermRef, _) =>
        if (flattenList && tp <:< scalaNilType) ""
        else tp.symbol.showName
      case Typ(tp, decomposed) =>

        val sym = tp.classSymbol

        if (ctx.definitions.isTupleNType(tp))
          params(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (scalaListType.isRef(sym))
          if (flattenList) "_*" else "_: List"
        else if (scalaConsType.isRef(sym))
          if (flattenList) "_, _*"  else "List(_, _*)"
        else if (tp.classSymbol.is(Sealed) && tp.classSymbol.hasAnonymousChild)
          "_: " + showType(tp) + " (anonymous)"
        else if (tp.classSymbol.is(CaseClass) && !hasCustomUnapply(tp.classSymbol))
        // use constructor syntax for case class
          showType(tp) + params(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (decomposed) "_: " + showType(tp, showTypeArgs = true)
        else "_"
      case Prod(tp, fun, params) =>
        if (ctx.definitions.isTupleNType(tp))
          "(" + params.map(doShow(_)).mkString(", ") + ")"
        else if (tp.isRef(scalaConsType.symbol))
          if (flattenList) params.map(doShow(_, flattenList)).filter(_.nonEmpty).mkString(", ")
          else params.map(doShow(_, flattenList = true)).filter(!_.isEmpty).mkString("List(", ", ", ")")
        else {
          val sym = fun.symbol
          val isUnapplySeq = sym.name.eq(nme.unapplySeq)
          val paramsStr = params.map(doShow(_, flattenList = isUnapplySeq)).mkString("(", ", ", ")")
          showType(fun.prefix) + paramsStr
        }
      case Or(ss) =>
        ss.map(doShow(_, flattenList)).mkString(" | ")
    }

    doShow(s, flattenList = false)
  }

  private def exhaustivityCheckable(sel: Tree): Boolean = {
    val seen = collection.mutable.Set.empty[Type]

    // Possible to check everything, but be compatible with scalac by default
    def isCheckable(tp: Type): Boolean =
      val tpw = tp.widen.dealias
      val classSym = tpw.classSymbol
      classSym.is(Sealed) ||
      tpw.isInstanceOf[OrType] ||
      (tpw.isInstanceOf[AndType] && {
        val and = tpw.asInstanceOf[AndType]
        isCheckable(and.tp1) || isCheckable(and.tp2)
      }) ||
      tpw.isRef(defn.BooleanClass) ||
      classSym.isAllOf(JavaEnumTrait) ||
      classSym.is(Case) && {
        if seen.add(tpw) then productSelectorTypes(tpw, sel.srcPos).exists(isCheckable(_))
        else true // recursive case class: return true and other members can still fail the check
      }

    val res = !sel.tpe.hasAnnotation(defn.UncheckedAnnot) && {
      ctx.settings.YcheckAllPatmat.value
      || isCheckable(sel.tpe)
    }

    debug.println(s"exhaustivity checkable: ${sel.show} = $res")
    res
  }

  /** Whehter counter-examples should be further checked? True for GADTs. */
  private def shouldCheckExamples(tp: Type): Boolean =
    new TypeAccumulator[Boolean] {
      override def apply(b: Boolean, tp: Type): Boolean = tp match {
        case tref: TypeRef if tref.symbol.is(TypeParam) && variance != 1 => true
        case tp => b || foldOver(b, tp)
      }
    }.apply(false, tp)

  /** Return the underlying type of non-module, non-constant, non-enum case singleton types.
   *  Also widen ExprType to its result type, and rewrap any annotation wrappers.
   *  For example, with `val opt = None`, widen `opt.type` to `None.type`. */
  def toUnderlying(tp: Type): Type = trace(i"toUnderlying($tp)", show = true)(tp match {
    case _: ConstantType                            => tp
    case tp: TermRef if tp.symbol.is(Module)        => tp
    case tp: TermRef if tp.symbol.isAllOf(EnumCase) => tp
    case tp: SingletonType                          => toUnderlying(tp.underlying)
    case tp: ExprType                               => toUnderlying(tp.resultType)
    case AnnotatedType(tp, annot)                   => AnnotatedType(toUnderlying(tp), annot)
    case _                                          => tp
  })

  def checkExhaustivity(_match: Match): Unit = {
    val Match(sel, cases) = _match
    debug.println(i"checking exhaustivity of ${_match}")

    if (!exhaustivityCheckable(sel)) return

    val selTyp = toUnderlying(sel.tpe).dealias
    debug.println(i"selTyp = $selTyp")

    val patternSpace = Or(cases.foldLeft(List.empty[Space]) { (acc, x) =>
      val space = if (x.guard.isEmpty) project(x.pat) else Empty
      debug.println(s"${x.pat.show} ====> ${show(space)}")
      space :: acc
    })

    val checkGADTSAT = shouldCheckExamples(selTyp)

    val uncovered =
      flatten(simplify(minus(project(selTyp), patternSpace))).filter({ s =>
        s != Empty && (!checkGADTSAT || satisfiable(s))
      })


    if uncovered.nonEmpty then
      val hasMore = uncovered.lengthCompare(6) > 0
      val deduped = dedup(uncovered.take(6))
      report.warning(PatternMatchExhaustivity(show(deduped), hasMore), sel.srcPos)
  }

  private def redundancyCheckable(sel: Tree): Boolean =
    // Ignore Expr[T] and Type[T] for unreachability as a special case.
    // Quote patterns produce repeated calls to the same unapply method, but with different implicit parameters.
    // Since we assume that repeated calls to the same unapply method overlap
    // and implicit parameters cannot normally differ between two patterns in one `match`,
    // the easiest solution is just to ignore Expr[T] and Type[T].
    !sel.tpe.hasAnnotation(defn.UncheckedAnnot)
    && !sel.tpe.widen.isRef(defn.QuotedExprClass)
    && !sel.tpe.widen.isRef(defn.QuotedTypeClass)

  def checkRedundancy(_match: Match): Unit = {
    val Match(sel, _) = _match
    val cases = _match.cases.toIndexedSeq
    debug.println(i"checking redundancy in $_match")

    if (!redundancyCheckable(sel)) return

    val selTyp = toUnderlying(sel.tpe).dealias
    debug.println(i"selTyp = $selTyp")

    val isNullable = selTyp.classSymbol.isNullableClass
    val targetSpace = if isNullable
      then project(OrType(selTyp, constantNullType, soft = false))
      else project(selTyp)
    debug.println(s"targetSpace: ${show(targetSpace)}")

    var i        = 0
    val len      = cases.length
    var prevs    = List.empty[Space]
    var deferred = List.empty[Tree]

    while (i < len) {
      val CaseDef(pat, guard, _) = cases(i)

      debug.println(i"case pattern: $pat")

      val curr = project(pat)
      debug.println(i"reachable? ${show(curr)}")

      val prev = simplify(Or(prevs))
      debug.println(s"prev: ${show(prev)}")

      val covered = simplify(intersect(curr, targetSpace))
      debug.println(s"covered: ${show(covered)}")

      if prev == Empty && covered == Empty then // defer until a case is reachable
        deferred ::= pat
      else {
        for (pat <- deferred.reverseIterator)
          report.warning(MatchCaseUnreachable(), pat.srcPos)
        if pat != EmptyTree // rethrow case of catch uses EmptyTree
            && isSubspace(covered, prev)
        then {
          val nullOnly = isNullable && i == len - 1 && isWildcardArg(pat)
          val msg = if nullOnly then MatchCaseOnlyNullWarning() else MatchCaseUnreachable()
          report.warning(msg, pat.srcPos)
        }
        deferred = Nil
      }

      // in redundancy check, take guard as false in order to soundly approximate
      prevs ::= (if guard.isEmpty then covered else Empty)
      i += 1
    }
  }
}
