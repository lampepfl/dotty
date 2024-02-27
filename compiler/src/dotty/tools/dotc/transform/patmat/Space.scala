package dotty.tools
package dotc
package transform
package patmat

import core.*, Constants.*, Contexts.*, Decorators.*, Flags.*, Names.*, NameOps.*, StdNames.*, Symbols.*, Types.*
import ast.*, tpd.*
import config.Printers.*
import printing.{ Printer, * }, Texts.*
import reporting.*
import typer.*, Applications.*, Inferencing.*, ProtoTypes.*
import util.*

import scala.annotation.internal.sharable
import scala.collection.mutable

import SpaceEngine.*

/* Space logic for checking exhaustivity and unreachability of pattern matching
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
sealed trait Space extends Showable:

  @sharable private val isSubspaceCache = mutable.HashMap.empty[Space, Boolean]

  def isSubspace(b: Space)(using Context): Boolean =
    val a = this
    val a2 = a.simplify
    val b2 = b.simplify
    if (a ne a2) || (b ne b2) then a2.isSubspace(b2)
    else if a == Empty then true
    else if b == Empty then false
    else isSubspaceCache.getOrElseUpdate(b, computeIsSubspace(a, b))

  @sharable private var mySimplified: Space | Null = null

  def simplify(using Context): Space =
    val simplified = mySimplified
    if simplified == null then
      val simplified = SpaceEngine.computeSimplify(this)
      mySimplified = simplified
      simplified
    else simplified

  def toText(p: Printer): Text = inContext(p.printerContext)(this match {
    case Empty    => s"Empty"
    case sp: Typ  => s"Typ(${display(sp)})"
    case sp: Prod => s"Prod(${display(sp)})"
    case sp: Or   => s"Or(${display(sp)})"
  })
end Space

/** Empty space */
case object Empty extends Space

/** Space representing the set of all values of a type
 *
 * @param tp: the type this space represents
 * @param decomposed: does the space result from decomposition? Used for pretty print
 *
 */
case class Typ(tp: Type, decomposed: Boolean = true) extends Space:
  private var myDecompose: List[Typ] | Null = null

  def canDecompose(using Context): Boolean = decompose != ListOfTypNoType

  def decompose(using Context): List[Typ] =
    val decompose = myDecompose
    if decompose == null then
      val decompose = tp match
        case Parts(parts) => parts.map(Typ(_, decomposed = true))
        case _            => ListOfTypNoType
      myDecompose = decompose
      decompose
    else decompose
end Typ

/** Space representing an extractor pattern */
case class Prod(tp: Type, unappTp: TermRef, params: List[Space]) extends Space

/** Union of spaces */
case class Or(spaces: Seq[Space]) extends Space

object SpaceEngine {
  def simplify(space: Space)(using Context): Space           = space.simplify
  def isSubspace(a: Space, b: Space)(using Context): Boolean = a.isSubspace(b)
  def canDecompose(typ: Typ)(using Context): Boolean         = typ.canDecompose
  def decompose(typ: Typ)(using Context): List[Typ]          = typ.decompose

  /** Simplify space such that a space equal to `Empty` becomes `Empty` */
  def computeSimplify(space: Space)(using Context): Space = trace(i"simplify($space)")(space match {
    case Prod(tp, fun, spaces) =>
      val sps = spaces.mapconserve(simplify)
      if sps.contains(Empty) then Empty
      else if decompose(tp).isEmpty then Empty
      else if sps eq spaces then space else Prod(tp, fun, sps)
    case Or(spaces) =>
      val spaces2 = spaces.map(simplify).filter(_ != Empty)
      if spaces2.isEmpty then Empty
      else if spaces2.lengthIs == 1 then spaces2.head
      else if spaces2.corresponds(spaces)(_ eq _) then space else Or(spaces2)
    case typ: Typ =>
      if decompose(typ).isEmpty then Empty
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
        case (sp, sps) => isSubspace(sp, Or(LazyList(sps*)))
      }
      if (res.isEmpty) spaces
      else res.get._2
    }

  /** Flatten space to get rid of `Or` for pretty print */
  def flatten(space: Space)(using Context): Seq[Space] = space match {
    case Prod(tp, fun, spaces) =>
      val ss = LazyList(spaces*).map(flatten)

      ss.foldLeft(LazyList(Nil : List[Space])) { (acc, flat) =>
        for { sps <- acc; s <- flat }
        yield sps :+ s
      }.map { sps =>
        Prod(tp, fun, sps)
      }

    case Or(spaces) =>
      LazyList(spaces*).flatMap(flatten)

    case _ =>
      List(space)
  }

  /** Is `a` a subspace of `b`? Equivalent to `simplify(simplify(a) - simplify(b)) == Empty`, but faster */
  def computeIsSubspace(a: Space, b: Space)(using Context): Boolean = trace(i"isSubspace($a, $b)") {
    val a2 = simplify(a)
    val b2 = simplify(b)
    if (a ne a2) || (b ne b2) then isSubspace(a2, b2)
    else (a, b) match {
      case (Empty, _) => true
      case (_, Empty) => false
      case (Or(ss), _) => ss.forall(isSubspace(_, b))
      case (a @ Typ(tp1, _), Or(ss)) =>  // optimization: don't go to subtraction too early
        ss.exists(isSubspace(a, _))
        || canDecompose(a) && isSubspace(Or(decompose(a)), b)
      case (_, Or(_))  => simplify(minus(a, b)) == Empty
      case (a @ Typ(tp1, _), b @ Typ(tp2, _)) =>
        isSubType(tp1, tp2)
        || canDecompose(a) && isSubspace(Or(decompose(a)), b)
        || canDecompose(b) && isSubspace(a, Or(decompose(b)))
      case (Prod(tp1, _, _), Typ(tp2, _)) =>
        isSubType(tp1, tp2)
      case (a @ Typ(tp1, _), Prod(tp2, fun, ss)) =>
        isSubType(tp1, tp2)
        && covers(fun, tp1, ss.length)
        && isSubspace(Prod(tp2, fun, signature(fun, tp1, ss.length).map(Typ(_, false))), b)
        || canDecompose(a) && isSubspace(Or(decompose(a)), b)
      case (Prod(_, fun1, ss1), Prod(_, fun2, ss2)) =>
        isSameUnapply(fun1, fun2) && ss1.lazyZip(ss2).forall(isSubspace)
    }
  }

  /** Intersection of two spaces  */
  def intersect(a: Space, b: Space)(using Context): Space = trace(i"intersect($a & $b)") {
    (a, b) match {
      case (Empty, _) | (_, Empty) => Empty
      case (_, Or(ss)) => Or(ss.map(intersect(a, _)).filter(_ ne Empty))
      case (Or(ss), _) => Or(ss.map(intersect(_, b)).filter(_ ne Empty))
      case (a @ Typ(tp1, _), b @ Typ(tp2, _)) =>
        if isSubType(tp1, tp2) then a
        else if isSubType(tp2, tp1) then b
        else intersectUnrelatedAtomicTypes(tp1, tp2)(a)
      case (a @ Typ(tp1, _), b @ Prod(tp2, fun, ss)) =>
        if isSubType(tp2, tp1) then b
        else if isSubType(tp1, tp2) then a // problematic corner case: inheriting a case class
        else intersectUnrelatedAtomicTypes(tp1, tp2)(b)
      case (a @ Prod(tp1, fun, ss), b @ Typ(tp2, _)) =>
        if isSubType(tp1, tp2) then a
        else if isSubType(tp2, tp1) then a  // problematic corner case: inheriting a case class
        else intersectUnrelatedAtomicTypes(tp1, tp2)(a)
      case (a @ Prod(tp1, fun1, ss1), Prod(tp2, fun2, ss2)) =>
        if !isSameUnapply(fun1, fun2) then intersectUnrelatedAtomicTypes(tp1, tp2)(a)
        else if ss1.lazyZip(ss2).exists((a, b) => simplify(intersect(a, b)) == Empty) then Empty
        else Prod(tp1, fun1, ss1.lazyZip(ss2).map(intersect))
    }
  }

  /** The space of a not covered by b */
  def minus(a: Space, b: Space)(using Context): Space = trace(i"minus($a - $b)") {
    (a, b) match {
      case (Empty, _) => Empty
      case (_, Empty) => a
      case (Or(ss), _) => Or(ss.map(minus(_, b)))
      case (_, Or(ss)) => ss.foldLeft(a)(minus)
      case (a @ Typ(tp1, _), b @ Typ(tp2, _)) =>
        if isSubType(tp1, tp2) then Empty
        else if canDecompose(a) then minus(Or(decompose(a)), b)
        else if canDecompose(b) then minus(a, Or(decompose(b)))
        else a
      case (a @ Typ(tp1, _), Prod(tp2, fun, ss)) =>
        // rationale: every instance of `tp1` is covered by `tp2(_)`
        if isSubType(tp1, tp2) && covers(fun, tp1, ss.length) then
          minus(Prod(tp1, fun, signature(fun, tp1, ss.length).map(Typ(_, false))), b)
        else if canDecompose(a) then minus(Or(decompose(a)), b)
        else a
      case (Prod(tp1, fun, ss), b @ Typ(tp2, _)) =>
        // uncovered corner case: tp2 :< tp1, may happen when inheriting case class
        if isSubType(tp1, tp2) then Empty
        else if simplify(a) == Empty then Empty
        else if canDecompose(b) then minus(a, Or(decompose(b)))
        else a
      case (Prod(tp1, fun1, ss1), Prod(tp2, fun2, ss2))
          if !isSameUnapply(fun1, fun2) => a
      case (Prod(tp1, fun1, ss1), Prod(tp2, fun2, ss2))
          if fun1.symbol.name == nme.unapply && ss1.length != ss2.length => a
      case (a @ Prod(tp1, fun1, ss1), Prod(tp2, fun2, ss2)) =>
        val range = ss1.indices.toList
        val cache = Array.fill[Space | Null](ss2.length)(null)
        def sub(i: Int) =
          if cache(i) == null then
            cache(i) = minus(ss1(i), ss2(i))
          cache(i).nn

        if range.exists(i => isSubspace(ss1(i), sub(i))) then a
        else if cache.forall(sub => isSubspace(sub.nn, Empty)) then Empty
        else
          // `(_, _, _) - (Some, None, _)` becomes `(None, _, _) | (_, Some, _) | (_, _, Empty)`
          val spaces = LazyList(range*).flatMap { i =>
            flatten(sub(i)).map(s => Prod(tp1, fun1, ss1.updated(i, s)))
          }
          Or(spaces)
    }
  }

  /** Is the unapply or unapplySeq irrefutable?
   *  @param  unapp   The unapply function reference
   */
  def isIrrefutable(unapp: TermRef, argLen: Int)(using Context): Boolean = {
    val ext = Extractor(unapp.widen.finalResultType, unapp.symbol.name, argLen)
    ext match
    case ext: BooleanMatch                                                     => ext.resType <:< ConstantType(Constant(true))
    case ext if !ext.isGetMatch                                                => true // returns Product/Seq/ProdSeq
    case ext if ext.getMatchInfo.isEmptyType <:< ConstantType(Constant(false)) => true
    case ext if ext.unapplyResult.isRef(defn.SomeClass)                        => true
    case ext if isSyntheticScala2Case(unapp.symbol)                            => true
    case ext if unapp.symbol == defn.TupleXXL_unapplySeq                       => true // Fixes TupleXXL.unapplySeq which returns Some but declares Option
    case _                                                                     => false
  }

  /** Is the unapply or unapplySeq irrefutable?
   *  @param  unapp   The unapply function tree
   */
  def isIrrefutable(unapp: Tree, argLen: Int)(using Context): Boolean = {
    funPart(unapp).tpe match
      case funRef: TermRef => isIrrefutable(funRef, argLen)
      case _: ErrorType => false
  }

  /** Is this an `'{..}` or `'[..]` irrefutable quoted patterns?
   *  @param  body The body of the quoted pattern
   *  @param  bodyPt The scrutinee body type
   */
  def isIrrefutableQuotePattern(pat: QuotePattern, pt: Type)(using Context): Boolean = {
    if pat.body.isType then pat.bindings.isEmpty && pt =:= pat.tpe
    else pat.body match
      case _: SplicePattern | Typed(_: SplicePattern, _) => pat.bindings.isEmpty && pt <:< pat.tpe
      case _ => false
  }

  /** Return a space containing the values of both types.
   *
   * The types should be atomic (non-decomposable) and unrelated (neither
   * should be a subtype of the other).
   */
  def intersectUnrelatedAtomicTypes(tp1: Type, tp2: Type)(sp: Typ | Prod)(using Context): Space = trace(i"intersectUnrelatedAtomicTypes($tp1, $tp2)(${sp.className})") {
    // Precondition: !isSubType(tp1, tp2) && !isSubType(tp2, tp1).
    def intersection = sp match
      case sp: Prod => sp.copy(AndType(tp1, tp2))
      case sp: Typ  => sp.copy(AndType(tp1, tp2))
    if !ctx.mode.is(Mode.SafeNulls) && (tp1.isNullType || tp2.isNullType) then
      // Since projections of types don't include null, intersection with null is empty.
      Empty
    else if tp1.classSymbol.isNumericValueClass && tp2.classSymbol.isNumericValueClass then
      // unrelated numeric value classes can equal each other, so let's not consider type space intersection empty
      intersection
    else if isPrimToBox(tp1, tp2) || isPrimToBox(tp2, tp1) then intersection
    else if TypeComparer.provablyDisjoint(tp1, tp2) then Empty
    else intersection
  }

  /** Return the space that represents the pattern `pat` */
  def project(pat: Tree)(using Context): Space = trace(i"project($pat ${pat.className} ${pat.tpe})")(pat match {
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
      val funResult = fun.tpe.widen.finalResultType        // Foo[(1, 2)], not Foo[A]
      val funParam = pat.tpe.stripAnnots                   // Bar[A$1], not Bar[A]
      val funRef = funPart(fun).tpe.asInstanceOf[TermRef]  // Baz.unapply : [A](In[A]): Out[A]
      val ext = Extractor(funResult, funRef.name, pats.size)
      ext match
        case _: FixedArityExtractor => Prod(erase(funParam), funRef, pats.map(project))
        case ext: SeqMatch          =>
          if fun.symbol.owner == defn.SeqFactoryClass && defn.ListType.appliedTo(ext.elemTp) <:< pat.tpe then
            // The exhaustivity and reachability logic already handles decomposing sum types (into its subclasses)
            // and product types (into its components).  To get better counter-examples for patterns that are of type
            // List (or a super-type of list, like LinearSeq) we project them into spaces that use `::` and Nil.
            // Doing so with a pattern of `case Seq() =>` with a scrutinee of type `Vector()` doesn't work because the
            // space is then discarded leading to a false positive reachability warning, see #13931.
            projectSeq(pats)
          else
            Prod(erase(funParam), funRef, projectSeq(pats) :: Nil)
        case ext: ProdSeqMatch      =>
          val (prodPats, seqPats) = pats.splitAt(ext.productSelectors.size - 1)
          Prod(erase(funParam), funRef, prodPats.map(project) ::: projectSeq(seqPats) :: Nil)
        case x @ NoExtractor        => unreachable(x)

    case Typed(pat @ UnApply(_, _, _), _) =>
      project(pat)

    case Typed(_, tpt) =>
      Typ(erase(tpt.tpe.stripAnnots, isValue = true, isTyped = true), decomposed = false)

    case This(_) =>
      Typ(pat.tpe.stripAnnots, decomposed = false)

    case EmptyTree =>         // default rethrow clause of try/catch, check tests/patmat/try2.scala
      Typ(WildcardType, decomposed = false)

    case Block(Nil, expr) =>
      project(expr)

    case _ =>
      // Pattern is an arbitrary expression; assume a skolem (i.e. an unknown value) of the pattern type
      Typ(pat.tpe.narrow, decomposed = false)
  })

  private def project(tp: Type)(using Context): Space = tp match {
    case OrType(tp1, tp2) => Or(project(tp1) :: project(tp2) :: Nil)
    case tp => Typ(tp, decomposed = true)
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
   *  erasure would lose the constraint that `x` and `y` must be
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
   *  @param isTyped whether `tp` is the type from a `Typed` tree
   *
   *  If `isValue` is true, then pattern-bound symbols are erased to its upper bound.
   *  This is needed to avoid spurious unreachable warnings. See tests/patmat/i6197.scala.
   */
  private def erase(tp: Type, inArray: Boolean = false, isValue: Boolean = false, isTyped: Boolean = false)(using Context): Type =
    val inArr = if inArray then " inArray" else ""
    val isVal = if isValue then " isValue" else ""
    val isTyp = if isTyped then " isTyped" else ""
    trace(i"erase($tp ${tp.className}$inArr$isVal$isTyp)")(tp match {
      case tp @ AppliedType(tycon, args) if tycon.typeSymbol.isPatternBound =>
        WildcardType

      case tp @ AppliedType(tycon, args) =>
        val inArray = tycon.isRef(defn.ArrayClass) || tp.translucentSuperType.isRef(defn.ArrayClass)
        val args2 =
          if isTyped && !inArray then args.map(_ => WildcardType)
          else args.map(arg => erase(arg, inArray = inArray, isValue = false))
        tp.derivedAppliedType(erase(tycon, inArray, isValue = false), args2)

      case tp @ OrType(tp1, tp2) =>
        OrType(erase(tp1, inArray, isValue, isTyped), erase(tp2, inArray, isValue, isTyped), tp.isSoft)

      case AndType(tp1, tp2) =>
        AndType(erase(tp1, inArray, isValue, isTyped), erase(tp2, inArray, isValue, isTyped))

      case tp @ RefinedType(parent, _, _) =>
        erase(parent, inArray, isValue, isTyped)

      case tref: TypeRef if tref.symbol.isPatternBound =>
        if inArray then erase(tref.underlying, inArray, isValue, isTyped)
        else if isValue then erase(tref.superType, inArray, isValue, isTyped)
        else WildcardType

      case _ => tp
    })

  /** Space of the pattern: unapplySeq(a, b, c*)
   */
  def projectSeq(pats: List[Tree])(using Context): Space = {
    if (pats.isEmpty) return Typ(defn.NilType, false)

    val (items, zero) = if (isWildcardStarArg(pats.last))
      (pats.init, Typ(defn.ListType.appliedTo(pats.last.tpe.elemType), false))
    else
      (pats, Typ(defn.NilType, false))

    val unapplyTp = defn.ConsType.classSymbol.companionModule.termRef.select(nme.unapply)
    items.foldRight[Space](zero) { (pat, acc) =>
      val consTp = defn.ConsType.appliedTo(pats.head.tpe.widen)
      Prod(consTp, unapplyTp, project(pat) :: acc :: Nil)
    }
  }

  def isPrimToBox(tp: Type, pt: Type)(using Context): Boolean =
    tp.isPrimitiveValueType && (defn.boxedType(tp).classSymbol eq pt.classSymbol)

  /** Is `tp1` a subtype of `tp2`?  */
  def isSubType(tp1: Type, tp2: Type)(using Context): Boolean = trace(i"$tp1 <:< $tp2") {
    if tp1 == ConstantType(Constant(null)) && !ctx.mode.is(Mode.SafeNulls)
    then tp2 == ConstantType(Constant(null))
    else if tp1.isTupleXXLExtract(tp2) then true // See isTupleXXLExtract, fixes TupleXXL parameter type
    else tp1 <:< tp2
  }

  /** True if we can assume that the two unapply methods are the same.
   *  That is, given the same parameter, they return the same result.
   *
   *  We assume that unapply methods are pure, but the same method may
   *  be called with different prefixes, thus behaving differently.
   */
  def isSameUnapply(tp1: TermRef, tp2: TermRef)(using Context): Boolean = trace(i"isSameUnapply($tp1, $tp2)") {
    def isStable(tp: TermRef) =
      !tp.symbol.is(ExtensionMethod) // The "prefix" of an extension method may be, but the receiver isn't, so exclude
      && tp.prefix.isStable
    // always assume two TypeTest[S, T].unapply are the same if they are equal in types
    (isStable(tp1) && isStable(tp2) || tp1.symbol == defn.TypeTest_unapply)
    && tp1 =:= tp2
  }

  /** Return term parameter types of the extractor `unapp`.
   *  Parameter types of the case class type `tp`. Adapted from `unapplyPlan` in patternMatcher  */
  def signature(unapp: TermRef, scrutineeTp: Type, argLen: Int)(using Context): List[Type] = trace(i"signature($unapp, $scrutineeTp, $argLen)") {
    val unappSym = unapp.symbol

    val mt: MethodType = unapp.widen match {
      case mt: MethodType => mt
      case pt: PolyType   =>
          val tvars = constrained(pt)
          val mt = pt.instantiate(tvars).asInstanceOf[MethodType]
          scrutineeTp <:< mt.paramInfos(0)
          // force type inference to infer a narrower type: could be singleton
          // see tests/patmat/i4227.scala
          mt.paramInfos(0) <:< scrutineeTp
          instantiateSelected(mt, tvars)
          isFullyDefined(mt, ForceDegree.all)
          mt
    }

    val resTp = ctx.typeAssigner.safeSubstMethodParams(mt, scrutineeTp :: Nil).finalResultType

    val ext = Extractor(resTp, unappSym.name, argLen)
    val sig = ext match
      case ext: BooleanMatch   => Nil
      case ext: ProductMatch   => ext.productSelectorTypes
      case ext: TupleMatch     => ext.tupleComponentTypes
      case ext: SingleMatch    => ext.resType :: Nil
      case ext: NameBasedMatch => ext.productSelectorTypes
      case ext: SeqMatch       => defn.ListType.appliedTo(ext.elemTp) :: Nil
      case ext: ProdSeqMatch   => ext.productSelectorTypes.init ::: defn.ListType.appliedTo(ext.elemTp) :: Nil
      case x @ NoExtractor     => unreachable(x)

    sig
  }

  /** Whether the extractor covers the given type */
  def covers(unapp: TermRef, scrutineeTp: Type, argLen: Int)(using Context): Boolean = trace(i"covers($unapp, $scrutineeTp, $argLen)") {
    SpaceEngine.isIrrefutable(unapp, argLen)
    || unapp.symbol == defn.TypeTest_unapply && {
      val AppliedType(_, _ :: tp :: Nil) = unapp.prefix.widen.dealias: @unchecked
      scrutineeTp <:< tp
    }
    || unapp.symbol == defn.ClassTagClass_unapply && {
      val AppliedType(_, tp :: Nil) = unapp.prefix.widen.dealias: @unchecked
      scrutineeTp <:< tp
    }
  }

  /** Decompose a type into subspaces -- assume the type can be decomposed */
  def decompose(tp: Type)(using Context): List[Type] = trace(i"decompose($tp)") {
    def rec(tp: Type, mixins: List[Type]): List[Type] = tp.dealias match
      case AndType(tp1, tp2) =>
        var tpB   = tp2
        var parts = rec(tp1, tp2 :: mixins)
        if parts == ListOfNoType then
          tpB   = tp1
          parts = rec(tp2, tp1 :: mixins)
        if parts == ListOfNoType then ListOfNoType
        else parts.collect:
          case tp if tp <:< tpB                              => tp
          case tp if tpB <:< tp                              => tpB
          case tp if !TypeComparer.provablyDisjoint(tp, tpB) => AndType(tp, tpB)

      case OrType(tp1, tp2)                            => List(tp1, tp2)
      case tp if tp.isRef(defn.BooleanClass)           => List(ConstantType(Constant(true)), ConstantType(Constant(false)))
      case tp if tp.isRef(defn.UnitClass)              => ConstantType(Constant(())) :: Nil
      case tp @ NamedType(Parts(parts), _)             => if parts.exists(_ eq tp) then ListOfNoType else parts.map(tp.derivedSelect)
      case _: SingletonType                            => ListOfNoType
      case tp if tp.classSymbol.isAllOf(JavaEnum)      => tp.classSymbol.children.map(_.termRef)
        // the class of a java enum value is the enum class, so this must follow SingletonType to not loop infinitely

      case tp @ AppliedType(Parts(parts), targs) if tp.classSymbol.children.isEmpty =>
        // It might not obvious that it's OK to apply the type arguments of a parent type to child types.
        // But this is guarded by `tp.classSymbol.children.isEmpty`,
        // meaning we'll decompose to the same class, just not the same type.
        // For instance, from i15029, `decompose((X | Y).Field[T]) = [X.Field[T], Y.Field[T]]`.
        parts.map(tp.derivedAppliedType(_, targs))

      case tp if tp.isDecomposableToChildren =>
        def getChildren(sym: Symbol): List[Symbol] =
          sym.children.flatMap { child =>
            if child eq sym then List(sym) // i3145: sealed trait Baz, val x = new Baz {}, Baz.children returns Baz...
            else if tp.classSymbol == defn.TupleClass || tp.classSymbol == defn.NonEmptyTupleClass then
              List(child) // TupleN and TupleXXL classes are used for Tuple, but they aren't Tuple's children
            else if (child.is(Private) || child.is(Sealed)) && child.isOneOf(AbstractOrTrait) then getChildren(child)
            else List(child)
          }
        val children = trace(i"getChildren($tp)")(getChildren(tp.classSymbol))

        val parts = children.map { sym =>
          val sym1 = if (sym.is(ModuleClass)) sym.sourceModule else sym
          val refined = trace(i"refineUsingParent($tp, $sym1, $mixins)")(TypeOps.refineUsingParent(tp, sym1, mixins))

          def inhabited(tp: Type): Boolean = tp.dealias match
            case AndType(tp1, tp2) => !TypeComparer.provablyDisjoint(tp1, tp2)
            case OrType(tp1, tp2) => inhabited(tp1) || inhabited(tp2)
            case tp: RefinedType => inhabited(tp.parent)
            case tp: TypeRef => inhabited(tp.prefix)
            case _ => true

          if inhabited(refined) then refined
          else NoType
        }.filter(_.exists)
        parts

      case _ => ListOfNoType
    end rec

    rec(tp, Nil)
  }

  extension (tp: Type)
    def isDecomposableToChildren(using Context): Boolean =
      val sym = tp.typeSymbol  // e.g. Foo[List[Int]] = type Foo (i19275)
      val cls = tp.classSymbol // e.g. Foo[List[Int]] = class List
      tp.hasSimpleKind                  // can't decompose higher-kinded types
        && cls.is(Sealed)
        && cls.isOneOf(AbstractOrTrait) // ignore sealed non-abstract classes
        && !cls.hasAnonymousChild       // can't name anonymous classes as counter-examples
        && cls.children.nonEmpty        // can't decompose without children
        && !sym.isOpaqueAlias           // can't instantiate subclasses to conform to an opaque type (i19275)

  val ListOfNoType    = List(NoType)
  val ListOfTypNoType = ListOfNoType.map(Typ(_, decomposed = true))

  object Parts:
    def unapply(tp: Type)(using Context): PartsExtractor = PartsExtractor(decompose(tp))

  final class PartsExtractor(val get: List[Type]) extends AnyVal:
    def isEmpty: Boolean = get == ListOfNoType

  /** Show friendly type name with current scope in mind
   *
   *  E.g.    C.this.B     -->  B     if current owner is C
   *          C.this.x.T   -->  x.T   if current owner is C
   *          C            -->  C     if current owner is C !!!
   */
  private class LocalPrinter(_ctx: Context) extends RefinedPrinter(_ctx):
    val enclosingCls = ctx.owner.enclosingClass
    override def isOmittablePrefix(sym: Symbol) =
      super.isOmittablePrefix(sym)
      || sym == enclosingCls || sym == enclosingCls.sourceModule

  /** Whether the counterexample is satisfiable. The space is flattened and non-empty. */
  def satisfiable(sp: Space)(using Context): Boolean = {
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

  /** Display spaces.  Used for printing uncovered spaces in the in-exhaustive error message. */
  def display(s: Space)(using Context): String = inContext(ctx.fresh.setPrinterFn(LocalPrinter(_))) {
    def params(tp: Type): List[Type] = tp.classSymbol.primaryConstructor.info.firstParamTypes

    /** does the companion object of the given symbol have custom unapply */
    def hasCustomUnapply(sym: Symbol): Boolean = {
      val companion = sym.companionModule
      companion.findMember(nme.unapply, NoPrefix, required = EmptyFlags, excluded = Synthetic).exists ||
        companion.findMember(nme.unapplySeq, NoPrefix, required = EmptyFlags, excluded = Synthetic).exists
    }

    def doShow(s: Space, flattenList: Boolean = false): String = s match {
      case Empty => "empty"
      case Typ(c: ConstantType, _) => c.value.show
      case Typ(tp: TermRef, _) =>
        if (flattenList && tp <:< defn.NilType) ""
        else tp.symbol.showName
      case Typ(tp, decomposed) =>
        val cls = tp.classSymbol
        if ctx.definitions.isTupleNType(tp) then
          params(tp).map(_ => "_").mkString("(", ", ", ")")
        else if defn.ListType.isRef(cls) then
          if flattenList then "_*" else "_: List"
        else if (defn.ConsType.isRef(cls))
          if flattenList then "_, _*"  else "List(_, _*)"
        else if cls.hasAnonymousChild then
          s"_: ${tp.typeConstructor.show} (anonymous)"
        else if cls.is(CaseClass) && !hasCustomUnapply(cls) then
          // use constructor syntax for case class
          tp.typeConstructor.show + params(tp).map(_ => "_").mkString("(", ", ", ")")
        else if !decomposed then "_"
        else "_: " + tp.show
      case Prod(tp, fun, params) =>
        if ctx.definitions.isTupleNType(tp) then
          "(" + params.map(doShow(_)).mkString(", ") + ")"
        else if tp.isRef(defn.ConsType.symbol) then
          val body = params.map(doShow(_, flattenList = true)).filter(_.nonEmpty).mkString(", ")
          if flattenList then body else s"List($body)"
        else
          val isUnapplySeq = fun.symbol.name eq nme.unapplySeq
          val paramsStr = params.map(doShow(_, flattenList = isUnapplySeq)).mkString("(", ", ", ")")
          val prefix = fun.prefix match
            case pre: TermRef => pre.symbol.typeRef
            case pre          => pre
          prefix.typeConstructor.show + paramsStr
      case Or(ss) =>
        ss.map(doShow(_, flattenList)).mkString(" | ")
    }

    doShow(s)
  }

  private def exhaustivityCheckable(sel: Tree)(using Context): Boolean = {
    val seen = collection.mutable.Set.empty[Type]

    // Possible to check everything, but be compatible with scalac by default
    def isCheckable(tp: Type): Boolean =
      val tpw = tp.widen.dealias
      val classSym = tpw.classSymbol
      classSym.is(Sealed) && !tpw.isLargeGenericTuple || // exclude large generic tuples from exhaustivity
                                                         // requires an unknown number of changes to make work
      tpw.isInstanceOf[OrType] ||
      (tpw.isInstanceOf[AndType] && {
        val and = tpw.asInstanceOf[AndType]
        isCheckable(and.tp1) || isCheckable(and.tp2)
      }) ||
      tpw.isRef(defn.BooleanClass) ||
      classSym.isAllOf(JavaEnum) ||
      classSym.is(Case) && {
        if seen.add(tpw) then productSelectorTypes(tpw).exists(isCheckable(_))
        else true // recursive case class: return true and other members can still fail the check
      }

    !sel.tpe.hasAnnotation(defn.UncheckedAnnot)
    && {
      ctx.settings.YcheckAllPatmat.value
      || isCheckable(sel.tpe)
    }
  }

  /** Whether counter-examples should be further checked? True for GADTs. */
  private def shouldCheckExamples(tp: Type)(using Context): Boolean =
    new TypeAccumulator[Boolean] {
      override def apply(b: Boolean, tp: Type): Boolean = tp match {
        case tref: TypeRef if tref.symbol.is(TypeParam) && variance != 1 => true
        case tp => b || foldOver(b, tp)
      }
    }.apply(false, tp)

  /** Return the underlying type of non-module, non-constant, non-enum case singleton types.
   *  Also widen ExprType to its result type, and rewrap any annotation wrappers.
   *  For example, with `val opt = None`, widen `opt.type` to `None.type`. */
  def toUnderlying(tp: Type)(using Context): Type = trace(i"toUnderlying($tp)")(tp match {
    case _: ConstantType                            => tp
    case tp: TermRef if tp.symbol.is(Module)        => tp
    case tp: TermRef if tp.symbol.isAllOf(EnumCase) => tp
    case tp: SingletonType                          => toUnderlying(tp.underlying)
    case tp: ExprType                               => toUnderlying(tp.resultType)
    case AnnotatedType(tp, annot)                   => AnnotatedType(toUnderlying(tp), annot)
    case _                                          => tp
  })

  def checkExhaustivity(m: Match)(using Context): Unit = trace(i"checkExhaustivity($m)") {
    val selTyp = toUnderlying(m.selector.tpe).dealias
    val targetSpace = trace(i"targetSpace($selTyp)")(project(selTyp))

    val patternSpace = Or(m.cases.foldLeft(List.empty[Space]) { (acc, x) =>
      val space = if x.guard.isEmpty then trace(i"project(${x.pat})")(project(x.pat)) else Empty
      space :: acc
    })

    val checkGADTSAT = shouldCheckExamples(selTyp)

    val uncovered =
      flatten(simplify(minus(targetSpace, patternSpace))).filter({ s =>
        s != Empty && (!checkGADTSAT || satisfiable(s))
      })

    if uncovered.nonEmpty then
      val deduped = dedup(uncovered)
      report.warning(PatternMatchExhaustivity(deduped.map(display), m), m.selector)
  }

  private def reachabilityCheckable(sel: Tree)(using Context): Boolean =
    // Ignore Expr[T] and Type[T] for unreachability as a special case.
    // Quote patterns produce repeated calls to the same unapply method, but with different implicit parameters.
    // Since we assume that repeated calls to the same unapply method overlap
    // and implicit parameters cannot normally differ between two patterns in one `match`,
    // the easiest solution is just to ignore Expr[T] and Type[T].
    !sel.tpe.hasAnnotation(defn.UncheckedAnnot)
    && !sel.tpe.widen.isRef(defn.QuotedExprClass)
    && !sel.tpe.widen.isRef(defn.QuotedTypeClass)

  def checkReachability(m: Match)(using Context): Unit = trace(i"checkReachability($m)") {
    val cases = m.cases.toIndexedSeq

    val selTyp = toUnderlying(m.selector.tpe).dealias

    val isNullable = selTyp.classSymbol.isNullableClass
    val targetSpace = trace(i"targetSpace($selTyp)")(if isNullable
      then project(OrType(selTyp, ConstantType(Constant(null)), soft = false))
      else project(selTyp)
    )

    var i        = 0
    val len      = cases.length
    var prevs    = List.empty[Space]
    var deferred = List.empty[Tree]

    while (i < len) {
      val CaseDef(pat, guard, _) = cases(i)

      val curr = trace(i"project($pat)")(project(pat))

      val covered = trace("covered")(simplify(intersect(curr, targetSpace)))

      val prev = trace("prev")(simplify(Or(prevs)))

      if prev == Empty && covered == Empty then // defer until a case is reachable
        deferred ::= pat
      else {
        for (pat <- deferred.reverseIterator)
          report.warning(MatchCaseUnreachable(), pat.srcPos)
        if pat != EmptyTree // rethrow case of catch uses EmptyTree
            && !pat.symbol.isAllOf(SyntheticCase, butNot=Method) // ExpandSAMs default cases use SyntheticCase
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

  def checkMatch(m: Match)(using Context): Unit =
    if exhaustivityCheckable(m.selector) then checkExhaustivity(m)
    if reachabilityCheckable(m.selector) then checkReachability(m)
}
