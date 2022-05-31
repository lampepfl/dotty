package dotty.tools
package dotc
package core

import Symbols._
import Flags._
import Names._
import StdNames._, NameOps._
import NullOpsDecorator._
import NameKinds.SkolemName
import Scopes._
import Constants._
import Contexts._
import Phases._
import Annotations._
import SymDenotations._
import Decorators._
import Denotations._
import Periods._
import CheckRealizable._
import Variances.{Variance, setStructuralVariances, Invariant}
import typer.Nullables
import util.Stats._
import util.SimpleIdentitySet
import ast.tpd._
import ast.TreeTypeMap
import printing.Texts._
import printing.Printer
import Hashable._
import Uniques._
import collection.mutable
import config.Config
import annotation.{tailrec, constructorOnly}
import scala.util.hashing.{ MurmurHash3 => hashing }
import config.Printers.{core, typr, matchTypes}
import reporting.{trace, Message}
import java.lang.ref.WeakReference

import scala.annotation.internal.sharable
import scala.annotation.threadUnsafe

import dotty.tools.dotc.transform.SymUtils._

object Types {

  @sharable private var nextId = 0

  implicit def eqType: CanEqual[Type, Type] = CanEqual.derived

  /** Main class representing types.
   *
   *  The principal subclasses and sub-objects are as follows:
   *
   *  ```none
   *  Type -+- ProxyType --+- NamedType ----+--- TypeRef
   *        |              |                 \
   *        |              +- SingletonType-+-+- TermRef
   *        |              |                |
   *        |              |                +--- ThisType
   *        |              |                +--- SuperType
   *        |              |                +--- ConstantType
   *        |              |                +--- TermParamRef
   *        |              |                +----RecThis
   *        |              |                +--- SkolemType
   *        |              +- TypeParamRef
   *        |              +- RefinedOrRecType -+-- RefinedType
   *        |              |                   -+-- RecType
   *        |              +- AppliedType
   *        |              +- TypeBounds
   *        |              +- ExprType
   *        |              +- AnnotatedType
   *        |              +- TypeVar
   *        |              +- HKTypeLambda
   *        |              +- MatchType
   *        |
   *        +- GroundType -+- AndType
   *                       +- OrType
   *                       +- MethodOrPoly ---+-- PolyType
   *                       |                  +-- MethodType
   *                       +- ClassInfo
   *                       |
   *                       +- NoType
   *                       +- NoPrefix
   *                       +- ErrorType
   *                       +- WildcardType
   *  ```
   *
   *  Note: please keep in sync with copy in `docs/docs/internals/type-system.md`.
   */
  abstract class Type extends Hashable with printing.Showable {

// ----- Tests -----------------------------------------------------

//    // debug only: a unique identifier for a type
//    val uniqId = {
//      nextId = nextId + 1
//      if (nextId == 19555)
//        println("foo")
//      nextId
//    }

    /** A cache indicating whether the type was still provisional, last time we checked */
    @sharable private var mightBeProvisional = true

    /** Is this type still provisional? This is the case if the type contains, or depends on,
     *  uninstantiated type variables or type symbols that have the Provisional flag set.
     *  This is an antimonotonic property - once a type is not provisional, it stays so forever.
     */
    def isProvisional(using Context): Boolean = mightBeProvisional && testProvisional

    private def testProvisional(using Context): Boolean =
      class ProAcc extends TypeAccumulator[Boolean]:
        override def apply(x: Boolean, t: Type) = x || test(t, this)
      def test(t: Type, theAcc: TypeAccumulator[Boolean] | Null): Boolean =
        if t.mightBeProvisional then
          t.mightBeProvisional = t match
            case t: TypeRef =>
              !t.currentSymbol.isStatic && {
                (t: Type).mightBeProvisional = false // break cycles
                t.symbol.is(Provisional)
                || test(t.prefix, theAcc)
                || t.info.match
                    case info: AliasingBounds => test(info.alias, theAcc)
                    case TypeBounds(lo, hi) => test(lo, theAcc) || test(hi, theAcc)
                    case _ => false
              }
            case t: TermRef =>
              !t.currentSymbol.isStatic && test(t.prefix, theAcc)
            case t: AppliedType =>
              t.fold(false, (x, tp) => x || test(tp, theAcc))
            case t: TypeVar =>
              !t.inst.exists || test(t.inst, theAcc)
            case t: LazyRef =>
              !t.completed || test(t.ref, theAcc)
            case _ =>
              (if theAcc != null then theAcc else ProAcc()).foldOver(false, t)
        end if
        t.mightBeProvisional
      end test
      test(this, null)
    end testProvisional

    /** Is this type different from NoType? */
    final def exists: Boolean = this.ne(NoType)

    /** This type, if it exists, otherwise `that` type */
    inline def orElse(inline that: Type): Type = if (exists) this else that

    /** Is this type a value type? */
    final def isValueType: Boolean = this.isInstanceOf[ValueType]

    /** Is this a value type or a type lambda? */
    final def isValueTypeOrLambda: Boolean = isValueType || this.isInstanceOf[TypeLambda]

    /** Is this a value type or a wildcard? */
    final def isValueTypeOrWildcard: Boolean = isValueType || this.isInstanceOf[WildcardType]

    /** Does this type denote a stable reference (i.e. singleton type)?
      *
      * Like in isStableMember, "stability" means idempotence.
      * Rationale: If an expression has a stable type, the expression must be idempotent, so stable types
      * must be singleton types of stable expressions. */
    final def isStable(using Context): Boolean = stripTypeVar match {
      case tp: TermRef => tp.symbol.isStableMember && tp.prefix.isStable || tp.info.isStable
      case _: SingletonType | NoPrefix => true
      case tp: RefinedOrRecType => tp.parent.isStable
      case tp: ExprType => tp.resultType.isStable
      case tp: AnnotatedType =>
        // NOTE UncheckedStableAnnot was originally meant to be put on fields,
        // not on types. Allowing it on types is a Scala 3 extension. See:
        // https://www.scala-lang.org/files/archive/spec/2.11/11-annotations.html#scala-compiler-annotations
        tp.annot.symbol == defn.UncheckedStableAnnot || tp.parent.isStable
      case tp: AndType =>
        // TODO: fix And type check when tp contains type parames for explicit-nulls flow-typing
        // see: tests/explicit-nulls/pos/flow-stable.scala.disabled
        tp.tp1.isStable && (realizability(tp.tp2) eq Realizable) ||
        tp.tp2.isStable && (realizability(tp.tp1) eq Realizable)
      case _ => false
    }

    /** Is this type a (possibly refined or applied or aliased) type reference
     *  to the given type symbol?
     *  @sym  The symbol to compare to. It must be a class symbol or abstract type.
     *        It makes no sense for it to be an alias type because isRef would always
     *        return false in that case.
     */
    def isRef(sym: Symbol, skipRefined: Boolean = true)(using Context): Boolean = stripped match {
      case this1: TypeRef =>
        this1.info match { // see comment in Namer#typeDefSig
          case TypeAlias(tp) => tp.isRef(sym, skipRefined)
          case _ => this1.symbol eq sym
        }
      case this1: RefinedOrRecType if skipRefined =>
        this1.parent.isRef(sym, skipRefined)
      case this1: AppliedType =>
        val this2 = this1.dealias
        if (this2 ne this1) this2.isRef(sym, skipRefined)
        else this1.underlying.isRef(sym, skipRefined)
      case _ => false
    }

    /** Is this type a (neither aliased nor applied nor annotated) reference to class `sym`? */
    def isDirectRef(sym: Symbol)(using Context): Boolean = stripTypeVar match {
      case this1: TypeRef =>
        this1.name == sym.name && // avoid forcing info if names differ
        (this1.symbol eq sym)
      case _ =>
        false
    }

    def isAny(using Context): Boolean     = isRef(defn.AnyClass, skipRefined = false)
    def isAnyRef(using Context): Boolean  = isRef(defn.ObjectClass, skipRefined = false)
    def isAnyKind(using Context): Boolean = isRef(defn.AnyKindClass, skipRefined = false)

    def isTopType(using Context): Boolean = dealias match
      case tp: TypeRef => defn.topClasses.contains(tp.symbol)
      case _ => false

    /** Is this type exactly Null (no vars, aliases, refinements etc allowed)? */
    def isExactlyNull(using Context): Boolean = this match {
      case tp: TypeRef =>
        tp.name == tpnme.Null && (tp.symbol eq defn.NullClass)
      case _ => false
    }

    /** Is this type exactly Nothing (no vars, aliases, refinements etc allowed)? */
    def isExactlyNothing(using Context): Boolean = this match {
      case tp: TypeRef =>
        tp.name == tpnme.Nothing && (tp.symbol eq defn.NothingClass)
      case _ => false
    }

    /** Is this type exactly Any (no vars, aliases, refinements etc allowed)? */
    def isExactlyAny(using Context): Boolean = this match {
      case tp: TypeRef =>
        tp.name == tpnme.Any && (tp.symbol eq defn.AnyClass)
      case _ => false
    }

    def isBottomType(using Context): Boolean =
      if ctx.mode.is(Mode.SafeNulls) && !ctx.phase.erasedTypes then hasClassSymbol(defn.NothingClass)
      else isBottomTypeAfterErasure

    def isBottomTypeAfterErasure(using Context): Boolean =
      val d = defn
      hasClassSymbol(d.NothingClass) || hasClassSymbol(d.NullClass)

    /** Does this type refer exactly to class symbol `sym`, instead of to a subclass of `sym`?
     *  Implemented like `isRef`, but follows more types: all type proxies as well as and- and or-types
     */
    private[Types] def isTightPrefix(sym: Symbol)(using Context): Boolean = stripTypeVar match {
      case tp: NamedType => tp.info.isTightPrefix(sym)
      case tp: ClassInfo => tp.cls eq sym
      case tp: Types.ThisType => tp.cls eq sym
      case tp: TypeProxy => tp.underlying.isTightPrefix(sym)
      case tp: AndType => tp.tp1.isTightPrefix(sym) && tp.tp2.isTightPrefix(sym)
      case tp: OrType => tp.tp1.isTightPrefix(sym) || tp.tp2.isTightPrefix(sym)
      case _ => false
    }

    /** True if this type is an instance of the given `cls` or an instance of
     *  a non-bottom subclass of `cls`.
     */
    final def derivesFrom(cls: Symbol)(using Context): Boolean = {
      def isLowerBottomType(tp: Type) =
        tp.isBottomType
        && (tp.hasClassSymbol(defn.NothingClass)
            || cls != defn.NothingClass && !cls.isValueClass)
      def loop(tp: Type): Boolean = tp match {
        case tp: TypeRef =>
          val sym = tp.symbol
          if (sym.isClass) sym.derivesFrom(cls) else loop(tp.superType)
        case tp: AppliedType =>
          tp.superType.derivesFrom(cls)
        case tp: MatchType =>
          tp.bound.derivesFrom(cls) || tp.reduced.derivesFrom(cls)
        case tp: TypeProxy =>
          loop(tp.underlying)
        case tp: AndType =>
          loop(tp.tp1) || loop(tp.tp2)
        case tp: OrType =>
          // If the type is `T | Null` or `T | Nothing`, the class is != Nothing,
          // and `T` derivesFrom the class, then the OrType derivesFrom the class.
          // Otherwise, we need to check both sides derivesFrom the class.
          if isLowerBottomType(tp.tp1) then
            loop(tp.tp2)
          else if isLowerBottomType(tp.tp2) then
            loop(tp.tp1)
          else
            loop(tp.tp1) && loop(tp.tp2)
        case tp: JavaArrayType =>
          cls == defn.ObjectClass
        case _ =>
          false
      }
      loop(this)
    }

    def isFromJavaObject(using Context): Boolean =
      isRef(defn.ObjectClass) && (typeSymbol eq defn.FromJavaObjectSymbol)

    def containsFromJavaObject(using Context): Boolean = this match
      case tp: OrType => tp.tp1.containsFromJavaObject || tp.tp2.containsFromJavaObject
      case tp: AndType => tp.tp1.containsFromJavaObject && tp.tp2.containsFromJavaObject
      case _ => isFromJavaObject

    /** True iff `symd` is a denotation of a class type parameter and the reference
     *  `<pre> . <symd>` is an actual argument reference, i.e. `pre` is not the
     *  ThisType of `symd`'s owner, or a reference to `symd`'s owner.'
     */
    def isArgPrefixOf(symd: SymDenotation)(using Context): Boolean =
      symd.exists && !symd.owner.is(Package) && // Early exit if possible because the next check would force SymbolLoaders
      symd.isAllOf(ClassTypeParam) && {
        this match {
          case tp: ThisType => tp.cls ne symd.owner
          case tp: TypeRef => tp.symbol ne symd.owner
          case _ => true
        }
      }

    /** Is this type a (possibly aliased) singleton type? */
    def isSingleton(using Context): Boolean = dealias.isInstanceOf[SingletonType]

    /** Is this type of kind `AnyKind`? */
    def hasAnyKind(using Context): Boolean = {
      @tailrec def loop(tp: Type): Boolean = tp match {
        case tp: TypeRef =>
          val sym = tp.symbol
          if (sym.isClass) sym == defn.AnyKindClass else loop(tp.translucentSuperType)
        case tp: TypeProxy =>
          loop(tp.underlying)
        case _ =>
          false
      }
      loop(this)
    }

    /** Is this type guaranteed not to have `null` as a value? */
    final def isNotNull(using Context): Boolean = this match {
      case tp: ConstantType => tp.value.value != null
      case tp: ClassInfo => !tp.cls.isNullableClass && tp.cls != defn.NothingClass
      case tp: TypeBounds => tp.lo.isNotNull
      case tp: TypeProxy => tp.underlying.isNotNull
      case AndType(tp1, tp2) => tp1.isNotNull || tp2.isNotNull
      case OrType(tp1, tp2) => tp1.isNotNull && tp2.isNotNull
      case _ => false
    }

    /** Is this type produced as a repair for an error? */
    final def isError(using Context): Boolean = stripTypeVar.isInstanceOf[ErrorType]

    /** Is some part of the widened version of this type produced as a repair for an error?
     *
     */
    def isErroneous(using Context): Boolean =
      widen.existsPart(_.isError, forceLazy = false)

    /** Is this type unusable for implicit search or overloading resolution
     *  since it has embedded errors that can match anything? This is weaker and more
     *  ad-hoc than isErroneous. The main differences are that we always consider aliases
     *  (since these are relevant for inference or resolution) but never consider prefixes
     *  (since these often do not constrain the search space anyway).
     */
    def unusableForInference(using Context): Boolean = widenDealias match
      case AppliedType(tycon, args) => tycon.unusableForInference || args.exists(_.unusableForInference)
      case RefinedType(parent, _, rinfo) => parent.unusableForInference || rinfo.unusableForInference
      case TypeBounds(lo, hi) => lo.unusableForInference || hi.unusableForInference
      case tp: AndOrType => tp.tp1.unusableForInference || tp.tp2.unusableForInference
      case tp: LambdaType => tp.resultType.unusableForInference || tp.paramInfos.exists(_.unusableForInference)
      case WildcardType(optBounds) => optBounds.unusableForInference
      case _: ErrorType => true
      case _ => false

    /** Does the type carry an annotation that is an instance of `cls`? */
    @tailrec final def hasAnnotation(cls: ClassSymbol)(using Context): Boolean = stripTypeVar match {
      case AnnotatedType(tp, annot) => (annot matches cls) || (tp hasAnnotation cls)
      case _ => false
    }

    /** Does this type have a supertype with an annotation satisfying given predicate `p`? */
    def derivesAnnotWith(p: Annotation => Boolean)(using Context): Boolean = this match {
      case tp: AnnotatedType => p(tp.annot) || tp.parent.derivesAnnotWith(p)
      case tp: TypeProxy => tp.superType.derivesAnnotWith(p)
      case AndType(l, r) => l.derivesAnnotWith(p) || r.derivesAnnotWith(p)
      case OrType(l, r) => l.derivesAnnotWith(p) && r.derivesAnnotWith(p)
      case _ => false
    }

    /** Does this type occur as a part of type `that`? */
    def occursIn(that: Type)(using Context): Boolean =
      that.existsPart(this == _)

    /** Does this type not refer to TypeParamRefs or uninstantiated TypeVars? */
    final def isGround(using Context): Boolean =
      (new isGroundAccumulator).apply(true, this)

    /** Is this a type of a repeated parameter? */
    def isRepeatedParam(using Context): Boolean =
      typeSymbol eq defn.RepeatedParamClass

    /** Is this the type of a method that has a repeated parameter type as
     *  last parameter type?
     */
    def isVarArgsMethod(using Context): Boolean = stripPoly match {
      case mt: MethodType => mt.paramInfos.nonEmpty && mt.paramInfos.last.isRepeatedParam
      case _ => false
    }

    /** Is this the type of a method with a leading empty parameter list?
     */
    def isNullaryMethod(using Context): Boolean = stripPoly match {
      case MethodType(Nil) => true
      case _ => false
    }

    /** Is this an alias TypeBounds? */
    final def isTypeAlias: Boolean = this.isInstanceOf[TypeAlias]

    /** Is this a Method or PolyType which has implicit or contextual parameters? */
    def isImplicitMethod: Boolean = false

    /** Is this a Method or PolyType which has contextual parameters as first value parameter list? */
    def isContextualMethod: Boolean = false

    /** Is this a MethodType for which the parameters will not be used? */
    def isErasedMethod: Boolean = false

    /** Is this a match type or a higher-kinded abstraction of one?
     */
    def isMatch(using Context): Boolean = stripped match {
      case _: MatchType => true
      case tp: HKTypeLambda => tp.resType.isMatch
      case tp: AppliedType => tp.isMatchAlias
      case _ => false
    }

    /** Is this a higher-kinded type lambda with given parameter variances? */
    def isDeclaredVarianceLambda: Boolean = false

    /** Does this type contain wildcard types? */
    final def containsWildcardTypes(using Context) =
      existsPart(_.isInstanceOf[WildcardType], StopAt.Static, forceLazy = false)

// ----- Higher-order combinators -----------------------------------

    /** Returns true if there is a part of this type that satisfies predicate `p`.
     */
    final def existsPart(p: Type => Boolean, stopAt: StopAt = StopAt.None, forceLazy: Boolean = true)(using Context): Boolean =
      new ExistsAccumulator(p, stopAt, forceLazy).apply(false, this)

    /** Returns true if all parts of this type satisfy predicate `p`.
     */
    final def forallParts(p: Type => Boolean)(using Context): Boolean =
      !existsPart(!p(_))

    /** Performs operation on all parts of this type */
    final def foreachPart(p: Type => Unit, stopAt: StopAt = StopAt.None)(using Context): Unit =
      new ForeachAccumulator(p, stopAt).apply((), this)

    /** The parts of this type which are type or term refs and which
     *  satisfy predicate `p`.
     *
     *  @param p                   The predicate to satisfy
     */
    def namedPartsWith(p: NamedType => Boolean)(using Context): List[NamedType] =
      new NamedPartsAccumulator(p).apply(Nil, this)

    /** Map function `f` over elements of an AndType, rebuilding with function `g` */
    def mapReduceAnd[T](f: Type => T)(g: (T, T) => T)(using Context): T = stripTypeVar match {
      case AndType(tp1, tp2) => g(tp1.mapReduceAnd(f)(g), tp2.mapReduceAnd(f)(g))
      case _ => f(this)
    }

    /** Map function `f` over elements of an OrType, rebuilding with function `g` */
    final def mapReduceOr[T](f: Type => T)(g: (T, T) => T)(using Context): T = stripTypeVar match {
      case OrType(tp1, tp2) => g(tp1.mapReduceOr(f)(g), tp2.mapReduceOr(f)(g))
      case _ => f(this)
    }

// ----- Associated symbols ----------------------------------------------

    /** The type symbol associated with the type */
    @tailrec final def typeSymbol(using Context): Symbol = this match {
      case tp: TypeRef => tp.symbol
      case tp: TypeProxy => tp.underlying.typeSymbol
      case tp: ClassInfo => tp.cls
      case  _: JavaArrayType => defn.ArrayClass
      case _ => NoSymbol
    }

    /** The least class or trait of which this type is a subtype or parameterized
     *  instance, or NoSymbol if none exists (either because this type is not a
     *  value type, or because superclasses are ambiguous).
     */
    final def classSymbol(using Context): Symbol = this match
      case tp: TypeRef =>
        val sym = tp.symbol
        if (sym.isClass) sym else tp.superType.classSymbol
      case tp: TypeProxy =>
        tp.underlying.classSymbol
      case tp: ClassInfo =>
        tp.cls
      case AndType(l, r) =>
        val lsym = l.classSymbol
        val rsym = r.classSymbol
        if (lsym isSubClass rsym) lsym
        else if (rsym isSubClass lsym) rsym
        else NoSymbol
      case tp: OrType =>
        if tp.tp1.hasClassSymbol(defn.NothingClass) then
          tp.tp2.classSymbol
        else if tp.tp2.hasClassSymbol(defn.NothingClass) then
          tp.tp1.classSymbol
        else
          def tp1Null = tp.tp1.hasClassSymbol(defn.NullClass)
          def tp2Null = tp.tp2.hasClassSymbol(defn.NullClass)
          if ctx.erasedTypes && (tp1Null || tp2Null) then
            val otherSide = if tp1Null then tp.tp2.classSymbol else tp.tp1.classSymbol
            if otherSide.isValueClass then defn.AnyClass else otherSide
          else
            tp.join.classSymbol
      case _: JavaArrayType =>
        defn.ArrayClass
      case _ =>
        NoSymbol

    /** The least (wrt <:<) set of symbols satisfying the `include` predicate of which this type is a subtype
     */
    final def parentSymbols(include: Symbol => Boolean)(using Context): List[Symbol] = this match {
      case tp: TypeRef =>
        val sym = tp.symbol
        if (include(sym)) sym :: Nil else tp.superType.parentSymbols(include)
      case tp: TypeProxy =>
        tp.underlying.parentSymbols(include)
      case tp: ClassInfo =>
        tp.cls :: Nil
      case AndType(l, r) =>
        l.parentSymbols(include) | r.parentSymbols(include)
      case OrType(l, r) =>
        l.parentSymbols(include) intersect r.parentSymbols(include) // TODO does not conform to spec
      case _ =>
        Nil
    }

    /** The least (wrt <:<) set of class symbols of which this type is a subtype
     */
    final def classSymbols(using Context): List[ClassSymbol] =
      parentSymbols(_.isClass).asInstanceOf

    /** Same as `this.classSymbols.contains(cls)` but more efficient */
    final def hasClassSymbol(cls: Symbol)(using Context): Boolean = this match
      case tp: TypeRef   =>
        val sym = tp.symbol
        sym == cls || !sym.isClass && tp.superType.hasClassSymbol(cls)
      case tp: TypeProxy =>
        tp.underlying.hasClassSymbol(cls)
      case tp: ClassInfo =>
        tp.cls == cls
      case AndType(l, r) =>
        l.hasClassSymbol(cls) || r.hasClassSymbol(cls)
      case OrType(l, r) =>
        l.hasClassSymbol(cls) && r.hasClassSymbol(cls)
      case _ =>
        false

    /** Same as hasClassSmbol(MatchableClass), except that we also follow the constraint
     *  bounds of type variables in the constraint.
     */
    def isMatchableBound(using Context): Boolean = dealias match
      case tp: TypeRef => tp.symbol == defn.MatchableClass
      case tp: TypeParamRef =>
        ctx.typerState.constraint.entry(tp) match
          case bounds: TypeBounds => bounds.hi.isMatchableBound
          case _ => false
      case tp: TypeProxy => tp.underlying.isMatchableBound
      case tp: AndType => tp.tp1.isMatchableBound || tp.tp2.isMatchableBound
      case tp: OrType => tp.tp1.isMatchableBound && tp.tp2.isMatchableBound
      case _ => false

    /** The term symbol associated with the type */
    @tailrec final def termSymbol(using Context): Symbol = this match {
      case tp: TermRef => tp.symbol
      case tp: TypeProxy => tp.underlying.termSymbol
      case _ => NoSymbol
    }

    /** The base classes of this type as determined by ClassDenotation
     *  in linearization order, with the class itself as first element.
     *  Inherited by all type proxies. Overridden for And and Or types.
     *  `Nil` for all other types.
     */
    def baseClasses(using Context): List[ClassSymbol] =
      record("baseClasses")
      try
        this match
          case tp: TypeProxy =>
            tp.underlying.baseClasses
          case tp: ClassInfo =>
            tp.cls.classDenot.baseClasses
          case _ => Nil
      catch case ex: Throwable =>
        handleRecursive("base classes of", this.show, ex)

// ----- Member access -------------------------------------------------

    /** The scope of all declarations of this type.
     *  Defined by ClassInfo, inherited by type proxies.
     *  Empty scope for all other types.
     */
    @tailrec final def decls(using Context): Scope = this match {
      case tp: ClassInfo =>
        tp.decls
      case tp: TypeProxy =>
        tp.underlying.decls
      case _ =>
        EmptyScope
    }

    /** A denotation containing the declaration(s) in this type with the given name.
     *  The result is either a SymDenotation or a MultiDenotation of SymDenotations.
     *  The info(s) are the original symbol infos, no translation takes place.
     */
    final def decl(name: Name)(using Context): Denotation = {
      record("decl")
      findDecl(name, EmptyFlags)
    }

    /** A denotation containing the non-private declaration(s) in this type with the given name */
    final def nonPrivateDecl(name: Name)(using Context): Denotation =
      findDecl(name, Private)

    /** A denotation containing the declaration(s) in this type with the given
     *  name, as seen from prefix type `pre`. Declarations that have a flag
     *  in `excluded` are omitted.
     */
    @tailrec final def findDecl(name: Name, excluded: FlagSet)(using Context): Denotation = this match {
      case tp: ClassInfo =>
        tp.decls.denotsNamed(name).filterWithFlags(EmptyFlags, excluded).toDenot(NoPrefix)
      case tp: TypeProxy =>
        tp.underlying.findDecl(name, excluded)
      case err: ErrorType =>
        newErrorSymbol(classSymbol orElse defn.RootClass, name, err.msg)
      case _ =>
        NoDenotation
    }

    /** The member of this type with the given name  */
    final def member(name: Name)(using Context): Denotation = {
      record("member")
      memberBasedOnFlags(name, required = EmptyFlags, excluded = EmptyFlags)
    }

    /** The non-private member of this type with the given name. */
    final def nonPrivateMember(name: Name)(using Context): Denotation = {
      record("nonPrivateMember")
      memberBasedOnFlags(name, required = EmptyFlags, excluded = Flags.Private)
    }

    /** The member with given `name` and required and/or excluded flags */
    final def memberBasedOnFlags(name: Name, required: FlagSet = EmptyFlags, excluded: FlagSet = EmptyFlags)(using Context): Denotation = {
      // We need a valid prefix for `asSeenFrom`
      val pre = this match {
        case tp: ClassInfo => tp.appliedRef
        case _ => widenIfUnstable
      }
      findMember(name, pre, required, excluded)
    }

    /** Find member of this type with given `name`, all `required`
     *  flags and no `excluded` flag and produce a denotation that contains
     *  the type of the member as seen from given prefix `pre`.
     */
    final def findMember(name: Name, pre: Type, required: FlagSet = EmptyFlags, excluded: FlagSet = EmptyFlags)(using Context): Denotation = {
      @tailrec def go(tp: Type): Denotation = tp match {
        case tp: TermRef =>
          go (tp.underlying match {
            case mt: MethodType
            if mt.paramInfos.isEmpty && tp.symbol.is(StableRealizable) => mt.resultType
            case tp1 => tp1
          })
        case tp: TypeRef =>
          tp.denot match {
            case d: ClassDenotation => d.findMember(name, pre, required, excluded)
            case d => go(d.info)
          }
        case tp: AppliedType =>
          tp.tycon match {
            case tc: TypeRef =>
              if (tc.symbol.isClass) go(tc)
              else {
                val normed = tp.tryNormalize
                go(if (normed.exists) normed else tp.superType)
              }
            case tc: HKTypeLambda =>
              goApplied(tp, tc)
            case _ =>
              go(tp.superType)
          }
        case tp: ThisType => // ??? inline
          goThis(tp)
        case tp: RefinedType =>
          if (name eq tp.refinedName) goRefined(tp) else go(tp.parent)
        case tp: RecType =>
          goRec(tp)
        case tp: TypeParamRef =>
          goParam(tp)
        case tp: SuperType =>
          goSuper(tp)
        case tp: MatchType =>
          val normed = tp.tryNormalize
          go(if (normed.exists) normed else tp.underlying)
        case tp: TypeProxy =>
          go(tp.underlying)
        case tp: ClassInfo =>
          tp.cls.findMember(name, pre, required, excluded)
        case AndType(l, r) =>
          goAnd(l, r)
        case tp: OrType =>
          goOr(tp)
        case tp: JavaArrayType =>
          defn.ObjectType.findMember(name, pre, required, excluded)
        case err: ErrorType =>
          newErrorSymbol(pre.classSymbol orElse defn.RootClass, name, err.msg)
        case _ =>
          NoDenotation
      }
      def goRec(tp: RecType) =
        // TODO: change tp.parent to nullable or other values
        if ((tp.parent: Type | Null) == null) NoDenotation
        else if (tp eq pre) go(tp.parent)
        else {
          //println(s"find member $pre . $name in $tp")

          // We have to be careful because we might open the same (wrt eq) recursive type
          // twice during findMember which risks picking the wrong prefix in the `substRecThis(rt, pre)`
          // call below. To avoid this problem we do a defensive copy of the recursive
          // type first. But if we do this always we risk being inefficient and we ran into
          // stackoverflows when compiling pos/hk.scala under the refinement encoding
          // of hk-types. So we only do a copy if the type
          // is visited again in a recursive call to `findMember`, as tracked by `tp.opened`.
          // Furthermore, if this happens we mark the original recursive type with `openedTwice`
          // which means that we always defensively copy the type in the future. This second
          // measure is necessary because findMember calls might be cached, so do not
          // necessarily appear in nested order.
          // Without the defensive copy, Typer.scala fails to compile at the line
          //
          //      untpd.rename(lhsCore, setterName).withType(setterType), WildcardType)
          //
          // because the subtype check
          //
          //      ThisTree[Untyped]#ThisTree[Typed] <: Tree[Typed]
          //
          // fails (in fact it thinks the underlying type of the LHS is `Tree[Untyped]`.)
          //
          // Without the `openedTwice` trick, Typer.scala fails to Ycheck
          // at phase resolveSuper.
          val rt =
            if (tp.opened) { // defensive copy
              tp.openedTwice = true
              RecType(rt => tp.parent.substRecThis(tp, rt.recThis))
            }
            else tp
          rt.opened = true
          try go(rt.parent).mapInfo(_.substRecThis(rt, pre))
          finally
            if (!rt.openedTwice) rt.opened = false
        }

      def goRefined(tp: RefinedType) = {
        val pdenot = go(tp.parent)
        val pinfo = pdenot.info
        val rinfo = tp.refinedInfo
        if (name.isTypeName && !pinfo.isInstanceOf[ClassInfo]) { // simplified case that runs more efficiently
          val jointInfo =
            if rinfo.isInstanceOf[TypeAlias] && !ctx.mode.is(Mode.CheckBounds) then
              // In normal situations, the only way to "improve" on rinfo is to return an empty type bounds
              // So, we do not lose anything essential in "widening" to rinfo.
              // We need to compute the precise info only when checking for empty bounds
              // which is communicated by the CheckBounds mode.
              rinfo
            else if ctx.base.pendingMemberSearches.contains(name) then
              pinfo safe_& rinfo
            else
              pinfo recoverable_& rinfo
          pdenot.asSingleDenotation.derivedSingleDenotation(pdenot.symbol, jointInfo)
        }
        else
          val isRefinedMethod = rinfo.isInstanceOf[MethodOrPoly]
          val joint = pdenot.meet(
            new JointRefDenotation(NoSymbol, rinfo, Period.allInRun(ctx.runId), pre, isRefinedMethod),
            pre,
            safeIntersection = ctx.base.pendingMemberSearches.contains(name))
          joint match
            case joint: SingleDenotation
            if isRefinedMethod && rinfo <:< joint.info =>
              // use `rinfo` to keep the right parameter names for named args. See i8516.scala.
              joint.derivedSingleDenotation(joint.symbol, rinfo, pre, isRefinedMethod)
            case _ =>
              joint
      }

      def goApplied(tp: AppliedType, tycon: HKTypeLambda) =
        go(tycon.resType).mapInfo(info =>
          tycon.derivedLambdaAbstraction(tycon.paramNames, tycon.paramInfos, info).appliedTo(tp.args))

      def goThis(tp: ThisType) =
        val underlying = tp.underlying
        val d = go(underlying)
        if d.exists then
          if underlying.isInstanceOf[AndType] then
            // The underlying type of `this` is specified in a self type clause.
            // In this case we need to exclude all private members from `d` which are
            // not defined in the class of the `this` type. We could do this test
            // always, but the restriction to test only if `underlying` is an AndType
            // is made to save execution time in the common case. See i9844.scala for test cases.
            def qualifies(sd: SingleDenotation) =
              !sd.symbol.is(Private) || sd.symbol.owner == tp.cls
            d match
              case d: SingleDenotation => if qualifies(d) then d else NoDenotation
              case d => d.filterWithPredicate(qualifies)
          else d
        else
          // There is a special case to handle:
          //   trait Super { this: Sub => private class Inner {} println(this.Inner) }
          //   class Sub extends Super
          // When resolving Super.this.Inner, the normal logic goes to the self type and
          // looks for Inner from there. But this fails because Inner is private.
          // We fix the problem by having the following fallback case, which links up the
          // member in Super instead of Sub.
          // As an example of this in the wild, see
          // loadClassWithPrivateInnerAndSubSelf in ShowClassTests
          go(tp.cls.typeRef) orElse d

      def goParam(tp: TypeParamRef) = {
        val next = tp.underlying
        ctx.typerState.constraint.entry(tp) match {
          case bounds: TypeBounds if bounds ne next =>
            go(bounds.hi)
          case _ =>
            go(next)
        }
      }

      def goSuper(tp: SuperType) = go(tp.underlying) match {
        case d: JointRefDenotation =>
          typr.println(i"redirecting super.$name from $tp to ${d.symbol.showLocated}")
          new UniqueRefDenotation(d.symbol, tp.memberInfo(d.symbol), d.validFor, pre)
        case d => d
      }

      def goAnd(l: Type, r: Type) =
        go(l).meet(go(r), pre, safeIntersection = ctx.base.pendingMemberSearches.contains(name))

      def goOr(tp: OrType) =
        inline def searchAfterJoin =
          // we need to keep the invariant that `pre <: tp`. Branch `union-types-narrow-prefix`
          // achieved that by narrowing `pre` to each alternative, but it led to merge errors in
          // lots of places. The present strategy is instead of widen `tp` using `join` to be a
          // supertype of `pre`.
          go(tp.join)

        if Nullables.unsafeNullsEnabled then tp match
          case OrNull(tp1) if tp1 <:< defn.ObjectType  =>
            // Selecting `name` from a type `T | Null` is like selecting `name` from `T`, if
            // unsafeNulls is enabled and T is a subtype of AnyRef.
            // This can throw at runtime, but we trade soundness for usability.
            tp1.findMember(name, pre.stripNull, required, excluded)
          case _ =>
            searchAfterJoin
        else searchAfterJoin

      val recCount = ctx.base.findMemberCount
      if (recCount >= Config.LogPendingFindMemberThreshold)
        ctx.base.pendingMemberSearches = name :: ctx.base.pendingMemberSearches
      ctx.base.findMemberCount = recCount + 1
      try go(this)
      catch {
        case ex: Throwable =>
          core.println(s"findMember exception for $this member $name, pre = $pre, recCount = $recCount")

          def showPrefixSafely(pre: Type)(using Context): String = pre.stripTypeVar match {
            case pre: TermRef => i"${pre.symbol.name}."
            case pre: TypeRef => i"${pre.symbol.name}#"
            case pre: TypeProxy => showPrefixSafely(pre.underlying)
            case _ => if (pre.typeSymbol.exists) i"${pre.typeSymbol.name}#" else "."
          }

          handleRecursive("find-member", i"${showPrefixSafely(pre)}$name", ex)
      }
      finally {
        if (recCount >= Config.LogPendingFindMemberThreshold)
          ctx.base.pendingMemberSearches = ctx.base.pendingMemberSearches.tail
        ctx.base.findMemberCount = recCount
      }
    }

    /** The set of names of members of this type that pass the given name filter
     *  when seen as members of `pre`. More precisely, these are all
     *  of members `name` such that `keepOnly(pre, name)` is `true`.
     *  @note: OK to use a Set[Name] here because Name hashcodes are replayable,
     *         hence the Set will always give the same names in the same order.
     */
    final def memberNames(keepOnly: NameFilter, pre: Type = this)(using Context): Set[Name] = this match {
      case tp: ClassInfo =>
        val names = tp.cls.classDenot.memberNames(keepOnly)
        if keepOnly.isStable then names else names.filter(keepOnly(pre, _))
      case tp: RefinedType =>
        val ns = tp.parent.memberNames(keepOnly, pre)
        if (keepOnly(pre, tp.refinedName)) ns + tp.refinedName else ns
      case tp: TypeProxy =>
        tp.underlying.memberNames(keepOnly, pre)
      case tp: AndType =>
        tp.tp1.memberNames(keepOnly, pre) | tp.tp2.memberNames(keepOnly, pre)
      case tp: OrType =>
        tp.tp1.memberNames(keepOnly, pre) & tp.tp2.memberNames(keepOnly, pre)
      case _ =>
        Set()
    }

    def memberDenots(keepOnly: NameFilter, f: (Name, mutable.Buffer[SingleDenotation]) => Unit)(using Context): Seq[SingleDenotation] = {
      val buf = mutable.ListBuffer[SingleDenotation]()
      for (name <- memberNames(keepOnly)) f(name, buf)
      buf.toList
    }

    /** The set of abstract term members of this type. */
    final def abstractTermMembers(using Context): Seq[SingleDenotation] = {
      record("abstractTermMembers")
      memberDenots(abstractTermNameFilter,
          (name, buf) => buf ++= nonPrivateMember(name).altsWith(_.is(Deferred)))
    }

    /**
     * Returns the set of methods that are abstract and do not overlap with any of
     * [[java.lang.Object]] methods.
     *
     * Conceptually, a SAM (functional interface) has exactly one abstract method.
     * If an interface declares an abstract method overriding one of the public
     * methods of [[java.lang.Object]], that also does not count toward the interface's
     * abstract method count.
     *
     * @see https://docs.oracle.com/javase/8/docs/api/java/lang/FunctionalInterface.html
     *
     * @return the set of methods that are abstract and do not match any of [[java.lang.Object]]
     *
     */
    final def possibleSamMethods(using Context): Seq[SingleDenotation] = {
      record("possibleSamMethods")
      atPhaseNoLater(erasurePhase) {
        abstractTermMembers.toList.filterConserve { m =>
          !m.symbol.matchingMember(defn.ObjectType).exists
          && !m.symbol.isSuperAccessor
          && !m.symbol.isInlineMethod
        }
      }.map(_.current)
    }

    /** The set of abstract type members of this type. */
    final def abstractTypeMembers(using Context): Seq[SingleDenotation] = {
      record("abstractTypeMembers")
      memberDenots(abstractTypeNameFilter,
          (name, buf) => buf += nonPrivateMember(name).asSingleDenotation)
    }

    /** The set of abstract type members of this type. */
    final def nonClassTypeMembers(using Context): Seq[SingleDenotation] = {
      record("nonClassTypeMembers")
      memberDenots(nonClassTypeNameFilter,
          (name, buf) => buf += member(name).asSingleDenotation)
    }

    /** The set of type alias members of this type */
    final def typeAliasMembers(using Context): Seq[SingleDenotation] = {
      record("typeAliasMembers")
      memberDenots(typeAliasNameFilter,
          (name, buf) => buf += member(name).asSingleDenotation)
    }

    /** The set of type members of this type */
    final def typeMembers(using Context): Seq[SingleDenotation] = {
      record("typeMembers")
      memberDenots(typeNameFilter,
          (name, buf) => buf += member(name).asSingleDenotation)
    }

    /** The set of implicit term members of this type
     *  @param kind   A subset of {Implicit, Given} that specifies what kind of implicit should
     *                be returned
     */
    final def implicitMembers(using Context): List[TermRef] = {
      record("implicitMembers")
      memberDenots(implicitFilter,
          (name, buf) => buf ++= member(name).altsWith(_.isOneOf(GivenOrImplicitVal)))
        .toList.map(d => TermRef(this, d.symbol.asTerm))
    }

    /** The set of member classes of this type */
    final def memberClasses(using Context): Seq[SingleDenotation] = {
      record("memberClasses")
      memberDenots(typeNameFilter,
        (name, buf) => buf ++= member(name).altsWith(x => x.isClass))
    }

    final def fields(using Context): Seq[SingleDenotation] = {
      record("fields")
      memberDenots(fieldFilter,
        (name, buf) => buf ++= member(name).altsWith(x => !x.is(Method)))
    }

    /** The set of members of this type that have all of `required` flags but none of `excluded` flags set. */
    final def membersBasedOnFlags(required: FlagSet, excluded: FlagSet)(using Context): Seq[SingleDenotation] = {
      record("membersBasedOnFlags")
      memberDenots(takeAllFilter,
        (name, buf) => buf ++= memberBasedOnFlags(name, required, excluded).alternatives)
    }

    /** All members of this type. Warning: this can be expensive to compute! */
    final def allMembers(using Context): Seq[SingleDenotation] = {
      record("allMembers")
      memberDenots(takeAllFilter, (name, buf) => buf ++= member(name).alternatives)
    }

    /** The info of `sym`, seen as a member of this type. */
    final def memberInfo(sym: Symbol)(using Context): Type =
      sym.info.asSeenFrom(this, sym.owner)

    /** This type seen as if it were the type of a member of prefix type `pre`
     *  declared in class `cls`.
     */
    final def asSeenFrom(pre: Type, cls: Symbol)(using Context): Type = {
      record("asSeenFrom")
      if (!cls.membersNeedAsSeenFrom(pre)) this
      else TypeOps.asSeenFrom(this, pre, cls)
    }

// ----- Subtype-related --------------------------------------------

    /** Is this type a subtype of that type? */
    final def <:<(that: Type)(using Context): Boolean = {
      record("<:<")
      TypeComparer.topLevelSubType(this, that)
    }

    /** Is this type a subtype of that type? */
    final def frozen_<:<(that: Type)(using Context): Boolean = {
      record("frozen_<:<")
      TypeComparer.isSubTypeWhenFrozen(this, that)
    }

    /** Is this type the same as that type?
     *  This is the case iff `this <:< that` and `that <:< this`.
     */
    final def =:=(that: Type)(using Context): Boolean = {
      record("=:=")
      TypeComparer.isSameType(this, that)
    }

    final def frozen_=:=(that: Type)(using Context): Boolean =
      TypeComparer.isSameTypeWhenFrozen(this, that)

    /** Is this type a primitive value type which can be widened to the primitive value type `that`? */
    def isValueSubType(that: Type)(using Context): Boolean = widenDealias match
      case self: TypeRef if self.symbol.isPrimitiveValueClass =>
        that.widenExpr.dealias match
          case that: TypeRef if that.symbol.isPrimitiveValueClass =>
            defn.isValueSubClass(self.symbol, that.symbol)
          case _ =>
            false
      case _ =>
        false

    def relaxed_<:<(that: Type)(using Context): Boolean =
      (this <:< that) || (this isValueSubType that)

    /** Is this type a legal type for member `sym1` that overrides another
     *  member `sym2` of type `that`? This is the same as `<:<`, except that
     *  @param relaxedCheck   if true type `Null` becomes a subtype of non-primitive value types in TypeComparer.
     *  @param matchLoosely   if true the types `=> T` and `()T` are seen as overriding each other.
     *  @param checkClassInfo if true we check that ClassInfos are within bounds of abstract types
     */
    final def overrides(that: Type, relaxedCheck: Boolean, matchLoosely: => Boolean, checkClassInfo: Boolean = true)(using Context): Boolean = {
      val overrideCtx = if relaxedCheck then ctx.relaxedOverrideContext else ctx
      inContext(overrideCtx) {
        !checkClassInfo && this.isInstanceOf[ClassInfo]
        || (this.widenExpr frozen_<:< that.widenExpr)
        || matchLoosely && {
            val this1 = this.widenNullaryMethod
            val that1 = that.widenNullaryMethod
            ((this1 `ne` this) || (that1 `ne` that)) && this1.overrides(that1, relaxedCheck, false, checkClassInfo)
          }
      }
    }

    /** Is this type close enough to that type so that members
     *  with the two types would override each other?
     *  This means:
     *    - Either both types are polytypes with the same number of
     *      type parameters and their result types match after renaming
     *      corresponding type parameters
     *    - Or both types are method types with =:=-equivalent(*) parameter types
     *      and matching result types after renaming corresponding parameter types
     *      if the method types are dependent.
     *    - Or both types are =:=-equivalent
     *    - Or phase.erasedTypes is false, and neither type takes
     *      term or type parameters.
     *
     *  (*) when matching with a Java method, we also regard Any and Object as equivalent
     *      parameter types.
     *
     *  Under explicit nulls, this function will always use unsafe-nulls semamtics to
     *  check the types. This is because we are using a relaxed rule (ignoring `Null` types)
     *  to check overriding Java methods.
     */
    def matches(that: Type)(using Context): Boolean = {
      record("matches")
      val overrideCtx = if ctx.explicitNulls then ctx.relaxedOverrideContext else ctx
      TypeComparer.matchesType(this, that, relaxed = !ctx.phase.erasedTypes)(using overrideCtx)
    }

    /** This is the same as `matches` except that it also matches => T with T and
     *  vice versa.
     */
    def matchesLoosely(that: Type)(using Context): Boolean =
      (this matches that) || {
        val thisResult = this.widenExpr
        val thatResult = that.widenExpr
        (this eq thisResult) != (that eq thatResult) && (thisResult matchesLoosely thatResult)
      }

    /** The basetype of this type with given class symbol, NoType if `base` is not a class. */
    final def baseType(base: Symbol)(using Context): Type = {
      record("baseType")
      base.denot match {
        case classd: ClassDenotation => classd.baseTypeOf(this)
        case _ => NoType
      }
    }

    def & (that: Type)(using Context): Type = {
      record("&")
      TypeComparer.glb(this, that)
    }

    /** Safer version of `&`.
     *
     *  This version does not simplify the bounds of the intersection of
     *  two TypeBounds. The simplification done by `&` requires subtyping checks
     *  which may end up calling `&` again, in most cases this should be safe
     *  but because of F-bounded types, this can result in an infinite loop
     *  (which will be masked unless `-Yno-deep-subtypes` is enabled).
     *  pos/i536 demonstrates that the infinite loop can also involve lower bounds.
     */
    def safe_& (that: Type)(using Context): Type = (this, that) match {
      case (TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) =>
        TypeBounds(
          OrType.makeHk(lo1.stripLazyRef, lo2.stripLazyRef),
          AndType.makeHk(hi1.stripLazyRef, hi2.stripLazyRef))
      case _ =>
        this & that
    }

    /** `this & that`, but handle CyclicReferences by falling back to `safe_&`.
     */
    def recoverable_&(that: Type)(using Context): Type =
      try this & that
      catch {
        case ex: CyclicReference => this safe_& that
          // A test case where this happens is tests/pos/i536.scala.
          // The & causes a subtype check which calls baseTypeRef again with the same
          // superclass.
      }

    def | (that: Type)(using Context): Type = {
      record("|")
      TypeComparer.lub(this, that)
    }

// ----- Unwrapping types -----------------------------------------------

    /** Map a TypeVar to either its instance if it is instantiated, or its origin,
     *  if not, until the result is no longer a TypeVar. Identity on all other types.
     */
    def stripTypeVar(using Context): Type = this

    /** Remove all AnnotatedTypes wrapping this type.
     */
    def stripAnnots(using Context): Type = this

    /** Strip TypeVars and Annotation wrappers */
    def stripped(using Context): Type = this

    def rewrapAnnots(tp: Type)(using Context): Type = tp.stripTypeVar match {
      case AnnotatedType(tp1, annot) => AnnotatedType(rewrapAnnots(tp1), annot)
      case _ => this
    }

    /** Strip PolyType prefixes */
    def stripPoly(using Context): Type = this match {
      case tp: PolyType => tp.resType.stripPoly
      case _ => this
    }

    /** Strip LazyRef wrappers */
    def stripLazyRef(using Context): Type = this match
      case lzy: LazyRef => lzy.ref.stripLazyRef
      case _ => this

    /** Widen from singleton type to its underlying non-singleton
     *  base type by applying one or more `underlying` dereferences,
     *  Also go from => T to T.
     *  Identity for all other types. Example:
     *
     *  class Outer { class C ; val x: C }
     *  def o: Outer
     *  <o.x.type>.widen = o.C
     */
    final def widen(using Context): Type = this match
      case _: TypeRef | _: MethodOrPoly => this // fast path for most frequent cases
      case tp: TermRef => // fast path for next most frequent case
        if tp.isOverloaded then tp else tp.underlying.widen
      case tp: SingletonType => tp.underlying.widen
      case tp: ExprType => tp.resultType.widen
      case tp =>
        val tp1 = tp.stripped
        if tp1 eq tp then tp
        else
          val tp2 = tp1.widen
          if tp2 ne tp1 then tp2 else tp

    /** Widen from singleton type to its underlying non-singleton
     *  base type by applying one or more `underlying` dereferences.
     */
    final def widenSingleton(using Context): Type = stripped match {
      case tp: SingletonType if !tp.isOverloaded => tp.underlying.widenSingleton
      case _ => this
    }

    /** Widen from TermRef to its underlying non-termref
     *  base type, while also skipping Expr types.
     */
    final def widenTermRefExpr(using Context): Type = stripTypeVar match {
      case tp: TermRef if !tp.isOverloaded => tp.underlying.widenExpr.widenTermRefExpr
      case _ => this
    }

    /** Widen from ExprType type to its result type.
     *  (Note: no stripTypeVar needed because TypeVar's can't refer to ExprTypes.)
     */
    final def widenExpr: Type = this match {
      case tp: ExprType => tp.resType
      case _ => this
    }

    /** Widen type if it is unstable (i.e. an ExprType, or TermRef to unstable symbol */
    final def widenIfUnstable(using Context): Type = stripTypeVar match {
      case tp: ExprType => tp.resultType.widenIfUnstable
      case tp: TermRef if tp.symbol.exists && !tp.symbol.isStableMember => tp.underlying.widenIfUnstable
      case _ => this
    }

    /** If this is a skolem, its underlying type, otherwise the type itself */
    final def widenSkolem(using Context): Type = this match {
      case tp: SkolemType => tp.underlying
      case _ => this
    }

    /** Widen this type and if the result contains embedded soft union types, replace
     *  them by their joins.
     *  "Embedded" means: inside type lambdas, intersections or recursive types,
     *  in prefixes of refined types, or in hard union types.
     *  If an embedded soft union is found, we first try to simplify or eliminate it by
     *  re-lubbing it while allowing type parameters to be constrained further.
     *  Any remaining union types are replaced by their joins.
     *
     *  For instance, if `A` is an unconstrained type variable, then
     *
     * 	      ArrayBuffer[Int] | ArrayBuffer[A]
     *
     *  is approximated by constraining `A` to be =:= to `Int` and returning `ArrayBuffer[Int]`
     *  instead of `ArrayBuffer[? >: Int | A <: Int & A]`
     *
     *  Exception (if `-YexplicitNulls` is set): if this type is a nullable union (i.e. of the form `T | Null`),
     *  then the top-level union isn't widened. This is needed so that type inference can infer nullable types.
     */
    def widenUnion(using Context): Type = widen match
      case tp @ OrNull(tp1): OrType =>
        // Don't widen `T|Null`, since otherwise we wouldn't be able to infer nullable unions.
        val tp1Widen = tp1.widenUnionWithoutNull
        if (tp1Widen.isRef(defn.AnyClass)) tp1Widen
        else tp.derivedOrType(tp1Widen, defn.NullType)
      case tp =>
        tp.widenUnionWithoutNull

    /** Overridden in OrType */
    def widenUnionWithoutNull(using Context): Type = widen match
      case tp: AndType =>
        tp.derivedAndType(tp.tp1.widenUnionWithoutNull, tp.tp2.widenUnionWithoutNull)
      case tp: RefinedType =>
        tp.derivedRefinedType(tp.parent.widenUnion, tp.refinedName, tp.refinedInfo)
      case tp: RecType =>
        tp.rebind(tp.parent.widenUnion)
      case tp: HKTypeLambda =>
        tp.derivedLambdaType(resType = tp.resType.widenUnion)
      case tp =>
        tp

    /** Widen all top-level singletons reachable
     *  by going to the operands of & and |.
     *  Overridden and cached in OrType.
     */
    def widenSingletons(using Context): Type = this match {
      case tp: SingletonType =>
        tp.widen
      case tp: OrType =>
        val tp1w = tp.widenSingletons
        if (tp1w eq tp) this else tp1w
      case tp: AndType =>
        val tp1w = tp.tp1.widenSingletons
        val tp2w = tp.tp2.widenSingletons
        if ((tp.tp1 eq tp1w) && (tp.tp2 eq tp2w)) this else tp1w & tp2w
      case _ =>
        this
    }

    /** If this is a nullary method type, its result type */
    def widenNullaryMethod(using Context): Type = this match
      case tp @ MethodType(Nil) => tp.resType
      case _ => this

    /** The singleton types that must or may be in this type. @see Atoms.
     *  Overridden and cached in OrType.
     */
    def atoms(using Context): Atoms =
      def normalize(tp: Type): Type = tp match
        case tp: SingletonType =>
          tp.underlying.dealias match
            case tp1: SingletonType => normalize(tp1)
            case _ =>
              tp match
                case tp: TermRef => tp.derivedSelect(normalize(tp.prefix))
                case _ => tp
        case _ => tp

      def single(tp: Type): Atoms =
        if tp.isStable then
          val set = Set.empty + normalize(tp)
          Atoms.Range(set, set)
        else Atoms.Unknown

      dealias match
        case tp: SingletonType =>
          tp.underlying.atoms match
            case as @ Atoms.Range(lo, hi) =>
              if hi.size == 1 then as // if there's just one atom, there's no uncertainty which one it is
              else Atoms.Range(Set.empty, hi)
            case Atoms.Unknown =>
              single(tp)
        case tp: ExprType => tp.resType.atoms
        case tp: OrType => tp.atoms // `atoms` overridden in OrType
        case tp: AndType => tp.tp1.atoms & tp.tp2.atoms
        case tp: TypeRef if tp.symbol.is(ModuleClass) =>
          // The atom of a module class is the module itself,
          // this corresponds to the special case in TypeComparer
          // which ensures that `X$ <:< X.type` returns true.
          single(tp.symbol.companionModule.termRef.asSeenFrom(tp.prefix, tp.symbol.owner))
        case tp: TypeProxy =>
          tp.underlying.atoms match
            case Atoms.Range(_, hi) => Atoms.Range(Set.empty, hi)
            case Atoms.Unknown => Atoms.Unknown
        case _ => Atoms.Unknown

    private def dealias1(keep: AnnotatedType => Context ?=> Boolean, keepOpaques: Boolean)(using Context): Type = this match {
      case tp: TypeRef =>
        if (tp.symbol.isClass) tp
        else tp.info match {
          case TypeAlias(alias) if !(keepOpaques && tp.symbol.is(Opaque)) =>
            alias.dealias1(keep, keepOpaques)
          case _ => tp
        }
      case app @ AppliedType(tycon, _) =>
        val tycon1 = tycon.dealias1(keep, keepOpaques)
        if (tycon1 ne tycon) app.superType.dealias1(keep, keepOpaques)
        else this
      case tp: TypeVar =>
        val tp1 = tp.instanceOpt
        if (tp1.exists) tp1.dealias1(keep, keepOpaques) else tp
      case tp: AnnotatedType =>
        val tp1 = tp.parent.dealias1(keep, keepOpaques)
        if keep(tp) then tp.derivedAnnotatedType(tp1, tp.annot) else tp1
      case tp: LazyRef =>
        tp.ref.dealias1(keep, keepOpaques)
      case _ => this
    }

    /** Follow aliases and dereferences LazyRefs, annotated types and instantiated
     *  TypeVars until type is no longer alias type, annotated type, LazyRef,
     *  or instantiated type variable.
     */
    final def dealias(using Context): Type = dealias1(keepNever, keepOpaques = false)

    /** Follow aliases and dereferences LazyRefs and instantiated TypeVars until type
     *  is no longer alias type, LazyRef, or instantiated type variable.
     *  Goes through annotated types and rewraps annotations on the result.
     */
    final def dealiasKeepAnnots(using Context): Type = dealias1(keepAlways, keepOpaques = false)

    /** Like `dealiasKeepAnnots`, but keeps only refining annotations */
    final def dealiasKeepRefiningAnnots(using Context): Type = dealias1(keepIfRefining, keepOpaques = false)

    /** Follow non-opaque aliases and dereferences LazyRefs, annotated types and instantiated
     *  TypeVars until type is no longer alias type, annotated type, LazyRef,
     *  or instantiated type variable.
     */
    final def dealiasKeepOpaques(using Context): Type = dealias1(keepNever, keepOpaques = true)

    /** Approximate this type with a type that does not contain skolem types. */
    final def deskolemized(using Context): Type =
      val deskolemizer = new ApproximatingTypeMap {
        def apply(tp: Type) = /*trace(i"deskolemize($tp) at $variance", show = true)*/
          tp match {
            case tp: SkolemType => range(defn.NothingType, atVariance(1)(apply(tp.info)))
            case _ => mapOver(tp)
          }
      }
      deskolemizer(this)

    /** The result of normalization using `tryNormalize`, or the type itself if
     *  tryNormlize yields NoType
     */
    final def normalized(using Context): Type = {
      val normed = tryNormalize
      if (normed.exists) normed else this
    }

    /** If this type can be normalized at the top-level by rewriting match types
     *  of S[n] types, the result after applying all toplevel normalizations,
     *  otherwise NoType
     */
    def tryNormalize(using Context): Type = NoType

    private def widenDealias1(keep: AnnotatedType => Context ?=> Boolean)(using Context): Type = {
      val res = this.widen.dealias1(keep, keepOpaques = false)
      if (res eq this) res else res.widenDealias1(keep)
    }

    /** Perform successive widenings and dealiasings until none can be applied anymore */
    final def widenDealias(using Context): Type = widenDealias1(keepNever)

    /** Perform successive widenings and dealiasings while rewrapping annotations, until none can be applied anymore */
    final def widenDealiasKeepAnnots(using Context): Type = widenDealias1(keepAlways)

    /** Perform successive widenings and dealiasings while rewrapping refining annotations, until none can be applied anymore */
    final def widenDealiasKeepRefiningAnnots(using Context): Type = widenDealias1(keepIfRefining)

    /** Widen from constant type to its underlying non-constant
     *  base type.
     */
    final def deconst(using Context): Type = stripTypeVar match {
      case tp: ConstantType => tp.value.tpe
      case _ => this
    }

    /** Dealias, and if result is a dependent function type, drop the `apply` refinement. */
    final def dropDependentRefinement(using Context): Type = dealias match {
      case RefinedType(parent, nme.apply, _) => parent
      case tp => tp
    }

    /** The type constructor of an applied type, otherwise the type itself */
    final def typeConstructor(using Context): Type = this match {
      case AppliedType(tycon, _) => tycon
      case _ => this
    }

    /** If this is a (possibly aliased, annotated, and/or parameterized) reference to
     *  a class, the class type ref, otherwise NoType.
     *  @param  refinementOK   If `true` we also skip refinements.
     */
    def underlyingClassRef(refinementOK: Boolean)(using Context): Type = dealias match {
      case tp: TypeRef =>
        if (tp.symbol.isClass) tp
        else if (tp.symbol.isAliasType) tp.underlying.underlyingClassRef(refinementOK)
        else NoType
      case tp: AppliedType =>
        if (tp.tycon.isLambdaSub) NoType
        else tp.superType.underlyingClassRef(refinementOK)
      case tp: AnnotatedType =>
        tp.underlying.underlyingClassRef(refinementOK)
      case tp: RefinedType =>
        if (refinementOK) tp.underlying.underlyingClassRef(refinementOK) else NoType
      case tp: RecType =>
        tp.underlying.underlyingClassRef(refinementOK)
      case _ =>
        NoType
    }

    /** The iterator of underlying types as long as type is a TypeProxy.
     *  Useful for diagnostics
     */
    def underlyingIterator(using Context): Iterator[Type] = new Iterator[Type] {
      var current = Type.this
      var hasNext = true
      def next = {
        val res = current
        hasNext = current.isInstanceOf[TypeProxy]
        if (hasNext) current = current.asInstanceOf[TypeProxy].underlying
        res
      }
    }

    /** A prefix-less refined this or a termRef to a new skolem symbol
     *  that has the given type as info.
     */
    def narrow(using Context): TermRef =
      TermRef(NoPrefix, newSkolem(this))

    /** Useful for diagnostics: The underlying type if this type is a type proxy,
     *  otherwise NoType
     */
    def underlyingIfProxy(using Context): Type = this match {
      case this1: TypeProxy => this1.underlying
      case _ => NoType
    }

    /** If this is a repeated type, its element type, otherwise the type itself */
    def repeatedToSingle(using Context): Type = this match {
      case tp @ ExprType(tp1) => tp.derivedExprType(tp1.repeatedToSingle)
      case _                  => if (isRepeatedParam) this.argTypesHi.head else this
    }

    // ----- Normalizing typerefs over refined types ----------------------------

    /** If this normalizes* to a refinement type that has a refinement for `name` (which might be followed
     *  by other refinements), and the refined info is a type alias, return the alias,
     *  otherwise return NoType. Used to reduce types of the form
     *
     *    P { ... type T = / += / -= U ... } # T
     *
     *  to just U. Does not perform the reduction if the resulting type would contain
     *  a reference to the "this" of the current refined type, except in the following situation
     *
     *  (1) The "this" reference can be avoided by following an alias. Example:
     *
     *      P { type T = String, type R = P{...}.T } # R  -->  String
     *
     *  (*) normalizes means: follow instantiated typevars and aliases.
     */
    def lookupRefined(name: Name)(using Context): Type = {
      @tailrec def loop(pre: Type): Type = pre.stripTypeVar match {
        case pre: RefinedType =>
          pre.refinedInfo match {
            case tp: AliasingBounds =>
              if (pre.refinedName ne name) loop(pre.parent) else tp.alias
            case _ =>
              loop(pre.parent)
          }
        case pre: RecType =>
          val candidate = pre.parent.lookupRefined(name)
          if (candidate.exists && !pre.isReferredToBy(candidate))
            //println(s"lookupRefined ${this.toString} . $name, pre: $pre ---> $candidate / ${candidate.toString}")
            candidate
          else NoType
        case SkolemType(tp) =>
          loop(tp)
        case pre: WildcardType =>
          WildcardType
        case pre: TypeRef =>
          pre.info match {
            case TypeAlias(alias) => loop(alias)
            case _ => NoType
          }
        case _ =>
          NoType
      }

      loop(this)
    }

    /** The type <this . name> , reduced if possible */
    def select(name: Name)(using Context): Type =
      NamedType(this, name, member(name)).reduceProjection

    /** The type <this . name> with given denotation, reduced if possible. */
    def select(name: Name, denot: Denotation)(using Context): Type =
      NamedType(this, name, denot).reduceProjection

    /** The type <this . sym>, reduced if possible */
    def select(sym: Symbol)(using Context): Type =
      NamedType(this, sym).reduceProjection

    def select(name: TermName)(using Context): TermRef =
      TermRef(this, name, member(name))

    def select(name: TermName, sig: Signature, target: Name)(using Context): TermRef =
      TermRef(this, name, member(name).atSignature(sig, target, relaxed = !ctx.erasedTypes))

// ----- Access to parts --------------------------------------------

    /** The normalized prefix of this type is:
     *  For an alias type, the normalized prefix of its alias
     *  For all other named type and class infos: the prefix.
     *  Inherited by all other type proxies.
     *  `NoType` for all other types.
     */
    @tailrec final def normalizedPrefix(using Context): Type = this match {
      case tp: NamedType =>
        if (tp.symbol.info.isTypeAlias) tp.info.normalizedPrefix else tp.prefix
      case tp: ClassInfo =>
        tp.prefix
      case tp: TypeProxy =>
        tp.underlying.normalizedPrefix
      case _ =>
        NoType
    }

    /** The full parent types, including all type arguments */
    def parents(using Context): List[Type] = this match {
      case tp @ AppliedType(tycon, args) if tycon.typeSymbol.isClass =>
        tycon.parents.map(_.subst(tycon.typeSymbol.typeParams, args))
      case tp: TypeRef =>
        if (tp.info.isInstanceOf[TempClassInfo])
          tp.recomputeDenot()
            // We usually should have `!tp.info.isInstanceOf[TempClassInfo]` here, but
            // this can be falsified for code with illegal cyclic references. See neg/i7107.scala.
        tp.info.parents
      case tp: TypeProxy =>
        tp.superType.parents
      case _ => Nil
    }

    /** The first parent of this type, AnyRef if list of parents is empty */
    def firstParent(using Context): Type = parents match {
      case p :: _ => p
      case _ => defn.AnyType
    }

    /** The parameter types of a PolyType or MethodType, Empty list for others */
    final def paramInfoss(using Context): List[List[Type]] = stripPoly match {
      case mt: MethodType => mt.paramInfos :: mt.resultType.paramInfoss
      case _ => Nil
    }

    /** The parameter names of a PolyType or MethodType, Empty list for others */
    final def paramNamess(using Context): List[List[TermName]] = stripPoly match {
      case mt: MethodType => mt.paramNames :: mt.resultType.paramNamess
      case _ => Nil
    }

    /** The parameter types in the first parameter section of a generic type or MethodType, Empty list for others */
    final def firstParamTypes(using Context): List[Type] = stripPoly match {
      case mt: MethodType => mt.paramInfos
      case _ => Nil
    }

    /** The parameter names in the first parameter section of a generic type or MethodType, Empty list for others */
    final def firstParamNames(using Context): List[TermName] = stripPoly match {
      case mt: MethodType => mt.paramNames
      case _ => Nil
    }

    /** Is this either not a method at all, or a parameterless method? */
    final def isParameterless(using Context): Boolean = stripPoly match {
      case mt: MethodType => false
      case _ => true
    }

    /** Is this (an alias of) the `scala.Null` type? */
    final def isNullType(using Context) = isRef(defn.NullClass)

    /** Is this (an alias of) the `scala.Nothing` type? */
    final def isNothingType(using Context) = isRef(defn.NothingClass)

    /** The resultType of a LambdaType, or ExprType, the type itself for others */
    def resultType(using Context): Type = this

    /** The final result type of a PolyType, MethodType, or ExprType, after skipping
     *  all parameter sections, the type itself for all others.
     */
    def finalResultType(using Context): Type = resultType.stripPoly match {
      case mt: MethodType => mt.resultType.finalResultType
      case _ => resultType
    }

    /** Determine the expected function type from the prototype. If multiple
     *  function types are found in a union or intersection, their intersection
     *  is returned. If no function type is found, Any is returned.
     */
    def findFunctionType(using Context): Type = dealias match
      case tp: AndOrType =>
        tp.tp1.findFunctionType & tp.tp2.findFunctionType
      case t if defn.isNonRefinedFunction(t) =>
        t
      case t @ SAMType(_) =>
        t
      case _ =>
        defn.AnyType

    /** This type seen as a TypeBounds */
    final def bounds(using Context): TypeBounds = this match {
      case tp: TypeBounds => tp
      case ci: ClassInfo => TypeAlias(ci.appliedRef)
      case wc: WildcardType =>
        wc.optBounds match {
          case bounds: TypeBounds => bounds
          case NoType => TypeBounds.empty
        }
      case _ => TypeAlias(this)
    }

    /** The lower bound of a TypeBounds type, the type itself otherwise */
    def loBound: Type = this match {
      case tp: TypeBounds => tp.lo
      case _ => this
    }

    /** The upper bound of a TypeBounds type, the type itself otherwise */
    def hiBound: Type = this match {
      case tp: TypeBounds => tp.hi
      case _ => this
    }

    /** The type parameter with given `name`. This tries first `decls`
     *  in order not to provoke a cycle by forcing the info. If that yields
     *  no symbol it tries `member` as an alternative.
     */
    def typeParamNamed(name: TypeName)(using Context): Symbol =
      classSymbol.unforcedDecls.lookup(name) orElse member(name).symbol

    /** If this is a prototype with some ignored component, reveal one more
     *  layer of it. Otherwise the type itself.
     */
    def deepenProto(using Context): Type = this

    /** If this is a prototype with some ignored component, reveal it, and
     *  deepen the result transitively. Otherwise the type itself.
     */
    def deepenProtoTrans(using Context): Type = this

    /** If this is an ignored proto type, its underlying type, otherwise the type itself */
    def revealIgnored: Type = this

    /** If this is a proto type, WildcardType, otherwise the type itself */
    def dropIfProto: Type = this

    /** If this is an AndType, the number of factors, 1 for all other types */
    def andFactorCount: Int = 1

    /** If this is a OrType, the number of factors if that match `soft`,
     *  1 for all other types.
     */
    def orFactorCount(soft: Boolean): Int = 1

// ----- Substitutions -----------------------------------------------------

    /** Substitute all types that refer in their symbol attribute to
     *  one of the symbols in `from` by the corresponding types in `to`.
     */
    final def subst(from: List[Symbol], to: List[Type])(using Context): Type =
      if (from.isEmpty) this
      else {
        val from1 = from.tail
        if (from1.isEmpty) Substituters.subst1(this, from.head, to.head, null)
        else {
          val from2 = from1.tail
          if (from2.isEmpty) Substituters.subst2(this, from.head, to.head, from1.head, to.tail.head, null)
          else Substituters.subst(this, from, to, null)
        }
      }

    /** Substitute all types of the form `TypeParamRef(from, N)` by
     *  `TypeParamRef(to, N)`.
     */
    final def subst(from: BindingType, to: BindingType)(using Context): Type =
      Substituters.subst(this, from, to, null)

    /** Substitute all occurrences of `This(cls)` by `tp` */
    final def substThis(cls: ClassSymbol, tp: Type)(using Context): Type =
      Substituters.substThis(this, cls, tp, null)

    /** As substThis, but only is class is a static owner (i.e. a globally accessible object) */
    final def substThisUnlessStatic(cls: ClassSymbol, tp: Type)(using Context): Type =
      if (cls.isStaticOwner) this else Substituters.substThis(this, cls, tp, null)

    /** Substitute all occurrences of `RecThis(binder)` by `tp` */
    final def substRecThis(binder: RecType, tp: Type)(using Context): Type =
      Substituters.substRecThis(this, binder, tp, null)

    /** Substitute a bound type by some other type */
    final def substParam(from: ParamRef, to: Type)(using Context): Type =
      Substituters.substParam(this, from, to, null)

    /** Substitute bound types by some other types */
    final def substParams(from: BindingType, to: List[Type])(using Context): Type =
      Substituters.substParams(this, from, to, null)

    /** Substitute all occurrences of symbols in `from` by references to corresponding symbols in `to`
     */
    final def substSym(from: List[Symbol], to: List[Symbol])(using Context): Type =
      Substituters.substSym(this, from, to, null)

    /** Substitute all occurrences of symbols in `from` by corresponding types in `to`.
     *  Unlike for `subst`, the `to` types can be type bounds. A TypeBounds target
     *  will be replaced by range that gets absorbed in an approximating type map.
     */
    final def substApprox(from: List[Symbol], to: List[Type])(using Context): Type =
      new Substituters.SubstApproxMap(from, to).apply(this)

// ----- misc -----------------------------------------------------------

    /** Turn type into a function type.
     *  @pre this is a method type without parameter dependencies.
     *  @param dropLast  The number of trailing parameters that should be dropped
     *                   when forming the function type.
     */
    def toFunctionType(isJava: Boolean, dropLast: Int = 0)(using Context): Type = this match {
      case mt: MethodType if !mt.isParamDependent =>
        val formals1 = if (dropLast == 0) mt.paramInfos else mt.paramInfos dropRight dropLast
        val isContextual = mt.isContextualMethod && !ctx.erasedTypes
        val isErased = mt.isErasedMethod && !ctx.erasedTypes
        val result1 = mt.nonDependentResultApprox match {
          case res: MethodType => res.toFunctionType(isJava)
          case res => res
        }
        val funType = defn.FunctionOf(
          formals1 mapConserve (_.translateFromRepeated(toArray = isJava)),
          result1, isContextual, isErased)
        if (mt.isResultDependent) RefinedType(funType, nme.apply, mt)
        else funType
    }

    /** The signature of this type. This is by default NotAMethod,
     *  but is overridden for PolyTypes, MethodTypes, and TermRef types.
     *  (the reason why we deviate from the "final-method-with-pattern-match-in-base-class"
     *   pattern is that method signatures use caching, so encapsulation
     *   is improved using an OO scheme).
     */
    def signature(using Context): Signature = Signature.NotAMethod

    /** Drop annotation of given `cls` from this type */
    def dropAnnot(cls: Symbol)(using Context): Type = stripTypeVar match {
      case self @ AnnotatedType(pre, annot) =>
        if (annot.symbol eq cls) pre
        else self.derivedAnnotatedType(pre.dropAnnot(cls), annot)
      case _ =>
        this
    }

    def dropRepeatedAnnot(using Context): Type = dropAnnot(defn.RepeatedAnnot)

    def annotatedToRepeated(using Context): Type = this match {
      case tp @ ExprType(tp1) => tp.derivedExprType(tp1.annotatedToRepeated)
      case AnnotatedType(tp, annot) if annot matches defn.RepeatedAnnot =>
        val typeSym = tp.typeSymbol.asClass
        assert(typeSym == defn.SeqClass || typeSym == defn.ArrayClass)
        tp.translateParameterized(typeSym, defn.RepeatedParamClass)
      case _ => this
    }

    /** The set of distinct symbols referred to by this type */
    def coveringSet(using Context): Set[Symbol] =
      (new CoveringSetAccumulator).apply(Set.empty[Symbol], this)

    /** The number of applications and refinements in this type */
    def typeSize(using Context): Int =
      (new TypeSizeAccumulator).apply(0, this)

    /** Convert to text */
    def toText(printer: Printer): Text = printer.toText(this)

    /** Utility method to show the underlying type of a TypeProxy chain together
     *  with the proxy type itself.
     */
    def showWithUnderlying(n: Int = 1)(using Context): String = this match {
      case tp: TypeProxy if n > 0 => s"$show with underlying ${tp.underlying.showWithUnderlying(n - 1)}"
      case _ => show
    }

    /** A simplified version of this type which is equivalent wrt =:= to this type.
     *  This applies a typemap to the type which (as all typemaps) follows type
     *  variable instances and reduces typerefs over refined types. It also
     *
     *   - re-evaluates all occurrences of And/OrType with &/| because
     *     what was a union or intersection of type variables might be a simpler type
     *     after the type variables are instantiated.
     *   - maps poly params in the current constraint set back to their type vars.
     *   - forces match types to be fully defined and tries to normalize them.
     *
     *  NOTE: Simplifying an intersection type might change its erasure (for
     *  example, the Java erasure of `Object & Serializable` is `Object`,
     *  but its simplification is `Serializable`). This means that simplification
     *  should never be used in a `MethodicType`, because that could
     *  lead to a different `signature`. Since this isn't very useful anyway,
     *  this method handles this by never simplifying inside a `MethodicType`.
     */
    def simplified(using Context): Type = TypeOps.simplify(this, null)

    /** Compare `this == that`, assuming corresponding binders in `bs` are equal.
     *  The normal `equals` should be equivalent to `equals(that, null`)`.
     *  We usually override `equals` when we override `iso` except if the
     *  `equals` comes from a case class, so it already has the right definition anyway.
     */
    final def equals(that: Any, bs: BinderPairs): Boolean =
      (this `eq` that.asInstanceOf[AnyRef]) || this.iso(that, bs)

    /** Is `this` isomorphic to `that`, assuming pairs of matching binders `bs`?
     *  It is assumed that `this.ne(that)`.
     */
    protected def iso(that: Any, bs: BinderPairs): Boolean = this.equals(that)

    /** Equality used for hash-consing; uses `eq` on all recursive invocations,
     *  except where a BindingType is involved. The latter demand a deep isomorphism check.
     */
    def eql(that: Type): Boolean = this.equals(that)

    /** customized hash code of this type.
     *  NotCached for uncached types. Cached types
     *  compute hash and use it as the type's hashCode.
     */
    def hash: Int

    /** Compute hashcode relative to enclosing binders `bs` */
    def computeHash(bs: Binders): Int

    /** Is the `hash` of this type the same for all possible sequences of enclosing binders? */
    def hashIsStable: Boolean = true
  }

  // end Type

// ----- Type categories ----------------------------------------------

  /** A marker trait for cached types */
  trait CachedType extends Type

  /** A marker trait for type proxies.
   *  Each implementation is expected to redefine the `underlying` method.
   */
  abstract class TypeProxy extends Type {

    /** The type to which this proxy forwards operations. */
    def underlying(using Context): Type

    /** The closest supertype of this type. This is the same as `underlying`,
     *  except that
     *    - instead of a TyperBounds type it returns its upper bound, and
     *    - for applied types it returns the upper bound of the constructor re-applied to the arguments.
     */
    def superType(using Context): Type = underlying match {
      case TypeBounds(_, hi) => hi
      case st => st
    }

    /** Same as superType, except for two differences:
     *   - opaque types are treated as transparent aliases
     *   - applied type are matchtype-reduced if possible
     *
     *  Note: the reason to reduce match type aliases here and not in `superType`
     *  is that `superType` is context-independent and cached, whereas matchtype
     *  reduction depends on context and should not be cached (at least not without
     *  the very specific cache invalidation condition for matchtypes).
     */
    def translucentSuperType(using Context): Type = superType
  }

  // Every type has to inherit one of the following four abstract type classes.,
  // which determine whether the type is cached, and whether
  // it is a proxy of some other type. The duplication in their methods
  // is for efficiency.

  /**  Instances of this class are cached and are not proxies. */
  abstract class CachedGroundType extends Type with CachedType {
    private var myHash = HashUnknown
    final def hash: Int = {
      if (myHash == HashUnknown) {
        myHash = computeHash(null)
        assert(myHash != HashUnknown)
      }
      myHash
    }
    override final def hashCode: Int =
      if (hash == NotCached) System.identityHashCode(this) else hash
  }

  /**  Instances of this class are cached and are proxies. */
  abstract class CachedProxyType extends TypeProxy with CachedType {
    protected var myHash: Int = HashUnknown
    final def hash: Int = {
      if (myHash == HashUnknown) {
        myHash = computeHash(null)
        assert(myHash != HashUnknown)
      }
      myHash
    }
    override final def hashCode: Int =
      if (hash == NotCached) System.identityHashCode(this) else hash
  }

  /**  Instances of this class are uncached and are not proxies. */
  abstract class UncachedGroundType extends Type {
    final def hash: Int = NotCached
    final def computeHash(bs: Binders): Int = NotCached
    if (monitored) {
      record(s"uncachable")
      record(s"uncachable: $getClass")
    }
  }

  /**  Instances of this class are uncached and are proxies. */
  abstract class UncachedProxyType extends TypeProxy {
    final def hash: Int = NotCached
    final def computeHash(bs: Binders): Int = NotCached
    if (monitored) {
      record(s"uncachable")
      record(s"uncachable: $getClass")
    }
  }

  /** A marker trait for types that apply only to type symbols */
  trait TypeType extends Type

  /** A marker trait for types that apply only to term symbols or that
   *  represent higher-kinded types.
   */
  trait TermType extends Type

  /** A marker trait for types that can be types of values or prototypes of value types */
  trait ValueTypeOrProto extends TermType

  /** A marker trait for types that can be types of values or that are higher-kinded  */
  trait ValueType extends ValueTypeOrProto

  /** A marker trait for types that are guaranteed to contain only a
   *  single non-null value (they might contain null in addition).
   */
  trait SingletonType extends TypeProxy with ValueType {
    def isOverloaded(using Context): Boolean = false
  }

  /** A trait for types that bind other types that refer to them.
   *  Instances are: LambdaType, RecType.
   */
  trait BindingType extends Type {

    /** If this type is in `bs`, a hashcode based on its position in `bs`.
     *  Otherise the standard identity hash.
     */
    override def identityHash(bs: Binders): Int = {
      def recur(n: Int, tp: BindingType, rest: Binders): Int =
        if (this `eq` tp) finishHash(hashing.mix(hashSeed, n), 1)
        else if (rest == null) System.identityHashCode(this)
        else recur(n + 1, rest.tp, rest.next)
      avoidSpecialHashes(
        if (bs == null) System.identityHashCode(this)
        else recur(1, bs.tp, bs.next))
    }

    def equalBinder(that: BindingType, bs: BinderPairs): Boolean =
      (this `eq` that) || bs != null && bs.matches(this, that)
  }

  /** A trait for proto-types, used as expected types in typer */
  trait ProtoType extends Type {
    def isMatchedBy(tp: Type, keepConstraint: Boolean = false)(using Context): Boolean
    def fold[T](x: T, ta: TypeAccumulator[T])(using Context): T
    def map(tm: TypeMap)(using Context): ProtoType

    /** If this prototype captures a context, the same prototype except that the result
     *  captures the given context `ctx`.
     */
    def withContext(ctx: Context): ProtoType = this

    override def dropIfProto = WildcardType
  }

  /** Implementations of this trait cache the results of `narrow`. */
  trait NarrowCached extends Type {
    private var myNarrow: TermRef | Null = null
    override def narrow(using Context): TermRef = {
      if (myNarrow == null) myNarrow = super.narrow
      myNarrow.nn
    }
  }

// --- NamedTypes ------------------------------------------------------------------

  abstract class NamedType extends CachedProxyType with ValueType { self =>

    type ThisType >: this.type <: NamedType
    type ThisName <: Name

    val prefix: Type
    def designator: Designator
    protected def designator_=(d: Designator): Unit

    assert(prefix.isValueType || (prefix eq NoPrefix), s"invalid prefix $prefix")

    private var myName: Name | Null = null
    private var lastDenotation: Denotation | Null = null
    private var lastSymbol: Symbol | Null = null
    private var checkedPeriod: Period = Nowhere
    private var myStableHash: Byte = 0
    private var mySignature: Signature = _
    private var mySignatureRunId: Int = NoRunId

    // Invariants:
    // (1) checkedPeriod != Nowhere     =>  lastDenotation != null
    // (2) lastDenotation != null       =>  lastSymbol != null
    // (3) mySignatureRunId != NoRunId  =>  mySignature != null

    def isType: Boolean = isInstanceOf[TypeRef]
    def isTerm: Boolean = isInstanceOf[TermRef]

    /** If designator is a name, this name. Otherwise, the original name
     *  of the designator symbol.
     */
    final def name(using Context): ThisName = {
      if (myName == null) myName = computeName
      myName.asInstanceOf[ThisName]
    }

    private def computeName: Name = designator match {
      case name: Name => name
      case sym: Symbol => sym.originDenotation.name
    }

    final override def signature(using Context): Signature =
      /** The signature computed from the last known denotation with `sigFromDenot`,
       *  or if there is none, the signature of the symbol. Signatures are always
       *  computed before erasure, since some symbols change their signature at erasure.
       */
      def computeSignature(using Context): Signature =
        val lastd = lastDenotation
        if lastd != null then sigFromDenot(lastd)
        else if ctx.erasedTypes then atPhase(erasurePhase)(computeSignature)
        else symbol.asSeenFrom(prefix).signature

      if ctx.runId != mySignatureRunId then
        mySignature = computeSignature
        if !mySignature.isUnderDefined then mySignatureRunId = ctx.runId
      mySignature
    end signature

    /** The signature computed from the current denotation with `sigFromDenot` if it is
     *  known without forcing.
     *  Otherwise the signature of the current symbol if it is known without forcing.
     *  Otherwise NotAMethod. Signatures are always computed before erasure, since
     *  some symbols change their signature at erasure.
     */
    private def currentSignature(using Context): Signature =
      if ctx.runId == mySignatureRunId then mySignature
      else
        val lastd = lastDenotation
        if lastd != null then sigFromDenot(lastd)
        else if ctx.erasedTypes then atPhase(erasurePhase)(currentSignature)
        else
          val sym = currentSymbol
          if sym.exists then sym.asSeenFrom(prefix).signature
          else Signature.NotAMethod

    /** The signature of a pre-erasure version of denotation `lastd`. */
    private def sigFromDenot(lastd: Denotation)(using Context) =
      if lastd.validFor.firstPhaseId <= erasurePhase.id then lastd.signature
      else lastd match
        case lastd: SingleDenotation => lastd.initial.signature
        case _ => Signature.OverloadedSignature

    final def symbol(using Context): Symbol =
      // We can rely on checkedPeriod (unlike in the definition of `denot` below)
      // because SymDenotation#installAfter never changes the symbol
      if (checkedPeriod == ctx.period) lastSymbol.nn else computeSymbol

    private def computeSymbol(using Context): Symbol =
      designator match {
        case sym: Symbol =>
          if (sym.isValidInCurrentRun) sym else denot.symbol
        case name =>
          (if (denotationIsCurrent) lastDenotation.nn else denot).symbol
      }

    /** There is a denotation computed which is valid (somewhere in) the
     *  current run.
     */
    def denotationIsCurrent(using Context): Boolean =
      lastDenotation != null && lastDenotation.uncheckedNN.validFor.runId == ctx.runId

    /** If the reference is symbolic or the denotation is current, its symbol, otherwise NoDenotation.
     *
     *  Note: This operation does not force the denotation, and is therefore
     *  timing dependent. It should only be used if the outcome of the
     *  essential computation does not depend on the symbol being present or not.
     *  It's currently used to take an optimized path in substituters and
     *  type accumulators, as well as to be safe in diagnostic printing.
     *  Normally, it's better to use `symbol`, not `currentSymbol`.
     */
    final def currentSymbol(using Context): Symbol = designator match {
      case sym: Symbol => sym
      case _ => if (denotationIsCurrent) lastDenotation.nn.symbol else NoSymbol
    }

    /** Retrieves currently valid symbol without necessarily updating denotation.
     *  Assumes that symbols do not change between periods in the same run.
     *  Used to get the class underlying a ThisType.
     */
    private[Types] def stableInRunSymbol(using Context): Symbol =
      if (checkedPeriod.runId == ctx.runId) lastSymbol.nn
      else symbol

    def info(using Context): Type = denot.info

    /** The denotation currently denoted by this type */
    final def denot(using Context): Denotation = {
      util.Stats.record("NamedType.denot")
      val now = ctx.period
      // Even if checkedPeriod == now we still need to recheck lastDenotation.validFor
      // as it may have been mutated by SymDenotation#installAfter
      if (checkedPeriod != Nowhere && lastDenotation.nn.validFor.contains(now)) {
        checkedPeriod = now
        lastDenotation.nn
      }
      else computeDenot
    }

    private def computeDenot(using Context): Denotation = {
      util.Stats.record("NamedType.computeDenot")

      def finish(d: Denotation) = {
        if (d.exists)
          // Avoid storing NoDenotations in the cache - we will not be able to recover from
          // them. The situation might arise that a type has NoDenotation in some later
          // phase but a defined denotation earlier (e.g. a TypeRef to an abstract type
          // is undefined after erasure.) We need to be able to do time travel back and
          // forth also in these cases.
          setDenot(d)
        d
      }

      def fromDesignator = designator match {
        case name: Name =>
          val sym = lastSymbol
          val allowPrivate = sym == null || (sym == NoSymbol) || sym.lastKnownDenotation.flagsUNSAFE.is(Private)
          finish(memberDenot(name, allowPrivate))
        case sym: Symbol =>
          val symd = sym.lastKnownDenotation
          if (symd.validFor.runId != ctx.runId && !stillValid(symd))
            finish(memberDenot(symd.initial.name, allowPrivate = false))
          else if (prefix.isArgPrefixOf(symd))
            finish(argDenot(sym.asType))
          else if (infoDependsOnPrefix(symd, prefix))
            finish(memberDenot(symd.initial.name, allowPrivate = symd.is(Private)))
          else
            finish(symd.current)
      }

      lastDenotation match {
        case lastd0: SingleDenotation =>
          val lastd = lastd0.skipRemoved
          if (lastd.validFor.runId == ctx.runId && (checkedPeriod != Nowhere)) finish(lastd.current)
          else lastd match {
            case lastd: SymDenotation =>
              if (stillValid(lastd) && (checkedPeriod != Nowhere)) finish(lastd.current)
              else finish(memberDenot(lastd.initial.name, allowPrivate = false))
            case _ =>
              fromDesignator
          }
        case _ => fromDesignator
      }
    }

    private def disambiguate(d: Denotation)(using Context): Denotation =
      disambiguate(d, currentSignature, currentSymbol.targetName)

    private def disambiguate(d: Denotation, sig: Signature | Null, target: Name)(using Context): Denotation =
      if (sig != null)
        d.atSignature(sig, target, relaxed = !ctx.erasedTypes) match {
          case d1: SingleDenotation => d1
          case d1 =>
            d1.atSignature(sig, target, relaxed = false) match {
              case d2: SingleDenotation => d2
              case d2 => d2.suchThat(currentSymbol.eq).orElse(d2)
            }
        }
      else d

    private def memberDenot(name: Name, allowPrivate: Boolean)(using Context): Denotation = {
      var d = memberDenot(prefix, name, allowPrivate)
      if (!d.exists && !allowPrivate && ctx.mode.is(Mode.Interactive))
        // In the IDE we might change a public symbol to private, and would still expect to find it.
        d = memberDenot(prefix, name, true)
      if (!d.exists && ctx.isAfterTyper && lastDenotation.isInstanceOf[SymDenotation])
        // name has changed; try load in earlier phase and make current
        d = atPhase(ctx.phaseId - 1)(memberDenot(name, allowPrivate)).current
      if (d.isOverloaded)
        d = disambiguate(d)
      d
    }

    private def memberDenot(prefix: Type, name: Name, allowPrivate: Boolean)(using Context): Denotation =
      if (allowPrivate) prefix.member(name) else prefix.nonPrivateMember(name)

    private def argDenot(param: TypeSymbol)(using Context): Denotation = {
      val cls = param.owner
      val args = prefix.baseType(cls).argInfos
      val typeParams = cls.typeParams

      def concretize(arg: Type, tparam: TypeSymbol) = arg match {
        case arg: TypeBounds => TypeRef(prefix, tparam)
        case arg => arg
      }
      val concretized = args.zipWithConserve(typeParams)(concretize)

      def rebase(arg: Type) = arg.subst(typeParams, concretized)

      val idx = typeParams.indexOf(param)

      if (0 <= idx && idx < args.length) {
        val argInfo = args(idx) match {
          case arg: TypeBounds =>
            val v = param.paramVarianceSign
            val pbounds = param.paramInfo
            if (v > 0 && pbounds.loBound.dealiasKeepAnnots.isExactlyNothing) TypeAlias(arg.hiBound & rebase(pbounds.hiBound))
            else if (v < 0 && pbounds.hiBound.dealiasKeepAnnots.isExactlyAny) TypeAlias(arg.loBound | rebase(pbounds.loBound))
            else arg recoverable_& rebase(pbounds)
          case arg => TypeAlias(arg)
        }
        param.derivedSingleDenotation(param, argInfo)
      }
      else {
        if (!ctx.reporter.errorsReported)
          throw new TypeError(
            i"""bad parameter reference $this at ${ctx.phase}
               |the parameter is ${param.showLocated} but the prefix $prefix
               |does not define any corresponding arguments.
               |idx = $idx, args = $args""")
        NoDenotation
      }
    }

    /** Reload denotation by computing the member with the reference's name as seen
     *  from the reference's prefix.
     */
    def recomputeDenot()(using Context): Unit =
      setDenot(memberDenot(name, allowPrivate = !symbol.exists || symbol.is(Private)))

    private def setDenot(denot: Denotation)(using Context): Unit = {
      if (Config.checkNoDoubleBindings)
        if (ctx.settings.YnoDoubleBindings.value)
          checkSymAssign(denot.symbol)

      lastDenotation = denot
      lastSymbol = denot.symbol
      checkedPeriod = if (prefix.isProvisional) Nowhere else ctx.period
      designator match {
        case sym: Symbol if designator ne lastSymbol.nn =>
          designator = lastSymbol.asInstanceOf[Designator{ type ThisName = self.ThisName }]
        case _ =>
      }
      checkDenot()
    }

    private def checkDenot()(using Context) = {}

    private def checkSymAssign(sym: Symbol)(using Context) = {
      def selfTypeOf(sym: Symbol) =
        if (sym.isClass) sym.asClass.givenSelfType else NoType
      val lastSym = lastSymbol
      assert(
        (lastSym == null)
        ||
        (lastSym eq sym)
        ||
        !denotationIsCurrent
        ||
        lastSym.infoOrCompleter.isInstanceOf[ErrorType]
        ||
        !sym.exists
        ||
        !lastSym.exists
        ||
        sym.isPackageObject // package objects can be visited before we get around to index them
        ||
        sym.owner != lastSym.owner &&
          (sym.owner.derivesFrom(lastSym.owner)
           ||
           selfTypeOf(sym).derivesFrom(lastSym.owner)
           ||
           selfTypeOf(lastSym).derivesFrom(sym.owner)
          )
        ||
        sym == defn.AnyClass.primaryConstructor, {
          if lastSym == null then
            s"""data race? overwriting $lastSym with $sym in type $this,
             |period = ${ctx.phase} at run ${ctx.runId}"""
          else
            s"""data race? overwriting $lastSym with $sym in type $this,
             |last sym id = ${lastSym.id}, new sym id = ${sym.id},
             |last owner = ${lastSym.owner}, new owner = ${sym.owner},
             |period = ${ctx.phase} at run ${ctx.runId}""" })
    }

    /** A reference with the initial symbol in `symd` has an info that
     *  might depend on the given prefix.
     */
    private def infoDependsOnPrefix(symd: SymDenotation, prefix: Type)(using Context): Boolean =
      symd.maybeOwner.membersNeedAsSeenFrom(prefix) && !symd.is(NonMember)
      || prefix.isInstanceOf[Types.ThisType] && symd.is(Opaque) // see pos/i11277.scala for a test where this matters

    /** Is this a reference to a class or object member? */
    def isMemberRef(using Context): Boolean = designator match {
      case sym: Symbol => infoDependsOnPrefix(sym, prefix)
      case _ => true
    }

    /** (1) Reduce a type-ref `W # X` or `W { ... } # U`, where `W` is a wildcard type
     *  to an (unbounded) wildcard type.
     *
     *  (2) Reduce a type-ref `T { X = U; ... } # X`  to   `U`
     *  provided `U` does not refer with a RecThis to the
     *  refinement type `T { X = U; ... }`
     */
    def reduceProjection(using Context): Type =
      if (isType) {
        val reduced = prefix.lookupRefined(name)
        if (reduced.exists) reduced else this
      }
      else this

    /** Guard against cycles that can arise if given `op`
     *  follows info. The problematic cases are a type alias to itself or
     *  bounded by itself or a val typed as itself:
     *
     *  type T <: T
     *  val x: x.type
     *
     *  These are errors but we have to make sure that operations do
     *  not loop before the error is detected.
     */
    final def controlled[T](op: => T)(using Context): T = try {
      ctx.base.underlyingRecursions += 1
      if (ctx.base.underlyingRecursions < Config.LogPendingUnderlyingThreshold)
        op
      else if (ctx.pendingUnderlying contains this)
        throw CyclicReference(symbol)
      else
        try {
          ctx.pendingUnderlying += this
          op
        }
        finally
          ctx.pendingUnderlying -= this
    }
    finally
      ctx.base.underlyingRecursions -= 1

    /** The argument corresponding to class type parameter `tparam` as seen from
     *  prefix `pre`. Can produce a TypeBounds type in case prefix is an & or | type
     *  and parameter is non-variant.
     */
    def argForParam(pre: Type)(using Context): Type = {
      val tparam = symbol
      val cls = tparam.owner
      val base = pre.baseType(cls)
      base match {
        case AppliedType(_, allArgs) =>
          var tparams = cls.typeParams
          var args = allArgs
          var idx = 0
          while (tparams.nonEmpty && args.nonEmpty) {
            if (tparams.head.eq(tparam))
              return args.head match {
                case _: TypeBounds => TypeRef(pre, tparam)
                case arg => arg
              }
            tparams = tparams.tail
            args = args.tail
            idx += 1
          }
          NoType
        case base: AndOrType =>
          var tp1 = argForParam(base.tp1)
          var tp2 = argForParam(base.tp2)
          val variance = tparam.paramVarianceSign
          if (isBounds(tp1) || isBounds(tp2) || variance == 0) {
            // compute argument as a type bounds instead of a point type
            tp1 = tp1.bounds
            tp2 = tp2.bounds
          }
          if (base.isAnd == variance >= 0) tp1 & tp2 else tp1 | tp2
        case _ =>
          if (pre.termSymbol.is(Package)) argForParam(pre.select(nme.PACKAGE))
          else if (pre.isExactlyNothing) pre
          else NoType
      }
    }

    /** A selection of the same kind, but with potentially a different prefix.
     *  The following normalizations are performed for type selections T#A:
     *
     *     T#A --> B                if A is bound to an alias `= B` in T
     *
     *  If Config.splitProjections is set:
     *
     *     (S & T)#A --> S#A        if T does not have a member named A
     *               --> T#A        if S does not have a member named A
     *               --> S#A & T#A  otherwise
     *     (S | T)#A --> S#A | T#A
     */
    def derivedSelect(prefix: Type)(using Context): Type =
      if (prefix eq this.prefix) this
      else if (prefix.isExactlyNothing) prefix
      else {
        if (isType) {
          val res =
            if (currentSymbol.isAllOf(ClassTypeParam)) argForParam(prefix)
            else prefix.lookupRefined(name)
          if (res.exists) return res
          if (Config.splitProjections)
            prefix match {
              case prefix: AndType =>
                def isMissing(tp: Type) = tp match {
                  case tp: TypeRef => !tp.info.exists
                  case _ => false
                }
                val derived1 = derivedSelect(prefix.tp1)
                val derived2 = derivedSelect(prefix.tp2)
                return (
                  if (isMissing(derived1)) derived2
                  else if (isMissing(derived2)) derived1
                  else prefix.derivedAndType(derived1, derived2))
              case prefix: OrType =>
                val derived1 = derivedSelect(prefix.tp1)
                val derived2 = derivedSelect(prefix.tp2)
                return prefix.derivedOrType(derived1, derived2)
              case _ =>
            }
        }
        if (prefix.isInstanceOf[WildcardType]) WildcardType
        else withPrefix(prefix)
      }

    /** A reference like this one, but with the given symbol, if it exists */
    final def withSym(sym: Symbol)(using Context): ThisType =
      if ((designator ne sym) && sym.exists) NamedType(prefix, sym).asInstanceOf[ThisType]
      else this

    /** A reference like this one, but with the given denotation, if it exists.
     *  Returns a new named type with the denotation's symbol if that symbol exists, and
     *  one of the following alternatives applies:
     *   1. The current designator is a symbol and the symbols differ, or
     *   2. The current designator is a name and the new symbolic named type
     *      does not have a currently known denotation.
     *   3. The current designator is a name and the new symbolic named type
     *      has the same info as the current info
     *  Otherwise the current denotation is overwritten with the given one.
     *
     *  Note: (2) and (3) are a "lock in mechanism" where a reference with a name as
     *  designator can turn into a symbolic reference.
     *
     *  Note: This is a subtle dance to keep the balance between going to symbolic
     *  references as much as we can (since otherwise we'd risk getting cycles)
     *  and to still not lose any type info in the denotation (since symbolic
     *  references often recompute their info directly from the symbol's info).
     *  A test case is neg/opaque-self-encoding.scala.
     */
    final def withDenot(denot: Denotation)(using Context): ThisType =
      if denot.exists then
        val adapted = withSym(denot.symbol)
        val result =
          if (adapted.eq(this)
              || designator.isInstanceOf[Symbol]
              || !adapted.denotationIsCurrent
              || adapted.info.eq(denot.info))
            adapted
          else this
        val lastDenot = result.lastDenotation
        denot match
          case denot: SymDenotation
          if denot.validFor.firstPhaseId < ctx.phase.id
            && lastDenot != null
            && lastDenot.validFor.lastPhaseId > denot.validFor.firstPhaseId
            && !lastDenot.isInstanceOf[SymDenotation] =>
            // In this case the new SymDenotation might be valid for all phases, which means
            // we would not recompute the denotation when travelling to an earlier phase, maybe
            // in the next run. We fix that problem by creating a UniqueRefDenotation instead.
            core.println(i"overwrite ${result.toString} / ${result.lastDenotation}, ${result.lastDenotation.getClass} with $denot at ${ctx.phaseId}")
            result.setDenot(
              UniqueRefDenotation(
                denot.symbol, denot.info,
                Period(ctx.runId, ctx.phaseId, denot.validFor.lastPhaseId),
                this.prefix))
          case _ =>
            result.setDenot(denot)
        result.asInstanceOf[ThisType]
      else // don't assign NoDenotation, we might need to recover later. Test case is pos/avoid.scala.
        this

    /** A reference like this one, but with the given prefix. */
    final def withPrefix(prefix: Type)(using Context): NamedType = {
      def reload(): NamedType = {
        val lastSym = lastSymbol.nn
        val allowPrivate = !lastSym.exists || lastSym.is(Private)
        var d = memberDenot(prefix, name, allowPrivate)
        if (d.isOverloaded && lastSym.exists)
          d = disambiguate(d,
                if (lastSym.signature == Signature.NotAMethod) Signature.NotAMethod
                else lastSym.asSeenFrom(prefix).signature,
                lastSym.targetName)
        NamedType(prefix, name, d)
      }
      if (prefix eq this.prefix) this
      else if (lastDenotation == null) NamedType(prefix, designator)
      else designator match {
        case sym: Symbol =>
          if (infoDependsOnPrefix(sym, prefix) && !prefix.isArgPrefixOf(sym)) {
            val candidate = reload()
            val falseOverride = sym.isClass && candidate.symbol.exists && candidate.symbol != symbol
              // A false override happens if we rebind an inner class to another type with the same name
              // in an outer subclass. This is wrong, since classes do not override. We need to
              // return a type with the existing class info as seen from the new prefix instead.
            if (falseOverride) NamedType(prefix, sym.name, denot.asSeenFrom(prefix))
            else candidate
          }
          else NamedType(prefix, sym)
        case name: Name => reload()
      }
    }

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: NamedType =>
        designator.equals(that.designator) &&
        prefix.equals(that.prefix, bs)
      case _ =>
        false
    }

    override def computeHash(bs: Binders): Int = doHash(bs, designator, prefix)

    override def hashIsStable: Boolean = {
      if (myStableHash == 0) myStableHash = if (prefix.hashIsStable) 1 else -1
      myStableHash > 0
    }

    override def eql(that: Type): Boolean = this eq that // safe because named types are hash-consed separately
  }

  /** A reference to an implicit definition. This can be either a TermRef or a
   *  Implicits.RenamedImplicitRef.
   */
  trait ImplicitRef {
    def implicitName(using Context): TermName
    def underlyingRef: TermRef
  }

  /** The singleton type for path prefix#myDesignator.
   */
  abstract case class TermRef(override val prefix: Type,
                              private var myDesignator: Designator)
    extends NamedType with SingletonType with ImplicitRef {

    type ThisType = TermRef
    type ThisName = TermName

    override def designator: Designator = myDesignator
    override protected def designator_=(d: Designator): Unit = myDesignator = d

    //assert(name.toString != "<local Coder>")
    override def underlying(using Context): Type = {
      val d = denot
      if (d.isOverloaded) NoType else d.info
    }

    override def isOverloaded(using Context): Boolean = denot.isOverloaded

    def alternatives(using Context): List[TermRef] =
      denot.alternatives.map(withDenot(_))

    def altsWith(p: Symbol => Boolean)(using Context): List[TermRef] =
      denot.altsWith(p).map(withDenot(_))

    def implicitName(using Context): TermName = name
    def underlyingRef: TermRef = this
  }

  abstract case class TypeRef(override val prefix: Type,
                              private var myDesignator: Designator)
    extends NamedType {

    type ThisType = TypeRef
    type ThisName = TypeName

    private var myCanDropAliasPeriod: Period = Nowhere
    private var myCanDropAlias: Boolean = _

    /** Given an alias type `type A = B` where a recursive comparison with `B` yields
     *  `false`, can we conclude that the comparison is definitely false?
     *  This could not be the case if `A` overrides some abstract type. Example:
     *
     *    class C { type A }
     *    class D { type A = Int }
     *    val c: C
     *    val d: D & c.type
     *    c.A <:< d.A   ?
     *
     *  The test should return true, by performing the logic in the bottom half of
     *  firstTry (where we check the names of types). But just following the alias
     *  from d.A to Int reduces the problem to `c.A <:< Int`, which returns `false`.
     *  So we can't drop the alias here, we need to do the backtracking to the name-
     *  based tests.
     */
    def canDropAlias(using Context) =
      if myCanDropAliasPeriod != ctx.period then
        myCanDropAlias =
          !symbol.canMatchInheritedSymbols
          || !prefix.baseClasses.exists(_.info.decls.lookup(name).is(Deferred))
        myCanDropAliasPeriod = ctx.period
      myCanDropAlias

    override def designator: Designator = myDesignator
    override protected def designator_=(d: Designator): Unit = myDesignator = d

    override def underlying(using Context): Type = info

    override def translucentSuperType(using Context) = info match {
      case TypeAlias(aliased) => aliased
      case TypeBounds(_, hi) =>
        if (symbol.isOpaqueAlias)
          symbol.opaqueAlias.asSeenFrom(prefix, symbol.owner).orElse(hi) // orElse can happen for malformed input
        else hi
      case _ => underlying
    }

    /** Hook that can be called from creation methods in TermRef and TypeRef */
    def validated(using Context): this.type =
      this
  }

  final class CachedTermRef(prefix: Type, designator: Designator, hc: Int) extends TermRef(prefix, designator) {
    assert((prefix ne NoPrefix) || designator.isInstanceOf[Symbol])
    myHash = hc
  }

  final class CachedTypeRef(prefix: Type, designator: Designator, hc: Int) extends TypeRef(prefix, designator) {
    assert((prefix ne NoPrefix) || designator.isInstanceOf[Symbol])
    myHash = hc
  }

  /** Assert current phase does not have erasure semantics */
  private def assertUnerased()(using Context) =
    if (Config.checkUnerased) assert(!ctx.phase.erasedTypes)

  /** The designator to be used for a named type creation with given prefix, name, and denotation.
   *  This is the denotation's symbol, if it exists and the prefix is not the this type
   *  of the class owning the symbol. The reason for the latter qualification is that
   *  when re-computing the denotation of a `this.<symbol>` reference we read the
   *  type directly off the symbol. But the given denotation might contain a more precise
   *  type than what can be computed from the symbol's info. We have to create in this case
   *  a reference with a name as designator so that the denotation will be correctly updated in
   *  the future. See also NamedType#withDenot. Test case is neg/opaque-self-encoding.scala.
   */
  private def designatorFor(prefix: Type, name: Name, denot: Denotation)(using Context): Designator = {
    def ownerIsPrefix(owner: Symbol) = prefix match
      case prefix: ThisType => prefix.sameThis(owner.thisType)
      case _ => false
    val sym = denot.symbol
    if (sym.exists && (prefix.eq(NoPrefix) || !ownerIsPrefix(sym.owner)))
      sym
    else
      name
  }

  object NamedType {
    def isType(desig: Designator)(using Context): Boolean = desig match {
      case sym: Symbol => sym.isType
      case name: Name => name.isTypeName
    }
    def apply(prefix: Type, designator: Designator)(using Context): NamedType =
      if (isType(designator)) TypeRef.apply(prefix, designator)
      else TermRef.apply(prefix, designator)
    def apply(prefix: Type, designator: Name, denot: Denotation)(using Context): NamedType =
      if (designator.isTermName) TermRef.apply(prefix, designator.asTermName, denot)
      else TypeRef.apply(prefix, designator.asTypeName, denot)
  }

  object TermRef {

    /** Create a term ref with given designator */
    def apply(prefix: Type, desig: Designator)(using Context): TermRef =
      ctx.uniqueNamedTypes.enterIfNew(prefix, desig, isTerm = true).asInstanceOf[TermRef]

    /** Create a term ref with given initial denotation. The name of the reference is taken
     *  from the denotation's symbol if the latter exists, or else it is the given name.
     */
    def apply(prefix: Type, name: TermName, denot: Denotation)(using Context): TermRef =
      apply(prefix, designatorFor(prefix, name, denot)).withDenot(denot)
  }

  object TypeRef {

    /** Create a type ref with given prefix and name */
    def apply(prefix: Type, desig: Designator)(using Context): TypeRef =
      ctx.uniqueNamedTypes.enterIfNew(prefix, desig, isTerm = false).asInstanceOf[TypeRef]

    /** Create a type ref with given initial denotation. The name of the reference is taken
     *  from the denotation's symbol if the latter exists, or else it is the given name.
     */
    def apply(prefix: Type, name: TypeName, denot: Denotation)(using Context): TypeRef =
      apply(prefix, designatorFor(prefix, name, denot)).withDenot(denot)
  }

  // --- Other SingletonTypes: ThisType/SuperType/ConstantType ---------------------------

  /** The type cls.this
   *  @param tref    A type ref which indicates the class `cls`.
   *  Note: we do not pass a class symbol directly, because symbols
   *  do not survive runs whereas typerefs do.
   */
  abstract case class ThisType(tref: TypeRef) extends CachedProxyType with SingletonType {
    def cls(using Context): ClassSymbol = tref.stableInRunSymbol match {
      case cls: ClassSymbol => cls
      case _ if ctx.mode.is(Mode.Interactive) => defn.AnyClass // was observed to happen in IDE mode
    }

    override def underlying(using Context): Type =
      if (ctx.erasedTypes) tref
      else cls.info match {
        case cinfo: ClassInfo => cinfo.selfType
        case _: ErrorType | NoType if ctx.mode.is(Mode.Interactive) => cls.info
          // can happen in IDE if `cls` is stale
      }

    override def computeHash(bs: Binders): Int = doHash(bs, tref)

    override def eql(that: Type): Boolean = that match {
      case that: ThisType => tref.eq(that.tref)
      case _ => false
    }

    /** Check that the rhs is a ThisType that refers to the same class.
     */
    def sameThis(that: Type)(using Context): Boolean = (that eq this) || that.match
      case that: ThisType => this.cls eq that.cls
      case _ => false
  }

  final class CachedThisType(tref: TypeRef) extends ThisType(tref)

  object ThisType {
    /** Normally one should use ClassSymbol#thisType instead */
    def raw(tref: TypeRef)(using Context): CachedThisType =
      unique(new CachedThisType(tref))
  }

  /** The type of a super reference cls.super where
   *  `thistpe` is cls.this and `supertpe` is the type of the value referenced
   *  by `super`.
   */
  abstract case class SuperType(thistpe: Type, supertpe: Type) extends CachedProxyType with SingletonType {
    override def underlying(using Context): Type = supertpe
    override def superType(using Context): Type =
      thistpe.baseType(supertpe.typeSymbol)
    def derivedSuperType(thistpe: Type, supertpe: Type)(using Context): Type =
      if ((thistpe eq this.thistpe) && (supertpe eq this.supertpe)) this
      else SuperType(thistpe, supertpe)

    override def computeHash(bs: Binders): Int = doHash(bs, thistpe, supertpe)

    override def eql(that: Type): Boolean = that match {
      case that: SuperType => thistpe.eq(that.thistpe) && supertpe.eq(that.supertpe)
      case _ => false
    }
  }

  final class CachedSuperType(thistpe: Type, supertpe: Type) extends SuperType(thistpe, supertpe)

  object SuperType {
    def apply(thistpe: Type, supertpe: Type)(using Context): SuperType = {
      assert(thistpe != NoPrefix)
      unique(new CachedSuperType(thistpe, supertpe))
    }
  }

  /** A constant type with single `value`. */
  abstract case class ConstantType(value: Constant) extends CachedProxyType with SingletonType {
    override def underlying(using Context): Type = value.tpe

    override def computeHash(bs: Binders): Int = doHash(value)
  }

  final class CachedConstantType(value: Constant) extends ConstantType(value)

  object ConstantType {
    def apply(value: Constant)(using Context): ConstantType = {
      assertUnerased()
      unique(new CachedConstantType(value))
    }
  }

  // `refFn` can be null only if `computed` is true.
  case class LazyRef(private var refFn: (Context => (Type | Null)) | Null) extends UncachedProxyType with ValueType {
    private var myRef: Type | Null = null
    private var computed = false

    override def tryNormalize(using Context): Type = ref.tryNormalize

    def ref(using Context): Type =
      if computed then
        if myRef == null then
          // if errors were reported previously handle this by throwing a CyclicReference
          // instead of crashing immediately. A test case is neg/i6057.scala.
          assert(ctx.mode.is(Mode.CheckCyclic)
              || ctx.mode.is(Mode.Printing)
              || ctx.reporter.errorsReported)
          throw CyclicReference(NoDenotation)
      else
        computed = true
        val result = refFn.nn(ctx)
        refFn = null
        if result != null then myRef = result
        else assert(myRef != null)  // must have been `update`d
      myRef.nn

    /** Update the value of the lazyref, discarding the compute function `refFn`
     *  Can be called only as long as the ref is still undefined.
     */
    def update(tp: Type)(using Context) =
      assert(myRef == null || ctx.reporter.errorsReported)
      myRef = tp
      computed = true
      refFn = null

    def evaluating: Boolean = computed && myRef == null
    def completed: Boolean = myRef != null
    override def underlying(using Context): Type = ref
    override def toString: String = s"LazyRef(${if (computed) myRef else "..."})"
    override def equals(other: Any): Boolean = this.eq(other.asInstanceOf[AnyRef])
    override def hashCode: Int = System.identityHashCode(this)
  }
  object LazyRef:
    def of(refFn: Context ?=> (Type | Null)): LazyRef = LazyRef(refFn(using _))

  // --- Refined Type and RecType ------------------------------------------------

  abstract class RefinedOrRecType extends CachedProxyType with ValueType {
    def parent: Type
  }

  /** A refined type parent { refinement }
   *  @param parent      The type being refined
   *  @param refinedName The name of the refinement declaration
   *  @param refinedInfo The info of the refinement declaration
   */
  abstract case class RefinedType(parent: Type, refinedName: Name, refinedInfo: Type) extends RefinedOrRecType {

    if (refinedName.isTermName) assert(refinedInfo.isInstanceOf[TermType])
    else assert(refinedInfo.isInstanceOf[TypeType], this)
    assert(!refinedName.is(NameKinds.ExpandedName), this)

    override def underlying(using Context): Type = parent

    private def badInst =
      throw new AssertionError(s"bad instantiation: $this")

    def checkInst(using Context): this.type = this // debug hook

    def derivedRefinedType(parent: Type, refinedName: Name, refinedInfo: Type)(using Context): Type =
      if ((parent eq this.parent) && (refinedName eq this.refinedName) && (refinedInfo eq this.refinedInfo)) this
      else RefinedType(parent, refinedName, refinedInfo)

    /** Add this refinement to `parent`, provided `refinedName` is a member of `parent`. */
    def wrapIfMember(parent: Type)(using Context): Type =
      if (parent.member(refinedName).exists) derivedRefinedType(parent, refinedName, refinedInfo)
      else parent

    override def computeHash(bs: Binders): Int = doHash(bs, refinedName, refinedInfo, parent)
    override def hashIsStable: Boolean = refinedInfo.hashIsStable && parent.hashIsStable

    override def eql(that: Type): Boolean = that match {
      case that: RefinedType =>
        refinedName.eq(that.refinedName) &&
        refinedInfo.eq(that.refinedInfo) &&
        parent.eq(that.parent)
      case _ => false
    }

    // equals comes from case class; no matching override is needed

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: RefinedType =>
        refinedName.eq(that.refinedName) &&
        refinedInfo.equals(that.refinedInfo, bs) &&
        parent.equals(that.parent, bs)
      case _ => false
    }
  }

  class CachedRefinedType(parent: Type, refinedName: Name, refinedInfo: Type)
  extends RefinedType(parent, refinedName, refinedInfo)

  object RefinedType {
    @tailrec def make(parent: Type, names: List[Name], infos: List[Type])(using Context): Type =
      if (names.isEmpty) parent
      else make(RefinedType(parent, names.head, infos.head), names.tail, infos.tail)

    def apply(parent: Type, name: Name, info: Type)(using Context): RefinedType = {
      assert(!ctx.erasedTypes)
      unique(new CachedRefinedType(parent, name, info)).checkInst
    }
  }

  /** A recursive type. Instances should be constructed via the companion object.
   *
   *  @param parentExp A function that, given a recursive type R, produces a type
   *                   that can refer to R via a `RecThis(R)` node. This is used to
   *                   "tie the knot".
   *
   *  For example, in
   *    class C { type T1; type T2 }
   *    type C2 = C { type T1; type T2 = T1  }
   *
   *  The type of `C2` is a recursive type `{(x) => C{T1; T2 = x.T1}}`, written as
   *
   *    RecType(
   *      RefinedType(
   *        RefinedType(
   *         TypeRef(...,class C),
   *         T1,
   *         TypeBounds(...)),
   *        T2,
   *        TypeBounds(
   *          TypeRef(RecThis(...),T1),
   *          TypeRef(RecThis(...),T1))))
   *
   *  Where `RecThis(...)` points back to the enclosing `RecType`.
   */
  class RecType(parentExp: RecType => Type) extends RefinedOrRecType with BindingType {

    // See discussion in findMember#goRec why these vars are needed
    private[Types] var opened: Boolean = false
    private[Types] var openedTwice: Boolean = false

    val parent: Type = parentExp(this: @unchecked)

    private var myRecThis: RecThis | Null = null

    def recThis: RecThis = {
      if (myRecThis == null) myRecThis = new RecThisImpl(this)
      myRecThis.nn
    }

    override def underlying(using Context): Type = parent

    def derivedRecType(parent: Type)(using Context): RecType =
      if (parent eq this.parent) this
      else RecType(rt => parent.substRecThis(this, rt.recThis))

    def rebind(parent: Type)(using Context): Type =
      if (parent eq this.parent) this
      else RecType.closeOver(rt => parent.substRecThis(this, rt.recThis))

    def isReferredToBy(tp: Type)(using Context): Boolean = {
      val refacc = new TypeAccumulator[Boolean] {
        override def apply(x: Boolean, tp: Type) = x || {
          tp match {
            case tp: TypeRef => apply(x, tp.prefix)
            case tp: RecThis => RecType.this eq tp.binder
            case tp: LazyRef => this(x, tp.ref)
            case _ => foldOver(x, tp)
          }
        }
      }
      refacc.apply(false, tp)
    }

    override def computeHash(bs: Binders): Int = doHash(new SomeBinders(this, bs), parent)

    override def hashIsStable: Boolean = false
      // this is a conservative observation. By construction RecTypes contain at least
      // one RecThis occurrence. Since `stableHash` does not keep track of enclosing
      // bound types, it will return "unstable" for this occurrence and this would propagate.

    // No definition of `eql` --> fall back on equals, which calls iso

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: RecType =>
        parent.equals(that.parent, new SomeBinderPairs(this, that, bs))
      case _ => false
    }

    override def toString: String = s"RecType($parent | $hashCode)"

    private def checkInst(using Context): this.type = this // debug hook
  }

  object RecType {

    /** Create a RecType, normalizing its contents. This means:
     *
     *   1. Nested Rec types on the type's spine are merged with the outer one.
     *   2. Any refinement of the form `type T = z.T` on the spine of the type
     *      where `z` refers to the created rec-type is replaced by
     *      `type T`. This avoids infinite recursions later when we
     *      try to follow these references.
     *   TODO: Figure out how to guarantee absence of cycles
     *         of length > 1
     */
    def apply(parentExp: RecType => Type)(using Context): RecType = {
      val rt = new RecType(parentExp)
      def normalize(tp: Type): Type = tp.stripTypeVar match {
        case tp: RecType =>
          normalize(tp.parent.substRecThis(tp, rt.recThis))
        case tp @ RefinedType(parent, rname, rinfo) =>
          val rinfo1 = rinfo match {
            case TypeAlias(ref @ TypeRef(RecThis(`rt`), _)) if ref.name == rname => TypeBounds.empty
            case _ => rinfo
          }
          tp.derivedRefinedType(normalize(parent), rname, rinfo1)
        case tp =>
          tp
      }
      unique(rt.derivedRecType(normalize(rt.parent))).checkInst
    }

    /** Create a `RecType`, but only if the type generated by `parentExp` is indeed recursive. */
    def closeOver(parentExp: RecType => Type)(using Context): Type = {
      val rt = this(parentExp)
      if (rt.isReferredToBy(rt.parent)) rt else rt.parent
    }
  }

  // --- AndType/OrType ---------------------------------------------------------------

  abstract class AndOrType extends CachedGroundType with ValueType {
    def isAnd: Boolean
    def tp1: Type
    def tp2: Type

    def derivedAndOrType(tp1: Type, tp2: Type)(using Context) =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else this match
        case tp: OrType => OrType.make(tp1, tp2, tp.isSoft)
        case tp: AndType => AndType.make(tp1, tp2, checkValid = true)
  }

  abstract case class AndType(tp1: Type, tp2: Type) extends AndOrType {
    def isAnd: Boolean = true
    private var myBaseClassesPeriod: Period = Nowhere
    private var myBaseClasses: List[ClassSymbol] = _
    /** Base classes of are the merge of the operand base classes. */
    override final def baseClasses(using Context): List[ClassSymbol] = {
      if (myBaseClassesPeriod != ctx.period) {
        val bcs1 = tp1.baseClasses
        val bcs1set = BaseClassSet(bcs1)
        def recur(bcs2: List[ClassSymbol]): List[ClassSymbol] = bcs2 match {
          case bc2 :: bcs2rest =>
            if (bcs1set contains bc2)
              if (bc2.is(Trait)) recur(bcs2rest)
              else bcs1 // common class, therefore rest is the same in both sequences
            else bc2 :: recur(bcs2rest)
          case nil => bcs1
        }
        myBaseClasses = recur(tp2.baseClasses)
        myBaseClassesPeriod = ctx.period
      }
      myBaseClasses
    }

    private var myFactorCount = 0
    override def andFactorCount =
      if myFactorCount == 0 then
      	myFactorCount = tp1.andFactorCount + tp2.andFactorCount
      myFactorCount

    def derivedAndType(tp1: Type, tp2: Type)(using Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else AndType.make(tp1, tp2, checkValid = true)

    def derived_& (tp1: Type, tp2: Type)(using Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else tp1 & tp2

    override def computeHash(bs: Binders): Int = doHash(bs, tp1, tp2)

    override def eql(that: Type): Boolean = that match {
      case that: AndType => tp1.eq(that.tp1) && tp2.eq(that.tp2)
      case _ => false
    }
  }

  final class CachedAndType(tp1: Type, tp2: Type) extends AndType(tp1, tp2)

  object AndType {
    def apply(tp1: Type, tp2: Type)(using Context): AndType = {
      assert(tp1.isValueTypeOrWildcard &&
             tp2.isValueTypeOrWildcard, i"$tp1 & $tp2 / " + s"$tp1 & $tp2")
      unchecked(tp1, tp2)
    }

    def balanced(tp1: Type, tp2: Type)(using Context): AndType =
      tp1 match
        case AndType(tp11, tp12) if tp1.andFactorCount > tp2.andFactorCount * 2 =>
          if tp11.andFactorCount < tp12.andFactorCount then
            return apply(tp12, balanced(tp11, tp2))
          else
            return apply(tp11, balanced(tp12, tp2))
        case _ =>
      tp2 match
        case AndType(tp21, tp22) if tp2.andFactorCount > tp1.andFactorCount * 2 =>
          if tp22.andFactorCount < tp21.andFactorCount then
            return apply(balanced(tp1, tp22), tp21)
          else
            return apply(balanced(tp1, tp21), tp22)
        case _ =>
      apply(tp1, tp2)

    def unchecked(tp1: Type, tp2: Type)(using Context): AndType = {
      assertUnerased()
      unique(new CachedAndType(tp1, tp2))
    }

    /** Make an AndType using `op` unless clearly unnecessary (i.e. without
     *  going through `&`).
     */
    def make(tp1: Type, tp2: Type, checkValid: Boolean = false)(using Context): Type =
      if ((tp1 eq tp2) || (tp2 eq defn.AnyType))
        tp1
      else if (tp1 eq defn.AnyType)
        tp2
      else
        if (checkValid) apply(tp1, tp2) else unchecked(tp1, tp2)

    /** Like `make`, but also supports higher-kinded types as argument */
    def makeHk(tp1: Type, tp2: Type)(using Context): Type =
      TypeComparer.liftIfHK(tp1, tp2, AndType.make(_, _, checkValid = false), makeHk, _ | _)
  }

  abstract case class OrType(tp1: Type, tp2: Type) extends AndOrType {
    def isAnd: Boolean = false
    def isSoft: Boolean
    private var myBaseClassesPeriod: Period = Nowhere
    private var myBaseClasses: List[ClassSymbol] = _
    /** Base classes of are the intersection of the operand base classes. */
    override final def baseClasses(using Context): List[ClassSymbol] = {
      if (myBaseClassesPeriod != ctx.period) {
        val bcs1 = tp1.baseClasses
        val bcs1set = BaseClassSet(bcs1)
        def recur(bcs2: List[ClassSymbol]): List[ClassSymbol] = bcs2 match {
          case bc2 :: bcs2rest =>
            if (bcs1set contains bc2)
              if (bc2.is(Trait)) bc2 :: recur(bcs2rest)
              else bcs2
            else recur(bcs2rest)
          case nil =>
            bcs2
        }
        myBaseClasses = recur(tp2.baseClasses)
        myBaseClassesPeriod = ctx.period
      }
      myBaseClasses
    }

    private var myFactorCount = 0
    override def orFactorCount(soft: Boolean) =
      if this.isSoft == soft then
        if myFactorCount == 0 then
          myFactorCount = tp1.orFactorCount(soft) + tp2.orFactorCount(soft)
        myFactorCount
      else 1

    assert(tp1.isValueTypeOrWildcard &&
           tp2.isValueTypeOrWildcard, s"$tp1 $tp2")

    private var myJoin: Type = _
    private var myJoinPeriod: Period = Nowhere

    /** Replace or type by the closest non-or type above it */
    def join(using Context): Type = {
      if (myJoinPeriod != ctx.period) {
        myJoin = TypeOps.orDominator(this)
        core.println(i"join of $this == $myJoin")
        assert(myJoin != this)
        myJoinPeriod = ctx.period
      }
      myJoin
    }

    private var myUnion: Type = _
    private var myUnionPeriod: Period = Nowhere

    override def widenUnionWithoutNull(using Context): Type =
      if myUnionPeriod != ctx.period then
        myUnion =
          if isSoft then
            TypeComparer.lub(tp1.widenUnionWithoutNull, tp2.widenUnionWithoutNull, canConstrain = true, isSoft = isSoft) match
              case union: OrType => union.join
              case res => res
          else derivedOrType(tp1.widenUnionWithoutNull, tp2.widenUnionWithoutNull, soft = isSoft)
        if !isProvisional then myUnionPeriod = ctx.period
      myUnion

    private var atomsRunId: RunId = NoRunId
    private var myAtoms: Atoms = _
    private var myWidened: Type = _

    private def ensureAtomsComputed()(using Context): Unit =
      if atomsRunId != ctx.runId then
        myAtoms =
          if tp1.hasClassSymbol(defn.NothingClass) then tp2.atoms
          else if tp2.hasClassSymbol(defn.NothingClass) then tp1.atoms
          else tp1.atoms | tp2.atoms
        val tp1w = tp1.widenSingletons
        val tp2w = tp2.widenSingletons
        myWidened = if ((tp1 eq tp1w) && (tp2 eq tp2w)) this else TypeComparer.lub(tp1w, tp2w, isSoft = isSoft)
        atomsRunId = ctx.runId

    override def atoms(using Context): Atoms =
      ensureAtomsComputed()
      myAtoms

    override def widenSingletons(using Context): Type = {
      ensureAtomsComputed()
      myWidened
    }

    def derivedOrType(tp1: Type, tp2: Type, soft: Boolean = isSoft)(using Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2) && soft == isSoft) this
      else OrType.make(tp1, tp2, soft)

    override def computeHash(bs: Binders): Int =
      doHash(bs, if isSoft then 0 else 1, tp1, tp2)

    override def eql(that: Type): Boolean = that match {
      case that: OrType => tp1.eq(that.tp1) && tp2.eq(that.tp2) && isSoft == that.isSoft
      case _ => false
    }
  }

  final class CachedOrType(tp1: Type, tp2: Type, override val isSoft: Boolean) extends OrType(tp1, tp2)

  object OrType {

    def apply(tp1: Type, tp2: Type, soft: Boolean)(using Context): OrType = {
      assertUnerased()
      unique(new CachedOrType(tp1, tp2, soft))
    }

    def balanced(tp1: Type, tp2: Type, soft: Boolean)(using Context): OrType =
      tp1 match
        case OrType(tp11, tp12) if tp1.orFactorCount(soft) > tp2.orFactorCount(soft) * 2 =>
          if tp11.orFactorCount(soft) < tp12.orFactorCount(soft) then
            return apply(tp12, balanced(tp11, tp2, soft), soft)
          else
            return apply(tp11, balanced(tp12, tp2, soft), soft)
        case _ =>
      tp2 match
        case OrType(tp21, tp22) if tp2.orFactorCount(soft) > tp1.orFactorCount(soft) * 2 =>
          if tp22.orFactorCount(soft) < tp21.orFactorCount(soft) then
            return apply(balanced(tp1, tp22, soft), tp21, soft)
          else
            return apply(balanced(tp1, tp21, soft), tp22, soft)
        case _ =>
      apply(tp1, tp2, soft)

    def make(tp1: Type, tp2: Type, soft: Boolean)(using Context): Type =
      if (tp1 eq tp2) tp1
      else apply(tp1, tp2, soft)

    /** Like `make`, but also supports higher-kinded types as argument */
    def makeHk(tp1: Type, tp2: Type)(using Context): Type =
      TypeComparer.liftIfHK(tp1, tp2, OrType(_, _, soft = true), makeHk, _ & _)
  }

  /** An extractor object to pattern match against a nullable union.
   *  e.g.
   *
   *  (tp: Type) match
   *    case OrNull(tp1) => // tp had the form `tp1 | Null`
   *    case _ => // tp was not a nullable union
   */
  object OrNull {
    def apply(tp: Type)(using Context) =
      if tp.isNullType then tp else OrType(tp, defn.NullType, soft = false)
    def unapply(tp: Type)(using Context): Option[Type] =
      val tp1 = tp.stripNull
      if tp1 ne tp then Some(tp1) else None
  }

  // ----- ExprType and LambdaTypes -----------------------------------

  // Note: method types are cached whereas poly types are not. The reason
  // is that most poly types are cyclic via poly params,
  // and therefore two different poly types would never be equal.

  trait MethodicType extends TermType

  /** A by-name parameter type of the form `=> T`, or the type of a method with no parameter list. */
  abstract case class ExprType(resType: Type)
  extends CachedProxyType with MethodicType {
    override def resultType(using Context): Type = resType
    override def underlying(using Context): Type = resType

    override def signature(using Context): Signature = Signature.NotAMethod

    def derivedExprType(resType: Type)(using Context): ExprType =
      if (resType eq this.resType) this else ExprType(resType)

    override def computeHash(bs: Binders): Int = doHash(bs, resType)
    override def hashIsStable: Boolean = resType.hashIsStable

    override def eql(that: Type): Boolean = that match {
      case that: ExprType => resType.eq(that.resType)
      case _ => false
    }

    // equals comes from case class; no matching override is needed

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: ExprType => resType.equals(that.resType, bs)
      case _ => false
    }
  }

  final class CachedExprType(resultType: Type) extends ExprType(resultType)

  object ExprType {
    def apply(resultType: Type)(using Context): ExprType = {
      assertUnerased()
      unique(new CachedExprType(resultType))
    }
  }

  /** The lambda type square:
   *
   *    LambdaType   |   TermLambda      |   TypeLambda
   *    -------------+-------------------+------------------
   *    HKLambda     |   HKTermLambda    |   HKTypeLambda
   *    MethodOrPoly |   MethodType	     |   PolyType
   */
  trait LambdaType extends BindingType with TermType { self =>
    type ThisName <: Name
    type PInfo <: Type
    type This <: LambdaType{type PInfo = self.PInfo}
    type ParamRefType <: ParamRef

    def paramNames: List[ThisName]
    def paramInfos: List[PInfo]
    def resType: Type
    protected def newParamRef(n: Int): ParamRefType

    override def resultType(using Context): Type = resType

    def isResultDependent(using Context): Boolean
    def isParamDependent(using Context): Boolean

    final def isTermLambda: Boolean = isInstanceOf[TermLambda]
    final def isTypeLambda: Boolean = isInstanceOf[TypeLambda]
    final def isHigherKinded: Boolean = isInstanceOf[TypeProxy]

    private var myParamRefs: List[ParamRefType] | Null = null

    def paramRefs: List[ParamRefType] = {
      if myParamRefs == null then
        def recur(paramNames: List[ThisName], i: Int): List[ParamRefType] =
          paramNames match
            case _ :: rest => newParamRef(i) :: recur(rest, i + 1)
            case _ => Nil
        myParamRefs = recur(paramNames, 0)
      myParamRefs.nn
    }

    /** Like `paramInfos` but substitute parameter references with the given arguments */
    final def instantiateParamInfos(argTypes: => List[Type])(using Context): List[Type] =
      if (isParamDependent) paramInfos.mapConserve(_.substParams(this, argTypes))
      else paramInfos

    /** Like `resultType` but substitute parameter references with the given arguments */
    final def instantiate(argTypes: => List[Type])(using Context): Type =
      if (isResultDependent) resultType.substParams(this, argTypes)
      else resultType

    def companion: LambdaTypeCompanion[ThisName, PInfo, This]

    /** The type `[tparams := paramRefs] tp`, where `tparams` can be
     *  either a list of type parameter symbols or a list of lambda parameters
     */
    def integrate(tparams: List[ParamInfo], tp: Type)(using Context): Type =
      (tparams: @unchecked) match {
        case LambdaParam(lam, _) :: _ => tp.subst(lam, this)
        case params: List[Symbol @unchecked] => tp.subst(params, paramRefs)
      }

    final def derivedLambdaType(paramNames: List[ThisName] = this.paramNames,
                          paramInfos: List[PInfo] = this.paramInfos,
                          resType: Type = this.resType)(using Context): LambdaType =
      if ((paramNames eq this.paramNames) && (paramInfos eq this.paramInfos) && (resType eq this.resType)) this
      else newLikeThis(paramNames, paramInfos, resType)

    def newLikeThis(paramNames: List[ThisName], paramInfos: List[PInfo], resType: Type)(using Context): This =
      def substParams(pinfos: List[PInfo], to: This): List[PInfo] = pinfos match
        case pinfos @ (pinfo :: rest) =>
          pinfos.derivedCons(pinfo.subst(this, to).asInstanceOf[PInfo], substParams(rest, to))
        case nil =>
          nil
      companion(paramNames)(
          x => substParams(paramInfos, x),
          x => resType.subst(this, x))

    protected def prefixString: String
    override def toString: String = s"$prefixString($paramNames, $paramInfos, $resType)"
  }

  abstract class HKLambda extends CachedProxyType with LambdaType {
    final override def underlying(using Context): Type = resType
    final override def hashIsStable: Boolean = resType.hashIsStable && paramInfos.hashIsStable
    final override def equals(that: Any): Boolean = equals(that, null)
  }

  /** The superclass of MethodType and PolyType. */
  sealed abstract class MethodOrPoly extends UncachedGroundType with LambdaType with MethodicType {

    // Invariants:
    // (1) mySignatureRunId != NoRunId      =>  mySignature != null
    // (2) myJavaSignatureRunId != NoRunId  =>  myJavaSignature != null

    private var mySignature: Signature = _
    private var mySignatureRunId: Int = NoRunId
    private var myJavaSignature: Signature = _
    private var myJavaSignatureRunId: Int = NoRunId
    private var myScala2Signature: Signature = _
    private var myScala2SignatureRunId: Int = NoRunId

    /** If `isJava` is false, the Scala signature of this method. Otherwise, its Java signature.
     *
     *  This distinction is needed because the same method type
     *  might be part of both a Java and Scala class and each language has
     *  different type erasure rules.
     *
     *  Invariants:
     *  - Two distinct method overloads defined in the same _Scala_ class will
     *    have distinct _Scala_ signatures.
     *  - Two distinct methods overloads defined in the same _Java_ class will
     *    have distinct _Java_ signatures.
     *
     *  @see SingleDenotation#signature
     */
    def signature(sourceLanguage: SourceLanguage)(using Context): Signature =
      def computeSignature(using Context): Signature =
        val resultSignature = resultType match
          case tp: MethodOrPoly => tp.signature(sourceLanguage)
          case tp: ExprType => tp.signature
          case tp =>
            if tp.isRef(defn.UnitClass) then Signature(Nil, defn.UnitClass.fullName.asTypeName)
            else Signature(tp, sourceLanguage)
        this match
          case tp: MethodType =>
            val params = if (isErasedMethod) Nil else tp.paramInfos
            resultSignature.prependTermParams(params, sourceLanguage)
          case tp: PolyType =>
            resultSignature.prependTypeParams(tp.paramNames.length)

      sourceLanguage match
        case SourceLanguage.Java =>
          if ctx.runId != myJavaSignatureRunId then
            myJavaSignature = computeSignature
            if !myJavaSignature.isUnderDefined then myJavaSignatureRunId = ctx.runId
          myJavaSignature
        case SourceLanguage.Scala2 =>
          if ctx.runId != myScala2SignatureRunId then
            myScala2Signature = computeSignature
            if !myScala2Signature.isUnderDefined then myScala2SignatureRunId = ctx.runId
          myScala2Signature
        case SourceLanguage.Scala3 =>
          if ctx.runId != mySignatureRunId then
            mySignature = computeSignature
            if !mySignature.isUnderDefined then mySignatureRunId = ctx.runId
          mySignature
    end signature

    /** The Scala signature of this method. Note that two distinct Java method
     *  overloads may have the same Scala signature, the other overload of
     *  `signature` can be used to avoid ambiguity if necessary.
     */
    final override def signature(using Context): Signature =
      signature(sourceLanguage = SourceLanguage.Scala3)

    final override def hashCode: Int = System.identityHashCode(this)

    final override def equals(that: Any): Boolean = equals(that, null)

    // No definition of `eql` --> fall back on equals, which is `eq`

    final override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: MethodOrPoly =>
        paramNames.eqElements(that.paramNames) &&
        companion.eq(that.companion) && {
          val bs1 = new SomeBinderPairs(this, that, bs)
          paramInfos.equalElements(that.paramInfos, bs1) &&
          resType.equals(that.resType, bs1)
        }
      case _ =>
        false
    }
  }

  trait TermLambda extends LambdaType { thisLambdaType =>
    import DepStatus._
    type ThisName = TermName
    type PInfo = Type
    type This <: TermLambda
    type ParamRefType = TermParamRef

    override def resultType(using Context): Type =
      if (dependencyStatus == FalseDeps) { // dealias all false dependencies
        val dealiasMap = new TypeMap {
          def apply(tp: Type) = tp match {
            case tp @ TypeRef(pre, _) =>
              tp.info match {
                case TypeAlias(alias) if depStatus(NoDeps, pre) == TrueDeps => apply(alias)
                case _ => mapOver(tp)
              }
            case _ =>
              mapOver(tp)
          }
        }
        dealiasMap(resType)
      }
      else resType

    private var myDependencyStatus: DependencyStatus = Unknown
    private var myParamDependencyStatus: DependencyStatus = Unknown

    private def depStatus(initial: DependencyStatus, tp: Type)(using Context): DependencyStatus =
      class DepAcc extends TypeAccumulator[DependencyStatus]:
        def apply(status: DependencyStatus, tp: Type) = compute(status, tp, this)
      def combine(x: DependencyStatus, y: DependencyStatus) =
        val status = (x & StatusMask) max (y & StatusMask)
        val provisional = (x | y) & Provisional
        (if status == TrueDeps then status else status | provisional).toByte
      def compute(status: DependencyStatus, tp: Type, theAcc: TypeAccumulator[DependencyStatus] | Null): DependencyStatus =
        def applyPrefix(tp: NamedType) =
          if tp.currentSymbol.isStatic then status
          else compute(status, tp.prefix, theAcc)
        if status == TrueDeps then status
        else tp match
          case tp: TypeRef =>
            val status1 = applyPrefix(tp)
            tp.info match { // follow type alias to avoid dependency
              case TypeAlias(alias) if status1 == TrueDeps =>
                combine(compute(status, alias, theAcc), FalseDeps)
              case _ =>
                status1
            }
          case tp: TermRef => applyPrefix(tp)
          case tp: AppliedType => tp.fold(status, compute(_, _, theAcc))
          case tp: TypeVar if !tp.isInstantiated => combine(status, Provisional)
          case tp: TermParamRef if tp.binder eq thisLambdaType => TrueDeps
          case AnnotatedType(parent, ann) =>
            if ann.refersToParamOf(thisLambdaType) then TrueDeps
            else compute(status, parent, theAcc)
          case _: ThisType | _: BoundType | NoPrefix => status
          case t: LazyRef =>
            if t.completed then compute(status, t.ref, theAcc)
            else Unknown
          case _ =>
            (if theAcc != null then theAcc else DepAcc()).foldOver(status, tp)
      compute(initial, tp, null)
    end depStatus

    /** The dependency status of this method. Some examples:
     *
     *    class C extends { type S; type T = String }
     *    def f(x: C)(y: Boolean)   // dependencyStatus = NoDeps
     *    def f(x: C)(y: x.S)       // dependencyStatus = TrueDeps
     *    def f(x: C)(y: x.T)       // dependencyStatus = FalseDeps, i.e.
     *                              // dependency can be eliminated by dealiasing.
     */
    private def dependencyStatus(using Context): DependencyStatus =
      if (myDependencyStatus != Unknown) myDependencyStatus
      else {
        val result = depStatus(NoDeps, resType)
        if ((result & Provisional) == 0) myDependencyStatus = result
        (result & StatusMask).toByte
      }

    /** The parameter dependency status of this method. Analogous to `dependencyStatus`,
     *  but tracking dependencies in same parameter list.
     */
    private def paramDependencyStatus(using Context): DependencyStatus =
      if (myParamDependencyStatus != Unknown) myParamDependencyStatus
      else {
        val result =
          if (paramInfos.isEmpty) NoDeps
          else paramInfos.tail.foldLeft(NoDeps)(depStatus(_, _))
        if ((result & Provisional) == 0) myParamDependencyStatus = result
        (result & StatusMask).toByte
      }

    /** Does result type contain references to parameters of this method type,
     *  which cannot be eliminated by de-aliasing?
     */
    def isResultDependent(using Context): Boolean = dependencyStatus == TrueDeps

    /** Does one of the parameter types contain references to earlier parameters
     *  of this method type which cannot be eliminated by de-aliasing?
     */
    def isParamDependent(using Context): Boolean = paramDependencyStatus == TrueDeps

    def newParamRef(n: Int): TermParamRef = new TermParamRefImpl(this, n)

    /** The least supertype of `resultType` that does not contain parameter dependencies */
    def nonDependentResultApprox(using Context): Type =
      if (isResultDependent) {
        val dropDependencies = new ApproximatingTypeMap {
          def apply(tp: Type) = tp match {
            case tp @ TermParamRef(`thisLambdaType`, _) =>
              range(defn.NothingType, atVariance(1)(apply(tp.underlying)))
            case AnnotatedType(parent, ann) if ann.refersToParamOf(thisLambdaType) =>
              mapOver(parent)
            case _ => mapOver(tp)
          }
        }
        dropDependencies(resultType)
      }
      else resultType
  }

  abstract case class MethodType(paramNames: List[TermName])(
      paramInfosExp: MethodType => List[Type],
      resultTypeExp: MethodType => Type)
    extends MethodOrPoly with TermLambda with NarrowCached { thisMethodType =>

    type This = MethodType

    val paramInfos: List[Type] = paramInfosExp(this: @unchecked)
    val resType: Type = resultTypeExp(this: @unchecked)
    assert(resType.exists)

    def companion: MethodTypeCompanion

    final override def isImplicitMethod: Boolean =
      companion.eq(ImplicitMethodType) ||
      companion.eq(ErasedImplicitMethodType) ||
      isContextualMethod
    final override def isErasedMethod: Boolean =
      companion.eq(ErasedMethodType) ||
      companion.eq(ErasedImplicitMethodType) ||
      companion.eq(ErasedContextualMethodType)
    final override def isContextualMethod: Boolean =
      companion.eq(ContextualMethodType) ||
      companion.eq(ErasedContextualMethodType)

    protected def prefixString: String = companion.prefixString
  }

  final class CachedMethodType(paramNames: List[TermName])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type, val companion: MethodTypeCompanion)
    extends MethodType(paramNames)(paramInfosExp, resultTypeExp)

  abstract class LambdaTypeCompanion[N <: Name, PInfo <: Type, LT <: LambdaType] {
    def syntheticParamName(n: Int): N

    @sharable private val memoizedNames = util.HashMap[Int, List[N]]()
    def syntheticParamNames(n: Int): List[N] = synchronized {
      memoizedNames.getOrElseUpdate(n, (0 until n).map(syntheticParamName).toList)
    }

    def apply(paramNames: List[N])(paramInfosExp: LT => List[PInfo], resultTypeExp: LT => Type)(using Context): LT
    def apply(paramNames: List[N], paramInfos: List[PInfo], resultType: Type)(using Context): LT =
      apply(paramNames)(_ => paramInfos, _ => resultType)
    def apply(paramInfos: List[PInfo])(resultTypeExp: LT => Type)(using Context): LT =
      apply(syntheticParamNames(paramInfos.length))(_ => paramInfos, resultTypeExp)
    def apply(paramInfos: List[PInfo], resultType: Type)(using Context): LT =
      apply(syntheticParamNames(paramInfos.length), paramInfos, resultType)

    protected def toPInfo(tp: Type)(using Context): PInfo

    def fromParams[PI <: ParamInfo.Of[N]](params: List[PI], resultType: Type)(using Context): Type =
      if (params.isEmpty) resultType
      else apply(params.map(_.paramName))(
        tl => params.map(param => toPInfo(tl.integrate(params, param.paramInfo))),
        tl => tl.integrate(params, resultType))
  }

  abstract class TermLambdaCompanion[LT <: TermLambda]
  extends LambdaTypeCompanion[TermName, Type, LT] {
    def toPInfo(tp: Type)(using Context): Type = tp
    def syntheticParamName(n: Int): TermName = nme.syntheticParamName(n)
  }

  abstract class TypeLambdaCompanion[LT <: TypeLambda]
  extends LambdaTypeCompanion[TypeName, TypeBounds, LT] {
    def toPInfo(tp: Type)(using Context): TypeBounds = (tp: @unchecked) match {
      case tp: TypeBounds => tp
      case tp: ErrorType => TypeAlias(tp)
    }
    def syntheticParamName(n: Int): TypeName = tpnme.syntheticTypeParamName(n)
  }

  abstract class MethodTypeCompanion(val prefixString: String) extends TermLambdaCompanion[MethodType] { self =>

    /** Produce method type from parameter symbols, with special mappings for repeated
     *  and inline parameters:
     *   - replace @repeated annotations on Seq or Array types by <repeated> types
     *   - add @inlineParam to inline parameters
     */
    def fromSymbols(params: List[Symbol], resultType: Type)(using Context): MethodType = {
      def translateInline(tp: Type): Type = tp match {
        case ExprType(resType) => ExprType(AnnotatedType(resType, Annotation(defn.InlineParamAnnot)))
        case _ => AnnotatedType(tp, Annotation(defn.InlineParamAnnot))
      }
      def translateErased(tp: Type): Type = tp match {
        case ExprType(resType) => ExprType(AnnotatedType(resType, Annotation(defn.ErasedParamAnnot)))
        case _ => AnnotatedType(tp, Annotation(defn.ErasedParamAnnot))
      }
      def paramInfo(param: Symbol) = {
        var paramType = param.info.annotatedToRepeated
        if (param.is(Inline)) paramType = translateInline(paramType)
        if (param.is(Erased)) paramType = translateErased(paramType)
        paramType
      }

      apply(params.map(_.name.asTermName))(
         tl => params.map(p => tl.integrate(params, paramInfo(p))),
         tl => tl.integrate(params, resultType))
    }

    final def apply(paramNames: List[TermName])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type)(using Context): MethodType =
      checkValid(unique(new CachedMethodType(paramNames)(paramInfosExp, resultTypeExp, self)))

    def checkValid(mt: MethodType)(using Context): mt.type = {
      if (Config.checkMethodTypes)
        for ((paramInfo, idx) <- mt.paramInfos.zipWithIndex)
          paramInfo.foreachPart {
            case TermParamRef(`mt`, j) => assert(j < idx, mt)
            case _ =>
          }
      mt
    }
  }

  object MethodType extends MethodTypeCompanion("MethodType") {
    def companion(isContextual: Boolean = false, isImplicit: Boolean = false, isErased: Boolean = false): MethodTypeCompanion =
      if (isContextual)
        if (isErased) ErasedContextualMethodType else ContextualMethodType
      else if (isImplicit)
        if (isErased) ErasedImplicitMethodType else ImplicitMethodType
      else
        if (isErased) ErasedMethodType else MethodType
  }
  object ErasedMethodType extends MethodTypeCompanion("ErasedMethodType")
  object ContextualMethodType extends MethodTypeCompanion("ContextualMethodType")
  object ErasedContextualMethodType extends MethodTypeCompanion("ErasedContextualMethodType")
  object ImplicitMethodType extends MethodTypeCompanion("ImplicitMethodType")
  object ErasedImplicitMethodType extends MethodTypeCompanion("ErasedImplicitMethodType")

  /** A ternary extractor for MethodType */
  object MethodTpe {
    def unapply(mt: MethodType)(using Context): Some[(List[TermName], List[Type], Type)] =
      Some((mt.paramNames, mt.paramInfos, mt.resultType))
  }

  trait TypeLambda extends LambdaType {
    type ThisName = TypeName
    type PInfo = TypeBounds
    type This <: TypeLambda
    type ParamRefType = TypeParamRef

    def isResultDependent(using Context): Boolean = true
    def isParamDependent(using Context): Boolean = true

    def newParamRef(n: Int): TypeParamRef = new TypeParamRefImpl(this, n)

    @threadUnsafe lazy val typeParams: List[LambdaParam] =
      paramNames.indices.toList.map(new LambdaParam(this, _))

    def derivedLambdaAbstraction(paramNames: List[TypeName], paramInfos: List[TypeBounds], resType: Type)(using Context): Type =
      resType match {
        case resType: AliasingBounds =>
          resType.derivedAlias(newLikeThis(paramNames, paramInfos, resType.alias))
        case resType @ TypeBounds(lo, hi) =>
          resType.derivedTypeBounds(
            if (lo.isRef(defn.NothingClass)) lo else newLikeThis(paramNames, paramInfos, lo),
            newLikeThis(paramNames, paramInfos, hi))
        case _ =>
          derivedLambdaType(paramNames, paramInfos, resType)
      }
  }

  /** A type lambda of the form `[X_0 B_0, ..., X_n B_n] => T`
   *
   *  @param  paramNames      The names `X_0`, ..., `X_n`
   *  @param  paramInfosExp  A function that, given the polytype itself, returns the
   *                          parameter bounds `B_1`, ..., `B_n`
   *  @param  resultTypeExp   A function that, given the polytype itself, returns the
   *                          result type `T`.
   *  @param  variances       The variances of the type parameters, if the type lambda
   *                          carries variances, i.e. it is a bound of an abstract type
   *                          or the rhs of a match alias or opaque alias. The parameter
   *                          is Nil for all other lambdas.
   *
   *  Variances are stored in the `typeParams` list of the lambda.
   */
  class HKTypeLambda(val paramNames: List[TypeName], @constructorOnly variances: List[Variance])(
      paramInfosExp: HKTypeLambda => List[TypeBounds], resultTypeExp: HKTypeLambda => Type)
  extends HKLambda with TypeLambda {
    type This = HKTypeLambda
    def companion: HKTypeLambda.type = HKTypeLambda

    val paramInfos: List[TypeBounds] = paramInfosExp(this: @unchecked)
    val resType: Type = resultTypeExp(this: @unchecked)

    private def setVariances(tparams: List[LambdaParam], vs: List[Variance]): Unit =
      if tparams.nonEmpty then
        tparams.head.declaredVariance = vs.head
        setVariances(tparams.tail, vs.tail)

    override val isDeclaredVarianceLambda = variances.nonEmpty
    if isDeclaredVarianceLambda then setVariances(typeParams, variances)

    def declaredVariances =
      if isDeclaredVarianceLambda then typeParams.map(_.declaredVariance)
      else Nil

    override def computeHash(bs: Binders): Int =
      doHash(new SomeBinders(this, bs), declaredVariances ::: paramNames, resType, paramInfos)

    // No definition of `eql` --> fall back on equals, which calls iso

    final override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: HKTypeLambda =>
        paramNames.eqElements(that.paramNames)
        && isDeclaredVarianceLambda == that.isDeclaredVarianceLambda
        && (!isDeclaredVarianceLambda
            || typeParams.corresponds(that.typeParams)((x, y) =>
                  x.declaredVariance == y.declaredVariance))
        && {
          val bs1 = new SomeBinderPairs(this, that, bs)
          // `paramInfos` and `resType` might still be uninstantiated at this point
          (paramInfos: List[TypeBounds] | Null) != null && (resType: Type | Null) != null &&
          paramInfos.equalElements(that.paramInfos, bs1) &&
          resType.equals(that.resType, bs1)
        }
      case _ =>
        false
    }

    override def newLikeThis(paramNames: List[ThisName], paramInfos: List[PInfo], resType: Type)(using Context): This =
      newLikeThis(paramNames, declaredVariances, paramInfos, resType)

    def newLikeThis(paramNames: List[ThisName], variances: List[Variance], paramInfos: List[PInfo], resType: Type)(using Context): This =
      HKTypeLambda(paramNames, variances)(
          x => paramInfos.mapConserve(_.subst(this, x).asInstanceOf[PInfo]),
          x => resType.subst(this, x))

    def withVariances(variances: List[Variance])(using Context): This =
      newLikeThis(paramNames, variances, paramInfos, resType)

    protected def prefixString: String = "HKTypeLambda"
    final override def toString: String =
      if isDeclaredVarianceLambda then
        s"HKTypeLambda($paramNames, $paramInfos, $resType, ${declaredVariances.map(_.flagsString)})"
      else super.toString

    assert(resType.isInstanceOf[TermType], this)
    assert(paramNames.nonEmpty)
  }

  /** The type of a polymorphic method. It has the same form as HKTypeLambda,
   *  except it applies to terms and parameters do not have variances.
   */
  class PolyType(val paramNames: List[TypeName])(
      paramInfosExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)
  extends MethodOrPoly with TypeLambda {

    type This = PolyType
    def companion: PolyType.type = PolyType

    val paramInfos: List[TypeBounds] = paramInfosExp(this: @unchecked)
    val resType: Type = resultTypeExp(this: @unchecked)

    assert(resType.isInstanceOf[TermType], this)
    assert(paramNames.nonEmpty)

    override def isContextualMethod = resType.isContextualMethod
    override def isImplicitMethod = resType.isImplicitMethod

    /** Merge nested polytypes into one polytype. nested polytypes are normally not supported
     *  but can arise as temporary data structures.
     */
    def flatten(using Context): PolyType = resType match {
      case that: PolyType =>
        val shiftedSubst = (x: PolyType) => new TypeMap {
          def apply(t: Type) = t match {
            case TypeParamRef(`that`, n) => x.paramRefs(n + paramNames.length)
            case t => mapOver(t)
          }
        }
        PolyType(paramNames ++ that.paramNames)(
          x => this.paramInfos.mapConserve(_.subst(this, x).bounds) ++
               that.paramInfos.mapConserve(shiftedSubst(x)(_).bounds),
          x => shiftedSubst(x)(that.resultType).subst(this, x))
      case _ => this
    }

    protected def prefixString: String = "PolyType"
  }

  object HKTypeLambda extends TypeLambdaCompanion[HKTypeLambda] {
    def apply(paramNames: List[TypeName])(
        paramInfosExp: HKTypeLambda => List[TypeBounds],
        resultTypeExp: HKTypeLambda => Type)(using Context): HKTypeLambda =
      apply(paramNames, Nil)(paramInfosExp, resultTypeExp)

    def apply(paramNames: List[TypeName], variances: List[Variance])(
        paramInfosExp: HKTypeLambda => List[TypeBounds],
        resultTypeExp: HKTypeLambda => Type)(using Context): HKTypeLambda =
      unique(new HKTypeLambda(paramNames, variances)(paramInfosExp, resultTypeExp))

    def unapply(tl: HKTypeLambda): Some[(List[LambdaParam], Type)] =
      Some((tl.typeParams, tl.resType))

    def any(n: Int)(using Context): HKTypeLambda =
      apply(syntheticParamNames(n))(
        pt => List.fill(n)(TypeBounds.empty), pt => defn.AnyType)

    override def fromParams[PI <: ParamInfo.Of[TypeName]](params: List[PI], resultType: Type)(using Context): Type =
      resultType match
        case bounds: TypeBounds => boundsFromParams(params, bounds)
        case _ => super.fromParams(params, resultType)

    /** Distributes Lambda inside type bounds. Examples:
     *
     *      type T[X] = U        becomes    type T = [X] -> U
     *      type T[X] <: U       becomes    type T >: Nothing <: ([X] -> U)
     *      type T[X] >: L <: U  becomes    type T >: ([X] -> L) <: ([X] -> U)
     *
     *  The variances of regular TypeBounds types, as well as of match aliases
     *  and of opaque aliases are always determined from the given parameters
     *  `params`. The variances of other type aliases are determined from
     *  the given parameters only if one of these parameters carries a `+`
     *  or `-` variance annotation. Type aliases without variance annotation
     *  are treated structurally. That is, their parameter variances are
     *  determined by how the parameter(s) appear in the result type.
     *
     *  Examples:
     *
     *    type T[X] >: A              // X is invariant
     *    type T[X] <: List[X]        // X is invariant
     *    type T[X] = List[X]         // X is covariant (determined structurally)
     *    opaque type T[X] = List[X]  // X is invariant
     *    opaque type T[+X] = List[X] // X is covariant
     *    type T[A, B] = A => B       // A is contravariant, B is covariant (determined structurally)
     *    type T[A, +B] = A => B      // A is invariant, B is covariant
     */
    def boundsFromParams[PI <: ParamInfo.Of[TypeName]](params: List[PI], bounds: TypeBounds)(using Context): TypeBounds = {
      def expand(tp: Type, useVariances: Boolean) =
        if params.nonEmpty && useVariances then
          apply(params.map(_.paramName), params.map(_.paramVariance))(
            tl => params.map(param => toPInfo(tl.integrate(params, param.paramInfo))),
            tl => tl.integrate(params, tp))
        else
          super.fromParams(params, tp)
      def isOpaqueAlias = params match
        case (param: Symbol) :: _ => param.owner.is(Opaque)
        case _ => false
      bounds match {
        case bounds: MatchAlias =>
          bounds.derivedAlias(expand(bounds.alias, true))
        case bounds: TypeAlias =>
          bounds.derivedAlias(expand(bounds.alias,
            isOpaqueAlias || params.exists(!_.paramVariance.isEmpty)))
        case TypeBounds(lo, hi) =>
          bounds.derivedTypeBounds(
            if lo.isRef(defn.NothingClass) then lo else expand(lo, true),
            expand(hi, true))
      }
    }
  }

  object PolyType extends TypeLambdaCompanion[PolyType] {
    def apply(paramNames: List[TypeName])(
        paramInfosExp: PolyType => List[TypeBounds],
        resultTypeExp: PolyType => Type)(using Context): PolyType =
      unique(new PolyType(paramNames)(paramInfosExp, resultTypeExp))

    def unapply(tl: PolyType): Some[(List[LambdaParam], Type)] =
      Some((tl.typeParams, tl.resType))
  }

  private object DepStatus {
    type DependencyStatus = Byte
    final val Unknown: DependencyStatus = 0   // not yet computed
    final val NoDeps: DependencyStatus = 1    // no dependent parameters found
    final val FalseDeps: DependencyStatus = 2 // all dependent parameters are prefixes of non-depended alias types
    final val TrueDeps: DependencyStatus = 3  // some truly dependent parameters exist
    final val StatusMask: DependencyStatus = 3 // the bits indicating actual dependency status
    final val Provisional: DependencyStatus = 4  // set if dependency status can still change due to type variable instantiations
  }

  // ----- Type application: LambdaParam, AppliedType ---------------------

  /** The parameter of a type lambda */
  case class LambdaParam(tl: TypeLambda, n: Int) extends ParamInfo, printing.Showable {
    type ThisName = TypeName

    def isTypeParam(using Context): Boolean = tl.paramNames.head.isTypeName
    def paramName(using Context): tl.ThisName = tl.paramNames(n)
    def paramInfo(using Context): tl.PInfo = tl.paramInfos(n)
    def paramInfoAsSeenFrom(pre: Type)(using Context): tl.PInfo = paramInfo
    def paramInfoOrCompleter(using Context): Type = paramInfo
    def paramRef(using Context): Type = tl.paramRefs(n)

    private var myVariance: FlagSet = UndefinedFlags

    /** Low level setter, only called from Variances.setStructuralVariances */
    def storedVariance_= (v: Variance): Unit =
      myVariance = v

    /** Low level getter, only called from Variances.setStructuralVariances */
    def storedVariance: Variance =
      myVariance

    /** Set the declared variance of this parameter.
     *  @pre the containing lambda is a isDeclaredVarianceLambda
     */
    def declaredVariance_=(v: Variance): Unit =
      assert(tl.isDeclaredVarianceLambda)
      assert(myVariance == UndefinedFlags)
      myVariance = v

    /** The declared variance of this parameter.
     *  @pre the containing lambda is a isDeclaredVarianceLambda
     */
    def declaredVariance: Variance =
      assert(tl.isDeclaredVarianceLambda)
      assert(myVariance != UndefinedFlags)
      myVariance

    /** The declared or structural variance of this parameter. */
    def paramVariance(using Context): Variance =
      if myVariance == UndefinedFlags then
        tl match
          case tl: HKTypeLambda =>
            setStructuralVariances(tl)
          case _ =>
            myVariance = Invariant
      myVariance

    def toText(printer: Printer): Text = printer.toText(this)
  }

  /** A type application `C[T_1, ..., T_n]` */
  abstract case class AppliedType(tycon: Type, args: List[Type])
  extends CachedProxyType with ValueType {

    private var validSuper: Period = Nowhere
    private var cachedSuper: Type = _

    // Boolean caches: 0 = uninitialized, -1 = false, 1 = true
    private var myStableHash: Byte = 0
    private var myGround: Byte = 0

    def isGround(acc: TypeAccumulator[Boolean])(using Context): Boolean =
      if myGround == 0 then myGround = if acc.foldOver(true, this) then 1 else -1
      myGround > 0

    override def underlying(using Context): Type = tycon

    override def superType(using Context): Type =
      if ctx.period != validSuper then
        validSuper = if (tycon.isProvisional) Nowhere else ctx.period
        cachedSuper = tycon match
          case tycon: HKTypeLambda => defn.AnyType
          case tycon: TypeRef if tycon.symbol.isClass => tycon
          case tycon: TypeProxy =>
            if isMatchAlias then validSuper = Nowhere
            tycon.superType.applyIfParameterized(args).normalized
          case _ => defn.AnyType
      cachedSuper

    override def translucentSuperType(using Context): Type = tycon match {
      case tycon: TypeRef if tycon.symbol.isOpaqueAlias =>
        tycon.translucentSuperType.applyIfParameterized(args)
      case _ =>
        tryNormalize.orElse(superType)
    }

    inline def map(inline op: Type => Type)(using Context) =
      def mapArgs(args: List[Type]): List[Type] = args match
        case args @ (arg :: rest) => args.derivedCons(op(arg), mapArgs(rest))
        case nil => nil
      derivedAppliedType(op(tycon), mapArgs(args))

    inline def fold[T](x: T, inline op: (T, Type) => T)(using Context): T =
      def foldArgs(x: T, args: List[Type]): T = args match
        case arg :: rest => foldArgs(op(x, arg), rest)
        case nil => x
      foldArgs(op(x, tycon), args)

    override def tryNormalize(using Context): Type = tycon.stripTypeVar match {
      case tycon: TypeRef =>
        def tryMatchAlias = tycon.info match {
          case MatchAlias(alias) =>
            trace(i"normalize $this", typr, show = true) {
              MatchTypeTrace.recurseWith(this) {
                alias.applyIfParameterized(args.map(_.normalized)).tryNormalize
              }
            }
          case _ =>
            NoType
        }
        tryCompiletimeConstantFold.orElse(tryMatchAlias)
      case _ =>
        NoType
    }

    /** Does this application expand to a match type? */
    def isMatchAlias(using Context): Boolean = tycon.stripTypeVar match
      case tycon: TypeRef =>
        tycon.info match
          case _: MatchAlias => true
          case _ => false
      case _ => false

    /** Is this an unreducible application to wildcard arguments?
     *  This is the case if tycon is higher-kinded. This means
     *  it is a subtype of a hk-lambda, but not a match alias.
     *  (normal parameterized aliases are removed in `appliedTo`).
     *  Applications of higher-kinded type constructors to wildcard arguments
     *  are equivalent to existential types, which are not supported.
     */
    def isUnreducibleWild(using Context): Boolean =
      tycon.isLambdaSub && hasWildcardArg && !isMatchAlias

    def tryCompiletimeConstantFold(using Context): Type = tycon match {
      case tycon: TypeRef if defn.isCompiletimeAppliedType(tycon.symbol) =>
        extension (tp: Type) def fixForEvaluation: Type =
          tp.normalized.dealias match {
            // enable operations for constant singleton terms. E.g.:
            // ```
            // final val one = 1
            // type Two = one.type + one.type
            // ```
            case tp: TermRef => tp.underlying
            case tp => tp
          }

        def constValue(tp: Type): Option[Any] = tp.fixForEvaluation match {
          case ConstantType(Constant(n)) => Some(n)
          case _ => None
        }

        def boolValue(tp: Type): Option[Boolean] = tp.fixForEvaluation match {
          case ConstantType(Constant(n: Boolean)) => Some(n)
          case _ => None
        }

        def intValue(tp: Type): Option[Int] = tp.fixForEvaluation match {
          case ConstantType(Constant(n: Int)) => Some(n)
          case _ => None
        }

        def longValue(tp: Type): Option[Long] = tp.fixForEvaluation match {
          case ConstantType(Constant(n: Long)) => Some(n)
          case _ => None
        }

        def floatValue(tp: Type): Option[Float] = tp.fixForEvaluation match {
          case ConstantType(Constant(n: Float)) => Some(n)
          case _ => None
        }

        def doubleValue(tp: Type): Option[Double] = tp.fixForEvaluation match {
          case ConstantType(Constant(n: Double)) => Some(n)
          case _ => None
        }

        def stringValue(tp: Type): Option[String] = tp.fixForEvaluation match {
          case ConstantType(Constant(n: String)) => Some(n)
          case _ => None
        }

        // Returns Some(true) if the type is a constant.
        // Returns Some(false) if the type is not a constant.
        // Returns None if there is not enough information to determine if the type is a constant.
        // The type is a constant if it is a constant type or a type operation composition of constant types.
        // If we get a type reference for an argument, then the result is not yet known.
        def isConst(tp: Type): Option[Boolean] = tp.dealias match {
          // known to be constant
          case ConstantType(_) => Some(true)
          // currently not a concrete known type
          case TypeRef(NoPrefix,_) => None
          // currently not a concrete known type
          case _: TypeParamRef => None
          // constant if the term is constant
          case t: TermRef => isConst(t.underlying)
          // an operation type => recursively check all argument compositions
          case applied: AppliedType if defn.isCompiletimeAppliedType(applied.typeSymbol) =>
            val argsConst = applied.args.map(isConst)
            if (argsConst.exists(_.isEmpty)) None
            else Some(argsConst.forall(_.get))
          // all other types are considered not to be constant
          case _ => Some(false)
        }

        def expectArgsNum(expectedNum: Int): Unit =
        // We can use assert instead of a compiler type error because this error should not
        // occur since the type signature of the operation enforces the proper number of args.
          assert(args.length == expectedNum, s"Type operation expects $expectedNum arguments but found ${args.length}")

        def natValue(tp: Type): Option[Int] = intValue(tp).filter(n => n >= 0 && n < Int.MaxValue)

        // Runs the op and returns the result as a constant type.
        // If the op throws an exception, then this exception is converted into a type error.
        def runConstantOp(op: => Any): Type =
          val result = try {
            op
          } catch {
            case e: Throwable =>
              throw new TypeError(e.getMessage.nn)
          }
          ConstantType(Constant(result))

        def constantFold1[T](extractor: Type => Option[T], op: T => Any): Option[Type] =
          expectArgsNum(1)
          extractor(args.head).map(a => runConstantOp(op(a)))

        def constantFold2[T](extractor: Type => Option[T], op: (T, T) => Any): Option[Type] =
          constantFold2AB(extractor, extractor, op)

        def constantFold2AB[TA, TB](extractorA: Type => Option[TA], extractorB: Type => Option[TB], op: (TA, TB) => Any): Option[Type] =
          expectArgsNum(2)
          for {
            a <- extractorA(args(0))
            b <- extractorB(args(1))
          } yield runConstantOp(op(a, b))

        def constantFold3[TA, TB, TC](
          extractorA: Type => Option[TA],
          extractorB: Type => Option[TB],
          extractorC: Type => Option[TC],
          op: (TA, TB, TC) => Any
        ): Option[Type] =
          expectArgsNum(3)
          for {
            a <- extractorA(args(0))
            b <- extractorB(args(1))
            c <- extractorC(args(2))
          } yield runConstantOp(op(a, b, c))

        trace(i"compiletime constant fold $this", typr, show = true) {
          val name = tycon.symbol.name
          val owner = tycon.symbol.owner
          val constantType =
            if (defn.isCompiletime_S(tycon.symbol)) {
              constantFold1(natValue, _ + 1)
            } else if (owner == defn.CompiletimeOpsAnyModuleClass) name match {
              case tpnme.Equals     => constantFold2(constValue, _ == _)
              case tpnme.NotEquals  => constantFold2(constValue, _ != _)
              case tpnme.ToString   => constantFold1(constValue, _.toString)
              case tpnme.IsConst    => isConst(args.head).map(b => ConstantType(Constant(b)))
              case _ => None
            } else if (owner == defn.CompiletimeOpsIntModuleClass) name match {
              case tpnme.Abs        => constantFold1(intValue, _.abs)
              case tpnme.Negate     => constantFold1(intValue, x => -x)
              // ToString is deprecated for ops.int, and moved to ops.any
              case tpnme.ToString   => constantFold1(intValue, _.toString)
              case tpnme.Plus       => constantFold2(intValue, _ + _)
              case tpnme.Minus      => constantFold2(intValue, _ - _)
              case tpnme.Times      => constantFold2(intValue, _ * _)
              case tpnme.Div        => constantFold2(intValue, _ / _)
              case tpnme.Mod        => constantFold2(intValue, _ % _)
              case tpnme.Lt         => constantFold2(intValue, _ < _)
              case tpnme.Gt         => constantFold2(intValue, _ > _)
              case tpnme.Ge         => constantFold2(intValue, _ >= _)
              case tpnme.Le         => constantFold2(intValue, _ <= _)
              case tpnme.Xor        => constantFold2(intValue, _ ^ _)
              case tpnme.BitwiseAnd => constantFold2(intValue, _ & _)
              case tpnme.BitwiseOr  => constantFold2(intValue, _ | _)
              case tpnme.ASR        => constantFold2(intValue, _ >> _)
              case tpnme.LSL        => constantFold2(intValue, _ << _)
              case tpnme.LSR        => constantFold2(intValue, _ >>> _)
              case tpnme.Min        => constantFold2(intValue, _ min _)
              case tpnme.Max        => constantFold2(intValue, _ max _)
              case tpnme.NumberOfLeadingZeros => constantFold1(intValue, Integer.numberOfLeadingZeros(_))
              case tpnme.ToLong     => constantFold1(intValue, _.toLong)
              case tpnme.ToFloat    => constantFold1(intValue, _.toFloat)
              case tpnme.ToDouble   => constantFold1(intValue, _.toDouble)
              case _ => None
            } else if (owner == defn.CompiletimeOpsLongModuleClass) name match {
              case tpnme.Abs        => constantFold1(longValue, _.abs)
              case tpnme.Negate     => constantFold1(longValue, x => -x)
              case tpnme.Plus       => constantFold2(longValue, _ + _)
              case tpnme.Minus      => constantFold2(longValue, _ - _)
              case tpnme.Times      => constantFold2(longValue, _ * _)
              case tpnme.Div        => constantFold2(longValue, _ / _)
              case tpnme.Mod        => constantFold2(longValue, _ % _)
              case tpnme.Lt         => constantFold2(longValue, _ < _)
              case tpnme.Gt         => constantFold2(longValue, _ > _)
              case tpnme.Ge         => constantFold2(longValue, _ >= _)
              case tpnme.Le         => constantFold2(longValue, _ <= _)
              case tpnme.Xor        => constantFold2(longValue, _ ^ _)
              case tpnme.BitwiseAnd => constantFold2(longValue, _ & _)
              case tpnme.BitwiseOr  => constantFold2(longValue, _ | _)
              case tpnme.ASR        => constantFold2(longValue, _ >> _)
              case tpnme.LSL        => constantFold2(longValue, _ << _)
              case tpnme.LSR        => constantFold2(longValue, _ >>> _)
              case tpnme.Min        => constantFold2(longValue, _ min _)
              case tpnme.Max        => constantFold2(longValue, _ max _)
              case tpnme.NumberOfLeadingZeros =>
                constantFold1(longValue, java.lang.Long.numberOfLeadingZeros(_))
              case tpnme.ToInt      => constantFold1(longValue, _.toInt)
              case tpnme.ToFloat    => constantFold1(longValue, _.toFloat)
              case tpnme.ToDouble   => constantFold1(longValue, _.toDouble)
              case _ => None
            } else if (owner == defn.CompiletimeOpsFloatModuleClass) name match {
              case tpnme.Abs        => constantFold1(floatValue, _.abs)
              case tpnme.Negate     => constantFold1(floatValue, x => -x)
              case tpnme.Plus       => constantFold2(floatValue, _ + _)
              case tpnme.Minus      => constantFold2(floatValue, _ - _)
              case tpnme.Times      => constantFold2(floatValue, _ * _)
              case tpnme.Div        => constantFold2(floatValue, _ / _)
              case tpnme.Mod        => constantFold2(floatValue, _ % _)
              case tpnme.Lt         => constantFold2(floatValue, _ < _)
              case tpnme.Gt         => constantFold2(floatValue, _ > _)
              case tpnme.Ge         => constantFold2(floatValue, _ >= _)
              case tpnme.Le         => constantFold2(floatValue, _ <= _)
              case tpnme.Min        => constantFold2(floatValue, _ min _)
              case tpnme.Max        => constantFold2(floatValue, _ max _)
              case tpnme.ToInt      => constantFold1(floatValue, _.toInt)
              case tpnme.ToLong     => constantFold1(floatValue, _.toLong)
              case tpnme.ToDouble   => constantFold1(floatValue, _.toDouble)
              case _ => None
            } else if (owner == defn.CompiletimeOpsDoubleModuleClass) name match {
              case tpnme.Abs        => constantFold1(doubleValue, _.abs)
              case tpnme.Negate     => constantFold1(doubleValue, x => -x)
              case tpnme.Plus       => constantFold2(doubleValue, _ + _)
              case tpnme.Minus      => constantFold2(doubleValue, _ - _)
              case tpnme.Times      => constantFold2(doubleValue, _ * _)
              case tpnme.Div        => constantFold2(doubleValue, _ / _)
              case tpnme.Mod        => constantFold2(doubleValue, _ % _)
              case tpnme.Lt         => constantFold2(doubleValue, _ < _)
              case tpnme.Gt         => constantFold2(doubleValue, _ > _)
              case tpnme.Ge         => constantFold2(doubleValue, _ >= _)
              case tpnme.Le         => constantFold2(doubleValue, _ <= _)
              case tpnme.Min        => constantFold2(doubleValue, _ min _)
              case tpnme.Max        => constantFold2(doubleValue, _ max _)
              case tpnme.ToInt      => constantFold1(doubleValue, _.toInt)
              case tpnme.ToLong     => constantFold1(doubleValue, _.toLong)
              case tpnme.ToFloat    => constantFold1(doubleValue, _.toFloat)
              case _ => None
            } else if (owner == defn.CompiletimeOpsStringModuleClass) name match {
              case tpnme.Plus       => constantFold2(stringValue, _ + _)
              case tpnme.Length     => constantFold1(stringValue, _.length)
              case tpnme.Matches    => constantFold2(stringValue, _ matches _)
              case tpnme.Substring  =>
                constantFold3(stringValue, intValue, intValue, (s, b, e) => s.substring(b, e))
              case tpnme.CharAt     =>
                constantFold2AB(stringValue, intValue, _ charAt _)
              case _ => None
            } else if (owner == defn.CompiletimeOpsBooleanModuleClass) name match {
              case tpnme.Not        => constantFold1(boolValue, x => !x)
              case tpnme.And        => constantFold2(boolValue, _ && _)
              case tpnme.Or         => constantFold2(boolValue, _ || _)
              case tpnme.Xor        => constantFold2(boolValue, _ ^ _)
              case _ => None
            } else None

          constantType.getOrElse(NoType)
        }

      case _ => NoType
    }

    def lowerBound(using Context): Type = tycon.stripTypeVar match {
      case tycon: TypeRef =>
        tycon.info match {
          case TypeBounds(lo, hi) =>
            if (lo eq hi) superType // optimization, can profit from caching in this case
            else lo.applyIfParameterized(args)
          case _ => NoType
        }
      case tycon: AppliedType =>
        tycon.lowerBound.applyIfParameterized(args)
      case _ =>
        NoType
    }

    def tyconTypeParams(using Context): List[ParamInfo] = {
      val tparams = tycon.typeParams
      if (tparams.isEmpty) HKTypeLambda.any(args.length).typeParams else tparams
    }

    def hasWildcardArg(using Context): Boolean = args.exists(isBounds)

    def derivedAppliedType(tycon: Type, args: List[Type])(using Context): Type =
      if ((tycon eq this.tycon) && (args eq this.args)) this
      else tycon.appliedTo(args)

    override def computeHash(bs: Binders): Int = doHash(bs, tycon, args)

    override def hashIsStable: Boolean = {
      if (myStableHash == 0) myStableHash = if (tycon.hashIsStable && args.hashIsStable) 1 else -1
      myStableHash > 0
    }

    override def eql(that: Type): Boolean = this `eq` that // safe because applied types are hash-consed separately

    // equals comes from case class; no matching override is needed

    final override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: AppliedType => tycon.equals(that.tycon, bs) && args.equalElements(that.args, bs)
      case _ => false
    }
  }

  final class CachedAppliedType(tycon: Type, args: List[Type], hc: Int) extends AppliedType(tycon, args) {
    myHash = hc
  }

  object AppliedType {
    def apply(tycon: Type, args: List[Type])(using Context): AppliedType = {
      assertUnerased()
      ctx.base.uniqueAppliedTypes.enterIfNew(tycon, args)
    }
  }

  // ----- BoundTypes: ParamRef, RecThis ----------------------------------------

  abstract class BoundType extends CachedProxyType with ValueType {
    type BT <: Type
    val binder: BT
    def copyBoundType(bt: BT): Type
    override def hashIsStable: Boolean = false
  }

  abstract class ParamRef extends BoundType {
    type BT <: LambdaType
    def paramNum: Int
    def paramName: binder.ThisName = binder.paramNames(paramNum)

    override def underlying(using Context): Type = {
      // TODO: update paramInfos's type to nullable
      val infos: List[Type] | Null = binder.paramInfos
      if (infos == null) NoType // this can happen if the referenced generic type is not initialized yet
      else infos(paramNum)
    }

    override def computeHash(bs: Binders): Int = doHash(paramNum, binder.identityHash(bs))

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: ParamRef => paramNum == that.paramNum && binder.equalBinder(that.binder, bs)
      case _ => false
    }

    protected def kindString: String

    override def toString: String =
      try s"${kindString}ParamRef($paramName)"
      catch {
        case ex: IndexOutOfBoundsException => s"ParamRef(<bad index: $paramNum>)"
      }
  }

  /** Only created in `binder.paramRefs`. Use `binder.paramRefs(paramNum)` to
   *  refer to `TermParamRef(binder, paramNum)`.
   */
  abstract case class TermParamRef(binder: TermLambda, paramNum: Int) extends ParamRef with SingletonType {
    type BT = TermLambda
    def kindString: String = "Term"
    def copyBoundType(bt: BT): Type = bt.paramRefs(paramNum)
  }

  private final class TermParamRefImpl(binder: TermLambda, paramNum: Int) extends TermParamRef(binder, paramNum)

  /** Only created in `binder.paramRefs`. Use `binder.paramRefs(paramNum)` to
   *  refer to `TypeParamRef(binder, paramNum)`.
   */
  abstract case class TypeParamRef(binder: TypeLambda, paramNum: Int) extends ParamRef {
    type BT = TypeLambda
    def kindString: String = "Type"
    def copyBoundType(bt: BT): Type = bt.paramRefs(paramNum)

    /** Optimized version of occursIn, avoid quadratic blowup when solving
     *  constraints over large ground types.
     */
    override def occursIn(that: Type)(using Context): Boolean = !that.isGround && super.occursIn(that)

    /** Looking only at the structure of `bound`, is one of the following true?
     *     - fromBelow and param <:< bound
     *     - !fromBelow and param >:> bound
     */
    def occursIn(bound: Type, fromBelow: Boolean)(using Context): Boolean = bound.stripTypeVar match {
      case bound: ParamRef => bound == this
      case bound: AndType  => occursIn(bound.tp1, fromBelow) && occursIn(bound.tp2, fromBelow)
      case bound: OrType   => occursIn(bound.tp1, fromBelow) || occursIn(bound.tp2, fromBelow)
      case _ => false
    }
  }

  private final class TypeParamRefImpl(binder: TypeLambda, paramNum: Int) extends TypeParamRef(binder, paramNum)

  /** a self-reference to an enclosing recursive type. The only creation method is
   *  `binder.recThis`, returning `RecThis(binder)`.
   */
  abstract case class RecThis(binder: RecType) extends BoundType with SingletonType {
    type BT = RecType
    override def underlying(using Context): RecType = binder
    def copyBoundType(bt: BT): RecThis = bt.recThis

    // need to customize hashCode and equals to prevent infinite recursion
    // between RecTypes and RecRefs.
    override def computeHash(bs: Binders): Int = addDelta(binder.identityHash(bs), 41)

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: RecThis => binder.equalBinder(that.binder, bs)
      case _ => false
    }

    override def toString: String =
      try s"RecThis(${binder.hashCode})"
      catch {
        case ex: NullPointerException => s"RecThis(<under construction>)"
      }
  }

  private final class RecThisImpl(binder: RecType) extends RecThis(binder)

  // @sharable private var skid: Int = 0

  // ----- Skolem types -----------------------------------------------

  /** A skolem type reference with underlying type `info`.
   *
   * For Dotty, a skolem type is a singleton type of some unknown value of type `info`.
   * Note that care is needed when creating them, since not all types need to be inhabited.
   * A skolem is equal to itself and no other type.
   */
  case class SkolemType(info: Type) extends CachedProxyType with ValueType with SingletonType {
    override def underlying(using Context): Type = info
    def derivedSkolemType(info: Type)(using Context): SkolemType =
      if (info eq this.info) this else SkolemType(info)

    override def computeHash(bs: Binders): Int = identityHash(bs)
    override def equals(that: Any): Boolean = this.eq(that.asInstanceOf[AnyRef])

    def withName(name: Name): this.type = { myRepr = name; this }

    //skid += 1
    //val id = skid
    //assert(id != 10)

    private var myRepr: Name | Null = null
    def repr(using Context): Name = {
      if (myRepr == null) myRepr = SkolemName.fresh()
      myRepr.nn
    }

    override def toString: String = s"SkolemType($hashCode)"
  }

  /** A skolem type used to wrap the type of the qualifier of a selection.
   *
   *  When typing a selection `e.f`, if `e` is unstable then we unconditionally
   *  skolemize it. We use a subclass of `SkolemType` for this so that
   *  [[TypeOps#asSeenFrom]] may treat it specially for optimization purposes,
   *  see its implementation for more details.
   */
  class QualSkolemType(info: Type) extends SkolemType(info) {
    override def derivedSkolemType(info: Type)(using Context): SkolemType =
      if (info eq this.info) this else QualSkolemType(info)
  }
  object QualSkolemType {
    def apply(info: Type): QualSkolemType = new QualSkolemType(info)
  }

  // ------------ Type variables ----------------------------------------

  /** In a TypeApply tree, a TypeVar is created for each argument type to be inferred.
   *  Every type variable is referred to by exactly one inferred type parameter of some
   *  TypeApply tree.
   *
   *  A type variable is essentially a switch that models some part of a substitution.
   *  It is first linked to `origin`, a poly param that's in the current constraint set.
   *  It can then be (once) instantiated to some other type. The instantiation is
   *  recorded in the type variable itself, or else, if the current type state
   *  is different from the variable's creation state (meaning unrolls are possible)
   *  in the current typer state.
   *
   *  @param  origin        The parameter that's tracked by the type variable.
   *  @param  creatorState  The typer state in which the variable was created.
   *  @param  nestingLevel  Symbols with a nestingLevel strictly greater than this
   *                        will not appear in the instantiation of this type variable.
   *                        This is enforced in `ConstraintHandling` by:
   *                        - Maintaining the invariant that the `nonParamBounds`
   *                          of a type variable never refer to a type with a
   *                          greater `nestingLevel` (see `legalBound` for the reason
   *                          why this cannot be delayed until instantiation).
   *                        - On instantiation, replacing any param in the param bound
   *                          with a level greater than nestingLevel (see `fullLowerBound`).
   */
  final class TypeVar private(initOrigin: TypeParamRef, creatorState: TyperState | Null, val nestingLevel: Int) extends CachedProxyType with ValueType {
    private var currentOrigin = initOrigin

    def origin: TypeParamRef = currentOrigin

    /** Set origin to new parameter. Called if we merge two conflicting constraints.
     *  See OrderingConstraint#merge, OrderingConstraint#rename
     */
    def setOrigin(p: TypeParamRef) = currentOrigin = p

    /** The permanent instance type of the variable, or NoType is none is given yet */
    private var myInst: Type = NoType

    private[core] def inst: Type = myInst
    private[core] def setInst(tp: Type): Unit =
      myInst = tp
      if tp.exists && owningState != null then
        val owningState1 = owningState.uncheckedNN.get
        if owningState1 != null then
          owningState1.ownedVars -= this
          owningState = null // no longer needed; null out to avoid a memory leak

    private[core] def resetInst(ts: TyperState): Unit =
      myInst = NoType
      owningState = new WeakReference(ts)

    /** The state owning the variable. This is at first `creatorState`, but it can
     *  be changed to an enclosing state on a commit.
     */
    private[core] var owningState: WeakReference[TyperState] | Null =
      if (creatorState == null) null else new WeakReference(creatorState)

    /** The instance type of this variable, or NoType if the variable is currently
     *  uninstantiated
     */
    def instanceOpt(using Context): Type =
      if (inst.exists) inst else ctx.typerState.constraint.instType(this)

    /** Is the variable already instantiated? */
    def isInstantiated(using Context): Boolean = instanceOpt.exists

    /** Instantiate variable with given type */
    def instantiateWith(tp: Type)(using Context): Type = {
      assert(tp ne this, i"self instantiation of $origin, constraint = ${ctx.typerState.constraint}")
      assert(!myInst.exists, i"$origin is already instantiated to $myInst but we attempted to instantiate it to $tp")
      typr.println(i"instantiating $this with $tp")

      if Config.checkConstraintsSatisfiable then
        assert(currentEntry.bounds.contains(tp),
          i"$origin is constrained to be $currentEntry but attempted to instantiate it to $tp")

      if ((ctx.typerState eq owningState.nn.get.uncheckedNN) && !TypeComparer.subtypeCheckInProgress)
        setInst(tp)
      ctx.typerState.constraint = ctx.typerState.constraint.replace(origin, tp)
      tp
    }

    /** Instantiate variable from the constraints over its `origin`.
     *  If `fromBelow` is true, the variable is instantiated to the lub
     *  of its lower bounds in the current constraint; otherwise it is
     *  instantiated to the glb of its upper bounds. However, a lower bound
     *  instantiation can be a singleton type only if the upper bound
     *  is also a singleton type.
     */
    def instantiate(fromBelow: Boolean)(using Context): Type =
      val tp = TypeComparer.instanceType(origin, fromBelow)
      if myInst.exists then // The line above might have triggered instantiation of the current type variable
        myInst
      else
        instantiateWith(tp)

    /** For uninstantiated type variables: the entry in the constraint (either bounds or
     *  provisional instance value)
     */
    private def currentEntry(using Context): Type = ctx.typerState.constraint.entry(origin)

    /** For uninstantiated type variables: Is the lower bound different from Nothing? */
    def hasLowerBound(using Context): Boolean = !currentEntry.loBound.isExactlyNothing

    /** For uninstantiated type variables: Is the upper bound different from Any? */
    def hasUpperBound(using Context): Boolean = !currentEntry.hiBound.isRef(defn.AnyClass)

    /** Unwrap to instance (if instantiated) or origin (if not), until result
     *  is no longer a TypeVar
     */
    override def stripTypeVar(using Context): Type = {
      val inst = instanceOpt
      if (inst.exists) inst.stripTypeVar else origin
    }

    override def stripped(using Context): Type = stripTypeVar.stripped

    /** If the variable is instantiated, its instance, otherwise its origin */
    override def underlying(using Context): Type = {
      val inst = instanceOpt
      if (inst.exists) inst else origin
    }

    override def computeHash(bs: Binders): Int = identityHash(bs)
    override def equals(that: Any): Boolean = this.eq(that.asInstanceOf[AnyRef])

    override def toString: String = {
      def instStr = if (inst.exists) s" -> $inst" else ""
      s"TypeVar($origin$instStr)"
    }
  }
  object TypeVar:
    def apply(using Context)(initOrigin: TypeParamRef, creatorState: TyperState | Null, nestingLevel: Int = ctx.nestingLevel) =
      new TypeVar(initOrigin, creatorState, nestingLevel)

  type TypeVars = SimpleIdentitySet[TypeVar]

  // ------ MatchType ---------------------------------------------------------------

  /**    scrutinee match { case_1 ... case_n }
   *
   *  where
   *
   *     case_i  =   [X1, ..., Xn] patternType => resultType
   *
   *  and `X_1,...X_n` are the type variables bound in `patternType`
   */
  abstract case class MatchType(bound: Type, scrutinee: Type, cases: List[Type]) extends CachedProxyType with ValueType {
    def derivedMatchType(bound: Type, scrutinee: Type, cases: List[Type])(using Context): MatchType =
      if (bound.eq(this.bound) && scrutinee.eq(this.scrutinee) && cases.eqElements(this.cases)) this
      else MatchType(bound, scrutinee, cases)

    def caseType(tp: Type)(using Context): Type = tp match {
      case tp: HKTypeLambda => caseType(tp.resType)
      case defn.MatchCase(_, body) => body
    }

    def alternatives(using Context): List[Type] = cases.map(caseType)
    def underlying(using Context): Type = bound

    private var myReduced: Type | Null = null
    private var reductionContext: util.MutableMap[Type, Type] = _

    override def tryNormalize(using Context): Type =
      try
        reduced.normalized
      catch
        case ex: Throwable =>
          handleRecursive("normalizing", s"${scrutinee.show} match ..." , ex)

    def reduced(using Context): Type = {

      def contextInfo(tp: Type): Type = tp match {
        case tp: TypeParamRef =>
          val constraint = ctx.typerState.constraint
          if (constraint.entry(tp).exists) TypeComparer.fullBounds(tp)
          else NoType
        case tp: TypeRef =>
          val bounds = ctx.gadt.fullBounds(tp.symbol)
          if (bounds == null) NoType else bounds
        case tp: TypeVar =>
          tp.underlying
      }

      def updateReductionContext(footprint: collection.Set[Type]): Unit =
        reductionContext = util.HashMap()
        for (tp <- footprint)
          reductionContext(tp) = contextInfo(tp)
        typr.println(i"footprint for $this $hashCode: ${footprint.toList.map(x => (x, contextInfo(x)))}%, %")

      def isUpToDate: Boolean =
        reductionContext.keysIterator.forall { tp =>
          reductionContext(tp) `eq` contextInfo(tp)
        }

      record("MatchType.reduce called")
      if !Config.cacheMatchReduced
          || myReduced == null
          || !isUpToDate
          || MatchTypeTrace.isRecording
      then
        record("MatchType.reduce computed")
        if (myReduced != null) record("MatchType.reduce cache miss")
        myReduced =
          trace(i"reduce match type $this $hashCode", matchTypes, show = true) {
            def matchCases(cmp: TrackingTypeComparer): Type =
              val saved = ctx.typerState.snapshot()
              try cmp.matchCases(scrutinee.normalized, cases)
              catch case ex: Throwable =>
                handleRecursive("reduce type ", i"$scrutinee match ...", ex)
              finally
                updateReductionContext(cmp.footprint)
                ctx.typerState.resetTo(saved)
                  // this drops caseLambdas in constraint and undoes any typevar
                  // instantiations during matchtype reduction

            TypeComparer.tracked(matchCases)
          }
      myReduced.nn
    }

    override def computeHash(bs: Binders): Int = doHash(bs, scrutinee, bound :: cases)

    override def eql(that: Type): Boolean = that match {
      case that: MatchType =>
        bound.eq(that.bound) && scrutinee.eq(that.scrutinee) && cases.eqElements(that.cases)
      case _ => false
    }
  }

  class CachedMatchType(bound: Type, scrutinee: Type, cases: List[Type]) extends MatchType(bound, scrutinee, cases)

  object MatchType {
    def apply(bound: Type, scrutinee: Type, cases: List[Type])(using Context): MatchType =
      unique(new CachedMatchType(bound, scrutinee, cases))
  }

  // ------ ClassInfo, Type Bounds --------------------------------------------------

  type TypeOrSymbol = Type | Symbol

  /** Roughly: the info of a class during a period.
   *  @param prefix           The prefix on which parents, decls, and selfType need to be rebased.
   *  @param cls              The class symbol.
   *  @param declaredParents  The parent types of this class.
   *                          These are all normalized to be TypeRefs by moving any refinements
   *                          to be member definitions of the class itself.
   *                          Unlike `parents`, the types are not seen as seen from `prefix`.
   *  @param decls            The symbols defined directly in this class.
   *  @param selfInfo         The type of `this` in this class, if explicitly given,
   *                          NoType otherwise. If class is compiled from source, can also
   *                          be a reference to the self symbol containing the type.
   */
  abstract case class ClassInfo(
      prefix: Type,
      cls: ClassSymbol,
      declaredParents: List[Type],
      decls: Scope,
      selfInfo: TypeOrSymbol) extends CachedGroundType with TypeType {

    private var selfTypeCache: Type | Null = null
    private var appliedRefCache: Type | Null = null

    /** The self type of a class is the conjunction of
     *   - the explicit self type if given (or the info of a given self symbol), and
     *   - the fully applied reference to the class itself.
     */
    def selfType(using Context): Type = {
      val clsd = cls.classDenot
      if (selfTypeCache == null)
        selfTypeCache = {
          val givenSelf = clsd.givenSelfType
          if (!givenSelf.isValueType) appliedRef
          else if (clsd.is(Module)) givenSelf
          else if (ctx.erasedTypes) appliedRef
          else AndType(givenSelf, appliedRef)
        }
      selfTypeCache.nn
    }

    def appliedRef(using Context): Type = {
      if (appliedRefCache == null)
        appliedRefCache =
          TypeRef(prefix, cls).appliedTo(cls.classDenot.typeParams.map(_.typeRef))
      appliedRefCache.nn
    }

    // cached because baseType needs parents
    private var parentsCache: List[Type] | Null = null

    override def parents(using Context): List[Type] = {
      if (parentsCache == null)
        parentsCache = declaredParents.mapConserve(_.asSeenFrom(prefix, cls.owner))
      parentsCache.nn
    }

    protected def newLikeThis(prefix: Type, declaredParents: List[Type], decls: Scope, selfInfo: TypeOrSymbol)(using Context): ClassInfo =
      ClassInfo(prefix, cls, declaredParents, decls, selfInfo)

    def derivedClassInfo(prefix: Type)(using Context): ClassInfo =
      if (prefix eq this.prefix) this
      else newLikeThis(prefix, declaredParents, decls, selfInfo)

    def derivedClassInfo(prefix: Type = this.prefix, declaredParents: List[Type] = this.declaredParents, decls: Scope = this.decls, selfInfo: TypeOrSymbol = this.selfInfo)(using Context): ClassInfo =
      if ((prefix eq this.prefix) && (declaredParents eq this.declaredParents) && (decls eq this.decls) && (selfInfo eq this.selfInfo)) this
      else newLikeThis(prefix, declaredParents, decls, selfInfo)

    /** If this class has opaque type alias members, a new class info
     *  with their aliases added as refinements to the self type of the class.
     *  Otherwise, this classInfo.
     *  If there are opaque alias members, updates `cls` to have `Opaque` flag as a side effect.
     */
    def integrateOpaqueMembers(using Context): ClassInfo =
      decls.toList.foldLeft(this) { (cinfo, sym) =>
        if sym.isOpaqueAlias then
          cls.setFlag(Opaque)
          def force(using Context) =
            if sym.isOpaqueAlias then // could have been reset because of a syntax error
              sym.infoOrCompleter match
                case completer: LazyType =>
                  completer.complete(sym) // will update the LazyRef
                  null                    // tells the LazyRef to use the updated value
                case info => // can occur under cyclic references, e.g. i6225.scala
                  defn.AnyType
            else defn.AnyType         // dummy type in case of errors
          def refineSelfType(selfType: Type) =
            RefinedType(selfType, sym.name,
              TypeAlias(
                withMode(Mode.CheckCyclic)(
                  LazyRef.of(force))))
          cinfo.selfInfo match
            case self: Type =>
              cinfo.derivedClassInfo(
                selfInfo = refineSelfType(self.orElse(defn.AnyType)))
            case self: Symbol =>
              self.info = refineSelfType(self.info)
              cinfo
        else cinfo
      }

    override def computeHash(bs: Binders  | Null): Int = doHash(bs, cls, prefix)
    override def hashIsStable: Boolean = prefix.hashIsStable && declaredParents.hashIsStable

    override def eql(that: Type): Boolean = that match {
      case that: ClassInfo =>
        prefix.eq(that.prefix) &&
        cls.eq(that.cls) &&
        declaredParents.eqElements(that.declaredParents) &&
        decls.eq(that.decls) &&
        selfInfo.eq(that.selfInfo)
      case _ => false
    }

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: ClassInfo =>
        prefix.equals(that.prefix, bs) &&
        cls.eq(that.cls) &&
        declaredParents.equalElements(that.declaredParents, bs) &&
        decls.eq(that.decls) &&
        selfInfo.eq(that.selfInfo)
      case _ => false
    }

    override def toString: String = s"ClassInfo($prefix, $cls, $declaredParents)"
  }

  class CachedClassInfo(prefix: Type, cls: ClassSymbol, declaredParents: List[Type], decls: Scope, selfInfo: TypeOrSymbol)
    extends ClassInfo(prefix, cls, declaredParents, decls, selfInfo)

  /** A class for temporary class infos where `parents` are not yet known */
  final class TempClassInfo(prefix: Type, cls: ClassSymbol, decls: Scope, selfInfo: TypeOrSymbol)
  extends CachedClassInfo(prefix, cls, Nil, decls, selfInfo) {

    /** Convert to classinfo with known parents */
    def finalized(parents: List[Type])(using Context): ClassInfo =
      ClassInfo(prefix, cls, parents, decls, selfInfo)

    override def newLikeThis(prefix: Type, declaredParents: List[Type], decls: Scope, selfInfo: TypeOrSymbol)(using Context): ClassInfo =
      TempClassInfo(prefix, cls, decls, selfInfo)

    override def toString: String = s"TempClassInfo($prefix, $cls)"
  }

  object ClassInfo {
    def apply(prefix: Type, cls: ClassSymbol, declaredParents: List[Type], decls: Scope, selfInfo: TypeOrSymbol = NoType)(using Context): ClassInfo =
      unique(new CachedClassInfo(prefix, cls, declaredParents, decls, selfInfo))
  }

  /** Type bounds >: lo <: hi */
  abstract case class TypeBounds(lo: Type, hi: Type) extends CachedProxyType with TypeType {

    assert(lo.isInstanceOf[TermType], lo)
    assert(hi.isInstanceOf[TermType], hi)

    override def underlying(using Context): Type = hi

    /** The non-alias type bounds type with given bounds */
    def derivedTypeBounds(lo: Type, hi: Type)(using Context): TypeBounds =
      if ((lo eq this.lo) && (hi eq this.hi)) this
      else TypeBounds(lo, hi)

    def contains(tp: Type)(using Context): Boolean = tp match {
      case tp: TypeBounds => lo <:< tp.lo && tp.hi <:< hi
      case tp: ClassInfo =>
        val cls = tp.cls
        // Note: Taking a normal typeRef does not work here. A normal ref might contain
        // also other information about the named type (e.g. bounds).
        contains(
          TypeRef(tp.prefix, cls)
            .withDenot(new UniqueRefDenotation(cls, tp, cls.validFor, tp.prefix)))
      case _ =>
        lo <:< tp && tp <:< hi
    }

    def & (that: TypeBounds)(using Context): TypeBounds =
      // This will try to preserve the FromJavaObjects type in upper bounds.
      // For example, (? <: FromJavaObjects | Null) & (? <: Any),
      // we want to get (? <: FromJavaObjects | Null) intead of (? <: Any),
      // because we may check the result <:< (? <: Object | Null) later.
      if this.hi.containsFromJavaObject
        && (this.hi frozen_<:< that.hi)
        && (that.lo frozen_<:< this.lo) then
        // FromJavaObject in tp1.hi guarantees tp2.hi <:< tp1.hi
        // prefer tp1 if FromJavaObject is in its hi
        this
      else if that.hi.containsFromJavaObject
        && (that.hi frozen_<:< this.hi)
        && (this.lo frozen_<:< that.lo) then
        // Similarly, prefer tp2 if FromJavaObject is in its hi
        that
      else if (this.lo frozen_<:< that.lo) && (that.hi frozen_<:< this.hi) then that
      else if (that.lo frozen_<:< this.lo) && (this.hi frozen_<:< that.hi) then this
      else TypeBounds(this.lo | that.lo, this.hi & that.hi)

    def | (that: TypeBounds)(using Context): TypeBounds =
      if ((this.lo frozen_<:< that.lo) && (that.hi frozen_<:< this.hi)) this
      else if ((that.lo frozen_<:< this.lo) && (this.hi frozen_<:< that.hi)) that
      else TypeBounds(this.lo & that.lo, this.hi | that.hi)

    override def & (that: Type)(using Context): Type = that match {
      case that: TypeBounds => this & that
      case _ => super.& (that)
    }

    override def | (that: Type)(using Context): Type = that match {
      case that: TypeBounds => this | that
      case _ => super.| (that)
    }

    override def computeHash(bs: Binders): Int = doHash(bs, lo, hi)
    override def hashIsStable: Boolean = lo.hashIsStable && hi.hashIsStable

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: AliasingBounds => false
      case that: TypeBounds => lo.equals(that.lo, bs) && hi.equals(that.hi, bs)
      case _ => false
    }

    override def eql(that: Type): Boolean = that match {
      case that: AliasingBounds => false
      case that: TypeBounds => lo.eq(that.lo) && hi.eq(that.hi)
      case _ => false
    }
  }

  class RealTypeBounds(lo: Type, hi: Type) extends TypeBounds(lo, hi)

  /** Common supertype of `TypeAlias` and `MatchAlias` */
  abstract class AliasingBounds(val alias: Type) extends TypeBounds(alias, alias) {

    def derivedAlias(alias: Type)(using Context): AliasingBounds

    override def computeHash(bs: Binders): Int = doHash(bs, alias)
    override def hashIsStable: Boolean = alias.hashIsStable

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: AliasingBounds => this.isTypeAlias == that.isTypeAlias && alias.equals(that.alias, bs)
      case _ => false
    }

    // equals comes from case class; no matching override is needed

    override def eql(that: Type): Boolean = that match {
      case that: AliasingBounds => this.isTypeAlias == that.isTypeAlias && alias.eq(that.alias)
      case _ => false
    }
  }

  /**    = T
   */
  class TypeAlias(alias: Type) extends AliasingBounds(alias) {
    def derivedAlias(alias: Type)(using Context): AliasingBounds =
      if (alias eq this.alias) this else TypeAlias(alias)
  }

  /**    = T     where `T` is a `MatchType`
   *
   *  Match aliases are treated differently from type aliases. Their sides are mutually
   *  subtypes of each other but one side is not generally substitutable for the other.
   *  If we assumed full substitutivity, we would have to reject all recursive match
   *  aliases (or else take the jump and allow full recursive types).
   */
  class MatchAlias(alias: Type) extends AliasingBounds(alias) {
    def derivedAlias(alias: Type)(using Context): AliasingBounds =
      if (alias eq this.alias) this else MatchAlias(alias)
  }

  object TypeBounds {
    def apply(lo: Type, hi: Type)(using Context): TypeBounds =
      unique(new RealTypeBounds(lo, hi))
    def empty(using Context): TypeBounds =
      val result = ctx.base.emptyTypeBounds
      if result == null then
        ctx.base.emptyTypeBounds = apply(defn.NothingType, defn.AnyType)
        empty
      else
        result
    def emptyPolyKind(using Context): TypeBounds = apply(defn.NothingType, defn.AnyKindType)
    def upper(hi: Type)(using Context): TypeBounds = apply(defn.NothingType, hi)
    def lower(lo: Type)(using Context): TypeBounds = apply(lo, defn.AnyType)
  }

  object TypeAlias {
    def apply(alias: Type)(using Context): TypeAlias = unique(new TypeAlias(alias))
    def unapply(tp: TypeAlias): Option[Type] = Some(tp.alias)
  }

  object MatchAlias {
    def apply(alias: Type)(using Context): MatchAlias = unique(new MatchAlias(alias))
    def unapply(tp: MatchAlias): Option[Type] = Some(tp.alias)
  }

  // ----- Annotated and Import types -----------------------------------------------

  /** An annotated type tpe @ annot */
  abstract case class AnnotatedType(parent: Type, annot: Annotation) extends CachedProxyType with ValueType {

    override def underlying(using Context): Type = parent

    def derivedAnnotatedType(parent: Type, annot: Annotation)(using Context): AnnotatedType =
      if ((parent eq this.parent) && (annot eq this.annot)) this
      else AnnotatedType(parent, annot)

    override def stripTypeVar(using Context): Type =
      derivedAnnotatedType(parent.stripTypeVar, annot)

    override def stripAnnots(using Context): Type = parent.stripAnnots

    override def stripped(using Context): Type = parent.stripped

    private var isRefiningKnown = false
    private var isRefiningCache: Boolean = _

    def isRefining(using Context): Boolean = {
      if (!isRefiningKnown) {
        isRefiningCache = annot.symbol.derivesFrom(defn.RefiningAnnotationClass)
        isRefiningKnown = true
      }
      isRefiningCache
    }

    // equals comes from case class; no matching override is needed

    override def computeHash(bs: Binders): Int =
      doHash(bs, System.identityHashCode(annot), parent)
    override def hashIsStable: Boolean =
      parent.hashIsStable

    override def eql(that: Type): Boolean = that match
      case that: AnnotatedType => (parent eq that.parent) && (annot eq that.annot)
      case _ => false

    override def iso(that: Any, bs: BinderPairs): Boolean = that match
      case that: AnnotatedType => parent.equals(that.parent, bs) && (annot eq that.annot)
      case _ => false
  }

  class CachedAnnotatedType(parent: Type, annot: Annotation) extends AnnotatedType(parent, annot)

  object AnnotatedType:
    def make(underlying: Type, annots: List[Annotation])(using Context): Type =
      annots.foldLeft(underlying)(apply(_, _))
    def apply(parent: Type, annot: Annotation)(using Context): AnnotatedType =
      unique(CachedAnnotatedType(parent, annot))

  // Special type objects and classes -----------------------------------------------------

  /** The type of an erased array */
  abstract case class JavaArrayType(elemType: Type) extends CachedGroundType with ValueType {
    def derivedJavaArrayType(elemtp: Type)(using Context): JavaArrayType =
      if (elemtp eq this.elemType) this else JavaArrayType(elemtp)

    override def computeHash(bs: Binders): Int = doHash(bs, elemType)
    override def hashIsStable: Boolean = elemType.hashIsStable

    override def eql(that: Type): Boolean = that match {
      case that: JavaArrayType => elemType.eq(that.elemType)
      case _ => false
    }
  }
  final class CachedJavaArrayType(elemType: Type) extends JavaArrayType(elemType)
  object JavaArrayType {
    def apply(elemType: Type)(using Context): JavaArrayType = unique(new CachedJavaArrayType(elemType))
  }

  /** The type of an import clause tree */
  case class ImportType(expr: Tree) extends UncachedGroundType

  /** Sentinel for "missing type" */
  @sharable case object NoType extends CachedGroundType {
    override def computeHash(bs: Binders): Int = hashSeed
  }

  /** Missing prefix */
  @sharable case object NoPrefix extends CachedGroundType {
    override def computeHash(bs: Binders): Int = hashSeed
  }

  /** A common superclass of `ErrorType` and `TryDynamicCallSite`. Instances of this
   *  class are at the same time subtypes and supertypes of every other type.
   */
  abstract class FlexType extends UncachedGroundType with ValueType

  abstract class ErrorType extends FlexType {
    def msg(using Context): Message
  }

  object ErrorType:
    def apply(m: Message)(using Context): ErrorType =
      val et = new PreviousErrorType
      ctx.base.errorTypeMsg(et) = m
      et
  end ErrorType

  class PreviousErrorType extends ErrorType:
    def msg(using Context): Message =
      ctx.base.errorTypeMsg.get(this) match
        case Some(m) => m
        case None => "error message from previous run no longer available"

  object UnspecifiedErrorType extends ErrorType {
    override def msg(using Context): Message = "unspecified error"
  }

  /* Type used to track Select nodes that could not resolve a member and their qualifier is a scala.Dynamic. */
  object TryDynamicCallType extends FlexType

  /** Wildcard type, possibly with bounds */
  abstract case class WildcardType(optBounds: Type) extends CachedGroundType with TermType {

    def effectiveBounds(using Context): TypeBounds = optBounds match
      case bounds: TypeBounds => bounds
      case _ => TypeBounds.empty

    def derivedWildcardType(optBounds: Type)(using Context): WildcardType =
      if (optBounds eq this.optBounds) this
      else if (!optBounds.exists) WildcardType
      else WildcardType(optBounds.asInstanceOf[TypeBounds])

    override def computeHash(bs: Binders): Int = doHash(bs, optBounds)
    override def hashIsStable: Boolean = optBounds.hashIsStable

    override def eql(that: Type): Boolean = that match {
      case that: WildcardType => optBounds.eq(that.optBounds)
      case _ => false
    }

    // equals comes from case class; no matching override is needed

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: WildcardType => optBounds.equals(that.optBounds, bs)
      case _ => false
    }
  }

  final class CachedWildcardType(optBounds: Type) extends WildcardType(optBounds)

  @sharable object WildcardType extends WildcardType(NoType) {
    def apply(bounds: TypeBounds)(using Context): WildcardType =
      if bounds eq TypeBounds.empty then
        val result = ctx.base.emptyWildcardBounds
        if result == null then
          ctx.base.emptyWildcardBounds = unique(CachedWildcardType(bounds))
          apply(bounds)
        else
          result
      else unique(CachedWildcardType(bounds))
  }

  /** An extractor for single abstract method types.
   *  A type is a SAM type if it is a reference to a class or trait, which
   *
   *   - has a single abstract method with a method type (ExprType
   *     and PolyType not allowed!) whose result type is not an implicit function type
   *     and which is not marked inline.
   *   - can be instantiated without arguments or with just () as argument.
   *
   *  The pattern `SAMType(sam)` matches a SAM type, where `sam` is the
   *  type of the single abstract method.
   */
  object SAMType {
    def zeroParamClass(tp: Type)(using Context): Type = tp match {
      case tp: ClassInfo =>
        def zeroParams(tp: Type): Boolean = tp.stripPoly match {
          case mt: MethodType => mt.paramInfos.isEmpty && !mt.resultType.isInstanceOf[MethodType]
          case et: ExprType => true
          case _ => false
        }
        // `ContextFunctionN` does not have constructors
        val ctor = tp.cls.primaryConstructor
        if (!ctor.exists || zeroParams(ctor.info)) tp
        else NoType
      case tp: AppliedType =>
        zeroParamClass(tp.superType)
      case tp: TypeRef =>
        zeroParamClass(tp.underlying)
      case tp: RefinedType =>
        zeroParamClass(tp.underlying)
      case tp: TypeBounds =>
        zeroParamClass(tp.underlying)
      case tp: TypeVar =>
        zeroParamClass(tp.underlying)
      case tp: AnnotatedType =>
        zeroParamClass(tp.underlying)
      case _ =>
        NoType
    }
    def isInstantiatable(tp: Type)(using Context): Boolean = zeroParamClass(tp) match {
      case cinfo: ClassInfo if !cinfo.cls.isOneOf(FinalOrSealed) =>
        val selfType = cinfo.selfType.asSeenFrom(tp, cinfo.cls)
        tp <:< selfType
      case _ =>
        false
    }
    def unapply(tp: Type)(using Context): Option[MethodType] =
      if (isInstantiatable(tp)) {
        val absMems = tp.possibleSamMethods
        if (absMems.size == 1)
          absMems.head.info match {
            case mt: MethodType if !mt.isParamDependent &&
                !defn.isContextFunctionType(mt.resultType) =>
              val cls = tp.classSymbol

              // Given a SAM type such as:
              //
              //     import java.util.function.Function
              //     Function[? >: String, ? <: Int]
              //
              // the single abstract method will have type:
              //
              //     (x: Function[? >: String, ? <: Int]#T): Function[? >: String, ? <: Int]#R
              //
              // which is not implementable outside of the scope of Function.
              //
              // To avoid this kind of issue, we approximate references to
              // parameters of the SAM type by their bounds, this way in the
              // above example we get:
              //
              //    (x: String): Int
              val approxParams = new ApproximatingTypeMap {
                def apply(tp: Type): Type = tp match {
                  case tp: TypeRef if tp.symbol.isAllOf(ClassTypeParam) && tp.symbol.owner == cls =>
                    tp.info match {
                      case info: AliasingBounds =>
                        mapOver(info.alias)
                      case TypeBounds(lo, hi) =>
                        range(atVariance(-variance)(apply(lo)), apply(hi))
                      case _ =>
                        range(defn.NothingType, defn.AnyType) // should happen only in error cases
                    }
                  case _ =>
                    mapOver(tp)
                }
              }
              val approx =
                if ctx.owner.isContainedIn(cls) then mt
                else approxParams(mt).asInstanceOf[MethodType]
              Some(approx)
            case _ =>
              None
          }
        else if (tp isRef defn.PartialFunctionClass)
          // To maintain compatibility with 2.x, we treat PartialFunction specially,
          // pretending it is a SAM type. In the future it would be better to merge
          // Function and PartialFunction, have Function1 contain a isDefinedAt method
          //     def isDefinedAt(x: T) = true
          // and overwrite that method whenever the function body is a sequence of
          // case clauses.
          absMems.find(_.symbol.name == nme.apply).map(_.info.asInstanceOf[MethodType])
        else None
      }
      else None
  }

  // ----- TypeMaps --------------------------------------------------------------------

  /** Where a traversal should stop */
  enum StopAt:
    case None    // traverse everything
    case Package // stop at package references
    case Static  // stop at static references

  /** Common base class of TypeMap and TypeAccumulator */
  abstract class VariantTraversal:
    protected[core] var variance: Int = 1

    inline protected def atVariance[T](v: Int)(op: => T): T = {
      val saved = variance
      variance = v
      val res = op
      variance = saved
      res
    }

    protected def stopAt: StopAt = StopAt.Static

    /** Can the prefix of this static reference be omitted if the reference
     *  itself can be omitted? Overridden in TypeOps#avoid.
     */
    protected def isStaticPrefix(pre: Type)(using Context): Boolean = true

    protected def stopBecauseStaticOrLocal(tp: NamedType)(using Context): Boolean =
      (tp.prefix eq NoPrefix)
      || {
        val stop = stopAt
        stop == StopAt.Static && tp.currentSymbol.isStatic && isStaticPrefix(tp.prefix)
        || stop == StopAt.Package && tp.currentSymbol.is(Package)
      }
  end VariantTraversal

  abstract class TypeMap(implicit protected var mapCtx: Context)
  extends VariantTraversal with (Type => Type) { thisMap =>

    def apply(tp: Type): Type

    protected def derivedSelect(tp: NamedType, pre: Type): Type =
      tp.derivedSelect(pre)
    protected def derivedRefinedType(tp: RefinedType, parent: Type, info: Type): Type =
      tp.derivedRefinedType(parent, tp.refinedName, info)
    protected def derivedRecType(tp: RecType, parent: Type): Type =
      tp.rebind(parent)
    protected def derivedAlias(tp: AliasingBounds, alias: Type): Type =
      tp.derivedAlias(alias)
    protected def derivedTypeBounds(tp: TypeBounds, lo: Type, hi: Type): Type =
      tp.derivedTypeBounds(lo, hi)
    protected def derivedSuperType(tp: SuperType, thistp: Type, supertp: Type): Type =
      tp.derivedSuperType(thistp, supertp)
    protected def derivedAppliedType(tp: AppliedType, tycon: Type, args: List[Type]): Type =
      tp.derivedAppliedType(tycon, args)
    protected def derivedAndType(tp: AndType, tp1: Type, tp2: Type): Type =
      tp.derivedAndType(tp1, tp2)
    protected def derivedOrType(tp: OrType, tp1: Type, tp2: Type): Type =
      tp.derivedOrType(tp1, tp2)
    protected def derivedMatchType(tp: MatchType, bound: Type, scrutinee: Type, cases: List[Type]): Type =
      tp.derivedMatchType(bound, scrutinee, cases)
    protected def derivedAnnotatedType(tp: AnnotatedType, underlying: Type, annot: Annotation): Type =
      tp.derivedAnnotatedType(underlying, annot)
    protected def derivedWildcardType(tp: WildcardType, bounds: Type): Type =
      tp.derivedWildcardType(bounds)
    protected def derivedSkolemType(tp: SkolemType, info: Type): Type =
      tp.derivedSkolemType(info)
    protected def derivedClassInfo(tp: ClassInfo, pre: Type): Type =
      tp.derivedClassInfo(pre)
    protected def derivedJavaArrayType(tp: JavaArrayType, elemtp: Type): Type =
      tp.derivedJavaArrayType(elemtp)
    protected def derivedExprType(tp: ExprType, restpe: Type): Type =
      tp.derivedExprType(restpe)
    // note: currying needed  because Scala2 does not support param-dependencies
    protected def derivedLambdaType(tp: LambdaType)(formals: List[tp.PInfo], restpe: Type): Type =
      tp.derivedLambdaType(tp.paramNames, formals, restpe)

    protected def mapArgs(args: List[Type], tparams: List[ParamInfo]): List[Type] = args match
      case arg :: otherArgs if tparams.nonEmpty =>
        val arg1 = arg match
          case arg: TypeBounds => this(arg)
          case arg => atVariance(variance * tparams.head.paramVarianceSign)(this(arg))
        val otherArgs1 = mapArgs(otherArgs, tparams.tail)
        if ((arg1 eq arg) && (otherArgs1 eq otherArgs)) args
        else arg1 :: otherArgs1
      case nil =>
        nil

    protected def mapOverLambda(tp: LambdaType) =
      val restpe = tp.resultType
      val saved = variance
      variance = if (defn.MatchCase.isInstance(restpe)) 0 else -variance
      val ptypes1 = tp.paramInfos.mapConserve(this).asInstanceOf[List[tp.PInfo]]
      variance = saved
      derivedLambdaType(tp)(ptypes1, this(restpe))

    def isRange(tp: Type): Boolean = tp.isInstanceOf[Range]

    /** Map this function over given type */
    def mapOver(tp: Type): Type = {
      record(s"TypeMap mapOver ${getClass}")
      record("TypeMap mapOver total")
      val ctx = this.mapCtx // optimization for performance
      given Context = ctx
      tp match {
        case tp: NamedType =>
          if stopBecauseStaticOrLocal(tp) then tp
          else
            val prefix1 = atVariance(variance max 0)(this(tp.prefix))
              // A prefix is never contravariant. Even if say `p.A` is used in a contravariant
              // context, we cannot assume contravariance for `p` because `p`'s lower
              // bound might not have a binding for `A` (e.g. the lower bound could be `Nothing`).
              // By contrast, covariance does translate to the prefix, since we have that
              // if `p <: q` then `p.A <: q.A`, and well-formedness requires that `A` is a member
              // of `p`'s upper bound.
            derivedSelect(tp, prefix1)

        case tp: AppliedType =>
          derivedAppliedType(tp, this(tp.tycon), mapArgs(tp.args, tp.tyconTypeParams))

        case tp: LambdaType =>
          mapOverLambda(tp)

        case tp: AliasingBounds =>
          derivedAlias(tp, atVariance(0)(this(tp.alias)))

        case tp: TypeBounds =>
          variance = -variance
          val lo1 = this(tp.lo)
          variance = -variance
          derivedTypeBounds(tp, lo1, this(tp.hi))

        case tp: TypeVar =>
          val inst = tp.instanceOpt
          if (inst.exists) apply(inst) else tp

        case tp: ExprType =>
          derivedExprType(tp, this(tp.resultType))

        case tp @ AnnotatedType(underlying, annot) =>
          val underlying1 = this(underlying)
          val annot1 = annot.mapWith(this)
          if annot1 eq EmptyAnnotation then underlying1
          else derivedAnnotatedType(tp, underlying1, annot1)

        case _: ThisType
          | _: BoundType
          | NoPrefix =>
          tp

        case tp: ProtoType =>
          tp.map(this)

        case tp: RefinedType =>
          derivedRefinedType(tp, this(tp.parent), this(tp.refinedInfo))

        case tp: RecType =>
          record("TypeMap.RecType")
          derivedRecType(tp, this(tp.parent))

        case tp @ SuperType(thistp, supertp) =>
          derivedSuperType(tp, this(thistp), this(supertp))

        case tp: LazyRef =>
          LazyRef { refCtx =>
            given Context = refCtx
            val ref1 = tp.ref
            if refCtx.runId == mapCtx.runId then this(ref1)
            else // splice in new run into map context
              val saved = mapCtx
              mapCtx = mapCtx.fresh
                .setPeriod(Period(refCtx.runId, mapCtx.phaseId))
                .setRun(refCtx.run)
              try this(ref1) finally mapCtx = saved
          }

        case tp: ClassInfo =>
          mapClassInfo(tp)

        case tp: AndType =>
          derivedAndType(tp, this(tp.tp1), this(tp.tp2))

        case tp: OrType =>
          derivedOrType(tp, this(tp.tp1), this(tp.tp2))

        case tp: MatchType =>
          val bound1 = this(tp.bound)
          val scrut1 = atVariance(0)(this(tp.scrutinee))
          derivedMatchType(tp, bound1, scrut1, tp.cases.mapConserve(this))

        case tp: SkolemType =>
          derivedSkolemType(tp, this(tp.info))

        case tp: WildcardType =>
          derivedWildcardType(tp, mapOver(tp.optBounds))

        case tp: JavaArrayType =>
          derivedJavaArrayType(tp, this(tp.elemType))

        case _ =>
          tp
      }
    }

    private def treeTypeMap = new TreeTypeMap(typeMap = this)

    def mapOver(syms: List[Symbol]): List[Symbol] = mapSymbols(syms, treeTypeMap)

    def mapOver(scope: Scope): Scope = {
      val elems = scope.toList
      val elems1 = mapOver(elems)
      if (elems1 eq elems) scope
      else newScopeWith(elems1: _*)
    }

    def mapOver(tree: Tree): Tree = treeTypeMap(tree)

    /** Can be overridden. By default, only the prefix is mapped. */
    protected def mapClassInfo(tp: ClassInfo): Type =
      derivedClassInfo(tp, this(tp.prefix))

    def andThen(f: Type => Type): TypeMap = new TypeMap {
      override def stopAt = thisMap.stopAt
      def apply(tp: Type) = f(thisMap(tp))
    }
  }

  /** A type map that maps also parents and self type of a ClassInfo */
  abstract class DeepTypeMap(using Context) extends TypeMap {
    override def mapClassInfo(tp: ClassInfo): ClassInfo = {
      val prefix1 = this(tp.prefix)
      val parents1 = tp.declaredParents mapConserve this
      val selfInfo1: TypeOrSymbol = tp.selfInfo match {
        case selfInfo: Type => this(selfInfo)
        case selfInfo => selfInfo
      }
      tp.derivedClassInfo(prefix1, parents1, tp.decls, selfInfo1)
    }
  }

  @sharable object IdentityTypeMap extends TypeMap()(NoContext) {
    def apply(tp: Type): Type = tp
  }

  /** A type map that approximates TypeBounds types depending on
   *  variance.
   *
   *  if variance > 0 : approximate by upper bound
   *     variance < 0 : approximate by lower bound
   *     variance = 0 : propagate bounds to next outer level
   */
  abstract class ApproximatingTypeMap(using Context) extends TypeMap { thisMap =>

    protected def range(lo: Type, hi: Type): Type =
      if (variance > 0) hi
      else if (variance < 0) lo
      else if (lo `eq` hi) lo
      else Range(lower(lo), upper(hi))

    protected def emptyRange = range(defn.NothingType, defn.AnyType)

    protected def lower(tp: Type): Type = tp match {
      case tp: Range => tp.lo
      case _ => tp
    }

    protected def upper(tp: Type): Type = tp match {
      case tp: Range => tp.hi
      case _ => tp
    }

    protected def rangeToBounds(tp: Type): Type = tp match {
      case Range(lo, hi) => TypeBounds(lo, hi)
      case _ => tp
    }

    private var expandingBounds: Boolean = false

    /** Whether it is currently expanding bounds
     *
     *  It is used to avoid following LazyRef in F-Bounds
     */
    def isExpandingBounds: Boolean = expandingBounds

    protected def expandBounds(tp: TypeBounds): Type =
      val saved = expandingBounds
      expandingBounds = true
      val res = range(atVariance(-variance)(reapply(tp.lo)), reapply(tp.hi))
      expandingBounds = saved
      res

    /** Try to widen a named type to its info relative to given prefix `pre`, where possible.
     *  The possible cases are listed inline in the code.
     */
    def tryWiden(tp: NamedType, pre: Type): Type = pre.member(tp.name) match {
      case d: SingleDenotation =>
        val tp1 = d.info.dealiasKeepAnnots
        tp1.stripAnnots match {
          case TypeAlias(alias) =>
            // if H#T = U, then for any x in L..H, x.T =:= U,
            // hence we can replace with U under all variances
            reapply(alias.rewrapAnnots(tp1))
          case tp: TypeBounds =>
            // If H#T = ? >: S <: U, then for any x in L..H, S <: x.T <: U,
            // hence we can replace with S..U under all variances
            expandBounds(tp)
          case info: SingletonType =>
            // if H#x: y.type, then for any x in L..H, x.type =:= y.type,
            // hence we can replace with y.type under all variances
            reapply(info)
          case _ =>
            NoType
        }
      case _ => NoType
    }

    /** Expand parameter reference corresponding to prefix `pre`;
     *  If the expansion is a wildcard parameter reference, convert its
     *  underlying bounds to a range, otherwise return the expansion.
     */
    def expandParam(tp: NamedType, pre: Type): Type =
      tp.argForParam(pre) match {
        case arg @ TypeRef(pre, _) if pre.isArgPrefixOf(arg.symbol) =>
          arg.info match {
            case argInfo: TypeBounds => expandBounds(argInfo)
            case argInfo => reapply(arg)
          }
        case arg: TypeBounds => expandBounds(arg)
        case arg => reapply(arg)
      }

    /** Derived selection.
     *  @pre   the (upper bound of) prefix `pre` has a member named `tp.name`.
     */
    override protected def derivedSelect(tp: NamedType, pre: Type): Type =
      if (pre eq tp.prefix) tp
      else pre match {
        case Range(preLo, preHi) =>
          val forwarded =
            if (tp.symbol.isAllOf(ClassTypeParam)) expandParam(tp, preHi)
            else tryWiden(tp, preHi)
          forwarded.orElse(
            range(super.derivedSelect(tp, preLo).loBound, super.derivedSelect(tp, preHi).hiBound))
        case _ =>
          super.derivedSelect(tp, pre) match {
            case TypeBounds(lo, hi) => range(lo, hi)
            case tp => tp
          }
      }

    override protected def derivedRefinedType(tp: RefinedType, parent: Type, info: Type): Type =
      if ((parent eq tp.parent) && (info eq tp.refinedInfo)) tp
      else parent match {
        case Range(parentLo, parentHi) =>
          range(derivedRefinedType(tp, parentLo, info), derivedRefinedType(tp, parentHi, info))
        case _ =>
          def propagate(lo: Type, hi: Type) =
            range(derivedRefinedType(tp, parent, lo), derivedRefinedType(tp, parent, hi))
          if (parent.isExactlyNothing) parent
          else info match {
            case Range(infoLo: TypeBounds, infoHi: TypeBounds) =>
              assert(variance == 0)
              if (!infoLo.isTypeAlias && !infoHi.isTypeAlias) propagate(infoLo, infoHi)
              else range(defn.NothingType, parent)
            case Range(infoLo, infoHi) =>
              propagate(infoLo, infoHi)
            case _ =>
              tp.derivedRefinedType(parent, tp.refinedName, info)
          }
      }

    override protected def derivedRecType(tp: RecType, parent: Type): Type =
      if (parent eq tp.parent) tp
      else parent match {
        case Range(lo, hi) => range(tp.rebind(lo), tp.rebind(hi))
        case _ => tp.rebind(parent)
      }

    override protected def derivedAlias(tp: AliasingBounds, alias: Type): Type =
      if (alias eq tp.alias) tp
      else alias match {
        case Range(lo, hi) =>
          if (variance > 0) TypeBounds(lo, hi)
          else range(tp.derivedAlias(lo), tp.derivedAlias(hi))
        case _ => tp.derivedAlias(alias)
      }

    override protected def derivedTypeBounds(tp: TypeBounds, lo: Type, hi: Type): Type =
      if ((lo eq tp.lo) && (hi eq tp.hi)) tp
      else if (isRange(lo) || isRange(hi))
        if (variance > 0) TypeBounds(lower(lo), upper(hi))
        else range(TypeBounds(upper(lo), lower(hi)), TypeBounds(lower(lo), upper(hi)))
      else tp.derivedTypeBounds(lo, hi)

    override protected def derivedSuperType(tp: SuperType, thistp: Type, supertp: Type): Type =
      if (isRange(thistp) || isRange(supertp)) emptyRange
      else tp.derivedSuperType(thistp, supertp)

    override protected def derivedAppliedType(tp: AppliedType, tycon: Type, args: List[Type]): Type =
      tycon match {
        case Range(tyconLo, tyconHi) =>
          range(derivedAppliedType(tp, tyconLo, args), derivedAppliedType(tp, tyconHi, args))
        case _ =>
          if args.exists(isRange) then
            if variance > 0 then
              tp.derivedAppliedType(tycon, args.map(rangeToBounds)) match
                case tp1: AppliedType if tp1.isUnreducibleWild =>
                  // don't infer a type that would trigger an error later in
                  // Checking.checkAppliedType; fall through to default handling instead
                case tp1 =>
                  return tp1
            end if
            val loBuf, hiBuf = new mutable.ListBuffer[Type]
            // Given `C[A1, ..., An]` where some A's are ranges, try to find
            // non-range arguments L1, ..., Ln and H1, ..., Hn such that
            // C[L1, ..., Ln] <: C[H1, ..., Hn] by taking the right limits of
            // ranges that appear in as co- or contravariant arguments.
            // Fail for non-variant argument ranges (see use-site else branch below).
            // If successful, the L-arguments are in loBut, the H-arguments in hiBuf.
            // @return  operation succeeded for all arguments.
            def distributeArgs(args: List[Type], tparams: List[ParamInfo]): Boolean = args match {
              case Range(lo, hi) :: args1 =>
                val v = tparams.head.paramVarianceSign
                if (v == 0) false
                else {
                  if (v > 0) { loBuf += lo; hiBuf += hi }
                  else { loBuf += hi; hiBuf += lo }
                  distributeArgs(args1, tparams.tail)
                }
              case arg :: args1 =>
                loBuf += arg; hiBuf += arg
                distributeArgs(args1, tparams.tail)
              case nil =>
                true
            }
            if (distributeArgs(args, tp.tyconTypeParams))
              range(tp.derivedAppliedType(tycon, loBuf.toList),
                    tp.derivedAppliedType(tycon, hiBuf.toList))
            else if tycon.isLambdaSub || args.exists(isRangeOfNonTermTypes) then
              range(defn.NothingType, defn.AnyType)
            else
              // See lampepfl/dotty#14152
              range(defn.NothingType, tp.derivedAppliedType(tycon, args.map(rangeToBounds)))
          else tp.derivedAppliedType(tycon, args)
      }

    private def isRangeOfNonTermTypes(tp: Type): Boolean = tp match
      case Range(lo, hi) => !lo.isInstanceOf[TermType] || !hi.isInstanceOf[TermType]
      case _             => false

    override protected def derivedAndType(tp: AndType, tp1: Type, tp2: Type): Type =
      if (isRange(tp1) || isRange(tp2)) range(lower(tp1) & lower(tp2), upper(tp1) & upper(tp2))
      else tp.derivedAndType(tp1, tp2)

    override protected def derivedOrType(tp: OrType, tp1: Type, tp2: Type): Type =
      if (isRange(tp1) || isRange(tp2)) range(lower(tp1) | lower(tp2), upper(tp1) | upper(tp2))
      else tp.derivedOrType(tp1, tp2)

    override protected def derivedAnnotatedType(tp: AnnotatedType, underlying: Type, annot: Annotation): Type =
      underlying match {
        case Range(lo, hi) =>
          range(tp.derivedAnnotatedType(lo, annot), tp.derivedAnnotatedType(hi, annot))
        case _ =>
          if (underlying.isExactlyNothing) underlying
          else tp.derivedAnnotatedType(underlying, annot)
      }
    override protected def derivedWildcardType(tp: WildcardType, bounds: Type): WildcardType =
      tp.derivedWildcardType(rangeToBounds(bounds))

    override protected def derivedMatchType(tp: MatchType, bound: Type, scrutinee: Type, cases: List[Type]): Type =
      bound match
        case Range(lo, hi) =>
          range(derivedMatchType(tp, lo, scrutinee, cases), derivedMatchType(tp, hi, scrutinee, cases))
        case _ =>
          scrutinee match
            case Range(lo, hi) => range(bound.bounds.lo, bound.bounds.hi)
            case _ => tp.derivedMatchType(bound, scrutinee, cases)

    override protected def derivedSkolemType(tp: SkolemType, info: Type): Type =
      if info eq tp.info then tp
      // By definition, a skolem is neither a subtype nor a supertype of a
      // different skolem. So, regardless of `variance`, we cannot return a
      // fresh skolem when approximating an existing skolem, we can only return
      // a range.
      else range(defn.NothingType, info)

    override protected def derivedClassInfo(tp: ClassInfo, pre: Type): Type = {
      assert(!isRange(pre))
        // we don't know what to do here; this case has to be handled in subclasses
        // (typically by handling ClassInfo's specially, in case they can be encountered).
      tp.derivedClassInfo(pre)
    }

    override protected def derivedLambdaType(tp: LambdaType)(formals: List[tp.PInfo], restpe: Type): Type =
      restpe match {
        case Range(lo, hi) =>
          range(derivedLambdaType(tp)(formals, lo), derivedLambdaType(tp)(formals, hi))
        case _ =>
          if formals.exists(isRange) then
            range(
              derivedLambdaType(tp)(formals.map(upper(_).asInstanceOf[tp.PInfo]), restpe),
              derivedLambdaType(tp)(formals.map(lower(_).asInstanceOf[tp.PInfo]), restpe))
          else
            tp.derivedLambdaType(tp.paramNames, formals, restpe)
      }

    protected def reapply(tp: Type): Type = apply(tp)
  }

  /** A range of possible types between lower bound `lo` and upper bound `hi`.
   *  Only used internally in `ApproximatingTypeMap`.
   */
  private case class Range(lo: Type, hi: Type) extends UncachedGroundType {
    assert(!lo.isInstanceOf[Range])
    assert(!hi.isInstanceOf[Range])

    override def toText(printer: Printer): Text =
      lo.toText(printer) ~ ".." ~ hi.toText(printer)
  }

  /** Approximate wildcards by their bounds */
  class AvoidWildcardsMap(using Context) extends ApproximatingTypeMap:
    protected def mapWild(t: WildcardType) =
      val bounds = t.effectiveBounds
      range(atVariance(-variance)(apply(bounds.lo)), apply(bounds.hi))
    def apply(t: Type): Type = t match
      case t: WildcardType => mapWild(t)
      case _ => mapOver(t)

  // ----- TypeAccumulators ----------------------------------------------------

  abstract class TypeAccumulator[T](implicit protected val accCtx: Context)
  extends VariantTraversal with ((T, Type) => T) {

    def apply(x: T, tp: Type): T

    protected def applyToAnnot(x: T, annot: Annotation): T = x // don't go into annotations

    protected final def applyToPrefix(x: T, tp: NamedType): T =
      atVariance(variance max 0)(this(x, tp.prefix)) // see remark on NamedType case in TypeMap

    def foldOver(x: T, tp: Type): T = {
      record(s"foldOver $getClass")
      record(s"foldOver total")
      tp match {
      case tp: TypeRef =>
        if stopBecauseStaticOrLocal(tp) then x
        else
          val tp1 = tp.prefix.lookupRefined(tp.name)
          if (tp1.exists) this(x, tp1) else applyToPrefix(x, tp)

      case tp @ AppliedType(tycon, args) =>
        @tailrec def foldArgs(x: T, tparams: List[ParamInfo], args: List[Type]): T =
          if (args.isEmpty || tparams.isEmpty) x
          else {
            val tparam = tparams.head
            val acc = args.head match {
              case arg: TypeBounds => this(x, arg)
              case arg => atVariance(variance * tparam.paramVarianceSign)(this(x, arg))
            }
            foldArgs(acc, tparams.tail, args.tail)
          }
        foldArgs(this(x, tycon), tp.tyconTypeParams, args)

      case _: BoundType | _: ThisType => x

      case tp: LambdaType =>
        val restpe = tp.resultType
        val saved = variance
        variance = if (defn.MatchCase.isInstance(restpe)) 0 else -variance
        val y = foldOver(x, tp.paramInfos)
        variance = saved
        this(y, restpe)

      case tp: TermRef =>
        if stopBecauseStaticOrLocal(tp) then x else applyToPrefix(x, tp)

      case tp: TypeVar =>
        this(x, tp.underlying)

      case ExprType(restpe) =>
        this(x, restpe)

      case bounds @ TypeBounds(lo, hi) =>
        if (lo eq hi) atVariance(0)(this(x, lo))
        else {
          variance = -variance
          val y = this(x, lo)
          variance = -variance
          this(y, hi)
        }

      case tp: AndType =>
        this(this(x, tp.tp1), tp.tp2)

      case tp: OrType =>
        this(this(x, tp.tp1), tp.tp2)

      case tp: MatchType =>
        val x1 = this(x, tp.bound)
        val x2 = atVariance(0)(this(x1, tp.scrutinee))
        foldOver(x2, tp.cases)

      case AnnotatedType(underlying, annot) =>
        this(applyToAnnot(x, annot), underlying)

      case tp: ProtoType =>
        tp.fold(x, this)

      case tp: RefinedType =>
        this(this(x, tp.parent), tp.refinedInfo)

      case tp: WildcardType =>
        this(x, tp.optBounds)

      case tp @ ClassInfo(prefix, _, _, _, _) =>
        this(x, prefix)

      case tp: JavaArrayType =>
        this(x, tp.elemType)

      case tp: SkolemType =>
        this(x, tp.info)

      case SuperType(thistp, supertp) =>
        this(this(x, thistp), supertp)

      case tp: LazyRef =>
        this(x, tp.ref)

      case tp: RecType =>
        this(x, tp.parent)

      case _ => x
    }}

    @tailrec final def foldOver(x: T, ts: List[Type]): T = ts match {
      case t :: ts1 => foldOver(apply(x, t), ts1)
      case nil => x
    }
  }

  abstract class TypeTraverser(using Context) extends TypeAccumulator[Unit] {
    def traverse(tp: Type): Unit
    def apply(x: Unit, tp: Type): Unit = traverse(tp)
    protected def traverseChildren(tp: Type): Unit = foldOver((), tp)
  }

  class ExistsAccumulator(
      p: Type => Boolean,
      override val stopAt: StopAt,
      forceLazy: Boolean)(using Context) extends TypeAccumulator[Boolean]:
    def apply(x: Boolean, tp: Type): Boolean =
      x || p(tp) || (forceLazy || !tp.isInstanceOf[LazyRef]) && foldOver(x, tp)

  class ForeachAccumulator(p: Type => Unit, override val stopAt: StopAt)(using Context) extends TypeAccumulator[Unit] {
    def apply(x: Unit, tp: Type): Unit = foldOver(p(tp), tp)
  }

  class NamedPartsAccumulator(p: NamedType => Boolean)(using Context)
  extends TypeAccumulator[List[NamedType]]:
    def maybeAdd(xs: List[NamedType], tp: NamedType): List[NamedType] = if p(tp) then tp :: xs else xs
    val seen = util.HashSet[Type]()
    def apply(xs: List[NamedType], tp: Type): List[NamedType] =
      if seen contains tp then xs
      else
        seen += tp
        tp match
          case tp: TypeRef =>
            foldOver(maybeAdd(xs, tp), tp)
          case tp: ThisType =>
            apply(xs, tp.tref)
          case NoPrefix =>
            foldOver(xs, tp)
          case tp: TermRef =>
            apply(foldOver(maybeAdd(xs, tp), tp), tp.underlying)
          case tp: AppliedType =>
            foldOver(xs, tp)
          case TypeBounds(lo, hi) =>
            apply(apply(xs, lo), hi)
          case tp: ParamRef =>
            apply(xs, tp.underlying)
          case tp: ConstantType =>
            apply(xs, tp.underlying)
          case _ =>
            foldOver(xs, tp)
  end NamedPartsAccumulator

  class isGroundAccumulator(using Context) extends TypeAccumulator[Boolean] {
    def apply(x: Boolean, tp: Type): Boolean = x && {
      tp match {
        case _: TypeParamRef => false
        case tp: TypeVar => apply(x, tp.underlying)
        case tp: AppliedType => tp.isGround(this)
        case _ => foldOver(x, tp)
      }
    }
  }

  class TypeSizeAccumulator(using Context) extends TypeAccumulator[Int] {
    var seen = util.HashSet[Type](initialCapacity = 8)
    def apply(n: Int, tp1: Type): Int =
      val tp0 = tp1.dealias
      if seen.contains(tp0) then n
      else {
        seen += tp0
        tp0 match {
          case tp: AppliedType =>
            foldOver(n + 1, tp)
          case tp: RefinedType =>
            foldOver(n + 1, tp)
          case tp: TypeRef if tp.info.isTypeAlias =>
            apply(n, tp.superType)
          case tp: TypeParamRef =>
            apply(n, TypeComparer.bounds(tp))
          case _ =>
            foldOver(n, tp0)
        }
      }
  }

  class CoveringSetAccumulator(using Context) extends TypeAccumulator[Set[Symbol]] {
    var seen = util.HashSet[Type](initialCapacity = 8)
    def apply(cs: Set[Symbol], tp1: Type): Set[Symbol] =
      val tp0 = tp1.dealias
      if seen.contains(tp0) then cs
      else {
        seen += tp0
        tp0 match {
          case tp if tp.isExactlyAny || tp.isExactlyNothing =>
            cs
          case tp: AppliedType if !tp.typeSymbol.isAliasType =>
            foldOver(cs + tp.typeSymbol, tp)
          case tp: RefinedType if !tp.typeSymbol.isAliasType =>
            foldOver(cs + tp.typeSymbol, tp)
          case tp: TypeRef if tp.info.isTypeAlias =>
            apply(cs, tp.superType)
          case tp: TypeRef if tp.symbol.isClass =>
            foldOver(cs + tp.typeSymbol, tp)
          case tp: TermRef =>
            val tsym = if (tp.termSymbol.is(Param)) tp.underlying.typeSymbol else tp.termSymbol
            foldOver(cs + tsym, tp)
          case tp: TypeParamRef =>
            apply(cs, TypeComparer.bounds(tp))
          case other =>
            foldOver(cs, tp0)
        }
      }
  }

  //   ----- Name Filters --------------------------------------------------

  /** A name filter selects or discards a member name of a type `pre`.
   *  To enable efficient caching, name filters have to satisfy the
   *  following invariant: If `keep` is a name filter, and `pre` has
   *  class `C` as a base class, then
   *
   *    keep(pre, name)  implies  keep(C.this, name)
   */
  abstract class NameFilter {
    def apply(pre: Type, name: Name)(using Context): Boolean

    /** Filter does not need to be rechecked with full prefix, if it
     *  has been already checked for the class denotation of the prefix
     */
    def isStable: Boolean
  }

  /** A filter for names of abstract types of a given type */
  object abstractTypeNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(using Context): Boolean =
      name.isTypeName && {
        val mbr = pre.nonPrivateMember(name)
        mbr.symbol.is(Deferred) && mbr.info.isInstanceOf[RealTypeBounds]
      }
    def isStable = false
  }

  /** A filter for names of abstract types of a given type */
  object nonClassTypeNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(using Context): Boolean =
      name.isTypeName && {
        val mbr = pre.member(name)
        mbr.symbol.isType && !mbr.symbol.isClass
      }
    def isStable = false
  }

  /** A filter for names of deferred term definitions of a given type */
  object abstractTermNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(using Context): Boolean =
      name.isTermName && pre.nonPrivateMember(name).hasAltWith(_.symbol.is(Deferred))
    def isStable = false
  }

  /** A filter for names of type aliases of a given type */
  object typeAliasNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(using Context): Boolean =
      name.isTypeName && {
        val mbr = pre.nonPrivateMember(name)
        mbr.symbol.isAliasType
      }
    def isStable = false
  }

  object typeNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(using Context): Boolean = name.isTypeName
    def isStable = true
  }

  object fieldFilter extends NameFilter {
    def apply(pre: Type, name: Name)(using Context): Boolean =
      name.isTermName && (pre member name).hasAltWith(!_.symbol.is(Method))
    def isStable = true
  }

  object takeAllFilter extends NameFilter {
    def apply(pre: Type, name: Name)(using Context): Boolean = name != nme.CONSTRUCTOR
    def isStable = true
  }

  object implicitFilter extends NameFilter {
    /** A dummy filter method.
     *  Implicit filtering is handled specially in computeMemberNames, so
     *  no post-filtering is needed.
     */
    def apply(pre: Type, name: Name)(using Context): Boolean = true
    def isStable = true
  }

  // ----- Debug ---------------------------------------------------------

  @sharable var debugTrace: Boolean = false

  val watchList: List[TypeName] = List[String](
  ) map (_.toTypeName)

  def isWatched(tp: Type)(using Context): Boolean = tp match {
    case ref: TypeRef => watchList contains ref.name
    case _ => false
  }

  // ----- Helpers and Decorator implicits --------------------------------------

  implicit def decorateTypeApplications(tpe: Type): TypeApplications = new TypeApplications(tpe)

  extension (tps1: List[Type]) {
    @tailrec def hashIsStable: Boolean =
      tps1.isEmpty || tps1.head.hashIsStable && tps1.tail.hashIsStable
    @tailrec def equalElements(tps2: List[Type], bs: BinderPairs): Boolean =
      (tps1 `eq` tps2) || {
        if (tps1.isEmpty) tps2.isEmpty
        else tps2.nonEmpty && tps1.head.equals(tps2.head, bs) && tps1.tail.equalElements(tps2.tail, bs)
      }
  }

  private val keepAlways: AnnotatedType => Context ?=> Boolean = _ => true
  private val keepNever: AnnotatedType => Context ?=> Boolean = _ => false
  private val keepIfRefining: AnnotatedType => Context ?=> Boolean = _.isRefining

  val isBounds: Type => Boolean = _.isInstanceOf[TypeBounds]
}
