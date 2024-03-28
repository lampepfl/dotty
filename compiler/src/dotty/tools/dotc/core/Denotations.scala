package dotty.tools
package dotc
package core

import SymDenotations.{ SymDenotation, ClassDenotation, NoDenotation, LazyType, stillValid, acceptStale, traceInvalid }
import Contexts.*
import Names.*
import NameKinds.*
import StdNames.*
import Symbols.NoSymbol
import Symbols.*
import Types.*
import Periods.*
import Flags.*
import DenotTransformers.*
import Decorators.*
import Signature.MatchDegree.*
import printing.Texts.*
import printing.Printer
import io.AbstractFile
import config.Config
import config.Printers.overload
import util.common.*
import typer.ProtoTypes.NoViewsAllowed
import reporting.Message
import collection.mutable.ListBuffer

import scala.compiletime.uninitialized

/** Denotations represent the meaning of symbols and named types.
 *  The following diagram shows how the principal types of denotations
 *  and their denoting entities relate to each other. Lines ending in
 *  a down-arrow `v` are member methods. The two methods shown in the diagram are
 *  "symbol" and "deref". Both methods are parameterized by the current context,
 *  and are effectively indexed by current period.
 *
 *  Lines ending in a horizontal line mean subtyping (right is a subtype of left).
 *
 *  NamedType
 *    |                                          Symbol---------ClassSymbol
 *    |                                            |                |
 *    | denot                                      | denot          | denot
 *    v                                            v                v
 *  Denotation-+-----SingleDenotation-+------SymDenotation-+----ClassDenotation
 *             |                      |
 *             +-----MultiDenotation  |
 *                                    |
 *                                    +--UniqueRefDenotation
 *                                    +--JointRefDenotation
 *
 *  Here's a short summary of the classes in this diagram.
 *
 *  NamedType                A type consisting of a prefix type and a name, with fields
 *                              prefix: Type
 *                              name: Name
 *                           It has two subtypes: TermRef and TypeRef
 *  Symbol                   A label for a definition or declaration in one compiler run
 *  ClassSymbol              A symbol representing a class
 *  Denotation               The meaning of a named type or symbol during a period
 *  MultiDenotation          A denotation representing several overloaded members
 *  SingleDenotation         A denotation representing a non-overloaded member or definition, with main fields
 *                              symbol: Symbol
 *                              info: Type
 *  UniqueRefDenotation      A denotation referring to a single definition with some member type
 *  JointRefDenotation       A denotation referring to a member that could resolve to several definitions
 *  SymDenotation            A denotation representing a single definition with its original type, with main fields
 *                              name: Name
 *                              owner: Symbol
 *                              flags: Flags
 *                              privateWithin: Symbol
 *                              annotations: List[Annotation]
 *  ClassDenotation          A denotation representing a single class definition.
 */
object Denotations {

  implicit def eqDenotation: CanEqual[Denotation, Denotation] = CanEqual.derived

  /** A PreDenotation represents a group of single denotations or a single multi-denotation
   *  It is used as an optimization to avoid forming MultiDenotations too eagerly.
   */
  abstract class PreDenotation {

    /** A denotation in the group exists */
    def exists: Boolean

    /** First/last denotation in the group */
    def first: Denotation
    def last: Denotation

    /** Convert to full denotation by &-ing all elements */
    def toDenot(pre: Type)(using Context): Denotation

    /** Group contains a denotation that refers to given symbol */
    def containsSym(sym: Symbol): Boolean

    /** Group contains a denotation with the same signature as `other` */
    def matches(other: SingleDenotation)(using Context): Boolean

    /** Keep only those denotations in this group which satisfy predicate `p`. */
    def filterWithPredicate(p: SingleDenotation => Boolean): PreDenotation

    /** Keep only those denotations in this group which have a signature
     *  that's not already defined by `denots`.
     */
    def filterDisjoint(denots: PreDenotation)(using Context): PreDenotation

    /** Keep only those inherited members M of this predenotation for which the following is true
     *   - M is not marked Private
     *   - If M has a unique symbol, it does not appear in `prevDenots`.
     *   - M's signature as seen from prefix `pre` does not appear in `ownDenots`
     *  Return the denotation as seen from `pre`.
     *  Called from SymDenotations.computeMember. There, `ownDenots` are the denotations found in
     *  the base class, which shadow any inherited denotations with the same signature.
     *  `prevDenots` are the denotations that are defined in the class or inherited from
     *  a base type which comes earlier in the linearization.
     */
    def mapInherited(ownDenots: PreDenotation, prevDenots: PreDenotation, pre: Type)(using Context): PreDenotation

    /** Keep only those denotations in this group that have all of the flags in `required`,
     *  but none of the flags in `excluded`.
     */
    def filterWithFlags(required: FlagSet, excluded: FlagSet)(using Context): PreDenotation

    /** Map `f` over all single denotations and aggregate the results with `g`. */
    def aggregate[T](f: SingleDenotation => T, g: (T, T) => T): T

    private var cachedPrefix: Type = uninitialized
    private var cachedAsSeenFrom: AsSeenFromResult = uninitialized
    private var validAsSeenFrom: Period = Nowhere

    type AsSeenFromResult <: PreDenotation

    /** The denotation with info(s) as seen from prefix type */
    def asSeenFrom(pre: Type)(using Context): AsSeenFromResult =
      if (Config.cacheAsSeenFrom) {
        if ((cachedPrefix ne pre) || ctx.period != validAsSeenFrom) {
          cachedAsSeenFrom = computeAsSeenFrom(pre)
          cachedPrefix = pre
          validAsSeenFrom = if (pre.isProvisional) Nowhere else ctx.period
        }
        cachedAsSeenFrom
      }
      else computeAsSeenFrom(pre)

    protected def computeAsSeenFrom(pre: Type)(using Context): AsSeenFromResult

    /** The union of two groups. */
    def union(that: PreDenotation): PreDenotation =
      if (!this.exists) that
      else if (!that.exists) this
      else DenotUnion(this, that)
  }

  /** A denotation is the result of resolving
   *  a name (either simple identifier or select) during a given period.
   *
   *  Denotations can be combined with `&` and `|`.
   *  & is conjunction, | is disjunction.
   *
   *  `&` will create an overloaded denotation from two
   *  non-overloaded denotations if their signatures differ.
   *  Analogously `|` of two denotations with different signatures will give
   *  an empty denotation `NoDenotation`.
   *
   *  A denotation might refer to `NoSymbol`. This is the case if the denotation
   *  was produced from a disjunction of two denotations with different symbols
   *  and there was no common symbol in a superclass that could substitute for
   *  both symbols. Here is an example:
   *
   *  Say, we have:
   *
   *    class A { def f: A }
   *    class B { def f: B }
   *    val x: A | B = if (test) new A else new B
   *    val y = x.f
   *
   *  Then the denotation of `y` is `SingleDenotation(NoSymbol, A | B)`.
   *
   *  @param symbol  The referencing symbol, or NoSymbol is none exists
   */
  abstract class Denotation(val symbol: Symbol, protected var myInfo: Type, val isType: Boolean) extends PreDenotation with printing.Showable {
    type AsSeenFromResult <: Denotation

    /** The type info.
     *  The info is an instance of TypeType iff this is a type denotation
     *  Uncompleted denotations set myInfo to a LazyType.
     */
    final def info(using Context): Type = {
      def completeInfo = { // Written this way so that `info` is small enough to be inlined
        this.asInstanceOf[SymDenotation].completeFrom(myInfo.asInstanceOf[LazyType]); info
      }
      if (myInfo.isInstanceOf[LazyType]) completeInfo else myInfo
    }

    /** The type info, or, if this is a SymDenotation where the symbol
     *  is not yet completed, the completer
     */
    def infoOrCompleter: Type

    /** Is this a reference to a term symbol? */
    def isTerm: Boolean = !isType

    /** Is this denotation overloaded? */
    final def isOverloaded: Boolean = isInstanceOf[MultiDenotation]

    /** Denotation points to unique symbol; false for overloaded denotations
     * and JointRef denotations.
     */
    def hasUniqueSym: Boolean

    /** The name of the denotation */
    def name(using Context): Name

    /** The signature of the denotation. */
    def signature(using Context): Signature

    /** Resolve overloaded denotation to pick the ones with the given signature
     *  when seen from prefix `site`.
     *  @param relaxed  When true, consider only parameter signatures for a match.
     */
    def atSignature(sig: Signature, targetName: Name, site: Type = NoPrefix, relaxed: Boolean = false)(using Context): Denotation

    /** The variant of this denotation that's current in the given context.
     *  If no such denotation exists, returns the denotation with each alternative
     *  at its first point of definition.
     */
    def current(using Context): Denotation

    /** The period during which this denotation is valid. */
    private var myValidFor: Period = Nowhere

    final def validFor: Period = myValidFor
    final def validFor_=(p: Period): Unit = {
      myValidFor = p
      symbol.invalidateDenotCache()
    }

    /** Is this denotation different from NoDenotation or an ErrorDenotation? */
    def exists: Boolean = true

    /** A denotation with the info of this denotation transformed using `f` */
    def mapInfo(f: Type => Type)(using Context): Denotation

    /** If this denotation does not exist, fallback to alternative */
    inline def orElse(inline that: Denotation): Denotation = if (this.exists) this else that

    /** The set of alternative single-denotations making up this denotation */
    final def alternatives: List[SingleDenotation] = altsWith(alwaysTrue)

    /** The alternatives of this denotation that satisfy the predicate `p`. */
    def altsWith(p: Symbol => Boolean): List[SingleDenotation]

    /** The unique alternative of this denotation that satisfies the predicate `p`,
     *  or NoDenotation if no satisfying alternative exists.
     *  @throws TypeError if there is at more than one alternative that satisfies `p`.
     */
    def suchThat(p: Symbol => Boolean)(using Context): SingleDenotation

    override def filterWithPredicate(p: SingleDenotation => Boolean): Denotation

    /** If this is a SingleDenotation, return it, otherwise throw a TypeError */
    def checkUnique(using Context): SingleDenotation = suchThat(alwaysTrue)

    /** Does this denotation have an alternative that satisfies the predicate `p`? */
    def hasAltWith(p: SingleDenotation => Boolean): Boolean

    inline final def hasAltWithInline(inline p: SingleDenotation => Boolean): Boolean = inline this match
      case mbr: SingleDenotation => mbr.exists && p(mbr)
      case mbr => mbr.hasAltWith(p)

    /** The denotation made up from the alternatives of this denotation that
     *  are accessible from prefix `pre`, or NoDenotation if no accessible alternative exists.
     */
    def accessibleFrom(pre: Type, superAccess: Boolean = false)(using Context): Denotation

    /** Find member of this denotation with given `name`, all `required`
     *  flags and no `excluded` flag, and produce a denotation that contains the type of the member
     *  as seen from given prefix `pre`.
     */
    def findMember(name: Name, pre: Type, required: FlagSet, excluded: FlagSet)(using Context): Denotation =
      info.findMember(name, pre, required, excluded)

    /** If this denotation is overloaded, filter with given predicate.
     *  If result is still overloaded throw a TypeError.
     *  Note: disambiguate is slightly different from suchThat in that
     *  single-denotations that do not satisfy the predicate are left alone
     *  (whereas suchThat would map them to NoDenotation).
     */
    inline def disambiguate(inline p: Symbol => Boolean)(using Context): SingleDenotation = this match {
      case sdenot: SingleDenotation => sdenot
      case mdenot => suchThat(p) orElse NoQualifyingRef(alternatives)
    }

    /** Return symbol in this denotation that satisfies the given predicate.
     *  if generateStubs is specified, return a stubsymbol if denotation is a missing ref.
     *  Throw a `TypeError` if predicate fails to disambiguate symbol or no alternative matches.
     */
    def requiredSymbol(kind: String,
                       name: Name,
                       site: Denotation = NoDenotation,
                       args: List[Type] = Nil,
                       generateStubs: Boolean = true)
                      (p: Symbol => Boolean)
                      (using Context): Symbol =
      disambiguate(p) match {
        case m @ MissingRef(ownerd, name) if generateStubs =>
          if ctx.settings.YdebugMissingRefs.value then m.ex.printStackTrace()
          newStubSymbol(ownerd.symbol, name)
        case NoDenotation | _: NoQualifyingRef | _: MissingRef =>
          def argStr = if (args.isEmpty) "" else i" matching ($args%, %)"
          val msg =
            if site.exists then em"$site does not have a member $kind $name$argStr"
            else em"missing: $kind $name$argStr"
          throw TypeError(msg)
        case denot =>
          denot.symbol
      }

    def requiredMethod(pname: PreName)(using Context): TermSymbol = {
      val name = pname.toTermName
      info.member(name).requiredSymbol("method", name, this)(_.is(Method)).asTerm
    }
    def requiredMethodRef(name: PreName)(using Context): TermRef =
      requiredMethod(name).termRef

    def requiredMethod(pname: PreName, argTypes: List[Type])(using Context): TermSymbol = {
      val name = pname.toTermName
      info.member(name).requiredSymbol("method", name, this, argTypes) { x =>
        x.is(Method) && {
          x.info.paramInfoss match {
            case paramInfos :: Nil => paramInfos.corresponds(argTypes)(_ =:= _)
            case _ => false
          }
        }
      }.asTerm
    }
    def requiredMethodRef(name: PreName, argTypes: List[Type])(using Context): TermRef =
      requiredMethod(name, argTypes).termRef

    def requiredValue(pname: PreName)(using Context): TermSymbol = {
      val name = pname.toTermName
      info.member(name).requiredSymbol("field or getter", name, this)(_.info.isParameterless).asTerm
    }
    def requiredValueRef(name: PreName)(using Context): TermRef =
      requiredValue(name).termRef

    def requiredClass(pname: PreName)(using Context): ClassSymbol = {
      val name = pname.toTypeName
      info.member(name).requiredSymbol("class", name, this)(_.isClass).asClass
    }

    def requiredType(pname: PreName)(using Context): TypeSymbol = {
      val name = pname.toTypeName
      info.member(name).requiredSymbol("type", name, this)(_.isType).asType
    }

    /** The alternative of this denotation that has a type matching `targetType` when seen
     *  as a member of type `site` and that has a target name matching `targetName`, or
     *  `NoDenotation` if none exists.
     */
    def matchingDenotation(site: Type, targetType: Type, targetName: Name)(using Context): SingleDenotation = {
      def qualifies(sym: Symbol) =
        site.memberInfo(sym).matchesLoosely(targetType) && sym.hasTargetName(targetName)
      if (isOverloaded)
        atSignature(targetType.signature, targetName, site, relaxed = true) match {
          case sd: SingleDenotation => sd.matchingDenotation(site, targetType, targetName)
          case md => md.suchThat(qualifies(_))
        }
      else if (exists && !qualifies(symbol)) NoDenotation
      else asSingleDenotation
    }

    /** Form a denotation by conjoining with denotation `that`.
     *
     *  NoDenotations are dropped. MultiDenotations are handled by merging
     *  parts with same signatures. SingleDenotations with equal signatures
     *  are joined by following this sequence of steps:
     *
     *  1. If exactly one the denotations has an inaccessible symbol, pick the other one.
     *  2. Otherwise, if one of the infos overrides the other one, and the associated
     *     symbol does not score strictly lower than the other one,
     *     pick the associated denotation.
     *  3. Otherwise, if the two infos can be combined with `infoMeet`, pick that as
     *     result info, and pick the symbol that scores higher as result symbol,
     *     or pick `sym1` as a tie breaker. The picked info and symbol are combined
     *     in a JointDenotation.
     *  4. Otherwise, if one of the two symbols scores strongly higher than the
     *     other one, pick the associated denotation.
     *  5. Otherwise return a multi-denotation consisting of both denotations.
     *
     *  Symbol scoring is determined according to the following ranking
     *  where earlier criteria trump later ones. Cases marked with (*)
     *  give a strong score advantage, the others a weak one.
     *
     *  1. The symbol exists, and the other one does not. (*)
     *  2. The symbol is not a bridge, but the other one is. (*)
     *  3. The symbol is concrete, and the other one is deferred
     *  4. The symbol appears before the other in the linearization of `pre`
     *  5. The symbol's visibility is strictly greater than the other one's.
     *  6. The symbol is a method, but the other one is not.
     */
    def meet(that: Denotation, pre: Type, safeIntersection: Boolean = false)(using Context): Denotation = {
      /** Try to merge denot1 and denot2 without adding a new signature. */
      def mergeDenot(denot1: Denotation, denot2: SingleDenotation): Denotation = denot1 match {
        case denot1 @ MultiDenotation(denot11, denot12) =>
          val d1 = mergeDenot(denot11, denot2)
          if (d1.exists) denot1.derivedUnionDenotation(d1, denot12)
          else {
            val d2 = mergeDenot(denot12, denot2)
            if (d2.exists) denot1.derivedUnionDenotation(denot11, d2)
            else NoDenotation
          }
        case denot1: SingleDenotation =>
          if (denot1 eq denot2) denot1
          else if denot1.matches(denot2) then mergeSingleDenot(denot1, denot2)
          else NoDenotation
      }

      /** Try to merge single-denotations. */
      def mergeSingleDenot(denot1: SingleDenotation, denot2: SingleDenotation): Denotation =
        val info1 = denot1.info
        val info2 = denot2.info
        val sym1 = denot1.symbol
        val sym2 = denot2.symbol

        /** Does `owner1` come before `owner2` in the linearization of `pre`? */
        def linearScore(owner1: Symbol, owner2: Symbol): Int =

          def searchBaseClasses(bcs: List[ClassSymbol]): Int = bcs match
            case bc :: bcs1 =>
              if bc eq owner1 then 1
              else if bc eq owner2 then -1
              else searchBaseClasses(bcs1)
            case Nil => 0

          if owner1 eq owner2 then 0
          else if owner1.derivesFrom(owner2) then 1
          else if owner2.derivesFrom(owner1) then -1
          else searchBaseClasses(pre.baseClasses)
        end linearScore

        /** Similar to SymDenotation#accessBoundary, but without the special cases. */
        def accessBoundary(sym: Symbol) =
          if (sym.is(Private)) sym.owner
          else sym.privateWithin.orElse(
            if (sym.is(Protected)) sym.owner.enclosingPackageClass
            else defn.RootClass)

        def isHidden(sym: Symbol) = sym.exists && !sym.isAccessibleFrom(pre)
        // In typer phase filter out denotations with symbols that are not
        // accessible. After typer, this is not possible since we cannot guarantee
        // that the current owner is set correctly. See pos/14660.scala.
        val hidden1 = isHidden(sym1) && ctx.isTyper
        val hidden2 = isHidden(sym2) && ctx.isTyper
        if hidden1 && !hidden2 then denot2
        else if hidden2 && !hidden1 then denot1
        else
          // The score that determines which symbol to pick for the result denotation.
          // A value > 0 means pick `sym1`, < 0 means pick `sym2`.
          // A value of +/- 2 means pick one of the denotations as a tie-breaker
          // if a common info does not exist.
          val symScore: Int =
            if !sym1.exists then -2
            else if !sym2.exists then 2
            else if sym1.is(Bridge) && !sym2.is(Bridge) then -2
            else if sym2.is(Bridge) && !sym1.is(Bridge) then 2
            else if !sym1.isAsConcrete(sym2) then -1
            else if !sym2.isAsConcrete(sym1) then 1
            else
              val linScore = linearScore(sym1.owner, sym2.owner)
              if linScore != 0 then linScore
              else
                val boundary1 = accessBoundary(sym1)
                val boundary2 = accessBoundary(sym2)
                if boundary1.isProperlyContainedIn(boundary2) then -1
                else if boundary2.isProperlyContainedIn(boundary1) then 1
                else if sym2.is(Method) && !sym1.is(Method) then -1
                else if sym1.is(Method) && !sym2.is(Method) then 1
                else 0

          val relaxedOverriding = ctx.explicitNulls && (sym1.is(JavaDefined) || sym2.is(JavaDefined))
          val matchLoosely = sym1.matchNullaryLoosely || sym2.matchNullaryLoosely

          if symScore <= 0 && info2.overrides(info1, relaxedOverriding, matchLoosely, checkClassInfo = false) then
            denot2
          else if symScore >= 0 && info1.overrides(info2, relaxedOverriding, matchLoosely, checkClassInfo = false) then
            denot1
          else
            val jointInfo = infoMeet(info1, info2, safeIntersection)
            if jointInfo.exists then
              val sym = if symScore >= 0 then sym1 else sym2
              JointRefDenotation(sym, jointInfo, denot1.validFor & denot2.validFor, pre, denot1.isRefinedMethod || denot2.isRefinedMethod)
            else if symScore == 2 then denot1
            else if symScore == -2 then denot2
            else
              overload.println(i"overloaded with same signature: ${sym1.showLocated}: $info1 / ${sym2.showLocated}: $info2, info = ${info1.getClass}, ${info2.getClass}, $jointInfo")
              MultiDenotation(denot1, denot2)
      end mergeSingleDenot

      if (this eq that) this
      else if (!this.exists) that
      else if (!that.exists) this
      else that match {
        case that: SingleDenotation =>
          val r = mergeDenot(this, that)
          if (r.exists) r else MultiDenotation(this, that)
        case that @ MultiDenotation(denot1, denot2) =>
          this.meet(denot1, pre).meet(denot2, pre)
      }
    }

    final def asSingleDenotation: SingleDenotation = asInstanceOf[SingleDenotation]
    final def asSymDenotation: SymDenotation = asInstanceOf[SymDenotation]

    def toText(printer: Printer): Text = printer.toText(this)

    // ------ PreDenotation ops ----------------------------------------------

    final def toDenot(pre: Type)(using Context): Denotation = this
    final def containsSym(sym: Symbol): Boolean = hasUniqueSym && (symbol eq sym)
  }

  // ------ Info meets ----------------------------------------------------

  /** Merge parameter names of lambda types. If names in corresponding positions match, keep them,
    *  otherwise generate new synthetic names.
    */
  private def mergeParamNames(tp1: LambdaType, tp2: LambdaType): List[tp1.ThisName] =
    (for ((name1, name2, idx) <- tp1.paramNames.lazyZip(tp2.paramNames).lazyZip(tp1.paramNames.indices))
      yield if (name1 == name2) name1 else tp1.companion.syntheticParamName(idx)).toList

  /** Normally, `tp1 & tp2`, with extra care taken to return `tp1` or `tp2` directly if that's
   *  a valid answer. Special cases for matching methods and classes, with
   *  the possibility of returning NoType. Special handling of ExprTypes, where mixed
   *  intersections widen the ExprType away.
   */
  def infoMeet(tp1: Type, tp2: Type, safeIntersection: Boolean)(using Context): Type =
    if tp1 eq tp2 then tp1
    else tp1 match
      case tp1: TypeBounds =>
        tp2 match
          case tp2: TypeBounds => if safeIntersection then tp1 safe_& tp2 else tp1 & tp2
          case tp2: ClassInfo => tp2
          case _ => NoType
      case tp1: ClassInfo =>
        tp2 match
          case tp2: ClassInfo if tp1.cls eq tp2.cls => tp1.derivedClassInfo(tp1.prefix & tp2.prefix)
          case tp2: TypeBounds => tp1
          case _ => NoType
      case tp1: MethodType =>
        tp2 match
          case tp2: MethodType
          if TypeComparer.matchingMethodParams(tp1, tp2)
             && tp1.isImplicitMethod == tp2.isImplicitMethod =>
            val resType = infoMeet(tp1.resType, tp2.resType.subst(tp2, tp1), safeIntersection)
            if resType.exists then
              tp1.derivedLambdaType(mergeParamNames(tp1, tp2), tp1.paramInfos, resType)
            else NoType
          case _ => NoType
      case tp1: PolyType =>
        tp2 match
          case tp2: PolyType if tp1.paramNames.hasSameLengthAs(tp2.paramNames) =>
            val resType = infoMeet(tp1.resType, tp2.resType.subst(tp2, tp1), safeIntersection)
            if resType.exists then
              tp1.derivedLambdaType(
                mergeParamNames(tp1, tp2),
                tp1.paramInfos.zipWithConserve(tp2.paramInfos)( _ & _ ),
                resType)
            else NoType
          case _ => NoType
      case ExprType(rtp1) =>
        tp2 match
          case ExprType(rtp2) => ExprType(rtp1 & rtp2)
          case _ => infoMeet(rtp1, tp2, safeIntersection)
      case _ =>
        tp2 match
          case _: MethodType | _: PolyType => NoType
          case _ => tp1 & tp2.widenExpr
  end infoMeet

  /** A non-overloaded denotation */
  abstract class SingleDenotation(symbol: Symbol, initInfo: Type, isType: Boolean) extends Denotation(symbol, initInfo, isType) {
    protected def newLikeThis(symbol: Symbol, info: Type, pre: Type, isRefinedMethod: Boolean)(using Context): SingleDenotation

    final def name(using Context): Name = symbol.name

    /** For SymDenotation, this is NoPrefix. For other denotations this is the prefix
     *  under which the denotation was constructed.
     *
     *  Note that `asSeenFrom` might return a `SymDenotation` and therefore in
     *  general one cannot rely on `prefix` being set, see
     *  `Config.reuseSymDenotations` for details.
     */
    def prefix: Type = NoPrefix

    /** True if the info of this denotation comes from a refinement. */
    def isRefinedMethod: Boolean = false

    /** For SymDenotations, the language-specific signature of the info, depending on
     *  where the symbol is defined. For non-SymDenotations, the Scala 3
     *  signature.
     *
     *  Invariants:
     *  - Before erasure, the signature of a denotation is always equal to the
     *    signature of its corresponding initial denotation.
     *  - Two distinct overloads will have SymDenotations with distinct
     *    signatures (the SELECTin tag in Tasty relies on this to refer to an
     *    overload unambiguously). Note that this only applies to
     *    SymDenotations, in general we cannot assume that distinct
     *    SingleDenotations will have distinct signatures (cf #9050).
     */
    final def signature(using Context): Signature =
      signature(sourceLanguage = if isType || !this.isInstanceOf[SymDenotation] then SourceLanguage.Scala3 else SourceLanguage(symbol))

    /** Overload of `signature` which lets the caller pick the language used
     *  to compute the signature of the info. Useful to match denotations defined in
     *  different classes (see `matchesLoosely`).
     */
    def signature(sourceLanguage: SourceLanguage)(using Context): Signature =
      if (isType) Signature.NotAMethod // don't force info if this is a type denotation
      else info match
        case info: MethodOrPoly =>
          try info.signature(sourceLanguage)
          catch case ex: Exception =>
            if ctx.debug then report.echo(s"cannot take signature of $info")
            throw ex
        case _ => Signature.NotAMethod

    def derivedSingleDenotation(symbol: Symbol, info: Type, pre: Type = this.prefix, isRefinedMethod: Boolean = this.isRefinedMethod)(using Context): SingleDenotation =
      if ((symbol eq this.symbol) && (info eq this.info) && (pre eq this.prefix) && (isRefinedMethod == this.isRefinedMethod)) this
      else newLikeThis(symbol, info, pre, isRefinedMethod)

    def mapInfo(f: Type => Type)(using Context): SingleDenotation =
      derivedSingleDenotation(symbol, f(info))

    inline def orElse(inline that: SingleDenotation): SingleDenotation = if (this.exists) this else that

    def altsWith(p: Symbol => Boolean): List[SingleDenotation] =
      if (exists && p(symbol)) this :: Nil else Nil

    def suchThat(p: Symbol => Boolean)(using Context): SingleDenotation =
      if (exists && p(symbol)) this else NoDenotation

    def hasAltWith(p: SingleDenotation => Boolean): Boolean =
      exists && p(this)

    def accessibleFrom(pre: Type, superAccess: Boolean)(using Context): Denotation =
      if (!symbol.exists || symbol.isAccessibleFrom(pre, superAccess)) this else NoDenotation

    def atSignature(sig: Signature, targetName: Name, site: Type, relaxed: Boolean)(using Context): SingleDenotation =
      val situated = if site == NoPrefix then this else asSeenFrom(site)
      val sigMatches =
        try
          sig.matchDegree(situated.signature) match
            case FullMatch =>
              true
            case MethodNotAMethodMatch =>
              // See comment in `matches`
              relaxed && !symbol.is(JavaDefined)
            case ParamMatch =>
              relaxed
            case noMatch =>
              false
        catch case ex: MissingType =>
          false
      if sigMatches && symbol.hasTargetName(targetName) then this else NoDenotation

    def matchesImportBound(bound: Type)(using Context): Boolean =
      if bound.isRef(defn.NothingClass) then false
      else if bound.isAny then true
      else NoViewsAllowed.normalizedCompatible(info, bound, keepConstraint = false)

    // ------ Transformations -----------------------------------------

    /** The next SingleDenotation in this run, with wrap-around from last to first.
     *
     *  There may be several `SingleDenotation`s with different validity
     *  representing the same underlying definition at different phases.
     *  These are called a "flock". Flock members are generated by
     *  @See current. Flock members are connected in a ring
     *  with their `nextInRun` fields.
     *
     *  There are the following invariants concerning flock members
     *
     *  1) validity periods are non-overlapping
     *  2) the union of all validity periods is a contiguous
     *     interval.
     */
    protected var nextInRun: SingleDenotation = this

    /** The version of this SingleDenotation that was valid in the first phase
     *  of this run.
     */
    def initial: SingleDenotation =
      if (validFor.firstPhaseId <= 1) this
      else {
        var current = nextInRun
        while (current.validFor.code > this.validFor.code) current = current.nextInRun
        current
      }

    def history: List[SingleDenotation] = {
      val b = new ListBuffer[SingleDenotation]
      var current = initial
      while ({
        b += (current)
        current = current.nextInRun
        current ne initial
      })
      ()
      b.toList
    }

    /** Invalidate all caches and fields that depend on base classes and their contents */
    def invalidateInheritedInfo(): Unit = ()

    private def updateValidity()(using Context): this.type = {
      assert(
        ctx.runId >= validFor.runId
        || ctx.settings.YtestPickler.value // mixing test pickler with debug printing can travel back in time
        || ctx.mode.is(Mode.Printing)  // no use to be picky when printing error messages
        || symbol.isOneOf(ValidForeverFlags),
        s"denotation $this invalid in run ${ctx.runId}. ValidFor: $validFor")
      var d: SingleDenotation = this
      while ({
        d.validFor = Period(ctx.runId, d.validFor.firstPhaseId, d.validFor.lastPhaseId)
        d.invalidateInheritedInfo()
        d = d.nextInRun
        d ne this
      })
      ()
      this
    }

    /** Move validity period of this denotation to a new run. Throw a StaleSymbol error
     *  if denotation is no longer valid.
     *  However, StaleSymbol error is not thrown in the following situations:
     *
     *   - If acceptStale returns true (e.g. because we are in the IDE),
     *     update the symbol to the new version if it exists, or return
     *     the old version otherwise.
     *   - If the symbol did not have a denotation that was defined at the current phase
     *     return a NoDenotation instead.
     */
    private def bringForward()(using Context): SingleDenotation = {
      this match {
        case symd: SymDenotation =>
          if (stillValid(symd)) return updateValidity()
          if acceptStale(symd) && symd.initial.validFor.firstPhaseId <= ctx.lastPhaseId then
            // New run might have fewer phases than old, so symbol might no longer be
            // visible at all. TabCompleteTests have examples where this happens.
            return symd.currentSymbol.denot.orElse(symd).updateValidity()
        case _ =>
      }
      if (!symbol.exists) return updateValidity()
      if (!coveredInterval.containsPhaseId(ctx.phaseId)) return NoDenotation
      if (ctx.debug) traceInvalid(this)
      staleSymbolError
    }

    /** The next defined denotation (following `nextInRun`) or an arbitrary
     *  undefined denotation, if all denotations in a `nextinRun` cycle are
     *  undefined.
     */
    private def nextDefined: SingleDenotation = {
      var p1 = this
      var p2 = nextInRun
      while (p1.validFor == Nowhere && (p1 ne p2)) {
        p1 = p1.nextInRun
        p2 = p2.nextInRun.nextInRun
      }
      p1
    }

    /** Skip any denotations that have been removed by an installAfter or that
     *  are otherwise undefined.
     */
    def skipRemoved(using Context): SingleDenotation =
      if (validFor.code <= 0) nextDefined else this

    /** Produce a denotation that is valid for the given context.
     *  Usually called when !(validFor contains ctx.period)
     *  (even though this is not a precondition).
     *  If the runId of the context is the same as runId of this denotation,
     *  the right flock member is located, or, if it does not exist yet,
     *  created by invoking a transformer (@See Transformers).
     *  If the runId's differ, but this denotation is a SymDenotation
     *  and its toplevel owner class or module
     *  is still a member of its enclosing package, then the whole flock
     *  is brought forward to be valid in the new runId. Otherwise
     *  the symbol is stale, which constitutes an internal error.
     */
    def current(using Context): SingleDenotation =
      util.Stats.record("current")
      val currentPeriod = ctx.period
      val valid = validFor

      def assertNotPackage(d: SingleDenotation, transformer: DenotTransformer) = d match
        case d: ClassDenotation =>
          assert(!d.is(Package), s"illegal transformation of package denotation by transformer $transformer")
        case _ =>

      def toNewRun =
        util.Stats.record("current.bringForward")
        if exists then initial.bringForward().current else this

      def goForward =
        var cur = this
        // search for containing period as long as nextInRun increases.
        var next = nextInRun
        while next.validFor.code > valid.code && !(next.validFor contains currentPeriod) do
          cur = next
          next = next.nextInRun
        if next.validFor.code > valid.code then
          // in this case, next.validFor contains currentPeriod
          cur = next
          cur
        else
          //println(s"might need new denot for $cur, valid for ${cur.validFor} at $currentPeriod")
          // not found, cur points to highest existing variant
          val nextTransformerId = ctx.base.nextDenotTransformerId(cur.validFor.lastPhaseId)
          if currentPeriod.lastPhaseId <= nextTransformerId then
            cur.validFor = Period(currentPeriod.runId, cur.validFor.firstPhaseId, nextTransformerId)
          else
            var startPid = nextTransformerId + 1
            val transformer = ctx.base.denotTransformers(nextTransformerId)
            //println(s"transforming $this with $transformer")
            val savedPeriod = ctx.period
            val mutCtx = ctx.asInstanceOf[FreshContext]
            try
              mutCtx.setPhase(transformer)
              next = transformer.transform(cur)
                // We temporarily update the context with the new phase instead of creating a
                // new one. This is done for performance. We cut down on about 30% of context
                // creations that way, and also avoid phase caches in contexts to get large.
                // To work correctly, we need to demand that the context with the new phase
                // is not retained in the result.
            finally
              mutCtx.setPeriod(savedPeriod)
            if next eq cur then
              startPid = cur.validFor.firstPhaseId
            else
              assertNotPackage(next, transformer)
              next.insertAfter(cur)
              cur = next
            cur.validFor = Period(currentPeriod.runId, startPid, transformer.lastPhaseId)
            //printPeriods(cur)
            //println(s"new denot: $cur, valid for ${cur.validFor}")
          cur.current // multiple transformations could be required
      end goForward

      def goBack: SingleDenotation =
        // currentPeriod < end of valid; in this case a version must exist
        // but to be defensive we check for infinite loop anyway
        var cur = this
        var cnt = 0
        while !(cur.validFor contains currentPeriod) do
          //println(s"searching: $cur at $currentPeriod, valid for ${cur.validFor}")
          cur = cur.nextInRun
          // Note: One might be tempted to add a `prev` field to get to the new denotation
          // more directly here. I tried that, but it degrades rather than improves
          // performance: Test setup: Compile everything in dotc and immediate subdirectories
          // 10 times. Best out of 10: 18154ms with `prev` field, 17777ms without.
          cnt += 1
          if cnt > MaxPossiblePhaseId then
            return atPhase(coveredInterval.firstPhaseId)(current)
        cur
      end goBack

      if valid.code <= 0 then
        // can happen if we sit on a stale denotation which has been replaced
        // wholesale by an installAfter; in this case, proceed to the next
        // denotation and try again.
        nextDefined
      else if valid.runId != currentPeriod.runId then
        toNewRun
      else if currentPeriod.code > valid.code then
        goForward
      else
        goBack
    end current

    private def demandOutsideDefinedMsg(using Context): String =
      s"demanding denotation of $this at phase ${ctx.phase}(${ctx.phaseId}) outside defined interval: defined periods are${definedPeriodsString}"

    /** Install this denotation to be the result of the given denotation transformer.
     *  This is the implementation of the same-named method in SymDenotations.
     *  It's placed here because it needs access to private fields of SingleDenotation.
     */
    protected def installAfter(phase: DenotTransformer)(using Context): Unit = {
      val targetId = phase.next.id
      if (ctx.phaseId != targetId) atPhase(phase.next)(installAfter(phase))
      else {
        val current = symbol.current
        // println(s"installing $this after $phase/${phase.id}, valid = ${current.validFor}")
        // println(current.definedPeriodsString)
        this.validFor = Period(ctx.runId, targetId, current.validFor.lastPhaseId)
        if (current.validFor.firstPhaseId >= targetId)
          current.replaceWith(this)
          symbol.denot
            // Let symbol point to updated denotation
            // Without this we can get problems when we immediately recompute the denotation
            // at another phase since the invariant that symbol used to point to a valid
            // denotation is lost.
        else {
          current.validFor = Period(ctx.runId, current.validFor.firstPhaseId, targetId - 1)
          insertAfter(current)
        }
        // println(current.definedPeriodsString)
      }
    }

    /** Apply a transformation `f` to all denotations in this group that start at or after
     *  given phase. Denotations are replaced while keeping the same validity periods.
     */
    protected def transformAfter(phase: DenotTransformer, f: SymDenotation => SymDenotation)(using Context): Unit = {
      var current = symbol.current
      while (current.validFor.firstPhaseId < phase.id && (current.nextInRun.validFor.code > current.validFor.code))
        current = current.nextInRun
      var hasNext = true
      while ((current.validFor.firstPhaseId >= phase.id) && hasNext) {
        val current1: SingleDenotation = f(current.asSymDenotation)
        if (current1 ne current) {
          current1.validFor = current.validFor
          current.replaceWith(current1)
        }
        hasNext = current1.nextInRun.validFor.code > current1.validFor.code
        current = current1.nextInRun
      }
    }

    /** Insert this denotation so that it follows `prev`. */
    private def insertAfter(prev: SingleDenotation) = {
      this.nextInRun = prev.nextInRun
      prev.nextInRun = this
    }

    /** Insert this denotation instead of `old`.
     *  Also ensure that `old` refers with `nextInRun` to this denotation
     *  and set its `validFor` field to `Nowhere`. This is necessary so that
     *  references to the old denotation can be brought forward via `current`
     *  to a valid denotation.
     *
     *  The code to achieve this is subtle in that it works correctly
     *  whether the replaced denotation is the only one in its cycle or not.
     */
    private[dotc] def replaceWith(newd: SingleDenotation): Unit = {
      var prev = this
      while (prev.nextInRun ne this) prev = prev.nextInRun
      // order of next two assignments is important!
      prev.nextInRun = newd
      newd.nextInRun = nextInRun
      validFor = Nowhere
      nextInRun = newd
    }

    def staleSymbolError(using Context): Nothing =
      if symbol.isPackageObject && ctx.run != null && ctx.run.nn.isCompilingSuspended
      then throw StaleSymbolTypeError(symbol)
      else throw StaleSymbolException(staleSymbolMsg)

    def staleSymbolMsg(using Context): String = {
      def ownerMsg = this match {
        case denot: SymDenotation => s"in ${denot.owner}"
        case _ => ""
      }
      s"stale symbol; $this#${symbol.id} $ownerMsg, defined in ${validFor}, is referred to in run ${ctx.period}"
    }

    /** The period (interval of phases) for which there exists
     *  a valid denotation in this flock.
     */
    def coveredInterval(using Context): Period = {
      var cur = this
      var cnt = 0
      var interval = validFor
      while ({
        cur = cur.nextInRun
        cnt += 1
        assert(cnt <= MaxPossiblePhaseId, demandOutsideDefinedMsg)
        interval |= cur.validFor
        cur ne this
      })
      ()
      interval
    }

    /** Show declaration string; useful for showing declarations
     *  as seen from subclasses.
     */
    def showDcl(using Context): String = ctx.printer.dclText(this).show

    override def toString: String =
      if (symbol == NoSymbol) symbol.toString
      else s"<SingleDenotation of type $infoOrCompleter>"

    /** Show all defined periods and the info of the denotation at each */
    def definedPeriodsString(using Context): String = {
      var sb = new StringBuilder()
      var cur = this
      var cnt = 0
      while
        sb.append(i" ${cur.validFor.toString}:${cur.infoOrCompleter}")
        cur = cur.nextInRun
        cnt += 1
        if (cnt > MaxPossiblePhaseId) { sb.append(" ..."); cur = this }
        cur ne this
      do ()
      sb.toString
    }

    // ------ PreDenotation ops ----------------------------------------------

    final def first: SingleDenotation = this
    final def last: SingleDenotation = this

    def matches(other: SingleDenotation)(using Context): Boolean =
      symbol.hasTargetName(other.symbol.targetName)
      && matchesLoosely(other)

    /** `matches` without a target name check.
     *
     *  For definitions coming from different languages, we pick a common
     *  language to compute their signatures. This allows us for example to
     *  override some Java definitions from Scala even if they have a different
     *  erasure (see i8615b, i9109b), Erasure takes care of adding any necessary
     *  bridge to make this work at runtime.
     */
    def matchesLoosely(other: SingleDenotation, alwaysCompareTypes: Boolean = false)(using Context): Boolean =
      if isType then true
      else
        val thisLanguage = SourceLanguage(symbol)
        val otherLanguage = SourceLanguage(other.symbol)
        val commonLanguage = SourceLanguage.commonLanguage(thisLanguage, otherLanguage)
        val sig = signature(commonLanguage)
        val otherSig = other.signature(commonLanguage)
        sig.matchDegree(otherSig) match
          case FullMatch =>
            !alwaysCompareTypes || info.matches(other.info)
          case MethodNotAMethodMatch =>
            !ctx.erasedTypes && {
              // A Scala zero-parameter method and a Scala non-method always match.
              if !thisLanguage.isJava && !otherLanguage.isJava then
                true
              // Java allows defining both a field and a zero-parameter method with the same name,
              // so they must not match.
              else if thisLanguage.isJava && otherLanguage.isJava then
                false
              // A Java field never matches a Scala method.
              else if thisLanguage.isJava then
                symbol.is(Method)
              else // otherLanguage.isJava
                other.symbol.is(Method)
            }
          case ParamMatch =>
            // The signatures do not tell us enough to be sure about matching
            !ctx.erasedTypes && info.matches(other.info)
          case noMatch =>
            false

    def mapInherited(ownDenots: PreDenotation, prevDenots: PreDenotation, pre: Type)(using Context): SingleDenotation =
      if hasUniqueSym && prevDenots.containsSym(symbol) then NoDenotation
      else if isType then filterDisjoint(ownDenots).asSeenFrom(pre)
      else asSeenFrom(pre).filterDisjoint(ownDenots)

    def filterWithPredicate(p: SingleDenotation => Boolean): SingleDenotation =
      if (p(this)) this else NoDenotation
    def filterDisjoint(denots: PreDenotation)(using Context): SingleDenotation =
      if (denots.exists && denots.matches(this)) NoDenotation else this
    def filterWithFlags(required: FlagSet, excluded: FlagSet)(using Context): SingleDenotation =
      val realExcluded = if ctx.isAfterTyper then excluded else excluded | Invisible
      def symd: SymDenotation = this match
        case symd: SymDenotation => symd
        case _ => symbol.denot
      if !required.isEmpty && !symd.isAllOf(required)
         || symd.isOneOf(realExcluded) then NoDenotation
      else this
    def aggregate[T](f: SingleDenotation => T, g: (T, T) => T): T = f(this)

    type AsSeenFromResult = SingleDenotation

    protected def computeAsSeenFrom(pre: Type)(using Context): SingleDenotation = {
      val symbol = this.symbol
      val owner = this match {
        case thisd: SymDenotation => thisd.owner
        case _ => if (symbol.exists) symbol.owner else NoSymbol
      }

      /** The derived denotation with the given `info` transformed with `asSeenFrom`.
       *
       *  As a performance hack, we might reuse an existing SymDenotation,
       *  instead of creating a new denotation with a given `prefix`,
       *  see `Config.reuseSymDenotations`.
       */
      def derived(info: Type) =
        /** Do we need to return a denotation with a prefix set? */
        def needsPrefix =
          // For opaque types, the prefix is used in `ElimOpaques#transform`,
          // without this i7159.scala would fail when compiled from tasty.
          symbol.is(Opaque)

        val derivedInfo = info.asSeenFrom(pre, owner)
        if Config.reuseSymDenotations && this.isInstanceOf[SymDenotation]
           && (derivedInfo eq info) && !needsPrefix then
          this
        else
          derivedSingleDenotation(symbol, derivedInfo, pre)
      end derived

      // Tt could happen that we see the symbol with prefix `this` as a member a different class
      // through a self type and that it then has a different info. In this case we have to go
      // through the asSeenFrom to switch the type back. Test case is pos/i9352.scala.
      def hasOriginalInfo: Boolean = this match
        case sd: SymDenotation => true
        case _ => info eq symbol.info

      def ownerIsPrefix = pre match
        case pre: ThisType => pre.sameThis(owner.thisType)
        case _ => false

      if !owner.membersNeedAsSeenFrom(pre) && (!ownerIsPrefix || hasOriginalInfo)
         || symbol.is(NonMember)
      then this
      else if symbol.isAllOf(ClassTypeParam) then
        val arg = symbol.typeRef.argForParam(pre, widenAbstract = true)
        if arg.exists
        then derivedSingleDenotation(symbol, normalizedArgBounds(arg.bounds), pre)
        else derived(symbol.info)
      else derived(symbol.info)
    }

    /** The argument bounds, possibly intersected with the parameter's info TypeBounds,
     *  if the latter is not F-bounded and does not refer to other type parameters
     *  of the same class, and the intersection is provably nonempty.
     */
    private def normalizedArgBounds(argBounds: TypeBounds)(using Context): TypeBounds =
      if symbol.isCompleted && !hasBoundsDependingOnParamsOf(symbol.owner) then
        val combined @ TypeBounds(lo, hi) = symbol.info.bounds & argBounds
        if (lo frozen_<:< hi) then combined
        else argBounds
      else argBounds

    private def hasBoundsDependingOnParamsOf(cls: Symbol)(using Context): Boolean =
      val acc = new TypeAccumulator[Boolean]:
        def apply(x: Boolean, tp: Type): Boolean = tp match
          case _: LazyRef => true
          case tp: TypeRef
          if tp.symbol.isAllOf(ClassTypeParam) && tp.symbol.owner == cls => true
          case _ => foldOver(x, tp)
      acc(false, symbol.info)
  }

  abstract class NonSymSingleDenotation(symbol: Symbol, initInfo: Type, override val prefix: Type)
  extends SingleDenotation(symbol, initInfo, initInfo.isInstanceOf[TypeType]) {
    def infoOrCompleter: Type = initInfo
  }

  class UniqueRefDenotation(
    symbol: Symbol,
    initInfo: Type,
    initValidFor: Period,
    prefix: Type) extends NonSymSingleDenotation(symbol, initInfo, prefix) {
    validFor = initValidFor
    override def hasUniqueSym: Boolean = true
    protected def newLikeThis(s: Symbol, i: Type, pre: Type, isRefinedMethod: Boolean)(using Context): SingleDenotation =
      if isRefinedMethod then
        new JointRefDenotation(s, i, currentStablePeriod, pre, isRefinedMethod)
      else
        new UniqueRefDenotation(s, i, currentStablePeriod, pre)
  }

  class JointRefDenotation(
    symbol: Symbol,
    initInfo: Type,
    initValidFor: Period,
    prefix: Type,
    override val isRefinedMethod: Boolean) extends NonSymSingleDenotation(symbol, initInfo, prefix) {
    validFor = initValidFor
    override def hasUniqueSym: Boolean = false
    protected def newLikeThis(s: Symbol, i: Type, pre: Type, isRefinedMethod: Boolean)(using Context): SingleDenotation =
      new JointRefDenotation(s, i, currentStablePeriod, pre, isRefinedMethod)
  }

  class ErrorDenotation(using Context) extends NonSymSingleDenotation(NoSymbol, NoType, NoType) {
    override def exists: Boolean = false
    override def hasUniqueSym: Boolean = false
    validFor = Period.allInRun(ctx.runId)
    protected def newLikeThis(s: Symbol, i: Type, pre: Type, isRefinedMethod: Boolean)(using Context): SingleDenotation =
      this
  }

  /** An error denotation that provides more info about the missing reference.
   *  Produced by staticRef, consumed by requiredSymbol.
   */
  case class MissingRef(val owner: SingleDenotation, name: Name)(using Context) extends ErrorDenotation {
    val ex: Exception = new Exception // DEBUG
  }

  /** An error denotation that provides more info about alternatives
   *  that were found but that do not qualify.
   *  Produced by staticRef, consumed by requiredSymbol.
   */
  case class NoQualifyingRef(alts: List[SingleDenotation])(using Context) extends ErrorDenotation

  /** A double definition
   */
  def isDoubleDef(sym1: Symbol, sym2: Symbol)(using Context): Boolean =
    (sym1.exists && sym2.exists &&
    (sym1 `ne` sym2) && (sym1.effectiveOwner `eq` sym2.effectiveOwner) &&
    !sym1.is(Bridge) && !sym2.is(Bridge))

  // --- Overloaded denotations and predenotations -------------------------------------------------

  trait MultiPreDenotation extends PreDenotation {
    def denot1: PreDenotation
    def denot2: PreDenotation

    assert(denot1.exists && denot2.exists, s"Union of non-existing denotations ($denot1) and ($denot2)")
    def first: Denotation = denot1.first
    def last: Denotation = denot2.last
    def matches(other: SingleDenotation)(using Context): Boolean =
      denot1.matches(other) || denot2.matches(other)
    def mapInherited(owndenot: PreDenotation, prevdenot: PreDenotation, pre: Type)(using Context): PreDenotation =
      derivedUnion(denot1.mapInherited(owndenot, prevdenot, pre), denot2.mapInherited(owndenot, prevdenot, pre))
    def filterWithPredicate(p: SingleDenotation => Boolean): PreDenotation =
      derivedUnion(denot1 filterWithPredicate p, denot2 filterWithPredicate p)
    def filterDisjoint(denot: PreDenotation)(using Context): PreDenotation =
      derivedUnion(denot1 filterDisjoint denot, denot2 filterDisjoint denot)
    def filterWithFlags(required: FlagSet, excluded: FlagSet)(using Context): PreDenotation =
      derivedUnion(denot1.filterWithFlags(required, excluded), denot2.filterWithFlags(required, excluded))
    def aggregate[T](f: SingleDenotation => T, g: (T, T) => T): T =
      g(denot1.aggregate(f, g), denot2.aggregate(f, g))
    protected def derivedUnion(denot1: PreDenotation, denot2: PreDenotation) =
      if ((denot1 eq this.denot1) && (denot2 eq this.denot2)) this
      else denot1 union denot2
  }

  final case class DenotUnion(denot1: PreDenotation, denot2: PreDenotation) extends MultiPreDenotation {
    def exists: Boolean = true
    def toDenot(pre: Type)(using Context): Denotation =
      denot1.toDenot(pre).meet(denot2.toDenot(pre), pre)
    def containsSym(sym: Symbol): Boolean =
      (denot1 containsSym sym) || (denot2 containsSym sym)
    type AsSeenFromResult = PreDenotation
    def computeAsSeenFrom(pre: Type)(using Context): PreDenotation =
      derivedUnion(denot1.asSeenFrom(pre), denot2.asSeenFrom(pre))
  }

  /** An overloaded denotation consisting of the alternatives of both given denotations.
   */
  case class MultiDenotation(denot1: Denotation, denot2: Denotation) extends Denotation(NoSymbol, NoType, isType = false) with MultiPreDenotation {
    validFor = denot1.validFor & denot2.validFor

    final def infoOrCompleter: Type = multiHasNot("info")
    final def hasUniqueSym: Boolean = false
    final def name(using Context): Name = denot1.name
    final def signature(using Context): Signature = Signature.OverloadedSignature
    def atSignature(sig: Signature, targetName: Name, site: Type, relaxed: Boolean)(using Context): Denotation =
      if (sig eq Signature.OverloadedSignature) this
      else derivedUnionDenotation(
            denot1.atSignature(sig, targetName, site, relaxed),
            denot2.atSignature(sig, targetName, site, relaxed))
    def current(using Context): Denotation =
      derivedUnionDenotation(denot1.current, denot2.current)
    def altsWith(p: Symbol => Boolean): List[SingleDenotation] =
      denot1.altsWith(p) ++ denot2.altsWith(p)
    def suchThat(p: Symbol => Boolean)(using Context): SingleDenotation = {
      val sd1 = denot1.suchThat(p)
      val sd2 = denot2.suchThat(p)
      if sd1.exists then
        if sd2.exists then
          throw TypeError(
            em"""Failure to disambiguate overloaded reference with
                |  ${denot1.symbol.showLocated}: ${denot1.info}  and
                |  ${denot2.symbol.showLocated}: ${denot2.info}""")
        else sd1
      else sd2
    }
    override def filterWithPredicate(p: SingleDenotation => Boolean): Denotation =
      derivedUnionDenotation(denot1.filterWithPredicate(p), denot2.filterWithPredicate(p))
    def hasAltWith(p: SingleDenotation => Boolean): Boolean =
      denot1.hasAltWith(p) || denot2.hasAltWith(p)
    def accessibleFrom(pre: Type, superAccess: Boolean)(using Context): Denotation = {
      val d1 = denot1.accessibleFrom(pre, superAccess)
      val d2 = denot2.accessibleFrom(pre, superAccess)
      if (!d1.exists) d2
      else if (!d2.exists) d1
      else derivedUnionDenotation(d1, d2)
    }
    def mapInfo(f: Type => Type)(using Context): Denotation =
      derivedUnionDenotation(denot1.mapInfo(f), denot2.mapInfo(f))
    def derivedUnionDenotation(d1: Denotation, d2: Denotation): Denotation =
      if ((d1 eq denot1) && (d2 eq denot2)) this
      else if (!d1.exists) d2
      else if (!d2.exists) d1
      else MultiDenotation(d1, d2)
    type AsSeenFromResult = Denotation
    def computeAsSeenFrom(pre: Type)(using Context): Denotation =
      derivedUnionDenotation(denot1.asSeenFrom(pre), denot2.asSeenFrom(pre))
    override def toString: String = alternatives.mkString(" <and> ")

    private def multiHasNot(op: String): Nothing =
      throw new UnsupportedOperationException(
        s"multi-denotation with alternatives $alternatives does not implement operation $op")
  }

  /** The current denotation of the static reference given by path,
    *  or a MissingRef or NoQualifyingRef instance, if it does not exist.
    *  if generateStubs is set, generates stubs for missing top-level symbols
    */
  def staticRef(path: Name, generateStubs: Boolean = true, isPackage: Boolean = false)(using Context): Denotation = {
    def select(prefix: Denotation, selector: Name): Denotation = {
      val owner = prefix.disambiguate(_.info.isParameterless)
      def isPackageFromCoreLibMissing: Boolean =
        // if the scala package is missing, the stdlib must be missing
        owner.symbol == defn.RootClass && selector == nme.scala
      if (owner.exists) {
        val result = if (isPackage) owner.info.decl(selector) else owner.info.member(selector)
        if (result.exists) result
        else if (isPackageFromCoreLibMissing) throw new MissingCoreLibraryException(selector.toString)
        else {
          val alt =
            if (generateStubs) missingHook(owner.symbol.moduleClass, selector)
            else NoSymbol
          if (alt.exists) alt.denot
          else MissingRef(owner, selector)
        }
      }
      else owner
    }
    def recur(path: Name, wrap: TermName => Name = identity): Denotation = path match {
      case path: TypeName =>
        recur(path.toTermName, n => n.toTypeName)
      case ModuleClassName(underlying) =>
        recur(underlying, n => wrap(ModuleClassName(n)))
      case QualifiedName(prefix, selector) =>
        select(recur(prefix), wrap(selector))
      case qn @ AnyQualifiedName(prefix, _) =>
        recur(prefix, n => wrap(qn.info.mkString(n).toTermName))
      case path: SimpleName =>
        def recurSimple(len: Int, wrap: TermName => Name): Denotation = {
          val point = path.lastIndexOf('.', len - 1)
          val selector = wrap(path.slice(point + 1, len).asTermName)
          val prefix =
            if (point > 0) recurSimple(point, identity)
            else if (selector.isTermName) defn.RootClass.denot
            else defn.EmptyPackageClass.denot
          select(prefix, selector)
        }
        recurSimple(path.length, wrap)
    }

    val run = ctx.run
    if run == null then recur(path)
    else run.staticRefs.getOrElseUpdate(path, recur(path))
  }

  /** If we are looking for a non-existing term name in a package,
    *  assume it is a package for which we do not have a directory and
    *  enter it.
    */
  def missingHook(owner: Symbol, name: Name)(using Context): Symbol =
    if (owner.is(Package) && name.isTermName)
      newCompletePackageSymbol(owner, name.asTermName).entered
    else
      NoSymbol

  trait StaleSymbol extends Exception

  /** An exception for accessing symbols that are no longer valid in current run */
  class StaleSymbolException(msg: => String) extends Exception, StaleSymbol {
    util.Stats.record("stale symbol")
    override def getMessage(): String = msg
  }

  /** An exception that is at the same type a StaleSymbol and a TypeError.
   *  Sine it is a TypeError it can be reported as a nroaml error instead of crashing
   *  the compiler.
   */
  class StaleSymbolTypeError(symbol: Symbol)(using Context) extends TypeError, StaleSymbol:
    def toMessage(using Context) =
      em"Cyclic macro dependency; macro refers to a toplevel symbol in ${symbol.source} from which the macro is called"
}
