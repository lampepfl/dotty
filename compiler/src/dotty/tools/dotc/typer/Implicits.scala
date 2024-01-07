package dotty.tools
package dotc
package typer

import backend.sjs.JSDefinitions
import core.*
import ast.{TreeTypeMap, untpd, tpd}
import util.Spans.*
import util.Stats.{record, monitored}
import printing.{Showable, Printer}
import printing.Texts.*
import Contexts.*
import Types.*
import Flags.*
import Mode.ImplicitsEnabled
import NameKinds.{LazyImplicitName, ContextBoundParamName}
import Symbols.*
import Types.*
import Decorators.*
import Names.*
import StdNames.*
import ProtoTypes.*
import ErrorReporting.*
import Inferencing.{fullyDefinedType, isFullyDefined}
import Scopes.newScope
import Typer.BindingPrec, BindingPrec.*
import Hashable.*
import util.{EqHashMap, Stats}
import config.{Config, Feature, SourceVersion}
import Feature.{migrateTo3, sourceVersion}
import config.Printers.{implicits, implicitsDetailed}
import collection.mutable
import reporting.*
import transform.Splicer
import annotation.tailrec

import scala.annotation.internal.sharable
import scala.annotation.threadUnsafe
import scala.compiletime.uninitialized

/** Implicit resolution */
object Implicits:
  import tpd.*

  /** An implicit definition `implicitRef` that is visible under a different name, `alias`.
   *  Gets generated if an implicit ref is imported via a renaming import.
   */
  class RenamedImplicitRef(val underlyingRef: TermRef, val alias: TermName) extends ImplicitRef {
    def implicitName(using Context): TermName = alias
  }

  /** Both search candidates and successes are references with a specific nesting level. */
  sealed trait RefAndLevel extends Showable {
    def ref: TermRef
    def level: Int
  }

  /** An eligible implicit candidate, consisting of an implicit reference and a nesting level */
  case class Candidate(implicitRef: ImplicitRef, kind: Candidate.Kind, level: Int) extends RefAndLevel with Showable {
    def ref: TermRef = implicitRef.underlyingRef

    def isExtension = (kind & Candidate.Extension) != 0
    def isConversion = (kind & Candidate.Conversion) != 0

    def toText(printer: Printer): Text = printer.toText(this)
  }
  object Candidate {
    type Kind = Int
    inline val None = 0
    inline val Value = 1
    inline val Conversion = 2
    inline val Extension = 4
  }

  /** If `expected` is a selection prototype, does `tp` have an extension
   *  method with the selecting name? False otherwise.
   */
  def hasExtMethod(tp: Type, expected: Type)(using Context) = expected match
    case selProto @ SelectionProto(selName: TermName, _, _, _, _) =>
      tp.memberBasedOnFlags(selName, required = ExtensionMethod).exists
    case _ =>
      false

  def strictEquality(using Context): Boolean =
    ctx.mode.is(Mode.StrictEquality) || Feature.enabled(nme.strictEquality)


  /** A common base class of contextual implicits and of-type implicits which
   *  represents a set of references to implicit definitions.
   */
  abstract class ImplicitRefs(initctx: Context) {
    val irefCtx =
      if (initctx eq NoContext) initctx else initctx.retractMode(Mode.ImplicitsEnabled)
    protected given Context = irefCtx

    /** The nesting level of this context. Non-zero only in ContextualImplicits */
    def level: Int = 0

    /** The implicit references */
    def refs: List[ImplicitRef]

    /** If comes from an implicit scope of a type, the companion objects making
     *  up that implicit scope, otherwise the empty set.
     */
    def companionRefs: TermRefSet = TermRefSet.empty

    private var mySingletonClass: ClassSymbol | Null = null

    /** Widen type so that it is neither a singleton type nor a type that inherits from scala.Singleton. */
    private def widenSingleton(tp: Type)(using Context): Type = {
      if (mySingletonClass == null) mySingletonClass = defn.SingletonClass
      val wtp = tp.widenSingleton
      if (wtp.derivesFrom(mySingletonClass.uncheckedNN)) defn.AnyType else wtp
    }

    protected def isAccessible(ref: TermRef)(using Context): Boolean

    /** Return those references in `refs` that are compatible with type `pt`. */
    protected def filterMatching(pt: Type)(using Context): List[Candidate] = {
      record("filterMatching")

      val considerExtension = pt match
        case ViewProto(_, _: SelectionProto) => true
        case _ => false

      def candidateKind(ref: TermRef)(using Context): Candidate.Kind = { /*trace(i"candidateKind $ref $pt")*/

        def viewCandidateKind(tpw: Type, argType: Type, resType: Type): Candidate.Kind = {

          def methodCandidateKind(mt: MethodType, approx: Boolean) =
            if (mt.isImplicitMethod)
              viewCandidateKind(normalize(mt, pt), argType, resType)
            else if (mt.paramInfos.lengthCompare(1) == 0 && {
                  var formal = widenSingleton(mt.paramInfos.head)
                  if (approx) formal = wildApprox(formal)
                  explore(argType relaxed_<:< formal.widenExpr)
                })
              Candidate.Conversion
            else
              Candidate.None

          tpw match {
            case mt: MethodType =>
              methodCandidateKind(mt, approx = false)
            case poly: PolyType =>
              // We do not need to call ProtoTypes#constrained on `poly` because
              // `candidateKind` is always called with mode TypevarsMissContext enabled.
              poly.resultType match {
                case mt: MethodType =>
                  methodCandidateKind(mt, approx = true)
                case rtp =>
                  viewCandidateKind(wildApprox(rtp), argType, resType)
              }
            case tpw: TermRef => // can't discard overloaded refs
              Candidate.Conversion
              | (if considerExtension then Candidate.Extension else Candidate.None)
            case tpw =>
              // Only direct instances of Function1 and direct or indirect instances of <:< are eligible as views.
              // However, Predef.$conforms is not eligible, because it is a no-op.
              //
              // In principle, it would be cleanest if only implicit methods qualified
              // as implicit conversions. We could achieve that by having standard conversions like
              // this in Predef:
              //
              //    implicit def convertIfConforms[A, B](x: A)(implicit ev: A <:< B): B = ev(a)
              //    implicit def convertIfConverter[A, B](x: A)(implicit ev: Conversion[A, B]): B = ev(a)
              //
              // (Once `<:<` inherits from `Conversion` we only need the 2nd one.)
              // But clauses like this currently slow down implicit search a lot, because
              // they are eligible for all pairs of types, and therefore are tried too often.
              // We emulate instead these conversions directly in the search.
              // The reason for leaving out `Predef_conforms` is that we know it adds
              // nothing since it only relates subtype with supertype.
              //
              // We keep the old behavior under -source 3.0-migration.
              val isFunctionInS2 =
                migrateTo3
                && tpw.derivesFrom(defn.Function1)
                && ref.symbol != defn.Predef_conforms
              val isImplicitConversion = tpw.derivesFrom(defn.ConversionClass)
              // An implementation of <:< counts as a view
              val isConforms = tpw.derivesFrom(defn.SubTypeClass)
              val conversionKind =
                if (isFunctionInS2 || isImplicitConversion || isConforms) Candidate.Conversion
                else Candidate.None
              val extensionKind =
                if considerExtension && hasExtMethod(tpw, resType) then Candidate.Extension
                else Candidate.None
              conversionKind | extensionKind
          }
        }

        def valueTypeCandidateKind(tpw: Type): Candidate.Kind = tpw.stripPoly match {
          case tpw: MethodType =>
            if (tpw.isImplicitMethod) Candidate.Value else Candidate.None
          case _ =>
            Candidate.Value
        }

        /** Widen singleton arguments of implicit conversions to their underlying type.
         *  This is necessary so that they can be found eligible for the argument type.
         *  Note that we always take the underlying type of a singleton type as the argument
         *  type, so that we get a reasonable implicit cache hit ratio.
         */
        def adjustSingletonArg(tp: Type): Type = tp.widenSingleton match
          case tp: PolyType =>
            val res = adjustSingletonArg(tp.resType)
            if res eq tp.resType then tp else tp.derivedLambdaType(resType = res)
          case tp: MethodType =>
            tp.derivedLambdaType(paramInfos = tp.paramInfos.mapConserve(widenSingleton))
          case _ =>
            tp.baseType(defn.ConversionClass) match
              case app @ AppliedType(tycon, from :: rest) =>
                val wideFrom = from.widenSingleton
                if wideFrom ne from then app.derivedAppliedType(tycon, wideFrom :: rest)
                else tp
              case _ => tp

        var ckind =
          if !isAccessible(ref) then
            Candidate.None
          else pt match {
            case pt: ViewProto =>
              viewCandidateKind(ref.widen, pt.argType, pt.resType)
            case _: ValueTypeOrProto =>
              if (defn.isFunctionNType(pt)) Candidate.Value
              else valueTypeCandidateKind(ref.widen)
            case _ =>
              Candidate.Value
          }

        if (ckind == Candidate.None)
          record("discarded eligible")
        else {
          val ptNorm = normalize(pt, pt) // `pt` could be implicit function types, check i2749
          val refAdjusted =
            if (pt.isInstanceOf[ViewProto]) adjustSingletonArg(ref)
            else ref
          val refNorm = normalize(refAdjusted, pt)
          Stats.record("eligible check matches")
          if (!NoViewsAllowed.isCompatible(refNorm, ptNorm))
            ckind = Candidate.None
        }
        ckind
      }


      if refs.isEmpty && (!considerExtension || companionRefs.isEmpty) then
        Nil
      else
        val candidates = new mutable.ListBuffer[Candidate]
        def tryCandidate(extensionOnly: Boolean)(ref: ImplicitRef) =
          var ckind = exploreInFreshCtx { (ctx: FreshContext) ?=>
            ctx.setMode(ctx.mode &~ Mode.SafeNulls | Mode.TypevarsMissContext)
            candidateKind(ref.underlyingRef)
          }
          if extensionOnly then ckind &= Candidate.Extension
          if ckind != Candidate.None then
            candidates += Candidate(ref, ckind, level)

        if considerExtension then
          companionRefs.foreach(tryCandidate(extensionOnly = true))
        if refs.nonEmpty then
          refs.foreach(tryCandidate(extensionOnly = false))
        candidates.toList
    }
  }

  /** The implicit references coming from the implicit scope of a type.
   *  @param tp              the type determining the implicit scope
   *  @param companionRefs   the companion objects in the implicit scope.
   */
  class OfTypeImplicits(tp: Type, override val companionRefs: TermRefSet)(initctx: Context) extends ImplicitRefs(initctx) {
    implicits.println(i"implicit scope of type $tp = ${companionRefs.showAsList}%, %")
    @threadUnsafe lazy val refs: List[ImplicitRef] = {
      val buf = new mutable.ListBuffer[TermRef]
      for (companion <- companionRefs) buf ++= companion.implicitMembers
      buf.toList
    }

    /** The candidates that are eligible for expected type `tp` */
    @threadUnsafe lazy val eligible: List[Candidate] =
      trace(i"eligible($tp), companions = ${companionRefs.showAsList}%, %", implicitsDetailed, show = true) {
        if (refs.nonEmpty && monitored) record(s"check eligible refs in tpe", refs.length)
        filterMatching(tp)
      }

    override def isAccessible(ref: TermRef)(using Context): Boolean =
      ref.symbol.exists

    override def toString: String =
      i"OfTypeImplicits($tp), companions = ${companionRefs.showAsList}%, %; refs = $refs%, %."
  }

  /** The implicit references coming from the context.
   *  @param refs      the implicit references made visible by the current context.
   *                   Note: The name of the reference might be different from the name of its symbol.
   *                   In the case of a renaming import a => b, the name of the reference is the renamed
   *                   name, b, whereas the name of the symbol is the original name, a.
   *  @param outerCtx  the next outer context that makes visible further implicits
   */
  class ContextualImplicits(
      val refs: List[ImplicitRef],
      val outerImplicits: ContextualImplicits | Null,
      val isImport: Boolean)(initctx: Context) extends ImplicitRefs(initctx) {
    private val eligibleCache = EqHashMap[Type, List[Candidate]]()

    /** The level increases if current context has a different owner or scope than
     *  the context of the next-outer ImplicitRefs. This is however disabled under
     *  Scala2 mode, since we do not want to change the implicit disambiguation then.
     */
    override val level: Int =
      def isSameOwner = irefCtx.owner eq outerImplicits.uncheckedNN.irefCtx.owner
      def isSameScope = irefCtx.scope eq outerImplicits.uncheckedNN.irefCtx.scope
      def isLazyImplicit = refs.head.implicitName.is(LazyImplicitName)

      if outerImplicits == null then 1
      else if migrateTo3(using irefCtx)
              || isSameOwner && (isImport || isSameScope && !isLazyImplicit)
      then outerImplicits.uncheckedNN.level
      else outerImplicits.uncheckedNN.level + 1
    end level

    /** Is this the outermost implicits? This is the case if it either the implicits
     *  of NoContext, or the last one before it.
     */
    private def isOutermost = {
      val finalImplicits = NoContext.implicits
      (this eq finalImplicits) || (outerImplicits eqn finalImplicits)
    }

    def bindingPrec: BindingPrec =
      if isImport then if ctx.importInfo.uncheckedNN.isWildcardImport then WildImport else NamedImport else Definition

    private def combineEligibles(ownEligible: List[Candidate], outerEligible: List[Candidate]): List[Candidate] =
      if ownEligible.isEmpty then outerEligible
      else if outerEligible.isEmpty then ownEligible
      else
        val ownNames = mutable.Set(ownEligible.map(_.ref.implicitName)*)
        val outer = outerImplicits.uncheckedNN
        if !migrateTo3(using irefCtx) && level == outer.level && outer.bindingPrec.beats(bindingPrec) then
          val keptOuters = outerEligible.filterConserve: cand =>
            if ownNames.contains(cand.ref.implicitName) then
              val keepOuter = cand.level == level
              if keepOuter then ownNames -= cand.ref.implicitName
              keepOuter
            else true
          val keptOwn = ownEligible.filterConserve: cand =>
            ownNames.contains(cand.ref.implicitName)
          keptOwn ::: keptOuters
        else
          ownEligible ::: outerEligible.filterConserve: cand =>
            !ownNames.contains(cand.ref.implicitName)

    def uncachedEligible(tp: Type)(using Context): List[Candidate] =
      Stats.record("uncached eligible")
      if monitored then record(s"check uncached eligible refs in irefCtx", refs.length)
      val ownEligible = filterMatching(tp)
      if isOutermost then ownEligible
      else combineEligibles(ownEligible, outerImplicits.nn.uncachedEligible(tp))

    /** The implicit references that are eligible for type `tp`. */
    def eligible(tp: Type): List[Candidate] =
      if (tp.hash == NotCached)
        Stats.record(i"compute eligible not cached ${tp.getClass}")
        Stats.record("compute eligible not cached")
        computeEligible(tp)
      else {
        val eligibles = eligibleCache.lookup(tp)
        if (eligibles != null) {
          Stats.record("cached eligible")
          eligibles
        }
        else if (irefCtx eq NoContext) Nil
        else {
          Stats.record(i"compute eligible cached")
          val result = computeEligible(tp)
          eligibleCache(tp) = result
          result
        }
      }

    private def computeEligible(tp: Type): List[Candidate] = /*>|>*/ trace(i"computeEligible $tp in $refs%, %", implicitsDetailed) /*<|<*/ {
      if (monitored) record(s"check eligible refs in irefCtx", refs.length)
      val ownEligible = filterMatching(tp)
      if isOutermost then ownEligible
      else combineEligibles(ownEligible, outerImplicits.nn.eligible(tp))
    }

    override def isAccessible(ref: TermRef)(using Context): Boolean =
      ref.symbol.isAccessibleFrom(ref.prefix)

    override def toString: String = {
      val own = i"(implicits: $refs%, %)"
      if (isOutermost) own else own + "\n " + outerImplicits
    }

    /** This context, or a copy, ensuring root import from symbol `root`
     *  is not present in outer implicits.
     */
    def exclude(root: Symbol): ContextualImplicits =
      if (this == NoContext.implicits) this
      else {
        val outerExcluded = outerImplicits.nn exclude root
        if (irefCtx.importInfo.nn.site.termSymbol == root) outerExcluded
        else if (outerExcluded eqn outerImplicits) this
        else new ContextualImplicits(refs, outerExcluded, isImport)(irefCtx)
      }
  }

  /** Search mode to use for possibly avoiding looping givens */
  enum SearchMode:
    case Old,          // up to 3.3, old mode w/o protection
    	 CompareWarn,  // from 3.4, old mode, warn if new mode would change result
    	 CompareErr,   // from 3.5, old mode, error if new mode would change result
    	 New           // from future, new mode where looping givens are avoided

  /** The result of an implicit search */
  sealed abstract class SearchResult extends Showable {
    def tree: Tree
    def toText(printer: Printer): Text = printer.toText(this)
    def recoverWith(other: SearchFailure => SearchResult): SearchResult = this match {
      case _: SearchSuccess => this
      case fail: SearchFailure => other(fail)
    }
    def isSuccess: Boolean = isInstanceOf[SearchSuccess]
  }

  /** A successful search
   *  @param tree        The typed tree that needs to be inserted
   *  @param ref         The implicit reference that succeeded
   *  @param level       The level where the reference was found
   *  @param isExtension Whether the result is an extension method application
   *  @param tstate The typer state to be committed if this alternative is chosen
   */
  case class SearchSuccess(tree: Tree, ref: TermRef, level: Int, isExtension: Boolean = false)(val tstate: TyperState, val gstate: GadtConstraint)
  extends SearchResult with RefAndLevel with Showable

  /** A failed search */
  case class SearchFailure(tree: Tree) extends SearchResult {
    require(tree.tpe.isInstanceOf[SearchFailureType], s"unexpected type for ${tree}")
    final def isAmbiguous: Boolean = tree.tpe.isInstanceOf[AmbiguousImplicits | TooUnspecific]
    final def reason: SearchFailureType = tree.tpe.asInstanceOf[SearchFailureType]
  }

  object SearchFailure {
    def apply(tpe: SearchFailureType, span: Span)(using Context): SearchFailure = {
      val id = tpe match
        case tpe: (AmbiguousImplicits | TooUnspecific) =>
          untpd.SearchFailureIdent(nme.AMBIGUOUS, s"/* ambiguous: ${tpe.explanation} */")
        case _ =>
          untpd.SearchFailureIdent(nme.MISSING, "/* missing */")
      SearchFailure(id.withTypeUnchecked(tpe).withSpan(span))
    }
  }

  abstract class SearchFailureType extends ErrorType, Addenda {
    def expectedType: Type
    def argument: Tree

    /** A "massaging" function for displayed types to give better info in error diagnostics */
    def clarify(tp: Type)(using Context): Type = tp

    final protected def qualify(using Context): String = expectedType match {
      case SelectionProto(name, mproto, _, _, _) if !argument.isEmpty =>
        i"provide an extension method `$name` on ${argument.tpe}"
      case NoType =>
        if (argument.isEmpty) i"match expected type"
        else i"convert from ${argument.tpe} to expected type"
      case _ =>
        if (argument.isEmpty) i"match type ${clarify(expectedType)}"
        else i"convert from ${argument.tpe} to ${clarify(expectedType)}"
    }
  }

  class NoMatchingImplicits(val expectedType: Type, val argument: Tree, constraint: Constraint = OrderingConstraint.empty)
  extends SearchFailureType {

    /** Replace all type parameters in constraint by their bounds, to make it clearer
     *  what was expected
     */
    override def clarify(tp: Type)(using Context): Type =
      val ctx1 = ctx.fresh.setExploreTyperState()
      ctx1.typerState.constraint = constraint
      inContext(ctx1) {
        val map = new TypeMap:
          def apply(t: Type): Type = t match
            case t: TypeParamRef =>
              constraint.entry(t) match
                case NoType | _: TypeBounds => t
                case t1 => t1
            case t: TypeVar =>
              t.instanceOpt.orElse(apply(t.origin))
            case _ =>
              mapOver(t)

          override def mapArgs(args: List[Type], tparams: List[ParamInfo]) =
            args.mapConserve {
              case t: TypeParamRef =>
                constraint.entry(t) match
                  case bounds: TypeBounds => TypeComparer.fullBounds(t)
                  case _ => this(t)
              case t => this(t)
            }
        end map
        map(tp)
      }

    def msg(using Context): Message =
      em"no implicit values were found that $qualify"

    override def toString = s"NoMatchingImplicits($expectedType, $argument)"
  }

  @sharable object NoMatchingImplicits extends NoMatchingImplicits(NoType, EmptyTree, OrderingConstraint.empty)

  @sharable val NoMatchingImplicitsFailure: SearchFailure =
    SearchFailure(NoMatchingImplicits, NoSpan)(using NoContext)

  @sharable object ImplicitSearchTooLarge extends NoMatchingImplicits(NoType, EmptyTree, OrderingConstraint.empty)

  @sharable val ImplicitSearchTooLargeFailure: SearchFailure =
    SearchFailure(ImplicitSearchTooLarge, NoSpan)(using NoContext)

  /** A failure value indicating that an implicit search for a conversion was not tried */
  case class TooUnspecific(target: Type) extends NoMatchingImplicits(NoType, EmptyTree, OrderingConstraint.empty):

    override def toAdd(using Context) =
      i"""
         |Note that implicit conversions were not tried because the result of an implicit conversion
         |must be more specific than $target""" :: Nil

    override def msg(using Context) =
      super.msg.append("\nThe expected type $target is not specific enough, so no search was attempted")

    override def toString = s"TooUnspecific"
  end TooUnspecific

  /** An ambiguous implicits failure */
  class AmbiguousImplicits(val alt1: SearchSuccess, val alt2: SearchSuccess, val expectedType: Type, val argument: Tree) extends SearchFailureType:

    def msg(using Context): Message =
      var str1 = err.refStr(alt1.ref)
      var str2 = err.refStr(alt2.ref)
      if str1 == str2 then
        str1 = ctx.printer.toTextRef(alt1.ref).show
        str2 = ctx.printer.toTextRef(alt2.ref).show
      em"both $str1 and $str2 $qualify".withoutDisambiguation()

    override def toAdd(using Context) =
      if !argument.isEmpty && argument.tpe.widen.isRef(defn.NothingClass) then
        Nil
      else
        val what = if (expectedType.isInstanceOf[SelectionProto]) "extension methods" else "conversions"
        i"""
           |Note that implicit $what cannot be applied because they are ambiguous;
           |$explanation""" :: Nil
  end AmbiguousImplicits

  class MismatchedImplicit(ref: TermRef,
                           val expectedType: Type,
                           val argument: Tree) extends SearchFailureType {
    def msg(using Context): Message =
      em"${err.refStr(ref)} does not $qualify"
  }

  class DivergingImplicit(ref: TermRef,
                          val expectedType: Type,
                          val argument: Tree) extends SearchFailureType {
    def msg(using Context): Message =
      em"${err.refStr(ref)} produces a diverging implicit search when trying to $qualify"
  }

  /** A search failure type for attempted ill-typed extension method calls */
  class FailedExtension(extApp: Tree, val expectedType: Type, val whyFailed: Message) extends SearchFailureType:
    def argument = EmptyTree
    def msg(using Context) = em"$extApp does not $qualify"

   /** A search failure type for aborted searches of extension methods, typically
    *  because of a cyclic reference or similar.
    */
  class NestedFailure(_msg: Message, val expectedType: Type) extends SearchFailureType:
    def argument = EmptyTree
    override def msg(using Context) = _msg

  /** A search failure type for failed synthesis of terms for special types */
  class SynthesisFailure(reasons: List[String], val expectedType: Type) extends SearchFailureType:
    def argument = EmptyTree

    private def formatReasons =
      if reasons.length > 1 then
        reasons.mkString("\n\t* ", "\n\t* ", "")
      else
        reasons.mkString(" ", "", "")

    def msg(using Context) = em"Failed to synthesize an instance of type ${clarify(expectedType)}:${formatReasons}"

  class MacroErrorsFailure(errors: List[Diagnostic.Error],
                           val expectedType: Type,
                           val argument: Tree) extends SearchFailureType {
    def msg(using Context): Message =
      em"${errors.map(_.msg).mkString("\n")}"
  }
end Implicits

import Implicits.*

/** Info relating to implicits that is kept for one run */
trait ImplicitRunInfo:
  self: Run =>

  private val implicitScopeCache = util.EqHashMap[Type, OfTypeImplicits]()

  private def isExcluded(sym: Symbol) =
    if migrateTo3 then false else sym.is(Package) || sym.isPackageObject

  /** Is `sym` an anchor type for which givens may exist? Anchor types are classes,
    *  opaque type aliases, match aliases and abstract types, but not type parameters
    *  or package objects.
    */
  private def isAnchor(sym: Symbol) =
    sym.isClass && !isExcluded(sym)
    || sym.isOpaqueAlias
    || sym.is(Deferred, butNot = Param)
    || sym.info.isInstanceOf[MatchAlias]

  private def computeIScope(rootTp: Type): OfTypeImplicits =

    object collectParts extends TypeTraverser:

      private var parts: mutable.LinkedHashSet[Type] = uninitialized
      private val partSeen = util.HashSet[Type]()

      def traverse(t: Type) = try
        if partSeen.contains(t) then ()
        else if implicitScopeCache.contains(t) then parts += t
        else
          partSeen += t
          t.dealias match
            case t: TypeRef =>
              if isAnchor(t.symbol) then
                parts += t
                traverse(t.prefix)
              else
                traverse(t.underlying)
            case t: TermRef =>
              if !isExcluded(t.symbol) then
                traverse(t.info)
                traverse(t.prefix)
            case t: ThisType if t.cls.is(Module) && t.cls.isStaticOwner =>
              traverse(t.cls.sourceModule.termRef)
            case t: ThisType =>
              traverse(t.tref)
            case t: ConstantType =>
              traverse(t.underlying)
            case t: TypeParamRef =>
              assert(!ctx.typerState.constraint.contains(t), i"`wildApprox` failed to remove uninstantiated $t")
              traverse(t.underlying)
            case t: TermParamRef =>
              traverse(t.underlying)
            case t: TypeLambda =>
              for p <- t.paramRefs do partSeen += p
              traverseChildren(t)
            case t =>
              traverseChildren(t)
              traverse(t.normalized)
      catch case ex: Throwable => handleRecursive("collectParts of", t.show, ex)

      def apply(tp: Type): collection.Set[Type] =
        parts = mutable.LinkedHashSet()
        partSeen.clear(resetToInitial = false)
        traverse(tp)
        parts
    end collectParts

    val seen = util.HashSet[Type]()
    val incomplete = util.HashSet[Type]()

    def collectCompanions(tp: Type, parts: collection.Set[Type]): TermRefSet =
      val companions = new TermRefSet

      def iscopeRefs(t: Type): TermRefSet =
        implicitScopeCache.lookup(t) match
          case is: OfTypeImplicits =>
            is.companionRefs
          case null =>
            if seen.contains(t) then
              incomplete += tp // all references for `t` will be accounted for in `seen` so we return `EmptySet`.
              TermRefSet.empty        // on the other hand, the refs of `tp` are now inaccurate, so `tp` is marked incomplete.
            else
              seen += t
              val is = recur(t)
              if !implicitScopeCache.contains(t) then incomplete += tp
              is.companionRefs
      end iscopeRefs

      def addCompanion(pre: Type, companion: Symbol) =
        if companion.exists && !companion.isAbsent() then
          companions += TermRef(pre, companion)

      def addCompanions(t: Type) = implicitScopeCache.lookup(t) match
        case iscope: OfTypeImplicits =>
          companions ++= iscope.companionRefs
        case null => t match
          case t: TypeRef =>
            val sym = t.symbol
            val pre = t.prefix
            addPath(pre)
            addCompanion(pre,
              if sym.isClass then sym.companionModule
              else pre.member(sym.name.toTermName)
                .suchThat(companion => companion.is(Module) && companion.owner == sym.owner)
                .symbol)

            // The companion of `js.|` defines an implicit conversions from
            // `A | Unit` to `js.UndefOrOps[A]`. To keep this conversion in scope
            // in Scala 3, where we re-interpret `js.|` as a real union, we inject
            // it in the scope of `Unit`.
            if t.isRef(defn.UnitClass) && ctx.settings.scalajs.value then
              companions += JSDefinitions.jsdefn.UnionOpsModuleRef

            if sym.isClass then
              for p <- t.parents do companions ++= iscopeRefs(p)
            else
              companions ++= iscopeRefs(t.underlying)
      end addCompanions

      def addPath(pre: Type): Unit = pre.dealias match
        case pre: ThisType if pre.cls.is(Module) && pre.cls.isStaticOwner =>
          addPath(pre.cls.sourceModule.termRef)
        case pre: TermRef if !isExcluded(pre.symbol) =>
          pre.info match
            case info: SingletonType =>
              addPath(info)
            case info: TypeRef if info.symbol.is(Module) =>
              addCompanion(info.prefix, info.symbol.sourceModule)
              addPath(info.prefix)
            case _ =>
              companions += pre
              addPath(pre.prefix)
        case _ =>

      parts.foreach(addCompanions)
      companions
    end collectCompanions

    def recur(tp: Type): OfTypeImplicits =
      val parts = collectParts(tp)
      val companions = collectCompanions(tp, parts)
      val result = OfTypeImplicits(tp, companions)(runContext)
      if Config.cacheImplicitScopes
        && tp.hash != NotCached
        && (tp eq rootTp)              // first type traversed is always cached
           || !incomplete.contains(tp) // other types are cached if they are not incomplete
      then implicitScopeCache(tp) = result
      result

    record(i"computeIScope")
    recur(rootTp)
  end computeIScope

  /** The implicit scope of a type `tp`, which is specified by the following definitions.
   *
   *  A reference is an _anchor_ if it refers to an object, a class, a trait, an
   *  abstract type, an opaque type alias, or a match type alias. References to
   *  packages and package objects are anchors only under -source:3.0-migration.
   *
   *  The _anchors_ of a type `T` is a set of references defined as follows:
   *
   *   - If `T` is a reference to an anchor, `T` itself plus, if `T` is of the form
   *     `P#A`, the anchors of `P`.
   *   - If `T` is an alias of `U`, the anchors of `U`.
   *   - If `T` is a reference to a type parameter, the union of the anchors of both of its bounds.
   *   - If `T` is a singleton reference, the anchors of its underlying type, plus,
   *     if `T` is of the form `(P#x).type`, the anchors of `P`.
   *   - If `T` is the this-type of a static object, the anchors of a term reference to that object.
   *   - If `T` is some other this-type `P.this.type`, the anchors of `P`.
   *   - If `T` is some other type, the union of the anchors of each constituent type of `T`.
   *
   *  The _implicit scope_ of a type `tp` is the smallest set S of term references (i.e. TermRefs)
   *  such that
   *
   *   - If `T` is a reference to a class, S includes a reference to the companion object
   *     of the class, if it exists, as well as the implicit scopes of all of `T`'s parent classes.
   *   - If `T` is a reference to an object, S includes `T` itself as well as
   *     the implicit scopes of all of `T`'s parent classes.
   *   - If `T` is a reference to an opaque type alias named `A`, S includes
   *     a reference to an object `A` defined in the same scope as the type, if it exists,
   *     as well as the implicit scope of `T`'s underlying type or bounds.
   *   - If `T` is a reference to an an abstract type or match type alias named `A`,
   *     S includes a reference to an object `A` defined in the same scope as the type,
   *     if it exists, as well as the implicit scopes of `T`'s lower and upper bound,
   *     if present.
   *   - If `T` is a reference to an anchor of the form `p.A` then S also includes
   *     all term references on the path `p`.
   *   - If `T` is some other type, S includes the implicit scopes of all anchors of `T`.
   */
  def implicitScope(tp: Type)(using Context): OfTypeImplicits =
    implicitScopeCache.lookup(tp) match
      case is: OfTypeImplicits =>
        record("implicitScope cache hit")
        is
      case null =>
        record(i"implicitScope")
        val liftToAnchors = new TypeMap:
          override def stopAt = StopAt.Static
          private val seen = util.HashSet[Type]()

          def applyToUnderlying(t: TypeProxy) =
            if seen.contains(t) then
              WildcardType
            else
              seen += t
              t.superType match
                case TypeBounds(lo, hi) =>
                  if lo.isBottomTypeAfterErasure then apply(hi)
                  else AndType.make(apply(lo), apply(hi))
                case u => apply(u)

          def apply(t: Type) = t.dealias match
            case t: TypeRef =>
              if t.symbol.isClass || isAnchor(t.symbol) then t else applyToUnderlying(t)
            case t: TypeVar => apply(t.underlying)
            case t: ParamRef => applyToUnderlying(t)
            case t: ConstantType => apply(t.underlying)
            case t => mapOver(t)
        end liftToAnchors
        val liftedTp = liftToAnchors(tp)
        if liftedTp eq tp then
          record(i"implicitScope unlifted")
          computeIScope(tp)
        else
          record("implicitScope lifted")
          val liftedIScope = implicitScopeCache.getOrElse(liftedTp, computeIScope(liftedTp))
          val result = OfTypeImplicits(tp, liftedIScope.companionRefs)(runContext)
          implicitScopeCache(tp) = result
          result
  end implicitScope

  protected def reset(): Unit =
    implicitScopeCache.clear()
end ImplicitRunInfo

/** The implicit resolution part of type checking */
trait Implicits:
  self: Typer =>

  import tpd.*

  override def viewExists(from: Type, to: Type)(using Context): Boolean =
       !from.isError
    && !to.isError
    && !ctx.isAfterTyper
    && ctx.mode.is(Mode.ImplicitsEnabled)
    && from.isValueType
    && (  from.isValueSubType(to)
       || inferView(dummyTreeOfType(from), to)
            (using ctx.fresh.addMode(Mode.ImplicitExploration).setExploreTyperState()).isSuccess
          // TODO: investigate why we can't TyperState#test here
       )

  /** Find an implicit conversion to apply to given tree `from` so that the
   *  result is compatible with type `to`.
   */
  def inferView(from: Tree, to: Type)(using Context): SearchResult = {
    record("inferView")
    if !ctx.mode.is(Mode.ImplicitsEnabled) || from.isInstanceOf[Super] then
      NoMatchingImplicitsFailure
    else {
      def adjust(to: Type) = to.stripTypeVar.widenExpr match {
        case SelectionProto(name, memberProto, compat, true, nameSpan) =>
          SelectionProto(name, memberProto, compat, privateOK = false, nameSpan)
        case tp => tp
      }

      def isOldStyleFunctionConversion(tpe: Type): Boolean =
        tpe match {
          case PolyType(_, resType) => isOldStyleFunctionConversion(resType)
          case _ => tpe.derivesFrom(defn.FunctionSymbol(1)) && !tpe.derivesFrom(defn.ConversionClass) && !tpe.derivesFrom(defn.SubTypeClass)
        }

      try
        val inferred = inferImplicit(adjust(to), from, from.span)

        inferred match {
          case SearchSuccess(_, ref, _, false) if isOldStyleFunctionConversion(ref.underlying) =>
            report.migrationWarning(
              em"The conversion ${ref} will not be applied implicitly here in Scala 3 because only implicit methods and instances of Conversion class will continue to work as implicit views.",
              from
            )
          case _ =>
        }

        inferred
      catch {
        case ex: AssertionError =>
          implicits.println(s"view $from ==> $to")
          implicits.println(ctx.typerState.constraint.show)
          implicits.println(TypeComparer.explained(_.isSubType(from.tpe, to)))
          throw ex
      }
    }
  }

  private var synthesizer: Synthesizer | Null = null

  /** Find an implicit argument for parameter `formal`.
   *  Return a failure as a SearchFailureType in the type of the returned tree.
   */
  def inferImplicitArg(formal: Type, span: Span)(using Context): Tree =
    inferImplicit(formal, EmptyTree, span) match
      case SearchSuccess(arg, _, _, _) => arg
      case fail @ SearchFailure(failed) =>
        if fail.isAmbiguous then failed
        else
          if synthesizer == null then synthesizer = Synthesizer(this)
          val (tree, errors) = synthesizer.uncheckedNN.tryAll(formal, span)
          if errors.nonEmpty then
            SearchFailure(new SynthesisFailure(errors, formal), span).tree
          else
            tree.orElse(failed)


  /** Search an implicit argument and report error if not found */
  def implicitArgTree(formal: Type, span: Span, where: => String = "")(using Context): Tree = {
    val arg = inferImplicitArg(formal, span)
    if (arg.tpe.isInstanceOf[SearchFailureType])
      report.error(missingArgMsg(arg, formal, where), ctx.source.atSpan(span))
    arg
  }

  /** @param arg                          Tree representing a failed result of implicit search
   *  @param pt                           Type for which an implicit value was searched
   *  @param where                        Description of where the search was performed. Might be empty
   *  @param paramSymWithMethodCallTree   Symbol of the parameter for which the implicit was searched and tree of the method call that triggered the implicit search
   */
  def missingArgMsg(
    arg: Tree,
    pt: Type,
    where: String,
    paramSymWithMethodCallTree: Option[(Symbol, Tree)] = None
  )(using Context): Message = {
    def findHiddenImplicitsCtx(c: Context): Context =
      if c == NoContext then c
      else c.freshOver(findHiddenImplicitsCtx(c.outer)).addMode(Mode.FindHiddenImplicits)

    def ignoredInstanceNormalImport = arg.tpe match
      case fail: SearchFailureType =>
        if (fail.expectedType eq pt) || isFullyDefined(fail.expectedType, ForceDegree.none) then
          inferImplicit(fail.expectedType, fail.argument, arg.span)(
            using findHiddenImplicitsCtx(ctx)) match {
            case s: SearchSuccess => Some(s)
            case f: SearchFailure =>
              f.reason match {
                case ambi: AmbiguousImplicits => Some(ambi.alt1)
                case r => None
              }
          }
        else
          // It's unsafe to search for parts of the expected type if they are not fully defined,
          // since these come with nested contexts that are lost at this point. See #7249 for an
          // example where searching for a nested type causes an infinite loop.
          None

    def allImplicits(currImplicits: ContextualImplicits): List[ImplicitRef] =
      if currImplicits.outerImplicits == null then currImplicits.refs
      else currImplicits.refs ::: allImplicits(currImplicits.outerImplicits)

    /** Whether the given type is for an implicit def that's a Scala 2 implicit conversion */
    def isImplicitDefConversion(typ: Type): Boolean = typ match {
      case PolyType(_, resType) => isImplicitDefConversion(resType)
      case mt: MethodType => !mt.isImplicitMethod && !mt.isContextualMethod
      case _ => false
    }

    def ignoredConvertibleImplicits = arg.tpe match
      case fail: SearchFailureType =>
        if (fail.expectedType eq pt) || isFullyDefined(fail.expectedType, ForceDegree.none) then
          // Get every implicit in scope and try to convert each
          allImplicits(ctx.implicits)
            .view
            .map(_.underlyingRef)
            .distinctBy(_.denot)
            .filter { imp =>
              !isImplicitDefConversion(imp.underlying)
                && imp.symbol != defn.Predef_conforms
                && viewExists(imp.underlying.resultType, fail.expectedType)
            }
        else
          Nil

    MissingImplicitArgument(arg, pt, where, paramSymWithMethodCallTree, ignoredInstanceNormalImport, ignoredConvertibleImplicits)
  }

  /** A string indicating the formal parameter corresponding to a  missing argument */
  def implicitParamString(paramName: TermName, methodStr: String, tree: Tree)(using Context): String =
    tree match {
      case Select(qual, nme.apply) if defn.isFunctionNType(qual.tpe.widen) =>
        val qt = qual.tpe.widen
        val qt1 = qt.dealiasKeepAnnots
        def addendum = if (qt1 eq qt) "" else (i"\nWhere $qt is an alias of: $qt1")
        i"parameter of ${qual.tpe.widen}$addendum"
      case _ =>
        i"${ if paramName.is(ContextBoundParamName) then "a context parameter"
             else s"parameter $paramName" } of $methodStr"
    }

  /** A CanEqual[T, U] instance is assumed
   *   - if one of T, U is an error type, or
   *   - if one of T, U is a subtype of the lifted version of the other,
   *     unless strict equality is set.
   */
  def assumedCanEqual(ltp: Type, rtp: Type)(using Context) = {
    // Map all non-opaque abstract types to their upper bound.
    // This is done to check whether such types might plausibly be comparable to each other.
    val lift = new TypeMap {
      def apply(t: Type): Type = t match {
        case t: TypeRef =>
          t.info match {
            case TypeBounds(lo, hi) if lo.ne(hi) && !t.symbol.is(Opaque) => apply(hi)
            case _ => t
          }
        case t: SingletonType =>
          apply(t.widen)
        case t: RefinedType =>
          apply(t.parent)
        case t: LazyRef =>
          t
        case _ =>
          if (variance > 0) mapOver(t) else t
      }
    }

    ltp.isError
    || rtp.isError
    || !strictEquality && (ltp <:< lift(rtp) || rtp <:< lift(ltp))
  }

  /** Check that equality tests between types `ltp` and `rtp` make sense */
  def checkCanEqual(ltp: Type, rtp: Type, span: Span)(using Context): Unit =
    if (!ctx.isAfterTyper && !assumedCanEqual(ltp, rtp)) {
      val res = implicitArgTree(defn.CanEqualClass.typeRef.appliedTo(ltp, rtp), span)
      implicits.println(i"CanEqual witness found for $ltp / $rtp: $res: ${res.tpe}")
    }

  object hasSkolem extends TreeAccumulator[Boolean]:
    def apply(x: Boolean, tree: Tree)(using Context): Boolean =
      x || {
        tree match
          case tree: Ident => tree.symbol.isSkolem
          case Select(qual, _) => apply(x, qual)
          case Apply(fn, _) => apply(x, fn)
          case TypeApply(fn, _) => apply(x, fn)
          case _: This => false
          case _ => foldOver(x, tree)
      }

  /** Find an implicit parameter or conversion.
   *  @param pt              The expected type of the parameter or conversion.
   *  @param argument        If an implicit conversion is searched, the argument to which
   *                         it should be applied, EmptyTree otherwise.
   *  @param span            The position where errors should be reported.
   */
  def inferImplicit(pt: Type, argument: Tree, span: Span)(using Context): SearchResult =
    trace(s"search implicit ${pt.show}, arg = ${argument.show}: ${argument.tpe.show}", implicits, show = true) {
      record("inferImplicit")
      assert(ctx.phase.allowsImplicitSearch,
        if (argument.isEmpty) i"missing implicit parameter of type $pt after typer at phase ${ctx.phase.phaseName}"
        else i"type error: ${argument.tpe} does not conform to $pt${err.whyNoMatchStr(argument.tpe, pt)}")

      val usableForInference = pt.exists && !pt.unusableForInference
        && (argument.isEmpty || !argument.tpe.unusableForInference)

      val result0 = if usableForInference then
        // If we are searching implicits when resolving an import symbol, start the search
        // in the first enclosing context that does not have the same scope and owner as the current
        // context. Without that precaution, an eligible implicit in the current scope
        // would cause a cyclic reference error (if the import is named) or cause a
        // spurious import skip (if the import is a wildcard import). See i12802 for a test case.
        var searchCtx = ctx
        if ctx.owner.isImport then
          while
            searchCtx = searchCtx.outer
            (searchCtx.scope eq ctx.scope) && (searchCtx.owner eq ctx.owner.owner)
          do ()

        try ImplicitSearch(pt, argument, span)(using searchCtx).bestImplicit
        catch case ce: CyclicReference =>
          ce.inImplicitSearch = true
          throw ce
      else NoMatchingImplicitsFailure

      val result =
        result0 match {
          case result: SearchSuccess =>
            if result.tstate ne ctx.typerState then
              result.tstate.commit()
            if result.gstate ne ctx.gadt then
              ctx.gadtState.restore(result.gstate)
            if hasSkolem(false, result.tree) then
              report.error(SkolemInInferred(result.tree, pt, argument), ctx.source.atSpan(span))
            implicits.println(i"success: $result")
            implicits.println(i"committing ${result.tstate.constraint} yielding ${ctx.typerState.constraint} in ${ctx.typerState}")
            result
          case result: SearchFailure if result.isAmbiguous =>
            val deepPt = pt.deepenProto
            if (deepPt ne pt) inferImplicit(deepPt, argument, span)
            else if (migrateTo3 && !ctx.mode.is(Mode.OldOverloadingResolution))
              withMode(Mode.OldOverloadingResolution)(inferImplicit(pt, argument, span)) match {
                case altResult: SearchSuccess =>
                  report.migrationWarning(
                    result.reason.msg
                      .prepend(s"According to new implicit resolution rules, this will be ambiguous:\n"),
                    ctx.source.atSpan(span))
                  altResult
                case _ =>
                  result
              }
            else result
          case NoMatchingImplicitsFailure if usableForInference =>
            SearchFailure(new NoMatchingImplicits(pt, argument, ctx.typerState.constraint), span)
          case _ =>
            result0
        }
      // If we are at the outermost implicit search then emit the implicit dictionary, if any.
      ctx.searchHistory.emitDictionary(span, result)
    }

  /** Try to typecheck an implicit reference */
  def typedImplicit(cand: Candidate, pt: Type, argument: Tree, span: Span)(using Context): SearchResult =  trace(i"typed implicit ${cand.ref}, pt = $pt, implicitsEnabled == ${ctx.mode is ImplicitsEnabled}", implicits, show = true) {
    if ctx.run.nn.isCancelled then NoMatchingImplicitsFailure
    else
      record("typedImplicit")
      val ref = cand.ref
      val generated: Tree = tpd.ref(ref).withSpan(span.startPos)
      val locked = ctx.typerState.ownedVars
      val adapted =
        if argument.isEmpty then
          if defn.isContextFunctionType(pt) then
            // need to go through typed, to build the context closure
            typed(untpd.TypedSplice(generated), pt, locked)
          else
            // otherwise we can skip typing and go directly to adapt
            adapt(generated, pt.widenExpr, locked)
        else {
          def untpdGenerated = untpd.TypedSplice(generated)
          def producesConversion(info: Type): Boolean = info match
            case info: PolyType => producesConversion(info.resType)
            case info: MethodType if info.isImplicitMethod => producesConversion(info.resType)
            case _ => info.derivesFrom(defn.ConversionClass)
          def tryConversion(using Context) = {
            val untpdConv =
              if ref.symbol.is(Given) && producesConversion(ref.symbol.info) then
                untpd.Select(
                  untpd.TypedSplice(
                    adapt(generated,
                      defn.ConversionClass.typeRef.appliedTo(argument.tpe, pt),
                      locked)),
                  nme.apply)
              else untpdGenerated
            typed(
              untpd.Apply(untpdConv, untpd.TypedSplice(argument) :: Nil),
              pt, locked)
          }
          pt match
            case selProto @ SelectionProto(selName: TermName, mbrType, _, _, nameSpan) =>

              def tryExtension(using Context) =
                extMethodApply(untpd.Select(untpdGenerated, selName).withSpan(nameSpan), argument, mbrType)

              def tryConversionForSelection(using Context) =
                val converted = tryConversion
                if !ctx.reporter.hasErrors && !selProto.isMatchedBy(converted.tpe) then
                  // this check is needed since adapting relative to a `SelectionProto` can succeed
                  // even if the term is not a subtype of the `SelectionProto`
                  err.typeMismatch(converted, selProto)
                converted

              if cand.isExtension && cand.isConversion then
                val extensionCtx, conversionCtx = ctx.fresh.setNewTyperState()
                val extensionResult = tryExtension(using extensionCtx)
                val conversionResult = tryConversionForSelection(using conversionCtx)
                if !extensionCtx.reporter.hasErrors then
                  extensionCtx.typerState.commit()
                  if !conversionCtx.reporter.hasErrors then
                    report.error(em"ambiguous implicit: $generated is eligible both as an implicit conversion and as an extension method container")
                  extensionResult
                else
                  conversionCtx.typerState.commit()
                  conversionResult
              else if cand.isExtension then tryExtension
              else tryConversionForSelection
            case _ =>
              tryConversion
        }
      if ctx.reporter.hasErrors
         || !cand.ref.symbol.isAccessibleFrom(cand.ref.prefix)
      then
        val res = adapted.tpe match {
          case _: SearchFailureType => SearchFailure(adapted)
          case error: PreviousErrorType if !adapted.symbol.isAccessibleFrom(cand.ref.prefix) =>
            SearchFailure(adapted.withType(new NestedFailure(error.msg, pt)))
          case tpe =>
            // Special case for `$conforms` and `<:<.refl`. Showing them to the users brings
            // no value, so we instead report a `NoMatchingImplicitsFailure`
            if (adapted.symbol == defn.Predef_conforms || adapted.symbol == defn.SubType_refl)
              NoMatchingImplicitsFailure
            else if Splicer.inMacroExpansion && tpe <:< pt then
              SearchFailure(adapted.withType(new MacroErrorsFailure(ctx.reporter.allErrors.reverse, pt, argument)))
            else
              SearchFailure(adapted.withType(new MismatchedImplicit(ref, pt, argument)))
        }
        ctx.reporter.removeBufferedMessages
        res
      else
        SearchSuccess(adapted, ref, cand.level, cand.isExtension)(ctx.typerState, ctx.gadt)
    }

  /** An implicit search; parameters as in `inferImplicit` */
  class ImplicitSearch(protected val pt: Type, protected val argument: Tree, span: Span)(using Context):
    assert(argument.isEmpty || argument.tpe.isValueType || argument.tpe.isInstanceOf[ExprType],
        em"found: $argument: ${argument.tpe}, expected: $pt")

    private def nestedContext() =
      ctx.fresh.setMode(ctx.mode &~ Mode.ImplicitsEnabled)

    private def isCoherent = pt.isRef(defn.CanEqualClass)

    private val wideProto = pt.widenExpr

    private val srcPos = ctx.source.atSpan(span)

    /** The expected type where parameters and uninstantiated typevars are replaced by wildcard types */
    private val wildProto: Type =
      if argument.isEmpty then wildApprox(pt)
      else ViewProto(wildApprox(argument.tpe.widen.normalized), wildApprox(pt))
        // Not clear whether we need to drop the `.widen` here. All tests pass with it in place, though.

    private val isNotGiven: Boolean = wildProto.classSymbol == defn.NotGivenClass

    private def searchTooLarge(): Boolean = ctx.searchHistory match
      case root: SearchRoot =>
        root.nestedSearches = 1
        false
      case h =>
        val limit = ctx.settings.XimplicitSearchLimit.value
        val nestedSearches = h.root.nestedSearches
        val result = nestedSearches > limit
        if result then
          var c = ctx
          while c.outer.typer eq ctx.typer do c = c.outer
          report.warning(ImplicitSearchTooLargeWarning(limit, h.openSearchPairs), srcPos)(using c)
        else
          h.root.nestedSearches = nestedSearches + 1
        result

    /** Try to type-check implicit reference, after checking that this is not
      * a diverging search
      */
    def tryImplicit(cand: Candidate, contextual: Boolean): SearchResult =
      if checkDivergence(cand) then
        SearchFailure(new DivergingImplicit(cand.ref, wideProto, argument), span)
      else if searchTooLarge() then
        ImplicitSearchTooLargeFailure
      else
        val history = ctx.searchHistory.nest(cand, pt)
        val typingCtx =
          nestedContext().setNewTyperState().setFreshGADTBounds.setSearchHistory(history)
        val result = typedImplicit(cand, pt, argument, span)(using typingCtx)
        result match
          case res: SearchSuccess =>
            ctx.searchHistory.defineBynameImplicit(wideProto, res)
          case _ =>
            // Since the search failed, the local typerstate will be discarded
            // without being committed, but type variables local to that state
            // might still appear in an error message, so we run `gc()` here to
            // make sure we don't forget their instantiation. This leads to more
            // precise error messages in tests/neg/missing-implicit3.check and
            // tests/neg/implicitSearch.check
            typingCtx.typerState.gc()
            result

    /** Search a list of eligible implicit references */
    private def searchImplicit(eligible: List[Candidate], contextual: Boolean): SearchResult =

      /** Compare `alt1` with `alt2` to determine which one should be chosen.
       *
       *  @return  a number > 0   if `alt1` is preferred over `alt2`
       *           a number < 0   if `alt2` is preferred over `alt1`
       *           0              if neither alternative is preferred over the other
       */
      def compareAlternatives(alt1: RefAndLevel, alt2: RefAndLevel): Int =
        if alt1.ref eq alt2.ref then 0
        else if alt1.level != alt2.level then alt1.level - alt2.level
        else explore(compare(alt1.ref, alt2.ref))(using nestedContext())

      /** If `alt1` is also a search success, try to disambiguate as follows:
       *    - If alt2 is preferred over alt1, pick alt2, otherwise return an
       *      ambiguous implicits error.
       */
      def disambiguate(alt1: SearchResult, alt2: SearchSuccess) = alt1 match
        case alt1: SearchSuccess =>
          var diff = compareAlternatives(alt1, alt2)
          assert(diff <= 0)   // diff > 0 candidates should already have been eliminated in `rank`
          if diff == 0 && alt2.isExtension then
            if alt1.isExtension then
              // Fall back: if both results are extension method applications,
              // compare the extension methods instead of their wrappers.
              def stripExtension(alt: SearchSuccess) = methPart(stripApply(alt.tree)).tpe
              (stripExtension(alt1), stripExtension(alt2)) match
                case (ref1: TermRef, ref2: TermRef) =>
                  // ref1 and ref2 might refer to type variables owned by
                  // alt1.tstate and alt2.tstate respectively, to compare the
                  // alternatives correctly we need a TyperState that includes
                  // constraints from both sides, see
                  // tests/*/extension-specificity2.scala for test cases.
                  val constraintsIn1 = alt1.tstate.constraint ne ctx.typerState.constraint
                  val constraintsIn2 = alt2.tstate.constraint ne ctx.typerState.constraint
                  def exploreState(alt: SearchSuccess): TyperState =
                    alt.tstate.fresh(committable = false)
                  val comparisonState =
                    if constraintsIn1 && constraintsIn2 then
                      exploreState(alt1).mergeConstraintWith(alt2.tstate)
                    else if constraintsIn1 then
                      exploreState(alt1)
                    else if constraintsIn2 then
                      exploreState(alt2)
                    else
                      ctx.typerState

                  diff = inContext(ctx.withTyperState(comparisonState)):
                    compare(ref1, ref2)
            else // alt1 is a conversion, prefer extension alt2 over it
              diff = -1
          if diff < 0 then alt2
          else if diff > 0 then alt1
          else SearchFailure(new AmbiguousImplicits(alt1, alt2, pt, argument), span)
        case _: SearchFailure => alt2

      /** Try to find a best matching implicit term among all the candidates in `pending`.
       *  @param pending   The list of candidates that remain to be tested
       *  @param found     The result obtained from previously tried candidates
       *  @param rfailures A list of all failures from previously tried candidates in reverse order
       *
       *  The scheme is to try candidates one-by-one. If a trial is successful:
       *   - if the query term is a `Not[T]` treat it a failure,
       *   - otherwise, if a previous search was also successful, handle the ambiguity
       *     in `disambiguate`,
       *   - otherwise, continue the search with all candidates that are not strictly
       *     worse than the successful candidate.
       *  If a trial failed:
       *    - if the query term is a `Not[T]` treat it as a success,
       *    - otherwise, if the failure is an ambiguity, try to heal it (see `healAmbiguous`)
       *      and return an ambiguous error otherwise. However, under Scala2 mode this is
       *      treated as a simple failure, with a warning that semantics will change.
       *    - otherwise add the failure to `rfailures` and continue testing the other candidates.
       */
      def rank(pending: List[Candidate], found: SearchResult, rfailures: List[SearchFailure]): SearchResult =
        pending match {
          case cand :: remaining =>
            /** To recover from an ambiguous implicit failure, we need to find a pending
             *  candidate that is strictly better than the failed candidate(s).
             *  If no such candidate is found, we propagate the ambiguity.
             */
            def healAmbiguous(fail: SearchFailure, betterThanFailed: Candidate => Boolean) =
              val newPending = remaining.filter(betterThanFailed)
              rank(newPending, fail, Nil).recoverWith(_ => fail)

            negateIfNot(tryImplicit(cand, contextual)) match {
              case fail: SearchFailure =>
                if fail eq ImplicitSearchTooLargeFailure then
                  fail
                else if (fail.isAmbiguous)
                  if migrateTo3 then
                    val result = rank(remaining, found, NoMatchingImplicitsFailure :: rfailures)
                    if (result.isSuccess)
                      warnAmbiguousNegation(fail.reason.asInstanceOf[AmbiguousImplicits])
                    result
                  else
                    // The ambiguity happened in a nested search: to recover we
                    // need a candidate better than `cand`
                    healAmbiguous(fail, newCand =>
                      compareAlternatives(newCand, cand) > 0)
                else rank(remaining, found, fail :: rfailures)
              case best: SearchSuccess =>
                if (ctx.mode.is(Mode.ImplicitExploration) || isCoherent)
                  best
                else disambiguate(found, best) match {
                  case retained: SearchSuccess =>
                    val newPending =
                      if (retained eq found) || remaining.isEmpty then remaining
                      else remaining.filterConserve(cand =>
                        compareAlternatives(retained, cand) <= 0)
                    rank(newPending, retained, rfailures)
                  case fail: SearchFailure =>
                    // The ambiguity happened in the current search: to recover we
                    // need a candidate better than the two ambiguous alternatives.
                    val ambi = fail.reason.asInstanceOf[AmbiguousImplicits]
                    healAmbiguous(fail, newCand =>
                      compareAlternatives(newCand, ambi.alt1) > 0 &&
                      compareAlternatives(newCand, ambi.alt2) > 0)
                }
            }
          case nil =>
            if (rfailures.isEmpty) found
            else found.recoverWith(_ => rfailures.reverse.maxBy(_.tree.treeSize))
        }

      def negateIfNot(result: SearchResult) =
        if (isNotGiven)
          result match {
            case _: SearchFailure =>
              SearchSuccess(ref(defn.NotGiven_value), defn.NotGiven_value.termRef, 0)(
                ctx.typerState.fresh().setCommittable(true),
                ctx.gadt
              )
            case _: SearchSuccess =>
              NoMatchingImplicitsFailure
          }
        else result

      def warnAmbiguousNegation(ambi: AmbiguousImplicits) =
        report.migrationWarning(
          em"""Ambiguous implicits ${ambi.alt1.ref.symbol.showLocated} and ${ambi.alt2.ref.symbol.showLocated}
              |seem to be used to implement a local failure in order to negate an implicit search.
              |According to the new implicit resolution rules this is no longer possible;
              |the search will fail with a global ambiguity error instead.
              |
              |Consider using the scala.util.NotGiven class to implement similar functionality.""",
          srcPos)

      /** Compare the length of the baseClasses of two symbols (except for objects,
       *  where we use the length of the companion class instead if it's bigger).
       *
       *  This relation is meant to approximate `Applications#compareOwner` while also
       *  inducing a total ordering: `compareOwner` returns `0` for unrelated symbols
       *  and therefore only induces a partial ordering, meaning it cannot be used
       *  as a sorting function (see `java.util.Comparator#compare`).
       */
      def compareBaseClassesLength(sym1: Symbol, sym2: Symbol): Int =
        def len(sym: Symbol) =
          if sym.is(ModuleClass) && sym.companionClass.exists then
            Math.max(sym.asClass.baseClassesLength, sym.companionClass.asClass.baseClassesLength)
          else if sym.isClass then
            sym.asClass.baseClassesLength
          else
            0
        len(sym1) - len(sym2)

      /** A relation that influences the order in which eligible implicits are tried.
       *
       *  We prefer (in order of importance)
       *   1. more deeply nested definitions
       *   2. definitions with fewer implicit parameters
       *   3. definitions whose owner has more parents (see `compareBaseClassesLength`)
       *  The reason for (2) is that we want to fail fast if the search type
       *  is underconstrained. So we look for "small" goals first, because that
       *  will give an ambiguity quickly.
       */
      def compareEligibles(e1: Candidate, e2: Candidate): Int =
        if e1 eq e2 then return 0
        val cmpLevel = e1.level - e2.level
        if cmpLevel != 0 then return -cmpLevel // 1.
        val sym1 = e1.ref.symbol
        val sym2 = e2.ref.symbol
        val arity1 = sym1.info.firstParamTypes.length
        val arity2 = sym2.info.firstParamTypes.length
        val cmpArity = arity1 - arity2
        if cmpArity != 0 then return cmpArity // 2.
        val cmpBcs = compareBaseClassesLength(sym1.owner, sym2.owner)
        -cmpBcs // 3.

      /** Check if `ord` respects the contract of `Ordering`.
       *
       *  More precisely, we check that its `compare` method respects the invariants listed
       *  in https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html#compare-T-T-
       */
      def validateOrdering(ord: Ordering[Candidate]): Unit =
        for
          x <- eligible
          y <- eligible
          cmpXY = Integer.signum(ord.compare(x, y))
          cmpYX = Integer.signum(ord.compare(y, x))
          z <- eligible
          cmpXZ = Integer.signum(ord.compare(x, z))
          cmpYZ = Integer.signum(ord.compare(y, z))
        do
          def reportViolation(msg: String): Unit =
            Console.err.println(s"Internal error: comparison function violated ${msg.stripMargin}")
          def showCandidate(c: Candidate): String =
            s"$c (${c.ref.symbol.showLocated})"

          if cmpXY != -cmpYX then
            reportViolation(
              s"""signum(cmp(x, y)) == -signum(cmp(y, x)) given:
                 |x = ${showCandidate(x)}
                 |y = ${showCandidate(y)}
                 |cmpXY = $cmpXY
                 |cmpYX = $cmpYX""")
          if cmpXY != 0 && cmpXY == cmpYZ && cmpXZ != cmpXY then
            reportViolation(
              s"""transitivity given:
                 |x = ${showCandidate(x)}
                 |y = ${showCandidate(y)}
                 |z = ${showCandidate(z)}
                 |cmpXY = $cmpXY
                 |cmpXZ = $cmpXZ
                 |cmpYZ = $cmpYZ""")
          if cmpXY == 0 && cmpXZ != cmpYZ then
            reportViolation(
              s"""cmp(x, y) == 0 implies that signum(cmp(x, z)) == signum(cmp(y, z)) given:
                 |x = ${showCandidate(x)}
                 |y = ${showCandidate(y)}
                 |z = ${showCandidate(z)}
                 |cmpXY = $cmpXY
                 |cmpXZ = $cmpXZ
                 |cmpYZ = $cmpYZ""")
      end validateOrdering

      /** Sort list of implicit references according to `compareEligibles`.
       *  This is just an optimization that aims at reducing the average
       *  number of candidates to be tested.
       */
      def sort(eligible: List[Candidate]) = eligible match
        case Nil => eligible
        case e1 :: Nil => eligible
        case e1 :: e2 :: Nil =>
          if compareEligibles(e2, e1) < 0 then e2 :: e1 :: Nil
          else eligible
        case _ =>
          val ord: Ordering[Candidate] = (a, b) => compareEligibles(a, b)
          try eligible.sorted(using ord)
          catch case ex: IllegalArgumentException =>
            // This exception being thrown probably means that our comparison
            // function is broken, check if that's the case
            validateOrdering(ord)
            throw ex

      rank(sort(eligible), NoMatchingImplicitsFailure, Nil)
    end searchImplicit

    def isUnderSpecifiedArgument(tp: Type): Boolean =
      tp.isRef(defn.NothingClass) || tp.isRef(defn.NullClass) || (tp eq NoPrefix)

    private def isUnderspecified(tp: Type): Boolean = tp.stripTypeVar match
      case tp: WildcardType =>
        !tp.optBounds.exists || isUnderspecified(tp.optBounds.hiBound)
      case tp: ViewProto =>
        isUnderspecified(tp.resType)
        || tp.resType.isRef(defn.UnitClass)
        || isUnderSpecifiedArgument(tp.argType.widen)
      case _ =>
        tp.isAny || tp.isAnyRef

    /** Search implicit in context `ctxImplicits` or else in implicit scope
     *  of expected type if `ctxImplicits == null`.
     */
    private def searchImplicit(ctxImplicits: ContextualImplicits | Null, mode: SearchMode): SearchResult =
      if isUnderspecified(wildProto) then
        SearchFailure(TooUnspecific(pt), span)
      else
        val contextual = ctxImplicits != null
        val preEligible = // the eligible candidates, ignoring positions
          if ctxImplicits != null then
            if ctx.gadt.isNarrowing then
              withoutMode(Mode.ImplicitsEnabled) {
                ctxImplicits.uncachedEligible(wildProto)
              }
            else ctxImplicits.eligible(wildProto)
          else implicitScope(wildProto).eligible

        /** Does candidate `cand` come too late for it to be considered as an
         *  eligible candidate? This is the case if `cand` appears in the same
         *  scope as a given definition of the form `given ... = ...` that
         *  encloses the search point and `cand` comes later in the source or
         *  coincides with that given definition.
         */
        def comesTooLate(cand: Candidate): Boolean =
          val candSym = cand.ref.symbol
          def candSucceedsGiven(sym: Symbol): Boolean =
            val owner = sym.owner
            if owner == candSym.owner then
              sym.is(GivenVal) && sym.span.exists && sym.span.start <= candSym.span.start
            else if owner.isClass then false
            else candSucceedsGiven(owner)

          ctx.isTyper
          && !candSym.isOneOf(TermParamOrAccessor | Synthetic)
          && candSym.span.exists
          && candSucceedsGiven(ctx.owner)
        end comesTooLate

        val eligible = // the eligible candidates that come before the search point
          if contextual && mode != SearchMode.Old
          then preEligible.filterNot(comesTooLate)
          else preEligible

        def checkResolutionChange(result: SearchResult) =
          if (eligible ne preEligible) && mode != SearchMode.New then
            searchImplicit(preEligible, contextual) match
              case prevResult: SearchSuccess =>
                def remedy = pt match
                  case _: SelectionProto =>
                    "conversion,\n  - use an import to get extension method into scope"
                  case _: ViewProto =>
                    "conversion"
                  case _ =>
                    "argument"

                def showResult(r: SearchResult) = r match
                  case r: SearchSuccess => ctx.printer.toTextRef(r.ref).show
                  case r => r.show

                result match
                  case result: SearchSuccess if prevResult.ref frozen_=:= result.ref =>
                    // OK
                  case _ =>
                    val msg =
                      em"""Result of implicit search for $pt will change.
                          |Current result ${showResult(prevResult)} will be no longer eligible
                          |  because it is not defined before the search position.
                          |Result with new rules: ${showResult(result)}.
                          |To opt into the new rules, compile with `-source future` or use
                          |the `scala.language.future` language import.
                          |
                          |To fix the problem without the language import, you could try one of the following:
                          |  - use a `given ... with` clause as the enclosing given,
                          |  - rearrange definitions so that ${showResult(prevResult)} comes earlier,
                          |  - use an explicit $remedy."""
                    if mode == SearchMode.CompareErr
                    then report.error(msg, srcPos)
                    else report.warning(msg.append("\nThis will be an error in Scala 3.5 and later."), srcPos)
                prevResult
              case prevResult: SearchFailure => result
          else result
        end checkResolutionChange

        checkResolutionChange:
          searchImplicit(eligible, contextual).recoverWith:
            case failure: SearchFailure =>
              failure.reason match
                case _: AmbiguousImplicits => failure
                case reason =>
                  if contextual then
                    // If we filtered out some candidates for being too late, we should
                    // do another contextual search further out, since the dropped candidates
                    // might have shadowed an eligible candidate in an outer level.
                    // Otherwise, proceed with a search of the implicit scope.
                    val newCtxImplicits =
                      if eligible eq preEligible then null
                      else ctxImplicits.nn.outerImplicits: ContextualImplicits | Null
                        // !!! Dotty problem: without the ContextualImplicits | Null type ascription
                        // we get a Ycheck failure after arrayConstructors due to "Types differ"
                    searchImplicit(newCtxImplicits, SearchMode.New).recoverWith:
                      failure2 => failure2.reason match
                        case _: AmbiguousImplicits => failure2
                        case _ =>
                          reason match
                            case (_: DivergingImplicit) => failure
                            case _ => List(failure, failure2).maxBy(_.tree.treeSize)
                  else failure
    end searchImplicit

    /** Find a unique best implicit reference */
    def bestImplicit: SearchResult =
      // Before searching for contextual or implicit scope candidates we first check if
      // there is an under construction or already constructed term with which we can tie
      // the knot.
      //
      // Since any suitable term found is defined as part of this search it will always be
      // effectively in a more inner context than any other definition provided by
      // explicit definitions. Consequently these terms have the highest priority and no
      // other candidates need to be considered.
      recursiveRef match
        case ref: TermRef =>
          SearchSuccess(tpd.ref(ref).withSpan(span.startPos), ref, 0)(ctx.typerState, ctx.gadt)
        case _ =>
          searchImplicit(ctx.implicits,
            if sourceVersion.isAtLeast(SourceVersion.future) then SearchMode.New
            else if sourceVersion.isAtLeast(SourceVersion.`3.5`) then SearchMode.CompareErr
            else if sourceVersion.isAtLeast(SourceVersion.`3.4`) then SearchMode.CompareWarn
            else SearchMode.Old)
    end bestImplicit

    def implicitScope(tp: Type): OfTypeImplicits = ctx.run.nn.implicitScope(tp)

    /** All available implicits, without ranking */
    def allImplicits: Set[SearchSuccess] = {
      val contextuals = ctx.implicits.eligible(wildProto).map(tryImplicit(_, contextual = true))
      val inscope = implicitScope(wildProto).eligible.map(tryImplicit(_, contextual = false))
      (contextuals.toSet ++ inscope).collect {
        case success: SearchSuccess => success
      }
    }

    /** Fields needed for divergence checking */
    @threadUnsafe lazy val ptCoveringSet = wideProto.coveringSet
    @threadUnsafe lazy val ptSize = wideProto.typeSize
    @threadUnsafe lazy val wildPt = wildApprox(wideProto)

    /**
    * Check if the supplied candidate implicit and target type indicate a diverging
    * implicit search.
    *
    * @param cand The candidate implicit to be explored.
    * @result     True if this candidate/pt are divergent, false otherwise.
    */
    def checkDivergence(cand: Candidate): Boolean =
      // For full details of the algorithm see the SIP:
      //   https://docs.scala-lang.org/sips/byname-implicits.html
      Stats.record("checkDivergence")

      // Unless we are able to tie a recursive knot, we report divergence if there is an
      // open implicit using the same candidate implicit definition which has a type which
      // is larger (see `typeSize`) and is constructed using the same set of types and type
      // constructors (see `coveringSet`).
      //
      // We are able to tie a recursive knot if there is compatible term already under
      // construction which is separated from this context by at least one by name argument
      // as we ascend the chain of open implicits to the outermost search context.

      @tailrec
      def loop(history: SearchHistory, belowByname: Boolean): Boolean =
        history match
          case prev @ OpenSearch(cand1, tp, outer) =>
            if cand1.ref eq cand.ref then
              lazy val wildTp = wildApprox(tp.widenExpr)
              if belowByname && (wildTp <:< wildPt) then
                fullyDefinedType(tp, "by-name implicit parameter", srcPos)
                false
              else if prev.typeSize > ptSize || prev.coveringSet != ptCoveringSet then
                loop(outer, tp.isByName || belowByname)
              else
                prev.typeSize < ptSize
                || wildTp =:= wildPt
                || loop(outer, tp.isByName || belowByname)
            else loop(outer, tp.isByName || belowByname)
          case _ => false

      loop(ctx.searchHistory, pt.isByName)
    end checkDivergence

    /**
     * Return the reference, if any, to a term under construction or already constructed in
     * the current search history corresponding to the supplied target type.
     *
     * A term is eligible if its type is a subtype of the target type and either it has
     * already been constructed and is present in the current implicit dictionary, or it is
     * currently under construction and is separated from the current search context by at
     * least one by name argument position.
     *
     * Note that because any suitable term found is defined as part of this search it will
     * always be effectively in a more inner context than any other definition provided by
     * explicit definitions. Consequently these terms have the highest priority and no other
     * candidates need to be considered.
     *
     * @param pt  The target type being searched for.
     * @result    The corresponding dictionary reference if any, NoType otherwise.
     */
    def recursiveRef: Type =
      val found = ctx.searchHistory.refBynameImplicit(wideProto)
      if found.exists then
        found
      else if !ctx.searchHistory.byname && !pt.isByName then
        NoType // No recursion unless at least one open implicit is by name ...
      else
        // We are able to tie a recursive knot if there is compatible term already under
        // construction which is separated from this context by at least one by name
        // argument as we ascend the chain of open implicits to the outermost search
        // context.
        @tailrec
        def loop(history: SearchHistory, belowByname: Boolean): Type =
          history match
            case OpenSearch(cand, tp, outer) =>
              if (belowByname || tp.isByName) && tp.widenExpr <:< wideProto then tp
              else loop(outer, belowByname || tp.isByName)
            case _ => NoType

        loop(ctx.searchHistory, pt.isByName) match
          case NoType => NoType
          case tp => ctx.searchHistory.linkBynameImplicit(tp.widenExpr)
    end recursiveRef
  end ImplicitSearch
end Implicits

/**
 * Records the history of currently open implicit searches.
 *
 * A search history maintains a list of open implicit searches (`openSearchPairs`) a shortcut flag
 * indicating whether any of these are by name (`byname`) and a reference to the root
 * search history (`root`) which in turn maintains a possibly empty dictionary of
 * recursive implicit terms constructed during this search.
 *
 * A search history provides operations to create a nested search history, check for
 * divergence, enter by name references and definitions in the implicit dictionary, lookup
 * recursive references and emit a complete implicit dictionary when the outermost search
 * is complete.
 */
abstract class SearchHistory:
  val root: SearchRoot
  /** Does this search history contain any by name implicit arguments. */
  val byname: Boolean
  def openSearchPairs: List[(Candidate, Type)]

  /**
   * Create the state for a nested implicit search.
   * @param cand The candidate implicit to be explored.
   * @param pt   The target type for the above candidate.
   * @result     The nested history.
   */
  def nest(cand: Candidate, pt: Type)(using Context): OpenSearch = OpenSearch(cand, pt, this)

  // The following are delegated to the root of this search history.
  def linkBynameImplicit(tpe: Type)(using Context): TermRef =
    root.linkBynameImplicit(tpe)
  def refBynameImplicit(tpe: Type)(using Context): Type =
    root.refBynameImplicit(tpe)
  def defineBynameImplicit(tpe: Type, result: SearchSuccess)(using Context): SearchResult =
    root.defineBynameImplicit(tpe, result)

  // This is NOOP unless at the root of this search history.
  def emitDictionary(span: Span, result: SearchResult)(using Context): SearchResult = result

  override def toString: String = s"SearchHistory(open = $openSearchPairs, byname = $byname)"
end SearchHistory

case class OpenSearch(cand: Candidate, pt: Type, outer: SearchHistory)(using Context) extends SearchHistory:
  val root = outer.root
  val byname = outer.byname || pt.isByName
  def openSearchPairs = (cand, pt) :: outer.openSearchPairs

  // The typeSize and coveringSet of the current search.
  // Note: It is important to cache size and covering sets since types
  // in search histories can contain type variables that can be instantiated
  // by nested implicit searches, thus leading to types in search histories
  // that grow larger the deeper the search gets. This can mask divergence.
  // An example is in neg/9504.scala
  lazy val typeSize = pt.typeSize
  lazy val coveringSet = pt.coveringSet
end OpenSearch

/**
 * The state corresponding to the outermost context of an implicit searcch.
 */
final class SearchRoot extends SearchHistory:
  val root = this
  val byname = false
  def openSearchPairs = Nil

  /** How many expressions were constructed so far in the current toplevel implicit search?
   */
  var nestedSearches: Int = 0

  /** The dictionary of recursive implicit types and corresponding terms for this search. */
  var myImplicitDictionary: mutable.Map[Type, (TermRef, tpd.Tree)] | Null = null
  private def implicitDictionary =
    if myImplicitDictionary == null then
      myImplicitDictionary = mutable.Map.empty[Type, (TermRef, tpd.Tree)]
    myImplicitDictionary.uncheckedNN

  /**
   * Link a reference to an under-construction implicit for the provided type to its
   * defining occurrence via the implicit dictionary, creating a dictionary entry for this
   * type if one does not yet exist.
   *
   * @param tpe  The type to link.
   * @result     The TermRef of the corresponding dictionary entry.
   */
  override def linkBynameImplicit(tpe: Type)(using Context): TermRef =
    implicitDictionary.get(tpe) match {
      case Some((ref, _)) => ref
      case None =>
        val lazyImplicit = newLazyImplicit(tpe)
        val ref = lazyImplicit.termRef
        implicitDictionary.put(tpe, (ref, tpd.EmptyTree))
        ref
    }

  /**
   * Look up an implicit dictionary entry by type.
   *
   * If present yield the TermRef corresponding to the eventual dictionary entry,
   * otherwise NoType.
   *
   * @param tpe The type to look up.
   * @result    The corresponding TermRef, or NoType if none.
   */
  override def refBynameImplicit(tpe: Type)(using Context): Type =
    implicitDictionary.get(tpe) match
      case Some((tp, _)) => tp
      case None => NoType

  /**
   * Define a pending dictionary entry if any.
   *
   * If the provided type corresponds to an under-construction by name implicit, then use
   * the tree contained in the provided SearchSuccess as its definition, returning an
   * updated result referring to dictionary entry. Otherwise return the SearchSuccess
   * unchanged.
   *
   * @param  tpe    The type for which the entry is to be defined
   * @param  result The SearchSuccess corresponding to tpe
   * @result        A SearchResult referring to the newly created dictionary entry if tpe
   *                is an under-construction by name implicit, the provided result otherwise.
   */
  override def defineBynameImplicit(tpe: Type, result: SearchSuccess)(using Context): SearchResult =
    implicitDictionary.get(tpe) match {
      case Some((ref, _)) =>
        implicitDictionary.put(tpe, (ref, result.tree))
        SearchSuccess(tpd.ref(ref).withSpan(result.tree.span), result.ref, result.level)(result.tstate, result.gstate)
      case None => result
    }

  /**
   * Emit the implicit dictionary at the completion of an implicit search.
   *
   * @param span   The position at which the search is elaborated.
   * @param result The result of the search prior to substitution of recursive references.
   * @result       The elaborated result, comprising the implicit dictionary and a result tree
   *               substituted with references into the dictionary.
   */
  override def emitDictionary(span: Span, result: SearchResult)(using Context): SearchResult =
    if (myImplicitDictionary == null || implicitDictionary.isEmpty) result
    else
      result match {
        case failure: SearchFailure => failure
        case success: SearchSuccess =>
          import tpd.*

          // We might have accumulated dictionary entries for by name implicit arguments
          // which are not in fact used recursively either directly in the outermost result
          // term, or indirectly via other dictionary entries. We prune these out, recursively
          // eliminating entries until all remaining entries are at least transtively referred
          // to in the outermost result term.
          @tailrec
          def prune(trees: List[Tree], pending: List[(TermRef, Tree)], acc: List[(TermRef, Tree)]): List[(TermRef, Tree)] = pending match {
            case Nil => acc
            case ps =>
              val (in, out) = ps.partition {
                case (vref, rhs) =>
                  trees.exists(_.existsSubTree {
                    case id: Ident => id.symbol == vref.symbol
                    case _ => false
                  })
              }
              if (in.isEmpty) acc
              else prune(in.map(_._2) ++ trees, out, in ++ acc)
          }

          val pruned = prune(List(success.tree), implicitDictionary.map(_._2).toList, Nil)
          myImplicitDictionary = null
          if (pruned.isEmpty) result
          else if (pruned.exists(_._2 == EmptyTree)) NoMatchingImplicitsFailure
          else {
            // If there are any dictionary entries remaining after pruning, construct a dictionary
            // class of the form,
            //
            // class <dictionary> {
            //   val $_lazy_implicit_$0 = ...
            //   ...
            //   val $_lazy_implicit_$n = ...
            // }
            //
            // Where the RHSs of the $_lazy_implicit_$n are the terms used to populate the dictionary
            // via defineByNameImplicit.
            //
            // The returned search result is then of the form,
            //
            // {
            //   class <dictionary> { ... }
            //   val $_lazy_implicit_$nn = new <dictionary>
            //   result.tree // with dictionary references substituted in
            // }

            val parents = List(defn.ObjectType, defn.SerializableType)
            val classSym = newNormalizedClassSymbol(ctx.owner, LazyImplicitName.fresh().toTypeName, Synthetic | Final, parents, coord = span)
            val vsyms = pruned.map(_._1.symbol)
            val nsyms = vsyms.map(vsym => newSymbol(classSym, vsym.name, EmptyFlags, vsym.info, coord = span).entered)
            val vsymMap = (vsyms zip nsyms).toMap

            val rhss = pruned.map(_._2)
            // Substitute dictionary references into dictionary entry RHSs
            val rhsMap = new TreeTypeMap(treeMap = {
              case id: Ident if vsymMap.contains(id.symbol) =>
                tpd.ref(vsymMap(id.symbol))(using ctx.withSource(id.source)).withSpan(id.span)
              case tree => tree
            })
            val nrhss = rhss.map(rhsMap(_))

            val vdefs = (nsyms zip nrhss) map {
              case (nsym, nrhs) => ValDef(nsym.asTerm, nrhs.changeNonLocalOwners(nsym))
            }

            val constr = newConstructor(classSym, Synthetic, Nil, Nil).entered
            val classDef = ClassDef(classSym, DefDef(constr), vdefs)

            val valSym = newLazyImplicit(classSym.typeRef, span)
            val inst = ValDef(valSym, New(classSym.typeRef, Nil))

            // Substitute dictionary references into outermost result term.
            val resMap = new TreeTypeMap(treeMap = {
              case id: Ident if vsymMap.contains(id.symbol) =>
                Select(tpd.ref(valSym), id.name)
              case tree => tree
            })

            val res = resMap(success.tree)

            val blk = Block(classDef :: inst :: Nil, res).withSpan(span)

            success.copy(tree = blk)(success.tstate, success.gstate)
          }
      }
end SearchRoot

/** A set of term references where equality is =:= */
sealed class TermRefSet(using Context):
  private val elems = new java.util.LinkedHashMap[TermSymbol, Type | List[Type]]

  def isEmpty = elems.size == 0

  def += (ref: TermRef): Unit =
    val pre = ref.prefix
    if ref.symbol.exists then
      val sym = ref.symbol.asTerm
      elems.get(sym) match
        case null =>
          elems.put(sym, pre)
        case prefix: Type =>
          if !(prefix =:= pre) then elems.put(sym, pre :: prefix :: Nil)
        case prefixes: List[Type] =>
          if !prefixes.exists(_ =:= pre) then elems.put(sym, pre :: prefixes)

  def ++= (that: TermRefSet): Unit =
    if !that.isEmpty then that.foreach(+=)

  def foreach[U](f: TermRef => U): Unit =
    def handle(sym: TermSymbol | Null, prefixes: Type | List[Type] | Null): Unit =
      // We cannot use `.nn` here due to inference issue.
      val prefixes0: Type | List[Type] = prefixes.uncheckedNN
      prefixes0 match
        case prefix: Type => f(TermRef(prefix, sym.uncheckedNN))
        case prefixes: List[Type] => prefixes.foreach(pre => f(TermRef(pre, sym.uncheckedNN)))
    elems.forEach(handle)

  // used only for debugging
  def showAsList: List[TermRef] = {
    val buffer = new mutable.ListBuffer[TermRef]
    foreach(tr => buffer += tr)
    buffer.toList
  }

  override def toString = showAsList.toString

object TermRefSet:

  @sharable val empty = new TermRefSet(using NoContext):
    override def += (ref: TermRef): Unit = throw UnsupportedOperationException("+=")

end TermRefSet
