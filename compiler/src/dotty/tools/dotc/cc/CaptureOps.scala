package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import Names.TermName
import ast.{tpd, untpd}
import Decorators.*, NameOps.*
import config.SourceVersion
import config.Printers.capt
import util.Property.Key
import tpd.*
import StdNames.nme
import config.Feature
import collection.mutable

private val Captures: Key[CaptureSet] = Key()

object ccConfig:

  /** If true, allow mappping capture set variables under captureChecking with maps that are neither
   *  bijective nor idempotent. We currently do now know how to do this correctly in all
   *  cases, though.
   */
  inline val allowUnsoundMaps = false

  /** If true, use `sealed` as encapsulation mechanism instead of the
   *  previous global retriction that `cap` can't be boxed or unboxed.
   */
  def allowUniversalInBoxed(using Context) =
    Feature.sourceVersion.isAtLeast(SourceVersion.`3.3`)
end ccConfig


/** Are we at checkCaptures phase? */
def isCaptureChecking(using Context): Boolean =
  ctx.phaseId == Phases.checkCapturesPhase.id

/** Are we at checkCaptures or Setup phase? */
def isCaptureCheckingOrSetup(using Context): Boolean =
  val ccId = Phases.checkCapturesPhase.id
  val ctxId = ctx.phaseId
  ctxId == ccId || ctxId == ccId - 1

/** A dependent function type with given arguments and result type
 *  TODO Move somewhere else where we treat all function type related ops together.
 */
def depFun(args: List[Type], resultType: Type, isContextual: Boolean, paramNames: List[TermName] = Nil)(using Context): Type =
  val make = MethodType.companion(isContextual = isContextual)
  val mt =
    if paramNames.length == args.length then make(paramNames, args, resultType)
    else make(args, resultType)
  mt.toFunctionType(alwaysDependent = true)

/** An exception thrown if a @retains argument is not syntactically a CaptureRef */
class IllegalCaptureRef(tpe: Type) extends Exception(tpe.toString)

/** Capture checking state, which is known to other capture checking components */
class CCState:

  /** The last pair of capture reference and capture set where
   *  the reference could not be added to the set due to a level conflict.
   */
  var levelError: Option[CaptureSet.CompareResult.LevelError] = None

end CCState

/** The currently valid CCState */
def ccState(using Context) =
  Phases.checkCapturesPhase.asInstanceOf[CheckCaptures].ccState

class NoCommonRoot(rs: Symbol*)(using Context) extends Exception(
  i"No common capture root nested in ${rs.mkString(" and ")}"
)

extension (tree: Tree)

  /** Map tree with CaptureRef type to its type, throw IllegalCaptureRef otherwise */
  def toCaptureRef(using Context): CaptureRef = tree match
    case ReachCapabilityApply(arg) =>
      arg.toCaptureRef.reach
    case _ => tree.tpe match
      case ref: CaptureRef if ref.isTrackableRef =>
        ref
      case tpe =>
        throw IllegalCaptureRef(tpe) // if this was compiled from cc syntax, problem should have been reported at Typer

  /** Convert a @retains or @retainsByName annotation tree to the capture set it represents.
   *  For efficience, the result is cached as an Attachment on the tree.
   */
  def toCaptureSet(using Context): CaptureSet =
    tree.getAttachment(Captures) match
      case Some(refs) => refs
      case None =>
        val refs = CaptureSet(tree.retainedElems.map(_.toCaptureRef)*)
          .showing(i"toCaptureSet $tree --> $result", capt)
        tree.putAttachment(Captures, refs)
        refs

  /** The arguments of a @retains, @retainsCap or @retainsByName annotation */
  def retainedElems(using Context): List[Tree] = tree match
    case Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil) =>
      elems
    case _ =>
      if tree.symbol.maybeOwner == defn.RetainsCapAnnot
      then ref(defn.captureRoot.termRef) :: Nil
      else Nil

extension (tp: Type)

  /** @pre `tp` is a CapturingType */
  def derivedCapturingType(parent: Type, refs: CaptureSet)(using Context): Type = tp match
    case tp @ CapturingType(p, r) =>
      if (parent eq p) && (refs eq r) then tp
      else CapturingType(parent, refs, tp.isBoxed)

  /** If this is a unboxed capturing type with nonempty capture set, its boxed version.
   *  Or, if type is a TypeBounds of capturing types, the version where the bounds are boxed.
   *  The identity for all other types.
   */
  def boxed(using Context): Type = tp.dealias match
    case tp @ CapturingType(parent, refs) if !tp.isBoxed && !refs.isAlwaysEmpty =>
      tp.annot match
        case ann: CaptureAnnotation => AnnotatedType(parent, ann.boxedAnnot)
        case ann => tp
    case tp: RealTypeBounds =>
      tp.derivedTypeBounds(tp.lo.boxed, tp.hi.boxed)
    case _ =>
      tp

  /** If this is a unboxed capturing type with nonempty capture set, its boxed version.
   *  Or, if type is a TypeBounds of capturing types, the version where the bounds are boxed.
   *  The identity for all other types.
   */
  def unboxed(using Context): Type = tp.dealias match
    case tp @ CapturingType(parent, refs) if tp.isBoxed && !refs.isAlwaysEmpty =>
      CapturingType(parent, refs)
    case tp: RealTypeBounds =>
      tp.derivedTypeBounds(tp.lo.unboxed, tp.hi.unboxed)
    case _ =>
      tp

  /** The capture set consisting of all top-level captures of `tp` that appear under a box.
   *  Unlike for `boxed` this also considers parents of capture types, unions and
   *  intersections, and type proxies other than abstract types.
   */
  def boxedCaptureSet(using Context): CaptureSet =
    def getBoxed(tp: Type): CaptureSet = tp match
      case tp @ CapturingType(parent, refs) =>
        val pcs = getBoxed(parent)
        if tp.isBoxed then refs ++ pcs else pcs
      case tp: TypeRef if tp.symbol.isAbstractType => CaptureSet.empty
      case tp: TypeProxy => getBoxed(tp.superType)
      case tp: AndType => getBoxed(tp.tp1) ** getBoxed(tp.tp2)
      case tp: OrType => getBoxed(tp.tp1) ++ getBoxed(tp.tp2)
      case _ => CaptureSet.empty
    getBoxed(tp)

  /** Is the boxedCaptureSet of this type nonempty? */
  def isBoxedCapturing(using Context) = !tp.boxedCaptureSet.isAlwaysEmpty

  /** If this type is a capturing type, the version with boxed statues as given by `boxed`.
   *  If it is a TermRef of a capturing type, and the box status flips, widen to a capturing
   *  type that captures the TermRef.
   */
  def forceBoxStatus(boxed: Boolean)(using Context): Type = tp.widenDealias match
    case tp @ CapturingType(parent, refs) if tp.isBoxed != boxed =>
      val refs1 = tp match
        case ref: CaptureRef if ref.isTracked || ref.isReach => ref.singletonCaptureSet
        case _ => refs
      CapturingType(parent, refs1, boxed)
    case _ =>
      tp

  /** Map capturing type to their parents. Capturing types accessible
   *  via dealising are also stripped.
   */
  def stripCapturing(using Context): Type = tp.dealiasKeepAnnots match
    case CapturingType(parent, _) =>
      parent.stripCapturing
    case atd @ AnnotatedType(parent, annot) =>
      atd.derivedAnnotatedType(parent.stripCapturing, annot)
    case _ =>
      tp

  /** Is type known to be always pure by its class structure,
   *  so that adding a capture set to it would not make sense?
   */
  def isAlwaysPure(using Context): Boolean = tp.dealias match
    case tp: (TypeRef | AppliedType) =>
      val sym = tp.typeSymbol
      if sym.isClass then sym.isPureClass
      else tp.superType.isAlwaysPure
    case CapturingType(parent, refs) =>
      parent.isAlwaysPure || refs.isAlwaysEmpty
    case tp: TypeProxy =>
      tp.superType.isAlwaysPure
    case tp: AndType =>
      tp.tp1.isAlwaysPure || tp.tp2.isAlwaysPure
    case tp: OrType =>
      tp.tp1.isAlwaysPure && tp.tp2.isAlwaysPure
    case _ =>
      false

  def isCapabilityClassRef(using Context) = tp.dealiasKeepAnnots match
    case _: TypeRef | _: AppliedType => tp.typeSymbol.hasAnnotation(defn.CapabilityAnnot)
    case _ => false

  /** Check if the class has universal capability, which means:
   *  1. the class has a capability annotation,
   *  2. the class is an impure function type,
   *  3. or one of its base classes has universal capability.
   */
  def hasUniversalCapability(using Context): Boolean = tp match
    case CapturingType(parent, ref) =>
      ref.isUniversal || parent.hasUniversalCapability
    case tp =>
      tp.isCapabilityClassRef || tp.parents.exists(_.hasUniversalCapability)

  /** Drop @retains annotations everywhere */
  def dropAllRetains(using Context): Type = // TODO we should drop retains from inferred types before unpickling
    val tm = new TypeMap:
      def apply(t: Type) = t match
        case AnnotatedType(parent, annot) if annot.symbol.isRetains =>
          apply(parent)
        case _ =>
          mapOver(t)
    tm(tp)

  /** If `x` is a capture ref, its reach capability `x*`, represented internally
   *  as `x @reachCapability`. `x*` stands for all capabilities reachable through `x`".
   *  We have `{x} <: {x*} <: dcs(x)}` where the deep capture set `dcs(x)` of `x`
   *  is the union of all capture sets that appear in covariant position in the
   *  type of `x`. If `x` and `y` are different variables then `{x*}` and `{y*}`
   *  are unrelated.
   */
  def reach(using Context): CaptureRef = tp match
    case tp: CaptureRef if tp.isTrackableRef =>
      if tp.isReach then tp else ReachCapability(tp)

  /** If `x` is a capture ref, its maybe capability `x?`, represented internally
   *  as `x @maybeCapability`. `x?` stands for a capability `x` that might or might
   *  not be part of a capture set. We have `{} <: {x?} <: {x}`. Maybe capabilities
   *  cannot be propagated between sets. If `a <: b` and `a` acquires `x?` then
   *  `x` is propagated to `b` as a conservative approximation.
   *
   *  Maybe capabilities should only arise for capture sets that appear in invariant
   *  position in their surrounding type. They are similar to TypeBunds types, but
   *  restricted to capture sets. For instance,
   *
   *      Array[C^{x?}]
   *
   *  should be morally equivalent to
   *
   *      Array[_ >: C^{} <: C^{x}]
   *
   *   but it has fewer issues with type inference.
   */
  def maybe(using Context): CaptureRef = tp match
    case tp: CaptureRef if tp.isTrackableRef =>
      if tp.isMaybe then tp else MaybeCapability(tp)

  /** If `ref` is a trackable capture ref, and `tp` has only covariant occurrences of a
   *  universal capture set, replace all these occurrences by `{ref*}`. This implements
   *  the new aspect of the (Var) rule, which can now be stated as follows:
   *
   *     x: T in E
   *     -----------
   *     E |- x: T'
   *
   *  where T' is T with (1) the toplevel capture set replaced by `{x}` and
   *  (2) all covariant occurrences of cap replaced by `x*`, provided there
   *  are no occurrences in `T` at other variances. (1) is standard, whereas
   *  (2) is new.
   *
   *  For (2), multiple-flipped covariant occurrences of cap won't be replaced.
   *  In other words,
   *
   *    - For xs: List[File^]  ==>  List[File^{xs*}], the cap is replaced;
   *    - while f: [R] -> (op: File^ => R) -> R remains unchanged.
   *
   *  Without this restriction, the signature of functions like withFile:
   *
   *    (path: String) -> [R] -> (op: File^ => R) -> R
   *
   *  could be refined to
   *
   *    (path: String) -> [R] -> (op: File^{withFile*} => R) -> R
   *
   *  which is clearly unsound.
   *
   *  Why is this sound? Covariant occurrences of cap must represent capabilities
   *  that are reachable from `x`, so they are included in the meaning of `{x*}`.
   *  At the same time, encapsulation is still maintained since no covariant
   *  occurrences of cap are allowed in instance types of type variables.
   */
  def withReachCaptures(ref: Type)(using Context): Type =
    class CheckContraCaps extends TypeTraverser:
      var ok = true
      def traverse(t: Type): Unit =
        if ok then
          t match
            case CapturingType(_, cs) if cs.isUniversal && variance <= 0 =>
              ok = false
            case _ =>
              traverseChildren(t)

    object narrowCaps extends TypeMap:
      /** Has the variance been flipped at this point? */
      private var isFlipped: Boolean = false

      def apply(t: Type) =
        val saved = isFlipped
        try
          if variance <= 0 then isFlipped = true
          t.dealias match
            case t1 @ CapturingType(p, cs) if cs.isUniversal && !isFlipped =>
              t1.derivedCapturingType(apply(p), ref.reach.singletonCaptureSet)
            case _ => t match
              case t @ CapturingType(p, cs) =>
                t.derivedCapturingType(apply(p), cs) // don't map capture set variables
              case t =>
                mapOver(t)
        finally isFlipped = saved
    ref match
      case ref: CaptureRef if ref.isTrackableRef =>
        val checker = new CheckContraCaps
        checker.traverse(tp)
        if checker.ok then
          val tp1 = narrowCaps(tp)
          if tp1 ne tp then capt.println(i"narrow $tp of $ref to $tp1")
          tp1
        else
          capt.println(i"cannot narrow $tp of $ref")
          tp
      case _ =>
        tp

extension (cls: ClassSymbol)

  def pureBaseClass(using Context): Option[Symbol] =
    cls.baseClasses.find: bc =>
      defn.pureBaseClasses.contains(bc)
      || bc.is(CaptureChecked)
          && bc.givenSelfType.dealiasKeepAnnots.match
            case CapturingType(_, refs) => refs.isAlwaysEmpty
            case RetainingType(_, refs) => refs.isEmpty
            case selfType =>
              isCaptureChecking  // At Setup we have not processed self types yet, so
                                 // unless a self type is explicitly given, we can't tell
                                 // and err on the side of impure.
              && selfType.exists && selfType.captureSet.isAlwaysEmpty

  def baseClassHasExplicitSelfType(using Context): Boolean =
    cls.baseClasses.exists: bc =>
      bc.is(CaptureChecked) && bc.givenSelfType.exists

  def matchesExplicitRefsInBaseClass(refs: CaptureSet)(using Context): Boolean =
    cls.baseClasses.tail.exists: bc =>
      val selfType = bc.givenSelfType
      bc.is(CaptureChecked) && selfType.exists && selfType.captureSet.elems == refs.elems

extension (sym: Symbol)

  /** This symbol is one of `retains` or `retainsCap` */
  def isRetains(using Context): Boolean =
    sym == defn.RetainsAnnot || sym == defn.RetainsCapAnnot

  /** This symbol is one of `retains`, `retainsCap`, or`retainsByName` */
  def isRetainsLike(using Context): Boolean =
    isRetains || sym == defn.RetainsByNameAnnot

  /** A class is pure if:
   *   - one its base types has an explicitly declared self type with an empty capture set
   *   - or it is a value class
   *   - or it is an exception
   *   - or it is one of Nothing, Null, or String
   */
  def isPureClass(using Context): Boolean = sym match
    case cls: ClassSymbol =>
      cls.pureBaseClass.isDefined || defn.pureSimpleClasses.contains(cls)
    case _ =>
      false

  /** Does this symbol allow results carrying the universal capability?
   *  Currently this is true only for function type applies (since their
   *  results are unboxed) and `erasedValue` since this function is magic in
   *  that is allows to conjure global capabilies from nothing (aside: can we find a
   *  more controlled way to achieve this?).
   *  But it could be generalized to other functions that so that they can take capability
   *  classes as arguments.
   */
  def allowsRootCapture(using Context): Boolean =
    sym == defn.Compiletime_erasedValue
    || defn.isFunctionClass(sym.maybeOwner)

  /** When applying `sym`, would the result type be unboxed?
   *  This is the case if the result type contains a top-level reference to an enclosing
   *  class or method type parameter and the method does not allow root capture.
   *  If the type parameter is instantiated to a boxed type, that type would
   *  have to be unboxed in the method's result.
   */
  def unboxesResult(using Context): Boolean =
    def containsEnclTypeParam(tp: Type): Boolean = tp.strippedDealias match
      case tp @ TypeRef(pre: ThisType, _) => tp.symbol.is(Param)
      case tp: TypeParamRef => true
      case tp: AndOrType => containsEnclTypeParam(tp.tp1) || containsEnclTypeParam(tp.tp2)
      case tp: RefinedType => containsEnclTypeParam(tp.parent) || containsEnclTypeParam(tp.refinedInfo)
      case _ => false
    containsEnclTypeParam(sym.info.finalResultType)
    && !sym.allowsRootCapture
    && sym != defn.Caps_unsafeBox
    && sym != defn.Caps_unsafeUnbox

  /** Does this symbol define a level where we do not want to let local variables
   *  escape into outer capture sets?
   */
  def isLevelOwner(using Context): Boolean =
    sym.isClass
    || sym.is(Method, butNot = Accessor)

  /** The owner of the current level. Qualifying owners are
   *   - methods other than constructors and anonymous functions
   *   - anonymous functions, provided they either define a local
   *     root of type caps.Cap, or they are the rhs of a val definition.
   *   - classes, if they are not staticOwners
   *   - _root_
   */
  def levelOwner(using Context): Symbol =
    def recur(sym: Symbol): Symbol =
      if !sym.exists || sym.isRoot || sym.isStaticOwner then defn.RootClass
      else if sym.isLevelOwner then sym
      else recur(sym.owner)
    recur(sym)

  /** The outermost symbol owned by both `sym` and `other`. if none exists
   *  since the owning scopes of `sym` and `other` are not nested, invoke
   *  `onConflict` to return a symbol.
   */
  def maxNested(other: Symbol, onConflict: (Symbol, Symbol) => Context ?=> Symbol)(using Context): Symbol =
    if !sym.exists || other.isContainedIn(sym) then other
    else if !other.exists || sym.isContainedIn(other) then sym
    else onConflict(sym, other)

  /** The innermost symbol owning both `sym` and `other`.
   */
  def minNested(other: Symbol)(using Context): Symbol =
    if !other.exists || other.isContainedIn(sym) then sym
    else if !sym.exists || sym.isContainedIn(other) then other
    else sym.owner.minNested(other.owner)

extension (tp: AnnotatedType)
  /** Is this a boxed capturing type? */
  def isBoxed(using Context): Boolean = tp.annot match
    case ann: CaptureAnnotation => ann.boxed
    case _ => false

/** An extractor for `caps.reachCapability(ref)`, which is used to express a reach
 *  capability as a tree in a @retains annotation.
 */
object ReachCapabilityApply:
  def unapply(tree: Apply)(using Context): Option[Tree] = tree match
    case Apply(reach, arg :: Nil) if reach.symbol == defn.Caps_reachCapability => Some(arg)
    case _ => None

class AnnotatedCapability(annot: Context ?=> ClassSymbol):
  def apply(tp: Type)(using Context) =
    AnnotatedType(tp, Annotation(annot, util.Spans.NoSpan))
  def unapply(tree: AnnotatedType)(using Context): Option[SingletonCaptureRef] = tree match
    case AnnotatedType(parent: SingletonCaptureRef, ann) if ann.symbol == annot => Some(parent)
    case _ => None

/** An extractor for `ref @annotation.internal.reachCapability`, which is used to express
 *  the reach capability `ref*` as a type.
 */
object ReachCapability extends AnnotatedCapability(defn.ReachCapabilityAnnot)

/** An extractor for `ref @maybeCapability`, which is used to express
 *  the maybe capability `ref?` as a type.
 */
object MaybeCapability extends AnnotatedCapability(defn.MaybeCapabilityAnnot)


