package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import ast.{tpd, untpd}
import Decorators.*, NameOps.*
import config.SourceVersion
import config.Printers.capt
import util.Property.Key
import tpd.*
import config.Feature
import dotty.tools.dotc.cc.CaptureAnnotation.SeparationCaptureAnnotation

private val Captures: Key[CaptureSet] = Key()
private val CapturesAndSeps: Key[(CaptureSet, CaptureSet)] = Key()
private val BoxedType: Key[BoxedTypeCache] = Key()

/** The arguments of a @retains or @retainsByName annotation */
private[cc] def retainedElems(tree: Tree)(using Context): List[Tree] = tree match
  case Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil) => elems
  case _ => Nil

private[cc] def retainedWithSepElems(tree: Tree)(using Context): (List[Tree], List[Tree]) = tree match
  case Apply(Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil), Typed(SeqLiteral(seps, _), _) :: Nil) =>
    (elems, seps)
  case _ => (Nil, Nil)

def allowUniversalInBoxed(using Context) =
  Feature.sourceVersion.isAtLeast(SourceVersion.`3.3`)

/** An exception thrown if a @retains argument is not syntactically a CaptureRef */
class IllegalCaptureRef(tpe: Type) extends Exception

extension (tree: Tree)

  /** Map tree with CaptureRef type to its type, throw IllegalCaptureRef otherwise */
  def toCaptureRef(using Context): CaptureRef = tree.tpe match
    case ref: CaptureRef => ref
    case tpe => throw IllegalCaptureRef(tpe)

  /** Convert a @retains or @retainsByName annotation tree to the capture set it represents.
   *  For efficience, the result is cached as an Attachment on the tree.
   */
  def toCaptureSet(using Context): CaptureSet =
    tree.getAttachment(Captures) match
      case Some(refs) => refs
      case None =>
        val refs = CaptureSet(retainedElems(tree).map(_.toCaptureRef)*)
          .showing(i"toCaptureSet $tree --> $result", capt)
        tree.putAttachment(Captures, refs)
        refs

  def toCapturesAndSeps(using Context): (CaptureSet, CaptureSet) =
    tree.getAttachment(CapturesAndSeps) match
      case Some(res) => res
      case None =>
        val (refs, seps) = retainedWithSepElems(tree)
        val cset = CaptureSet(refs.map(_.toCaptureRef)*)
        val sset = CaptureSet(seps.map(_.toCaptureRef)*)
        tree.putAttachment(CapturesAndSeps, (cset, sset))
        (cset, sset)

  /** Under pureFunctions, add a @retainsByName(*)` annotation to the argument of
   *  a by name parameter type, turning the latter into an impure by name parameter type.
   */
  def adaptByNameArgUnderPureFuns(using Context): Tree =
    if Feature.pureFunsEnabledSomewhere then
      val rbn = defn.RetainsByNameAnnot
      Annotated(tree,
        New(rbn.typeRef).select(rbn.primaryConstructor).appliedTo(
          Typed(
            SeqLiteral(ref(defn.captureRoot) :: Nil, TypeTree(defn.AnyType)),
            TypeTree(defn.RepeatedParamType.appliedTo(defn.AnyType))
          )
        )
      )
    else tree

extension (tp: Type)

  /** @pre `tp` is a CapturingType */
  def derivedCapturingType(parent: Type, refs: CaptureSet)(using Context): Type = tp match
    case tp @ CapturingType(p, r) =>
      if (parent eq p) && (refs eq r) then tp
      else CapturingType(parent, refs, tp.separationSet, tp.isBoxed)

  def derivedCapturingType(parent: Type, refs: CaptureSet, seps: CaptureSet)(using Context): Type = tp match
    case tp @ CapturingType(p, r) =>
      if (parent eq p) && (refs eq r) && (seps eq tp.separationSet) then tp
      else CapturingType(parent, refs, seps, tp.isBoxed)

  /** If this is a unboxed capturing type with nonempty capture set, its boxed version.
   *  Or, if type is a TypeBounds of capturing types, the version where the bounds are boxed.
   *  The identity for all other types.
   */
  def boxed(using Context): Type = tp.dealias match
    case tp @ CapturingType(parent, refs) if !tp.isBoxed && !refs.isAlwaysEmpty =>
      tp.annot match
        case ann: CaptureAnnotation =>
          ann.boxedType(tp)
        case ann =>
          ann.tree.getAttachment(BoxedType) match
            case None => ann.tree.putAttachment(BoxedType, BoxedTypeCache())
            case _ =>
          ann.tree.attachment(BoxedType)(tp)
    case tp: RealTypeBounds =>
      tp.derivedTypeBounds(tp.lo.boxed, tp.hi.boxed)
    case _ =>
      tp

  /** If `sym` is a type parameter, the boxed version of `tp`, otherwise `tp` */
  def boxedIfTypeParam(sym: Symbol)(using Context) =
    if sym.is(TypeParam) then tp.boxed else tp

  /** The boxed version of `tp`, unless `tycon` is a function symbol */
  def boxedUnlessFun(tycon: Type)(using Context) =
    if ctx.phase != Phases.checkCapturesPhase || defn.isFunctionSymbol(tycon.typeSymbol)
    then tp
    else tp.boxed

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
        case ref: CaptureRef if ref.isTracked => ref.singletonCaptureSet
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

  /** Under pureFunctions, map regular function type to impure function type
   */
  def adaptFunctionTypeUnderPureFuns(using Context): Type = tp match
    case AppliedType(fn, args)
    if Feature.pureFunsEnabledSomewhere && defn.isFunctionClass(fn.typeSymbol) =>
      val fname = fn.typeSymbol.name
      defn.FunctionType(
        fname.functionArity,
        isContextual = fname.isContextFunction,
        isImpure = true).appliedTo(args)
    case _ =>
      tp

  /** Under pureFunctions, add a @retainsByName(*)` annotation to the argument of
   *  a by name parameter type, turning the latter into an impure by name parameter type.
   */
  def adaptByNameArgUnderPureFuns(using Context): Type =
    if Feature.pureFunsEnabledSomewhere then
      AnnotatedType(tp,
        CaptureAnnotation(CaptureSet.universal, boxed = false)(defn.RetainsByNameAnnot))
    else
      tp

  def isCapturingType(using Context): Boolean =
    tp match
      case CapturingType(_, _) => true
      case _ => false

  def isEventuallyCapturingType(using Context): Boolean =
    tp match
      case EventuallyCapturingType(_, _) => true
      case _ => false

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

extension (cls: ClassSymbol)

  def pureBaseClass(using Context): Option[Symbol] =
    cls.baseClasses.find(bc =>
      defn.pureBaseClasses.contains(bc)
      || {
        val selfType = bc.givenSelfType
        selfType.exists && selfType.captureSet.isAlwaysEmpty
      })

extension (sym: Symbol)

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

extension (tp: AnnotatedType)
  /** Is this a boxed capturing type? */
  def isBoxed(using Context): Boolean = tp.annot match
    case ann: CaptureAnnotation => ann.boxed
    case _ => false

extension (tp: Type)
  def separationSet(using Context): CaptureSet = tp match
    case tp: AnnotatedType =>
      tp.annot match
      case ann: SeparationCaptureAnnotation => ann.seps
      case ann if ann.symbol == defn.RetainsWithSepAnnot =>
        try
          ann.tree.toCapturesAndSeps._2
        catch case ex: IllegalCaptureRef => CaptureSet.empty
      case _ => CaptureSet.empty
    case _ => CaptureSet.empty

extension (ts: List[Type])
  /** Equivalent to ts.mapconserve(_.boxedUnlessFun(tycon)) but more efficient where
   *  it is the identity.
   */
  def boxedUnlessFun(tycon: Type)(using Context) =
    if ctx.phase != Phases.checkCapturesPhase || defn.isFunctionClass(tycon.typeSymbol)
    then ts
    else ts.mapconserve(_.boxed)

