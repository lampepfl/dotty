package dotty.tools
package dotc
package cc

import core.*
import Decorators.*
import Types.*, Symbols.*, Contexts.*
import NameKinds.UniqueName
import util.SimpleIdentityMap

/** A (possibly boxed) capturing type. This is internally represented as an annotated type with a @retains
 *  or @retainsByName annotation, but the extractor will succeed only at phase CheckCaptures.
 *  That way, we can ignore caturing information until phase CheckCaptures since it is
 *  wrapped in a plain annotation.
 *
 *  The same trick does not work for the boxing information. Boxing is context dependent, so
 *  we have to add that information in the Setup step preceding CheckCaptures. Boxes are
 *  added for all type arguments of methods. For type arguments of applied types a different
 *  strategy is used where we box arguments of applied types that are not functions when
 *  accessing the argument.
 *
 *  An alternative strategy would add boxes also to arguments of applied types during setup.
 *  But this would have to be done for all possibly accessibly types from the compiled units
 *  as well as their dependencies. It's difficult to do this in a DenotationTransformer without
 *  accidentally forcing symbol infos. That's why this alternative was not implemented.
 *  If we would go back on this it would make sense to also treat captuyring types different
 *  from annotations and to generate them all during Setup and in DenotationTransformers.
 */
object CapturingType:

  /** Smart constructor that drops empty capture sets and fuses compatible capturiong types.
   *  An outer type capturing type A can be fused with an inner capturing type B if their
   *  boxing status is the same or if A is boxed.
   */
  def apply(parent: Type, refs: CaptureSet, boxed: Boolean = false)(using Context): Type =
    if refs.isAlwaysEmpty then parent
    else parent match
      case parent @ CapturingType(parent1, refs1) if boxed || !parent.isBoxed =>
        apply(parent1, refs ++ refs1, boxed)
      case _ =>
        AnnotatedType(parent, CaptureAnnotation(refs, boxed)(defn.RetainsAnnot))

  /** An extractor that succeeds only during CheckCapturingPhase. Boxing statis is
   *  returned separately by CaptureOps.isBoxed.
   */
  def unapply(tp: Type)(using Context): Option[(Type, CaptureSet)] =
    if ctx.phase == Phases.checkCapturesPhase
      && !ctx.mode.is(Mode.IgnoreCaptures)
    then
      tp match
        case Annotated(parent, cs) => Some(parent, cs)
        case Capability(parent, cs) => Some(parent, cs)
        case _ => None
    else None

  /** Check whether a type is uncachable when computing `baseType`.
    * - Avoid caching all the types during the setup phase, since at that point
    *   the capture set variables are not fully installed yet.
    * - Avoid caching capturing types when IgnoreCaptures mode is set, since the
    *   capture sets may be thrown away in the computed base type.
    */
  def isUncachable(tp: Type)(using Context): Boolean =
    ctx.phase == Phases.checkCapturesPhase &&
      (Setup.isDuringSetup || ctx.mode.is(Mode.IgnoreCaptures) && tp.isEventuallyCapturingType)

  object Annotated:
    def unapply(tp: AnnotatedType)(using Context): Option[(Type, CaptureSet)] =
      if ctx.phase == Phases.checkCapturesPhase
        && !ctx.mode.is(Mode.IgnoreCaptures)
        && tp.annot.symbol == defn.RetainsAnnot
      then
        EventuallyCapturingType.unapplyAnnot(tp)
      else None

  object Capability:
    def unapply(tp: Type)(using Context): Option[(Type, CaptureSet)] =
      if ctx.phase == Phases.checkCapturesPhase
        && !ctx.mode.is(Mode.IgnoreCaptures)
      then
        EventuallyCapturingType.unapplyCap(tp)
      else None

end CapturingType


/** An extractor for types that will be capturing types at phase CheckCaptures. Also
 *  included are types that indicate captures on enclosing call-by-name parameters
 *  before phase ElimByName.
 */
object EventuallyCapturingType:

  object Annotated:
    def unapply(tp: AnnotatedType)(using Context): Option[(Type, CaptureSet)] = unapplyAnnot(tp)

  object Capability:
    def unapply(tp: TypeRef)(using Context): Option[(Type, CaptureSet)] = unapplyCap(tp)

  private var pureCapClassSymCache: SimpleIdentityMap[ClassSymbol, ClassSymbol] = SimpleIdentityMap.empty

  private def createPureSymbolOf(csym: ClassSymbol)(using Context): ClassSymbol =
    csym.copy(flags = csym.flags | Flags.CapabilityBase).asClass

  private def pureSymbolOf(csym: ClassSymbol)(using Context): ClassSymbol =
    pureCapClassSymCache(csym) match
      case psym: ClassSymbol => psym
      case null =>
        val sym = createPureSymbolOf(csym)
        pureCapClassSymCache = pureCapClassSymCache.updated(csym, sym)
        sym

  def unapply(tp: Type)(using Context): Option[(Type, CaptureSet)] =
    tp match
      case tp: AnnotatedType => unapplyAnnot(tp)
      case _ => unapplyCap(tp)

  def unapplyCap(tp: Type)(using Context): Option[(Type, CaptureSet)] =
    if tp.classSymbol.hasAnnotation(defn.CapabilityAnnot) && !tp.classSymbol.is(Flags.CapabilityBase) then
      val sym = tp.classSymbol
      val psym = pureSymbolOf(sym.asClass)
      tp match
        case tp: TypeRef => Some((psym.typeRef, CaptureSet.universal))
        case tp: AppliedType =>
          Some((tp.derivedAppliedType(psym.typeRef, tp.args), CaptureSet.universal))
        case _ => None
    else None
    // None

  def unapplyAnnot(tp: AnnotatedType)(using Context): Option[(Type, CaptureSet)] =
    val sym = tp.annot.symbol
    if sym == defn.RetainsAnnot || sym == defn.RetainsByNameAnnot then
      tp.annot match
        case ann: CaptureAnnotation =>
          Some((tp.parent, ann.refs))
        case ann =>
          try Some((tp.parent, ann.tree.toCaptureSet))
          catch case ex: IllegalCaptureRef => None
    else None

end EventuallyCapturingType


