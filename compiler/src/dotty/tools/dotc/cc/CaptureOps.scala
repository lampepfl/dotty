package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import ast.{tpd, untpd}
import Decorators.*, NameOps.*
import config.Printers.capt
import util.Property.Key
import tpd.*

private val Captures: Key[CaptureSet] = Key()
private val Boxed: Key[Type] = Key()

def retainedElems(tree: Tree)(using Context): List[Tree] = tree match
  case Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil) => elems
  case _ => Nil

class IllegalCaptureRef(tpe: Type) extends Exception

extension (tree: Tree)

  def toCaptureRef(using Context): CaptureRef = tree.tpe match
    case ref: CaptureRef => ref
    case tpe => throw IllegalCaptureRef(tpe)

  def toCaptureSet(using Context): CaptureSet =
    tree.getAttachment(Captures) match
      case Some(refs) => refs
      case None =>
        val refs = CaptureSet(retainedElems(tree).map(_.toCaptureRef)*)
          .showing(i"toCaptureSet $tree --> $result", capt)
        tree.putAttachment(Captures, refs)
        refs

extension (tp: Type)

  def derivedCapturingType(parent: Type, refs: CaptureSet)(using Context): Type = tp match
    case tp @ CapturingType(p, r) =>
      if (parent eq p) && (refs eq r) then tp
      else CapturingType(parent, refs, tp.isBoxed)

  def boxed(using Context): Type = tp.dealias match
    case tp @ CapturingType(parent, refs) =>
      def boxedTp = parent.boxed match
        case CapturingType(parent1, refs1) =>
          CapturingType(parent1, refs ++ refs1, boxed = true)
        case parent1 =>
          CapturingType(parent1, refs, boxed = true)
      if tp.isBoxed || refs.isAlwaysEmpty then tp
      else tp.annot match
        case ann: CaptureAnnotation =>
          if !ann.boxedType.exists then ann.boxedType = boxedTp
          ann.boxedType
        case ann =>
          ann.tree.getAttachment(Boxed) match
            case None => ann.tree.putAttachment(Boxed, boxedTp)
            case _ =>
          ann.tree.attachment(Boxed)
    case _ =>
      tp

  def boxedUnlessFun(tycon: Type)(using Context) =
    if ctx.phase != Phases.checkCapturesPhase || defn.isFunctionClass(tycon.typeSymbol)
    then tp
    else tp.boxed
        //.showing(i"boxedUF $tp in $tycon = $result")

  /** The boxed capture set of a type */
  def boxedCaptureSet(using Context): CaptureSet =
    def getBoxed(tp: Type): CaptureSet = tp match
      case tp @ CapturingType(parent, refs) =>
        val pcs = getBoxed(parent)
        if tp.isBoxed then refs ++ pcs else pcs
      case tp: TypeRef if tp.symbol.isAbstractType => CaptureSet.empty
      case tp: TypeProxy => getBoxed(tp.superType)
      case tp: AndType => getBoxed(tp.tp1) ++ getBoxed(tp.tp2)
      case tp: OrType => getBoxed(tp.tp1) ** getBoxed(tp.tp2)
      case _ => CaptureSet.empty
    getBoxed(tp)

  def isBoxedCapturing(using Context) = !tp.boxedCaptureSet.isAlwaysEmpty

  def stripCapturing(using Context): Type = tp.dealiasKeepAnnots match
    case CapturingType(parent, _) =>
      parent.stripCapturing
    case atd @ AnnotatedType(parent, annot) =>
      atd.derivedAnnotatedType(parent.stripCapturing, annot)
    case _ =>
      tp

  /** Under -Ycc, map regular function type to impure function type
   */
  def adaptFunctionType(using Context): Type = tp match
    case AppliedType(fn, args)
    if ctx.settings.Ycc.value && defn.isFunctionClass(fn.typeSymbol) =>
      val fname = fn.typeSymbol.name
      defn.FunctionType(
        fname.functionArity,
        isContextual = fname.isContextFunction,
        isErased = fname.isErasedFunction,
        isImpure = true).appliedTo(args)
    case _ =>
      tp

extension (sym: Symbol)

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

  def unboxesResult(using Context): Boolean =
    def containsEnclTypeParam(tp: Type): Boolean = tp.strippedDealias match
      case tp @ TypeRef(pre: ThisType, _) => tp.symbol.is(Param)
      case tp: TypeParamRef => true
      case tp: AndOrType => containsEnclTypeParam(tp.tp1) || containsEnclTypeParam(tp.tp2)
      case tp: RefinedType => containsEnclTypeParam(tp.parent) || containsEnclTypeParam(tp.refinedInfo)
      case _ => false
    containsEnclTypeParam(sym.info.finalResultType)
    && !sym.allowsRootCapture

extension (tp: AnnotatedType)
  def isBoxed(using Context): Boolean = tp.annot match
    case ann: CaptureAnnotation => ann.boxed
    case _ => false

extension (ts: List[Type])
  def boxedUnlessFun(tycon: Type)(using Context) =
    if ctx.phase != Phases.checkCapturesPhase || defn.isFunctionClass(tycon.typeSymbol)
    then ts
    else ts.mapconserve(_.boxed)

