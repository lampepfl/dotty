package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import ast.Trees.*
import ast.{tpd, untpd}
import Decorators.*
import config.Printers.capt
import printing.Printer
import printing.Texts.Text

/** An annotation representing a capture set and whether it is boxed.
 *  It simulates a normal @retains annotation except that it is more efficient,
 *  supports variables as capture sets, and adds a `boxed` flag.
 *  These annotations are created during capture checking. Before that
 *  there are only regular @retains and @retainsByName annotations.
 *  @param refs    the capture set
 *  @param boxed   whether the type carrying the annotation is boxed
 *  @param cls     the underlying class (either annotation.retains or annotation.retainsByName)
 */
case class CaptureAnnotation(refs: CaptureSet, boxed: Boolean)(cls: Symbol) extends Annotation:
  import CaptureAnnotation.*
  import tpd.*

  /** A cache for boxed version of a capturing type with this annotation */
  val boxedType = BoxedTypeCache()

  /** Reconstitute annotation tree from capture set */
  override def tree(using Context) =
    val elems = refs.elems.toList.map {
      case cr: TermRef => ref(cr)
      case cr: TermParamRef => untpd.Ident(cr.paramName).withType(cr)
      case cr: ThisType => This(cr.cls)
    }
    val arg = repeated(elems, TypeTree(defn.AnyType))
    New(symbol.typeRef, arg :: Nil)

  override def symbol(using Context) = cls

  override def derivedAnnotation(tree: Tree)(using Context): Annotation = this

  def derivedAnnotation(refs: CaptureSet, boxed: Boolean)(using Context): Annotation =
    if (this.refs eq refs) && (this.boxed == boxed) then this
    else CaptureAnnotation(refs, boxed)(cls)

  override def sameAnnotation(that: Annotation)(using Context): Boolean = that match
    case CaptureAnnotation(refs, boxed) =>
      this.refs == refs && this.boxed == boxed && this.symbol == that.symbol
    case _ => false

  override def mapWith(tm: TypeMap)(using Context) =
    val elems = refs.elems.toList
    val elems1 = elems.mapConserve(tm)
    if elems1 eq elems then this
    else if elems1.forall(_.isInstanceOf[CaptureRef])
    then derivedAnnotation(CaptureSet(elems1.asInstanceOf[List[CaptureRef]]*), boxed)
    else EmptyAnnotation

  override def refersToParamOf(tl: TermLambda)(using Context): Boolean =
    refs.elems.exists {
      case TermParamRef(tl1, _) => tl eq tl1
      case _ => false
    }

  override def toText(printer: Printer): Text = refs.toText(printer)

  override def hash: Int =
    (refs.hashCode << 1) | (if boxed then 1 else 0)

  override def eql(that: Annotation) = that match
    case that: CaptureAnnotation => (this.refs eq that.refs) && (this.boxed == that.boxed)
    case _ => false

end CaptureAnnotation

object CaptureAnnotation:
  /** Capture annotation with an additional field which is a set of
   *  captures that are known to be separated from. */
  final class SeparationCaptureAnnotation(refs: CaptureSet, val seps: CaptureSet, boxed: Boolean)(cls: Symbol) extends CaptureAnnotation(refs, boxed)(cls) {
    override def hash: Int = seps.hashCode * 31 + super.hash
  }

