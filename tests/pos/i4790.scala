class Test:
  def foo(as: Seq[Int]) =
    val List(_, bs: _*) = as: @unchecked
    val cs: Seq[Int] = bs

class Test2:
  def foo(as: SSeq[Int]) =
    val LList(_, tail) = as: @unchecked
    val cs: SSeq[Int] = tail

trait SSeq[+A]
sealed trait LList[+A] extends SSeq[A]
final case class CCons[+A](head: A, tail: LList[A]) extends LList[A]
case object NNil extends LList[Nothing]
object LList:
  def unapply[A](xs: LList[A]): Extractor[A] = Extractor[A](xs)
  final class Extractor[A](private val xs: LList[A]) extends AnyVal:
    def get: this.type   = this
    def isEmpty: Boolean = xs.isInstanceOf[CCons[?]]
    def _1: A            = xs.asInstanceOf[CCons[A]].head
    def _2: SSeq[A]      = xs.asInstanceOf[CCons[A]].tail
