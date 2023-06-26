package dotty.tools.benchmarks.inlinetraits
package standard

trait Numeric[T] {
  def zero: T
  def plus(x: T, y: T): T
  def times(x: T, y: T): T
}

object Numeric {
  @inline def apply[T](implicit num: Numeric[T]): Numeric[T] = num

  trait IntIsNumeric extends Numeric[Int] {
    def zero: Int = 0
    def plus(x: Int, y: Int): Int = x + y
    def times(x: Int, y: Int): Int = x * y
  }
  implicit object IntIsNumeric extends IntIsNumeric

  trait DoubleIsNumeric extends Numeric[Double] {
    def zero: Double = 0d
    def plus(x: Double, y: Double): Double = x + y
    def times(x: Double, y: Double): Double = x * y
  }
  implicit object DoubleIsNumeric extends DoubleIsNumeric
}
