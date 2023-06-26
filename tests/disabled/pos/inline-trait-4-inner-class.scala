inline trait Options[+T]:
  sealed trait Option:
    def get: T
    def isEmpty: Boolean

  class Some(x: T) extends Option:
    def get: T = x
    def isEmpty: Boolean = false

  object None extends Option:
    def get: T = throw new NoSuchElementException("None.get")
    def isEmpty: Boolean = true
end Options

object IntOptions extends Options[Int]
import IntOptions._

val o1: Option = Some(1) // specialized
val o2: Option = None
val x1: Int = o1.get // no unboxing
