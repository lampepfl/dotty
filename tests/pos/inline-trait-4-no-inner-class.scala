inline trait Option[+T]:
  def get: T
  def isEmpty: Boolean
end Option

inline trait Some[+T](x: T) extends Option[T]:
  def get: T = x
  def isEmpty: Boolean = false
end Some

inline trait None extends Option[Nothing]:
  def get: Nothing = throw new NoSuchElementException("None.get")
  def isEmpty: Boolean = true
end None

sealed trait IntOption extends Option[Int]
class IntSome(i: Int) extends IntOption, Some[Int](i)
object IntNone extends IntOption, None

val o1: IntOption = IntSome(1) // specialized
val o2: IntOption = IntNone
val o3: Some[Int] = IntSome(1) // non-specialized
val x1: Int = o1.get // no unboxing
val x3: Int = o3.get // unboxing
