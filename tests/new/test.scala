trait Monoid[T]{
  def unit: T
  type TypeMember = Int
}
object Monoid{
  implicit object intM extends Monoid[Int]{ def unit = 0 }
}
object Main{
  opaque type ABound = Unit
  def reduceSort[A](xs: List[A])(using monoid_a: Monoid[A], ordering_a: scala.math.Ordering[A]) =
    inline implicit def f(v: ABound): ordering_a.type = ordering_a
    inline implicit def g(v: ABound): monoid_a.type = monoid_a
    val A: ABound = null.asInstanceOf[ABound]
    val i: A.TypeMember = 123 // THIS WORKS
    A.unit +: xs.sorted(A)

  def main(args: Array[String]): Unit = {
    println("Hello World")
    println(reduceSort(List(1, 2, 3, 2, 1)))
  }
}