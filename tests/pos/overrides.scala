//> using options -experimental

class A[T] {

  def f(x: T)(y: T = x) = y

  import scala.language.experimental.clauseInterleaving

  def b[U <: T](x: Int)[V >: T](y: String) = false

}
class B extends A[Int] {

  override def f(x: Int)(y: Int) = y

  f(2)()


  import scala.language.experimental.clauseInterleaving

  override def b[T <: Int](x: Int)[U >: Int](y: String) = true

}
