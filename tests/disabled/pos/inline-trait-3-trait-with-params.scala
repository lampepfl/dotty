inline trait A[T](a: T):
  def f: T = a
  def f(x: T): T = x
  def f[U <: T](x: U, y: T): T = x
end A

class B extends A[Int](3):
  /*
  <generated> override def f: Int = ???
  <generated> override def f(x: Int): Int = ???
  <generated> override def f[U <: Int](x: U, y: Int): Int = ???
  */
end B
