inline trait A[T]:
  def f: T = f
  def f(x: T): T = x
  def f[U <: T](x: U, y: T): T = x
end A

class B extends A[Int]:
  /*
  <generated> override def f: Int = this.f
  <generated> override def f(x: Int): Int = x
  <generated> override def f[U <: Int](x: U, y: Int): Int = x
  */
end B
