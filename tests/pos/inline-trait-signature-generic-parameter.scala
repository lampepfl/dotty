inline trait A[T](val x: T):
  def f: T = x

class B extends A[Int](1):
  val y: Int = f
