inline trait A[T]:
  def f(x: T): T = x

class B extends A[Int]
