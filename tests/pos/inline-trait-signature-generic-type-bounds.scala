inline trait A[T >: Int <: AnyVal]:
  def f(x: T): T = x

class B extends A[Int]
