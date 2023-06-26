inline trait A[T](x: T):
  def f: T = x

class B extends A[1](1)
