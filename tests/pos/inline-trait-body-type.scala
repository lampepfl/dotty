inline trait A[T]:
  type U = String

class B extends A[Int]:
  def f: U = "ABD"