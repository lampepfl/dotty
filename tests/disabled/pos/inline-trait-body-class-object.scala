inline trait A[T]:
  object Inner:
    val x: T = ???

class B extends A[Int]:
  def i: Int = Inner.x
