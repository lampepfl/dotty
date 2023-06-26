inline trait A[T]:
  inline trait InnerA[U]: // error
    val x: (T, U) = ???

class B extends A[Int]:
  class InnerB extends InnerA[String]
  def f: (Int, String) = InnerB().x