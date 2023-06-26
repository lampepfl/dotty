inline trait A[T]:
  trait InnerA[U]:
    def x: (T, U) = ???

class B extends A[Int]:
  class InnerB extends InnerA[String]