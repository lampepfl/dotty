inline trait A[T]:
  trait InnerA:
    def x: T = ???

class B extends A[Int]:
  class InnerB extends InnerA