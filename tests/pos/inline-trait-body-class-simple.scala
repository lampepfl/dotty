inline trait A[T]:
  class InnerA:
    def x: T = ???

class B extends A[Int]:
  class InnerB extends InnerA