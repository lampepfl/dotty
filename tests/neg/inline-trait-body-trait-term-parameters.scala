
inline trait A[T]:
  trait InnerA(t: T): // error
    def x: T = t

class B extends A[Int]:
  class InnerB extends InnerA(???)