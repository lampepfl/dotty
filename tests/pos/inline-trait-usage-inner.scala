object O:
  inline trait A[T]:
    def t: T = ???

class B extends O.A[Int]
