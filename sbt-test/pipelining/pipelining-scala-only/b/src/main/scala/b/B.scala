package b

import a.A

object B {
  val b: 2 = A.foo(1)

  @main def run =
    assert(A.foo(0) == 1)
    assert(A.foo(1) == 2)
    assert(A.foo(2) == 3)
}
