package c

import a.A
import b.B

object C {
  val c_1: 2 = A.foo(1)
  val c_2: "B" = B.VALUE

  @main def run =
    assert(A.foo(0) == 1)
    assert(A.foo(1) == 2)
    assert(A.foo(2) == 3)
    assert(B.VALUE == "B")
}
