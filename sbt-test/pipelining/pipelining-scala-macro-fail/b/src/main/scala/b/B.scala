package b

import a.A

object B {
  @main def run =
    assert(A.power(2.0, 2) == 4.0)
    assert(A.power(2.0, 3) == 8.0)
    assert(A.power(2.0, 4) == 16.0)
}
