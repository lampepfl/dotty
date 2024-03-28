package b

import a.A
import a.ASuspendTyper
import a.ASuspendInlining

object B {
  @main def run =
    assert(A.power(2.0, 2) == 4.0)
    assert(A.power(2.0, 3) == 8.0)
    assert(A.power(2.0, 4) == 16.0)
    assert(ASuspendTyper.thirtyTwo == 32.0) // check that suspended definition is still available
    assert(ASuspendInlining.sixtyFour == 64.0) // check that suspended definition is still available
}
