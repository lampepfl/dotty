package b

import a.A
import a.AConsumeTransparent
import a.AConsume

object B {
  @main def run =
    assert(A.power(2.0, 2) == 4.0)
    assert(A.power(2.0, 3) == 8.0)
    assert(A.power(2.0, 4) == 16.0)
    assert(AConsumeTransparent.thirtyTwo == 32.0) // these are not actually suspended in this project
    assert(AConsume.sixtyFour == 64.0) // check that suspended definition is still available
}
