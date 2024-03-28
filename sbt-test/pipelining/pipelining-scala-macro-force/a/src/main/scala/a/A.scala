package a

import scala.quoted.*

object A {

  transparent inline def transparentPower(x: Double, inline n: Int): Double =
    ${ macros.MacroImpl.powerCode('x, 'n)  }

  inline def power(x: Double, inline n: Int): Double =
    ${ macros.MacroImpl.powerCode('x, 'n)  }

}
