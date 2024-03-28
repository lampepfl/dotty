package a

import scala.quoted.*

object A {
  inline def power(x: Double, inline n: Int): Double =
    inline if (n == 0) 1.0
    else inline if (n % 2 == 1) x * power(x, n - 1)
    else power(x * x, n / 2)
}
