package a

import scala.quoted.*

object A {

  inline def power(x: Double, inline n: Int): Double =
    ${ powerCode('x, 'n)  }

  def powerCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] = {
    def impl(x: Double, n: Int): Double =
      if (n == 0) 1.0
      else if (n % 2 == 1) x * impl(x, n - 1)
      else impl(x * x, n / 2)

    Expr(impl(x.valueOrError, n.valueOrError))
  }
}
