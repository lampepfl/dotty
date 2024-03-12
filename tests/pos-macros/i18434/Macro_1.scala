package defn

import scala.quoted.*

abstract class Macro {
  def impl()(using Quotes): Expr[Int] = '{1}
}
