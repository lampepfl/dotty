import scala.quoted._

object Macro {

  transparent def foo(b: Boolean): Int = {
    if (b) ~bar(true)
    else ~bar(false)
  }

  def bar(b: Boolean): Expr[Int] = if (b) '(1) else '(0)
}
