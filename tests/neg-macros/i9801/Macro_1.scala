import scala.quoted._

def f() = ()

def triggerStackOverflow(n: Int): Expr[Double] = {
  val r = triggerStackOverflow(n - 1)
  f()
  r
}

inline def loop(inline prog: Double): Double = ${impl('prog)}

def impl(prog: Expr[Double])(using Quotes) : Expr[Double] =
  import qctx.reflect._
  try {
    triggerStackOverflow(0)
  } catch {
    case e =>
      qctx.reflect.Reporting.error(e.getMessage, Term.of(prog).pos)
      '{ 42.0 }
  }
