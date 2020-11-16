import scala.quoted._

object Main {

  def myMacroImpl(body: Expr[_])(using qctx: QuoteContext) : Expr[_] = {
    import reflect._
    val bodyTerm = Term.of(underlyingArgument(body))
    val showed = bodyTerm.show
    '{
      println(${Expr(showed)})
      ${bodyTerm.asExpr}
    }
  }

  transparent inline def myMacro(body: => Any): Any = ${
    myMacroImpl('body)
  }

  def underlyingArgument[T](expr: Expr[T])(using qctx: QuoteContext): Expr[T] =
    import reflect._
    Term.of(expr).underlyingArgument.asExpr.asInstanceOf[Expr[T]]
}
