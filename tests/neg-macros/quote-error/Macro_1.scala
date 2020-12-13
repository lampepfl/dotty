import quoted._

object Macro_1 {
  inline def foo(inline b: Boolean): Unit = ${fooImpl('b)}
  def fooImpl(b: Expr[Boolean])(using Quotes) : Expr[Unit] =
    if (b.valueOrError) '{println("foo(true)")}
    else { quotes.reflect.report.error("foo cannot be called with false"); '{ ??? } }
}
