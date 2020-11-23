import quoted._

object Macro_1 {
  inline def foo(inline b: Boolean): Unit = ${ fooImpl('b) }
  def fooImpl(b: Expr[Boolean])(using Quotes): Expr[Unit] =
    '{println(${msg(b.unliftOrError)})}

  def msg(b: Boolean)(using Quotes): Expr[String] =
    if (b) '{"foo(true)"}
    else { report.error("foo cannot be called with false"); '{ ??? } }

}
