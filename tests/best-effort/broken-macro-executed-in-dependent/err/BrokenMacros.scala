import scala.quoted._
object BrokenMacros:
  transparent inline def macro1() = ${macroImpl()}
  def macroImpl(using Quotes)(): Expr[String] =
    val a: Int = "str" // source of the error
    '{a}

  sealed trait Foo
  case class FooA() extends Foo
  case class FooB()
  transparent inline def macro2(): Foo = ${macro2Impl()}
  def macro2Impl(using Quotes)(): Expr[Foo] =
    '{FooB()}
