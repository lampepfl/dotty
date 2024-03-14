package foo

import scala.quoted.*

private[foo] object Foo:
  def apply(): Int = ???

inline def test(): Unit = ${ testExpr() }

private def testExpr()(using Quotes): Expr[Unit] = {
  '{ Foo() } match
    case '{ Foo() } =>
  '{}
}
