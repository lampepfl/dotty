import scala.quoted.*
import scala.language.experimental.quotedPatternsWithPolymorphicFunctions

inline def testExpr(inline body: Any) = ${ testExprImpl1('body) }
def testExprImpl1(body: Expr[Any])(using Quotes): Expr[String] =
  body match
    case '{ [A] => (x : A, y : A) => (x, y) } => Expr("Case 1 matched")
    case '{ [A <: Iterable[Int]] => (x : A) => x } => Expr("Case 2 matched")
    case _ => Expr("not matched")
