import scala.quoted.*
import scala.language.experimental.quotedPatternsWithPolymorphicFunctions

inline def testExpr(inline body: Any) = ${ testExprImpl1('body) }
def testExprImpl1(body: Expr[Any])(using Quotes): Expr[String] =
  body match
    case '{ [A] => (x : Int, y : Int) => $b(x, y) : Int } =>
      '{ "case 2 matched => " + $b(2, 3) }
    case '{ [A] => (x : A, y : A) => $b[A](x, y) : A } =>
      '{ "case 3 matched => " + $b[String]("truthy", "falsy") }
    case '{ [A] => (x : A, y : A) => $b[A](x, y) : (A, A) } =>
      '{ "case 4 matched => " + $b[String]("truthy", "falsy")._2 }
    case '{ [A, B] => (x : A, y : A => B) => $a[A, B](x, y) : B } =>
      '{ "case 5 matchd => " + $a[Int, Int](0, x => x + 1) }
    case '{ [A] => (x : List[A], y : A) => $a[A](x) : Int } =>
      '{ "case 6 matchd => " + $a[Int](List(1, 2, 3)) }
    case '{ [A] => (x : List[A], y : A) => $a[A](x, y) : Int } =>
      '{ "case 7 matchd => " + $a[Int](List(1, 2, 3), 2) }
    case '{ [A] => (x : A) => [B] => (y : B) => $a[A, B](x, y) : (A, B) } =>
      '{ "case 8 matched => " + $a[Int, String](1, "str")}
    case '{ [A, B] => (x : Map[A, B], y: A) => $a[A, B](x, y) : Option[B] } =>
      '{ "case 9 matched => " + $a[Int, String](Map(0 -> "zero", 1 -> "one"), 0).getOrElse("failed") }
    case _ => Expr("not matched")
