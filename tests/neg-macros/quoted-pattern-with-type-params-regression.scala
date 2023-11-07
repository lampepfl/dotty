/**
  * Refer to: quoted-pattern-with-type-params.scala
  */
import scala.quoted.*

def test(body: Expr[Any])(using Quotes): Expr[String] =
  body match
    case '{ [A] => (x : A) => $b[A] : (A => A) } => ??? // error
    case '{ [A] => (x : A) => $b(x) : (A => A) } => ??? // error
    case '{ (a:Int) => $b[Int](a) : String } => ??? // error
    case _ => Expr("not matched")
