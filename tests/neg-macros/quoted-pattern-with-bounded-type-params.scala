/*
 * Supporting hoas quote pattern with bounded type variable
 * is future todo.
 */

import scala.quoted.*
import scala.language.experimental.quotedPatternsWithPolymorphicFunctions

def test(body: Expr[Any])(using Quotes): Expr[String] =
  body match
    case '{ [A <: Int, B] => (x : A, y : A) => $b[A](x, y) : A } => ??? // error
    case _ => Expr("not matched")
