import scala.quoted._

def typeNameOf[A: Type](using Quotes): Expr[String] =
  val name = Type.show[A]
  Expr(name)

inline def typeNameOf[A]: String = ${ typeNameOf[A] }

inline def typeNameOfF1[A](f: A => Unit): String = ${ typeNameOf[A] }
