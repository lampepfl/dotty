import scala.quoted._
import scala.quoted.staging._

sealed abstract class VarRef[T] {
  def update(expr: Expr[T])(using Quotes): Expr[Unit]
  def expr(using Quotes): Expr[T]
}

object VarRef {
  def apply[T: Type, U: Type](init: Expr[T])(body: VarRef[T] => Expr[U])(using Quotes): Expr[U] = '{
    var x = $init
    ${body(
      new VarRef {
        def update(e: Expr[T])(using Quotes): Expr[Unit] = '{ x = $e }
        def expr(using Quotes): Expr[T] = 'x
      }
    )}
  }

}

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    val q = VarRef('{4})(varRef => '{ ${varRef.update('{3})}; ${varRef.expr} })
    println(q.show)
  }
}
