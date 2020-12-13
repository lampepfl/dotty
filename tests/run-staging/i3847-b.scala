import scala.quoted._
import scala.quoted.staging._
import scala.reflect.ClassTag

object Arrays {
  implicit def ArrayIsToExpr[T: ToExpr](implicit t: Type[T], qctx: Quotes): ToExpr[Array[List[T]]] = {
    new ToExpr[Array[List[T]]] {
      def apply(arr: Array[List[T]])(using Quotes) = '{
        new Array[List[T]](${Expr(arr.length)})
        // TODO add elements
      }
    }
  }
}

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    import Arrays._
    implicit val ct: Expr[ClassTag[Int]] = '{ClassTag.Int}
    val arr: Expr[Array[List[Int]]] = Expr(Array[List[Int]](List(1, 2, 3)))
    println(arr.show)
  }
}
