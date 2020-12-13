import scala.quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    println(foo[Object].show)
    println(bar[Object].show)
  }
  def foo[H : Type](using Quotes): Expr[H] = {
    val t = Type.of[H]
    '{ null.asInstanceOf[t.Underlying] }
  }
  def bar[H : Type](using Quotes): Expr[List[H]] = {
    val t = Type.of[List[H]]
    '{ null.asInstanceOf[t.Underlying] }
  }
}
