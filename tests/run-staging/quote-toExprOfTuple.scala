
import scala.quoted._
import scala.quoted.staging._

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = {
    for (n <- 0 to 25) {
      prev = 0
      println(run { Expr.ofTupleFromSeq(Seq.fill(n)('{next})) })
    }
  }
  var prev = 0
  def next: Int = {
    prev += 1
    prev
  }
}
