import scala.quoted.*
import Macros.*

object Test {
  def main(args: Array[String]): Unit = {
    val seq = IndexedSeq.tabulate[Int](21)(x => x)
    unrolledForeach(seq, (x: Int) => println(2*x), 3)
  }
}
