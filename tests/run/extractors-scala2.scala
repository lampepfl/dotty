import scala.util.{ Try, Success, Failure }

object Scala2CaseClassTest:
  def t1(tri: Try[(String, Int)]) = tri match
    case Success((word, num)) => println(s"word length ${word.length} num + 1 = ${num + 1}")
    case Failure(exc)         => println(s"exc name: ${exc.getClass.getName}")

  def main(args: Array[String]): Unit =
    t1(Success(("foo", 3)))
    t1(Failure(new Exception()))
    ()

package scala.collection.immutable {
  import IntMap.*

  object IntMapTest:
    def t1[A](xs: IntMap[A]) = xs match
      case Tip(k, v) =>
        val key: Int = k
        val value: A = v
        println(s"key=$key value=$value")
      case Bin(p, m, l, r) =>
        val prefix: Int = p
        val mask: Int = m
        val left: IntMap[A] = l
        val right: IntMap[A] = r
        println(s"prefix=$prefix mask=$mask left=$left right=$right")
      case Nil =>
        println("nil")
      //case _ =>

    def main(args: Array[String]): Unit =
      t1(Nil)
      t1(Tip(1, "one"))
      t1(Bin(-1, 1111, Tip(2, "two"), Tip(3, "three")))
      ()
}

object Test:
  def main(args: Array[String]): Unit =
    Scala2CaseClassTest.main(args)
    scala.collection.immutable.IntMapTest.main(args)
