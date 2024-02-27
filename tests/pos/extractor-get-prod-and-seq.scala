// Both a named-based and Seq
case class Cons[+A <: AnyRef](a1: A, as: Seq[A]) extends Seq[A]:
  def apply(i: Int): A      = ???
  def length: Int           = ???
  def iterator: Iterator[A] = ???

object Foo1:
  def unapply(any: Any): Option[Cons[String]] = None

class Test:
  def t1(x: Any): Unit = x match
    case Cons(x, xs) =>
      val a1: AnyRef      = x
      val as: Seq[AnyRef] = xs

  def t2(x: Any): Unit = x match
    case Foo1(x, xs) => // was: Wrong number of argument patterns
      val s1: String      = x
      val ss: Seq[String] = xs
