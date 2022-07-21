import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.core.Types.*

object Patterns {
  val d: Object = null.asInstanceOf[Object]
  private def rebase(tp: NamedType): Type = {
    def rebaseFrom(prefix: Type): Type = ???
    tp.prefix match {
      case SkolemType(rt) => rebaseFrom(rt)
      case pre: ThisType => rebaseFrom(pre)
      case _ => tp
    }
  }
  d match {
    case WildcardType(bounds: TypeBounds) => bounds.lo
    case a @ Assign(Ident(id), rhs) => id
    case a: Object => a
  }

  ('1', "1") match {
    case (digit, str) => true
    case _ => false
  }


  def foo2(x: AnyRef) = x match { case x: Function0[Any] => x() }
  object Breakdown {
    def unapplySeq(x: Int): Some[List[String]] = Some(List("", "there"))
  }

  object Test2 {
    42 match {
      case a@Breakdown(f@"") =>  // needed to trigger bug
      case b@Breakdown(d@"foo") =>  // needed to trigger bug
      case c@Breakdown(e@"", who) => println ("hello " + who)
    }
  }

  val names = List("a", "b", "c")
  object SeqExtractors {
    val y = names match {
      case List(x, z) => x
      case List(x) => x
      case List() => ""
      case x @ _ => "wildcard"
    }
    val yy: String = y
  }



  val xs = List('2' -> "ABC", '3' -> "DEF")

  xs filter {
    case (digit, str) => true
    case _ => false
  }

  (xs: Any) match {
    case x: Int @unchecked => true
    case xs: List[Int @ unchecked] => true
    case _ => false
  }

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case x :: xs1 => x + sum(xs1)
  }

  def len[T](xs: List[T]): Int = xs match {
    case _ :: xs1 => 1 + len(xs1)
    case Nil => 0
  }

  final def sameLength[T](xs: List[T], ys: List[T]): Boolean = xs match {
    case _ :: xs1 => xs1.isEmpty
      ys match {
        case _ :: ys1 => sameLength(xs1, ys1)
        case _ => false
      }
    case _ => ys.isEmpty
  }

  class A{
    class B
  }
  val a1 = new A
  val a2 = new A
  d match {
    case t: a1.B =>
      t
    case t: a2.B =>
      t
  }

  class caseWithPatternVariableHelper1[A]
  class caseWithPatternVariableHelper2[A]

  def caseWithPatternVariable(x: Any) = x match {
    case a: caseWithPatternVariableHelper1[_] => ()
    case b: caseWithPatternVariableHelper2[_] => ()
  }

}

object NestedPattern {
  val xss: List[List[String]] = ???
  val List(List(x)) = xss
}

// Tricky case (exercised by Scala parser combinators) where we use
// both get/isEmpty and product-based pattern matching in different
// matches on the same types.
object ProductAndGet {

  trait Result[+T]
  case class Success[+T](in: String, x: T) extends Result[T] {
    def isEmpty = false
    def get: T = x
  }
  case class Failure[+T](in: String, msg: String) extends Result[T] {
    def isEmpty = false
    def get: String = msg
  }

  val r: Result[Int] = ???

  r match {
    case Success(in, x) => x
    case Failure(in, msg) => -1
  }

  r match {
    case Success(x) => x
    case Failure(msg) => -1
  }
}
