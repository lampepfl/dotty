trait Expr[T]
trait Liftable[T]

object test1 {
  class ToExpr[T] with Liftable[T] extends Conversion[T, Expr[T]] {
    def apply(x: T): Expr[T] = ???
  }
  given toExpr[T] with Liftable[T] as ToExpr[T]

  given Liftable[Int] = ???
  given Liftable[String] = ???

  def x = summon[ToExpr[String]]
  def y = summon[Conversion[String, Expr[String]]]

  def a: Expr[String] = "abc"
}

object test2 {

  given autoToExpr[T] with Liftable[T] as Conversion[T, Expr[T]] {
    def apply(x: T): Expr[T] = ???
  }

  given Liftable[Int] = ???
  given Liftable[String] = ???

  def a: Expr[String] = "abc"
}