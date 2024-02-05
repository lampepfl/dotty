//> using options -source future -language:experimental.modularity

trait Id { type Value }
trait X { type Value }
type IdOf[T] = Id { type Value = T }

case class Year(value: Int) extends AnyVal
  with (Id { type Value = Int })
  with Ordered[Year]

class Bar extends IdOf[Int], (X { type Value = String }) // error

class Baz extends IdOf[Int]:
  type Value = String
  val x: Value = 0 // error

val x: IdOf[Int] = Baz() // error

object Clash extends ({ def foo(x: Int): Int }):
  def foo(x: Boolean): Int = 1
  foo(2) // error
