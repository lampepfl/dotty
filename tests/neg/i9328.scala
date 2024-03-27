// fuzz test to avoid NoSymbol.owner
type Id[T] = T match {
  case _ => T
}

class Foo2[T <: Id[T]] // error

object Foo { // error
  object Foo { }
  Foo { } // error
}
implicit class Foo(a: Float)  // error
case class Foo()

case class Bar(
} { ;  // error
object Bar {
  class Foo(a: Int) extends AnyVal
  Foo()
}
type Bar