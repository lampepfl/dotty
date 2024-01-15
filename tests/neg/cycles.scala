class Foo[T <: U, U <: T] // error: illegal cyclic reference: upper bound U of type T refers back to the type itself

class Bar[T >: T] // error: illegal cyclic reference: lower bound T of type T refers back to the type itself


class A {
  val x: T = ???
  type T <: x.type // error: cyclic reference involving value x
}

class B {
  type T <: x.type // error: illegal cyclic reference: upper bound B.this.T(B.this.x) of type T refers back to the type itself
  val x: T = ???
}

class C {
  final val x: D#T = ???
  class D {
    type T <: x.type // error: cyclic reference involving value x
    val z: x.type = ???
  }
}

class E {
  class F {
    type T <: x.type // error
    val z: x.type = ??? // old-error: not stable
  }
  lazy val x: F#T = ???
}

class T1 {
  type X = (U, U)
  type U = X & Int // error: cycle
}
class T2 {
  type X = (U, U)
  type U = X | Int // error: cycle
}
object T12 {
  val _ : (T1 {})#U = ??? // old-error: conflicting bounds
  ??? : (T2 {})#U // old-error: conflicting bounds
}
