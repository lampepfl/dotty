package b

import a.A

class B[T] {
  val inner = new A[T](23) {}
}

object B {

  val someAny: Any = 23

  val inner = (new B[Int]).inner

  @main def test = {
    val derived: Int = inner.value
    assert(derived == 23, s"actually was $derived")
    assert(inner.hash(someAny) == someAny.hashCode, s"actually was ${inner.hash(someAny)}")
  }
}

