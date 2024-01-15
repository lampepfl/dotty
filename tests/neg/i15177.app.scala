// like tests/pos/i15177.scala
// but with an applied type B[D]
class X[T] {
  type Id
}
object A extends X[B[D]]
class B[C](id: A
  .Id) // error
class D
