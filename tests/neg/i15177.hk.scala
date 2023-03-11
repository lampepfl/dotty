// like tests/pos/i15177.scala
// but with B being higher kinded
class X[T[_]] {
  type Id
}
object A extends X[B]
class B[C](id: A
  .Id) // error: type Id is not a member of object A
