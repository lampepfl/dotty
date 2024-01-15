// like tests/pos/i15177.scala
// but with T having an upper bound
// that B doesn't conform to
// just to be sure that not forcing B
// doesn't backdoor an illegal X[B]
class X[T <: C] {
  type Id
}
object A
  extends X[ // error
    B] // error
class B(id: A.Id)
class C
