class X[T] {
  type Id
}
object A extends X[B]
class B(id: A.Id)
