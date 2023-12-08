import a.A
import b.B

object C extends App {
  assert(A.f(0) == 1)
  assert(A.g(0) == 1)
  assert(B.f(0) == 2)
  assert(B.g(0) == 2)
}
