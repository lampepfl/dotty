trait A:
  type T
object a extends A:
  type T = Int

trait B(tracked val b: A):
  type T = b.T

trait C(tracked val c: A):
  type T = c.T

class D extends B(a), C(a):
  val x: T = 2



