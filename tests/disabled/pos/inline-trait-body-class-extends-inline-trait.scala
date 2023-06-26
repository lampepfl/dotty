inline trait A:
  class Inner extends Trait[Int]:
    val x = 1

inline trait Trait[T]:
  def f(x: T): T = x

class B extends A:
  val inner = Inner()
  def x = inner.x
  def f = inner.f(x)