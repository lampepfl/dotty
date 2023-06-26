inline trait A[T](x: T):
  def f: T = x: T
  def g: T = identity[T](x)
  def h: this.type = this: this.type

class B extends A("Hello")
