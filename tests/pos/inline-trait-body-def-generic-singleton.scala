inline trait A:
  def foo: 3 = 3
  def bar[T](x: T): T = x

class B extends A:
  def f: "A" = bar("A")
  def g: 3 = foo