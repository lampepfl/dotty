inline trait A:
  def x(foo: Int, bar: Unit) =
    bar
    foo

class B extends A:
  def f = x(1, ())