inline trait A:
  def x(foo: Int)(bar: Int) =
    foo + bar

  def y(foo: Int) = x(foo)(foo)

class B extends A:
  def f = x(1)(2)
