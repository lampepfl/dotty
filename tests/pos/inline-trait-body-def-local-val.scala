inline trait A:
  def f =
    val foo = 1
    foo

class B extends A:
  def g = f