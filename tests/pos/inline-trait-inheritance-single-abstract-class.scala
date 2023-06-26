inline trait IT:
  def i: Int = 1
  def f[T](x: T): T = x

abstract class AC extends IT:
  def j: Int
