inline trait IT:
  def i: Int = 1
  def f[T](x: T): T = x

abstract class AC extends IT:
  override def i: Int = 123456
  def j: Int
