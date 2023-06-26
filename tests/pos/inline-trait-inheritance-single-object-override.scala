inline trait IT:
  def i: Int = 1
  def f[T](x: T): T = x

object O extends IT:
  override def i: Int = 123456
