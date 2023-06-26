inline trait IT:
  def i: Int = 1
  def f[T](x: T): T = x

object O extends IT
