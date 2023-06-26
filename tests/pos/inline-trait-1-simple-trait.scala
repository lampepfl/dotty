inline trait A:
  type X = String

  val x: Int = 3
  val y: Int = x + z
  private val z: Int = 1

  def f: Int = g
  def f(x: Int): Int = x
  def f[T](x: T): Int = 2

  private def g = 1
  protected[this] def p = 123
  private[this] def pp = 123456

  def xx: X = "foo".asInstanceOf[X]
end A

class B extends A:
  /*
  <generated> override type X = String

  <generated> override val x: Int = 3
  <generated> override val y: Int = this.x.+(this.z)

  <generated> private[this] val z: Int = 1

  <generated> override def f: Int = this.g
  <generated> override def f(x: Int): Int = x
  <generated> override def f[T](x: T): Int = 2

  <generated> private[this] def g: Int = 1

  <generated> override def xx: X = "foo".asInstanceOf[X]
  */
end B
