inline trait A(a: Int):
  def f: Int = a
  def g(b: Int): Int = a + b
end A

class B extends A(4):
  /*
  <generated> private val a: Int = 4
  <generated> override def f: Int = this.a
  <generated> override def g(x: Int): Int = this.a.+(b)
  */
end B
