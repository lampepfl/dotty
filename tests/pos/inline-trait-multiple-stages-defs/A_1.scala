inline trait A(x: Int):
  def f: Int = 1
  def g(a: Int): Int = 2
  def h: Int
  val i: Int = 3
  val j: Int
  var k: Int = 4

  inline val a = 5
  inline def b(a: Int): Int = 6
