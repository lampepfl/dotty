inline trait A[T](x: T):
  def f: T = x
  def g(a: T): T = a
  def h: T
  val i: T = x
  val j: T
  var k: T = x

  inline def b(a: T): T = x
