def m = {
  val j: J = ???

  def f1(x: String | Null): Array[String | Null] | Null = null

  def f2(i: Int): Unit = ()

  j.g1(f1)
  j.g1((_: String | Null) => null)
  j.g1(null)

  j.g2(f2)
  j.g2((_: Int) => ())
  j.g2(null)

  j.h1(f1)
  j.h1((_: String | Null) => null)
  j.h1(null)
}