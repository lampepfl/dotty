inline trait A:
  implicit val x: String = "AAA"
  def foo(implicit s: String): String = s + s

class B extends A:
  def f = foo