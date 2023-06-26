inline trait A:
  given String = "AAA"
  def foo(using s: String): String = s + s

class B extends A:
  def f = foo