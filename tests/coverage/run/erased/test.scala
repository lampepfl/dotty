import scala.language.experimental.erasedDefinitions

erased def parameterless: String = "y"

erased def e(erased x: String): String = "x"
def foo(erased a: String)(b: String): String =
  println(s"foo(a)($b)")
  b

def identity(s: String): String =
  println(s"identity($s)")
  s

@main
def Test: Unit =
  foo(parameterless)("b")
  foo(e("a"))("b")
  foo(e("a"))(identity("idem"))
