//> using options -language:experimental.erasedDefinitions

object Test {
  def foo0(a: Int): Int = a
  def foo1(erased a: Int): Int = {
    foo0(
      a // error
    )
    foo0({
      println()
      a // error
    })
    foo1(a) // OK
    foo2( // error
      a // Ok
    )
    a // error
  }
  erased def foo2(erased a: Int): Int = {
    foo0(a) // OK
    foo1(a) // OK
    foo2(a) // OK
    a // OK
  }
}
