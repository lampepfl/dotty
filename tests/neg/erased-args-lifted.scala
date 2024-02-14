//> using options -language:experimental.erasedDefinitions

object Test {
  def foo(a: Int)(b: Int, c: Int) = 42
  erased def bar(erased i: Int): Int = {
    println(1)
    42
  }
  def baz: Int = {
    println(1)
    2
  }
  foo(
    bar(baz) // error
  )(
    c = baz, b = baz  // force all args to be lifted in vals befor the call
  )
}
