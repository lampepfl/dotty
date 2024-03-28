type F <: F = 1 match { // error
  case _ => foo.foo // error
}
def foo(a: Int): Unit = ???
