//> using options -language:experimental.erasedDefinitions

class IntDeco(x: Int) extends AnyVal {
  def foo(erased y: Int) = x
}
