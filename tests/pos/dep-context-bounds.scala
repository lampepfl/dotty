//> using options -language:experimental.modularity -source future
trait A:
  type Self

object Test1:
  def foo[X: A](x: X.Self) = ???

  def bar[X: A](a: Int) = ???

  def baz[X: A](a: Int)(using String) = ???

object Test2:
  def foo[X: A as x](a: x.Self) = ???

  def bar[X: A as x](a: Int) = ???

  def baz[X: A as x](a: Int)(using String) = ???
