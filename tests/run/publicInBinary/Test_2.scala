//> using options -experimental
import foo.*

@main def Test: Unit =
  val foo: Foo = new Foo(1, 2)
  foo.foo

  val bar = new Bar()
  bar.foo
  bar.bar

  val baz = new Baz()
  baz.foo
  baz.baz

  val qux = new Qux()
  qux.foo
  qux.qux

  val c = new inlines.TestPassing
  c.foo(1)
  c.bar(2)

  testFoo()
  localTest()
  traits.test()
  testConstructors()
