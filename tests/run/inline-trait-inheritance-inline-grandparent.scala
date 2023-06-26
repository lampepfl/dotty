package simpleGrandParent:
  inline trait SimpleGrandParent[T]:
    def foo(): Int = 0
    def foooo(): T = ???

  inline trait SimpleParent[T, U](x: T, z: U) extends SimpleGrandParent[U]:
    def bar(a: T) = (a, x)

  class SimpleC extends SimpleParent("Hello", 1234)

package grandParentWithArgs:
  inline trait GrandParent[T](val x: T, val y: T):
    def foo(): T = x
    def foooo(): T = y

  inline trait Parent[T, U](x: T, z: U) extends GrandParent[U]:
    def bar(a: T) = (a, x, y)

  class C extends Parent("Hello", 1234), GrandParent(5678, 9)

@main def Test =
  import simpleGrandParent.SimpleC
  import grandParentWithArgs.C

  val simpleC = SimpleC()
  println(simpleC.foo())
  println(simpleC.bar("Test SimpleC"))
  println

  val c = C()
  println(c.foo())
  println(c.bar("Test C"))
  println(c.x)