trait A {
  type D >: Null <: C
  def foo(d: D)(d2: d.type): Unit
  trait C {
    def bar: Unit = {
      val nul = null
      foo(nul)(nul)
    }
  }
}
object B extends A {
  class D extends C

  def foo(d: D)(d2: d.type): Unit = () // Bridge method required here!
}

object Test extends App {
  new B.D().bar
}
