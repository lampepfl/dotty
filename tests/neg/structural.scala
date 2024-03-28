object Test3 {
  import scala.reflect.Selectable.reflectiveSelectable
  def g(x: { type T ; def t: T ; def f(a: T): Boolean }) = x.f(x.t) // error: it has a parameter type with an unstable erasure
  g(new { type T = Int; def t = 4; def f(a:T) = true })
  g(new { type T = Any; def t = 4; def f(a:T) = true })
  val y: { type T = Int; def t = 4; def f(a:T) = true }  // error: illegal refinement // error: illegal refinement
    = new { type T = Int; def t = 4; def f(a:T) = true }

  def h(x: { def f[T](a: T): Int }) = x.f[Int](4) // error: polymorphic refinement method ... no longer allowed // error: Structural access not allowed

  type A = { def foo(x: Int): Unit; def foo(x: String): Unit } // error: overloaded definition // error: overloaded definition
  type B = { val foo: Int; def foo: Int } // error: duplicate foo

  type C = { var foo: Int }

  trait Entry { type Key; val key: Key }
  type D = { def foo(e: Entry, k: e.Key): Unit }
  val e = new Entry { type Key = Int; val key = 0 }
  def i(x: D) = x.foo(e, 1) // error: foo has dependent params

  type G = { def foo(x: Int, y: Int): Unit }
  def j(x: G) = x.foo(???) // error: missing argument

  class H { type S = String; type I }
  class I extends H { type I = Int }
  type Dep = {
    def fun1(x: H, y: x.S): Int
    def fun2(x: H, y: x.I): Int
    def fun3(y: H): y.S
    def fun4(y: H): y.I
  }
  def k(x: Dep) = {
    val y = new I
    x.fun1(y, "Hello")
    x.fun2(y, 1) // error
    x.fun3(y)
    x.fun4(y) // error
  }
}
