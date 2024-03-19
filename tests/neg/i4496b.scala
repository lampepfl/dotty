import scala.reflect.Selectable.reflectiveSelectable

trait Foo1 { val a: Int }
trait Foo2 { def a: Int }
trait Foo3 { var a: Int }

object TestStructuralVar {
  type T0 = {var a: Int}
  object TestStructuralVar {
    type T = {val a: Int; def a_=(x: Int): Unit}
    def upcast1(v: Foo1): T = v // error
    def upcast2(v: Foo2): T = v // error
    def upcast3(v: Foo3): T = v // error
    def verify(v: T) = ()
    def test(): Unit = {
      verify(upcast1(new Foo1 { val a = 10 }))
      verify(upcast2(new Foo2 { val a = 10 }))
      verify(upcast3(new Foo3 { var a = 10 }))
    }
  }

  def main(args: Array[String]): Unit = {
    TestStructuralVar.test()
  }
}
