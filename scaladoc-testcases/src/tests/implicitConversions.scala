package tests

package implicitConversions

given Conversion[A, B] with {
  def apply(a: A): B = ???
}

extension (a: A)
  @annotation.nowarn
  def extended_bar(): String = ???

class A {
  implicit def conversion(c: C): D = ???
  implicit def conversion: Conversion[C,D] = ???
  implicit val a: Conversion[C,D] = ???

  extension (c: C) def extended_bar(): String = ???

  class C {
    def bar: String = ???
  }

  class D extends E() {
    def bar2: String = ???

    val string: String = ???

    class Bar()

    type ImplicitType >: String

    extension (e: E) def extended_bar(): String = ???
  }

  class E {
    def inherited: Int = ???
  }
}

class B {
  def foo: Int = ???

  var b: String = ???
}

class C {
  def extensionInCompanion: String = ???
}
@annotation.nowarn // extensionInCompanion
object C {
  implicit def companionConversion(c: C): B = ???

  extension (c: C) def extensionInCompanion: String = ???
}

package nested {
  extension (opt: Opt[Int]) def sum: Int = ???
  class Opt[A]

  class Lst[A]
  object Lst {
    extension (lst: Lst[Int]) def sum: Int = ???
  }

  object Wrapper {
    class Foo
    class Bar {
      def bar = "bar"
    }
    implicit def foobar(foo: Foo): Bar = Bar()
  }

  class Z
}
