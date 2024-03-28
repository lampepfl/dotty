//> using options -Yno-experimental

class Test0 {
  import language.experimental.namedTypeArguments // error
  object Foo {
    inline def f[S, T](x: S): T = ???
    def g(x: Int) = f[T = Any](x)
  }
}

class Test1 {
  import scala.language.experimental.erasedDefinitions // error
  import scala.compiletime.erasedValue
  type UnivEq[A]
  object UnivEq:
    erased def force[A]: UnivEq[A] = erasedValue
    extension [A](erased proof: UnivEq[A])
      inline def univEq(a: A, b: A): Boolean =
        a == b
}

class Test2 {
  import _root_.scala.language.experimental.{genericNumberLiterals, namedTypeArguments => _} // error
  val x: BigInt = 13232202002020202020202
  val y: BigInt = -0xaabb12345ACF12345AC
}

class Test6 {
  import scala.language.experimental  // ok
}
