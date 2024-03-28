class Foo
class Bar
class Baz

import scala.quoted._

def assertBetaReduction(using Quotes)(applied: Expr[Any], expected: String): quotes.reflect.Term =
  import quotes.reflect._
  val reducedMaybe = Term.betaReduce(applied.asTerm)
  assert(reducedMaybe.isDefined)
  val reduced = reducedMaybe.get
  assert(reduced.show == expected,s"obtained: ${reduced.show}, expected: ${expected}")
  reduced

inline def regularCurriedCtxFun2BetaReduceTest(inline f: Foo ?=> Bar ?=> Int): Unit =
  ${regularCurriedCtxFun2BetaReduceTestImpl('f)}
def regularCurriedCtxFun2BetaReduceTestImpl(f: Expr[Foo ?=> Bar ?=> Int])(using Quotes): Expr[Int] =
  val expected =
    """|{
       |  val contextual$3: Bar = new Bar()
       |  val contextual$2: Foo = new Foo()
       |  123
       |}""".stripMargin
  val applied = '{$f(using new Foo())(using new Bar())}
  assertBetaReduction(applied, expected).asExprOf[Int]

inline def regularCurriedFun2BetaReduceTest(inline f: Foo => Bar => Int): Int =
  ${regularCurriedFun2BetaReduceTestImpl('f)}
def regularCurriedFun2BetaReduceTestImpl(f: Expr[Foo => Bar => Int])(using Quotes): Expr[Int] =
  val expected =
    """|{
       |  val b: Bar = new Bar()
       |  val f: Foo = new Foo()
       |  123
       |}""".stripMargin
  val applied = '{$f(new Foo())(new Bar())}
  assertBetaReduction(applied, expected).asExprOf[Int]

inline def typeParamCurriedFun2BetaReduceTest(inline f: [A] => A => [B] => B => Unit): Unit =
  ${typeParamCurriedFun2BetaReduceTestImpl('f)}
def typeParamCurriedFun2BetaReduceTestImpl(f: Expr[[A] => (a: A) => [B] => (b: B) => Unit])(using Quotes): Expr[Unit] =
  val expected =
    """|{
       |  type Y = Bar
       |  val y: Bar = new Bar()
       |  type X = Foo
       |  val x: Foo = new Foo()
       |  typeParamFun2[Y, X](y, x)
       |}""".stripMargin
  val applied = '{$f.apply[Foo](new Foo()).apply[Bar](new Bar())}
  assertBetaReduction(applied, expected).asExprOf[Unit]

inline def regularCurriedFun3BetaReduceTest(inline f: Foo => Bar => Baz => Int): Int =
  ${regularCurriedFun3BetaReduceTestImpl('f)}
def regularCurriedFun3BetaReduceTestImpl(f: Expr[Foo => Bar => Baz => Int])(using Quotes): Expr[Int] =
  val expected =
    """|{
       |  val i: Baz = new Baz()
       |  val b: Bar = new Bar()
       |  val f: Foo = new Foo()
       |  123
       |}""".stripMargin
  val applied = '{$f(new Foo())(new Bar())(new Baz())}
  assertBetaReduction(applied, expected).asExprOf[Int]

inline def typeParamCurriedFun3BetaReduceTest(inline f: [A] => A => [B] => B => [C] => C => Unit): Unit =
  ${typeParamCurriedFun3BetaReduceTestImpl('f)}
def typeParamCurriedFun3BetaReduceTestImpl(f: Expr[[A] => A => [B] => B => [C] => C => Unit])(using Quotes): Expr[Unit] =
  val expected =
    """|{
       |  type Z = Baz
       |  val z: Baz = new Baz()
       |  type Y = Bar
       |  val y: Bar = new Bar()
       |  type X = Foo
       |  val x: Foo = new Foo()
       |  typeParamFun3[Z, Y, X](z, y, x)
       |}""".stripMargin
  val applied = '{$f.apply[Foo](new Foo()).apply[Bar](new Bar()).apply[Baz](new Baz())}
  assertBetaReduction(applied, expected).asExprOf[Unit]
