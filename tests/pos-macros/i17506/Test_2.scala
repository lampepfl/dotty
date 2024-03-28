@main def run() =
  def typeParamFun2[A, B](a: A, b: B): Unit = println(a.toString + " " + b.toString)
  def typeParamFun3[A, B, C](a: A, b: B, c: C): Unit = println(a.toString + " " + b.toString)

  regularCurriedCtxFun2BetaReduceTest((f: Foo) ?=> (b: Bar) ?=> 123)
  regularCurriedCtxFun2BetaReduceTest(123)
  regularCurriedFun2BetaReduceTest(((f: Foo) => (b: Bar) => 123))
  typeParamCurriedFun2BetaReduceTest([X] => (x: X) => [Y] => (y: Y) => typeParamFun2[Y, X](y, x))

  regularCurriedFun3BetaReduceTest((f: Foo) => (b: Bar) => (i: Baz) => 123)
  typeParamCurriedFun3BetaReduceTest([X] => (x: X) => [Y] => (y: Y) => [Z] => (z: Z) => typeParamFun3[Z, Y, X](z, y, x))
