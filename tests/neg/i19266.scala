object i19266:
  def fn1(x: Int, y: String = "y")(z: Double) = Some(s"$x$y$z")
  def fn2(p: Int)(q: String) = Some(s"$p$q")
  def fn3(x: Int, y: => String = "y")(z: Double) = Some(s"$x$y$z")

  def checkCompile =
    // It compiles because default value for by-value
    // parameter is impure and may perform side effect.
    fn1(1) // warn
    // This does not compile because (pure) synthesized lambda from
    // eta-expansion in statement position is prohibited.
    // See https://github.com/lampepfl/dotty/pull/11769
    fn2(2) // error
    // This compiles.
    val a = fn2(3)
    // This does not compile because default value for by-name parameter
    // is still pure. Thus, it also violates the rule for lambda in 
    // statement position.
    fn3(4) // error
    1