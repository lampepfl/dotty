object i19266:
  def fn1(x: Int, y: String = "y")(z: Double) = Some(s"$x$y$z")

  def checkWarning: Int =
    fn1(1) // warn
    1
