package a

object AConsumeTransparent {
  def thirtyTwo: Double = A.transparentPower(2.0, 5) // cause a suspension in typer
}
