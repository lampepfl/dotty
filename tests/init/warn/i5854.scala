class B {
  val a: String = (((1: Any): b.A): Nothing): String
  val b: { type A >: Any <: Nothing } = loop() // warn
  def loop(): Nothing = loop()
}
