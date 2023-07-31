class S {
  val j = new J()
  // Check that nullable and non-nullable work
  val x: I[String] | Null = j.foo("hello")
  val y: Array[I[String] | Null] | Null = j.bar[String](null)
  val x2: I[String] = j.foo("hello")
  val y2: Array[I[String] | Null] = j.bar[String](null)
  val y3: Array[I[String]] = j.bar[String](null)
}
