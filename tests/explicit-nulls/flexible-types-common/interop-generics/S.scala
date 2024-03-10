class S {
  val j = new J()

  // Check that the inside of a generic type is correctly nullified
  val x1: I[String] | Null = j.foo("hello") //ok
  val x2: I[String] = j.foo("hello") // error
  val x3: I[String | Null] = j.foo("hello") // error

  val y1: Array[I[String] | Null] = j.bar[String](null) // error
  val y2: Array[I[String]] | Null = j.bar[String](null) // error
  val y3: Array[I[String] | Null] | Null = j.bar[String](null)
}
