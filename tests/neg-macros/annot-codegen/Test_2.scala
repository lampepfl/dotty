@data class Bar(val one: String, val two: Int, val three: Int, val four: Double): // error: additional methods needed
  // This definition is OK and will be left as is
  def withOne(one: String): Bar =
    data.generated[Bar]()

  def withTwo(two: Int): Bar =
    new Bar("", two, 1, 1.0) // error: body needs to be replaced

  def withThree(three: Int): Bar // error: body needs to be data.generate[Bar]()

@data class Baz[T](val one: T, val two: Int, val three: Int, val four: Int):
  def withOne(one: T): Baz[T] = ??? // error
  def withTwo(two: Int): Baz[T] // error
  def withThree(three: Int): Baz[T] = data.generated[Baz[T]]() // error // FIXME: should be supported
  def withFour(four: Int): Baz[T] = data.generated[Nothing]() // error // FIXME: wrong error
