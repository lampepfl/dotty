inline trait A:
  val x: Int = 1

class B extends A:
  override val x = 2

@main def Test =
  val b = B()
  println(b.x)
