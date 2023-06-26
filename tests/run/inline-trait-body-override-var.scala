inline trait A:
  var x: Int = 1

class B extends A:
  override var x = 2

@main def Test =
  val b = B()
  println(b.x)
