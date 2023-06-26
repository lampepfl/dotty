inline trait A(val x: Int)

class B extends A({ println("I am a B!"); 1 })

@main() def Test: Unit = {
  val b = B()
  println(b.x)
  println(b.x)
  println(b.x)
}