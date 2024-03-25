case class Mappable[A](value: A):
  def map[B](f: A => B)(using loc: Int): Mappable[B] = Mappable(f(value))

@main
def main(): Unit =
  val position = for {
    p <- Mappable(3) // error
  } yield p
