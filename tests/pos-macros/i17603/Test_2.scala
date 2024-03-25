import Macros.given

case class Mappable[A](value: A):
  def map[B](f: Location => B)(using loc: Location): Mappable[B] = Mappable(f(loc))

@main
def main(): Unit =
  val position = for {
    p <- Mappable(3)










  } yield p
