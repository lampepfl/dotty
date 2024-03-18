object ReachableUnreachableCase {

  sealed trait SuperTrait {
    type Self
  }

  trait SubTrait extends SuperTrait

  case class Foo() extends SubTrait {
    type Self = Foo
  }

  def printError[T <: SuperTrait { type Self = T }](x: Either[String, T]): Unit = {
    x match {
      case Right(_) => println("No error found")
      case Left(message) => println(message)
    }
  }

  @main def main(): Unit = {
    printError(Right(Foo()))
    printError(Left("Error!"))
  }
}
