inline trait A:
  lazy val x =
    throw new Exception
    1

class B extends A

@main def Test: Unit =
  val b = B()