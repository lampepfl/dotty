object Test {
  object EssaInt {
    def unapply(i: Int): Some[Int] = Some(i)
  }

  def foo1[T](t: T): T = t match {
    case EssaInt(_) =>
      0 // error
  }

  case class Inv[T](t: T)

  def bar1[T](t: T): T = Inv(t) match {
    case Inv(EssaInt(_)) =>
      0 // error
  }
}
