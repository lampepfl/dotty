sealed trait A
case class X(i: Int) extends A

object Test extends App {
  TestMacro.test[A] // error
}
