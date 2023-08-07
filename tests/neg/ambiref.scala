object test1:

  class C:
    val x = 0
  object Test:
    val x = 1
    class D extends C:
      println(x)  // error
    new C:
      println(x)  // error

object test2:
  def c(y: Float) =
    class D:
      val y = 2
    new D:
      println(y)  // error

object test3:
  def c(y: Float) =
    class D:
      val y = 2
    class E extends D:
      class F:
        println(y)  // error

object test4:

  class C:
    val x = 0
  object Test:
    val x = 1
    class D extends C:
      def x(y: Int) = 3
      val y: Int = this.x // OK
      val z: Int = x      // OK
end test4

val global = 0
class C:
  val global = 1
object D extends C:
  println(global)    // error

package p:
  class T
  trait P { trait T }
  class C extends P:
    def t = new T { } // error

package scala:
  trait P { trait Option[+A] }
  class C extends P:
    def t = new Option[String] { } // OK, competing scala.Option is not defined in the same compilation unit

object test5:
  class Mu // generates a synthetic companion object with an apply method
  trait A {
    val Mu = 1
  }
  trait B extends A {
    def t = Mu // don't warn about synthetic companion
  }
