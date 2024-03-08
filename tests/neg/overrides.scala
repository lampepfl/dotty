//> using options -experimental

class Foo {
  type A = Int
  type B >: Int <: Int
  def get: A = 42
}
trait T {
  lazy val x: Int
  val y: Int
}
class Bar extends Foo with T {
  override type A = Any   // error
  type B >: String <: Any  // error
  override def get: A = "bar"
  val x = 2  // error
  lazy val y = 3 // error
}
object Test {
  def main(args: Array[String]): Unit = {
    val foo: Foo = new Bar
    val i: Int = foo.get
  }
}

package p1 {
  abstract class T1 {
    protected def bug(p: Int = 1): Int // without 'protected' compiles fine
  }
}
package p2 { // all being in the same package compiles fine
  import p1.*
  abstract class T2 extends T1 {
    class A {
      bug()
    }
  }

}

class A[T] {

  def f(x: T)(y: T = x) = y

  def next: T = ???

  import scala.language.experimental.clauseInterleaving

  def b[U <: T](x: Int)[V >: T](y: String) = false
}

class B extends A[Int] {

  def f(x: Int)(y: Int) = y // error: needs `override' modifier

  f(2)()

  override def next(): Int = ???    // error: incompatible type

  import scala.language.experimental.clauseInterleaving

  override def b[T <: Int](x: Int)(y: String) = true // error
}

class C extends A[String] {

  override def f(x: String) = x // error

  override def next: Int = ???    // error: incompatible type

  import scala.language.experimental.clauseInterleaving

  override def b[T <: String](x: Int)[U >: Int](y: String) = true // error: incompatible type
}

class X {
  def f: A[Int] = ???
}
class Y extends X {
  def f: A[Int] = ??? // error: needs `override' modifier
}


class A1
class B1

class X1 {
  def f(): A1 = ???
}
class Y1 extends X1 {
  override def f(): B1 = ??? // error: has incompatible type
}

class X2 {
  type T = A1
}
class Y2 extends X2 {
  type T = B1 // error: needs `override' modifier
}

class X3 {
  override type T = A1 // error: overrides nothing
}

package p4 {

  abstract class C[T] { def head: T }
  case class D[T](head: Int) extends C[T] // error: has incompatible type

}

package p5 {
class A {
  def m: String = "foo"
}

class B extends A {
  override val m: Int = 42 // error: has incompatible type
}

class C extends A {
  override def m: Int = 42 // error: has incompatible type
}
}

package p6 {
  class A { def apply(xs: Int*) = 42 }
  class B extends A { override def apply(xs: Seq[Int]) = 42 } // error
}
package p7 {
  class A { def apply(xs: Int*) = 42 }
  class B extends A { def apply(xs: Seq[Int]) = 42 } // error
}
package p8 {
  class A { def apply(xs: Seq[Int]) = 42 }
  class B extends A { override def apply(xs: Int*) = 42 } // error
}
package p9 {
  class A { def apply(xs: Seq[Int]) = 42 }
  class B extends A { def apply(xs: Int*) = 42 } // error
}
package p10 {
  class A { def apply(s: String)(xs: Int*) = 42 }
  class B extends A { def apply(s: String)(xs: Seq[Int]) = 42 } // error
}
