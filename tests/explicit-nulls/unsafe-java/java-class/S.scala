import scala.language.experimental.unsafeJavaReturn

import scala.annotation.CanEqualNull
import java.{util => ju}

def test[T <: AnyRef](j: J) = {
  val a: Int = j.a

  val b = j.b // it returns String @CanEqualNull
  val b2: String = b
  val b3: String @CanEqualNull = j.b
  val b4: String = j.b
  val bb = j.b == null // it's ok to compare String @CanEqualNull with Null
  val btl = j.b.trim().length() // String @CanEqualNull is just String, unsafe selecting

  val c = j.c
  val cl = c.length
  val c2: Array[String] = c
  val c3: Array[String @CanEqualNull] @CanEqualNull = j.c
  val c4: Array[String] = j.c
  val cml: Array[Int] = c.map(_.length())

  val f1: Int = j.f1()

  val f21: Array[Int] @CanEqualNull = j.f2()
  val f22: Array[Int] = j.f2()
  val f2n = j.f2() == null

  val g11: String @CanEqualNull = j.g1()
  val g12: String = j.g1()
  val g1n = j.g1() == null
  val g1tl = j.g1().trim().length()

  val g21: ju.List[String] @CanEqualNull = j.g2()
  val g22: ju.List[String] = j.g2()

  val g31: Array[String @CanEqualNull] @CanEqualNull = j.g3()
  val g32: Array[String] = j.g3()
  val g3n = j.g3() == null
  val g3m: Array[Boolean] = j.g3().map(_ == null)

  val h11: T @CanEqualNull = j.h1[T]()
  val h12: T = j.h1[T]()
  val h1n = j.h1[T]() == null

  val h21: ju.List[T] @CanEqualNull = j.h2[T]()
  val h22: ju.List[T] = j.h2[T]()

  val h31: Array[T @CanEqualNull] @CanEqualNull = j.h3[T]()
  val h32: Array[T] = j.h3[T]()
  val h3m = j.h3[T]().map(_ == null)
}
