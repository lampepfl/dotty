package tests.methodsAndConstructors

import scala.language.experimental.clauseInterleaving

class A
class B extends A
class C
class D[T]
class E[T] extends D[T]

class Constructors(a: String):
  def this()
  = this("Ala")

  def this(a: A)(b: A)
  = this("Ala")

/** Some methods to tests */
class Methods:
 def nobraces: A
 = ???

 def simple(): B
 = ???

 def oneParam(a: A): B
  = ???

 def multipleParams(a: A, b: B): C
 = ???

 def vararg(a: A*): C
 = ???

 def multipleList(a: A)(b: B): C
 = ???

 def generic[T](a: D[T]): D[T]
 = ???

 def generic2[T, V](a: D[T], b: E[V]): D[T]
 = ???

 def primitives(a: Int, b: Double, c: Short): Byte
 = 0

 def strings(a: String): String
 = ""

 def arrays(a: Array[String], b: Array[Int]): Array[Double]
 = ???

 def rightA1(a: Int): Int
 = ???

 def ++:(a: Int)(b: Double): Int
 = ???

 def withImplicitParam(implicit a: Int): Int
 = ???

 def withImplicitParam2(v: String)(implicit ab: Double, a: Int, b: String): String
 = ???

 def clauseInterleaving[T](x: T)[U](y: U)(using (T, U)): (T, U)
 = ???

