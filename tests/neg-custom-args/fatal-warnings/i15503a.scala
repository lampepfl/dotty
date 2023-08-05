// scalac: -Wunused:imports


object FooUnused:
  import collection.mutable.Set // error
  import collection.mutable.{Map => MutMap} // error
  import collection.mutable._ // error

object FooWildcardUnused:
  import collection.mutable._ // error

object Foo:
  import collection.mutable.Set // OK
  import collection.mutable.{Map => MutMap} // OK

  val bar = Set() // OK
  val baz = MutMap() // OK

object FooWildcard:
  import collection.mutable._ // OK

  val bar = Set() // OK

object FooNestedUnused:
  import collection.mutable.Set // error
  object Nested:
    def hello = 1

object FooNested:
  import collection.mutable.Set // OK
  object Nested:
    def hello = Set()

object FooGivenUnused:
  import SomeGivenImports.given // error

object FooGiven:
  import SomeGivenImports.given // OK
  import SomeGivenImports._ // error

  val foo = summon[Int]

/**
 * Import used as type name are considered
 * as used.
 *
 * Import here are only used as types, not as
 * Term
 */
object FooTypeName:
  import collection.mutable.Set // OK
  import collection.mutable.Map // OK
  import collection.mutable.Seq // OK
  import collection.mutable.ArrayBuilder // OK
  import collection.mutable.ListBuffer // error

  def checkImplicit[A](using Set[A]) = ()
  def checkParamType[B](a: Map[B,B]): Seq[B] = ???
  def checkTypeParam[A] = ()

  checkTypeParam[ArrayBuilder[Int]]


object InlineChecks:
  object InlineFoo:
    import collection.mutable.Set // ok
    import collection.mutable.Map // error
    inline def getSet = Set(1)

  object InlinedBar:
    import collection.mutable.Set // error
    import collection.mutable.Map // error
    val a = InlineFoo.getSet

object MacroChecks:
  object StringInterpol:
    import collection.mutable.Set // OK
    import collection.mutable.Map // OK
    println(s"This is a mutableSet : ${Set[Map[Int,Int]]()}")


object InnerMostCheck:
  import collection.mutable.* // error
  def check =
    import collection.mutable.* //OK
    val a = Set(1)

object IgnoreExclusion:
  import collection.mutable.{Set => _} // OK
  import collection.mutable.{Map => _} // OK
  import collection.mutable.{ListBuffer} // error
  def check =
    val a = Set(1)
    val b = Map(1 -> 2)
/**
  * Some given values for the test
  */
object SomeGivenImports:
  given Int = 0
  given String = "foo"

/* BEGIN : Check on packages*/
package testsamepackageimport:
  package p {
    class C
  }

  package p {
    import p._ // error
    package q {
      class U {
        def f = new C
      }
    }
  }
// -----------------------

package testpackageimport:
  package a:
    val x: Int = 0

  package b:
    import a._ // error


/* END : Check on packages*/

/* BEGIN : tests on meta-language features */
object TestGivenCoversionScala2:
  /* note: scala3 Conversion[U,T] do not require an import */
  import language.implicitConversions // OK

  implicit def doubleToInt(d:Double):Int = d.toInt

  def idInt(i:Int):Int = i
  val someInt = idInt(1.0)

object TestTailrecImport:
  import annotation.tailrec // OK
  @tailrec
  def fac(x:Int, acc:Int = 1): Int =
    if x == 0 then acc else fac(x - 1, acc * x)
/* END : tests on meta-language features */

/* BEGIN : tests on given import order */
object GivenImportOrderAtoB:
  class X
  class Y extends X
  object A { implicit val x: X = new X }
  object B { implicit val y: Y = new Y }
  class C {
    import A._ // error
    import B._ // OK
    def t = implicitly[X]
  }

object GivenImportOrderBtoA:
  class X
  class Y extends X
  object A { implicit val x: X = new X }
  object B { implicit val y: Y = new Y }
  class C {
    import B._ // OK
    import A._ // error
    def t = implicitly[X]
  }
/* END : tests on given import order */

/* Scala 2 implicits */
object Scala2ImplicitsGiven:
  object A:
    implicit val x: Int = 1
  object B:
    import A.given  // OK
    val b = summon[Int]
  object C:
    import A.given  // error
    val b = 1
  object D:
    import A._  // OK
    val b = summon[Int]
  object E:
    import A._  // error
    val b = 1
  object F:
    import A.x  // OK
    val b = summon[Int]
  object G:
    import A.x  // error
    val b = 1

// -------------------------------------
object TestNewKeyword:
  object Foo:
    class Aa[T](val x: T)
  object Bar:
    import Foo.Aa // OK
    val v = 1
    val a = new Aa(v)

// -------------------------------------
object testAnnotatedType:
  import annotation.switch // OK
  val a = (??? : @switch) match
    case _ => ???


//-------------------------------------
package testImportsInImports:
  package a:
    package b:
      val x = 1
  package c:
    import a.b // OK
    import b.x // OK
    val y = x

//-------------------------------------
package testOnOverloadedMethodsImports:
  package a:
    trait A
    trait B
    trait C:
      def foo(x: A):A = ???
      def foo(x: B):B = ???
  package b:
    object D extends a.C
  package c:
    import b.D.foo // error
  package d:
    import b.D.foo // OK
    def bar = foo((??? : a.A))
  package e:
    import b.D.foo // OK
    def bar = foo((??? : a.B))
  package f:
    import b.D.foo // OK
    def bar = foo((??? : a.A))
    def baz = foo((??? : a.B))

//-------------------------------------
package foo.testing.rename.imports:
  import collection.mutable.{Set => MutSet1} // OK
  import collection.mutable.{Set => MutSet2} // OK
  import collection.mutable.{Set => MutSet3} // error
  type A[X] = MutSet1[X]
  val a = MutSet2(1)

//-------------------------------------
package foo.testing.imports.precedence:
  import scala.collection.immutable.{BitSet => _, _} // error
  import scala.collection.immutable.BitSet // OK
  def t = BitSet.empty

package foo.test.enums:
  enum A: // OK
    case B extends A // OK
    case C extends A // OK

package foo.test.typeapply.hklamdba.i16680:
  package foo:
    trait IO[A]

  package bar:
    import foo.IO // OK

    def f[F[_]]: String = "hello"
    def go = f[IO]