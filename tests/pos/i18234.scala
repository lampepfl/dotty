//> using options -Xfatal-warnings -Wdollar-check

val goodVal = 1
def func() = 1
def func1(a: Int) = a
def func2(
  a: Int,
  b: Int
) = a + b

type GoodType = Int

val typedVal: GoodType = 2

enum GoodEnum:
  case GoodCase

case class CaseClass(someField: Int) // error

class GoodClass

object GoodClass:
  val companionObjectValue = 5
end GoodClass

class Class(
   var goodMember: Int,
):
  def goodMethod(x: Int) = goodMember
end Class

def testUsage =
  val instatiation = new Class(goodMember = 1)
  instatiation.goodMethod(1)


trait GoodTrait

class TestTraitUsage extends GoodTrait

package GoodPackage:
  val goodVal = 1


lazy val lazyVal = 23

val listWithAnonymousFunc = List(1, 2, 3).map(_ * 2).map((num) => num + 1)

object TestAnonymousClass:
  trait SomeTrait:
    def somefunc(): String

  val anObject = new SomeTrait:
    override def somefunc(): String = "hello"
end TestAnonymousClass

val partialFunc: PartialFunction[Int, Int] =
  case x if x <= 0 => Math.abs(x)

val anotherPartialFunc: PartialFunction[String, String] = { case x @ "abc" => x }

object TestInline:
  import scala.quoted.* // imports Quotes, Expr

  inline val pi = 3.141592653589793

  inline def power(inline x: Double, inline n: Int) =
    ${ powerCode('x, 'n) }


  def pow(x: Double, n: Int): Double =
    if n == 0 then 1 else x * pow(x, n - 1)

  def powerCode(
                 x: Expr[Double],
                 n: Expr[Int]
               )(using Quotes): Expr[Double] =
    val value: Double = pow(x.valueOrAbort, n.valueOrAbort)
    Expr(value)

end TestInline

object TestGivens:
  trait Showable[A]:
    extension (a: A) def show: String
  case class Person(firstName: String, lastName: String)
  // anonymous given
  given Showable[Person] with
    extension (p: Person) def show: String =
      s"${p.firstName} ${p.lastName}"
  // named given
  given caseClassShowable: Showable[CaseClass] with
    extension (c: CaseClass) def show: String = s"number ${c.someField}"
  def showMe[T: Showable](t: T) = t.show
  showMe(Person("John", "Doe"))
  showMe(CaseClass(42))

  given Int = 42
  def showUsing(using someNumber: Int) = CaseClass(someNumber)
end TestGivens