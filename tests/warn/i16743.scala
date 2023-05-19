
trait G
given G = new G { override def toString = "mygiven" }
given String = "aGivenString"

trait T:
  def t = 42
  def f(x: String): String = x*2
  def g(x: String)(y: String): String = (x+y)*2
  def h(x: Any): String = x.toString*2
  def i(x: Any, y: String): String = (x.toString+y)*2
  def j(x: Any, y: Any): String = (x.toString+y.toString)
  def k(using G): String = summon[G].toString
  def l(using G): String = summon[G].toString
  def m: String = "mystring"
  def n: Result = Result()
  def o: Int = 42
  def u: Int = 42
  def u(n: Int): Int = u + n
  def v(n: Int): Int = u + n
  def v(s: String): String = s + u
  def end: Int = 42
  def at(n: Int) = n
  def w(n: Int): Int = 42 + n
  def x(n: Int): Int = 42 + n
  def y(n: Int): Int = u + n
  def y(s: String): String = s + u

extension (_t: T)
  def t = 27 // warn
  def f(i: Int): String = String.valueOf(i)
  def g(x: String)(i: Int): String = x*i // warn
  def h(x: String): String = x // warn
  def i(x: Any, y: Int): String = (x.toString)*y
  def j(x: Any, y: Int): String = (x.toString)*y // warn
  def k(x: String): String = x // warn
  def l(using String): String = summon[String]
  def m(using String): String = "m" + summon[String] // warn
  def n(using String): String = "n" + summon[String] // warn
  def o: String = "42" // warn
  def u: Int = 27 // warn
  def v(d: Double) = 3.14
  def end(n: Int): Int = 42 + n
  def at: Int = 42 // warn
  def w(using String)(n: String): Int = (summon[String] + n).toInt
  def x(using String)(n: Int): Int = summon[String].toInt + n // warn
  def y(using String)(s: String): String = s + summon[String] // warn

// deferred extension is defined in subclass
trait Foo:
  type X
  extension (x: X) def t: Int

trait Bar extends Foo:
  type X = T
  extension (x: X) def t = x.t // nowarn see Quote below

// extension on opaque type matches member of underlying type
object Dungeon:
  opaque type IArray[+T] = Array[? <: T]
  object IArray:
    extension (arr: IArray[Byte]) def length: Int = arr.asInstanceOf[Array[Byte]].length
trait DungeonDweller:
  extension (arr: Dungeon.IArray[Byte]) def length: Int = 42 // nowarn
  def f[A <: Byte](x: Dungeon.IArray[A]) = x.length
trait SadDungeonDweller:
  def f[A](x: Dungeon.IArray[A]) = 27 // x.length // just to confirm, length is not a member

trait Quote:
  type Tree <: AnyRef
  given TreeMethods: TreeMethods
  trait TreeMethods:
    extension (self: Tree)
      def length(): Int
class QuotesImpl extends Quote:
  type Tree = String
  given TreeMethods: TreeMethods with
    extension (self: Tree)
      def length(): Int = self.length() // nowarn Tree already has a member with the same name.

class Result:
  def apply(using String): String = s"result ${summon[String]}"

class Depends:
  type Thing = String
  def thing: Thing = ""
object Depending:
  extension (using depends: Depends)(x: depends.Thing)
    def y = 42
    def length() = 42 // nowarn see Quote above
  def f(using d: Depends) = d.thing.y
  def g(using d: Depends) = d.thing.length()

@main def test() =
  val x = new T {}
  println(x.f(42)) // OK!
  //println(x.g("x")(42)) // NOT OK!
  println(x.h("hi")) // member!
  println(x.i("hi", 5)) // OK!
  println(x.j("hi", 5)) // member!
  println(x.k)
  //println(x.k("hi")) // no, implicit is either omitted (supplied implicitly) or explicitly (using foo)
  println(x.l) // usual, invokes member
  println("L"+x.l(using "x")) // explicit, member doesn't check, try extension
  println(x.m(using "x")) // same idea as previous, except member takes no implicits or any params
  println(x.m(2)) // member checks by adapting result
  println(x.n) // Result
  println(x.n.apply) // apply Result with given
  println(x.n(using "x"))  // apply Result explicitly, not extension
  println(x.end(2))
  println(x.at(2))
  println {
    val p = x.at
    p(2)
  }
  println {
    given String = "42"
    x.w("27")
  }
