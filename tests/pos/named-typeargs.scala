import language.experimental.namedTypeArguments

object t1:

  def construct[Elem, Coll[_]](xs: Elem*): Coll[Elem] = ???

  val xs3 = construct[Coll = List](1, 2, 3)

  val xs2 = construct[Coll = List, Elem = Int](1, 2, 3)

class Map[Key, Value](elems: (Key, Value)*)

val m: Map[Value = String, Key = Int] = new Map(1 -> "")

def test = new Map("a" -> 1)
def test2 = new Map[String, Int]("a" -> 1)
def test3 = new Map[Key = String]("a" -> 1)
def test4 = new Map[Value = Int]("a" -> 1)

class SubMap extends Map[Value = String](1 -> "")

object overloaded:
  def construct[Elem, Coll[_]](xs: Elem*): Coll[Elem] = ???
  def construct[C[_], E](xs: E*): Int = ???
  def construct[Coll[_], E](x: E, z: Boolean): Boolean = ???

  val xs1 = construct[Coll = List](1, 2, 3)
  val _ : List[Int] = xs1
  val xs2 = construct[C = List](1, 2, 3)
  val _ : Int = xs2
  val xs3 = construct[Coll = List, Elem = Int](1, 2, 3)
  val _ : List[Int] = xs3
  val xs4 = construct[Coll = List](1, true)
  val _ : Boolean = xs4


object constrs:
  class C[A, B](x: A)
  val x: C[B = Int, A = Int] =
    new C[B = Int](1)

  class E extends C[B = Int]("A")
  class F extends C[B = Int, A = String]("A")

trait Foo[F[_]]
trait Baz[F[_], A, B]

class Bar1 extends Foo[F = Map[Int, _]]
class Bar2 extends Foo[F = Map[_, Int]]
class Bar5 extends Foo[F = Tuple3[Int, _, Int]]
class Bar8 extends Foo[[X] =>> Baz[F = Int => _, A = X, B = Int]]
