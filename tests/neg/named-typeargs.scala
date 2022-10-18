import language.experimental.namedTypeArguments

object t1:

  def construct[Elem, Coll[_]](xs: Elem*): Coll[Elem] = ???

  val xs1 = construct[Coll = List, Coll = Seq](1, 2, 3) // error

  val xs2 = construct[C = List](1, 2, 3) // error

  val xs3 = construct[Coll = List, Int](1, 2, 3) // error

  val xs4 = construct[List, Elem = Int]() // error // error

class Map[Key, Value](elems: (Key, Value)*)

val m1: Map[Key = Int] = ???   // error
val m2: Map[Value = String] = ??? // error
val m3: Map[Key = Int, Key = String] = ??? // error
val m4: Map[K = Int, Key = String, Value = Int] = ??? // error

def test = new Map("a" -> 1)
def test2 = new Map[C = String, Value = Int]("a" -> 1) // error
def test3 = new Map[Key = ?, Value = Int]("a" -> 1) // error // error
def test4 = new Map[Value = Int, Key = Int]("a" -> 1) // error

trait Foo[F[_]]

type T = Foo[(T1 = Int)] // error // error
