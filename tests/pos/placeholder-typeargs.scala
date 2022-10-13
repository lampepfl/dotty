import language.experimental.namedTypeArguments

object t1:

  def construct[Elem, Coll[_]](xs: Elem*): Coll[Elem] = ???

  val xs = construct[_, List](1, 2, 3)

class Map[Key, Value](elems: (Key, Value)*)

def test3 = new Map[String, _]("a" -> 1)
def test4 = new Map[_, Int]("a" -> 1)

class SubMap extends Map[_, String](1 -> "")

object overloaded:
  def construct[Elem, Coll[_]](xs: Elem*): Coll[Elem] = ???
  def construct[Coll[_], E](x: E, z: Boolean): Boolean = ???

  val xs1 = construct[_, List](1, 2, 3)
  val _ : List[Int] = xs1
  val xs4 = construct[List, _](1, true)
  val _ : Boolean = xs4


object constrs:
  class C[A, B](x: A)
  val x: C[Int, Int] =
    new C[_, Int](1)

  class E extends C[_, Int]("A")
