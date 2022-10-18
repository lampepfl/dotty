import language.experimental.namedTypeArguments

class Map[Key, Value](elems: (Key, Value)*)

val m1: Map[Int, _] = ???   // error
val m2: Map[_, String] = ??? // error
val m4: Map[Int, _, String] = ??? // error

type E1 = Either[Int, _] // error
type E2 = Either[_, String] // error
type E3 = Either[_, List] // error
