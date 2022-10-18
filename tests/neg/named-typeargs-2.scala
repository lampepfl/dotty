import language.experimental.namedTypeArguments

class Map[Key, Value](elems: (Key, Value)*)
trait Foo[F[_]]
trait Baz[F[_], A, B]

class Bar1 extends Foo[Map[Key = Int]] // error
class Bar2 extends Foo[Map[Value = Int]] // error
class Bar5 extends Foo[Tuple3[T1 = Int, T3 = Int]] // error
class Bar8 extends Foo[Baz[F = Int => _, B = Int]] // error
