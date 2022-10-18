class C[T]
class D[type T] // error: identifier expected, but `type` found

object Test0:
  def f[X, Y](x: X, y: Y): Int = ???
  f[X = Int, Y = Int](1, 2) // error: experimental // error: experimental

object Test {
  import language.experimental.namedTypeArguments

  def f[X, Y](x: X, y: Y): Int = ???

  f[X = Int, String](1, "") // error
  f[X = Int][X = Int][Y = String](1, "") // error: illegal repeated type application

  f[X = Int][Y = String](1, "") // error: illegal repeated type application
  f[X = Int][String](1, "") // error: illegal repeated type application

  f[Y = String][X = Int](1, "") // error: illegal repeated type application
  f[Y = String][Int](1, "") // error: illegal repeated type application
}
