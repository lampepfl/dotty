
object Test:

  trait T:
    type S
    type F = T.F[S]

    def foo: F
    def bar: T.F[S]

  object T:
    type F[X] = X match
      case String => Option[Int]

    type G[X] = X match
      case Option[x] => Int

  val t: T {type S = String} = ???

  val b = t.bar
  val m1: T.G[b.type] = ???
  val _: Int = m1 // Ok

  val f = t.foo
  val m: T.G[f.type] = ???
  val _: Int = m // Error before changes
