object Test1:
  trait MyTypeclass[F[_]]
  def f[F[_]: MyTypeclass, U](t: F[U]) = ???

  type MyType[T] = String
  given MyTypeclass[MyType] = ???

  val stream: Option[MyType[Int]] = ???
  for
    keyStream <- stream
    x = 17
  yield f(keyStream)


object Test2:
  trait MyTypeclass[F[_]]
  def f[F[_]: MyTypeclass, U](t: F[U]) = ???

  type MyType[T] = Nil.type
  given MyTypeclass[MyType] = ???

  val stream: Option[MyType[Int]] = ???
  for
    keyStream <- stream
    x = 17
  yield f(keyStream)
