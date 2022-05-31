trait Bla[T]
object Bla {
  implicit def blaInt: Bla[Int] = new Bla[Int] {}
  implicit def blaString: Bla[String] = new Bla[String] {
    assert(false, "I should not be summoned!")
  }
}

trait ErasedFoo[FT]
object Test {
  type Foo[F[_], T] = ErasedFoo[F[T]]
  type Foo2[F[_], T] = Foo[F, T]
  type Foo3[T, F[_]] = Foo[F, T]

  def mkFoo[F[_], T](implicit gen: Bla[T]): Foo[F, T] = new Foo[F, T] {}
  def mkFoo2[F[_], T](implicit gen: Bla[T]): Foo2[F, T] = new Foo2[F, T] {}
  def mkFoo3[F[_], T](implicit gen: Bla[T]): Foo3[T, F] = new Foo3[T, F] {}

  def main(args: Array[String]): Unit = {
    val a1: Foo[[X] =>> (X, String), Int] = mkFoo
    val b1: Foo2[[X] =>> (X, String), Int] = mkFoo
    val c1: Foo3[Int, [X] =>> (X, String)] = mkFoo

    val a2: Foo[[X] =>> (X, String), Int] = mkFoo2
    val b2: Foo2[[X] =>> (X, String), Int] = mkFoo2
    val c2: Foo3[Int, [X] =>> (X, String)] = mkFoo2

    val a3: Foo[[X] =>> (X, String), Int] = mkFoo3
    val b3: Foo2[[X] =>> (X, String), Int] = mkFoo3
    val c3: Foo3[Int, [X] =>> (X, String)] = mkFoo3
  }
}

