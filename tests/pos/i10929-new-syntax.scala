//> using options -language:experimental.modularity -source future
trait TupleOf[+A]:
  type Self
  type Mapped[+A] <: Tuple
  def map[B](x: Self)(f: A => B): Mapped[B]

object TupleOf:

  given EmptyTuple is TupleOf[Nothing]:
    type Mapped[+A] = EmptyTuple
    def map[B](x: EmptyTuple)(f: Nothing => B): Mapped[B] = x

  given [A, Rest <: Tuple : TupleOf[A]] => A *: Rest is TupleOf[A]:
    type Mapped[+A] = A *: Rest.Mapped[A]
    def map[B](x: A *: Rest)(f: A => B): Mapped[B] =
      (f(x.head) *: Rest.map(x.tail)(f))

def foo[T: TupleOf[Int]](xs: T): T.Mapped[Int] = T.map(xs)(_ + 1)

@main def test =
  foo(EmptyTuple): EmptyTuple // ok
  foo(1 *: EmptyTuple): Int *: EmptyTuple // now also ok
