//> using options -language:experimental.modularity -source future
infix abstract class TupleOf[T, +A]:
  type Mapped[+A] <: Tuple
  def map[B](x: T)(f: A => B): Mapped[B]

object TupleOf:

  given TupleOf[EmptyTuple, Nothing] with
    type Mapped[+A] = EmptyTuple
    def map[B](x: EmptyTuple)(f: Nothing => B): Mapped[B] = x

  given [A, Rest <: Tuple](using tracked val tup: Rest TupleOf A): TupleOf[A *: Rest, A] with
    type Mapped[+A] = A *: tup.Mapped[A]
    def map[B](x: A *: Rest)(f: A => B): Mapped[B] =
      (f(x.head) *: tup.map(x.tail)(f))

def foo[T](xs: T)(using tup: T TupleOf Int): tup.Mapped[Int] = tup.map(xs)(_ + 1)

@main def test =
  foo(EmptyTuple): EmptyTuple // ok
  foo(1 *: EmptyTuple): Int *: EmptyTuple // now also ok