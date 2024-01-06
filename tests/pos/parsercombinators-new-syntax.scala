//> using options -language:experimental.modularity -source future
import collection.mutable

/// A parser combinator.
trait Combinator:
  type Self
  type Input
  type Result

  extension (self: Self)
    /// Parses and returns an element from input `in`.
    def parse(in: Input): Option[Result]
end Combinator

case class Apply[I, R](action: I => Option[R])
case class Combine[A, B](first: A, second: B)

given [I, R] => Apply[I, R] is Combinator:
  type Input = I
  type Result = R
  extension (self: Apply[I, R])
    def parse(in: I): Option[R] = self.action(in)

given [A: Combinator, B: Combinator { type Input = A.Input }]
    => Combine[A, B] is Combinator:
  type Input = A.Input
  type Result = (A.Result, B.Result)
  extension (self: Combine[A, B])
    def parse(in: Input): Option[Result] =
      for x <- self.first.parse(in); y <- self.second.parse(in) yield (x, y)

extension [A] (buf: mutable.ListBuffer[A]) def popFirst() =
  if buf.isEmpty then None
  else try Some(buf.head) finally buf.remove(0)

@main def hello: Unit =
  val source = (0 to 10).toList
  val stream = source.to(mutable.ListBuffer)

  val n = Apply[mutable.ListBuffer[Int], Int](s => s.popFirst())
  val m = Combine(n, n)

  val r = m.parse(stream) // was error: type mismatch, found `mutable.ListBuffer[Int]`, required `?1.Input`
  val rc: Option[(Int, Int)] = r

