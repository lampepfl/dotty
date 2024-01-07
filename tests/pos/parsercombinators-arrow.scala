//> using options -language:experimental.modularity -source future
import collection.mutable

/// A parser combinator.
trait Combinator:

  type Self

  /// The context from which elements are being parsed, typically a stream of tokens.
  type Context
  /// The element being parsed.
  type Element

  extension (self: Self)
    /// Parses and returns an element from `context`.
    def parse(context: Context): Option[Element]
end Combinator

final case class Apply[C, E](action: C => Option[E])
final case class Combine[A, B](first: A, second: B)

given [C, E] => Apply[C, E] is Combinator:
  type Context = C
  type Element = E
  extension(self: Apply[C, E])
    def parse(context: C): Option[E] = self.action(context)

given [A: Combinator, B: Combinator { type Context = A.Context }]
    => Combine[A, B] is Combinator:
  type Context = A.Context
  type Element = (A.Element, B.Element)
  extension(self: Combine[A, B])
    def parse(context: Context): Option[Element] = ???

extension [A] (buf: mutable.ListBuffer[A]) def popFirst() =
  if buf.isEmpty then None
  else try Some(buf.head) finally buf.remove(0)

@main def hello: Unit =
  val source = (0 to 10).toList
  val stream = source.to(mutable.ListBuffer)

  val n = Apply[mutable.ListBuffer[Int], Int](s => s.popFirst())
  val m = Combine(n, n)

  val r = m.parse(stream) // error: type mismatch, found `mutable.ListBuffer[Int]`, required `?1.Context`
  val rc: Option[(Int, Int)] = r
  // it would be great if this worked
