//> using options -source future -language:experimental.modularity

import collection.mutable

/// A parser combinator.
trait Combinator[T]:

  /// The context from which elements are being parsed, typically a stream of tokens.
  type Context
  /// The element being parsed.
  type Element

  extension (self: T)
    /// Parses and returns an element from `context`.
    def parse(context: Context): Option[Element]
end Combinator

final case class Apply[C, E](action: C => Option[E])
final case class Combine[A, B](first: A, second: B)

object test:

  class apply[C, E] extends Combinator[Apply[C, E]]:
    type Context = C
    type Element = E
    extension(self: Apply[C, E])
      def parse(context: C): Option[E] = self.action(context)

  def apply[C, E]: apply[C, E] = new apply[C, E]

  class combine[A, B](
      tracked val f: Combinator[A],
      tracked val s: Combinator[B] { type Context = f.Context}
  ) extends Combinator[Combine[A, B]]:
    type Context = f.Context
    type Element = (f.Element, s.Element)
    extension(self: Combine[A, B])
      def parse(context: Context): Option[Element] = ???

  def combine[A, B](
      _f: Combinator[A],
      _s: Combinator[B] { type Context = _f.Context}
    ) = new combine[A, B](_f, _s)
    // cast is needed since the type of new combine[A, B](_f, _s)
    // drops the required refinement.

  extension [A] (buf: mutable.ListBuffer[A]) def popFirst() =
    if buf.isEmpty then None
    else try Some(buf.head) finally buf.remove(0)

  @main def hello: Unit = {
    val source = (0 to 10).toList
    val stream = source.to(mutable.ListBuffer)

    val n = Apply[mutable.ListBuffer[Int], Int](s => s.popFirst())
    val m = Combine(n, n)

    val c = combine(
        apply[mutable.ListBuffer[Int], Int],
        apply[mutable.ListBuffer[Int], Int]
      )
    val r = c.parse(m)(stream) // was type mismatch, now OK
    val rc: Option[(Int, Int)] = r
  }
