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

given apply[C, E]: Combinator[Apply[C, E]] with {
  type Context = C
  type Element = E
  extension(self: Apply[C, E]) {
    def parse(context: C): Option[E] = self.action(context)
  }
}

given combine[A, B](using
    tracked val f: Combinator[A],
    tracked val s: Combinator[B] { type Context = f.Context }
): Combinator[Combine[A, B]] with {
  type Context = f.Context
  type Element = (f.Element, s.Element)
  extension(self: Combine[A, B]) {
    def parse(context: Context): Option[Element] = ???
  }
}

extension [A] (buf: mutable.ListBuffer[A]) def popFirst() =
  if buf.isEmpty then None
  else try Some(buf.head) finally buf.remove(0)

@main def hello: Unit = {
  val source = (0 to 10).toList
  val stream = source.to(mutable.ListBuffer)

  val n = Apply[mutable.ListBuffer[Int], Int](s => s.popFirst())
  val m = Combine(n, n)

  val r = m.parse(stream) // error: type mismatch, found `mutable.ListBuffer[Int]`, required `?1.Context`
  val rc: Option[(Int, Int)] = r
  // it would be great if this worked
}
