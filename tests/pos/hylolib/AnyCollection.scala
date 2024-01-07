//> using options -language:experimental.modularity -source future
package hylo

/** A type-erased collection.
  *
  * A `AnyCollection` forwards its operations to a wrapped value, hiding its implementation.
  */
final class AnyCollection[Element] private (
    val _start: () => AnyValue,
    val _end: () => AnyValue,
    val _after: (AnyValue) => AnyValue,
    val _at: (AnyValue) => Element
)

object AnyCollection {

  /** Creates an instance forwarding its operations to `base`. */
  def apply[Base: Collection](base: Base): AnyCollection[Base.Element] =

    def start(): AnyValue =
      AnyValue(base.startPosition)

    def end(): AnyValue =
      AnyValue(base.endPosition)

    def after(p: AnyValue): AnyValue =
      AnyValue(base.positionAfter(p.unsafelyUnwrappedAs[Base.Position]))

    def at(p: AnyValue): Base.Element =
      base.at(p.unsafelyUnwrappedAs[Base.Position])

    new AnyCollection[Base.Element](
      _start = start,
      _end = end,
      _after = after,
      _at = at
    )

}

given [T: Value] => AnyCollection[T] is Collection:

  type Element = T
  type Position = AnyValue

  extension (self: AnyCollection[T])
    def startPosition = self._start()
    def endPosition = self._end()
    def positionAfter(p: Position) = self._after(p)
    def at(p: Position) = self._at(p)

