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
  def apply[Base](using b: Collection[Base])(base: Base): AnyCollection[b.Element] =
    // NOTE: This evidence is redefined so the compiler won't report ambiguity between `intIsValue`
    // and `anyValueIsValue` when the method is called on a collection of `Int`s. None of these
    // choices is even correct! Note also that the ambiguity is suppressed if the constructor of
    // `AnyValue` is declared with a context bound rather than an implicit parameter.
    given Value[b.Position] = b.positionIsValue

    def start(): AnyValue =
      AnyValue(base.startPosition)

    def end(): AnyValue =
      AnyValue(base.endPosition)

    def after(p: AnyValue): AnyValue =
      AnyValue(base.positionAfter(p.unsafelyUnwrappedAs[b.Position]))

    def at(p: AnyValue): b.Element =
      base.at(p.unsafelyUnwrappedAs[b.Position])

    new AnyCollection[b.Element](
      _start = start,
      _end = end,
      _after = after,
      _at = at
    )

}

given anyCollectionIsCollection[T](using tIsValue: Value[T]): Collection[AnyCollection[T]] with {

  type Element = T
  //given elementIsValue: Value[Element] = tIsValue

  type Position = AnyValue
  given positionIsValue: Value[Position] = anyValueIsValue

  extension (self: AnyCollection[T]) {

    def startPosition =
      self._start()

    def endPosition =
      self._end()

    def positionAfter(p: Position) =
      self._after(p)

    def at(p: Position) =
      self._at(p)

  }

}
