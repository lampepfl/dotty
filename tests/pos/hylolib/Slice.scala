package hylo

/** A view into a collection. */
final class Slice[Base](using
    val b: Collection[Base]
)(
    val base: Base,
    val bounds: Range[b.Position]
) {

  /** Returns `true` iff `this` is empty. */
  def isEmpty: Boolean =
    bounds.lowerBound.eq(bounds.upperBound)

  def startPosition: b.Position =
    bounds.lowerBound

  def endPosition: b.Position =
    bounds.upperBound

  def positionAfter(p: b.Position): b.Position =
    base.positionAfter(p)

  def at(p: b.Position): b.Element =
    base.at(p)

}

given sliceIsCollection[T](using c: Collection[T]): Collection[Slice[T]] with {

  type Element = c.Element
  //given elementIsValue: Value[Element] = c.elementIsValue

  type Position = c.Position
  given positionIsValue: Value[Position] = c.positionIsValue

  extension (self: Slice[T]) {

    def startPosition = self.bounds.lowerBound.asInstanceOf[Position] // NOTE: Ugly hack

    def endPosition = self.bounds.upperBound.asInstanceOf[Position]

    def positionAfter(p: Position) = self.base.positionAfter(p)

    def at(p: Position) = self.base.at(p)

  }

}
