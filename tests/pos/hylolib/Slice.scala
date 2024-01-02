package hylo

/** A view into a collection. */
final class Slice[Base: Collection](
  tracked val base: Base,
  tracked val bounds: Range[Base.Position]
) {

  /** Returns `true` iff `this` is empty. */
  def isEmpty: Boolean =
    bounds.lowerBound.eq(bounds.upperBound)

  def startPosition: Base.Position =
    bounds.lowerBound

  def endPosition: Base.Position =
    bounds.upperBound

  def positionAfter(p: Base.Position): Base.Position =
    base.positionAfter(p)

  def at(p: Base.Position): Base.Element =
    base.at(p)

}

given [T: Collection as c] => Slice[T] is Collection:

  type Element = c.Element
  type Position = c.Position

  extension (self: Slice[T])

    def startPosition = self.bounds.lowerBound.asInstanceOf[Position] // NOTE: Ugly hack

    def endPosition = self.bounds.upperBound.asInstanceOf[Position]

    def positionAfter(p: Position) = self.base.positionAfter(p)

    def at(p: Position) = self.base.at(p)
