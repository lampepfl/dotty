package hylo

/** A view into a collection. */
final class Slice[Base: Collection](
  val base: Base,
  val bounds: Range[Base.Position]
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

given [C: Collection] => Slice[C] is Collection:

  type Element = C.Element
  type Position = C.Position

  extension (self: Slice[C])

    def startPosition = self.bounds.lowerBound.asInstanceOf[Position]
      // This is actually unsafe. We have:
      //   self.bounds: Range(Slice[C].Base.Position)
      // But the _value_ of Slice[C].Base is not necssarily this given, even
      // though it is true that `type Slice[C].Base = C`. There might be multiple
      // implementations of `Slice[C] is Collection` that define different `Position`
      // types. So we cannot conclude that `Slice[C].Base.Position = this.Position`.
      // To make this safe, we'd need some form of coherence, where we ensure that
      // there is only one way to implement `Slice is Collection`.
      //
      // As an alternativem we can make Slice dependent on the original Collection
      // _instance_ instead of the original Collection _type_. This design is
      // realized by the Slice2 definitions. It works without casts.

    def endPosition = self.bounds.upperBound.asInstanceOf[Position]

    def positionAfter(p: Position) = self.base.positionAfter(p)

    def at(p: Position) = self.base.at(p)

given [C: Collection] => C.Slice2 is Collection:
  type Element = C.Element
  type Position = C.Position

  extension (self: C.Slice2)

    def startPosition = self.bounds.lowerBound
    def endPosition = self.bounds.upperBound
    def positionAfter(p: Position) = self.base.positionAfter(p)
    def at(p: Position) = self.base.at(p)
