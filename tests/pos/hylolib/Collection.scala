//> using options -language:experimental.modularity -source future
package hylo

/** A collection of elements accessible by their position. */
trait Collection:
  type Self

  /** The type of the elements in the collection. */
  type Element: Value

  /** The type of a position in the collection. */
  type Position: Value

  extension (self: Self)

    /** Returns `true` iff `self` is empty. */
    def isEmpty: Boolean =
      startPosition `eq` endPosition

    /** Returns the number of elements in `self`.
      *
      * @complexity
      *   O(n) where n is the number of elements in `self`.
      */
    def count: Int =
      val e = endPosition
      def loop(p: Position, n: Int): Int =
        if p `eq` e then n else loop(self.positionAfter(p), n + 1)
      loop(startPosition, 0)

    /** Returns the position of `self`'s first element', or `endPosition` if `self` is empty.
      *
      * @complexity
      *   O(1)
      */
    def startPosition: Position

    /** Returns the "past the end" position in `self`, that is, the position immediately after the
      * last element in `self`.
      *
      * @complexity
      *   O(1).
      */
    def endPosition: Position

    /** Returns the position immediately after `p`.
      *
      * @requires
      *   `p` is a valid position in `self` different from `endPosition`.
      * @complexity
      *   O(1).
      */
    def positionAfter(p: Position): Position

    /** Accesses the element at `p`.
      *
      * @requires
      *   `p` is a valid position in `self` different from `endPosition`.
      * @complexity
      *   O(1).
      */
    def at(p: Position): Element

    /** Returns `true` iff `i` precedes `j`.
      *
      * @requires
      *   `i` and j` are valid positions in `self`.
      * @complexity
      *   O(n) where n is the number of elements in `self`.
      */
    def isBefore(i: Position, j: Position): Boolean =
      val e = self.endPosition
      if i `eq` e then false
      else if j `eq` e then true
      else
        def recur(n: Position): Boolean =
          if n `eq` j then true
          else if n `eq` e then false
          else recur(self.positionAfter(n))
        recur(self.positionAfter(i))

  class Slice2(val base: Self, val bounds: Range[Position]):

    def isEmpty: Boolean =
      bounds.lowerBound.eq(bounds.upperBound)

    def startPosition: Position =
      bounds.lowerBound

    def endPosition: Position =
      bounds.upperBound

    def at(p: Position): Element =
      base.at(p)
  end Slice2

end Collection

extension [Self: Collection](self: Self)

  /** Returns the first element of `self` along with a slice containing the suffix after this
    * element, or `None` if `self` is empty.
    *
    * @complexity
    *   O(1)
    */
  def headAndTail: Option[(Self.Element, Slice[Self])] =
    if self.isEmpty then
      None
    else
      val p = self.startPosition
      val q = self.positionAfter(p)
      val t = Slice(self, Range(q, self.endPosition, (a, b) => (a `eq` b) || self.isBefore(a, b)))
      Some((self.at(p), t))

  def headAndTail2: Option[(Self.Element, Self.Slice2)] =
    if self.isEmpty then
      None
    else
      val p = self.startPosition
      val q = self.positionAfter(p)
      val t = Self.Slice2(self, Range(q, self.endPosition, (a, b) => (a `eq` b) || self.isBefore(a, b)))
      Some((self.at(p), t))

  /** Applies `combine` on `partialResult` and each element of `self`, in order.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def reduce[T](partialResult: T)(combine: (T, Self.Element) => T): T =
    val e = self.endPosition
    def loop(p: Self.Position, r: T): T =
      if p `eq` e then r
      else loop(self.positionAfter(p), combine(r, self.at(p)))
    loop(self.startPosition, partialResult)

  /** Applies `action` on each element of `self`, in order, until `action` returns `false`, and
    * returns `false` iff `action` did.
    *
    * You can return `false` from `action` to emulate a `continue` statement as found in traditional
    * imperative languages (e.g., C).
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def forEach(action: Self.Element => Boolean): Boolean =
    val e = self.endPosition
    def loop(p: Self.Position): Boolean =
      if p `eq` e then true
      else if !action(self.at(p)) then false
      else loop(self.positionAfter(p))
    loop(self.startPosition)

  /** Returns a collection with the elements of `self` transformed by `transform`, in order.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def map[T: Value](transform: Self.Element => T): HyArray[T] =
    self.reduce(HyArray[T]()): (r, e) =>
      r.append(transform(e), assumeUniqueness = true)

  /** Returns a collection with the elements of `self` satisfying `isInclude`, in order.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def filter(isIncluded: Self.Element => Boolean): HyArray[Self.Element] =
    self.reduce(HyArray[Self.Element]()): (r, e) =>
      if isIncluded(e) then r.append(e, assumeUniqueness = true) else r

  /** Returns `true` if `self` contains an element satisfying `predicate`.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def containsWhere(predicate: Self.Element => Boolean): Boolean =
    self.firstPositionWhere(predicate) != None

  /** Returns `true` if all elements in `self` satisfy `predicate`.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def allSatisfy(predicate: Self.Element => Boolean): Boolean =
    self.firstPositionWhere(predicate) == None

  /** Returns the position of the first element of `self` satisfying `predicate`, or `None` if no
    * such element exists.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def firstPositionWhere(predicate: Self.Element => Boolean): Option[Self.Position] =
    val e = self.endPosition
    def loop(p: Self.Position): Option[Self.Position] =
      if p `eq` e then None
      else if predicate(self.at(p)) then Some(p)
      else loop(self.positionAfter(p))
    loop(self.startPosition)

  /** Returns the minimum element in `self`, using `isLessThan` to compare elements.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def minElement(isLessThan: (Self.Element, Self.Element) => Boolean): Option[Self.Element] =
    self.leastElement(isLessThan)

  // NOTE: I can't find a reasonable way to call this method.
  /** Returns the minimum element in `self`.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def minElement()(using Self.Element is Comparable): Option[Self.Element] =
    self.minElement(isLessThan = _ `lt` _)

  /** Returns the maximum element in `self`, using `isGreaterThan` to compare elements.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def maxElement(isGreaterThan: (Self.Element, Self.Element) => Boolean): Option[Self.Element] =
    self.leastElement(isGreaterThan)

  /** Returns the maximum element in `self`.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def maxElement()(using Self.Element is Comparable): Option[Self.Element] =
    self.maxElement(isGreaterThan = _ `gt` _)

  /** Returns the maximum element in `self`, using `isOrderedBefore` to compare elements.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def leastElement(isOrderedBefore: (Self.Element, Self.Element) => Boolean): Option[Self.Element] =
    if self.isEmpty then
      None
    else
      val e = self.endPosition
      def loop(p: Self.Position, least: Self.Element): Self.Element =
        if p `eq` e then
          least
        else
          val x = self.at(p)
          val y = if isOrderedBefore(x, least) then x else least
          loop(self.positionAfter(p), y)
      val b = self.startPosition
      Some(loop(self.positionAfter(b), self.at(b)))

  /** Returns `true` if `self` contains the same elements as `other`, in the same order. */
  def elementsEqual[T: Collection { type Element = Self.Element } ](other: T): Boolean =
    def loop(i: Self.Position, j: T.Position): Boolean =
      if i `eq` self.endPosition then
        j `eq` other.endPosition
      else if j `eq` other.endPosition then
        false
      else if self.at(i) `neq` other.at(j)then
        false
      else
        loop(self.positionAfter(i), other.positionAfter(j))
    loop(self.startPosition, other.startPosition)
end extension
