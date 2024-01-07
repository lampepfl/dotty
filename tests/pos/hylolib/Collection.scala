//> using options -language:experimental.modularity -source future
package hylo

/** A collection of elements accessible by their position. */
trait Collection[Self] {

  /** The type of the elements in the collection. */
  type Element: Value

  /** The type of a position in the collection. */
  type Position: Value as positionIsValue

  extension (self: Self) {

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
      def _count(p: Position, n: Int): Int =
        if p `eq` e then n else _count(self.positionAfter(p), n + 1)
      _count(startPosition, 0)

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
      if (i.eq(e)) {
        false
      } else if (j.eq(e)) {
        true
      } else {
        def _isBefore(n: Position): Boolean =
          if (n.eq(j)) {
            true
          } else if (n.eq(e)) {
            false
          } else {
            _isBefore(self.positionAfter(n))
          }
        _isBefore(self.positionAfter(i))
      }

  }

}

extension [Self: Collection as s](self: Self) {

  /** Returns the first element of `self` along with a slice containing the suffix after this
    * element, or `None` if `self` is empty.
    *
    * @complexity
    *   O(1)
    */
  def headAndTail: Option[(s.Element, Slice[Self])] =
    if (self.isEmpty) {
      None
    } else {
      val p = self.startPosition
      val q = self.positionAfter(p)
      val t = Slice(self, Range(q, self.endPosition, (a, b) => (a `eq` b) || self.isBefore(a, b)))
      Some((self.at(p), t))
    }

  /** Applies `combine` on `partialResult` and each element of `self`, in order.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def reduce[T](partialResult: T, combine: (T, s.Element) => T): T =
    val e = self.endPosition
    def loop(p: s.Position, r: T): T =
      if (p.eq(e)) {
        r
      } else {
        loop(self.positionAfter(p), combine(r, self.at(p)))
      }
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
  def forEach(action: (s.Element) => Boolean): Boolean =
    val e = self.endPosition
    def loop(p: s.Position): Boolean =
      if (p.eq(e)) {
        true
      } else if (!action(self.at(p))) {
        false
      } else {
        loop(self.positionAfter(p))
      }
    loop(self.startPosition)

  /** Returns a collection with the elements of `self` transformed by `transform`, in order.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def map[T: Value](transform: (s.Element) => T): HyArray[T] =
    self.reduce(
      HyArray[T](),
      (r, e) => r.append(transform(e), assumeUniqueness = true)
    )

  /** Returns a collection with the elements of `self` satisfying `isInclude`, in order.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def filter(isIncluded: (s.Element) => Boolean): HyArray[s.Element] =
    self.reduce(
      HyArray[s.Element](),
      (r, e) => if (isIncluded(e)) then r.append(e, assumeUniqueness = true) else r
    )

  /** Returns `true` if `self` contains an element satisfying `predicate`.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def containsWhere(predicate: (s.Element) => Boolean): Boolean =
    self.firstPositionWhere(predicate) != None

  /** Returns `true` if all elements in `self` satisfy `predicate`.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def allSatisfy(predicate: (s.Element) => Boolean): Boolean =
    self.firstPositionWhere(predicate) == None

  /** Returns the position of the first element of `self` satisfying `predicate`, or `None` if no
    * such element exists.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def firstPositionWhere(predicate: (s.Element) => Boolean): Option[s.Position] =
    val e = self.endPosition
    def loop(p: s.Position): Option[s.Position] =
      if (p.eq(e)) {
        None
      } else if (predicate(self.at(p))) {
        Some(p)
      } else {
        loop(self.positionAfter(p))
      }
    loop(self.startPosition)

  /** Returns the minimum element in `self`, using `isLessThan` to compare elements.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def minElement(isLessThan: (s.Element, s.Element) => Boolean): Option[s.Element] =
    self.leastElement(isLessThan)

  // NOTE: I can't find a reasonable way to call this method.
  /** Returns the minimum element in `self`.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def minElement()(using Comparable[s.Element]): Option[s.Element] =
    self.minElement(isLessThan = _ `lt` _)

  /** Returns the maximum element in `self`, using `isGreaterThan` to compare elements.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def maxElement(isGreaterThan: (s.Element, s.Element) => Boolean): Option[s.Element] =
    self.leastElement(isGreaterThan)

  /** Returns the maximum element in `self`.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def maxElement()(using Comparable[s.Element]): Option[s.Element] =
    self.maxElement(isGreaterThan = _ `gt` _)

  /** Returns the maximum element in `self`, using `isOrderedBefore` to compare elements.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def leastElement(isOrderedBefore: (s.Element, s.Element) => Boolean): Option[s.Element] =
    if (self.isEmpty) {
      None
    } else {
      val e = self.endPosition
      def _least(p: s.Position, least: s.Element): s.Element =
        if (p.eq(e)) {
          least
        } else {
          val x = self.at(p)
          val y = if isOrderedBefore(x, least) then x else least
          _least(self.positionAfter(p), y)
        }

      val b = self.startPosition
      Some(_least(self.positionAfter(b), self.at(b)))
    }

}

extension [Self: Collection as s](self: Self)(using
    Value[s.Element]
) {

  /** Returns `true` if `self` contains the same elements as `other`, in the same order. */
  def elementsEqual[T](using o: Collection[T] { type Element = s.Element })(other: T): Boolean =
    def loop(i: s.Position, j: o.Position): Boolean =
      if (i `eq` self.endPosition) {
        j `eq` other.endPosition
      } else if (j `eq` other.endPosition) {
        false
      } else if (self.at(i) `neq` other.at(j)) {
        false
      } else {
        loop(self.positionAfter(i), other.positionAfter(j))
      }
    loop(self.startPosition, other.startPosition)

}
