//> using options -language:experimental.modularity -source future
package hylo

import java.util.Arrays
import scala.collection.mutable

/** An ordered, random-access collection. */
final class HyArray[Element: Value as elementIsCValue](
    private var _storage: scala.Array[AnyRef | Null] | Null,
    private var _count: Int // NOTE: where do I document private fields
) {

  // NOTE: The fact that we need Array[AnyRef] is diappointing and difficult to discover
  // The compiler error sent me on a wild goose chase with ClassTag.

  /** Returns `true` iff `this` is empty. */
  def isEmpty: Boolean =
    _count == 0

  /** Returns the number of elements in `this`. */
  def count: Int =
    _count

  /** Returns the number of elements that `this` can contain before allocating new storage. */
  def capacity: Int =
    if _storage == null then 0 else _storage.length

  /** Reserves enough storage to store `n` elements in `this`. */
  def reserveCapacity(n: Int, assumeUniqueness: Boolean = false): HyArray[Element] =
    if (n <= capacity) {
      this
    } else {
      var newCapacity = max(1, capacity)
      while (newCapacity < n) { newCapacity = newCapacity << 1 }

      val newStorage = new scala.Array[AnyRef | Null](newCapacity)
      val s = _storage.asInstanceOf[scala.Array[AnyRef | Null]]
      var i = 0
      while (i < count) {
        newStorage(i) = _storage(i).asInstanceOf[Element].copy().asInstanceOf[AnyRef]
        i += 1
      }

      if (assumeUniqueness) {
        _storage = newStorage
        this
      } else {
        new HyArray(newStorage, count)
      }
    }

  /** Adds a new element at the end of the array. */
  def append(source: Element, assumeUniqueness: Boolean = false): HyArray[Element] =
    val result = if assumeUniqueness && (count < capacity) then this else copy(count + 1)
    result._storage(count) = source.asInstanceOf[AnyRef]
    result._count += 1
    result

  /** Adds the contents of `source` at the end of the array. */
  def appendContents[C: Collection { type Element = HyArray.this.Element }](
      source: C, assumeUniqueness: Boolean = false
  ): HyArray[Element] =
    val result = if (assumeUniqueness) { this } else { copy(count + source.count) }
    source.reduce(result): (r, e) =>
      r.append(e, assumeUniqueness = true)

  /** Removes and returns the last element, or returns `None` if the array is empty. */
  def popLast(assumeUniqueness: Boolean = false): (HyArray[Element], Option[Element]) =
    if (isEmpty) {
      (this, None)
    } else {
      val result = if assumeUniqueness then this else copy()
      result._count -= 1
      (result, Some(result._storage(result._count).asInstanceOf[Element]))
    }

  /** Removes all elements in the array, keeping allocated storage iff `keepStorage` is true. */
  def removeAll(
      keepStorage: Boolean = false,
      assumeUniqueness: Boolean = false
  ): HyArray[Element] =
    if (isEmpty) {
      this
    } else if (keepStorage) {
      val result = if assumeUniqueness then this else copy()
      Arrays.fill(result._storage, null)
      result._count = 0
      result
    } else {
      HyArray()
    }

  /** Accesses the element at `p`.
    *
    * @requires
    *   `p` is a valid position in `self` different from `endPosition`.
    * @complexity
    *   O(1).
    */
  def at(p: Int): Element =
    _storage(p).asInstanceOf[Element]

  /** Calls `transform` on the element at `p` to update its value.
    *
    * @requires
    *   `p` is a valid position in `self` different from `endPosition`.
    * @complexity
    *   O(1).
    */
  def modifyAt(
      p: Int,
      transform: (Element) => Element,
      assumeUniqueness: Boolean = false
  ): HyArray[Element] =
    val result = if assumeUniqueness then this else copy()
    result._storage(p) = transform(at(p)).asInstanceOf[AnyRef]
    result

  /** Returns a textual description of `this`. */
  override def toString: String =
    var s = "["
    var i = 0
    while (i < count) {
      if (i > 0) { s += ", " }
      s += s"${at(i)}"
      i += 1
    }
    s + "]"

  /** Returns an independent copy of `this`, capable of storing `minimumCapacity` elements before
    * allocating new storage.
    */
  def copy(minimumCapacity: Int = 0): HyArray[Element] =
    if (minimumCapacity > capacity) {
      // If the requested capacity on the copy is greater than what we have, `reserveCapacity` will
      // create an independent value.
      reserveCapacity(minimumCapacity)
    } else {
      val clone = HyArray[Element]().reserveCapacity(max(minimumCapacity, count))
      var i = 0
      while (i < count) {
        clone._storage(i) = _storage(i).asInstanceOf[Element].copy().asInstanceOf[AnyRef]
        i += 1
      }
      clone._count = count
      clone
    }

}

object HyArray {

  /** Creates an array with the given `elements`. */
  def apply[T: Value](elements: T*): HyArray[T] =
    var a = new HyArray[T](null, 0)
    for (e <- elements) a = a.append(e, assumeUniqueness = true)
    a

}

given [T: Value] => HyArray[T] is Value:

  extension (self: HyArray[T])

    def copy(): HyArray[T] =
      self.copy()

    def eq(other: HyArray[T]): Boolean =
      self.elementsEqual(other)

    def hashInto(hasher: Hasher): Hasher =
      self.reduce(hasher)((h, e) => e.hashInto(h))

given [T: Value] => HyArray[T] is Collection:

  type Element = T
  type Position = Int

  extension (self: HyArray[T])

    // NOTE: Having to explicitly override means that primary declaration can't automatically
    // specialize trait requirements.
    override def isEmpty: Boolean = self.isEmpty

    override def count: Int = self.count

    def startPosition = 0

    def endPosition = self.count

    def positionAfter(p: Int) = p + 1

    def at(p: Int) = self.at(p)

given [T: {Value, StringConvertible}] => HyArray[T] is StringConvertible:
  extension (self: HyArray[T])
    override def description: String =
      val contents = mutable.StringBuilder()
      self.forEach: e =>
        contents ++= e.description
        true
      s"[${contents.mkString(", ")}]"
