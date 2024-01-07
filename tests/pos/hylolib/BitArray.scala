package hylo

import scala.collection.mutable

/** An array of bit values represented as Booleans, where `true` indicates that the bit is on. */
final class BitArray private (
    private var _bits: HyArray[Int],
    private var _count: Int
) {

  /** Returns `true` iff `this` is empty. */
  def isEmpty: Boolean =
    _count == 0

  /** Returns the number of elements in `this`. */
  def count: Int =
    _count

  /** The number of bits that the array can contain before allocating new storage. */
  def capacity: Int =
    _bits.capacity << 5

  /** Reserves enough storage to store `n` elements in `this`. */
  def reserveCapacity(n: Int, assumeUniqueness: Boolean = false): BitArray =
    if (n == 0) {
      this
    } else {
      val k = 1 + ((n - 1) >> 5)
      if (assumeUniqueness) {
        _bits = _bits.reserveCapacity(k, assumeUniqueness)
        this
      } else {
        new BitArray(_bits.reserveCapacity(k), _count)
      }
    }

  /** Adds a new element at the end of the array. */
  def append(bit: Boolean, assumeUniqueness: Boolean = false): BitArray =
    val result = if assumeUniqueness && (count < capacity) then this else copy(count + 1)
    val p = BitArray.Position(count)
    if (p.bucket >= _bits.count) {
      result._bits = _bits.append(if bit then 1 else 0)
    } else {
      result.setValue(bit, p)
    }
    result._count += 1
    result

  /** Removes and returns the last element, or returns `None` if the array is empty. */
  def popLast(assumeUniqueness: Boolean = false): (BitArray, Option[Boolean]) =
    if (isEmpty) {
      (this, None)
    } else {
      val result = if assumeUniqueness then this else copy()
      val bit = result.at(BitArray.Position(count))
      result._count -= 1
      (result, Some(bit))
    }

  /** Removes all elements in the array, keeping allocated storage iff `keepStorage` is true. */
  def removeAll(
      keepStorage: Boolean = false,
      assumeUniqueness: Boolean = false
  ): BitArray =
    if (isEmpty) {
      this
    } else if (keepStorage) {
      val result = if assumeUniqueness then this else copy()
      result._bits.removeAll(keepStorage, assumeUniqueness = true)
      result._count = 0
      result
    } else {
      BitArray()
    }

  /** Returns `true` iff all elements in `this` are `false`. */
  def allFalse: Boolean =
    if (isEmpty) {
      true
    } else {
      val k = (count - 1) >> 5
      def loop(i: Int): Boolean =
        if (i == k) {
          val m = (1 << (count & 31)) - 1
          (_bits.at(k) & m) == 0
        } else if (_bits.at(i) != 0) {
          false
        } else {
          loop(i + 1)
        }
      loop(0)
    }

  /** Returns `true` iff all elements in `this` are `true`. */
  def allTrue: Boolean =
    if (isEmpty) {
      true
    } else {
      val k = (count - 1) >> 5
      def loop(i: Int): Boolean =
        if (i == k) {
          val m = (1 << (count & 31)) - 1
          (_bits.at(k) & m) == m
        } else if (_bits.at(i) != ~0) {
          false
        } else {
          loop(i + 1)
        }
      loop(0)
    }

  /** Returns the bitwise OR of `this` and `other`. */
  def | (other: BitArray): BitArray =
    val result = copy()
    result.applyBitwise(other, _ | _, assumeUniqueness = true)

  /** Returns the bitwise AND of `this` and `other`. */
  def & (other: BitArray): BitArray =
    val result = copy()
    result.applyBitwise(other, _ & _, assumeUniqueness = true)

  /** Returns the bitwise XOR of `this` and `other`. */
  def ^ (other: BitArray): BitArray =
    val result = copy()
    result.applyBitwise(other, _ ^ _, assumeUniqueness = true)

  /** Assigns each bits in `this` to the result of `operation` applied on those bits and their
    * corresponding bits in `other`.
    *
    * @requires
    *   `self.count == other.count`.
    */
  private def applyBitwise(
      other: BitArray,
      operation: (Int, Int) => Int,
      assumeUniqueness: Boolean = false
  ): BitArray =
    require(this.count == other.count)
    if (isEmpty) {
      this
    } else {
      val result = if assumeUniqueness then this else copy()
      var u = assumeUniqueness
      val k = (count - 1) >> 5

      for (i <- 0 until k) {
        result._bits = result._bits.modifyAt(
          i, (n) => operation(n, other._bits.at(n)),
          assumeUniqueness = u
        )
        u = true
      }
      val m = (1 << (count & 31)) - 1
      result._bits = result._bits.modifyAt(
        k, (n) => operation(n & m, other._bits.at(k) & m),
        assumeUniqueness = u
      )

      result
    }

  /** Returns the position of `this`'s first element', or `endPosition` if `this` is empty.
    *
    * @complexity
    *   O(1).
    */
  def startPosition: BitArray.Position =
    BitArray.Position(0)

  /** Returns the "past the end" position in `this`, that is, the position immediately after the
    * last element in `this`.
    *
    * @complexity
    *   O(1).
    */
  def endPosition: BitArray.Position =
    BitArray.Position(count)

  /** Returns the position immediately after `p`.
    *
    * @requires
    *   `p` is a valid position in `self` different from `endPosition`.
    * @complexity
    *   O(1).
    */
  def positionAfter(p: BitArray.Position): BitArray.Position =
    if (p.offsetInBucket == 63) {
      BitArray.Position(p.bucket + 1, 0)
    } else {
      BitArray.Position(p.bucket, p.offsetInBucket + 1)
    }

  /** Accesses the element at `p`.
    *
    * @requires
    *   `p` is a valid position in `self` different from `endPosition`.
    * @complexity
    *   O(1).
    */
  def at(p: BitArray.Position): Boolean =
    val m = 1 << p.offsetInBucket
    val b: Int = _bits.at(p.bucket)
    (b & m) == m

  /** Accesses the `i`-th element of `this`.
    *
    * @requires
    *   `i` is greater than or equal to 0, and less than `count`.
    * @complexity
    *   O(1).
    */
  def atIndex(i: Int): Boolean =
    at(BitArray.Position(i))

  /** Calls `transform` on the element at `p` to update its value.
    *
    * @requires
    *   `p` is a valid position in `self` different from `endPosition`.
    * @complexity
    *   O(1).
    */
  def modifyAt(
      p: BitArray.Position,
      transform: (Boolean) => Boolean,
      assumeUniqueness: Boolean = false
  ): BitArray =
    val result = if assumeUniqueness then this else copy()
    result.setValue(transform(result.at(p)), p)
    result

  /** Calls `transform` on `i`-th element of `this` to update its value.
    *
    * @requires
    *   `i` is greater than or equal to 0, and less than `count`.
    * @complexity
    *   O(1).
    */
  def modifyAtIndex(
      i: Int,
      transform: (Boolean) => Boolean,
      assumeUniqueness: Boolean = false
  ): BitArray =
    modifyAt(BitArray.Position(i), transform, assumeUniqueness)

  /** Returns an independent copy of `this`. */
  def copy(minimumCapacity: Int = 0): BitArray =
    if (minimumCapacity > capacity) {
      // If the requested capacity on the copy is greater than what we have, `reserveCapacity` will
      // create an independent value.
      reserveCapacity(minimumCapacity)
    } else {
      val k = 1 + ((minimumCapacity - 1) >> 5)
      val newBits = _bits.copy(k)
      new BitArray(newBits, _count)
    }

  /** Returns a textual description of `this`. */
  override def toString: String =
    _bits.toString

  /** Sets the value `b` for the bit at position `p`.
    *
    * @requires
    *   `this` is uniquely referenced and `p` is a valid position in `this`.
    */
  private def setValue(b: Boolean, p: BitArray.Position): Unit =
    val m = 1 << p.offsetInBucket
    _bits = _bits.modifyAt(
      p.bucket,
      (e) => if b then e | m else e & ~m,
      assumeUniqueness = true
    )

}

object BitArray {

  /** A position in a `BitArray`.
    *
    * @param bucket
    *   The bucket containing `this`.
    * @param offsetInBucket
    *   The offset of `this` in its containing bucket.
    */
  final class Position(
      private[BitArray] val bucket: Int,
      private[BitArray] val offsetInBucket: Int
  ) {

    /** Creates a position from an index. */
    private[BitArray] def this(index: Int) =
      this(index >> 5, index & 31)

    /** Returns the index corresponding to this position. */
    private def index: Int =
      (bucket >> 5) + offsetInBucket

    /** Returns a copy of `this`. */
    def copy(): Position =
      new Position(bucket, offsetInBucket)

    /** Returns `true` iff `this` and `other` have an equivalent value. */
    def eq(other: Position): Boolean =
      (this.bucket == other.bucket) && (this.offsetInBucket == other.offsetInBucket)

    /** Hashes the salient parts of `self` into `hasher`. */
    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(bucket)
      hasher.combine(offsetInBucket)

  }

  /** Creates an array with the given `bits`. */
  def apply[T](bits: Boolean*): BitArray =
    var result = new BitArray(HyArray[Int](), 0)
    for (b <- bits) result = result.append(b, assumeUniqueness = true)
    result

}

given bitArrayPositionIsValue: Value[BitArray.Position] with {

  extension (self: BitArray.Position) {

    def copy(): BitArray.Position =
      self.copy()

    def eq(other: BitArray.Position): Boolean =
      self.eq(other)

    def hashInto(hasher: Hasher): Hasher =
      self.hashInto(hasher)

  }

}

given bitArrayIsCollection: Collection[BitArray] with {

  type Element = Boolean
  type Position = BitArray.Position

  extension (self: BitArray) {

    override def count: Int =
      self.count

    def startPosition: BitArray.Position =
      self.startPosition

    def endPosition: BitArray.Position =
      self.endPosition

    def positionAfter(p: BitArray.Position): BitArray.Position =
      self.positionAfter(p)

    def at(p: BitArray.Position): Boolean =
      self.at(p)

  }

}

given bitArrayIsStringConvertible: StringConvertible[BitArray] with {

  extension (self: BitArray)
    override def description: String =
      var contents = mutable.StringBuilder()
      self.forEach((e) => { contents += (if e then '1' else '0'); true })
      contents.mkString

}
