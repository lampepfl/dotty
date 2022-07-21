package dotty.tools.dotc.util

import dotty.tools.uncheckedNN

object HashSet:

  /** The number of elements up to which dense packing is used.
   *  If the number of elements reaches `DenseLimit` a hash table is used instead
   */
  inline val DenseLimit = 8

  def from[T](xs: IterableOnce[T]): HashSet[T] =
    val set = new HashSet[T]()
    set ++= xs
    set

/** A hash set that allows some privileged protected access to its internals
 *  @param  initialCapacity  Indicates the initial number of slots in the hash table.
 *                           The actual number of slots is always a power of 2, so the
 *                           initial size of the table will be the smallest power of two
 *                           that is equal or greater than the given `initialCapacity`.
 *                           Minimum value is 4.
*  @param  capacityMultiple The minimum multiple of capacity relative to used elements.
 *                           The hash table will be re-sized once the number of elements
 *                           multiplied by capacityMultiple exceeds the current size of the hash table.
 *                           However, a table of size up to DenseLimit will be re-sized only
 *                           once the number of elements reaches the table's size.
 */
class HashSet[T](initialCapacity: Int = 8, capacityMultiple: Int = 2) extends MutableSet[T] {
  import HashSet.DenseLimit

  private var used: Int = _
  private var limit: Int = _
  private var table: Array[AnyRef | Null] = _

  clear()

  private def allocate(capacity: Int) =
    table = new Array[AnyRef | Null](capacity)
    limit = if capacity <= DenseLimit then capacity - 1 else capacity / capacityMultiple

  private def roundToPower(n: Int) =
    if n < 4 then 4
    else if Integer.bitCount(n) == 1 then n
    else 1 << (32 - Integer.numberOfLeadingZeros(n))

  /** Remove all elements from this set and set back to initial configuration */
  def clear(): Unit = {
    used = 0
    allocate(roundToPower(initialCapacity))
  }

  /** The number of elements in the set */
  def size: Int = used

  protected def isDense = limit < DenseLimit

  /** Hashcode, by default a processed `x.hashCode`, can be overridden */
  protected def hash(key: T): Int =
    val h = key.hashCode
    // Part of the MurmurHash3 32 bit finalizer
    val i = (h ^ (h >>> 16)) * 0x85EBCA6B
    val j = (i ^ (i >>> 13)) & 0x7FFFFFFF
    if j==0 then 0x41081989 else j

  /** Hashcode, by default `equals`, can be overridden */
  protected def isEqual(x: T, y: T): Boolean = x.equals(y)

  /** Turn hashcode `x` into a table index */
  protected def index(x: Int): Int = x & (table.length - 1)

  protected def currentTable: Array[AnyRef | Null] = table

  protected def firstIndex(x: T) = if isDense then 0 else index(hash(x))
  protected def nextIndex(idx: Int) =
    Stats.record(statsItem("miss"))
    index(idx + 1)

  protected def entryAt(idx: Int): T | Null = table(idx).asInstanceOf[T | Null]
  protected def setEntry(idx: Int, x: T) = table(idx) = x.asInstanceOf[AnyRef | Null]

  def lookup(x: T): T | Null =
    Stats.record(statsItem("lookup"))
    var idx = firstIndex(x)
    var e: T | Null = entryAt(idx)
    while e != null do
      if isEqual(e, x) then return e
      idx = nextIndex(idx)
      e = entryAt(idx)
    null

  /** Add entry at `x` at index `idx` */
  protected def addEntryAt(idx: Int, x: T): T =
    Stats.record(statsItem("addEntryAt"))
    setEntry(idx, x)
    used += 1
    if used > limit then growTable()
    x

  def put(x: T): T =
    Stats.record(statsItem("put"))
    var idx = firstIndex(x)
    var e: T | Null = entryAt(idx)
    while e != null do
      if isEqual(e, x) then return e
      idx = nextIndex(idx)
      e = entryAt(idx)
    addEntryAt(idx, x)

  def +=(x: T): Unit = put(x)

  def remove(x: T): Boolean =
    Stats.record(statsItem("remove"))
    var idx = firstIndex(x)
    var e: T | Null = entryAt(idx)
    while e != null do
      if isEqual(e, x) then
        var hole = idx
        while
          idx = nextIndex(idx)
          e = entryAt(idx)
          e != null
        do
          val eidx = index(hash(e))
          if isDense
            || index(eidx - (hole + 1)) > index(idx - (hole + 1))
               // entry `e` at `idx` can move unless `index(hash(e))` is in
               // the (ring-)interval [hole + 1 .. idx]
          then
            setEntry(hole, e)
            hole = idx
        table(hole) = null
        used -= 1
        return true
      idx = nextIndex(idx)
      e = entryAt(idx)
    false

  def -=(x: T): Unit =
    remove(x)

  private def addOld(x: T) =
    Stats.record(statsItem("re-enter"))
    var idx = firstIndex(x)
    var e = entryAt(idx)
    while e != null do
      idx = nextIndex(idx)
      e = entryAt(idx)
    setEntry(idx, x)

  def copyFrom(oldTable: Array[AnyRef | Null]): Unit =
    if isDense then
      Array.copy(oldTable, 0, table, 0, oldTable.length)
    else
      var idx = 0
      while idx < oldTable.length do
        val e: T | Null = oldTable(idx).asInstanceOf[T | Null]
        if e != null then addOld(e)
        idx += 1

  protected def growTable(): Unit =
    val oldTable = table
    val newLength =
      if oldTable.length == DenseLimit then DenseLimit * 2 * roundToPower(capacityMultiple)
      else table.length * 2
    allocate(newLength)
    copyFrom(oldTable)

  abstract class EntryIterator extends Iterator[T]:
    def entry(idx: Int): T | Null
    private var idx = 0
    def hasNext =
      while idx < table.length && table(idx) == null do idx += 1
      idx < table.length
    def next() =
      require(hasNext)
      try entry(idx).uncheckedNN finally idx += 1

  def iterator: Iterator[T] = new EntryIterator():
    def entry(idx: Int) = entryAt(idx)

  override def toString: String =
    iterator.mkString("HashSet(", ", ", ")")

  protected def statsItem(op: String) =
    val prefix = if isDense then "HashSet(dense)." else "HashSet."
    val suffix = getClass.getSimpleName
    s"$prefix$op $suffix"
}
