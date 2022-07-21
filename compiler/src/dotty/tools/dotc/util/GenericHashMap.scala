package dotty.tools
package dotc.util

object GenericHashMap:

  /** The number of elements up to which dense packing is used.
   *  If the number of elements reaches `DenseLimit` a hash table is used instead
   */
  inline val DenseLimit = 8

/** A hash table using open hashing with linear scan which is also very space efficient
 *  at small sizes. The implementations of `hash` and `isEqual` are left open. They have
 *  to be provided by subclasses.
 *
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
abstract class GenericHashMap[Key, Value]
    (initialCapacity: Int, capacityMultiple: Int) extends MutableMap[Key, Value]:
  import GenericHashMap.DenseLimit

  protected var used: Int = _
  protected var limit: Int = _
  protected var table: Array[AnyRef | Null] = _
  clear()

  private def allocate(capacity: Int) =
    table = new Array[AnyRef | Null](capacity * 2)
    limit = if capacity <= DenseLimit then capacity - 1 else capacity / capacityMultiple

  private def roundToPower(n: Int) =
    if n < 4 then 4
    else if Integer.bitCount(n) == 1 then n
    else 1 << (32 - Integer.numberOfLeadingZeros(n))

  /** Remove all elements from this table and set back to initial configuration */
  def clear(): Unit =
    used = 0
    allocate(roundToPower(initialCapacity))

  /** The number of elements in the set */
  def size: Int = used

  protected def isDense = limit < DenseLimit

  /** Hashcode, to be implemented in subclass */
  protected def hash(x: Key): Int

  /** Equality, to be implemented in subclass */
  protected def isEqual(x: Key, y: Key): Boolean

  /** Turn successor index or hash code `x` into a table index */
  private def index(x: Int): Int = x & (table.length - 2)

  private def firstIndex(key: Key) = if isDense then 0 else index(hash(key))
  private def nextIndex(idx: Int) =
    Stats.record(statsItem("miss"))
    index(idx + 2)

  private def keyAt(idx: Int): Key = table(idx).asInstanceOf[Key]
  private def valueAt(idx: Int): Value = table(idx + 1).asInstanceOf[Value]

  private def setKey(idx: Int, key: Key) =
    table(idx) = key.asInstanceOf[AnyRef]
  private def setValue(idx: Int, value: Value) =
    table(idx + 1) = value.asInstanceOf[AnyRef]

  def lookup(key: Key): Value | Null =
    Stats.record(statsItem("lookup"))
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      if isEqual(k, key) then return valueAt(idx)
      idx = nextIndex(idx)
      k = keyAt(idx)
    null

  def update(key: Key, value: Value): Unit =
    Stats.record(statsItem("update"))
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      if isEqual(k, key) then
        setValue(idx, value)
        return
      idx = nextIndex(idx)
      k = keyAt(idx)
    setKey(idx, key)
    setValue(idx, value)
    used += 1
    if used > limit then growTable()

  def remove(key: Key): Value | Null =
    Stats.record(statsItem("remove"))
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      if isEqual(k, key) then
        val result = valueAt(idx)
        var hole = idx
        while
          idx = nextIndex(idx)
          k = keyAt(idx)
          k != null
        do
          val eidx = index(hash(k))
          if isDense
            || index(eidx - (hole + 2)) > index(idx - (hole + 2))
               // entry `e` at `idx` can move unless `index(hash(e))` is in
               // the (ring-)interval [hole + 2 .. idx]
          then
            setKey(hole, k)
            setValue(hole, valueAt(idx))
            hole = idx
        table(hole) = null
        used -= 1
        return result
      idx = nextIndex(idx)
      k = keyAt(idx)
    null

  def getOrElseUpdate(key: Key, value: => Value): Value =
    var v: Value | Null = lookup(key)
    if v == null then
      val v1 = value
      v = v1
      update(key, v1)
    v

  private def addOld(key: Key, value: Value): Unit =
    Stats.record(statsItem("re-enter"))
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      idx = nextIndex(idx)
      k = keyAt(idx)
    setKey(idx, key)
    setValue(idx, value)

  def copyFrom(oldTable: Array[AnyRef | Null]): Unit =
    if isDense then
      Array.copy(oldTable, 0, table, 0, oldTable.length)
    else
      var idx = 0
      while idx < oldTable.length do
        val key = oldTable(idx)
        if key != null then addOld(key.asInstanceOf[Key], oldTable(idx + 1).asInstanceOf[Value])
        idx += 2

  protected def growTable(): Unit =
    val oldTable = table
    val newLength =
      if table.length == DenseLimit * 2 then table.length * roundToPower(capacityMultiple)
      else table.length
    allocate(newLength)
    copyFrom(oldTable)

  private abstract class EntryIterator[T] extends Iterator[T]:
    def entry(idx: Int): T
    private var idx = 0
    def hasNext =
      while idx < table.length && table(idx) == null do idx += 2
      idx < table.length
    def next() =
      require(hasNext)
      try entry(idx) finally idx += 2

  def iterator: Iterator[(Key, Value)] = new EntryIterator:
    def entry(idx: Int) = (keyAt(idx), valueAt(idx))

  def keysIterator: Iterator[Key] = new EntryIterator:
    def entry(idx: Int) = keyAt(idx)

  def valuesIterator: Iterator[Value] = new EntryIterator:
    def entry(idx: Int) = valueAt(idx)

  override def toString: String =
    iterator.map((k, v) => s"$k -> $v").mkString("HashMap(", ", ", ")")

  protected def statsItem(op: String) =
    val prefix = if isDense then "HashMap(dense)." else "HashMap."
    val suffix = getClass.getSimpleName
    s"$prefix$op $suffix"
end GenericHashMap
