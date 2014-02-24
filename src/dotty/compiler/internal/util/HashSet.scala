/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.compiler
package internal
package util

object HashSet {
  def apply[T >: Null <: AnyRef](): HashSet[T] = this(16)
  def apply[T >: Null <: AnyRef](label: String): HashSet[T] = this(label, 16)
  def apply[T >: Null <: AnyRef](initialCapacity: Int): HashSet[T] = this("No Label", initialCapacity)
  def apply[T >: Null <: AnyRef](label: String, initialCapacity: Int): HashSet[T] =
    new HashSet[T](label, initialCapacity)
}

class HashSet[T >: Null <: AnyRef](val label: String, initialCapacity: Int) extends Set[T] with scala.collection.generic.Clearable {
  private var used = 0
  private var table = new Array[AnyRef](initialCapacity)
  private def index(x: Int): Int = math.abs(x % table.length)

  def hash(x: T): Int = x.hashCode

  def size: Int = used
  def clear(): Unit = {
    used = 0
    table = new Array[AnyRef](initialCapacity)
  }

  def findEntryOrUpdate(x: T): T = {
    var h = index(hash(x))
    var entry = table(h)
    while (entry ne null) {
      if (x equals entry)
        return entry.asInstanceOf[T]

      h = index(h + 1)
      entry = table(h)
    }
    addEntryAt(h, x)
  }

  private def addEntryAt(h: Int, x: T) = {
    table(h) = x
    used += 1
    if (used > (table.length >> 2)) growTable()
    x
  }

  def findEntry(x: T): T = {
    var h = index(hash(x))
    var entry = table(h)
    while ((entry ne null) && !(x equals entry)) {
      h = index(h + 1)
      entry = table(h)
    }
    entry.asInstanceOf[T]
  }

  private var rover: Int = -1

  protected def findEntryByHash(hashCode: Int): T = {
    rover = index(hashCode)
    nextEntryByHash(hashCode)
  }

  protected def nextEntryByHash(hashCode: Int): T = {
    var entry = table(rover)
    while (entry ne null) {
      rover = index(rover + 1)
      if (hash(entry.asInstanceOf[T]) == hashCode) return entry.asInstanceOf[T]
      entry = table(rover)
    }
    null
  }

  protected def addEntryAfterScan(x: T): T = addEntryAt(rover, x)

  def addEntry(x: T): Unit = {
    var h = index(hash(x))
    var entry = table(h)
    while (entry ne null) {
      if (x equals entry) return
      h = index(h + 1)
      entry = table(h)
    }
    table(h) = x
    used += 1
    if (used > (table.length >> 2)) growTable()
  }
  def addEntries(xs: TraversableOnce[T]): Unit = {
    xs foreach addEntry
  }

  def iterator = new Iterator[T] {
    private var i = 0
    def hasNext: Boolean = {
      while (i < table.length && (table(i) eq null)) i += 1
      i < table.length
    }
    def next(): T =
      if (hasNext) { i += 1; table(i - 1).asInstanceOf[T] }
      else null
  }

  private def addOldEntry(x: T): Unit = {
    var h = index(hash(x))
    var entry = table(h)
    while (entry ne null) {
      h = index(h + 1)
      entry = table(h)
    }
    table(h) = x
  }

  private def growTable(): Unit = {
    val oldtable = table
    val growthFactor =
      if (table.length <= initialCapacity) 8
      else if (table.length <= (initialCapacity * 8)) 4
      else 2

    table = new Array[AnyRef](table.length * growthFactor)
    var i = 0
    while (i < oldtable.length) {
      val entry = oldtable(i)
      if (entry ne null) addOldEntry(entry.asInstanceOf[T])
      i += 1
    }
  }
  override def toString() = "HashSet %s(%d / %d)".format(label, used, table.length)
}
