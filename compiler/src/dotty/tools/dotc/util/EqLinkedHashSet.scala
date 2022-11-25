package dotty.tools.dotc.util

import scala.collection.mutable.ArrayBuffer

class EqLinkedHashSet[T](
  initialCapacity: Int = 8, capacityMultiple: Int = 2
) extends MutableSet[T] {

  private val map: MutableMap[T, Unit] = new EqHashMap(initialCapacity, capacityMultiple)
  private val linkingArray: ArrayBuffer[T] = new ArrayBuffer(initialCapacity)

  override def +=(x: T): Unit =
    map.update(x, ())
    if map.size != linkingArray.size then linkingArray += x

  override def put(x: T): T =
    this += x
    x

  override def -=(x: T): Unit =
    map -= x
    if map.size != linkingArray.size then linkingArray -= x

  override def clear(): Unit =
    map.clear()
    linkingArray.clear()

  override def lookup(x: T): T | Null = if map.contains(x) then x else null

  override def size: Int = map.size

  override def iterator: Iterator[T] = linkingArray.iterator

}

object EqLinkedHashSet {
  def apply[T](x: T): EqLinkedHashSet[T] =
    val set = new EqLinkedHashSet[T]
    set += x
    set

  def apply[T](x: T, y: T): EqLinkedHashSet[T] =
    val set = EqLinkedHashSet(x)
    set += y
    set
}
