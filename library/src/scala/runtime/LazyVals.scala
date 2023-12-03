package scala.runtime

import java.util.concurrent.CountDownLatch

import scala.annotation.*

/**
 * Helper methods used in thread-safe lazy vals.
 */
object LazyVals {
  @nowarn
  private val unsafe: sun.misc.Unsafe = {
    def throwInitializationException() =
      throw new ExceptionInInitializerError(
        new IllegalStateException("Can't find instance of sun.misc.Unsafe")
      )
    try
      val unsafeField = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe").nn
      if unsafeField.getType == classOf[sun.misc.Unsafe] then
        unsafeField.setAccessible(true)
        unsafeField.get(null).asInstanceOf[sun.misc.Unsafe]
      else
        throwInitializationException()
    catch case _: NoSuchFieldException =>
      throwInitializationException()
  }

  private val base: Int = {
    val processors = java.lang.Runtime.getRuntime.nn.availableProcessors()
    8 * processors * processors
  }

  private val monitors: Array[Object] =
    Array.tabulate(base)(_ => new Object)

  private def getMonitor(obj: Object, fieldId: Int = 0) = {
    var id = (java.lang.System.identityHashCode(obj) + fieldId) % base

    if (id < 0) id += base
    monitors(id)
  }

  private final val LAZY_VAL_MASK = 3L
  private final val debug = false

  /* ------------- Start of public API ------------- */

  // This trait extends Serializable to fix #16806 that caused a race condition
  sealed trait LazyValControlState extends Serializable

  /**
   * Used to indicate the state of a lazy val that is being
   * evaluated and of which other threads await the result.
   */
  final class Waiting extends CountDownLatch(1) with LazyValControlState

  /**
   * Used to indicate the state of a lazy val that is currently being
   * evaluated with no other thread awaiting its result.
   */
  object Evaluating extends LazyValControlState

  /**
   * Used to indicate the state of a lazy val that has been evaluated to
   * `null`.
   */
  object NullValue extends LazyValControlState

  final val BITS_PER_LAZY_VAL = 2L

  def STATE(cur: Long, ord: Int): Long = {
    val r = (cur >> (ord * BITS_PER_LAZY_VAL)) & LAZY_VAL_MASK
    if (debug)
      println(s"STATE($cur, $ord) = $r")
    r
  }

  def CAS(t: Object, offset: Long, e: Long, v: Int, ord: Int): Boolean = {
    if (debug)
      println(s"CAS($t, $offset, $e, $v, $ord)")
    val mask = ~(LAZY_VAL_MASK << ord * BITS_PER_LAZY_VAL)
    val n = (e & mask) | (v.toLong << (ord * BITS_PER_LAZY_VAL))
    unsafe.compareAndSwapLong(t, offset, e, n)
  }

  def objCAS(t: Object, offset: Long, exp: Object, n: Object): Boolean = {
    if (debug)
      println(s"objCAS($t, $exp, $n)")
    unsafe.compareAndSwapObject(t, offset, exp, n)
  }

  def setFlag(t: Object, offset: Long, v: Int, ord: Int): Unit = {
    if (debug)
      println(s"setFlag($t, $offset, $v, $ord)")
    var retry = true
    while (retry) {
      val cur = get(t, offset)
      if (STATE(cur, ord) == 1) retry = !CAS(t, offset, cur, v, ord)
      else {
        // cur == 2, somebody is waiting on monitor
        if (CAS(t, offset, cur, v, ord)) {
          val monitor = getMonitor(t, ord)
          monitor.synchronized {
            monitor.notifyAll()
          }
          retry = false
        }
      }
    }
  }

  def wait4Notification(t: Object, offset: Long, cur: Long, ord: Int): Unit = {
    if (debug)
      println(s"wait4Notification($t, $offset, $cur, $ord)")
    var retry = true
    while (retry) {
      val cur = get(t, offset)
      val state = STATE(cur, ord)
      if (state == 1) CAS(t, offset, cur, 2, ord)
      else if (state == 2) {
        val monitor = getMonitor(t, ord)
        monitor.synchronized {
          if (STATE(get(t, offset), ord) == 2) // make sure notification did not happen yet.
            monitor.wait()
        }
      }
      else retry = false
    }
  }

  def get(t: Object, off: Long): Long = {
    if (debug)
      println(s"get($t, $off)")
    unsafe.getLongVolatile(t, off)
  }

  // kept for backward compatibility
  def getOffset(clz: Class[?], name: String): Long = {
    @nowarn
    val r = unsafe.objectFieldOffset(clz.getDeclaredField(name))
    if (debug)
      println(s"getOffset($clz, $name) = $r")
    r
  }

  def getStaticFieldOffset(field: java.lang.reflect.Field): Long = {
    @nowarn
    val r = unsafe.staticFieldOffset(field)
    if (debug)
      println(s"getStaticFieldOffset(${field.getDeclaringClass}, ${field.getName}) = $r")
    r
  }

  def getOffsetStatic(field: java.lang.reflect.Field) =
    @nowarn
    val r = unsafe.objectFieldOffset(field)
    if (debug)
      println(s"getOffset(${field.getDeclaringClass}, ${field.getName}) = $r")
    r


  object Names {
    final val state = "STATE"
    final val cas = "CAS"
    final val setFlag = "setFlag"
    final val wait4Notification = "wait4Notification"
    final val get = "get"
    final val getOffset = "getOffset"
  }
}
