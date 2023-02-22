package dotty.tools
package dotc
package util

import scala.annotation.internal.sharable

import core.Contexts._
import collection.mutable

@sharable object Stats {

  inline val enabled = false
  inline val stackTraceEnabled = false

  var monitored: Boolean = false

  @volatile private var stack: List[String] = Nil

  val hits: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int] {
    override def default(key: String): Int = 0
  }

  inline def record(inline fn: String, inline n: Int = 1): Unit =
    if (enabled) doRecord(fn, n)

  def doRecord(fn: String, n: Int) =
    if (monitored) {
      val name = if (fn.startsWith("member-")) "member" else fn
      hits(name) += n
    }

  def doRecordSize(fn: String, coll: scala.collection.Iterable[_]): coll.type =
    doRecord(fn, coll.size)
    coll

  inline def trackTime[T](fn: String)(inline op: T): T =
    if (enabled) doTrackTime(fn)(op) else op

  def doTrackTime[T](fn: String)(op: => T): T = {
    if (monitored) {
      val start = System.nanoTime
      try op finally record(fn, ((System.nanoTime - start) / 1000).toInt)
    }
    else op
  }

  inline val GroupChar = '/'

  /** Aggregate all counts of all keys with a common prefix, followed by `:` */
  private def aggregate(): Unit = {
    val groups = hits.keys
      .filter(_.contains(GroupChar))
      .groupBy(_.takeWhile(_ != GroupChar))
    for ((prefix, names) <- groups; name <- names)
      hits(s"Total $prefix") += hits(name)
  }

  def maybeMonitored[T](op: => T)(using Context): T =
    if ctx.settings.YdetailedStats.value then
      monitored = true
      try op
      finally
        if hits.nonEmpty then
          aggregate()
          println()
          println(hits.toList.sortBy(_._2).map{ case (x, y) => s"$x -> $y" } mkString "\n")
          hits.clear()
    else op

  def recordElem(prefix: String, i: Int, st: StackTraceElement) =
    record(s"$prefix from $i, ${st.getClassName()}#${st.getMethodName()}/${st.getLineNumber}")

  inline def recordStackTrace(prefix: String): Unit =
    if enabled && stackTraceEnabled then
      val ex = new Error()
      val st = ex.getStackTrace().nn
      if st.length > 1 then recordElem(prefix, 1, st(1).nn)
      if st.length > 2 then recordElem(prefix, 2, st(2).nn)
      if st.length > 3 then recordElem(prefix, 3, st(3).nn)
}
