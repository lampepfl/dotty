package dotty.tools.dotc.profile

import scala.annotation.*
import scala.language.unsafeNulls

import java.io.{FileWriter, PrintWriter}
import java.nio.file.Paths
import java.lang.management.{ManagementFactory, GarbageCollectorMXBean, RuntimeMXBean, MemoryMXBean, ClassLoadingMXBean, CompilationMXBean}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}

import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.io.AbstractFile
import annotation.internal.sharable

object Profiler {
  def apply()(using Context): Profiler =
    if (!ctx.settings.YprofileEnabled.value) NoOpProfiler
    else {
      val reporter = if (ctx.settings.YprofileDestination.value != "")
        new StreamProfileReporter(new PrintWriter(new FileWriter(ctx.settings.YprofileDestination.value, true)))
      else ConsoleProfileReporter
      new RealProfiler(reporter)
    }

  private[profile] val emptySnap: ProfileSnap = ProfileSnap(0, "", 0, 0, 0, 0, 0, 0, 0, 0)
}

case class GcEventData(pool:String, reportTimeNs: Long, gcStartMillis:Long, gcEndMillis:Long, name:String, action:String, cause:String, threads:Long)

case class ProfileSnap(threadId: Long, threadName: String, snapTimeNanos : Long,
                       idleTimeNanos:Long, cpuTimeNanos: Long, userTimeNanos: Long,
                       allocatedBytes:Long, heapBytes:Long,totalClassesLoaded: Long, totalJITCompilationTime: Long) {
  def updateHeap(heapBytes:Long): ProfileSnap =
    copy(heapBytes = heapBytes)
}
case class ProfileRange(start: ProfileSnap, end:ProfileSnap, phase:Phase, purpose:String, taskCount:Int, thread:Thread) {
  def allocatedBytes: Long = end.allocatedBytes - start.allocatedBytes

  def userNs: Long = end.userTimeNanos - start.userTimeNanos

  def cpuNs: Long = end.cpuTimeNanos - start.cpuTimeNanos

  def idleNs: Long = end.idleTimeNanos - start.idleTimeNanos

  def runNs: Long = end.snapTimeNanos - start.snapTimeNanos


  private def toMillis(ns: Long) = ns / 1000000.0D

  private def toMegaBytes(bytes: Long) = bytes / 1000000.0D


  def wallClockTimeMillis: Double = toMillis(end.snapTimeNanos - start.snapTimeNanos)

  def idleTimeMillis: Double = toMillis(end.idleTimeNanos - start.idleTimeNanos)

  def cpuTimeMillis: Double = toMillis(end.cpuTimeNanos - start.cpuTimeNanos)

  def userTimeMillis: Double = toMillis(end.userTimeNanos - start.userTimeNanos)

  def allocatedMB: Double = toMegaBytes(end.allocatedBytes - start.allocatedBytes)

  def retainedHeapMB: Double = toMegaBytes(end.heapBytes - start.heapBytes)
}

sealed trait Profiler {

  def finished(): Unit

  def beforePhase(phase: Phase): ProfileSnap

  def afterPhase(phase: Phase, profileBefore: ProfileSnap): Unit

  def beforeUnit(phase: Phase, unit: CompilationUnit): Unit = ()
  def afterUnit(phase: Phase,  unit: CompilationUnit): Unit = ()

  def beforeTypedImplDef(sym: Symbol): Unit = ()
  def afterTypedImplDef(sym: Symbol): Unit = ()

  def beforeImplicitSearch(pt: Type): Unit  = ()
  def afterImplicitSearch(pt: Type): Unit = ()

  def beforeCompletion(root: Symbol, associatedFile: AbstractFile): Unit = ()
  def afterCompletion(root: Symbol, associatedFile: AbstractFile): Unit = ()
}
private [profile] object NoOpProfiler extends Profiler {

  override def beforePhase(phase: Phase): ProfileSnap = Profiler.emptySnap

  override def afterPhase(phase: Phase, profileBefore: ProfileSnap): Unit = ()

  override def finished(): Unit = ()
}
private [profile] object RealProfiler {
  import scala.jdk.CollectionConverters.*
  val runtimeMx: RuntimeMXBean = ManagementFactory.getRuntimeMXBean
  val memoryMx: MemoryMXBean = ManagementFactory.getMemoryMXBean
  val gcMx: List[GarbageCollectorMXBean] = ManagementFactory.getGarbageCollectorMXBeans.asScala.toList
  val classLoaderMx: ClassLoadingMXBean = ManagementFactory.getClassLoadingMXBean
  val compileMx: CompilationMXBean = ManagementFactory.getCompilationMXBean
  val threadMx: ExtendedThreadMxBean = ExtendedThreadMxBean.proxy
  if (threadMx.isThreadCpuTimeSupported) threadMx.setThreadCpuTimeEnabled(true)
  private val idGen = new AtomicInteger()
}

private [profile] class RealProfiler(reporter : ProfileReporter)(using Context) extends Profiler with NotificationListener {
  def completeBackground(threadRange: ProfileRange): Unit =
    reporter.reportBackground(this, threadRange)

  def outDir: AbstractFile = ctx.settings.outputDir.value

  val id: Int = RealProfiler.idGen.incrementAndGet()

  private val mainThread = Thread.currentThread()

  enum Category:
    def name: String = this.toString().toLowerCase()
    case Run, Phase, File, TypeCheck, Implicit, Macro, Completion
  given Conversion[Category, String] = _.name


  private [profile] val chromeTrace =
    if ctx.settings.YprofileTrace.isDefault
    then null
    else
      // Scala 2 combined traces from different runs by storing them in buffer
      // It would lead to high memory usage when profiling larger codebases
      // Instead create file with suffix for subsequent runs
      val filename = ctx.settings.YprofileTrace.value
      val suffix = if(id > 1) s".$id" else ""
      ChromeTrace(Paths.get(s"$filename$suffix"))
  private var nextAfterUnitSnap:Long = System.nanoTime()

  if chromeTrace != null
  then chromeTrace.traceDurationEventStart(Category.Run, "scalac-" + id)


  @nowarn("cat=deprecation")
  private[profile] def snapThread(idleTimeNanos: Long): ProfileSnap = {
    import RealProfiler.*
    val current = Thread.currentThread()

    ProfileSnap(
      threadId = current.getId,
      threadName = current.getName,
      snapTimeNanos = System.nanoTime(),
      idleTimeNanos = idleTimeNanos,
      cpuTimeNanos = threadMx.getCurrentThreadCpuTime,
      userTimeNanos = threadMx.getCurrentThreadUserTime,
      allocatedBytes = threadMx.getThreadAllocatedBytes(Thread.currentThread().getId),
      heapBytes = readHeapUsage(),
      totalClassesLoaded = classLoaderMx.getTotalLoadedClassCount,
      totalJITCompilationTime = compileMx.getTotalCompilationTime

    )
  }
  private def readHeapUsage() = RealProfiler.memoryMx.getHeapMemoryUsage.getUsed

  @nowarn
  private def doGC: Unit = {
    System.gc()
    System.runFinalization()
  }

  RealProfiler.gcMx foreach {
    case emitter: NotificationEmitter => emitter.addNotificationListener(this, null, null)
    case gc => println(s"Cant connect gcListener to ${gc.getClass}")
  }

  reporter.header(this)

  override def finished(): Unit = {
    //we may miss a GC event if gc is occurring as we call this
    RealProfiler.gcMx foreach {
      case emitter: NotificationEmitter => emitter.removeNotificationListener(this)
      case gc =>
    }
    reporter.close(this)
    if chromeTrace != null then
      chromeTrace.traceDurationEventEnd(Category.Run, "scalac-" + id)
      chromeTrace.close()

  }


  override def handleNotification(notification: Notification, handback: scala.Any): Unit = {
    import java.lang.{Long => jLong}
    import java.lang.{Integer => jInt}
    val reportNs = System.nanoTime()
    val data = notification.getUserData
    val seq = notification.getSequenceNumber
    val message = notification.getMessage
    val tpe = notification.getType
    val time= notification.getTimeStamp
    data match {
      case cd: CompositeData if tpe == "com.sun.management.gc.notification" =>
        val name = cd.get("gcName").toString
        val action = cd.get("gcAction").toString
        val cause = cd.get("gcCause").toString
        val info = cd.get("gcInfo").asInstanceOf[CompositeData]
        val duration = info.get("duration").asInstanceOf[jLong].longValue()
        val startTime = info.get("startTime").asInstanceOf[jLong].longValue()
        val endTime = info.get("endTime").asInstanceOf[jLong].longValue()
        val threads = info.get("GcThreadCount").asInstanceOf[jInt].longValue()
        reporter.reportGc(this, GcEventData("", reportNs, startTime, endTime, name, action, cause, threads))
    }
  }

  override def afterPhase(phase: Phase, snapBefore: ProfileSnap): Unit = {
    assert(mainThread eq Thread.currentThread())
    val initialSnap = snapThread(0)
    if (ctx.settings.YprofileExternalTool.value.contains(phase.toString)) {
      println("Profile hook stop")
      ExternalToolHook.after()
    }
    val finalSnap = if (ctx.settings.YprofileRunGcBetweenPhases.value.contains(phase.toString)) {
      doGC
      initialSnap.updateHeap(readHeapUsage())
    }
    else initialSnap

    if chromeTrace != null
    then chromeTrace.traceDurationEventEnd(Category.Phase, phase.phaseName)

    reporter.reportForeground(this, ProfileRange(snapBefore, finalSnap, phase, "", 0, Thread.currentThread))
  }

  override def beforePhase(phase: Phase): ProfileSnap = {
    assert(mainThread eq Thread.currentThread())
    if chromeTrace != null
    then chromeTrace.traceDurationEventStart(Category.Phase, phase.phaseName)

    if (ctx.settings.YprofileRunGcBetweenPhases.value.contains(phase.toString))
      doGC
    if (ctx.settings.YprofileExternalTool.value.contains(phase.toString)) {
      println("Profile hook start")
      ExternalToolHook.before()
    }
    snapThread(0)
  }

  override def beforeUnit(phase: Phase, unit: CompilationUnit): Unit = {
    assert(mainThread eq Thread.currentThread())
    if (chromeTrace != null) chromeTrace.traceDurationEventStart(Category.File, unit.source.name)
  }

  override def afterUnit(phase: Phase,  unit: CompilationUnit): Unit = {
    assert(mainThread eq Thread.currentThread())
    if (chromeTrace != null) {
      val now = System.nanoTime()
      chromeTrace.traceDurationEventEnd(Category.File, unit.source.name)
      if (now > nextAfterUnitSnap) {
        val initialSnap = snapThread(0)
        chromeTrace.nn.traceCounterEvent("allocBytes", "allocBytes", initialSnap.allocatedBytes, processWide = false)
        chromeTrace.nn.traceCounterEvent("heapBytes", "heapBytes", initialSnap.heapBytes, processWide = true)
        chromeTrace.nn.traceCounterEvent("classesLoaded", "classesLoaded", initialSnap.totalClassesLoaded, processWide = true)
        chromeTrace.nn.traceCounterEvent("jitCompilationTime", "jitCompilationTime", initialSnap.totalJITCompilationTime, processWide = true)
        chromeTrace.nn.traceCounterEvent("userTime", "userTime", initialSnap.userTimeNanos, processWide = false)
        chromeTrace.nn.traceCounterEvent("cpuTime", "cpuTime", initialSnap.cpuTimeNanos, processWide = false)
        chromeTrace.nn.traceCounterEvent("idleTime", "idleTime", initialSnap.idleTimeNanos, processWide = false)
        nextAfterUnitSnap = System.nanoTime() + 10 * 1000 * 1000
      }
    }
  }

  override def beforeTypedImplDef(sym: Symbol): Unit =
    if chromeTrace != null
    then chromeTrace.traceDurationEventStart(Category.TypeCheck, sym.fullName.toString)

  override def afterTypedImplDef(sym: Symbol): Unit =
    if chromeTrace != null
    then chromeTrace.traceDurationEventEnd(Category.TypeCheck, sym.fullName.toString)

  override def beforeImplicitSearch(pt: Type): Unit =
    if chromeTrace != null
    then chromeTrace.traceDurationEventStart(Category.Implicit, "?[" + pt.typeSymbol.fullName + "]", colour = "yellow")


  override def afterImplicitSearch(pt: Type): Unit =
    if chromeTrace != null
    then chromeTrace.traceDurationEventEnd(Category.Implicit, "?[" + pt.typeSymbol.fullName + "]", colour = "yellow")

}

case class EventType(name: String)
object EventType {
  //main thread with other tasks
  val MAIN: EventType = EventType("main")
  //other task ( background thread)
  val BACKGROUND: EventType = EventType("background")
  //total for compile
  val GC: EventType = EventType("GC")
}

sealed trait ProfileReporter {
  def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit
  def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit

  def reportGc(profiler: RealProfiler, data: GcEventData): Unit

  def header(profiler: RealProfiler) :Unit
  def close(profiler: RealProfiler) :Unit
}

object ConsoleProfileReporter extends ProfileReporter {
  @sharable var totalAlloc = 0L

  override def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit =
    reportCommon(EventType.BACKGROUND, profiler, threadRange)
  override def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit =
    reportCommon(EventType.MAIN, profiler, threadRange)
  @nowarn("cat=deprecation")
  private def reportCommon(tpe:EventType, profiler: RealProfiler, threadRange: ProfileRange): Unit =
    totalAlloc += threadRange.allocatedBytes
    println(s"${threadRange.phase.phaseName.replace(',', ' ')},run ns = ${threadRange.runNs},idle ns = ${threadRange.idleNs},cpu ns = ${threadRange.cpuNs},user ns = ${threadRange.userNs},allocated = ${threadRange.allocatedBytes},heap at end = ${threadRange.end.heapBytes}, total allocated = $totalAlloc ")

  override def close(profiler: RealProfiler): Unit = ()

  override def header(profiler: RealProfiler): Unit =
    println(s"Profiler start (${profiler.id}) ${profiler.outDir}")

  override def reportGc(profiler: RealProfiler, data: GcEventData): Unit =
    println(s"Profiler GC reported ${data.gcEndMillis - data.gcStartMillis}ms")
    if(profiler.chromeTrace != null){
      val duration = TimeUnit.MILLISECONDS.toNanos(data.gcEndMillis - data.gcStartMillis + 1)
      val start = data.reportTimeNs - duration
      profiler.chromeTrace.traceDurationEvent(data.name, start, duration, EventType.GC.name)
    }
}

class StreamProfileReporter(out:PrintWriter) extends ProfileReporter {
  override def header(profiler: RealProfiler): Unit = {
    out.println(s"info, ${profiler.id}, version, 2, output, ${profiler.outDir}")
    out.println(s"header(main/background),startNs,endNs,runId,phaseId,phaseName,purpose,task-count,threadId,threadName,runNs,idleNs,cpuTimeNs,userTimeNs,allocatedByte,heapSize")
    out.println(s"header(GC),startNs,endNs,startMs,endMs,name,action,cause,threads")
  }

  override def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit =
    reportCommon(EventType.BACKGROUND, profiler, threadRange)
  override def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit =
    reportCommon(EventType.MAIN, profiler, threadRange)
  @nowarn("cat=deprecation")
  private def reportCommon(tpe:EventType, profiler: RealProfiler, threadRange: ProfileRange): Unit =
    out.println(s"$tpe,${threadRange.start.snapTimeNanos},${threadRange.end.snapTimeNanos},${profiler.id},${threadRange.phase.id},${threadRange.phase.phaseName.replace(',', ' ')},${threadRange.purpose},${threadRange.taskCount},${threadRange.thread.getId},${threadRange.thread.getName},${threadRange.runNs},${threadRange.idleNs},${threadRange.cpuNs},${threadRange.userNs},${threadRange.allocatedBytes},${threadRange.end.heapBytes} ")

  override def reportGc(profiler: RealProfiler, data: GcEventData): Unit = {
    val duration = TimeUnit.MILLISECONDS.toNanos(data.gcEndMillis - data.gcStartMillis + 1)
    val start = data.reportTimeNs - duration
    out.println(s"${EventType.GC},$start,${data.reportTimeNs},${data.gcStartMillis}, ${data.gcEndMillis},${data.name},${data.action},${data.cause},${data.threads}")
    if profiler.chromeTrace != null
    then profiler.chromeTrace.traceDurationEvent(data.name, start, duration, EventType.GC.name)
  }


  override def close(profiler: RealProfiler): Unit = {
    out.flush()
    out.close()
  }
}
