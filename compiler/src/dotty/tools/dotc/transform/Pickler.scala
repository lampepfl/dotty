package dotty.tools
package dotc
package transform

import core.*
import Contexts.*
import Decorators.*
import tasty.*
import config.Printers.{noPrinter, pickling}
import config.Feature
import java.io.PrintStream
import io.ClassfileWriterOps
import StdNames.{str, nme}
import Periods.*
import Phases.*
import Symbols.*
import StdNames.str
import Flags.Module
import reporting.{ThrowingReporter, Profile, Message}
import collection.mutable
import util.concurrent.{Executor, Future}
import compiletime.uninitialized
import dotty.tools.io.JarArchive
import dotty.tools.dotc.printing.OutlinePrinter

object Pickler {
  val name: String = "pickler"
  val description: String = "generates TASTy info"

  /** If set, perform jump target compacting, position and comment pickling,
   *  as well as final assembly in parallel with downstream phases; force
   *  only in backend.
   */
  inline val ParallelPickling = true

  class EarlyFileWriter(writer: ClassfileWriterOps):
    export writer.{writeTasty, close, outputDir}

  def writeEarlyTasty(units: List[CompilationUnit])(using Context): Unit =
    val earlyOut = ctx.settings.YearlyTastyOutput.value
    val base =
      if earlyOut.isDirectory && earlyOut.exists then
        Pickler.EarlyFileWriter(ClassfileWriterOps(earlyOut)) :: Nil
      else
        Nil

    val writers =
      if ctx.isOutlineFirstPass then
        val extra = Pickler.EarlyFileWriter(ClassfileWriterOps(ctx.settings.YoutlineClasspath.value))
        extra +: base
      else
        base
    if writers.nonEmpty then
      writeSigFiles(units, writers)

  // Why we only write to early output in the first run?
  // ===================================================
  // TL;DR the point of pipeline compilation is to start downstream projects early,
  // so we don't want to wait for suspended units to be compiled.
  //
  // But why is it safe to ignore suspended units?
  // If this project contains a transparent macro that is called in the same project,
  // the compilation unit of that call will be suspended (if the macro implementation
  // is also in this project), causing a second run.
  // However before we do that run, we will have already requested sbt to begin
  // early downstream compilation. This means that the suspended definitions will not
  // be visible in *early* downstream compilation.
  //
  // However, sbt will by default prevent downstream compilation happening in this scenario,
  // due to the existence of macro definitions. So we are protected from failure if user tries
  // to use the suspended definitions.
  //
  // Additionally, it is recommended for the user to move macro implementations to another project
  // if they want to force early output. In this scenario the suspensions will no longer occur, so now
  // they will become visible in the early-output.
  //
  // See `sbt-test/pipelining/pipelining-scala-macro` and `sbt-test/pipelining/pipelining-scala-macro-force`
  // for examples of this in action.
  //
  // Therefore we only need to write to early output in the first run. We also provide the option
  // to diagnose suspensions with the `-Yno-suspended-units` flag.
  private def writeSigFiles(units: List[CompilationUnit], writers: Seq[Pickler.EarlyFileWriter])(using Context): Unit = {
    try
      for
        unit <- units
        (cls, pickled) <- locally {
          pickling.println(s"pickling tasty of unit $unit")
          if ctx.isOutlineFirstPass then
            unit.outlinePickled
          else
            assert(!ctx.isOutline)
            unit.pickled
        }
        if cls.isDefinedInCurrentRun || { report.warning(s"dropping $cls from tasty, could not find in current run"); true }
      do
        val binaryName = cls.binaryClassName.replace('.', java.io.File.separatorChar).nn
        val binaryClassName = if (cls.is(Module)) binaryName.stripSuffix(str.MODULE_SUFFIX).nn else binaryName
        pickling.println(s"writing class ${binaryClassName} to early tasty")
        val bytes = pickled()
        writers.foreach(_.writeTasty(binaryClassName, bytes))
    finally
      writers.foreach(_.close())
      if ctx.settings.verbose.value then
        report.echo(s"[sig files written to ${writers.map(_.outputDir).mkString(", ")}]")
    end try
  }
}

/** This phase pickles trees */
class Pickler extends Phase {
  import ast.tpd.*

  override def phaseName: String = Pickler.name

  override def description: String = Pickler.description

  // No need to repickle trees coming from TASTY
  override def isRunnable(using Context): Boolean =
    super.isRunnable && !ctx.settings.fromTasty.value

  // when `-Yjava-tasty` is set we actually want to run this phase on Java sources
  override def skipIfJava(using Context): Boolean = false

  private def output(name: String, msg: String) = {
    val s = new PrintStream(name)
    s.print(msg)
    s.close
  }

  // Maps that keep a record if -Ytest-pickler is set.
  private val beforePickling = new mutable.HashMap[ClassSymbol, String]
  private val pickledBytes = new mutable.HashMap[ClassSymbol, (CompilationUnit, Array[Byte])]

  /** Drop any elements of this list that are linked module classes of other elements in the list */
  private def dropCompanionModuleClasses(clss: List[ClassSymbol])(using Context): List[ClassSymbol] = {
    val companionModuleClasses =
      clss.filterNot(_.is(Module)).map(_.linkedClass).filterNot(_.isAbsent())
    clss.filterNot(companionModuleClasses.contains)
  }

  /** Runs given functions with a scratch data block in a serialized fashion (i.e.
   *  inside a synchronized block). Scratch data is re-used between calls.
   *  Used to conserve on memory usage by avoiding to create scratch data for each
   *  pickled unit.
   */
  object serialized:
    val scratch = new ScratchData
    def run(body: ScratchData => Array[Byte]): Array[Byte] =
      synchronized {
        scratch.reset()
        body(scratch)
      }

  private var executor = Executor[Array[Byte]]()

  private def useExecutor(using Context) = Pickler.ParallelPickling && !ctx.settings.YtestPickler.value

  private def printerContext(isOutline: Boolean)(using Context): Context =
    if isOutline then ctx.fresh.setPrinterFn(OutlinePrinter(_))
    else ctx

  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    pickling.println(i"unpickling in run ${ctx.runId}")

    for
      cls <- dropCompanionModuleClasses(topLevelClasses(unit.tpdTree))
      tree <- sliceTopLevel(unit.tpdTree, cls)
    do
      if ctx.settings.YtestPickler.value then beforePickling(cls) =
        tree.show(using printerContext(unit.typedAsJava))

      val sourceRelativePath =
        val reference = ctx.settings.sourceroot.value
        util.SourceFile.relativePath(unit.source, reference)
      val isJavaAttr = unit.isJava // we must always set JAVAattr when pickling Java sources
      if isJavaAttr then
        // assert that Java sources didn't reach Pickler without `-Yjava-tasty`.
        assert(ctx.settings.YjavaTasty.value, "unexpected Java source file without -Yjava-tasty")
      val isOutline = isJavaAttr || ctx.isOutline
      val attributes = Attributes(
        sourceFile = sourceRelativePath,
        scala2StandardLibrary = ctx.settings.YcompileScala2Library.value,
        explicitNulls = ctx.settings.YexplicitNulls.value,
        captureChecked = Feature.ccEnabled,
        withPureFuns = Feature.pureFunsEnabled,
        isJava = isJavaAttr,
        isOutline = isOutline
      )

      val pickler = new TastyPickler(cls)
      val treePkl = new TreePickler(pickler, attributes)
      treePkl.pickle(tree :: Nil)
      Profile.current.recordTasty(treePkl.buf.length)

      val positionWarnings = new mutable.ListBuffer[Message]()
      def reportPositionWarnings() = positionWarnings.foreach(report.warning(_))

      def computePickled(): Array[Byte] = inContext(ctx.fresh) {
        serialized.run { scratch =>
          treePkl.compactify(scratch)
          if tree.span.exists then
            val reference = ctx.settings.sourceroot.value
            PositionPickler.picklePositions(
                pickler, treePkl.buf.addrOfTree, treePkl.treeAnnots, reference,
                unit.source, tree :: Nil, positionWarnings,
                scratch.positionBuffer, scratch.pickledIndices)

          if !ctx.settings.YdropComments.value then
            CommentPickler.pickleComments(
                pickler, treePkl.buf.addrOfTree, treePkl.docString, tree,
                scratch.commentBuffer)

          AttributePickler.pickleAttributes(attributes, pickler, scratch.attributeBuffer)

          val pickled = pickler.assembleParts()

          def rawBytes = // not needed right now, but useful to print raw format.
            pickled.iterator.grouped(10).toList.zipWithIndex.map {
              case (row, i) => s"${i}0: ${row.mkString(" ")}"
            }

          // println(i"rawBytes = \n$rawBytes%\n%") // DEBUG
          if ctx.settings.YprintTasty.value || pickling != noPrinter then
            println(i"**** pickled info of $cls")
            println(TastyPrinter.showContents(pickled, ctx.settings.color.value == "never"))
            println(i"**** end of pickled info of $cls")
          pickled
        }
      }

      /** A function that returns the pickled bytes. Depending on `Pickler.ParallelPickling`
       *  either computes the pickled data in a future or eagerly before constructing the
       *  function value.
       */
      val demandPickled: () => Array[Byte] =
        if useExecutor then
          val futurePickled = executor.schedule(computePickled)
          () =>
            try futurePickled.force.get
            finally reportPositionWarnings()
        else
          val pickled = computePickled()
          reportPositionWarnings()
          if ctx.settings.YtestPickler.value then pickledBytes(cls) = (unit, pickled)
          () => pickled

      if ctx.isOutlineFirstPass then
        unit.outlinePickled += (cls -> demandPickled)
      else // either non-outline, or second pass
        unit.pickled += (cls -> demandPickled)
    end for
  }

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    val units0 =
      if useExecutor then
        executor.start()
        try super.runOn(units)
        finally executor.close() // NOTE: each run will create a new Pickler phase, so safe to close here.
      else
        super.runOn(units)
    if ctx.settings.YtestPickler.value then
      val ctx2 = ctx.fresh
        .setSetting(ctx.settings.YreadComments, true)
        .setSetting(ctx.settings.YshowPrintErrors, true)
      testUnpickler(
        using ctx2
          .setPeriod(Period(ctx.runId + 1, ctx.base.typerPhase.id))
          .setReporter(new ThrowingReporter(ctx.reporter))
          .addMode(Mode.ReadPositions)
      )
    units0
  }

  private def testUnpickler(using Context): Unit =
    pickling.println(i"testing unpickler at run ${ctx.runId}")
    ctx.initialize()
    val unpicklers =
      for ((cls, (unit, bytes)) <- pickledBytes) yield {
        val unpickler = new DottyUnpickler(unit.source.file, bytes)
        unpickler.enter(roots = Set.empty)
        cls -> (unit, unpickler)
      }
    pickling.println("************* entered toplevel ***********")
    val rootCtx = ctx
    for ((cls, (unit, unpickler)) <- unpicklers) do
      val testJava = unit.typedAsJava
      if testJava then
        if unpickler.unpickler.nameAtRef.contents.exists(_ == nme.FromJavaObject) then
          report.error(em"Pickled reference to FromJavaObject in Java defined $cls in ${cls.source}")
      val unpickled = unpickler.rootTrees
      val freshUnit = CompilationUnit(rootCtx.compilationUnit.source)
      freshUnit.needsCaptureChecking = unit.needsCaptureChecking
      freshUnit.knowsPureFuns = unit.knowsPureFuns
      inContext(printerContext(testJava)(using rootCtx.fresh.setCompilationUnit(freshUnit))):
        testSame(i"$unpickled%\n%", beforePickling(cls), cls)

  private def testSame(unpickled: String, previous: String, cls: ClassSymbol)(using Context) =
    import java.nio.charset.StandardCharsets.UTF_8
    def normal(s: String) = new String(s.getBytes(UTF_8), UTF_8)
    val unequal = unpickled.length() != previous.length() || normal(unpickled) != normal(previous)
    if unequal then
      output("before-pickling.txt", previous)
      output("after-pickling.txt", unpickled)
      //sys.process.Process("diff -u before-pickling.txt after-pickling.txt").!
      report.error(em"""pickling difference for $cls in ${cls.source}, for details:
                    |
                    |  diff before-pickling.txt after-pickling.txt""")
  end testSame
}
