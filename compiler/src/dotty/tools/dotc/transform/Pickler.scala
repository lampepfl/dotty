package dotty.tools.dotc
package transform

import core._
import Contexts._
import Decorators._
import tasty._
import config.Printers.{noPrinter, pickling}
import java.io.PrintStream
import Periods._
import Phases._
import Symbols._
import Flags.Module
import reporting.{ThrowingReporter, Profile}
import collection.mutable
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration.Duration

object Pickler {
  val name: String = "pickler"
  val description: String = "generates TASTy info"

  /** If set, perform jump target compacting, position and comment pickling,
   *  as well as final assembly in parallel with downstream phases; force
   *  only in backend.
   */
  inline val ParallelPickling = true
}

/** This phase pickles trees */
class Pickler extends Phase {
  import ast.tpd._

  override def phaseName: String = Pickler.name

  override def description: String = Pickler.description

  // No need to repickle trees coming from TASTY
  override def isRunnable(using Context): Boolean =
    super.isRunnable && !ctx.settings.fromTasty.value

  private def output(name: String, msg: String) = {
    val s = new PrintStream(name)
    s.print(msg)
    s.close
  }

  // Maps that keep a record if -Ytest-pickler is set.
  private val beforePickling = new mutable.HashMap[ClassSymbol, String]
  private val picklers = new mutable.HashMap[ClassSymbol, TastyPickler]

  private val typeSimplifier = new TypeSimplifier

  /** Drop any elements of this list that are linked module classes of other elements in the list */
  private def dropCompanionModuleClasses(clss: List[ClassSymbol])(using Context): List[ClassSymbol] = {
    val companionModuleClasses =
      clss.filterNot(_.is(Module)).map(_.linkedClass).filterNot(_.isAbsent())
    clss.filterNot(companionModuleClasses.contains)
  }

  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    pickling.println(i"unpickling in run ${ctx.runId}")

    for
      cls <- dropCompanionModuleClasses(topLevelClasses(unit.tpdTree))
      tree <- sliceTopLevel(unit.tpdTree, cls)
    do
      val pickler = new TastyPickler(cls)
      if ctx.settings.YtestPickler.value then
        beforePickling(cls) = tree.show
        picklers(cls) = pickler
      val treePkl = new TreePickler(pickler)
      treePkl.pickle(tree :: Nil)
      Profile.current.recordTasty(treePkl.buf.length)
      val positionWarnings = new mutable.ListBuffer[String]()
      val pickledF = inContext(ctx.fresh) {
        Future {
          treePkl.compactify()
          if tree.span.exists then
            val reference = ctx.settings.sourceroot.value
            new PositionPickler(pickler, treePkl.buf.addrOfTree, treePkl.treeAnnots, reference)
              .picklePositions(unit.source, tree :: Nil, positionWarnings)

          if !ctx.settings.YdropComments.value then
            new CommentPickler(pickler, treePkl.buf.addrOfTree, treePkl.docString)
              .pickleComment(tree)

          val pickled = pickler.assembleParts()

          def rawBytes = // not needed right now, but useful to print raw format.
            pickled.iterator.grouped(10).toList.zipWithIndex.map {
              case (row, i) => s"${i}0: ${row.mkString(" ")}"
            }

          // println(i"rawBytes = \n$rawBytes%\n%") // DEBUG
          if pickling ne noPrinter then
            pickling.synchronized {
              println(i"**** pickled info of $cls")
              println(TastyPrinter.showContents(pickled, ctx.settings.color.value == "never"))
            }
          // println(i"**** pickled info of $cls")
          // println(TastyPrinter.showContents(pickled, ctx.settings.color.value == "never"))
          if cls.show.contains("MainGenericRunner") then
            import java.nio.file.{Paths, Files}
            import java.nio.charset.StandardCharsets

            Files.write(Paths.get("debug.txt"), TastyPrinter.showContents(pickled, true).getBytes(StandardCharsets.UTF_8))
          pickled
        }(using ExecutionContext.global)
      }
      def force(): Array[Byte] =
        val result = Await.result(pickledF, Duration.Inf)
        positionWarnings.foreach(report.warning(_))
        result

      if !Pickler.ParallelPickling || ctx.settings.YtestPickler.value then force()

      unit.pickled += (cls -> force)
    end for
  }

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    val result = super.runOn(units)
    if ctx.settings.YtestPickler.value then
      val ctx2 = ctx.fresh.setSetting(ctx.settings.YreadComments, true)
      testUnpickler(
        using ctx2
            .setPeriod(Period(ctx.runId + 1, ctx.base.typerPhase.id))
            .setReporter(new ThrowingReporter(ctx.reporter))
            .addMode(Mode.ReadPositions)
            .addMode(Mode.PrintShowExceptions))
    result
  }

  private def testUnpickler(using Context): Unit = {
    pickling.println(i"testing unpickler at run ${ctx.runId}")
    ctx.initialize()
    val unpicklers =
      for ((cls, pickler) <- picklers) yield {
        val unpickler = new DottyUnpickler(pickler.assembleParts())
        unpickler.enter(roots = Set.empty)
        cls -> unpickler
      }
    pickling.println("************* entered toplevel ***********")
    for ((cls, unpickler) <- unpicklers) {
      val unpickled = typeSimplifier.transform(unpickler.rootTrees)
      testSame(i"$unpickled%\n%", beforePickling(cls), cls)
    }
  }

  private def testSame(unpickled: String, previous: String, cls: ClassSymbol)(using Context) =
    import java.nio.charset.StandardCharsets.UTF_8
    def normal(s: String) = new String(s.getBytes(UTF_8), UTF_8)
    val unequal = unpickled.length() != previous.length() || normal(unpickled) != normal(previous)
    if unequal then
      output("before-pickling.txt", previous)
      output("after-pickling.txt", unpickled)
      report.error(s"""pickling difference for $cls in ${cls.source}, for details:
                   |
                   |  diff before-pickling.txt after-pickling.txt""".stripMargin)
  end testSame

  // Overwrite types of If, Match, and Try nodes with simplified types
  // to avoid inconsistencies in unsafe nulls
  class TypeSimplifier extends TreeMapWithPreciseStatContexts:
    override def transform(tree: Tree)(using Context): Tree =
      try tree match
        case _: If | _: Match | _: Try =>
          val newTree = super.transform(tree)
          newTree.overwriteType(newTree.tpe.simplified)
          newTree
        case _ =>
          super.transform(tree)
      catch
        case ex: TypeError =>
          report.error(ex, tree.srcPos)
          tree
  end TypeSimplifier
}
