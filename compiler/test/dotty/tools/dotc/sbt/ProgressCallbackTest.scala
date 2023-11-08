package dotty.tools.dotc.sbt

import dotty.tools.DottyTest
import dotty.tools.dotc.core.Contexts.FreshContext
import dotty.tools.dotc.sbt.ProgressCallbackTest.*

import org.junit.Assert.*
import org.junit.Test

import dotty.tools.toOption
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Run
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.io.VirtualDirectory
import dotty.tools.dotc.NoCompilationUnit
import dotty.tools.dotc.interactive.Interactive.Include.all

final class ProgressCallbackTest extends DottyTest:

  @Test
  def testCallback: Unit =
    val source1 = """class Foo"""
    val source2 = """class Bar"""

    inspectProgress(List(source1, source2), terminalPhase = None): progressCallback =>
      locally:
        // (1) assert that the way we compute next phase in `Run.doAdvancePhase` is correct
        assertNextPhaseIsNext()

      locally:
        // (1) given correct computation, check that the recorded progression of phases is monotonic
        assertMonotonicProgression(progressCallback)

      locally:
        // (1) given monotonic progression, check that the recorded progression of phases is complete
        val expectedCurr = allSubPhases
        val expectedNext = expectedCurr.tail ++ syntheticNextPhases
        assertProgressPhases(progressCallback, expectedCurr, expectedNext)

      locally:
        // (2) next check that for each unit, we record all the "runnable" phases that could go through
        assertExpectedPhasesForUnits(progressCallback, expectedPhases = runnableSubPhases)

      locally:
        // (2) therefore we can now cross-reference the recorded progression with the recorded phases per unit
        assertTotalUnits(progressCallback)

      locally:
        // (3) finally, check that the callback was not cancelled
        assertFalse(progressCallback.isCancelled)
  end testCallback

  // TODO: test cancellation

  @Test
  def cancelMidTyper: Unit =
    inspectCancellationAtPhase("typer[typechecking]")

  @Test
  def cancelErasure: Unit =
    inspectCancellationAtPhase("erasure")

  @Test
  def cancelPickler: Unit =
    inspectCancellationAtPhase("pickler")

  def cancelOnEnter(targetPhase: String)(testCallback: TestProgressCallback): Boolean =
    testCallback.latestProgress.exists(_.currPhase == targetPhase)

  def inspectCancellationAtPhase(targetPhase: String): Unit =
    val source1 = """class Foo"""

    inspectProgress(List(source1), cancellation = Some(cancelOnEnter(targetPhase))): progressCallback =>
      locally:
        // (1) assert that the compiler was cancelled
        assertTrue("should have cancelled", progressCallback.isCancelled)

      locally:
        // (2) assert that compiler visited all the subphases before cancellation,
        //     and does not visit any after.
        // (2.2) first extract the surrounding phases of the target
        val (befores, target +: next +: _) = allSubPhases.span(_ != targetPhase): @unchecked
        // (2.3) we expect to see the subphases before&including target reported as a "current" phase, so extract here
        val expectedCurr = befores :+ target
        // (2.4) we expect to see next after target reported as a "next" phase, so extract here
        val expectedNext = expectedCurr.tail :+ next
        assertProgressPhases(progressCallback, expectedCurr, expectedNext)

      locally:
        // (3) assert that the compilation units were only entered in the phases before cancellation
        val (befores, target +: next +: _) = runnableSubPhases.span(_ != targetPhase): @unchecked
        assertExpectedPhasesForUnits(progressCallback, expectedPhases = befores)

      locally:
        // (4) assert that the final progress recorded is at the target phase,
        //     and progress is equal to the number of phases before the target.
        //
        // (4.1) extract the real befores by looking at the runnable phases
        val (befores, target +: _) = runnableSubPhases.span(_ != targetPhase): @unchecked
        // (4.2) extract the predicted next phase by looking at all phases
        val (_, `target` +: next +: _) = allSubPhases.span(_ != targetPhase): @unchecked
        // (4.1) we expect cancellation to occur *as we enter* the target phase,
        //       so no units should be visited in this phase. Therefore progress
        //       should be equal to the number of phases before the target. (as we have 1 unit)
        val expectedProgress = befores.size
        progressCallback.latestProgress match
          case Some(ProgressEvent(`expectedProgress`, _, `target`, `next`)) =>
          case other => fail(s"did not match expected progress, found $other")
  end inspectCancellationAtPhase

  /** Assert that the computed `next` phase matches the real next phase  */
  def assertNextPhaseIsNext()(using Context): Unit =
    val allPhases = ctx.base.allPhases
    for case Array(p1, p2) <- allPhases.sliding(2) do
      val p1Next = Run.SubPhases(p1).next.get.phase // used to compute the next phase in `Run.doAdvancePhase`
      assertEquals(p1Next, p2)

  /** Assert that the recorded progression of phases are all in the real progression, and that order is preserved */
  def assertMonotonicProgression(progressCallback: TestProgressCallback)(using Context): Unit =
    val allPhasePlan = ctx.base.allPhases.flatMap(asSubphases) ++ syntheticNextPhases
    for case List(
      PhaseTransition(curr1, next1),
      PhaseTransition(curr2, next2)
    ) <- progressCallback.progressPhasesFinal.sliding(2) do
      val curr1Index = indexOrFail(allPhasePlan, curr1)
      val curr2Index = indexOrFail(allPhasePlan, curr2)
      val next1Index = indexOrFail(allPhasePlan, next1)
      val next2Index = indexOrFail(allPhasePlan, next2)
      assertTrue(s"Phase `$curr1` did not come before `$curr2`", curr1Index < curr2Index)
      assertTrue(s"Phase `$next1` did not come before `$next2`", next1Index < next2Index)
      assertTrue(s"Phase `$curr1` did not come before `$next1`", curr1Index < next1Index)
      assertTrue(s"Phase `$curr2` did not come before `$next2`", curr2Index < next2Index)
      assertTrue(s"Predicted next phase `$next1` didn't match the following current `$curr2`", next1Index == curr2Index)

  /** Assert that the recorded progression of phases contains every phase in the plan */
  def assertProgressPhases(progressCallback: TestProgressCallback,
      currExpected: Seq[String], nextExpected: Seq[String])(using Context): Unit =
    val (allPhasePlan, expectedCurrPhases, expectedNextPhases) =
      val allPhases = currExpected
      val firstPhase = allPhases.head
      val expectedCurrPhases = allPhases.toSet
      val expectedNextPhases = nextExpected.toSet //expectedCurrPhases - firstPhase ++ syntheticNextPhases
      (allPhases.toList, expectedCurrPhases, expectedNextPhases)

    for (expectedCurr, recordedCurr) <- allPhasePlan.zip(progressCallback.progressPhasesFinal.map(_.curr)) do
      assertEquals(s"Phase $recordedCurr was not expected", expectedCurr, recordedCurr)

    val (seenCurrPhases, seenNextPhases) =
      val (currs0, nexts0) = progressCallback.progressPhasesFinal.unzip(Tuple.fromProductTyped)
      (currs0.toSet, nexts0.toSet)

    val missingCurrPhases = expectedCurrPhases.diff(seenCurrPhases)
    val extraCurrPhases = seenCurrPhases.diff(expectedCurrPhases)
    assertTrue(s"these phases were not visited ${missingCurrPhases}", missingCurrPhases.isEmpty)
    assertTrue(s"these phases were visited, but not in the real plan ${extraCurrPhases}", extraCurrPhases.isEmpty)

    val missingNextPhases = expectedNextPhases.diff(seenNextPhases)
    val extraNextPhases = seenNextPhases.diff(expectedNextPhases)
    assertTrue(s"these phases were not planned to visit, but were expected ${missingNextPhases}", missingNextPhases.isEmpty)
    assertTrue(s"these phases were planned to visit, but were not in the real plan ${extraNextPhases}", extraNextPhases.isEmpty)


  /** Assert that the phases recorded per unit match the actual phases ran on them */
  def assertExpectedPhasesForUnits(progressCallback: TestProgressCallback, expectedPhases: Seq[String])(using Context): Unit =
    for (unit, visitedPhases) <- progressCallback.unitPhases do
      val uniquePhases = visitedPhases.toSet
      assert(unit != NoCompilationUnit, s"unexpected NoCompilationUnit for phases $uniquePhases")
      val duplicatePhases = visitedPhases.view.groupBy(identity).values.filter(_.size > 1).map(_.head)
      assertEquals(s"some phases were visited twice for $unit! ${duplicatePhases.toList}", visitedPhases.size, uniquePhases.size)
      val unvisitedPhases = expectedPhases.filterNot(visitedPhases.contains)
      val extraPhases = visitedPhases.filterNot(expectedPhases.contains)
      assertTrue(s"these phases were not visited for $unit ${unvisitedPhases}", unvisitedPhases.isEmpty)
      assertTrue(s"these phases were visited for $unit, but not expected ${extraPhases}", extraPhases.isEmpty)

  /** Assert that the number of total units of work matches the number of files * the runnable phases */
  def assertTotalUnits(progressCallback: TestProgressCallback)(using Context): Unit =
    var fileTraversals = 0 // files * phases
    for (_, phases) <- progressCallback.unitPhases do
      fileTraversals += phases.size
    val expectedTotal = fileTraversals // assume that no late enters occur
    progressCallback.totalEvents match
      case Nil => fail("No total events recorded")
      case TotalEvent(total, _) :: _ =>
        assertEquals(expectedTotal, total)

  def inspectProgress(
      sources: List[String],
      terminalPhase: Option[String] = Some("typer"),
      cancellation: Option[TestProgressCallback => Boolean] = None)(
      op: Context ?=> TestProgressCallback => Unit)(using Context) =
    for cancelNow <- cancellation do
      testProgressCallback.withCancelNow(cancelNow)
    val sources0 = sources.map(_.linesIterator.map(_.trim.nn).filterNot(_.isEmpty).mkString("\n|").stripMargin)
    val terminalPhase0 = terminalPhase.getOrElse(defaultCompiler.phases.last.last.phaseName)
    checkAfterCompile(terminalPhase0, sources0) { case given Context =>
      op(testProgressCallback)
    }

  private def testProgressCallback(using Context): TestProgressCallback =
    ctx.progressCallback match
      case cb: TestProgressCallback => cb
      case _ =>
        fail(s"Expected TestProgressCallback but got ${ctx.progressCallback}")
        ???

  override protected def initializeCtx(fc: FreshContext): Unit =
    super.initializeCtx(
      fc.setProgressCallback(TestProgressCallback())
        .setSetting(fc.settings.outputDir, new VirtualDirectory("<TestProgressCallbackOutput>"))
    )

object ProgressCallbackTest:

  case class TotalEvent(total: Int, atPhase: String)
  case class ProgressEvent(curr: Int, total: Int, currPhase: String, nextPhase: String)
  case class PhaseTransition(curr: String, next: String)

  def asSubphases(phase: Phase): IndexedSeq[String] =
    val subPhases = Run.SubPhases(phase)
    val indices = 0 until phase.traversals
    indices.map(subPhases.subPhase)

  def runnableSubPhases(using Context): IndexedSeq[String] =
    ctx.base.allPhases.filter(_.isRunnable).flatMap(asSubphases).toIndexedSeq

  def allSubPhases(using Context): IndexedSeq[String] =
    ctx.base.allPhases.flatMap(asSubphases).toIndexedSeq

  private val syntheticNextPhases = List("<end>")

  /** Asserts that the computed phase name exists in the real phase plan */
  def indexOrFail(allPhasePlan: Array[String], phaseName: String): Int =
    val i = allPhasePlan.indexOf(phaseName)
    if i < 0 then
      fail(s"Phase $phaseName not found")
    i

  final class TestProgressCallback extends interfaces.ProgressCallback:
    import collection.immutable, immutable.SeqMap

    private var _cancelled: Boolean = false
    private var _unitPhases: SeqMap[CompilationUnit, List[String]] = immutable.SeqMap.empty // preserve order
    private var _totalEvents: List[TotalEvent] = List.empty
    private var _latestProgress: Option[ProgressEvent] = None
    private var _progressPhases: List[PhaseTransition] = List.empty
    private var _shouldCancelNow: TestProgressCallback => Boolean = _ => false

    def totalEvents = _totalEvents
    def latestProgress = _latestProgress
    def unitPhases = _unitPhases
    def progressPhasesFinal = _progressPhases.reverse
    def currentPhase = _progressPhases.headOption.map(_.curr)

    def withCancelNow(f: TestProgressCallback => Boolean): this.type =
      _shouldCancelNow = f
      this

    override def cancel(): Unit = _cancelled = true
    override def isCancelled(): Boolean = _cancelled

    override def informUnitStarting(phase: String, unit: CompilationUnit): Unit =
      _unitPhases += (unit -> (unitPhases.getOrElse(unit, Nil) :+ phase))

    override def progress(current: Int, total: Int, currPhase: String, nextPhase: String): Boolean =
      // record the total and current phase whenever the total changes
      _totalEvents = _totalEvents match
        case Nil => TotalEvent(total, currPhase) :: Nil
        case events @ (head :: _) if head.total != total => TotalEvent(total, currPhase) :: events
        case events => events

      _latestProgress = Some(ProgressEvent(current, total, currPhase, nextPhase))

      // record the current and next phase whenever the current phase changes
      _progressPhases = _progressPhases match
        case all @ PhaseTransition(head, _) :: rest =>
          if head != currPhase then
            PhaseTransition(currPhase, nextPhase) :: all
          else
            all
        case Nil => PhaseTransition(currPhase, nextPhase) :: Nil

      !_shouldCancelNow(this)

end ProgressCallbackTest
