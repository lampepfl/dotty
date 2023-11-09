package dotty.tools
package dotc

import core.*
import Contexts.*
import Periods.*
import Symbols.*
import Scopes.*
import Names.Name
import Denotations.Denotation
import typer.Typer
import typer.ImportInfo.withRootImports
import Decorators.*
import io.AbstractFile
import Phases.{unfusedPhases, Phase}

import sbt.interfaces.ProgressCallback

import util.*
import reporting.{Suppression, Action, Profile, ActiveProfile, NoProfile}
import reporting.Diagnostic
import reporting.Diagnostic.Warning
import rewrites.Rewrites
import profile.Profiler
import printing.XprintMode
import typer.ImplicitRunInfo
import config.Feature
import StdNames.nme

import java.io.{BufferedWriter, OutputStreamWriter}
import java.nio.charset.StandardCharsets

import scala.collection.mutable
import scala.util.control.NonFatal
import scala.io.Codec

import Run.Progress
import scala.compiletime.uninitialized
import dotty.tools.dotc.transform.MegaPhase

/** A compiler run. Exports various methods to compile source files */
class Run(comp: Compiler, ictx: Context) extends ImplicitRunInfo with ConstraintRunInfo {

  /** Default timeout to stop looking for further implicit suggestions, in ms.
   *  This is usually for the first import suggestion; subsequent suggestions
   *  may get smaller timeouts. @see ImportSuggestions.reduceTimeBudget
   */
  private var myImportSuggestionBudget: Int =
    Int.MinValue // sentinel value; means whatever is set in command line option

  def importSuggestionBudget =
    if myImportSuggestionBudget == Int.MinValue then ictx.settings.XimportSuggestionTimeout.value
    else myImportSuggestionBudget

  def importSuggestionBudget_=(x: Int) =
    myImportSuggestionBudget = x

  /** If this variable is set to `true`, some core typer operations will
   *  return immediately. Currently these early abort operations are
   *  `Typer.typed` and `Implicits.typedImplicit`.
   */
  @volatile var isCancelled = false

  private var compiling = false

  private var myUnits: List[CompilationUnit] = Nil
  private var myUnitsCached: List[CompilationUnit] = Nil
  private var myFiles: Set[AbstractFile] = uninitialized

  // `@nowarn` annotations by source file, populated during typer
  private val mySuppressions: mutable.LinkedHashMap[SourceFile, mutable.ListBuffer[Suppression]] = mutable.LinkedHashMap.empty
  // source files whose `@nowarn` annotations are processed
  private val mySuppressionsComplete: mutable.Set[SourceFile] = mutable.Set.empty
  // warnings issued before a source file's `@nowarn` annotations are processed, suspended so that `@nowarn` can filter them
  private val mySuspendedMessages: mutable.LinkedHashMap[SourceFile, mutable.LinkedHashSet[Warning]] = mutable.LinkedHashMap.empty

  object suppressions:
    // When the REPL creates a new run (ReplDriver.compile), parsing is already done in the old context, with the
    // previous Run. Parser warnings were suspended in the old run and need to be copied over so they are not lost.
    // Same as scala/scala/commit/79ca1408c7.
    def initSuspendedMessages(oldRun: Run | Null) = if oldRun != null then
      mySuspendedMessages.clear()
      mySuspendedMessages ++= oldRun.mySuspendedMessages

    def suppressionsComplete(source: SourceFile) = source == NoSource || mySuppressionsComplete(source)

    def addSuspendedMessage(warning: Warning) =
      mySuspendedMessages.getOrElseUpdate(warning.pos.source, mutable.LinkedHashSet.empty) += warning

    def nowarnAction(dia: Diagnostic): Action.Warning.type | Action.Verbose.type | Action.Silent.type =
      mySuppressions.getOrElse(dia.pos.source, Nil).find(_.matches(dia)) match {
        case Some(s) =>
          s.markUsed()
          if (s.verbose) Action.Verbose
          else Action.Silent
        case _ =>
          Action.Warning
      }

    def addSuppression(sup: Suppression): Unit =
      val source = sup.annotPos.source
      mySuppressions.getOrElseUpdate(source, mutable.ListBuffer.empty) += sup

    def reportSuspendedMessages(source: SourceFile)(using Context): Unit = {
      // sort suppressions. they are not added in any particular order because of lazy type completion
      for (sups <- mySuppressions.get(source))
        mySuppressions(source) = sups.sortBy(sup => 0 - sup.start)
      mySuppressionsComplete += source
      mySuspendedMessages.remove(source).foreach(_.foreach(ctx.reporter.issueIfNotSuppressed))
    }

    def runFinished(hasErrors: Boolean): Unit =
      // report suspended messages (in case the run finished before typer)
      mySuspendedMessages.keysIterator.toList.foreach(reportSuspendedMessages)
      // report unused nowarns only if all all phases are done
      if !hasErrors && ctx.settings.WunusedHas.nowarn then
        for {
          source <- mySuppressions.keysIterator.toList
          sups   <- mySuppressions.remove(source)
          sup    <- sups.reverse
        } if (!sup.used)
          report.warning("@nowarn annotation does not suppress any warnings", sup.annotPos)

  /** The compilation units currently being compiled, this may return different
   *  results over time.
   */
  def units: List[CompilationUnit] = myUnits

  private def units_=(us: List[CompilationUnit]): Unit =
    myUnits = us

  var suspendedUnits: mutable.ListBuffer[CompilationUnit] = mutable.ListBuffer()

  def checkSuspendedUnits(newUnits: List[CompilationUnit])(using Context): Unit =
    if newUnits.isEmpty && suspendedUnits.nonEmpty && !ctx.reporter.errorsReported then
      val where =
        if suspendedUnits.size == 1 then i"in ${suspendedUnits.head}."
        else i"""among
                |
                |  ${suspendedUnits.toList}%, %
                |"""
      val enableXprintSuspensionHint =
        if ctx.settings.XprintSuspension.value then ""
        else "\n\nCompiling with  -Xprint-suspension   gives more information."
      report.error(em"""Cyclic macro dependencies $where
                    |Compilation stopped since no further progress can be made.
                    |
                    |To fix this, place macros in one set of files and their callers in another.$enableXprintSuspensionHint""")

  /** The files currently being compiled (active or suspended).
   *  This may return different results over time.
   *  These files do not have to be source files since it's possible to compile
   *  from TASTY.
   */
  def files: Set[AbstractFile] = {
    if (myUnits ne myUnitsCached) {
      myUnitsCached = myUnits
      myFiles = (myUnits ++ suspendedUnits).map(_.source.file).toSet
    }
    myFiles
  }

  /** The source files of all late entered symbols, as a set */
  private val lateFiles = mutable.Set[AbstractFile]()

  /** A cache for static references to packages and classes */
  val staticRefs = util.EqHashMap[Name, Denotation](initialCapacity = 1024)

  /** Actions that need to be performed at the end of the current compilation run */
  private var finalizeActions = mutable.ListBuffer[() => Unit]()

  private var _progress: Progress | Null = null // Set if progress reporting is enabled

  private inline def trackProgress(using Context)(inline op: Context ?=> Progress => Unit): Unit =
    foldProgress(())(op)

  private inline def foldProgress[T](using Context)(inline default: T)(inline op: Context ?=> Progress => T): T =
    val local = _progress
    if local != null then
      op(using ctx)(local)
    else
      default

  def didEnterUnit(unit: CompilationUnit)(using Context): Boolean =
    foldProgress(true /* should progress by default */)(_.tryEnterUnit(unit))

  def canProgress()(using Context): Boolean =
    foldProgress(true /* not cancelled by default */)(p => !p.checkCancellation())

  def doAdvanceUnit()(using Context): Unit =
    trackProgress: progress =>
      progress.currentUnitCount += 1 // trace that we completed a unit in the current (sub)phase
      progress.refreshProgress()

  def doAdvanceLate()(using Context): Unit =
    trackProgress: progress =>
      progress.currentLateUnitCount += 1 // trace that we completed a late compilation
      progress.refreshProgress()

  private def doEnterPhase(currentPhase: Phase)(using Context): Unit =
    trackProgress: progress =>
      progress.enterPhase(currentPhase)

  /** interrupt the thread and set cancellation state */
  private def cancelInterrupted(): Unit =
    try
      trackProgress(_.cancel())
    finally
      Thread.currentThread().nn.interrupt()

  private def doAdvancePhase(currentPhase: Phase, wasRan: Boolean)(using Context): Unit =
    trackProgress: progress =>
      progress.currentUnitCount = 0 // reset unit count in current (sub)phase
      progress.currentCompletedSubtraversalCount = 0 // reset subphase index to initial
      progress.seenPhaseCount += 1 // trace that we've seen a (sub)phase
      if wasRan then
        // add an extra traversal now that we completed a (sub)phase
        progress.completedTraversalCount += 1
      else
        // no subphases were ran, remove traversals from expected total
        progress.totalTraversals -= currentPhase.traversals

  private def tryAdvanceSubPhase()(using Context): Unit =
    trackProgress: progress =>
      if progress.canAdvanceSubPhase then
        progress.currentUnitCount = 0 // reset unit count in current (sub)phase
        progress.seenPhaseCount += 1 // trace that we've seen a (sub)phase
        progress.completedTraversalCount += 1 // add an extra traversal now that we completed a (sub)phase
        progress.currentCompletedSubtraversalCount += 1 // record that we've seen a subphase
        if !progress.isCancelled() then
          progress.tickSubphase()

  /** Will be set to true if any of the compiled compilation units contains
   *  a pureFunctions language import.
   */
  var pureFunsImportEncountered = false

  /** Will be set to true if experimental.captureChecking is enabled
   *  or any of the compiled compilation units contains a captureChecking language import.
   */
  var ccEnabledSomewhere = Feature.enabledBySetting(Feature.captureChecking)(using ictx)

  private var myEnrichedErrorMessage = false

  def compile(files: List[AbstractFile]): Unit =
    try compileSources(files.map(runContext.getSource(_)))
    catch case NonFatal(ex) if !this.enrichedErrorMessage =>
      val files1 = if units.isEmpty then files else units.map(_.source.file)
      report.echo(this.enrichErrorMessage(s"exception occurred while compiling ${files1.map(_.path)}"))
      throw ex

  /** TODO: There's a fundamental design problem here: We assemble phases using `fusePhases`
   *  when we first build the compiler. But we modify them with -Yskip, -Ystop
   *  on each run. That modification needs to either transform the tree structure,
   *  or we need to assemble phases on each run, and take -Yskip, -Ystop into
   *  account. I think the latter would be preferable.
   */
  def compileSources(sources: List[SourceFile]): Unit =
    if (sources forall (_.exists)) {
      units = sources.map(CompilationUnit(_))
      compileUnits()
    }


  def compileUnits(us: List[CompilationUnit]): Unit = {
    units = us
    compileUnits()
  }

  def compileUnits(us: List[CompilationUnit], ctx: Context): Unit = {
    units = us
    compileUnits()(using ctx)
  }

  var profile: Profile = NoProfile

  private def compileUnits()(using Context) = Stats.maybeMonitored {
    if (!ctx.mode.is(Mode.Interactive)) // IDEs might have multi-threaded access, accesses are synchronized
      ctx.base.checkSingleThreaded()

    compiling = true

    profile =
      if ctx.settings.Vprofile.value
        || !ctx.settings.VprofileSortedBy.value.isEmpty
        || ctx.settings.VprofileDetails.value != 0
      then ActiveProfile(ctx.settings.VprofileDetails.value.max(0).min(1000))
      else NoProfile

    // If testing pickler, of outline first pass, make sure to stop after pickling phase:
    val stopAfter =
      if (ctx.settings.YtestPickler.value) List("pickler")
      else if (ctx.isOutlineFirstPass) List("sbt-api")
      else ctx.settings.YstopAfter.value

    val pluginPlan = ctx.base.addPluginPhases(ctx.base.phasePlan)
    val phases = ctx.base.fusePhases(pluginPlan,
      ctx.settings.Yskip.value, ctx.settings.YstopBefore.value, stopAfter, ctx.settings.Ycheck.value)
    ctx.base.usePhases(phases)

    if ctx.settings.YnoDoubleBindings.value then
      ctx.base.checkNoDoubleBindings = true

    def runPhases(allPhases: Array[Phase])(using Context) = {
      var lastPrintedTree: PrintedTree = NoPrintedTree
      val profiler = ctx.profiler
      var phasesWereAdjusted = false

      for phase <- allPhases do
        doEnterPhase(phase)
        val phaseWillRun = phase.isRunnable
        if phaseWillRun then
          Stats.trackTime(s"phase time ms/$phase") {
            val start = System.currentTimeMillis
            val profileBefore = profiler.beforePhase(phase)
            try units = phase.runOn(units)
            catch case _: InterruptedException => cancelInterrupted()
            profiler.afterPhase(phase, profileBefore)
            if (ctx.settings.Xprint.value.containsPhase(phase))
              for (unit <- units)
                def printCtx(unit: CompilationUnit) = phase.printingContext(
                  ctx.fresh.setPhase(phase.next).setCompilationUnit(unit))
                lastPrintedTree = printTree(lastPrintedTree)(using printCtx(unit))
            report.informTime(s"$phase ", start)
            Stats.record(s"total trees at end of $phase", ast.Trees.ntrees)
            for (unit <- units)
              Stats.record(s"retained typed trees at end of $phase", unit.tpdTree.treeSize)
            ctx.typerState.gc()
          }
          if !phasesWereAdjusted then
            phasesWereAdjusted = true
            if !Feature.ccEnabledSomewhere then
              ctx.base.unlinkPhaseAsDenotTransformer(Phases.checkCapturesPhase.prev)
              ctx.base.unlinkPhaseAsDenotTransformer(Phases.checkCapturesPhase)
            end if
          end if
        end if
        doAdvancePhase(phase, wasRan = phaseWillRun)
      end for
      profiler.finished()
    }

    val runCtx = ctx.fresh
    runCtx.setProfiler(Profiler())
    unfusedPhases.foreach(_.initContext(runCtx))
    val fusedPhases = runCtx.base.allPhases
    runCtx.withProgressCallback: cb =>
      _progress = Progress(cb, this, fusedPhases.map(_.traversals).sum)
    runPhases(allPhases = fusedPhases)(using runCtx)
    if (!ctx.reporter.hasErrors)
      Rewrites.writeBack()
    suppressions.runFinished(hasErrors = ctx.reporter.hasErrors)
    while (finalizeActions.nonEmpty && canProgress()) {
      val action = finalizeActions.remove(0)
      action()
    }
    compiling = false
  }

  /** Enter top-level definitions of classes and objects contained in source file `file`.
   *  The newly added symbols replace any previously entered symbols.
   *  If `typeCheck = true`, also run typer on the compilation unit, and set
   *  `rootTreeOrProvider`.
   */
  def lateCompile(file: AbstractFile, typeCheck: Boolean)(using Context): Unit =
    if (!files.contains(file) && !lateFiles.contains(file)) {
      lateFiles += file

      val unit = CompilationUnit(ctx.getSource(file))
      val unitCtx = runContext.fresh
        .setCompilationUnit(unit)
        .withRootImports

      def process()(using Context) =
        ctx.typer.lateEnterUnit(typeCheck)(doTypeCheck =>
          if compiling then finalizeActions += doTypeCheck
          else doTypeCheck()
        )

      process()(using unitCtx)
    }

  private sealed trait PrintedTree
  private /*final*/ case class SomePrintedTree(phase: String, tree: String) extends PrintedTree
  private object NoPrintedTree extends PrintedTree

  private def printTree(last: PrintedTree)(using Context): PrintedTree = {
    val unit = ctx.compilationUnit
    val fusedPhase = ctx.phase.prev.megaPhase
    val echoHeader = f"[[syntax trees at end of $fusedPhase%25s]] // ${unit.source}"
    val tree = if ctx.isAfterTyper then unit.tpdTree else unit.untpdTree
    val treeString = fusedPhase.show(tree)

    last match {
      case SomePrintedTree(phase, lastTreeString) if lastTreeString == treeString =>
        report.echo(s"$echoHeader: unchanged since $phase")
        last

      case SomePrintedTree(phase, lastTreeString) if ctx.settings.XprintDiff.value || ctx.settings.XprintDiffDel.value =>
        val diff = DiffUtil.mkColoredCodeDiff(treeString, lastTreeString, ctx.settings.XprintDiffDel.value)
        report.echo(s"$echoHeader\n$diff\n")
        SomePrintedTree(fusedPhase.phaseName, treeString)

      case _ =>
        report.echo(s"$echoHeader\n$treeString\n")
        SomePrintedTree(fusedPhase.phaseName, treeString)
    }
  }

  def compileFromStrings(scalaSources: List[String], javaSources: List[String] = Nil): Unit = {
    def sourceFile(source: String, isJava: Boolean): SourceFile = {
      val uuid = java.util.UUID.randomUUID().toString
      val ext = if (isJava) "java" else "scala"
      val name = s"compileFromString-$uuid.$ext"
      SourceFile.virtual(name, source)
    }
    val sources =
      scalaSources.map(sourceFile(_, isJava = false)) ++
       javaSources.map(sourceFile(_, isJava = true))

    compileSources(sources)
  }

  /** Print summary of warnings and errors encountered */
  def printSummary(): Unit = {
    printMaxConstraint()
    val r = runContext.reporter
    if !r.errorsReported then
      profile.printSummary()
    r.summarizeUnreportedWarnings()
    r.printSummary()
  }

  override def reset(): Unit = {
    super[ImplicitRunInfo].reset()
    super[ConstraintRunInfo].reset()
    myCtx = null
    myUnits = Nil
    myUnitsCached = Nil
  }

  /** Produces the following contexts, from outermost to innermost
   *
   *    bootStrap:   A context with next available runId and a scope consisting of
   *                 the RootPackage _root_
   *    start        A context with RootClass as owner and the necessary initializations
   *                 for type checking.
   *    imports      For each element of RootImports, an import context
   */
  protected def rootContext(using Context): Context = {
    ctx.initialize()
    ctx.base.setPhasePlan(comp.phases)
    val rootScope = new MutableScope(0)
    val bootstrap = ctx.fresh
      .setPeriod(Period(comp.nextRunId, FirstPhaseId))
      .setScope(rootScope)
    rootScope.enter(ctx.definitions.RootPackage)(using bootstrap)
    var start = bootstrap.fresh
      .setOwner(defn.RootClass)
      .setTyper(new Typer)
      .addMode(Mode.ImplicitsEnabled)
      .setTyperState(ctx.typerState.fresh(ctx.reporter))
    if ctx.settings.YexplicitNulls.value && !Feature.enabledBySetting(nme.unsafeNulls) then
      start = start.addMode(Mode.SafeNulls)
    ctx.initialize()(using start) // re-initialize the base context with start

    // `this` must be unchecked for safe initialization because by being passed to setRun during
    // initialization, it is not yet considered fully initialized by the initialization checker
    start.setRun(this: @unchecked)
  }

  private var myCtx: Context | Null = rootContext(using ictx)

  /** The context created for this run */
  given runContext[Dummy_so_its_a_def]: Context = myCtx.nn
  assert(runContext.runId <= Periods.MaxPossibleRunId)
}

object Run {

  case class SubPhase(val name: String):
    override def toString: String = name

  class SubPhases(val phase: Phase):
    require(phase.exists)

    private def baseName: String = phase match
      case phase: MegaPhase => phase.shortPhaseName
      case phase => phase.phaseName

    val all = IArray.from(phase.subPhases.map(sub => s"$baseName[$sub]"))

    def next(using Context): Option[SubPhases] =
      val next0 = phase.megaPhase.next.megaPhase
      if next0.exists then Some(SubPhases(next0))
      else None

    def size: Int = all.size

    def subPhase(index: Int) =
      if index < all.size then all(index)
      else baseName


  private class Progress(cb: ProgressCallback, private val run: Run, val initialTraversals: Int):
    export cb.{cancel, isCancelled}

    var totalTraversals: Int = initialTraversals  // track how many phases we expect to run
    var currentUnitCount: Int = 0 // current unit count in the current (sub)phase
    var currentLateUnitCount: Int = 0 // current late unit count
    var completedTraversalCount: Int = 0 // completed traversals over all files
    var currentCompletedSubtraversalCount: Int = 0 // completed subphases in the current phase
    var seenPhaseCount: Int = 0 // how many phases we've seen so far

    private var currPhase: Phase = uninitialized      // initialized by enterPhase
    private var subPhases: SubPhases = uninitialized  // initialized by enterPhase
    private var currPhaseName: String = uninitialized // initialized by enterPhase
    private var nextPhaseName: String = uninitialized // initialized by enterPhase

    /** Enter into a new real phase, setting the current and next (sub)phases */
    def enterPhase(newPhase: Phase)(using Context): Unit =
      if newPhase ne currPhase then
        currPhase = newPhase
        subPhases = SubPhases(newPhase)
        tickSubphase()

    def canAdvanceSubPhase: Boolean =
      currentCompletedSubtraversalCount + 1 < subPhases.size

    /** Compute the current (sub)phase name and next (sub)phase name */
    def tickSubphase()(using Context): Unit =
      val index = currentCompletedSubtraversalCount
      val s = subPhases
      currPhaseName = s.subPhase(index)
      nextPhaseName =
        if index + 1 < s.all.size then s.subPhase(index + 1)
        else s.next match
          case None => "<end>"
          case Some(next0) => next0.subPhase(0)
      if seenPhaseCount > 0 then
        refreshProgress()


    /** Counts the number of completed full traversals over files, plus the number of units in the current phase */
    private def currentProgress(): Int =
      completedTraversalCount * work() + currentUnitCount + currentLateUnitCount

    /**Total progress is computed as the sum of
     * - the number of traversals we expect to make over all files
     * - the number of late compilations
     */
    private def totalProgress(): Int =
      totalTraversals * work() + run.lateFiles.size

    private def work(): Int = run.files.size

    private def requireInitialized(): Unit =
      require((currPhase: Phase | Null) != null, "enterPhase was not called")

    def checkCancellation(): Boolean =
      if Thread.interrupted() then cancel()
      isCancelled()

    /** trace that we are beginning a unit in the current (sub)phase, unless cancelled */
    def tryEnterUnit(unit: CompilationUnit): Boolean =
      if checkCancellation() then false
      else
        requireInitialized()
        cb.informUnitStarting(currPhaseName, unit)
        true

    /** trace the current progress out of the total, in the current (sub)phase, reporting the next (sub)phase */
    def refreshProgress()(using Context): Unit =
      requireInitialized()
      val total = totalProgress()
      if total > 0 && !cb.progress(currentProgress(), total, currPhaseName, nextPhaseName) then
        cancel()

  extension (run: Run | Null)

    /** record that the current phase has begun for the compilation unit of the current Context */
    def enterUnit(unit: CompilationUnit)(using Context): Boolean =
      if run != null then run.didEnterUnit(unit)
      else true // don't check cancellation if we're not tracking progress

    /** check progress cancellation, true if not cancelled */
    def enterRegion()(using Context): Boolean =
      if run != null then run.canProgress()
      else true // don't check cancellation if we're not tracking progress

    /** advance the unit count and record progress in the current phase */
    def advanceUnit()(using Context): Unit =
      if run != null then run.doAdvanceUnit()

    /** if there exists another subphase, switch to it and record progress */
    def enterNextSubphase()(using Context): Unit =
      if run != null then run.tryAdvanceSubPhase()

    /** advance the late count and record progress in the current phase */
    def advanceLate()(using Context): Unit =
      if run != null then run.doAdvanceLate()

    def enrichedErrorMessage: Boolean = if run == null then false else run.myEnrichedErrorMessage
    def enrichErrorMessage(errorMessage: String)(using Context): String =
      if run == null then
        report.enrichErrorMessage(errorMessage)
      else if !run.enrichedErrorMessage then
        run.myEnrichedErrorMessage = true
        report.enrichErrorMessage(errorMessage)
      else
        errorMessage
}
