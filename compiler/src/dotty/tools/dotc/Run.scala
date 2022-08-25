package dotty.tools
package dotc

import core._
import Contexts._
import Periods._
import Symbols._
import Scopes._
import Names.Name
import Denotations.Denotation
import typer.Typer
import typer.ImportInfo.withRootImports
import Decorators._
import io.AbstractFile
import Phases.unfusedPhases

import util._
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
  private var myFiles: Set[AbstractFile] = _

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
  private var lateFiles = mutable.Set[AbstractFile]()

  /** A cache for static references to packages and classes */
  val staticRefs = util.EqHashMap[Name, Denotation](initialCapacity = 1024)

  /** Actions that need to be performed at the end of the current compilation run */
  private var finalizeActions = mutable.ListBuffer[() => Unit]()

  def compile(files: List[AbstractFile]): Unit =
    try
      val sources = files.map(runContext.getSource(_))
      compileSources(sources)
    catch
      case NonFatal(ex) =>
        if units.nonEmpty then report.echo(i"exception occurred while compiling $units%, %")
        else report.echo(s"exception occurred while compiling ${files.map(_.name).mkString(", ")}")
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

    // If testing pickler, make sure to stop after pickling phase:
    val stopAfter =
      if (ctx.settings.YtestPickler.value) List("pickler")
      else ctx.settings.YstopAfter.value

    val pluginPlan = ctx.base.addPluginPhases(ctx.base.phasePlan)
    val phases = ctx.base.fusePhases(pluginPlan,
      ctx.settings.Yskip.value, ctx.settings.YstopBefore.value, stopAfter, ctx.settings.Ycheck.value)
    ctx.base.usePhases(phases)

    def runPhases(using Context) = {
      var lastPrintedTree: PrintedTree = NoPrintedTree
      val profiler = ctx.profiler

      for (phase <- ctx.base.allPhases)
        if (phase.isRunnable)
          Stats.trackTime(s"$phase ms ") {
            val start = System.currentTimeMillis
            val profileBefore = profiler.beforePhase(phase)
            units = phase.runOn(units)
            profiler.afterPhase(phase, profileBefore)
            if (ctx.settings.Xprint.value.containsPhase(phase))
              for (unit <- units)
                lastPrintedTree =
                  printTree(lastPrintedTree)(using ctx.fresh.setPhase(phase.next).setCompilationUnit(unit))
            report.informTime(s"$phase ", start)
            Stats.record(s"total trees at end of $phase", ast.Trees.ntrees)
            for (unit <- units)
              Stats.record(s"retained typed trees at end of $phase", unit.tpdTree.treeSize)
            ctx.typerState.gc()
          }

      profiler.finished()
    }

    val runCtx = ctx.fresh
    runCtx.setProfiler(Profiler())
    unfusedPhases.foreach(_.initContext(runCtx))
    runPhases(using runCtx)
    if (!ctx.reporter.hasErrors)
      Rewrites.writeBack()
    suppressions.runFinished(hasErrors = ctx.reporter.hasErrors)
    while (finalizeActions.nonEmpty) {
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
        ctx.typer.lateEnterUnit(doTypeCheck =>
          if typeCheck then
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
    val fusedPhase = ctx.phase.prevMega
    val echoHeader = f"[[syntax trees at end of $fusedPhase%25s]] // ${unit.source}"
    val tree = if ctx.isAfterTyper then unit.tpdTree else unit.untpdTree
    val treeString = tree.show(using ctx.withProperty(XprintMode, Some(())))

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
    if ctx.settings.YexplicitNulls.value then
      if !Feature.enabledBySetting(nme.unsafeNulls) then
        start = start.addMode(Mode.SafeNulls)
      if Feature.enabledBySetting(Feature.unsafeJavaReturn) then
        start = start.addMode(Mode.UnsafeJavaReturn)
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
