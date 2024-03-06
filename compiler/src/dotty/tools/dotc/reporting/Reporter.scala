package dotty.tools
package dotc
package reporting

import scala.language.unsafeNulls

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}
import dotty.tools.dotc.reporting.Diagnostic._
import dotty.tools.dotc.reporting.Message._
import dotty.tools.dotc.util.NoSourcePosition

import java.io.{BufferedReader, PrintWriter}
import scala.annotation.internal.sharable
import scala.collection.mutable
import core.Decorators.em

object Reporter {
  /** Convert a SimpleReporter into a real Reporter */
  def fromSimpleReporter(simple: interfaces.SimpleReporter): Reporter =
    new Reporter with UniqueMessagePositions with HideNonSensicalMessages {
      override def doReport(dia: Diagnostic)(using Context): Unit = simple.report(dia)
    }

  /** A reporter that ignores reports, and doesn't record errors */
  @sharable object NoReporter extends Reporter {
    def doReport(dia: Diagnostic)(using Context): Unit = ()
    override def report(dia: Diagnostic)(using Context): Unit = ()
  }

  type ErrorHandler = (Diagnostic, Context) => Unit

  private val defaultIncompleteHandler: ErrorHandler =
    (mc, ctx) => ctx.reporter.report(mc)(using ctx)

  /** Show prompt if `-Xprompt` is passed as a flag to the compiler */
  def displayPrompt(reader: BufferedReader, writer: PrintWriter): Unit = {
    writer.println()
    writer.print("a)bort, s)tack, r)esume: ")
    writer.flush()
    if (reader != null) {
      def loop(): Unit = reader.read match {
        case 'a' | 'A' =>
          new Throwable().printStackTrace(writer)
          System.exit(1)
        case 's' | 'S' =>
          new Throwable().printStackTrace(writer)
          writer.println()
          writer.flush()
        case 'r' | 'R' =>
          ()
        case _ =>
          loop()
      }
      loop()
    }
  }
}

/**
 * This interface provides methods to issue information, warning and
 * error messages.
 */
abstract class Reporter extends interfaces.ReporterResult {
  import Reporter._

  /** Report a diagnostic */
  def doReport(dia: Diagnostic)(using Context): Unit

  /** Whether very long lines can be truncated.  This exists so important
   *  debugging information (like printing the classpath) is not rendered
   *  invisible due to the max message length.
   */
  private var _truncationOK: Boolean = true
  def truncationOK: Boolean = _truncationOK
  def withoutTruncating[T](body: => T): T = {
    val saved = _truncationOK
    _truncationOK = false
    try body
    finally _truncationOK = saved
  }

  private var incompleteHandler: ErrorHandler = defaultIncompleteHandler

  def withIncompleteHandler[T](handler: ErrorHandler)(op: => T): T = {
    val saved = incompleteHandler
    incompleteHandler = handler
    try op
    finally incompleteHandler = saved
  }

  private def isIncompleteChecking = incompleteHandler ne defaultIncompleteHandler

  private var _errorCount = 0
  private var _warningCount = 0

  /** The number of errors reported by this reporter (ignoring outer reporters) */
  def errorCount: Int = _errorCount

  /** The number of warnings reported by this reporter (ignoring outer reporters) */
  def warningCount: Int = _warningCount

  /** Have errors been reported by this reporter (ignoring outer reporters)? */
  def hasErrors: Boolean = errorCount > 0

  /** Have warnings been reported by this reporter (ignoring outer reporters)? */
  def hasWarnings: Boolean = warningCount > 0

  private var errors: List[Error] = Nil

  /** All errors reported by this reporter (ignoring outer reporters) */
  def allErrors: List[Error] = errors

  /** Were sticky errors reported? Overridden in StoreReporter. */
  def hasStickyErrors: Boolean = false

  /** Have errors been reported by this reporter, or in the
   *  case where this is a StoreReporter, by an outer reporter?
   */
  def errorsReported: Boolean = hasErrors

  /** Run `op` and return `true` if errors were reported by this reporter.
   */
  def reportsErrorsFor(op: Context ?=> Unit)(using Context): Boolean = {
    val initial = errorCount
    op
    errorCount > initial
  }

  private var reportedFeaturesUseSites = Set[Symbol]()

  def isReportedFeatureUseSite(featureTrait: Symbol): Boolean =
    featureTrait.ne(NoSymbol) && reportedFeaturesUseSites.contains(featureTrait)

  def reportNewFeatureUseSite(featureTrait: Symbol): Unit = reportedFeaturesUseSites += featureTrait

  var unreportedWarnings: Map[String, Int] = Map.empty

  def addUnreported(key: String, n: Int): Unit =
    val count = unreportedWarnings.getOrElse(key, 0)
    unreportedWarnings = unreportedWarnings.updated(key, count + n)

  /** Issue the diagnostic, ignoring `-Wconf` and `@nowarn` configurations,
   *  but still honouring `-nowarn`, `-Werror`, and conditional warnings. */
  def issueUnconfigured(dia: Diagnostic)(using Context): Unit = dia match
    case w: Warning if ctx.settings.silentWarnings.value    =>
    case w: ConditionalWarning if w.isSummarizedConditional =>
      val key = w.enablingOption.name
      addUnreported(key, 1)
    case _                                                  =>
      // conditional warnings that are not enabled are not fatal
      val d = dia match
        case w: Warning if ctx.settings.XfatalWarnings.value => w.toError
        case _                                               => dia
      if !isHidden(d) then // avoid isHidden test for summarized warnings so that message is not forced
        d match {
          case _: Warning => _warningCount += 1
          case e: Error   =>
            errors = e :: errors
            _errorCount += 1
            if ctx.typerState.isGlobalCommittable then
              ctx.base.errorsToBeReported = true
          case _: Info    => // nothing to do here
          // match error if d is something else
        }
        markReported(dia)
        withMode(Mode.Printing)(doReport(dia))
  end issueUnconfigured

  def issueIfNotSuppressed(dia: Diagnostic)(using Context): Unit =
    def go() =
      import Action._
      dia match
        case w: Warning => WConf.parsed.action(w) match
          case Error   => issueUnconfigured(w.toError)
          case Warning => issueUnconfigured(w)
          case Verbose => issueUnconfigured(w.setVerbose())
          case Info    => issueUnconfigured(w.toInfo)
          case Silent  =>
        case _ => issueUnconfigured(dia)

    // `ctx.run` can be null in test, also in the repl when parsing the first line. The parser runs early, the Run is
    // only created in ReplDriver.compile when a line is submitted. This means that `@nowarn` doesnt work on parser
    // warnings in the first line.
    val run = ctx.run
    dia match
      case w: Warning if run != null =>
        val sup = run.suppressions
        if sup.suppressionsComplete(w.pos.source) then sup.nowarnAction(w) match
          case Action.Warning => go()
          case Action.Verbose => w.setVerbose(); go()
          case Action.Silent =>
        else
          // ParseResult.isIncomplete creates a new source file and reporter to check if the input is complete.
          // The reporter's warnings are discarded, and we should not add them to the run's suspended messages,
          // otherwise they are later reported.
          if !isIncompleteChecking then
            sup.addSuspendedMessage(w)
      case _ => go()
  end issueIfNotSuppressed

  def report(dia: Diagnostic)(using Context): Unit = issueIfNotSuppressed(dia)

  def incomplete(dia: Diagnostic)(using Context): Unit =
    incompleteHandler(dia, ctx)

  /** Summary of warnings and errors */
  def summary: String = {
    val b = new mutable.ListBuffer[String]
    if (warningCount > 0)
      b += countString(warningCount, "warning") + " found"
    if (errorCount > 0)
      b += countString(errorCount, "error") + " found"
    b.mkString("\n")
  }

  def summarizeUnreportedWarnings()(using Context): Unit =
    for (settingName, count) <- unreportedWarnings do
      val were = if count == 1 then "was" else "were"
      val msg = em"there $were ${countString(count, settingName.tail + " warning")}; re-run with $settingName for details"
      report(Warning(msg, NoSourcePosition))

  /** Print the summary of warnings and errors */
  def printSummary()(using Context): Unit = {
    val s = summary
    if (s != "") report(new Info(s, NoSourcePosition))
  }

  /** Returns a string meaning "n elements". */
  protected def countString(n: Int, elements: String): String = n match {
    case 0 => s"no ${elements}s"
    case 1 => s"1 ${elements}"
    case _ => s"$n ${elements}s"
  }

  /** Should this diagnostic not be reported at all? */
  def isHidden(dia: Diagnostic)(using Context): Boolean =
    ctx.mode.is(Mode.Printing)

  def markReported(dia: Diagnostic)(using Context): Unit = ()

  /** Does this reporter contain errors that have yet to be reported by its outer reporter ?
   *  Note: this is always false when there is no outer reporter.
   */
  def hasUnreportedErrors: Boolean = false

  /** Does this reporter contain any message that have yet to be reported by its outer reporter ?
   *  This includes any warning stored in `unreportedWarnings` which need to be propagated to
   *  get an accurate count of unreported warnings in the outer reporter.
   */
  def hasUnreportedMessages(using Context): Boolean =
    pendingMessages.nonEmpty || unreportedWarnings.nonEmpty

  /** If this reporter buffers messages, remove and return all buffered messages. */
  def removeBufferedMessages(using Context): List[Diagnostic] = Nil

  /** Issue all messages in this reporter to next outer one, or make sure they are written. */
  def flush()(using Context): Unit =
    val msgs = removeBufferedMessages
    if msgs.nonEmpty then msgs.foreach(ctx.reporter.report)
    for (key, count) <- unreportedWarnings do
      ctx.reporter.addUnreported(key, count)
    unreportedWarnings = Map.empty

  /** If this reporter buffers messages, all buffered messages, otherwise Nil */
  def pendingMessages(using Context): List[Diagnostic] = Nil
}
