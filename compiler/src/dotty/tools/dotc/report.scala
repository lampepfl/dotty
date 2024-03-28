package dotty.tools.dotc

import reporting.*
import Diagnostic.*
import util.{SourcePosition, NoSourcePosition, SrcPos}
import core.*
import Contexts.*, Flags.*, Symbols.*, Decorators.*
import config.SourceVersion
import ast.*
import config.Feature.sourceVersion
import java.lang.System.currentTimeMillis
import dotty.tools.dotc.config.MigrationVersion

object report:

  /** For sending messages that are printed only if -verbose is set */
  def inform(msg: => String, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    if ctx.settings.verbose.value then echo(msg, pos)

  def echo(msg: => String, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    ctx.reporter.report(new Info(msg.toMessage, pos.sourcePos))

  private def issueWarning(warning: Warning)(using Context): Unit =
    ctx.reporter.report(warning)

  def deprecationWarning(msg: Message, pos: SrcPos)(using Context): Unit =
    issueWarning(new DeprecationWarning(msg, pos.sourcePos))

  def migrationWarning(msg: Message, pos: SrcPos)(using Context): Unit =
    issueWarning(new MigrationWarning(msg, pos.sourcePos))

  def uncheckedWarning(msg: Message, pos: SrcPos)(using Context): Unit =
    issueWarning(new UncheckedWarning(msg, pos.sourcePos))

  def featureWarning(msg: Message, pos: SrcPos)(using Context): Unit =
    issueWarning(new FeatureWarning(msg, pos.sourcePos))

  def featureWarning(feature: String, featureDescription: => String,
      featureUseSite: Symbol, required: Boolean, pos: SrcPos)(using Context): Unit =
    val req = if required then "needs to" else "should"
    val fqname = s"scala.language.$feature"

    val explain =
      if ctx.reporter.isReportedFeatureUseSite(featureUseSite) then ""
      else
        ctx.reporter.reportNewFeatureUseSite(featureUseSite)
        s"""
           |See the Scala docs for value $fqname for a discussion
           |why the feature $req be explicitly enabled.""".stripMargin

    def msg = em"""$featureDescription $req be enabled
                  |by adding the import clause 'import $fqname'
                  |or by setting the compiler option -language:$feature.$explain"""
    if required then error(msg, pos)
    else issueWarning(new FeatureWarning(msg, pos.sourcePos))
  end featureWarning

  def warning(msg: Message, pos: SrcPos)(using Context): Unit =
    issueWarning(new Warning(msg, addInlineds(pos)))

  def warning(msg: Message)(using Context): Unit =
    warning(msg, NoSourcePosition)

  def warning(msg: => String, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    warning(msg.toMessage, pos)

  def error(msg: Message, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    val fullPos = addInlineds(pos)
    ctx.reporter.report(new Error(msg, fullPos))
    if ctx.settings.YdebugError.value then Thread.dumpStack()

  def error(msg: => String, pos: SrcPos)(using Context): Unit =
    error(msg.toMessage, pos)

  def error(msg: => String)(using Context): Unit =
    error(msg, NoSourcePosition)

  def error(ex: TypeError, pos: SrcPos)(using Context): Unit =
    val fullPos = addInlineds(pos)
    ctx.reporter.report(new StickyError(ex.toMessage, fullPos))
    if ctx.settings.YdebugError.value then Thread.dumpStack()
    if ctx.settings.YdebugTypeError.value then ex.printStackTrace()

  def bestEffortError(ex: Throwable, msg: String)(using Context): Unit =
    val stackTrace =
      Option(ex.getStackTrace()).map { st =>
        if st.nn.isEmpty then ""
        else s"Stack trace: \n ${st.nn.mkString("\n ")}".stripMargin
      }.getOrElse("")
    // Build tools and dotty's test framework may check precisely for
    // "Unsuccessful best-effort compilation." error text.
    val fullMsg =
      em"""Unsuccessful best-effort compilation.
          |${msg}
          |Cause:
          | ${ex.toString.replace("\n", "\n ")}
          |${stackTrace}"""
    ctx.reporter.report(new Error(fullMsg, NoSourcePosition))

  def errorOrMigrationWarning(msg: Message, pos: SrcPos, migrationVersion: MigrationVersion)(using Context): Unit =
    if sourceVersion.isAtLeast(migrationVersion.errorFrom) then
      if !sourceVersion.isMigrating then error(msg, pos)
      else if ctx.settings.rewrite.value.isEmpty then migrationWarning(msg, pos)
    else if sourceVersion.isAtLeast(migrationVersion.warnFrom) then warning(msg, pos)

  def restrictionError(msg: Message, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    error(msg.mapMsg("Implementation restriction: " + _), pos)

  def incompleteInputError(msg: Message, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    ctx.reporter.incomplete(new Error(msg, pos.sourcePos))

  /** Log msg if settings.log contains the current phase.
   *  See [[config.CompilerCommand#explainAdvanced]] for the exact meaning of
   *  "contains" here.
   */
  def log(msg: => String, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    if (ctx.settings.Ylog.value.containsPhase(ctx.phase))
      echo(s"[log ${ctx.phase}] $msg", pos)

  def debuglog(msg: => String)(using Context): Unit =
    if (ctx.debug) log(msg)

  def informTime(msg: => String, start: Long)(using Context): Unit = {
    def elapsed = s" in ${currentTimeMillis - start}ms"
    informProgress(msg + elapsed)
  }

  def informProgress(msg: => String)(using Context): Unit =
    inform("[" + msg + "]")

  def logWith[T](msg: => String)(value: T)(using Context): T = {
    log(msg + " " + value)
    value
  }

  def debugwarn(msg: => String, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    if (ctx.settings.Ydebug.value) warning(msg, pos)

  private def addInlineds(pos: SrcPos)(using Context): SourcePosition =
    def recur(pos: SourcePosition, inlineds: List[Trees.Tree[?]]): SourcePosition = inlineds match
      case inlined :: inlineds1 => pos.withOuter(recur(inlined.sourcePos, inlineds1))
      case Nil => pos
    recur(pos.sourcePos, tpd.enclosingInlineds)

  private object messageRendering extends MessageRendering

  // Should only be called from Run#enrichErrorMessage.
  def enrichErrorMessage(errorMessage: String)(using Context): String =
    if ctx.settings.YnoEnrichErrorMessages.value then errorMessage
    else try enrichErrorMessage1(errorMessage)
    catch case _: Throwable => errorMessage // don't introduce new errors trying to report errors, so swallow exceptions

  private def enrichErrorMessage1(errorMessage: String)(using Context): String = {
    import untpd.*, config.Settings.*
    def formatExplain(pairs: List[(String, Any)]) = pairs.map((k, v) => f"$k%20s: $v").mkString("\n")

    val settings = ctx.settings.userSetSettings(ctx.settingsState).sortBy(_.name)
    def showSetting(s: Setting[?]): String = if s.value == "" then s"${s.name} \"\"" else s"${s.name} ${s.value}"

    val info1 = formatExplain(List(
      "while compiling"    -> ctx.compilationUnit,
      "during phase"       -> ctx.phase.megaPhase,
      "mode"               -> ctx.mode,
      "library version"    -> scala.util.Properties.versionString,
      "compiler version"   -> dotty.tools.dotc.config.Properties.versionString,
      "settings"           -> settings.map(showSetting).mkString(" "),
    ))
    s"""
       |  $errorMessage
       |
       |  An unhandled exception was thrown in the compiler.
       |  Please file a crash report here:
       |  https://github.com/scala/scala3/issues/new/choose
       |  For non-enriched exceptions, compile with -Yno-enrich-error-messages.
       |
       |$info1
       |""".stripMargin
  }
end report
