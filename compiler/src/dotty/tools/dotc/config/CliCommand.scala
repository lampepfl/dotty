package dotty.tools.dotc
package config

import scala.language.unsafeNulls

import dotty.tools.dotc.reporting.{ConsoleReporter}
import java.io.{ BufferedReader, PrintWriter }
import Settings._
import core.Contexts._
import printing.Highlighting

import scala.util.chaining.given
import scala.PartialFunction.cond

trait CliCommand:

  type ConcreteSettings <: CommonScalaSettings with Settings.SettingGroup

  def versionMsg: String

  def ifErrorsMsg: String

  /** The name of the command */
  def cmdName: String

  def isHelpFlag(using settings: ConcreteSettings)(using SettingsState): Boolean

  def helpMsg(using settings: ConcreteSettings)(using SettingsState, Context): String

  private def explainAdvanced = """
    |-- Notes on option parsing --
    |Boolean settings are always false unless set.
    |Where multiple values are accepted, they should be comma-separated.
    |  example: -Xplugin:plugin1,plugin2
    |<phases> means one or a comma-separated list of:
    |  - (partial) phase names with an optional "+" suffix to include the next phase
    |  - the string "all"
    |  example: -Xprint:all prints all phases.
    |  example: -Xprint:typer,mixin prints the typer and mixin phases.
    |  example: -Ylog:erasure+ logs the erasure phase and the phase after the erasure phase.
    |           This is useful because during the tree transform of phase X, we often
    |           already are in phase X + 1.
  """

  /** Distill arguments into summary detailing settings, errors and files to main */
  def distill(args: Array[String], sg: Settings.SettingGroup)(ss: SettingsState = sg.defaultState)(using Context): ArgsSummary =

    // expand out @filename to the contents of that filename
    def expandedArguments = args.toList flatMap {
      case x if x startsWith "@"  => CommandLineParser.expandArg(x)
      case x                      => List(x)
    }

    sg.processArguments(expandedArguments, processAll = true, settingsState = ss)
  end distill

  /** Creates a help message for a subset of options based on cond */
  protected def availableOptionsMsg(p: Setting[?] => Boolean)(using settings: ConcreteSettings)(using SettingsState): String =
    // result is (Option Name, descrption\ndefault: value\nchoices: x, y, z
    def help(s: Setting[?]): (String, String) =
      // For now, skip the default values that do not make sense for the end user, such as 'false' for the version command.
      def defaultValue = s.default match
        case _: Int | _: String => s.default.toString
        case _ => ""
      val info = List(shortHelp(s), if defaultValue.nonEmpty then s"Default $defaultValue" else "", if s.legalChoices.nonEmpty then s"Choices : ${s.legalChoices}" else "")
      (s.name, info.filter(_.nonEmpty).mkString("\n"))
    end help

    val ss = settings.allSettings.filter(p).toList.sortBy(_.name)
    val formatter = Columnator("", "", maxField = 30)
    val fresh = ContextBase().initialCtx.fresh.setSettings(summon[SettingsState])
    formatter(List(ss.map(help) :+ ("@<file>", "A text file containing compiler arguments (options and source files).")))(using fresh)
  end availableOptionsMsg

  protected def shortUsage: String = s"Usage: $cmdName <options> <source files>"

  protected def createUsageMsg(label: String, shouldExplain: Boolean, cond: Setting[?] => Boolean)(using settings: ConcreteSettings)(using SettingsState): String =
    val prefix = List(
      Some(shortUsage),
      Some(explainAdvanced).filter(_ => shouldExplain),
      Some(label + " options include:")
    ).flatten.mkString("\n")

    prefix + "\n" + availableOptionsMsg(cond)

  protected def isStandard(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    !isVerbose(s) && !isWarning(s) && !isAdvanced(s) && !isPrivate(s) || s.name == "-Werror" || s.name == "-Wconf"
  protected def isVerbose(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    s.name.startsWith("-V") && s.name != "-V"
  protected def isWarning(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    s.name.startsWith("-W") && s.name != "-W" || s.name == "-Xlint"
  protected def isAdvanced(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    s.name.startsWith("-X") && s.name != "-X"
  protected def isPrivate(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    s.name.startsWith("-Y") && s.name != "-Y"
  protected def shortHelp(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): String =
    s.description.linesIterator.next()
  protected def isHelping(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    cond(s.value) {
      case ss: List[?] if s.isMultivalue => ss.contains("help")
      case s: String                     => "help" == s
    }

  /** Messages explaining usage and options */
  protected def usageMessage(using settings: ConcreteSettings)(using SettingsState) =
    createUsageMsg("where possible standard", shouldExplain = false, isStandard)
  protected def vusageMessage(using settings: ConcreteSettings)(using SettingsState) =
    createUsageMsg("Possible verbose", shouldExplain = true, isVerbose)
  protected def wusageMessage(using settings: ConcreteSettings)(using SettingsState) =
    createUsageMsg("Possible warning", shouldExplain = true, isWarning)
  protected def xusageMessage(using settings: ConcreteSettings)(using SettingsState) =
    createUsageMsg("Possible advanced", shouldExplain = true, isAdvanced)
  protected def yusageMessage(using settings: ConcreteSettings)(using SettingsState) =
    createUsageMsg("Possible private", shouldExplain = true, isPrivate)

  /** Used for the formatted output of -Xshow-phases */
  protected def phasesMessage(using Context): String =
    val phases = new Compiler().phases
    val formatter = Columnator("phase name", "description", maxField = 25)
    formatter(phases.map(mega => mega.map(p => (p.phaseName, p.description))))

  /** Provide usage feedback on argument summary, assuming that all settings
   *  are already applied in context.
   *  @return  Either Some list of files passed as arguments or None if further processing should be interrupted.
   */
  def checkUsage(summary: ArgsSummary, sourcesRequired: Boolean)(using settings: ConcreteSettings)(using SettingsState, Context): Option[List[String]] =
    // Print all warnings encountered during arguments parsing
    summary.warnings.foreach(report.warning(_))

    if summary.errors.nonEmpty then
      summary.errors foreach (report.error(_))
      report.echo(ifErrorsMsg)
      None
    else if settings.version.value then
      // ctx defaults to Console.err
      ConsoleReporter(writer = new PrintWriter(Console.out, true)).printMessage(versionMsg)
      None
    else if isHelpFlag then
      report.echo(helpMsg)
      None
    else if (sourcesRequired && summary.arguments.isEmpty)
      report.echo(usageMessage)
      None
    else
      Some(summary.arguments)

  extension [T](setting: Setting[T])
    protected def value(using ss: SettingsState): T = setting.valueIn(ss)

  extension (s: String)
    def padLeft(width: Int): String = String.format(s"%${width}s", s)

  // Formatting for -help and -Vphases in two columns, handling long field1 and wrapping long field2
  class Columnator(heading1: String, heading2: String, maxField: Int, separation: Int = 2):
    def apply(texts: List[List[(String, String)]])(using Context): String = StringBuilder().tap(columnate(_, texts)).toString

    private def columnate(sb: StringBuilder, texts: List[List[(String, String)]])(using Context): Unit =
      import Highlighting.*
      val colors = Seq(Green(_), Yellow(_), Magenta(_), Cyan(_), Red(_))
      val nocolor = texts.length == 1
      def color(index: Int): String => Highlight = if nocolor then NoColor(_) else colors(index % colors.length)
      val maxCol = ctx.settings.pageWidth.value
      val field1 = maxField.min(texts.flatten.map(_._1.length).filter(_ < maxField).max) // widest field under maxField
      val field2 = if field1 + separation + maxField < maxCol then maxCol - field1 - separation else 0 // skinny window -> terminal wrap
      val separator = " " * separation
      val EOL = "\n"
      def formatField1(text: String): String = if text.length <= field1 then text.padLeft(field1) else text + EOL + "".padLeft(field1)
      def formatField2(text: String): String =
        def loopOverField2(fld: String): List[String] =
          if field2 == 0 || fld.length <= field2 then List(fld)
          else
            fld.lastIndexOf(" ", field2) match
              case -1 => List(fld)
              case i  => val (prefix, rest) = fld.splitAt(i) ; prefix :: loopOverField2(rest.trim)
        text.split("\n").toList.flatMap(loopOverField2).filter(_.nonEmpty).mkString(EOL + "".padLeft(field1) + separator)
      end formatField2
      def format(first: String, second: String, index: Int, colorPicker: Int => String => Highlight) =
        sb.append(colorPicker(index)(formatField1(first)).show)
          .append(separator)
          .append(formatField2(second))
          .append(EOL): Unit
      def fancy(first: String, second: String, index: Int) = format(first, second, index, color)
      def plain(first: String, second: String) = format(first, second, 0, _ => NoColor(_))

      if heading1.nonEmpty then
        plain(heading1, heading2)
        plain("-" * heading1.length, "-" * heading2.length)

      def emit(index: Int)(textPair: (String, String)): Unit = fancy(textPair._1, textPair._2, index)
      def group(index: Int)(body: Int => Unit): Unit =
        if !ctx.useColors then plain(s"{", "")
        body(index)
        if !ctx.useColors then plain(s"}", "")

      texts.zipWithIndex.foreach { (text, index) =>
        text match
          case List(single) => emit(index)(single)
          case Nil          =>
          case mega         => group(index)(i => mega.foreach(emit(i)))
      }
  end Columnator
