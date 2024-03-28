package dotty.tools
package dotc
package reporting

import scala.language.unsafeNulls

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition}
import dotty.tools.dotc.interfaces.SourceFile
import dotty.tools.dotc.reporting.MessageFilter.SourcePattern

import java.util.regex.PatternSyntaxException
import scala.annotation.internal.sharable
import scala.util.matching.Regex

enum MessageFilter:
  def matches(message: Diagnostic): Boolean = this match
    case Any => true
    case Deprecated => message.isInstanceOf[Diagnostic.DeprecationWarning]
    case Feature => message.isInstanceOf[Diagnostic.FeatureWarning]
    case Unchecked => message.isInstanceOf[Diagnostic.UncheckedWarning]
    case MessagePattern(pattern) =>
      val noHighlight = message.msg.message.replaceAll("\\e\\[[\\d;]*[^\\d;]","")
      pattern.findFirstIn(noHighlight).nonEmpty
    case MessageID(errorId) => message.msg.errorId == errorId
    case SourcePattern(pattern) =>
      val source = message.position.orElse(NoSourcePosition).source()
      val path = source.jfile()
        .map(_.toPath.toAbsolutePath.toUri.normalize().getRawPath)
        .orElse(source.path())
      pattern.findFirstIn(path).nonEmpty

    case None => false

  case Any, Deprecated, Feature, Unchecked, None
  case MessagePattern(pattern: Regex)
  case MessageID(errorId: ErrorMessageID)
  case SourcePattern(pattern: Regex)

enum Action:
  case Error, Warning, Verbose, Info, Silent

final case class WConf(confs: List[(List[MessageFilter], Action)]):
  def action(message: Diagnostic): Action = confs.collectFirst {
    case (filters, action) if filters.forall(_.matches(message)) => action
  }.getOrElse(Action.Warning)

object WConf:
  import Action.*
  import MessageFilter.*

  private type Conf = (List[MessageFilter], Action)

  def parseAction(s: String): Either[List[String], Action] = s match
    case "error" | "e"            => Right(Error)
    case "warning" | "w"          => Right(Warning)
    case "verbose" | "v"          => Right(Verbose)
    case "info" | "i"             => Right(Info)
    case "silent" | "s"           => Right(Silent)
    case _                        => Left(List(s"unknown action: `$s`"))

  private def regex(s: String) =
    try Right(s.r)
    catch case e: PatternSyntaxException => Left(s"invalid pattern `$s`: ${e.getMessage}")

  @sharable val Splitter = raw"([^=]+)=(.+)".r
  @sharable val ErrorId = raw"E?(\d+)".r

  def parseFilters(s: String): Either[List[String], List[MessageFilter]] =
    // TODO: don't split on escaped \&
    val (parseErrors, filters) = s.split('&').toList.partitionMap(parseFilter)
    if parseErrors.nonEmpty then Left(parseErrors)
    else if filters.isEmpty then Left(List("no filters or no action defined"))
    else Right(filters)

  def parseFilter(s: String): Either[String, MessageFilter] = s match
    case "any" => Right(Any)
    case Splitter(filter, conf) => filter match
      case "msg" => regex(conf).map(MessagePattern.apply)
      case "id" => conf match
        case ErrorId(num) =>
          ErrorMessageID.fromErrorNumber(num.toInt) match
            case Some(errId) if errId.isActive => Right(MessageID(errId))
            case Some(errId) => Left(s"E${num} is marked as inactive.")
            case _ => Left(s"Unknown error message number: E${num}")
        case _ =>
          Left(s"invalid error message id: $conf")
      case "name" =>
        try Right(MessageID(ErrorMessageID.valueOf(conf + "ID")))
        catch case _: IllegalArgumentException => Left(s"unknown error message name: $conf")

      case "cat" => conf match
        case "deprecation" => Right(Deprecated)
        case "feature"     => Right(Feature)
        case "unchecked"   => Right(Unchecked)
        case _             => Left(s"unknown category: $conf")

      case "src" => regex(conf).map(SourcePattern.apply)

      case _ => Left(s"unknown filter: $filter")
    case _ => Left(s"unknown filter: $s")

  def parsed(using Context): WConf =
    val setting = ctx.settings.Wconf.value
    def cached = ctx.base.wConfCache
    if cached == null || cached._1 != setting then
      val conf = fromSettings(setting)
      ctx.base.wConfCache = (setting, conf.getOrElse(WConf(Nil)))
      conf.swap.foreach(msgs =>
        val multiHelp =
          if setting.sizeIs > 1 then
            """
              |Note: for multiple filters, use `-Wconf:filter1:action1,filter2:action2`
              |      or alternatively          `-Wconf:filter1:action1 -Wconf:filter2:action2`""".stripMargin
          else ""
        report.warning(s"Failed to parse `-Wconf` configuration: ${ctx.settings.Wconf.value.mkString(",")}\n${msgs.mkString("\n")}$multiHelp"))
    cached._2

  def fromSettings(settings: List[String]): Either[List[String], WConf] =
    if (settings.isEmpty) Right(WConf(Nil))
    else
      val parsedConfs: List[Either[List[String], (List[MessageFilter], Action)]] = settings.reverse.map(conf =>
        val filtersAndAction = conf.split(':')
        if filtersAndAction.length != 2 then Left(List("exactly one `:` expected (<filter>&...&<filter>:<action>)"))
        else
          parseFilters(filtersAndAction(0)).flatMap(filters =>
            parseAction(filtersAndAction(1)).map((filters, _))))
      val (parseErrorss, configs) = parsedConfs.partitionMap(identity)
      if (parseErrorss.nonEmpty) Left(parseErrorss.flatten)
      else Right(WConf(configs))

class Suppression(val annotPos: SourcePosition, filters: List[MessageFilter], val start: Int, end: Int, val verbose: Boolean):
  private var _used = false
  def used: Boolean = _used
  def markUsed(): Unit = { _used = true }

  def matches(dia: Diagnostic): Boolean =
    val pos = dia.pos
    pos.exists && start <= pos.start && pos.end <= end && filters.forall(_.matches(dia))
