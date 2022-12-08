package dotty.tools
package dotc
package reporting

import core.*
import Contexts.*, Decorators.*, Symbols.*, Types.*, Flags.*
import printing.{RefinedPrinter, MessageLimiter, ErrorMessageLimiter}
import printing.Texts.Text
import printing.Formatting.hl
import config.SourceVersion

import scala.language.unsafeNulls

import scala.annotation.threadUnsafe

/** ## Tips for error message generation
 *
 *  - You can use the `em` interpolator for error messages. It's defined in core.Decorators.
 *  - You can also use a simple string argument for `error` or `warning` (not for the other variants),
 *    but the string should not be interpolated or composed of objects that require a
 *    Context for evaluation.
 *  - When embedding interpolated substrings defined elsewhere in error messages,
 *    use `i` and make sure they are defined as def's instead of vals. That way, the
 *    possibly expensive interpolation will performed only in the case where the message
 *    is eventually printed. Note: At least during typer, it's common for messages
 *    to be discarded without being printed. Also, by making them defs, you ensure that
 *    they will be evaluated in the Message context, which makes formatting safer
 *    and more robust.
 *  - For common messages, or messages that might require explanation, prefer defining
 *    a new `Message` class in file `messages.scala` and use that instead. The advantage is that these
 *    messages have unique IDs that can be referenced elsewhere.
 */
object Message:
  def rewriteNotice(what: String, version: SourceVersion | Null = null, options: String = "")(using Context): String =
    if !ctx.mode.is(Mode.Interactive) then
      val sourceStr = if version != null then i"-source $version" else ""
      val optionStr =
        if options.isEmpty then sourceStr
        else if sourceStr.isEmpty then options
        else i"$sourceStr $options"
      i"\n$what can be rewritten automatically under -rewrite $optionStr."
    else ""

  private type Recorded = Symbol | ParamRef | SkolemType

  private case class SeenKey(str: String, isType: Boolean)

  /** A class that records printed items of one of the types in `Recorded`,
   *  adds superscripts for disambiguations, and can explain recorded symbols
   *  in ` where` clause
   */
  private class Seen(disambiguate: Boolean):

    private val seen = new collection.mutable.HashMap[SeenKey, List[Recorded]]:
      override def default(key: SeenKey) = Nil

    var nonSensical = false

    /** If false, stop all recordings */
    private var recordOK = disambiguate

    /** Clear all entries and stop further entries to be added */
    def disable() =
      seen.clear()
      recordOK = false

    /** Record an entry `entry` with given String representation `str` and a
     *  type/term namespace identified by `isType`.
     *  If the entry was not yet recorded, allocate the next superscript corresponding
     *  to the same string in the same name space. The first recording is the string proper
     *  and following recordings get consecutive superscripts starting with 2.
     *  @return  The possibly superscripted version of `str`.
     */
    def record(str: String, isType: Boolean, entry: Recorded)(using Context): String =
      if !recordOK then return str
      //println(s"recording $str, $isType, $entry")

      /** If `e1` is an alias of another class of the same name, return the other
       *  class symbol instead. This normalization avoids recording e.g. scala.List
       *  and scala.collection.immutable.List as two different types
       */
      def followAlias(e1: Recorded): Recorded = e1 match {
        case e1: Symbol if e1.isAliasType =>
          val underlying = e1.typeRef.underlyingClassRef(refinementOK = false).typeSymbol
          if (underlying.name == e1.name) underlying else e1
        case _ => e1
      }
      val key = SeenKey(str, isType)
      val existing = seen(key)
      lazy val dealiased = followAlias(entry)

      // alts: The alternatives in `existing` that are equal, or follow (an alias of) `entry`
      var alts = existing.dropWhile(alt => dealiased ne followAlias(alt))
      if alts.isEmpty then
        alts = entry :: existing
        seen(key) = alts

      val suffix = alts.length match {
        case 1 => ""
        case n => n.toString.toCharArray.map {
          case '0' => '⁰'
          case '1' => '¹'
          case '2' => '²'
          case '3' => '³'
          case '4' => '⁴'
          case '5' => '⁵'
          case '6' => '⁶'
          case '7' => '⁷'
          case '8' => '⁸'
          case '9' => '⁹'
        }.mkString
      }
      str + suffix
    end record

    /** Create explanation for single `Recorded` type or symbol */
    private def explanation(entry: AnyRef)(using Context): String =
      def boundStr(bound: Type, default: ClassSymbol, cmp: String) =
        if (bound.isRef(default)) "" else i"$cmp $bound"

      def boundsStr(bounds: TypeBounds): String = {
        val lo = boundStr(bounds.lo, defn.NothingClass, ">:")
        val hi = boundStr(bounds.hi, defn.AnyClass, "<:")
        if (lo.isEmpty) hi
        else if (hi.isEmpty) lo
        else s"$lo and $hi"
      }

      def addendum(cat: String, info: Type): String = info match {
        case bounds @ TypeBounds(lo, hi) if bounds ne TypeBounds.empty =>
          if (lo eq hi) i" which is an alias of $lo"
          else i" with $cat ${boundsStr(bounds)}"
        case _ =>
          ""
      }

      entry match {
        case param: TypeParamRef =>
          s"is a type variable${addendum("constraint", TypeComparer.bounds(param))}"
        case param: TermParamRef =>
          s"is a reference to a value parameter"
        case sym: Symbol =>
          val info =
            if (ctx.gadt.contains(sym))
              sym.info & ctx.gadt.fullBounds(sym)
            else
              sym.info
          s"is a ${ctx.printer.kindString(sym)}${sym.showExtendedLocation}${addendum("bounds", info)}"
        case tp: SkolemType =>
          s"is an unknown value of type ${tp.widen.show}"
      }
    end explanation

    /** Produce a where clause with explanations for recorded iterms.
     */
    def explanations(using Context): String =
      def needsExplanation(entry: Recorded) = entry match {
        case param: TypeParamRef => ctx.typerState.constraint.contains(param)
        case param: ParamRef     => false
        case skolem: SkolemType => true
        case sym: Symbol =>
          ctx.gadt.contains(sym) && ctx.gadt.fullBounds(sym) != TypeBounds.empty
      }

      val toExplain: List[(String, Recorded)] = seen.toList.flatMap { kvs =>
        val res: List[(String, Recorded)] = kvs match {
          case (key, entry :: Nil) =>
            if (needsExplanation(entry)) (key.str, entry) :: Nil else Nil
          case (key, entries) =>
            for (alt <- entries) yield {
              val tickedString = record(key.str, key.isType, alt)
              (tickedString, alt)
            }
        }
        res // help the inferrencer out
      }.sortBy(_._1)

      def columnar(parts: List[(String, String)]): List[String] = {
        lazy val maxLen = parts.map(_._1.length).max
        parts.map {
          case (leader, trailer) =>
            val variable = hl(leader)
            s"""$variable${" " * (maxLen - leader.length)} $trailer"""
        }
      }

      val explainParts = toExplain.map { case (str, entry) => (str, explanation(entry)) }
      val explainLines = columnar(explainParts)
      if (explainLines.isEmpty) "" else i"where:    $explainLines%\n          %\n"
    end explanations
  end Seen

  /** Printer to be used when formatting messages */
  private class Printer(val seen: Seen, _ctx: Context) extends RefinedPrinter(_ctx):

    /** True if printer should a show source module instead of its module class */
    private def useSourceModule(sym: Symbol): Boolean =
      sym.is(ModuleClass, butNot = Package) && sym.sourceModule.exists && !_ctx.settings.YdebugNames.value

    override def simpleNameString(sym: Symbol): String =
      if useSourceModule(sym) then simpleNameString(sym.sourceModule)
      else seen.record(super.simpleNameString(sym), sym.isType, sym)

    override def ParamRefNameString(param: ParamRef): String =
      seen.record(super.ParamRefNameString(param), param.isInstanceOf[TypeParamRef], param)

    override def toTextRef(tp: SingletonType): Text = tp match
      case tp: SkolemType => seen.record(tp.repr.toString, isType = true, tp)
      case _ => super.toTextRef(tp)

    override def toText(tp: Type): Text =
      if !tp.exists || tp.isErroneous then seen.nonSensical = true
      tp match
        case tp: TypeRef if useSourceModule(tp.symbol) => Str("object ") ~ super.toText(tp)
        case _ => super.toText(tp)

    override def toText(sym: Symbol): Text =
      sym.infoOrCompleter match
        case _: ErrorType | TypeAlias(_: ErrorType) | NoType => seen.nonSensical = true
        case _ =>
      super.toText(sym)
  end Printer

end Message

/** A `Message` contains all semantic information necessary to easily
  * comprehend what caused the message to be logged. Each message can be turned
  * into a `Diagnostic` which contains the log level and can later be
  * consumed by a subclass of `Reporter`. However, the error position is only
  * part of `Diagnostic`, not `Message`.
  *
  * NOTE: you should not persist a message directly, because most messages take
  * an implicit `Context` and these contexts weigh in at about 4mb per instance.
  * Therefore, persisting these will result in a memory leak.
  *
  * Instead use the `persist` method to create an instance that does not keep a
  * reference to these contexts.
  *
  * @param errorId a unique id identifying the message, this will be
  *                used to reference documentation online
  *
  * Messages modify the rendendering of interpolated strings in several ways:
  *
  *  1. The size of the printed code is limited with a MessafeLimiter. If the message
  *    would get too large or too deeply nested, a `...` is printed instead.
  *  2. References to module classes are prefixed with `object ` for better recogniability.
  *  3. A where clause is sometimes added which contains the following additional explanations:
  *     - Rerences are disambiguated: If a message contains occurrences of the same identifier
  *       representing different symbols, the duplicates are printed with superscripts
  *       and the where-clause explains where each symbol is located.
  *     - Uninstantiated variables are explained in the where-clause with additional
  *       info about their bounds.
  *     - Skolems are explained with additional info about their underlying type.
  *
  *  Messages inheriting from the NoDisambiguation trait or returned from the
  *  `noDisambiguation()` method skip point (3) above. This makes sense if the
  *  message already exolains where different occurrences of the same identifier
  *  are located. Examples are NamingMsgs such as double definition errors,
  *  overriding errors, and ambiguous implicit errors.
  *
  *  We consciously made the design decision to disambiguate by default and disable
  *  disambiguation as an opt-in. The reason is that one usually does not consider all
  *  fine-grained details when writing an error message. If disambiguation is the default,
  *  some tests will show where clauses that look too noisy and that then can be disabled
  *  when needed. But if silence is the default, one usually does not realize that
  *  better info could be obtained by turning disambiguation on.
  */
abstract class Message(val errorId: ErrorMessageID)(using Context) { self =>
  import Message.*

  /** The kind of the error message, e.g. "Syntax" or "Type Mismatch".
    * This will be printed as "$kind Error", "$kind Warning", etc, on the first
    * line of the message.
    */
  def kind: MessageKind

  /** The `msg` contains the diagnostic message e.g:
    *
    * > expected: String
    * > found:    Int
    *
    * This message will be placed underneath the position given by the enclosing
    * `Diagnostic`. The message is given in raw form, with possible embedded
    *  <nonsensical> tags.
    */
  protected def msg(using Context): String

  /** The explanation should provide a detailed description of why the error
    * occurred and use examples from the user's own code to illustrate how to
    * avoid these errors. It might contain embedded <nonsensical> tags.
    */
  protected def explain(using Context): String

  /** What gets printed after the message proper */
  protected def msgPostscript(using Context): String =
    if ctx eq NoContext then ""
    else ctx.printer match
      case msgPrinter: Message.Printer =>
        myIsNonSensical = msgPrinter.seen.nonSensical
        val addendum = msgPrinter.seen.explanations
        msgPrinter.seen.disable()
          // Clear entries and stop futher recording so that messages containing the current
          // one don't repeat the explanations or use explanations from the msgPostscript.
        if addendum.isEmpty then "" else "\n\n" ++ addendum
      case _ =>
        ""

  /** Does this message have an explanation?
   *  This is normally the same as `explain.nonEmpty` but can be overridden
   *  if we need a way to return `true` without actually calling the
   *  `explain` method.
   */
  def canExplain: Boolean = explain.nonEmpty

  private var myIsNonSensical: Boolean = false

  /** A message is non-sensical if it contains references to internally
   *  generated error types. Normally we want to suppress error messages
   *  referring to types like this because they look weird and are normally
   *  follow-up errors to something that was diagnosed before.
   */
  def isNonSensical: Boolean = { message; myIsNonSensical }

  private var disambiguate: Boolean = true

  def withoutDisambiguation(): this.type =
    disambiguate = false
    this

  private def inMessageContext(disambiguate: Boolean)(op: Context ?=> String): String =
    if ctx eq NoContext then op
    else
      val msgContext = ctx.printer match
        case _: Message.Printer => ctx
        case _ =>
          val seen = Seen(disambiguate)
          val ctx1 = ctx.fresh.setPrinterFn(Message.Printer(seen, _))
          if !ctx1.property(MessageLimiter).isDefined then
            ctx1.setProperty(MessageLimiter, ErrorMessageLimiter())
          ctx1
      op(using msgContext)

  /** The message to report. <nonsensical> tags are filtered out */
  @threadUnsafe lazy val message: String =
    inMessageContext(disambiguate)(msg + msgPostscript)

  /** The explanation to report. <nonsensical> tags are filtered out */
  @threadUnsafe lazy val explanation: String =
    inMessageContext(disambiguate = false)(explain)

  /** The implicit `Context` in messages is a large thing that we don't want
    * persisted. This method gets around that by duplicating the message,
    * forcing its `msg` and `explanation` vals and dropping the implicit context
    * that was captured in the original message.
    */
  def persist: Message = new Message(errorId)(using NoContext):
    val kind  = self.kind
    private val persistedMsg = self.message
    private val persistedExplain = self.explanation
    def msg(using Context) = persistedMsg
    def explain(using Context) = persistedExplain
    override val canExplain = self.canExplain
    override def isNonSensical = self.isNonSensical

  def append(suffix: => String): Message = mapMsg(_ ++ suffix)
  def prepend(prefix: => String): Message = mapMsg(prefix ++ _)

  def mapMsg(f: String => String): Message = new Message(errorId):
    val kind = self.kind
    def msg(using Context) = f(self.msg)
    def explain(using Context) = self.explain
    override def canExplain = self.canExplain

  def appendExplanation(suffix: => String): Message = new Message(errorId):
    val kind = self.kind
    def msg(using Context) = self.msg
    def explain(using Context) = self.explain ++ suffix
    override def canExplain = true

  /** Override with `true` for messages that should always be shown even if their
   *  position overlaps another messsage of a different class. On the other hand
   *  multiple messages of the same class with overlapping positions will lead
   *  to only a single message of that class to be issued.
   */
  def showAlways = false

  override def toString = msg
}

/** A marker trait that suppresses generation of `where` clause for disambiguations */
trait NoDisambiguation extends Message:
  withoutDisambiguation()

/** The fallback `Message` containing no explanation and having no `kind` */
final class NoExplanation(msgFn: Context ?=> String)(using Context) extends Message(ErrorMessageID.NoExplanationID) {
  def msg(using Context): String = msgFn
  def explain(using Context): String = ""
  val kind: MessageKind = MessageKind.NoKind

  override def toString(): String = msg
}

/** The extractor for `NoExplanation` can be used to check whether any error
  * lacks an explanation
  */
object NoExplanation {
  def unapply(m: Message): Option[Message] =
    if (m.explanation == "") Some(m)
    else None
}
