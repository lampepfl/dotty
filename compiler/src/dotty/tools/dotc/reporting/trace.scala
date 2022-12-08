package dotty.tools
package dotc
package reporting

import scala.language.unsafeNulls

import core.*, Contexts.*, Decorators.*
import config.*
import printing.Formatting.*

import scala.compiletime.*

/** Exposes the {{{ trace("question") { op } }}} syntax.
 *
 * Traced operations will print indented messages if enabled.
 * Tracing depends on [[Config.tracingEnabled]] and [[dotty.tools.dotc.config.ScalaSettings.Ylog]].
 * Tracing can be forced by replacing [[trace]] with [[trace.force]] or [[trace.log]] (see below).
 */
object trace extends TraceSyntax:
  inline def isEnabled = Config.tracingEnabled
  protected val isForced = false

  object force extends TraceSyntax:
    inline def isEnabled: true = true
    protected val isForced = true

  object log extends TraceSyntax:
    inline def isEnabled: true = true
    protected val isForced = false
end trace

/** This module is carefully optimized to give zero overhead if Config.tracingEnabled
 *  is false. The `trace` operation is called in various hotspots, so every tiny bit
 *  of overhead is unacceptable: boxing, closures, additional method calls are all out.
 */
trait TraceSyntax:

  inline def isEnabled: Boolean
  protected val isForced: Boolean

  inline def onDebug[TD](inline question: String)(inline op: TD)(using Context): TD =
    conditionally(ctx.settings.YdebugTrace.value, question, false)(op)

  inline def conditionally[TC](inline cond: Boolean, inline question: String, inline show: Boolean)(inline op: TC)(using Context): TC =
    inline if isEnabled then
      apply(question, if cond then Printers.default else Printers.noPrinter, show)(op)
    else op

  inline def apply[T, U >: T](inline question: String, inline printer: Printers.Printer, inline showOp: U => String)(inline op: T)(using Context): T =
    inline if isEnabled then
      doTrace[T](question, printer, showOp)(op)
    else op

  inline def apply[T](inline question: String, inline printer: Printers.Printer, inline show: Boolean)(inline op: T)(using Context): T =
    apply(question, printer, {
      val showOp: T => String = inline if show == true then
        val showT = summonInline[Show[T]]
        {
          given Show[T] = showT
          t => i"$t"
        }
      else
        summonFrom {
          case given Show[T] => t => i"$t"
          case _             => alwaysToString
        }
      showOp
    })(op)

  inline def apply[T](inline question: String, inline printer: Printers.Printer)(inline op: T)(using Context): T =
    apply[T](question, printer, false)(op)

  inline def apply[T](inline question: String, inline show: Boolean)(inline op: T)(using Context): T =
    apply[T](question, Printers.default, show)(op)

  inline def apply[T](inline question: String)(inline op: T)(using Context): T =
    apply[T](question, false)(op)

  private val alwaysToString = (x: Any) => String.valueOf(x)

  private def doTrace[T](question: => String,
                         printer: Printers.Printer = Printers.default,
                         showOp: T => String)
                        (op: => T)(using Context): T =
    if ctx.mode.is(Mode.Printing) || !isForced && (printer eq Printers.noPrinter) then op
    else
      // Avoid evaluating question multiple time, since each evaluation
      // may cause some extra logging output.
      val q = question
      val leading = s"==> $q?"
      val trailing = (res: T) => s"<== $q = ${showOp(res)}"
      var finalized = false
      def skipStoreReporter(c: Context): Context = c.reporter match
        case _: StoreReporter => skipStoreReporter(c.outer)
        case _ => c
      val logctx = skipStoreReporter(ctx)
      def margin = ctx.base.indentTab * ctx.base.indent
      def doLog(s: String) = if isForced then println(s) else report.log(s)(using logctx)
      def finalize(msg: String) =
        if !finalized then
          ctx.base.indent -= 1
          doLog(s"$margin$msg")
          finalized = true
      try
        doLog(s"$margin$leading")
        ctx.base.indent += 1
        val res = op
        finalize(trailing(res))
        res
      catch
        case ex: runtime.NonLocalReturnControl[T] =>
          finalize(trailing(ex.value))
          throw ex
        case ex: Throwable =>
          val msg = s"<== $q = <missing> (with exception $ex)"
          finalize(msg)
          throw ex
end TraceSyntax
