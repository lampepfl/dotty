package dotty.tools
package dotc
package reporting

import core.Contexts.*
import collection.mutable
import config.Printers.typr
import Diagnostic.*

/** This class implements a Reporter that stores all messages
  *
  * Beware that this reporter can leak memory, and force messages in two
  * scenarios:
  *
  * - During debugging `config.Printers.typr` is set from `noPrinter` to `new
  *   Printer`, which forces the message
  * - The reporter is not flushed and the message containers capture a
  *   `Context` (about 4MB)
  */
class StoreReporter(outer: Reporter | Null = Reporter.NoReporter, fromTyperState: Boolean = false) extends Reporter {

  protected var infos: mutable.ListBuffer[Diagnostic] | Null = null

  override def doReport(dia: Diagnostic)(using Context): Unit = {
    typr.println(s">>>> StoredError: ${dia.message}") // !!! DEBUG
    if (infos == null) infos = new mutable.ListBuffer
    infos.uncheckedNN += dia
  }

  override def hasUnreportedErrors: Boolean =
    outer != null && infos != null && infos.uncheckedNN.exists(_.isInstanceOf[Error])

  override def hasStickyErrors: Boolean =
    infos != null && infos.uncheckedNN.exists(_.isInstanceOf[StickyError])

  override def removeBufferedMessages(using Context): List[Diagnostic] =
    if (infos != null) try infos.uncheckedNN.toList finally infos = null
    else Nil

  override def pendingMessages(using Context): List[Diagnostic] =
    if (infos != null) infos.uncheckedNN.toList else Nil

  override def errorsReported: Boolean = hasErrors || (outer != null && outer.errorsReported)

  // If this is a TyperState buffering reporter then buffer the messages,
  // so that then only when the messages are unbuffered (when the reporter if flushed)
  // do they go through -Wconf, and possibly then buffered on the Run as a suspended message
  override def report(dia: Diagnostic)(using Context): Unit =
    if fromTyperState then issueUnconfigured(dia)
    else super.report(dia)
}
