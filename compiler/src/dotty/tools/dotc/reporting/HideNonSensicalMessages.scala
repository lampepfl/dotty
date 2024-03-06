package dotty.tools
package dotc
package reporting

import core.Contexts._

/**
 * This trait implements `isHidden` so that we avoid reporting non-sensical messages.
 */
trait HideNonSensicalMessages extends Reporter {
  /** Hides non-sensical messages, unless we haven't reported any error yet or
   *  `-Yshow-suppressed-errors` is set.
   */
  override def isHidden(dia: Diagnostic)(using Context): Boolean =
    super.isHidden(dia) || {
        hasErrors  // if there are no errors yet, report even if diagnostic is non-sensical
        && dia.msg.isNonSensical // defer forcing the message by calling hasErrors first
        && !ctx.settings.YshowSuppressedErrors.value
    }
}
