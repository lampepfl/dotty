package dotty.tools
package dotc
package reporting

import org.junit.Assert._
import org.junit.Test
import core.Contexts._
import diagnostic._

class TestMessageLaziness extends DottyTest {
  ctx = ctx.fresh.setReporter(new NonchalantReporter)

  class NonchalantReporter(implicit ctx: Context) extends Reporter
  with UniqueMessagePositions with HideNonSensicalMessages {
    def doReport(m: MessageContainer)(implicit ctx: Context) = ???

    override def report(m: MessageContainer)(implicit ctx: Context) = ()
  }

  case class LazyError() extends Message(ErrorMessageID.LazyErrorId) {
    throw new Error("Didn't stay lazy.")

    val kind = ErrorKind.NO_KIND
    val msg = "Please don't blow up"
    val explanation = ""
  }

  @Test def assureLazy =
    ctx.error(LazyError())

  @Test def assureLazyExtendMessage =
    ctx.strictWarning(LazyError())
}
