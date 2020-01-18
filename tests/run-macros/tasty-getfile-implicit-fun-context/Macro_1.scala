import scala.quoted._
import scala.quoted.autolift.given

object SourceFiles {

  type Macro[X] = QuoteContext ?=> Expr[X]
  def tastyContext with (qctx: QuoteContext) : QuoteContext = qctx

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] = {
    val qctx = tastyContext
    import qctx.tasty.{_, given}
    rootContext.source.getFileName.toString
  }

}
