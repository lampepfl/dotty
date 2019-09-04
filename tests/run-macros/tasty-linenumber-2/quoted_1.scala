import scala.quoted._
import given scala.quoted.autolift._

class LineNumber(val value: Int) {
  override def toString: String = value.toString
}

object LineNumber {

  implicit inline def line: LineNumber = ${lineImpl}

  def lineImpl given (qctx: QuoteContext): Expr[LineNumber] = {
    import qctx.tasty._
    '{new LineNumber(${rootPosition.startLine})}
  }

}
