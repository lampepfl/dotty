import scala.quoted._
import given scala.quoted.autolift._

class LineNumber(val value: Int) {
  override def toString: String = value.toString
}

object LineNumber {

  implicit inline def line[T >: Unit <: Unit]: LineNumber =
    ${lineImpl('[T])}

  def lineImpl(x: Type[Unit]) given (qctx: QuoteContext): Expr[LineNumber] = {
    import qctx.tasty._
    '{new LineNumber(${rootPosition.startLine})}
  }

}
