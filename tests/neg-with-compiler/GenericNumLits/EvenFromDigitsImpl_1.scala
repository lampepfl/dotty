import language.experimental.genericNumberLiterals
import scala.util.FromDigits
import scala.quoted.*
import Even.*

object EvenFromDigitsImpl:
  def apply(digits: Expr[String])(using Quotes): Expr[Even] = digits.value match {
    case Some(ds) =>
      val ev =
        try evenFromDigits(ds)
        catch {
          case ex: FromDigits.FromDigitsException =>
            quotes.reflect.report.error(ex.getMessage.nn)
            Even(0)
        }
      '{Even(${Expr(ev.n)})}
    case _ =>
      '{evenFromDigits($digits)}
  }
