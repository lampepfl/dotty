import scala.quoted._
object Test {
  def impl(receiver: Expr[StringContext])(using qctx: scala.quoted.Quotes) = {
    import quotes.reflect.Repeated
    receiver match {
      case '{ StringContext(${Repeated(parts, tpt)}: _*) } => // now OK
    }
  }
}
