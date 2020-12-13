import scala.quoted._
import scala.language.implicitConversions

object Macro {

  extension (strCtx: => StringContext) implicit inline def f3(args: =>Any*): String = ${FIntepolator.apply('strCtx, 'args)}

}

object FIntepolator {
  def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using Quotes) : Expr[String] = {
    import quotes.reflect._
    report.error("there are no args", argsExpr.asTerm.underlyingArgument.pos)
    '{ ($strCtxExpr).s($argsExpr: _*) }
  }

}
