import scala.quoted._
import scala.language.implicitConversions

object Macro {

  extension (strCtx: => StringContext) implicit inline def f3(args: =>Any*): String = ${FIntepolator.apply('strCtx, 'args)}

}

object FIntepolator {
  def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using qctx: QuoteContext) : Expr[String] = {
    import qctx.reflect._
    Reporting.error("there are no args", Term.of(argsExpr).underlyingArgument.pos)
    '{ ($strCtxExpr).s($argsExpr: _*) }
  }

}
