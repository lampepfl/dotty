import scala.quoted._

inline def showParamSyms(inline x: Any): String =
  ${ showParamSymsExpr('x) }

def showParamSymsExpr(using Quotes)(x: Expr[Any]): Expr[String] =
  import quotes.reflect._
  val '{ $y: Any } = x // Drop Inlined not to access the symbol
  val sym = y.asTerm.symbol
  Expr(
    s"""sym: ${sym.fullName}
       |paramSymss: ${sym.paramSymss.map(_.map(_.fullName))}
       |""".stripMargin)
