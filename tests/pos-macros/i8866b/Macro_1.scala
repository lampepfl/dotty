import scala.quoted._

object Other {
  inline def apply = 5
}

object Macro {

  def impl(using qctx: QuoteContext): Expr[Int] = {
    import qctx.reflect._

    ValDef.let(
      Symbol.spliceOwner,
      Select.unique(
        Term.of('{ Other }),
        "apply"
      )
    )(identity).asExprOf[Int]

  }

  inline def apply = ${ Macro.impl }

}
