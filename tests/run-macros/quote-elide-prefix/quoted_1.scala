import scala.quoted._

object Macro {

  // By name StringContext is used to elide the prefix
  inline def (sc: => StringContext) ff (args: => Any*): String = ${ Macro.impl('sc, 'args) }

  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]]) with QuoteContext : Expr[String] = '{ $args.mkString }
}
