//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class print(msg: String) extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case DefDef(name, params, tpt, Some(rhsTree)) =>
        given Quotes = definition.symbol.asQuotes
        rhsTree.asExpr match
          case '{ $rhsExpr: t } =>
            val newRhs = '{ println(${Expr(msg)}); $rhsExpr }.asTerm
            List(DefDef.copy(definition)(name, params, tpt, Some(newRhs)))
      case _ =>
        report.error("Annotation only supported on `def`")
        List(definition)
