//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class foo extends MacroAnnotation {
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case DefDef(name, params, tpt, Some(t)) =>
        given Quotes = definition.symbol.asQuotes
        val s = Ref(params.head.params.head.symbol).asExprOf[String]
        val rhs = '{
          @hello def foo1(s: String): String = ${
            @hello def foo(s: String) = s + "a"
            Expr(foo("a"))
          }
          foo1($s)
        }.asTerm
        val newDef = DefDef.copy(definition)(name, params, tpt, Some(rhs))
        List(newDef)
}
