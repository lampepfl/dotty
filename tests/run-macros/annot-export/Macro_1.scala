//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable.Map

@experimental
class returnClassName extends MacroAnnotation {
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case DefDef(name, params, tpt, _) =>
        val rhs = Literal(StringConstant(Symbol.spliceOwner.name.stripSuffix("$")))
        List(DefDef.copy(definition)(name, params, tpt, Some(rhs)))
}
