//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class bind(str: String) extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case ValDef(name, tpt, Some(rhsTree)) =>
        val valSym = Symbol.newVal(Symbol.spliceOwner, Symbol.freshName(str), tpt.tpe, Flags.Private, Symbol.noSymbol)
        val valDef = ValDef(valSym, Some(rhsTree))
        val newRhs = Ref(valSym)
        val newTree = ValDef.copy(definition)(name, tpt, Some(newRhs))
        List(valDef, newTree)
      case _ =>
        report.error("Annotation only supported on `val` with a single argument are supported")
        List(definition)
