import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class addTopLevelVal extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case ClassDef(name, ctr, parents, self, body) =>
        val valSym = Symbol.newVal(Symbol.spliceOwner, Symbol.freshName("toLevelVal"), TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
        val valDef = ValDef(valSym, Some(Literal(IntConstant(1))))
        List(valDef, definition)
      case _ =>
        report.error("Annotation only supports `class`")
        List(definition)
