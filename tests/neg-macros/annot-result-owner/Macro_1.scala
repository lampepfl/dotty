import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class insertVal extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    // Use of wrong owner
    val valSym = Symbol.newVal(definition.symbol, Symbol.freshName("definitionWithWrongOwner"), TypeRepr.of[Unit], Flags.Private, Symbol.noSymbol)
    val valDef = ValDef(valSym, Some('{}.asTerm))
    List(valDef, definition)
