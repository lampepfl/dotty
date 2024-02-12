import scala.annotation.experimental
import scala.annotation.MacroAnnotation
import scala.quoted.*

@experimental
class wrongOwner extends MacroAnnotation :
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    definition match
      case ClassDef(name, ctr, parents, self, body) =>
        val cls = definition.symbol
        val toStringSym = Symbol.requiredMethod("java.lang.Object.toString")
        val toStringOverrideSym = Symbol.newMethod(Symbol.classSymbol("java.lang.String"), "toString", toStringSym.info, Flags.Override, Symbol.noSymbol)
        val toStringDef = DefDef(toStringOverrideSym, _ => Some(Literal(StringConstant("Hello from macro"))))
        val newClassDef = ClassDef.copy(definition)(name, ctr, parents, self, toStringDef :: body)
        List(newClassDef)
      case _ =>
        report.error("@toString can only be annotated on class definitions")
        definition :: Nil
