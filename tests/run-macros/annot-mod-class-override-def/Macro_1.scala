//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class genToString(msg: String) extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case ClassDef(name, ctr, parents, self, body) =>
        val cls = definition.symbol
        val toStringSym = Symbol.requiredMethod("java.lang.Object.toString")

        val toStringOverrideSym = Symbol.newMethod(cls, "toString", toStringSym.info, Flags.Override, Symbol.noSymbol)

        val toStringDef = DefDef(toStringOverrideSym, _ => Some(Literal(StringConstant(msg))))

        val newClassDef = ClassDef.copy(definition)(name, ctr, parents, self, toStringDef :: body)
        List(newClassDef)
      case _ =>
        report.error("Annotation only supports `class`")
        List(definition)
