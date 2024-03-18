//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class overrideField(field: String, value: String) extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case ClassDef(name, ctr, parents, self, body) =>
        val cls = definition.symbol

        val overrideSym = Symbol.newVal(cls, field, TypeRepr.of[String], Flags.Override, Symbol.noSymbol)

        val valDef = ValDef(overrideSym, Some(Literal(StringConstant(value))))

        val newClassDef = ClassDef.copy(definition)(name, ctr, parents, self, valDef :: body)
        List(newClassDef)
      case _ =>
        report.error("Annotation only supports `class`")
        List(definition)
