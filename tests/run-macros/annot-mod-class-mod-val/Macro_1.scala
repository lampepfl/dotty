//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class setValue(field: String, value: String) extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case ClassDef(name, ctr, parents, self, body) =>
        val cls = definition.symbol
        val valSym = cls.fieldMember(field)

        val newBody = body.span(_.symbol != valSym) match
          case (before, valDef :: after) =>
            val newValDef = ValDef(valSym, Some(Literal(StringConstant(value))))
            before ::: newValDef :: after
          case _ =>
            report.error(s"`val $field` was not defined")
            body

        List(ClassDef.copy(definition)(name, ctr, parents, self, newBody))
      case _ =>
        report.error("Annotation only supports `class`")
        List(definition)
