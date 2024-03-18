//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class addIndirectToString(msg: String) extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case ClassDef(name, ctr, parents, self, body) =>
        val cls = definition.symbol
        val stringMethType = ByNameType.apply(TypeRepr.of[String])
        val stringSym = Symbol.newMethod(cls, Symbol.freshName("string"), stringMethType, Flags.Private, Symbol.noSymbol)
        val stringDef = DefDef(stringSym, _ => Some(Literal(StringConstant(msg))))

        val toStringMethType = Symbol.requiredMethod("java.lang.Object.toString").info
        val toStringOverrideSym = Symbol.newMethod(cls, "toString", toStringMethType, Flags.Override, Symbol.noSymbol)
        val toStringDef = DefDef(toStringOverrideSym, _ => Some(Ref(stringSym)))


        val newClassDef = ClassDef.copy(definition)(name, ctr, parents, self, stringDef :: toStringDef :: body)
        List(newClassDef)

      case _ =>
        report.error("Annotation only supports `class`")
        List(definition)
