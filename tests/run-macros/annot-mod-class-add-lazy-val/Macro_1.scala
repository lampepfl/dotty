//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class addMemoToString(msg: String) extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case ClassDef(name, ctr, parents, self, body) =>
        val cls = definition.symbol
        val stringLazyValSym = Symbol.newVal(cls, Symbol.freshName("string"), TypeRepr.of[String], Flags.Lazy | Flags.Private, Symbol.noSymbol)

        val toStringMethType = Symbol.requiredMethod("java.lang.Object.toString").info
        val toStringOverrideSym = Symbol.newMethod(cls, "toString", toStringMethType, Flags.Override, Symbol.noSymbol)

        val stringLazyValDef = ValDef(stringLazyValSym, Some(Literal(StringConstant(msg))))
        val toStringDef = DefDef(toStringOverrideSym, _ => Some(Ref(stringLazyValSym)))

        val newClassDef = ClassDef.copy(definition)(name, ctr, parents, self, stringLazyValDef :: toStringDef :: body)
        List(newClassDef)

      case _ =>
        report.error("Annotation only supports `class`")
        List(definition)
