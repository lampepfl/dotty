//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class addInnerClass extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case ClassDef(name, ctr, parents, self, body) =>
        val cls = definition.symbol

        def showClassDecls(showCls: Symbol): List[Symbol] =
          List(Symbol.newMethod(showCls, "showMe", MethodType(List("x"))(_ => List(cls.typeRef), _ => TypeRepr.of[String])))
        val parents = List(TypeTree.of[Object])
        val showClassSym = Symbol.newClass(cls, Symbol.freshName("Show"), parents.map(_.tpe), showClassDecls, None)
        val showMeSym = showClassSym.declaredMethod("showMe").head

        val showMeDef = DefDef(showMeSym, argss => Some('{ "showMe: " + ${argss.head.head.asExpr}.getClass }.asTerm))
        val showClass = ClassDef(showClassSym, parents, body = List(showMeDef))
        val newShow = Apply(Select(New(TypeIdent(showClassSym)), showClassSym.primaryConstructor), Nil)
        val newShowCallShowMe = Apply(Select(newShow, showMeSym), List(This(cls)))

        val toStringMethType = Symbol.requiredMethod("java.lang.Object.toString").info
        val toStringOverrideSym = Symbol.newMethod(cls, "toString", toStringMethType, Flags.Override, Symbol.noSymbol)
        val toStringDef = DefDef(toStringOverrideSym, _ => Some(newShowCallShowMe))

        val newClassDef = ClassDef.copy(definition)(name, ctr, parents, self, showClass :: toStringDef :: body)
        List(newClassDef)

      case _ =>
        report.error("Annotation only supports `class`")
        List(definition)
