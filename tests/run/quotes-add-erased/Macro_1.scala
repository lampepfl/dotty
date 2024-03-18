//> using options -experimental -Yno-experimental

import scala.annotation.MacroAnnotation
import scala.annotation.internal.ErasedParam
import scala.quoted._

class NewAnnotation extends scala.annotation.Annotation

class erasedParamsMethod extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case ClassDef(name, ctr, parents, self, body) =>
        val erasedInt = AnnotatedType(TypeRepr.of[Int], '{ new ErasedParam }.asTerm)
        val methType = MethodType(List("x", "y"))(_ => List(erasedInt, TypeRepr.of[Int]), _ => TypeRepr.of[Int])

        assert(methType.hasErasedParams)
        assert(methType.erasedParams == List(true, false))

        val methSym = Symbol.newMethod(tree.symbol, "takesErased", methType, Flags.Override, Symbol.noSymbol)
        val methDef = DefDef(methSym, _ => Some(Literal(IntConstant(1))))

        val clsDef = ClassDef.copy(tree)(name, ctr, parents, self, methDef :: body)

        List(clsDef)
      case _ =>
        report.error("Annotation only supports `class`")
        List(tree)
