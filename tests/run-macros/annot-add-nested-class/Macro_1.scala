//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class addClass extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case DefDef(name, List(TermParamClause(Nil)), tpt, Some(rhs)) =>
        val parents = List(TypeTree.of[Object])
        def decls(cls: Symbol): List[Symbol] =
          List(Symbol.newMethod(cls, "run", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]), Flags.EmptyFlags, Symbol.noSymbol))

        val newClassName = Symbol.freshName("Bar")
        val cls = Symbol.newClass(Symbol.spliceOwner, newClassName, parents = parents.map(_.tpe), decls, selfType = None)
        val runSym = cls.declaredMethod("run").head

        val runDef = DefDef(runSym, _ => Some(rhs))
        val clsDef = ClassDef(cls, parents, body = List(runDef))

        val newCls = Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil)

        val newDef = DefDef.copy(definition)(name, List(TermParamClause(Nil)), tpt, Some(Apply(Select(newCls, runSym), Nil)))
        List(clsDef, newDef)
      case _ =>
        report.error("Annotation only supports `def` with one argument")
        List(definition)
