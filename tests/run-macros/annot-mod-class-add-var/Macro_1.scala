//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class addCountToString(msg: String) extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case ClassDef(name, ctr, parents, self, body) =>
        val cls = definition.symbol
        val countVarSym = Symbol.newVal(cls, Symbol.freshName("count"), TypeRepr.of[Int], Flags.Mutable | Flags.Private, Symbol.noSymbol)

        val toStringMethType = Symbol.requiredMethod("java.lang.Object.toString").info
        val toStringOverrideSym = Symbol.newMethod(cls, "toString", toStringMethType, Flags.Override, Symbol.noSymbol)

        val countRef = Ref(countVarSym)
        val countRefExpr = countRef.asExprOf[Int]
        val countVarDef = ValDef(countVarSym, Some(Literal(IntConstant(0))))
        val toStringDef = DefDef(toStringOverrideSym, _ => Some(
          Block(
            Assign(countRef, '{ $countRefExpr + 1 }.asTerm) :: Nil,
            '{ ${Expr(msg)} + $countRefExpr }.asTerm
          )
        ))

        val newClassDef = ClassDef.copy(definition)(name, ctr, parents, self, countVarDef :: toStringDef :: body)
        List(newClassDef)

      case _ =>
        report.error("Annotation only supports `class`")
        List(definition)
