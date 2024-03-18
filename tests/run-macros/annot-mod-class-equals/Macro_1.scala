//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*

@experimental
class equals extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    definition match
      case ClassDef(className, ctr, parents, self, body) =>
        val cls = definition.symbol

        val constructorParameters = ctr.paramss.collect { case clause: TermParamClause => clause }
        if constructorParameters.size != 1 || constructorParameters.head.params.isEmpty then
          report.errorAndAbort("@equals class must have a single argument list with at least one argument", ctr.pos)
        def checkNotOverridden(sym: Symbol): Unit =
          if sym.overridingSymbol(cls).exists then
            report.error(s"Cannot override ${sym.name} in a @equals class")

        val fields = body.collect {
          case vdef: ValDef if vdef.symbol.flags.is(Flags.ParamAccessor) =>
            Select(This(cls), vdef.symbol).asExpr
        }

        val equalsSym = Symbol.requiredMethod("java.lang.Object.equals")
        checkNotOverridden(equalsSym)
        val equalsOverrideSym = Symbol.newMethod(cls, "equals", equalsSym.info, Flags.Override, Symbol.noSymbol)
        def equalsOverrideDefBody(argss: List[List[Tree]]): Option[Term] =
          given Quotes = equalsOverrideSym.asQuotes
          cls.typeRef.asType match
            case '[c] =>
              Some(equalsExpr[c](argss.head.head.asExpr, fields).asTerm)
        val equalsOverrideDef = DefDef(equalsOverrideSym, equalsOverrideDefBody)

        val hashSym = Symbol.newVal(cls, Symbol.freshName("hash"), TypeRepr.of[Int], Flags.Private | Flags.Lazy, Symbol.noSymbol)
        val hashVal = ValDef(hashSym, Some(hashCodeExpr(className, fields)(using hashSym.asQuotes).asTerm))

        val hashCodeSym = Symbol.requiredMethod("java.lang.Object.hashCode")
        checkNotOverridden(hashCodeSym)
        val hashCodeOverrideSym = Symbol.newMethod(cls, "hashCode", hashCodeSym.info, Flags.Override, Symbol.noSymbol)
        val hashCodeOverrideDef = DefDef(hashCodeOverrideSym, _ => Some(Ref(hashSym)))

        val newBody = equalsOverrideDef :: hashVal :: hashCodeOverrideDef :: body
        List(ClassDef.copy(definition)(className, ctr, parents, self, newBody))
      case _ =>
        report.error("Annotation only supports `class`")
        List(definition)

  private def equalsExpr[T: Type](that: Expr[Any], thisFields: List[Expr[Any]])(using Quotes): Expr[Boolean] =
    '{
      $that match
        case that: T @unchecked =>
          ${
            val thatFields: List[Expr[Any]] =
              import quotes.reflect.*
              thisFields.map(field => Select('{that}.asTerm, field.asTerm.symbol).asExpr)
            thisFields.zip(thatFields)
              .map { case (thisField, thatField) => '{ $thisField == $thatField } }
              .reduce { case (pred1, pred2) => '{ $pred1 && $pred2 } }
          }
        case _ => false
    }

  private def hashCodeExpr(className: String, thisFields: List[Expr[Any]])(using Quotes): Expr[Int] =
    '{
      var acc: Int = ${ Expr(scala.runtime.Statics.mix(-889275714, className.hashCode)) }
      ${
        Expr.block(
          thisFields.map {
            case '{ $field: Boolean } => '{ if $field then 1231 else 1237 }
            case '{ $field: Byte } => '{ $field.toInt }
            case '{ $field: Char } => '{ $field.toInt }
            case '{ $field: Short } => '{ $field.toInt }
            case '{ $field: Int } => field
            case '{ $field: Long } => '{ scala.runtime.Statics.longHash($field) }
            case '{ $field: Double } => '{ scala.runtime.Statics.doubleHash($field) }
            case '{ $field: Float } => '{ scala.runtime.Statics.floatHash($field) }
            case '{ $field: Null } => '{ 0 }
            case '{ $field: Unit } => '{ 0 }
            case field => '{ scala.runtime.Statics.anyHash($field) }
          }.map(hash => '{ acc = scala.runtime.Statics.mix(acc, $hash) }),
          '{ scala.runtime.Statics.finalizeHash(acc, ${Expr(thisFields.size)}) }
        )
      }
    }
