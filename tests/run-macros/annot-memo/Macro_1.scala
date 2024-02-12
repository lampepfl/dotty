import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class memoize extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case DefDef(name, TermParamClause(param  :: Nil) :: Nil, tpt, Some(rhsTree)) =>
        (param.tpt.tpe.asType, tpt.tpe.asType) match
          case ('[t], '[u]) =>
            val cacheName = Symbol.freshName(name + "Cache")
            val cacheSymbol = Symbol.newVal(Symbol.spliceOwner, cacheName, TypeRepr.of[mutable.Map[t, u]], Flags.Private, Symbol.noSymbol)
            val cacheRhs =
              given Quotes = cacheSymbol.asQuotes
              '{ mutable.Map.empty[t, u] }.asTerm
            val cacheVal = ValDef(cacheSymbol, Some(cacheRhs))
            val newRhs =
              given Quotes = definition.symbol.asQuotes
              val cacheRefExpr = Ref(cacheSymbol).asExprOf[mutable.Map[t, u]]
              val paramRefExpr = Ref(param.symbol).asExprOf[t]
              val rhsExpr = rhsTree.asExprOf[u]
              '{ $cacheRefExpr.getOrElseUpdate($paramRefExpr, $rhsExpr) }.asTerm
            val newTree = DefDef.copy(definition)(name, TermParamClause(param :: Nil) :: Nil, tpt, Some(newRhs))
            List(cacheVal, newTree)
      case _ =>
        report.error("Annotation only supported on `def` with a single argument are supported")
        List(definition)
