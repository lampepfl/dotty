//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable.Map

@experimental
class memoize extends MacroAnnotation {
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case DefDef(name, params, tpt, Some(fibTree)) =>
        val cacheName = Symbol.freshName(name + "Cache")
        val cacheSymbol = Symbol.newVal(Symbol.spliceOwner, cacheName, TypeRepr.of[Map[Int, Int]], Flags.EmptyFlags, Symbol.noSymbol)
        val cacheRhs =
          given Quotes = cacheSymbol.asQuotes
          '{Map.empty[Int, Int]}.asTerm
        val cacheVal = ValDef(cacheSymbol, Some(cacheRhs))
        val rhs =
          given Quotes = definition.symbol.asQuotes
          val fibCache = Ref(cacheSymbol).asExprOf[Map[Int, Int]]
          val n = Ref(params.head.params.head.symbol).asExprOf[Int]
          '{
            if $fibCache.contains($n) then
                $fibCache($n)
            else
              val res = ${fibTree.asExprOf[Int]}
              $fibCache($n) = res
              res
          }.asTerm
        val newFib = DefDef.copy(definition)(name, params, tpt, Some(rhs))
        List(cacheVal, newFib)
}
