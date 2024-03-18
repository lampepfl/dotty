import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class foo extends MacroAnnotation {
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val s = '{@hello def foo1(x: Int): Int = x + 1;()}.asTerm
    val fooDef = s.asInstanceOf[Inlined].body.asInstanceOf[Block].statements.head.asInstanceOf[DefDef]
    val hello = Ref(Symbol.spliceOwner.declaredFields("hello").head).asExprOf[String] // error
    definition match
      case DefDef(name, params, tpt, Some(t)) =>
        val rhs = '{
          ${t.asExprOf[String]} + $hello
        }.asTerm
        val newDef = DefDef.copy(definition)(name, params, tpt, Some(rhs))
        List(fooDef, newDef)
}
