import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class hello extends MacroAnnotation {
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val helloSymbol = Symbol.newVal(Symbol.spliceOwner, Symbol.freshName("hello"), TypeRepr.of[String], Flags.EmptyFlags, Symbol.noSymbol)
    val helloVal = ValDef(helloSymbol, Some(Literal(StringConstant("Hello, World!"))))
    List(helloVal, definition)
}
