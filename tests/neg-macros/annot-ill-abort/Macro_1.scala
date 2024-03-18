import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class crash extends MacroAnnotation {
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    throw new scala.quoted.runtime.StopMacroExpansion
}
