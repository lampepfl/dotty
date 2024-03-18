import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class error extends MacroAnnotation {
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    quotes.reflect.report.error("MACRO ERROR", definition.pos)
    List(definition)
}
