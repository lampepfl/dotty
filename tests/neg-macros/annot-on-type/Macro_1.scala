import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class voidAnnot extends MacroAnnotation {
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    List(definition)
}
