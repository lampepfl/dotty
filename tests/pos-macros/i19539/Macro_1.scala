import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*

@experimental
class annotation extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    List(definition)
