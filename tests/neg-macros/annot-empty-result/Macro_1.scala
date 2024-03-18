import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class nilAnnot extends MacroAnnotation {
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    Nil
}
