import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class cycle extends MacroAnnotation {
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    new Foo
    List(definition)
}
