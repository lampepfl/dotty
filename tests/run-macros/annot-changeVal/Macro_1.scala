//> using options -experimental -Yno-experimental

import scala.annotation.experimental
import scala.quoted.*
import scala.annotation.MacroAnnotation

object ChangeVal:
  @experimental
  class change(i: Int) extends MacroAnnotation {
    def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
      import quotes.reflect.*
      definition match
        case ValDef(n, t, _) => List(ValDef.copy(definition)(n, t, Some(Literal(IntConstant(i)))))
  }
