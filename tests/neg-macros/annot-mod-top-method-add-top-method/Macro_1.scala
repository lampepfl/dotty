import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
// Assumes annotation is on top level def or val
class addTopLevelMethodOutsidePackageObject extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val methType = MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Int])
    val methSym = Symbol.newMethod(Symbol.spliceOwner.owner, Symbol.freshName("toLevelMethod"), methType, Flags.EmptyFlags, Symbol.noSymbol)
    val methDef = DefDef(methSym, _ => Some(Literal(IntConstant(1))))
    List(methDef, definition)
