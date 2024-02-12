import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class gen1 extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    definition match
      case ClassDef(name, ctr, parents, self, body) =>
        val cls = definition.symbol
        // val meth = cls.methodMember("foo").head
        // val fooTpe = cls.typeRef.memberType(meth)

        val overrideTpe =  MethodType(Nil)(_ => Nil, _ => defn.StringClass.typeRef)

        val fooOverrideSym = Symbol.newMethod(cls, "foo", overrideTpe, Flags.Override, Symbol.noSymbol)

        val fooDef = DefDef(fooOverrideSym, _ => Some(Literal(StringConstant("hi"))))

        val newClassDef = ClassDef.copy(definition)(name, ctr, parents, self, fooDef :: body)
        List(newClassDef)
      case _ =>
        report.error("Annotation only supports `class`")
        List(definition)
