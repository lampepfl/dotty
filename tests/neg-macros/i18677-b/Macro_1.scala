//> using -expermiental

import annotation.MacroAnnotation
import quoted.*

class Foo

class extendFoo extends MacroAnnotation :
    def transform(using Quotes)(definition: quotes.reflect.Definition,
                                companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
        import quotes.reflect.*
        definition match
            case ClassDef(name, ctr, p, self, body) =>
                val parents = List(TypeTree.of[Foo])
                val newTree = ClassDef.copy(definition)(name, ctr,  parents, self, body)
                newTree :: Nil
            case _ =>
                report.error("@extendFoo can only annotate class definitions")
                definition :: Nil