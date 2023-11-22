//> using option "-Ycheck:all"
import scala.annotation.MacroAnnotation
import scala.quoted.*

class annotation extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition) =
    import quotes.reflect.*

    tree match
      case tree: ClassDef =>
        val List(DefDef(name, paramss, tpt, Some(body))) = tree.body: @unchecked
        val rhs = body match
          case Inlined(_, _, Block(List(Apply(_ /* `locally` */, List(rhs))), _)) => rhs
        val method = DefDef.copy(tree.body.head)(name, paramss, tpt, Some(rhs.changeOwner(tree.body.head.symbol)))
        List(ClassDef.copy(tree)(tree.name, tree.constructor, tree.parents, tree.self, List(method)))
      case tree: DefDef =>
        val DefDef(name, paramss, tpt, Some(body)) = tree: @unchecked
        val rhs = body match
          case Inlined(_, _, Block(List(Apply(_ /* `locally` */, List(rhs))), _)) => rhs
        val defdef = DefDef.copy(tree)(name, paramss, tpt, Some(rhs.changeOwner(tree.symbol)))
        List(defdef)
      case tree: ValDef =>
        val ValDef(name, tpt, Some(body)) = tree: @unchecked
        val rhs = body match
          case Inlined(_, _, Block(List(Apply(_ /* `locally` */, List(rhs))), _)) => rhs
        val valdef = ValDef.copy(tree)(name, tpt, Some(rhs.changeOwner(tree.symbol)))
        List(valdef)

