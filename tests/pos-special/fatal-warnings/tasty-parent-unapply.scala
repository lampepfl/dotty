import scala.quoted._

object Macros {


  def impl(using QuoteContext): Unit = {
    import reflect._

    def foo(tree: Tree, term: Term, typeTree: TypeTree, parent: Tree) = {

      tree match {
        case tree: Tree =>
      }

      term match {
        case term: Term =>
      }

      typeTree match {
        case typeTree: TypeTree =>
      }

      parent match {
        case typeTree: Term =>
        case typeTree: TypeTree =>
      }

    }
  }

}
