import scala.quoted.*

enum E:
  case A, B

inline def showEnumChildren = ${ showEnumChildrenExpr }

def showEnumChildrenExpr(using Quotes) =
  import quotes.reflect.*
  val repr = TypeRepr.of[E.type] // we know that all the implementations of the class E are in the object E
  Expr(TypeRepr.of[E].classSymbol.get.children.map(sym => (sym.name, repr.memberType(sym).show)))

