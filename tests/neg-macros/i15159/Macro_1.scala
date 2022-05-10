import scala.quoted.*
object TestMacro:
  inline def test[T]: Unit = ${ testImpl[T] }
  def testImpl[T: Type](using Quotes): Expr[Unit] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    tpe.typeSymbol.children.map { childSymbol =>
      tpe.memberType(childSymbol) // not a member of tpe
    }
    '{ () }
