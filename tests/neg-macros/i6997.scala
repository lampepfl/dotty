
import scala.quoted.*
class Foo {
  def mcrImpl(body: Expr[Any])(using t: Type[_ <: Any])(using ctx: Quotes): Expr[Any] = '{
    val tmp = ???.asInstanceOf[t.Underlying] // error // error
    tmp // error
  }
}
