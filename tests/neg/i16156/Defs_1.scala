//import quotes.reflect._

trait MyEncoder[T]:
  def encode: String
class Context:
  inline def summonMyEncoder[T]: String =
    ${ SummonEncoder.impl[T] }
  implicit val encoderInstance: MyEncoder[String] =
    new MyEncoder[String] { def encode = "blah" }
end Context

import scala.quoted._

object SummonEncoder:
  def impl[T: Type](using Quotes) =
    import quotes.reflect._
    Expr.summon[MyEncoder[T]] match
      case Some(enc) => '{ $enc.encode }
      case None      => report.throwError("can't do it")

class Repo[T]:
  val ctx = new Context
  inline def summonEncoder = { import ctx._ // change to: import ctx.{given, _} for the given example
    ctx.summonMyEncoder[T]
  }
