import scala.quoted._

def test given QuoteContext = {

  val x: Int = 0

  '{
    given as QuoteContext = ???

    '{x + 1}  // error: wrong staging level

    '{(y: Expr[Int]) => $y }  // error: wrong staging level

  }

  '{x + 1}  // error: wrong staging level

  '{(y: Expr[Int]) => $y }  // error: wrong staging level

}
