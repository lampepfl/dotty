import scala.deriving._
import scala.quoted._
import scala.quoted.matching._

trait Eq[T] {
  def eqv(x: T, y: T): Boolean
}

object Eq {
  given Eq[String] {
    def eqv(x: String, y: String) = x == y
  }

  given Eq[Int] {
    def eqv(x: Int, y: Int) = x == y
  }

  def eqProduct[T](body: (T, T) => Boolean): Eq[T] =
    new Eq[T] {
      def eqv(x: T, y: T): Boolean = body(x, y)
    }

  def eqSum[T](body: (T, T) => Boolean): Eq[T] =
    new Eq[T] {
      def eqv(x: T, y: T): Boolean = body(x, y)
    }

  def summonAll[T](t: Type[T])(given qctx: QuoteContext): List[Expr[Eq[_]]] = t match {
    case '[$tpe *: $tpes] =>
      val eqInstance = summonExpr(given '[Eq[$tpe]]) match {
        case Some(ev) => ev
        case None => derived(given tpe, qctx)
      }
      eqInstance :: summonAll(tpes)
    case '[Unit] => Nil
  }

  given derived[T: Type](given qctx: QuoteContext): Expr[Eq[T]] = {
    import qctx.tasty.{_, given}

    val ev: Expr[Mirror.Of[T]] = summonExpr(given '[Mirror.Of[T]]).get

    ev match {
      case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = $elementTypes }} =>
        val elemInstances = summonAll(elementTypes)
        val eqProductBody: (Expr[T], Expr[T]) => Expr[Boolean] = (x, y) => {
          elemInstances.zipWithIndex.foldLeft(Expr(true: Boolean)) {
            case (acc, (elem, index)) =>
              val e1 = '{$x.asInstanceOf[Product].productElement(${Expr(index)})}
              val e2 = '{$y.asInstanceOf[Product].productElement(${Expr(index)})}

              '{ $acc && $elem.asInstanceOf[Eq[Any]].eqv($e1, $e2) }
          }
        }
        '{
          eqProduct((x: T, y: T) => ${eqProductBody('x, 'y)})
        }

      case '{ $m: Mirror.SumOf[T] { type MirroredElemTypes = $elementTypes }} =>
        val elemInstances = summonAll(elementTypes)
        val eqSumBody: (Expr[T], Expr[T]) => Expr[Boolean] = (x, y) => {
          val ordx = '{ $m.ordinal($x) }
          val ordy = '{ $m.ordinal($y) }

          val elements = Expr.ofList(elemInstances)
          '{
              $ordx == $ordy && $elements($ordx).asInstanceOf[Eq[Any]].eqv($x, $y)
          }
        }

        '{
          eqSum((x: T, y: T) => ${eqSumBody('x, 'y)})
        }
    }
  }
}

object Macro3 {
  inline def [T](x: =>T) === (y: =>T)(given eq: Eq[T]): Boolean = eq.eqv(x, y)

  implicit inline def eqGen[T]: Eq[T] = ${ Eq.derived[T] }
}