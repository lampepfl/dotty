import scala.quoted.*

object Macro:
  inline def generateCode: Unit = ${ testThisPaths }

  def testThisPaths(using Quotes): Expr[Unit] =
    '{
      trait E extends G:
        type V
        val f: F
        ${ val expr = '{ println(this) }; expr }
        ${ val expr = '{ println(f) }; expr }
        ${ val expr = '{ println(this.f) }; expr }
        ${ val expr = '{ println(??? : this.type) }; expr }
        ${ val expr = '{ println(??? : V) }; expr }
        ${ val expr = '{ println(??? : this.V) }; expr }
        ${ val expr = '{ println(??? : this.f.V) }; expr }
        ${ val expr = '{ println(??? : this.f.type) }; expr }
        ${
          val expr = '{
            println(this)
            println(f)
            println(this.f)
            println(??? : this.type)
            println(??? : V)
            println(??? : this.V)
            println(??? : this.f.V)
            println(??? : this.f.type)
          }
          expr
        }
      trait F:
        type V
    }

trait G:
  val f: Any
