import scala.deriving.Mirror

object C:
  def constructR2: R2 =
    val mirror = summon[Mirror.ProductOf[R2]]
    mirror.fromTuple((3, "asd"))
