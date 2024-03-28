package a

import scala.deriving.Mirror

object App {
  val m = summon[Mirror.SumOf[a.A]]
  def size = compiletime.constValue[Tuple.Size[m.MirroredElemTypes]]

  @main def test =
    assert(size == 2, s"Expected size 2, got $size")
}
