abstract class FF
case class F1() extends FF
case class F2() extends FF

type Fs = List[F1] | List[F2]

object F1s { def unapply(xs: List[FF]): Option[List[F1]] = None }
object F2s { def unapply(xs: List[FF]): Option[List[F2]] = None }

class C1

final case class Pair[+T1, +T2](_1: T1, _2: T2) extends Product2[T1, T2]

class Test:
  def t1(i: Int) =
    val Pair(c1, fs1: Fs) = (i match // warn: the type test for Fs cannot be checked at runtime because its type arguments can't be determined from List[F1 | F2]
      case 1 => Pair(new C1, List(F1()))
      case _ => Pair(new C1, List(F2()))
    ): @unchecked
