//> using options -language:experimental.modularity -source future

trait FromString:
  type Self
  def fromString(s: String): Self

given Int is FromString = _.toInt

given Double is FromString = _.toDouble

def add[N: {FromString, Numeric}](a: String, b: String): N =
  val num = summon[Numeric[N]]
  num.plus(N.fromString(a), N.fromString(b))
