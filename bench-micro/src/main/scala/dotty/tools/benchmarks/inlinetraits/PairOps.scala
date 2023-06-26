package dotty.tools.benchmarks.inlinetraits

import standard.{Pair => StdPair}
import specialized.{Pair => SpePair}
import inlinetrait.{
  Pair => InlPair,
  IntDoublePair => IDPair,
  CharShortPair => CSPair,
}

type First = Int | Char
type Second = Double | Short

class BenchmarkPair private (val _1: First, val _2: Second):
  def this(p: StdPair[First, Second]) = this(p._1, p._2)
  def this(p: SpePair[First, Second]) = this(p._1, p._2)
  def this(p: InlPair[First, Second]) = this(p._1, p._2)

object BenchmarkPair:
  def ofType(tpe: String): (First, Second) => BenchmarkPair =
    (_1: First, _2: Second) => tpe.toLowerCase() match {
      case "standard" => BenchmarkPair(StdPair(_1, _2))
      case "specialized" =>
        val concretePair: SpePair[First, Second] = (_1, _2) match {
          case (_1: Int, _2: Double) => SpePair(_1, _2)
          case (_1: Char, _2: Short) => SpePair(_1, _2)
          case _ => ???
        }
        BenchmarkPair(concretePair)
      case "inlinetrait" =>
        val concretePair: InlPair[First, Second] = (_1, _2) match {
          case (_1: Int, _2: Double) => IDPair(_1, _2)
          case (_1: Char, _2: Short) => CSPair(_1, _2)
          case _ => ???
        }
        BenchmarkPair(concretePair)
    }

  def unapply(p: BenchmarkPair): Option[(First, Second)] = Some(p._1, p._2)
