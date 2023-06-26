package dotty.tools.benchmarks.inlinetraits
package inlinetrait

inline trait Pair[+T1, +T2]:
  val _1: T1
  val _2: T2

class IntDoublePair(val _1: Int, val _2: Double) extends Pair[Int, Double]
class CharShortPair(val _1: Char, val _2: Short) extends Pair[Char, Short]