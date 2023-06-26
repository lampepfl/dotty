package dotty.tools.benchmarks.inlinetraits
package specialized

/*
 * This implementation relies on the @specialized tag to specialize Pair.
 * However, @specialized does nothing in Scala 3. Therefore, an equivalent version is
 * recreated by hand further down, and the actual Scala 2 code is provided below:
 *
 *   import scala.specialized
 *
 *   class Pair[@specialized(Int, Char) +T1, @specialized(Double, Short) +T2](_1: T1, _2: T2) {}
 */

class Pair[+T1, +T2] protected (val _1: T1, val _2: T2)

object Pair {
  def apply(_1: Int, _2: Double): Pair[Int, Double] = new Pair$mcID$sp(_1, _2)
  def apply(_1: Int, _2: Short): Pair[Int, Short] = new Pair$mcIS$sp(_1, _2)
  def apply(_1: Char, _2: Double): Pair[Char, Double] = new Pair$mcCD$sp(_1, _2)
  def apply(_1: Char, _2: Short): Pair[Char, Short] = new Pair$mcCS$sp(_1, _2)
}

private class Pair$mcID$sp(protected[this] val _1$mcI$sp: Int, protected[this] val _2$mcD$sp: Double) extends Pair[Int, Double](_1$mcI$sp, _2$mcD$sp)
private class Pair$mcIS$sp(protected[this] val _1$mcI$sp: Int, protected[this] val _2$mcS$sp: Short) extends Pair[Int, Short](_1$mcI$sp, _2$mcS$sp)
private class Pair$mcCD$sp(protected[this] val _1$mcC$sp: Char, protected[this] val _2$mcD$sp: Double) extends Pair[Char, Double](_1$mcC$sp, _2$mcD$sp)
private class Pair$mcCS$sp(protected[this] val _1$mcC$sp: Char, protected[this] val _2$mcS$sp: Short) extends Pair[Char, Short](_1$mcC$sp, _2$mcS$sp)
