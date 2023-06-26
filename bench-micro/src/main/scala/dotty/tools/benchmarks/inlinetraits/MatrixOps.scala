package dotty.tools.benchmarks.inlinetraits

import standard.IntMatrixLib.{Matrix => StdIntMatrix}
import specialized.IntMatrixLib.{Matrix => SpeIntMatrix}
import inlinetrait.IntMatrixLib.{Matrix => InlIntMatrix}

trait BenchmarkMatrix:
  def +(n: BenchmarkMatrix): BenchmarkMatrix
  def *(n: BenchmarkMatrix): BenchmarkMatrix

object BenchmarkMatrix:
  def ofType(tpe: String): Seq[Seq[Int]] => BenchmarkMatrix =
    (elems: Seq[Seq[Int]]) => tpe.toLowerCase() match {
      case "standard" => StdBenchmarkMatrix(StdIntMatrix(elems*))
      case "specialized" => SpeBenchmarkMatrix(SpeIntMatrix(elems*))
      case "inlinetrait" => InlBenchmarkMatrix(InlIntMatrix(elems*))
    }

private class StdBenchmarkMatrix(val m: StdIntMatrix) extends BenchmarkMatrix:
  override def +(n: BenchmarkMatrix): StdBenchmarkMatrix = n match {
    case stdN: StdBenchmarkMatrix => StdBenchmarkMatrix(this.m + stdN.m)
  }
  override def *(n: BenchmarkMatrix): StdBenchmarkMatrix = n match {
    case stdN: StdBenchmarkMatrix => StdBenchmarkMatrix(this.m * stdN.m)
  }

private class SpeBenchmarkMatrix(val m: SpeIntMatrix) extends BenchmarkMatrix:
  import specialized.IntMatrixLib

  override def +(n: BenchmarkMatrix): SpeBenchmarkMatrix = n match {
    case speN: SpeBenchmarkMatrix => SpeBenchmarkMatrix(IntMatrixLib.+(this.m)(speN.m))
  }
  override def *(n: BenchmarkMatrix): SpeBenchmarkMatrix = n match {
    case speN: SpeBenchmarkMatrix => SpeBenchmarkMatrix(IntMatrixLib.*(this.m)(speN.m))
  }

private class InlBenchmarkMatrix(val m: InlIntMatrix) extends BenchmarkMatrix:
  override def +(n: BenchmarkMatrix): InlBenchmarkMatrix = n match {
    case inlN: InlBenchmarkMatrix => InlBenchmarkMatrix(this.m + inlN.m)
  }
  override def *(n: BenchmarkMatrix): InlBenchmarkMatrix = n match {
    case inlN: InlBenchmarkMatrix => InlBenchmarkMatrix(this.m * inlN.m)
  }