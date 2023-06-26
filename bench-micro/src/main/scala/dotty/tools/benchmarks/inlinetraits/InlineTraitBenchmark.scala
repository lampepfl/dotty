package dotty.tools.benchmarks.inlinetraits

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit.{SECONDS, MILLISECONDS}
import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(3)
@Warmup(iterations = 3, time = 3, timeUnit = SECONDS)
@Measurement(iterations = 5, time = 5, timeUnit = SECONDS)
@OutputTimeUnit(MILLISECONDS)
@State(Scope.Benchmark)
class InlineTraitBenchmark {
  var matrixSize: Int = 300

  var numPairs: Int = 3_000_000

  def intMatrixElems: List[List[Int]] =
    List.tabulate(matrixSize, matrixSize)((_, _) => Random.nextInt())

  def pairElems: List[(First, Second)] = List.tabulate(numPairs)(_ % 2 match {
    case 0 => (Random.nextInt(), Random.nextDouble())
    case 1 => (Random.nextInt(Char.MaxValue).asInstanceOf[Char], Random.nextInt(Short.MaxValue).asInstanceOf[Short])
  })

  @Param(Array("standard", "specialized", "inlinetrait"))
  var libType: String = _

  var m1: BenchmarkMatrix = _
  var m2: BenchmarkMatrix = _

  var pairs: List[BenchmarkPair] = _

  @Setup(Level.Trial)
  def setup = {
    Random.setSeed(matrixSize)

    val matrixFactory = BenchmarkMatrix.ofType(libType)
    m1 = matrixFactory(intMatrixElems)
    m2 = matrixFactory(intMatrixElems)

    val pairFactory = (l: List[(First, Second)]) => l.map((_1, _2) => BenchmarkPair.ofType(libType)(_1, _2))
    pairs = pairFactory(pairElems)
  }

  @Benchmark
  def matrixBenchmark = (m1 + m2) * m1 // O(n^3) loops

  @Benchmark
  def pairsBenchmark = pairs.foldLeft(0){ case (sum, pair) => pair match {
      case BenchmarkPair(i: Int, d: Double) => 7 * i + 3 * d.toInt + sum
      case BenchmarkPair(c: Char, s: Short) => 5 * c + 2 * s + sum
    }
  }
}
