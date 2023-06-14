package dotty.tools.benchmarks.inlinetraits

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit.{SECONDS, MILLISECONDS}
import scala.util.Random

// @BenchmarkMode(Array(Mode.SampleTime))
@Fork(3)
@Threads(3)
@Warmup(iterations = 5, time = 10, timeUnit = SECONDS)
@Measurement(iterations = 10, time = 10, timeUnit = SECONDS)
// @OutputTimeUnit(MILLISECONDS)
@State(Scope.Benchmark)
class Boxing {
  class Wrapper[T](val x: T)
  class IntWrapper(val x: Int)

  val numbers = 1 to 1000000
  val ws = numbers.map(Wrapper(_))
  val iws = numbers.map(IntWrapper(_))

  @Benchmark
  def noUnboxing: Int = iws.map(w => IntWrapper(w.x*w.x + 2*w.x)).foldLeft(0)(_ + _.x)

  @Benchmark
  def withUnboxing: Int = ws.map(w => Wrapper(w.x*w.x + 2*w.x)).foldLeft(0)(_ + _.x)
}
