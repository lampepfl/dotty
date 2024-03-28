import sbt.internal.inc.Analysis
import complete.DefaultParsers._

ThisBuild / usePipelining := true

// Reset compiler iterations, necessary because tests run in batch mode
val recordPreviousIterations = taskKey[Unit]("Record previous iterations.")
recordPreviousIterations := {
  val log = streams.value.log
  CompileState.previousIterations = {
    val previousAnalysis = (previousCompile in Compile).value.analysis.asScala
    previousAnalysis match {
      case None =>
        log.info("No previous analysis detected")
        0
      case Some(a: Analysis) => a.compilations.allCompilations.size
    }
  }
}

val checkIterations = inputKey[Unit]("Verifies the accumulated number of iterations of incremental compilation.")

checkIterations := {
  val expected: Int = (Space ~> NatBasic).parsed
  val actual: Int = ((compile in Compile).value match { case a: Analysis => a.compilations.allCompilations.size }) - CompileState.previousIterations
  assert(expected == actual, s"Expected $expected compilations, got $actual (previous: ${CompileState.previousIterations})")
}
