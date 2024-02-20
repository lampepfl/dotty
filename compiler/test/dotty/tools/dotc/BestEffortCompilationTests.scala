package dotty
package tools
package dotc

import scala.concurrent.duration._
import dotty.tools.vulpix._
import org.junit.{ Test, AfterClass }
import reporting.TestReporter
import java.io.{File => JFile}

import scala.language.unsafeNulls

class BestEffortCompilationTests {
  import ParallelTesting._
  import vulpix.TestConfiguration._
  import BestEffortCompilationTests._
  import CompilationTest.aggregateTests

  // Since TASTy and beTASTy files are read in a lazy manner (only when referenced by the source .scala file)
  // we test by using the "-from-tasty" option. This guarantees that the tasty files will be read
  // (and that the Best Effort TASTy reader will be tested), but we unfortunately skip the useful
  // interactions a tree derived from beTASTy could have with other frontend phases.
  @Test def negTestFromBestEffortTasty: Unit = {
    // Can be reproduced with
    // > sbt
    // > scalac --Ybest-effort -Xsemanticdb <source>
    // > scalac --from-tasty -Ywith-best-effort-tasty META_INF/best-effort/<betasty>

    implicit val testGroup: TestGroup = TestGroup("negTestFromBestEffortTasty")
    compileBestEffortTastyInDir(s"tests${JFile.separator}neg", bestEffortBaselineOptions,
      picklingFilter = FileFilter.exclude(TestSources.negBestEffortPicklingBlacklisted),
      unpicklingFilter = FileFilter.exclude(TestSources.negBestEffortUnpicklingBlacklisted)
    ).checkNoCrash()
  }

  // Tests an actual use case of this compilation mode, where symbol definitions of the downstream
  // projects depend on the best effort tasty files generated with the Best Effort dir option
  @Test def bestEffortIntergrationTest: Unit = {
    implicit val testGroup: TestGroup = TestGroup("bestEffortIntegrationTests")
    compileBestEffortIntegration(s"tests${JFile.separator}best-effort", bestEffortBaselineOptions)
      .noCrashWithCompilingDependencies()
  }
}

object BestEffortCompilationTests extends ParallelTesting {
  def maxDuration = 45.seconds
  def numberOfSlaves = Runtime.getRuntime.availableProcessors()
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests

  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def tearDown(): Unit = {
    super.cleanup()
    summaryReport.echoSummary()
  }
}
