package dotty.tools.dotc.coverage

import org.junit.Test
import org.junit.AfterClass
import org.junit.Assert.*
import org.junit.Assume.*
import org.junit.experimental.categories.Category
import dotty.{BootstrappedOnlyTests, Properties}
import dotty.tools.vulpix.*
import dotty.tools.vulpix.TestConfiguration.*
import dotty.tools.dotc.Main
import dotty.tools.dotc.reporting.TestReporter

import java.nio.file.{FileSystems, Files, Path, Paths, StandardCopyOption}
import scala.jdk.CollectionConverters.*
import scala.util.Properties.userDir
import scala.language.unsafeNulls
import scala.collection.mutable.Buffer
import dotty.tools.dotc.util.DiffUtil

import java.util.stream.Collectors

@Category(Array(classOf[BootstrappedOnlyTests]))
class CoverageTests:
  import CoverageTests.{*, given}

  private val scalaFile = FileSystems.getDefault.getPathMatcher("glob:**.scala")
  private val rootSrc = Paths.get(userDir, "tests", "coverage")
  
  @Test
  def checkCoverageStatements(): Unit =
    assumeFalse(
      "FIXME: test output differs when using Scala 2 library TASTy",
      Properties.usingScalaLibraryTasty
    )
    checkCoverageIn(rootSrc.resolve("pos"), false)

  @Test
  def checkInstrumentedRuns(): Unit =
    assumeFalse(
      "FIXME: test output differs when using Scala 2 library TASTy",
      Properties.usingScalaLibraryTasty
    )
    checkCoverageIn(rootSrc.resolve("run"), true)

  def checkCoverageIn(dir: Path, run: Boolean)(using TestGroup): Unit =
    /** Converts \\ (escaped \) to / on windows, to make the tests pass without changing the serialization. */
    def fixWindowsPaths(lines: Buffer[String]): Buffer[String] =
      val separator = java.io.File.separatorChar
      if separator == '\\' then
        val escapedSep = "\\\\"
        lines.map(_.replace(escapedSep, "/"))
      else
        lines
    end fixWindowsPaths

    def runOnFile(p: Path): Boolean =
      scalaFile.matches(p) &&
      (Properties.testsFilter.isEmpty || Properties.testsFilter.exists(p.toString.contains))

    Files.walk(dir).filter(runOnFile).forEach(path => {
      val fileName = path.getFileName.toString.stripSuffix(".scala")
      val targetDir = computeCoverageInTmp(path, dir, run)
      val targetFile = targetDir.resolve(s"scoverage.coverage")
      val expectFile = path.resolveSibling(s"$fileName.scoverage.check")
      if updateCheckFiles then
        Files.copy(targetFile, expectFile, StandardCopyOption.REPLACE_EXISTING)
      else
        val expected = fixWindowsPaths(Files.readAllLines(expectFile).asScala)
        val obtained = fixWindowsPaths(Files.readAllLines(targetFile).asScala)
        if expected != obtained then
          val instructions = FileDiff.diffMessage(expectFile.toString, targetFile.toString)
          fail(s"Coverage report differs from expected data.\n$instructions")

      // measurement files only exist in the "run" category
      // as these are generated at runtime by the scala.runtime.coverage.Invoker
      val expectMeasurementFile = path.resolveSibling(s"$fileName.measurement.check")
      if run && Files.exists(expectMeasurementFile) then

        // Note that this assumes that the test invoked was single threaded,
        // if that is not the case then this will have to be adjusted
        val targetMeasurementFile = findMeasurementFile(targetDir)

        if updateCheckFiles then
          Files.copy(targetMeasurementFile, expectMeasurementFile, StandardCopyOption.REPLACE_EXISTING)

        else
          val targetMeasurementFile = findMeasurementFile(targetDir)
          val expectedMeasurements = fixWindowsPaths(Files.readAllLines(expectMeasurementFile).asScala)
          val obtainedMeasurements = fixWindowsPaths(Files.readAllLines(targetMeasurementFile).asScala)
          if expectedMeasurements != obtainedMeasurements then
            val instructions = FileDiff.diffMessage(expectMeasurementFile.toString, targetMeasurementFile.toString)
            fail(s"Measurement report differs from expected data.\n$instructions")
      ()
    })

  /** Generates the coverage report for the given input file, in a temporary directory. */
  def computeCoverageInTmp(inputFile: Path, sourceRoot: Path, run: Boolean)(using TestGroup): Path =
    val target = Files.createTempDirectory("coverage")
    val options = defaultOptions.and("-Ycheck:instrumentCoverage", "-coverage-out", target.toString, "-sourceroot", sourceRoot.toString)
    if run then
      val test = compileDir(inputFile.getParent.toString, options)
      test.checkRuns()
    else
      val test = compileFile(inputFile.toString, options)
      test.checkCompile()
    target

  private def findMeasurementFile(targetDir: Path): Path = {
    val allFilesInTarget = Files.list(targetDir).collect(Collectors.toList).asScala
    allFilesInTarget.filter(_.getFileName.toString.startsWith("scoverage.measurements.")).headOption.getOrElse(
      throw new AssertionError(s"Expected to find measurement file in targetDir [${targetDir}] but none were found.")
    )
  }


object CoverageTests extends ParallelTesting:
  import scala.concurrent.duration.*

  def maxDuration = 30.seconds
  def numberOfSlaves = 1

  def safeMode = Properties.testsSafeMode
  def testFilter = Properties.testsFilter
  def isInteractive = SummaryReport.isInteractive
  def updateCheckFiles = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests

  given summaryReport: SummaryReporting = SummaryReport()
  @AfterClass def tearDown(): Unit =
    super.cleanup()
    summaryReport.echoSummary()

  given TestGroup = TestGroup("instrumentCoverage")
