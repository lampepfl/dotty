package dotty
package tools
package dotc

import scala.language.unsafeNulls

import org.junit.{ Test, BeforeClass, AfterClass }
import org.junit.Assert._
import org.junit.Assume._
import org.junit.experimental.categories.Category

import java.io.File
import java.nio.file._
import java.util.stream.{ Stream => JStream }
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex
import scala.concurrent.duration._
import TestSources.sources
import vulpix._

class TastyBootstrapTests {
  import ParallelTesting._
  import TestConfiguration._
  import CompilationTests._
  import CompilationTest.aggregateTests

  /** The purpose of this test is three-fold, being able to compile dotty
   *  bootstrapped, and making sure that TASTY can link against a compiled
   *  version of Dotty, and compiling the compiler using the SemanticDB generation
   */
  @Test def tastyBootstrap: Unit = {
    implicit val testGroup: TestGroup = TestGroup("tastyBootstrap/tests")
    val libGroup = TestGroup("tastyBootstrap/lib")
    val tastyCoreGroup = TestGroup("tastyBootstrap/tastyCore")
    val dotty1Group = TestGroup("tastyBootstrap/dotty1")
    val dotty2Group = TestGroup("tastyBootstrap/dotty2")

    // Make sure that the directory is clean
    dotty.tools.io.Directory(defaultOutputDir + "tastyBootstrap").deleteRecursively()

    val opt = TestFlags(
      List(
        // compile with bootstrapped library on cp:
        defaultOutputDir + libGroup + "/lib/",
        // and bootstrapped tasty-core:
        defaultOutputDir + tastyCoreGroup + "/tastyCore/",
        // as well as bootstrapped compiler:
        defaultOutputDir + dotty1Group + "/dotty1/",
        // and the other compiler dependencies:
        Properties.compilerInterface, Properties.scalaLibrary, Properties.scalaAsm,
        Properties.dottyInterfaces, Properties.jlineTerminal, Properties.jlineReader,
      ).mkString(File.pathSeparator),
      Array("-Ycheck-reentrant", "-language:postfixOps", "-Xsemanticdb", "-Yexplicit-nulls")
    )

    val libraryDirs = List(Paths.get("library/src"), Paths.get("library/src-bootstrapped"))
    val librarySources = libraryDirs.flatMap(sources(_))

    val lib =
      compileList("lib", librarySources,
        defaultOptions.and("-Ycheck-reentrant",
          "-language:experimental.erasedDefinitions", // support declaration of scala.compiletime.erasedValue
          //  "-source", "future",  // TODO: re-enable once library uses updated syntax for vararg splices, wildcard imports, and import renaming
          ))(libGroup)

    val tastyCoreSources = sources(Paths.get("tasty/src"))
    val tastyCore = compileList("tastyCore", tastyCoreSources, opt)(tastyCoreGroup)

    val compilerSources = sources(Paths.get("compiler/src")) ++ sources(Paths.get("compiler/src-bootstrapped"))
    val compilerManagedSources = Properties.dottyCompilerManagedSources match
      case p if Files.isDirectory(p) => sources(p)
      case _                         => Nil

    val dotty1 = compileList("dotty1", compilerSources ++ compilerManagedSources, opt)(dotty1Group)
    val dotty2 = compileList("dotty2", compilerSources ++ compilerManagedSources, opt)(dotty2Group)

    val tests = {
      lib.keepOutput :: tastyCore.keepOutput :: dotty1.keepOutput :: aggregateTests(
        dotty2,
        compileShallowFilesInDir("compiler/src/dotty/tools", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/ast", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/config", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/parsing", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/printing", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/reporting", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/rewrites", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/transform", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/typer", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/util", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/backend", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/backend/jvm", opt),
        compileList("managed-sources", compilerManagedSources, opt)
      ).keepOutput :: Nil
    }.map(_.checkCompile())

    def assertExists(path: String) = assertTrue(Files.exists(Paths.get(path)))
    assertExists(s"out/$libGroup/lib/")
    assertExists(s"out/$tastyCoreGroup/tastyCore/")
    assertExists(s"out/$dotty1Group/dotty1/")
    assertExists(s"out/$dotty2Group/dotty2/")
    compileList("idempotency", List("tests/idempotency/BootstrapChecker.scala", "tests/idempotency/IdempotencyCheck.scala"), defaultOptions).checkRuns()

    tests.foreach(_.delete())
  }
}

object TastyBootstrapTests extends ParallelTesting {
  // Test suite configuration --------------------------------------------------

  def maxDuration = 45.seconds
  def numberOfSlaves = Runtime.getRuntime.availableProcessors()
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile

  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def tearDown(): Unit = {
    super.cleanup()
    summaryReport.echoSummary()
  }
}
