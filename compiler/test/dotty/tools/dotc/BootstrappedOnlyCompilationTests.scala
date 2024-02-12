package dotty
package tools
package dotc

import scala.language.unsafeNulls

import org.junit.{ Test, BeforeClass, AfterClass }
import org.junit.Assert._
import org.junit.Assume._
import org.junit.experimental.categories.Category

import scala.concurrent.duration._
import reporting.TestReporter
import vulpix._

import java.nio.file._

@Category(Array(classOf[BootstrappedOnlyTests]))
class BootstrappedOnlyCompilationTests {
  import ParallelTesting._
  import TestConfiguration._
  import BootstrappedOnlyCompilationTests._
  import CompilationTest.aggregateTests

  // Positive tests ------------------------------------------------------------

  @Test def posMacros: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePosMacros")
    aggregateTests(
      compileFilesInDir("tests/bench", defaultOptions.without("-Yno-deep-subtypes")),
      compileFilesInDir("tests/pos-macros", defaultOptions.and("-Xcheck-macros", "-Yretain-trees")),
    ).checkCompile()
  }

  // @Test
  def posWithCompilerCC: Unit =
    implicit val testGroup: TestGroup = TestGroup("compilePosWithCompilerCC")
    aggregateTests(
      compileDir("tests/pos-with-compiler-cc/dotc", withCompilerOptions.and("-language:experimental.captureChecking"))
    ).checkCompile()

  @Test def posWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePosWithCompiler")
    aggregateTests(
      compileFilesInDir("tests/pos-with-compiler", withCompilerOptions),
      compileFilesInDir("tests/pos-staging", withStagingOptions),
      compileDir("compiler/src/dotty/tools/dotc/ast", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/config", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/transform", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/parsing", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/printing", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/reporting", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/typer", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/util", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/io", withCompilerOptions),
      compileDir("tasty/src/dotty/tools/tasty", withCompilerOptions),
      compileList(
        "testIssue6460",
        List(
          "compiler/src/dotty/tools/dotc/core/SymbolLoaders.scala",
          "compiler/src/dotty/tools/dotc/core/Types.scala"
        ),
        withCompilerOptions
      ),
      compileList(
        "testIssue6603",
        List(
          "compiler/src/dotty/tools/dotc/ast/Desugar.scala",
          "compiler/src/dotty/tools/dotc/ast/Trees.scala",
          "compiler/src/dotty/tools/dotc/core/Types.scala"
        ),
        withCompilerOptions
      ),
    ).checkCompile()
  }

  @Test def posTwiceWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("posTwiceWithCompiler")
    aggregateTests(
      compileFile("tests/pos-with-compiler/Labels.scala", withCompilerOptions),
      compileFile("tests/pos-with-compiler/Patterns.scala", withCompilerOptions),
      compileList(
        "testNonCyclic",
        List(
          "compiler/src/dotty/tools/dotc/CompilationUnit.scala",
          "compiler/src/dotty/tools/dotc/core/Types.scala",
          "compiler/src/dotty/tools/dotc/ast/Trees.scala"
        ),
        withCompilerOptions
      ),
      compileList(
        "testIssue34",
        List(
          "compiler/src/dotty/tools/dotc/config/Properties.scala",
          "compiler/src/dotty/tools/dotc/config/PathResolver.scala"
        ),
        withCompilerOptions
      )
    ).times(2).checkCompile()
  }

  // Negative tests ------------------------------------------------------------

  @Test def negMacros: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNegWithCompiler")
    compileFilesInDir("tests/neg-macros", defaultOptions.and("-Xcheck-macros", "-Yretain-trees"))
      .checkExpectedErrors()
  }

  @Test def negWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNegWithCompiler")
    aggregateTests(
      compileFilesInDir("tests/neg-with-compiler", withCompilerOptions),
      compileFilesInDir("tests/neg-staging", withStagingOptions),
    ).checkExpectedErrors()
  }

  // Run tests -----------------------------------------------------------------

  @Test def runMacros: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runMacros")
    compileFilesInDir("tests/run-macros", defaultOptions.and("-Xcheck-macros", "-Yretain-trees"), FileFilter.exclude(TestSources.runMacrosScala2LibraryTastyBlacklisted))
      .checkRuns()
  }

  @Test def runWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runWithCompiler")
    val basicTests = List(
      compileFilesInDir("tests/run-with-compiler", withCompilerOptions),
      compileFilesInDir("tests/run-staging", withStagingOptions),
      compileFilesInDir("tests/run-tasty-inspector", withTastyInspectorOptions)
    )
    val tests =
      if scala.util.Properties.isWin then basicTests
      else compileDir("tests/old-tasty-interpreter-prototype", withTastyInspectorOptions) :: basicTests

    aggregateTests(tests*).checkRuns()
  }

  @Test def runBootstrappedOnly: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runBootstrappedOnly")
    aggregateTests(
      compileFilesInDir("tests/run-bootstrapped", withCompilerOptions),
    ).checkRuns()
  }

  // Pickling Tests ------------------------------------------------------------
  //
  // Pickling tests are very memory intensive and as such need to be run with a
  // lower level of concurrency as to not kill their running VMs

  @Test def picklingWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("testPicklingWithCompiler")
    aggregateTests(
      compileDir("compiler/src/dotty/tools", picklingWithCompilerOptions, recursive = false),
      compileDir("compiler/src/dotty/tools/dotc", picklingWithCompilerOptions, recursive = false),
      compileDir("library/src/scala/runtime/function", picklingWithCompilerOptions),
      compileFilesInDir("library/src/scala/runtime", picklingWithCompilerOptions),
      compileFilesInDir("compiler/src/dotty/tools/backend/jvm", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/ast", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core", picklingWithCompilerOptions, recursive = false),
      compileDir("compiler/src/dotty/tools/dotc/config", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/parsing", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/printing", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/repl", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/rewrites", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/transform", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/typer", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/util", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/io", picklingWithCompilerOptions),
      compileFile("tests/pos/pickleinf.scala", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core/classfile", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core/tasty", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core/unpickleScala2", picklingWithCompilerOptions),
      compileDir("tasty/src/dotty/tools/tasty", picklingWithCompilerOptions)
    ).limitThreads(4).checkCompile()
  }

  @Test def testPlugins: Unit = {
    implicit val testGroup: TestGroup = TestGroup("testPlugins")
    val pluginFile = "plugin.properties"

    // 1. hack with absolute path for -Xplugin
    // 2. copy `pluginFile` to destination
    def compileFilesInDir(dir: String): CompilationTest = {
      val outDir = defaultOutputDir + "testPlugins/"
      val sourceDir = new java.io.File(dir)

      val dirs = sourceDir.listFiles.toList.filter(_.isDirectory)
      val targets = dirs.map { dir =>
        val compileDir = createOutputDirsForDir(dir, sourceDir, outDir)
        Files.copy(dir.toPath.resolve(pluginFile), compileDir.toPath.resolve(pluginFile), StandardCopyOption.REPLACE_EXISTING)
        val flags = TestFlags(withCompilerClasspath, noCheckOptions).and("-Xplugin:" + compileDir.getAbsolutePath)
        SeparateCompilationSource("testPlugins", dir, flags, compileDir)
      }

      new CompilationTest(targets)
    }

    compileFilesInDir("tests/plugins/neg").checkExpectedErrors()
    compileDir("tests/plugins/custom/analyzer", withCompilerOptions.and("-Yretain-trees")).checkCompile()
  }
}

object BootstrappedOnlyCompilationTests extends ParallelTesting {
  // Test suite configuration --------------------------------------------------

  def maxDuration = 60.seconds
  def numberOfSlaves = Runtime.getRuntime().availableProcessors()
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
