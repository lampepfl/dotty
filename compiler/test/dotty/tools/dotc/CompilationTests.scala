package dotty
package tools
package dotc

import scala.language.unsafeNulls

import org.junit.{ Test, BeforeClass, AfterClass, Ignore }
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
import reporting.TestReporter
import vulpix._
import dotty.tools.dotc.config.ScalaSettings

class CompilationTests {
  import ParallelTesting._
  import TestConfiguration._
  import CompilationTests._
  import CompilationTest.aggregateTests

  // Positive tests ------------------------------------------------------------

  @Test def pos: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePos")
    var tests = List(
      compileFilesInDir("tests/pos", defaultOptions.and("-Ysafe-init", "-Wunused:all", "-Wshadow:private-shadow", "-Wshadow:type-parameter-shadow"), FileFilter.include(TestSources.posLintingAllowlist)),
      compileFilesInDir("tests/pos", defaultOptions.and("-Ysafe-init"), FileFilter.exclude(TestSources.posLintingAllowlist)),
      compileFilesInDir("tests/pos-deep-subtype", allowDeepSubtypes),
      compileFilesInDir("tests/pos-special/sourcepath/outer", defaultOptions.and("-sourcepath", "tests/pos-special/sourcepath")),
      compileFile("tests/pos-special/sourcepath/outer/nested/Test4.scala", defaultOptions.and("-sourcepath", "tests/pos-special/sourcepath")),
      compileFilesInDir("tests/pos-scala2", defaultOptions.and("-source", "3.0-migration")),
      compileFilesInDir("tests/pos-custom-args/captures", defaultOptions.and("-language:experimental.captureChecking")),
      compileFile("tests/pos-special/utf8encoded.scala", defaultOptions.and("-encoding", "UTF8")),
      compileFile("tests/pos-special/utf16encoded.scala", defaultOptions.and("-encoding", "UTF16")),
      compileDir("tests/pos-special/i18589", defaultOptions.and("-Ysafe-init").without("-Ycheck:all")),
      // Run tests for legacy lazy vals
      compileFilesInDir("tests/pos", defaultOptions.and("-Ysafe-init", "-Ylegacy-lazy-vals", "-Ycheck-constraint-deps"), FileFilter.include(TestSources.posLazyValsAllowlist)),
      compileDir("tests/pos-special/java-param-names", defaultOptions.withJavacOnlyOptions("-parameters")),
    ) ::: (
      // TODO create a folder for capture checking tests with the stdlib, or use tests/pos-custom-args/captures under this mode?
      if Properties.usingScalaLibraryCCTasty then List(compileDir("tests/pos-special/stdlib", allowDeepSubtypes))
      else Nil
    )

    if scala.util.Properties.isJavaAtLeast("16") then
      tests ::= compileFilesInDir("tests/pos-java16+", defaultOptions.and("-Ysafe-init"))

    aggregateTests(tests*).checkCompile()
  }

  @Test def rewrites: Unit = {
    implicit val testGroup: TestGroup = TestGroup("rewrites")

    aggregateTests(
      compileFile("tests/rewrites/rewrites.scala", defaultOptions.and("-source", "3.0-migration").and("-rewrite", "-indent")),
      compileFile("tests/rewrites/rewrites3x.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/rewrites3x-fatal-warnings.scala", defaultOptions.and("-rewrite", "-source", "future-migration", "-Xfatal-warnings")),
      compileFile("tests/rewrites/uninitialized-var.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/with-type-operator.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/private-this.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/alphanumeric-infix-operator.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/filtering-fors.scala", defaultOptions.and("-rewrite", "-source", "3.2-migration")),
      compileFile("tests/rewrites/refutable-pattern-bindings.scala", defaultOptions.and("-rewrite", "-source", "3.2-migration")),
      compileFile("tests/rewrites/i8982.scala", defaultOptions.and("-indent", "-rewrite")),
      compileFile("tests/rewrites/i9632.scala", defaultOptions.and("-indent", "-rewrite")),
      compileFile("tests/rewrites/i11895.scala", defaultOptions.and("-indent", "-rewrite")),
      compileFile("tests/rewrites/i12340.scala", unindentOptions.and("-rewrite")),
      compileFile("tests/rewrites/i17187.scala", unindentOptions.and("-rewrite")),
      compileFile("tests/rewrites/i17399.scala", unindentOptions.and("-rewrite")),
      compileFile("tests/rewrites/i20002.scala", defaultOptions.and("-indent", "-rewrite")),
    ).checkRewrites()
  }

  @Test def posTwice: Unit = {
    implicit val testGroup: TestGroup = TestGroup("posTwice")
    aggregateTests(
      compileFilesInDir("tests/pos-java-interop", defaultOptions),
      compileFilesInDir("tests/pos-java-interop-separate", defaultOptions),
      compileFile("tests/pos/t2168.scala", defaultOptions),
      compileFile("tests/pos/test-erasure.scala", defaultOptions),
      compileFile("tests/pos/Coder.scala", defaultOptions),
      compileFile("tests/pos/blockescapes.scala", defaultOptions),
      compileFile("tests/pos/functions1.scala", defaultOptions),
      compileFile("tests/pos/test-implicits1.scala", defaultOptions),
      compileFile("tests/pos/inferred.scala", defaultOptions),
      compileFile("tests/pos/selftypes.scala", defaultOptions),
      compileFile("tests/pos/varargs.scala", defaultOptions),
      compileFile("tests/pos/vararg-pattern.scala", defaultOptions),
      compileFile("tests/pos/opassign.scala", defaultOptions),
      compileFile("tests/pos/typedapply.scala", defaultOptions),
      compileFile("tests/pos/nameddefaults.scala", defaultOptions),
      compileFile("tests/pos/test-desugar.scala", defaultOptions),
      compileFile("tests/pos/sigs.scala", defaultOptions),
      compileFile("tests/pos/test-typers.scala", defaultOptions),
      compileDir("tests/pos/typedIdents", defaultOptions),
      compileFile("tests/pos/assignments.scala", defaultOptions),
      compileFile("tests/pos/packageobject.scala", defaultOptions),
      compileFile("tests/pos/overloaded.scala", defaultOptions),
      compileFile("tests/pos/overrides.scala", defaultOptions),
      compileDir("tests/pos/java-override", defaultOptions),
      compileFile("tests/pos/templateParents.scala", defaultOptions),
      compileFile("tests/pos/overloadedAccess.scala", defaultOptions),
      compileFile("tests/pos/approximateUnion.scala", defaultOptions),
      compileFilesInDir("tests/pos/tailcall", defaultOptions),
      compileShallowFilesInDir("tests/pos/pos_valueclasses", defaultOptions),
      compileFile("tests/pos/subtyping.scala", defaultOptions),
      compileFile("tests/pos/i0239.scala", defaultOptions),
      compileFile("tests/pos/anonClassSubtyping.scala", defaultOptions),
      compileFile("tests/pos/extmethods.scala", defaultOptions),
      compileFile("tests/pos/companions.scala", defaultOptions),
      compileFile("tests/pos/main.scala", defaultOptions)
    ).times(2).checkCompile()
  }

  // Warning tests ------------------------------------------------------------

  @Test def warn: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileWarn")
    aggregateTests(
      compileFilesInDir("tests/warn", defaultOptions),
    ).checkWarnings()
  }

  // Negative tests ------------------------------------------------------------

  @Test def negAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNeg")
    aggregateTests(
      compileFilesInDir("tests/neg", defaultOptions, FileFilter.exclude(TestSources.negScala2LibraryTastyBlacklisted)),
      compileFilesInDir("tests/neg-deep-subtype", allowDeepSubtypes),
      compileFilesInDir("tests/neg-custom-args/captures", defaultOptions.and("-language:experimental.captureChecking")),
      compileFile("tests/neg-custom-args/sourcepath/outer/nested/Test1.scala", defaultOptions.and("-sourcepath", "tests/neg-custom-args/sourcepath")),
      compileDir("tests/neg-custom-args/sourcepath2/hi", defaultOptions.and("-sourcepath", "tests/neg-custom-args/sourcepath2", "-Xfatal-warnings")),
      compileList("duplicate source", List(
        "tests/neg-custom-args/toplevel-samesource/S.scala",
        "tests/neg-custom-args/toplevel-samesource/nested/S.scala"),
        defaultOptions),
      compileFile("tests/neg/i7575.scala", defaultOptions.withoutLanguageFeatures.and("-language:_")),
    ).checkExpectedErrors()
  }

  @Test def fuzzyAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileFuzzy")
    compileFilesInDir("tests/fuzzy", defaultOptions).checkNoCrash()
  }

  // Run tests -----------------------------------------------------------------

  @Test def runAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runAll")
    aggregateTests(
      compileFilesInDir("tests/run", defaultOptions.and("-Ysafe-init")),
      compileFilesInDir("tests/run-deep-subtype", allowDeepSubtypes),
      compileFilesInDir("tests/run-custom-args/captures", allowDeepSubtypes.and("-language:experimental.captureChecking")),
      // Run tests for legacy lazy vals.
      compileFilesInDir("tests/run", defaultOptions.and("-Ysafe-init", "-Ylegacy-lazy-vals", "-Ycheck-constraint-deps"), FileFilter.include(TestSources.runLazyValsAllowlist)),
    ).checkRuns()
  }

  // Generic java signatures tests ---------------------------------------------

  @Test def genericJavaSignatures: Unit = {
    implicit val testGroup: TestGroup = TestGroup("genericJavaSignatures")
    compileFilesInDir("tests/generic-java-signatures", defaultOptions).checkRuns()
  }

  // Pickling Tests ------------------------------------------------------------

  @Test def pickling: Unit = {
    implicit val testGroup: TestGroup = TestGroup("testPickling")
    aggregateTests(
      compileFilesInDir("tests/pos", picklingOptions, FileFilter.exclude(TestSources.posTestPicklingBlacklisted)),
      compileFilesInDir("tests/run", picklingOptions, FileFilter.exclude(TestSources.runTestPicklingBlacklisted))
    ).checkCompile()
  }

  //@Test disabled in favor of posWithCompilerCC to save time.
  def recheck: Unit =
    given TestGroup = TestGroup("recheck")
    aggregateTests(
      compileFilesInDir("tests/run", defaultOptions.and("-Yrecheck-test"), FileFilter.exclude(TestSources.runTestRecheckExcluded))
      //Disabled to save some time.
      //compileFilesInDir("tests/pos", recheckOptions, FileFilter.exclude(TestSources.posTestRecheckExcluded)),
    ).checkCompile()

  // Explicit nulls tests
  @Test def explicitNullsNeg: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsNeg")
    aggregateTests(
      compileFilesInDir("tests/explicit-nulls/neg", defaultOptions and "-Yexplicit-nulls"),
      compileFilesInDir("tests/explicit-nulls/unsafe-common", defaultOptions and "-Yexplicit-nulls"),
    )
  }.checkExpectedErrors()

  @Test def explicitNullsPos: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsPos")
    aggregateTests(
      compileFilesInDir("tests/explicit-nulls/pos", defaultOptions and "-Yexplicit-nulls"),
      compileFilesInDir("tests/explicit-nulls/unsafe-common", defaultOptions and "-Yexplicit-nulls" and "-language:unsafeNulls"),
    )
  }.checkCompile()

  @Test def explicitNullsRun: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsRun")
    compileFilesInDir("tests/explicit-nulls/run", defaultOptions and "-Yexplicit-nulls")
  }.checkRuns()

  // initialization tests
  @Test def checkInitGlobal: Unit = {
    implicit val testGroup: TestGroup = TestGroup("checkInitGlobal")
    compileFilesInDir("tests/init-global/warn", defaultOptions.and("-Ysafe-init-global"), FileFilter.exclude(TestSources.negInitGlobalScala2LibraryTastyBlacklisted)).checkWarnings()
    compileFilesInDir("tests/init-global/pos", defaultOptions.and("-Ysafe-init-global", "-Xfatal-warnings"), FileFilter.exclude(TestSources.posInitGlobalScala2LibraryTastyBlacklisted)).checkCompile()
  }

  // initialization tests
  @Test def checkInit: Unit = {
    implicit val testGroup: TestGroup = TestGroup("checkInit")
    val options = defaultOptions.and("-Ysafe-init", "-Xfatal-warnings")
    compileFilesInDir("tests/init/neg", options).checkExpectedErrors()
    compileFilesInDir("tests/init/warn", defaultOptions.and("-Ysafe-init")).checkWarnings()
    compileFilesInDir("tests/init/pos", options).checkCompile()
    compileFilesInDir("tests/init/crash", options.without("-Xfatal-warnings")).checkCompile()
    // The regression test for i12128 has some atypical classpath requirements.
    // The test consists of three files: (a) Reflect_1  (b) Macro_2  (c) Test_3
    // which must be compiled separately. In addition:
    //   - the output from (a) must be on the classpath while compiling (b)
    //   - the output from (b) must be on the classpath while compiling (c)
    //   - the output from (a) _must not_ be on the classpath while compiling (c)
    locally {
      val i12128Group = TestGroup("checkInit/i12128")
      val i12128Options = options.without("-Xfatal-warnings")
      val outDir1 = defaultOutputDir + i12128Group + "/Reflect_1/i12128/Reflect_1"
      val outDir2 = defaultOutputDir + i12128Group + "/Macro_2/i12128/Macro_2"

      val tests = List(
        compileFile("tests/init/special/i12128/Reflect_1.scala", i12128Options)(i12128Group),
        compileFile("tests/init/special/i12128/Macro_2.scala", i12128Options.withClasspath(outDir1))(i12128Group),
        compileFile("tests/init/special/i12128/Test_3.scala", options.withClasspath(outDir2))(i12128Group)
      ).map(_.keepOutput.checkCompile())

      tests.foreach(_.delete())
    }
  }

  // parallel backend tests
  @Ignore("Temporarily disabled due to frequent timeouts")
  @Test def parallelBackend: Unit = {
    given TestGroup = TestGroup("parallelBackend")
    val parallelism = Runtime.getRuntime().availableProcessors().min(16)
    assumeTrue("Not enough available processors to run parallel tests", parallelism > 1)

    val options = defaultOptions.and(s"-Ybackend-parallelism:${parallelism}")
    def parCompileDir(directory: String) = compileDir(directory, options)

    // Compilation units containing more than 1 source file
    aggregateTests(
      parCompileDir("tests/pos/i10477"),
      parCompileDir("tests/pos/i4758"),
      parCompileDir("tests/pos/scala2traits"),
      parCompileDir("tests/pos/class-gadt"),
      parCompileDir("tests/pos/tailcall"),
      parCompileDir("tests/pos/reference"),
      parCompileDir("tests/pos/pos_valueclasses")
    ).checkCompile()

    aggregateTests(
      parCompileDir("tests/neg/package-implicit"),
      parCompileDir("tests/neg/package-export")
    ).checkExpectedErrors()

    aggregateTests(
      parCompileDir("tests/run/decorators"),
      parCompileDir("tests/run/generic")
    ).checkRuns()

  }
}

object CompilationTests extends ParallelTesting {
  // Test suite configuration --------------------------------------------------

  def maxDuration = 45.seconds
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
