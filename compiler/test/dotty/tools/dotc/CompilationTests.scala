package dotty
package tools
package dotc

import org.junit.{ Test, BeforeClass, AfterClass }
import org.junit.Assert._
import org.junit.Assume._
import org.junit.experimental.categories.Category

import java.io.File
import java.nio.file._
import java.util.stream.{ Stream => JStream }
import scala.collection.JavaConverters._
import scala.util.matching.Regex
import scala.concurrent.duration._
import TestSources.sources
import vulpix._

class CompilationTests {
  import ParallelTesting._
  import TestConfiguration._
  import CompilationTests._
  import CompilationTest.aggregateTests

  // Positive tests ------------------------------------------------------------

  @Test def pos: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePos")
    aggregateTests(
      compileFile("tests/pos-special/utf8encoded.scala", defaultOptions.and("-encoding", "UTF8")),
      compileFile("tests/pos-special/utf16encoded.scala", defaultOptions.and("-encoding", "UTF16")),
      compileFilesInDir("tests/pos-special/sourcepath/outer", defaultOptions.and("-sourcepath", "tests/pos-special/sourcepath")),
      compileFile("tests/pos-special/sourcepath/outer/nested/Test4.scala", defaultOptions.and("-sourcepath", "tests/pos-special/sourcepath")),
      compileFilesInDir("tests/pos", defaultOptions.and("-Ysafe-init")),
      compileDir("tests/pos-special/java-param-names", defaultOptions.withJavacOnlyOptions("-parameters")),
    ).checkCompile()
  }

  @Test def rewrites: Unit = {
    implicit val testGroup: TestGroup = TestGroup("rewrites")

    aggregateTests(
      compileFile("tests/rewrites/rewrites.scala", scala2CompatMode.and("-indent")),
      compileFile("tests/rewrites/rewrites3x.scala", defaultOptions.and("-source", "future-migration")),
      compileFile("tests/rewrites/i8982.scala"),
      compileFile("tests/rewrites/i9632.scala"),
      compileFile("tests/rewrites/i11895.scala"),
      compileFile("tests/rewrites/i12340.scala", defaultOptions.and("-no-indent").without("-indent", "-language:postfixOps")),
    ).checkRewrites()
  }

  @Test def posTwice: Unit = {
    implicit val testGroup: TestGroup = TestGroup("posTwice")
    aggregateTests(
      compileFilesInDir("tests/pos-java-interop"),
      compileFilesInDir("tests/pos-java-interop-separate"),
      compileFile("tests/pos/t2168.scala"),
      compileFile("tests/pos/test-erasure.scala"),
      compileFile("tests/pos/Coder.scala"),
      compileFile("tests/pos/blockescapes.scala"),
      compileFile("tests/pos/functions1.scala"),
      compileFile("tests/pos/test-implicits1.scala"),
      compileFile("tests/pos/inferred.scala"),
      compileFile("tests/pos/selftypes.scala"),
      compileFile("tests/pos/varargs.scala"),
      compileFile("tests/pos/vararg-pattern.scala"),
      compileFile("tests/pos/opassign.scala"),
      compileFile("tests/pos/typedapply.scala"),
      compileFile("tests/pos/nameddefaults.scala"),
      compileFile("tests/pos/test-desugar.scala"),
      compileFile("tests/pos/sigs.scala"),
      compileFile("tests/pos/test-typers.scala"),
      compileDir("tests/pos/typedIdents"),
      compileFile("tests/pos/assignments.scala"),
      compileFile("tests/pos/packageobject.scala"),
      compileFile("tests/pos/overloaded.scala"),
      compileFile("tests/pos/overrides.scala"),
      compileDir("tests/pos/java-override"),
      compileFile("tests/pos/templateParents.scala"),
      compileFile("tests/pos/overloadedAccess.scala"),
      compileFile("tests/pos/approximateUnion.scala"),
      compileFilesInDir("tests/pos/tailcall"),
      compileShallowFilesInDir("tests/pos/pos_valueclasses"),
      compileFile("tests/pos/subtyping.scala"),
      compileFile("tests/pos/i0239.scala"),
      compileFile("tests/pos/anonClassSubtyping.scala"),
      compileFile("tests/pos/extmethods.scala"),
      compileFile("tests/pos/companions.scala"),
      compileFile("tests/pos/main.scala"),
    ).times(2).checkCompile()
  }

  // Negative tests ------------------------------------------------------------

  @Test def negAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNeg")
    aggregateTests(
      compileFilesInDir("tests/neg"),
      compileFilesInDir("tests/neg-tailcall"),
      compileFilesInDir("tests/neg-strict", defaultOptions.and("-source", "future", "-deprecation", "-Xfatal-warnings")),
      compileFilesInDir("tests/neg-no-kind-polymorphism", defaultOptions.and("-Yno-kind-polymorphism")),
      compileFilesInDir("tests/neg-custom-args/deprecation", defaultOptions.and("-Xfatal-warnings", "-deprecation")),
      compileFilesInDir("tests/neg-custom-args/fatal-warnings", defaultOptions.and("-Xfatal-warnings")),
      compileFilesInDir("tests/neg-custom-args/erased", defaultOptions.and("-language:experimental.erasedDefinitions")),
      compileFilesInDir("tests/neg-custom-args/allow-double-bindings", defaultOptions.without("-Yno-double-bindings")),
      compileFilesInDir("tests/neg-custom-args/allow-deep-subtypes", allowDeepSubtypes),
      compileFilesInDir("tests/neg-custom-args/explicit-nulls", defaultOptions.and("-Yexplicit-nulls")),
      compileFilesInDir("tests/neg-custom-args/no-experimental", defaultOptions.and("-Yno-experimental")),
      compileDir("tests/neg-custom-args/impl-conv", defaultOptions.and("-Xfatal-warnings", "-feature")),
      compileFile("tests/neg-custom-args/implicit-conversions.scala", defaultOptions.and("-Xfatal-warnings", "-feature")),
      compileFile("tests/neg-custom-args/implicit-conversions-old.scala", defaultOptions.and("-Xfatal-warnings", "-feature")),
      compileFile("tests/neg-custom-args/i3246.scala", scala2CompatMode),
      compileFile("tests/neg-custom-args/overrideClass.scala", scala2CompatMode),
      compileFile("tests/neg-custom-args/ovlazy.scala", scala2CompatMode.and("-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/newline-braces.scala", scala2CompatMode.and("-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/autoTuplingTest.scala", defaultOptions.andLanguageFeature("noAutoTupling")),
      compileFile("tests/neg-custom-args/nopredef.scala", defaultOptions.and("-Yno-predef")),
      compileFile("tests/neg-custom-args/noimports.scala", defaultOptions.and("-Yno-imports")),
      compileFile("tests/neg-custom-args/noimports2.scala", defaultOptions.and("-Yno-imports")),
      compileFile("tests/neg-custom-args/i1650.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/i3882.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/i4372.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/i1754.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/i12650.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/i9517.scala", defaultOptions.and("-Xprint-types")),
      compileFile("tests/neg-custom-args/i11637.scala", defaultOptions.and("-explain")),
      compileFile("tests/neg-custom-args/interop-polytypes.scala", allowDeepSubtypes.and("-Yexplicit-nulls")),
      compileFile("tests/neg-custom-args/conditionalWarnings.scala", allowDeepSubtypes.and("-deprecation").and("-Xfatal-warnings")),
      compileFilesInDir("tests/neg-custom-args/isInstanceOf", allowDeepSubtypes.and("-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/i3627.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/sourcepath/outer/nested/Test1.scala", defaultOptions.and("-sourcepath", "tests/neg-custom-args/sourcepath")),
      compileDir("tests/neg-custom-args/sourcepath2/hi", defaultOptions.and("-sourcepath", "tests/neg-custom-args/sourcepath2", "-Xfatal-warnings")),
      compileList("duplicate source", List("tests/neg-custom-args/toplevel-samesource/S.scala", "tests/neg-custom-args/toplevel-samesource/nested/S.scala")),
      compileFile("tests/neg-custom-args/i6300.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/infix.scala", defaultOptions.and("-source", "future", "-deprecation", "-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/missing-alpha.scala", defaultOptions.and("-Yrequire-targetName", "-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/wildcards.scala", defaultOptions.and("-source", "future", "-deprecation", "-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/indentRight.scala", defaultOptions.and("-no-indent", "-Xfatal-warnings")),
      compileDir("tests/neg-custom-args/adhoc-extension", defaultOptions.and("-source", "future", "-feature", "-Xfatal-warnings")),
      compileFile("tests/neg/i7575.scala", defaultOptions.withoutLanguageFeatures.and("-language:_")),
      compileFile("tests/neg-custom-args/kind-projector.scala", defaultOptions.and("-Ykind-projector")),
      compileFile("tests/neg-custom-args/kind-projector-underscores.scala", defaultOptions.and("-Ykind-projector:underscores")),
      compileFile("tests/neg-custom-args/typeclass-derivation2.scala", defaultOptions.and("-language:experimental.erasedDefinitions")),
      compileFile("tests/neg-custom-args/i5498-postfixOps.scala", defaultOptions withoutLanguageFeature "postfixOps"),
      compileFile("tests/neg-custom-args/deptypes.scala", defaultOptions.and("-language:experimental.dependent")),
      compileFile("tests/neg-custom-args/matchable.scala", defaultOptions.and("-Xfatal-warnings", "-source", "future")),
      compileFile("tests/neg-custom-args/i7314.scala", defaultOptions.and("-Xfatal-warnings", "-source", "future")),
      compileFile("tests/neg-custom-args/feature-shadowing.scala", defaultOptions.and("-Xfatal-warnings", "-feature")),
      compileDir("tests/neg-custom-args/hidden-type-errors",  defaultOptions.and("-explain")),
    ).checkExpectedErrors()
  }

  @Test def fuzzyAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileFuzzy")
    compileFilesInDir("tests/fuzzy").checkNoCrash()
  }

  // Run tests -----------------------------------------------------------------

  @Test def runAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runAll")
    aggregateTests(
      compileFile("tests/run-custom-args/tuple-cons.scala", allowDeepSubtypes),
      compileFile("tests/run-custom-args/i5256.scala", allowDeepSubtypes),
      compileFile("tests/run-custom-args/fors.scala", defaultOptions.and("-source", "future")),
      compileFile("tests/run-custom-args/no-useless-forwarders.scala", defaultOptions and "-Xmixin-force-forwarders:false"),
      compileFile("tests/run-custom-args/defaults-serizaliable-no-forwarders.scala", defaultOptions and "-Xmixin-force-forwarders:false"),
      compileFilesInDir("tests/run-custom-args/erased", defaultOptions.and("-language:experimental.erasedDefinitions")),
      compileFilesInDir("tests/run-deep-subtype", allowDeepSubtypes),
      compileFilesInDir("tests/run", defaultOptions.and("-Ysafe-init")),
      compileFilesInDir("tests/generic-java-signatures"),
    ).checkRuns()
  }

  // Pickling Tests ------------------------------------------------------------

  @Test def pickling: Unit = {
    implicit val testGroup: TestGroup = TestGroup("testPickling")
    aggregateTests(
      compileFilesInDir("tests/new", picklingOptions),
      compileFilesInDir("tests/pos", picklingOptions, FileFilter.exclude(TestSources.posTestPicklingBlacklisted)),
      compileFilesInDir("tests/run", picklingOptions, FileFilter.exclude(TestSources.runTestPicklingBlacklisted))
    ).checkCompile()
  }

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
      Array("-Ycheck-reentrant", "-language:postfixOps", "-Xsemanticdb")
    )

    val libraryDirs = List(Paths.get("library/src"), Paths.get("library/src-bootstrapped"))
    val librarySources = libraryDirs.flatMap(sources(_))

    val lib =
      compileList("lib", librarySources,
        defaultOptions.and("-Ycheck-reentrant",
          "-language:experimental.erasedDefinitions", // support declaration of scala.compiletime.erasedValue
          //  "-source", "future",  // TODO: re-enable once we allow : @unchecked in pattern definitions. Right now, lots of narrowing pattern definitions fail.
          ))(libGroup)

    val tastyCoreSources = sources(Paths.get("tasty/src"))
    val tastyCore = compileList("tastyCore", tastyCoreSources, opt)(tastyCoreGroup)

    val compilerSources = sources(Paths.get("compiler/src")) ++ sources(Paths.get("compiler/src-bootstrapped"))
    val compilerManagedSources = sources(Properties.dottyCompilerManagedSources)

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
    compileList("idempotency", List("tests/idempotency/BootstrapChecker.scala", "tests/idempotency/IdempotencyCheck.scala")).checkRuns()

    tests.foreach(_.delete())
  }

  // Explicit nulls tests
  val explicitNullsOptions = defaultOptions.and("-Yexplicit-nulls")

  @Test def explicitNullsNeg: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsNeg")
    aggregateTests(
      compileFilesInDir("tests/explicit-nulls/neg", explicitNullsOptions),
      compileFilesInDir("tests/explicit-nulls/neg-patmat", explicitNullsOptions.and("-Xfatal-warnings")),
      compileFilesInDir("tests/explicit-nulls/unsafe-common", explicitNullsOptions),
    )
  }.checkExpectedErrors()

  @Test def explicitNullsPos: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsPos")
    aggregateTests(
      compileFilesInDir("tests/explicit-nulls/pos", explicitNullsOptions),
      compileFilesInDir("tests/explicit-nulls/pos-separate", explicitNullsOptions),
      compileFilesInDir("tests/explicit-nulls/unsafe-common", explicitNullsOptions.and("-language:unsafeNulls")),
    )
  }.checkCompile()

  @Test def explicitNullsRun: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsRun")
    compileFilesInDir("tests/explicit-nulls/run", explicitNullsOptions)
  }.checkRuns()

  // initialization tests
  @Test def checkInit: Unit = {
    implicit val testGroup: TestGroup = TestGroup("checkInit")
    val options = defaultOptions.and("-Ysafe-init", "-Xfatal-warnings")
    compileFilesInDir("tests/init/neg", options).checkExpectedErrors()
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
}

object CompilationTests extends ParallelTesting {
  // Test suite configuration --------------------------------------------------

  def maxDuration = 45.seconds
  def numberOfSlaves = Runtime.getRuntime().availableProcessors()
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
