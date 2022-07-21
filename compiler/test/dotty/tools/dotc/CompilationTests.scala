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

class CompilationTests {
  import ParallelTesting._
  import TestConfiguration._
  import CompilationTests._
  import CompilationTest.aggregateTests

  // Positive tests ------------------------------------------------------------

  @Test def pos: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePos")
    aggregateTests(
      compileFile("tests/pos/nullarify.scala", defaultOptions.and("-Ycheck:nullarify")),
      compileFile("tests/pos-special/utf8encoded.scala", explicitUTF8),
      compileFile("tests/pos-special/utf16encoded.scala", explicitUTF16),
      compileFilesInDir("tests/pos-special/sourcepath/outer", defaultOptions.and("-sourcepath", "tests/pos-special/sourcepath")),
      compileFile("tests/pos-special/sourcepath/outer/nested/Test4.scala", defaultOptions.and("-sourcepath", "tests/pos-special/sourcepath")),
      compileFilesInDir("tests/pos-special/fatal-warnings", defaultOptions.and("-Xfatal-warnings", "-deprecation", "-feature")),
      compileFilesInDir("tests/pos-special/spec-t5545", defaultOptions),
      compileFilesInDir("tests/pos-special/strawman-collections", allowDeepSubtypes),
      compileFilesInDir("tests/pos-special/isInstanceOf", allowDeepSubtypes.and("-Xfatal-warnings")),
      compileFilesInDir("tests/new", defaultOptions.and("-source", "3.2")), // just to see whether 3.2 works
      compileFilesInDir("tests/pos-scala2", scala2CompatMode),
      compileFilesInDir("tests/pos-custom-args/erased", defaultOptions.and("-language:experimental.erasedDefinitions")),
      compileFilesInDir("tests/pos", defaultOptions.and("-Ysafe-init")),
      compileFilesInDir("tests/pos-deep-subtype", allowDeepSubtypes),
      compileFilesInDir("tests/pos-custom-args/no-experimental", defaultOptions.and("-Yno-experimental")),
      compileDir("tests/pos-special/java-param-names", defaultOptions.withJavacOnlyOptions("-parameters")),
      compileFile(
        // succeeds despite -Xfatal-warnings because of -nowarn
        "tests/neg-custom-args/fatal-warnings/xfatalWarnings.scala",
        defaultOptions.and("-nowarn", "-Xfatal-warnings")
      ),
      compileFile("tests/pos-special/typeclass-scaling.scala", defaultOptions.and("-Xmax-inlines", "40")),
      compileFile("tests/pos-special/i7296.scala", defaultOptions.and("-source", "future", "-deprecation", "-Xfatal-warnings")),
      compileDir("tests/pos-special/adhoc-extension", defaultOptions.and("-source", "future", "-feature", "-Xfatal-warnings")),
      compileFile("tests/pos-special/i7575.scala", defaultOptions.andLanguageFeature("dynamics")),
      compileFile("tests/pos-special/kind-projector.scala", defaultOptions.and("-Ykind-projector")),
      compileFile("tests/pos-special/kind-projector-underscores.scala", defaultOptions.and("-Ykind-projector:underscores")),
      compileFile("tests/run/i5606.scala", defaultOptions.and("-Yretain-trees")),
      compileFile("tests/pos-custom-args/i8875.scala", defaultOptions.and("-Xprint:getters")),
      compileFile("tests/pos-custom-args/i9267.scala", defaultOptions.and("-Ystop-after:erasure")),
      compileFile("tests/pos-special/extend-java-enum.scala", defaultOptions.and("-source", "3.0-migration")),
      compileFile("tests/pos-custom-args/help.scala", defaultOptions.and("-help", "-V", "-W", "-X", "-Y")),
      compileFile("tests/pos-custom-args/i10383.scala", defaultOptions.and("-source", "future", "-deprecation", "-Xfatal-warnings")),
      compileFile("tests/pos-custom-args/i13044.scala", defaultOptions.and("-Xmax-inlines:33")),
      compileFile("tests/pos-custom-args/jdk-8-app.scala", defaultOptions.and("-release:8")),
    ).checkCompile()
  }

  @Test def rewrites: Unit = {
    implicit val testGroup: TestGroup = TestGroup("rewrites")

    aggregateTests(
      compileFile("tests/rewrites/rewrites.scala", scala2CompatMode.and("-rewrite", "-indent")),
      compileFile("tests/rewrites/rewrites3x.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/filtering-fors.scala", defaultOptions.and("-rewrite", "-source", "3.2-migration")),
      compileFile("tests/rewrites/refutable-pattern-bindings.scala", defaultOptions.and("-rewrite", "-source", "3.2-migration")),
      compileFile("tests/rewrites/i8982.scala", defaultOptions.and("-indent", "-rewrite")),
      compileFile("tests/rewrites/i9632.scala", defaultOptions.and("-indent", "-rewrite")),
      compileFile("tests/rewrites/i11895.scala", defaultOptions.and("-indent", "-rewrite")),
      compileFile("tests/rewrites/i12340.scala", unindentOptions.and("-rewrite")),
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

  // Negative tests ------------------------------------------------------------

  @Test def negAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNeg")
    aggregateTests(
      compileFilesInDir("tests/neg", defaultOptions),
      compileFilesInDir("tests/neg-tailcall", defaultOptions),
      compileFilesInDir("tests/neg-strict", defaultOptions.and("-source", "future", "-deprecation", "-Xfatal-warnings")),
      compileFilesInDir("tests/neg-no-kind-polymorphism", defaultOptions and "-Yno-kind-polymorphism"),
      compileFilesInDir("tests/neg-custom-args/deprecation", defaultOptions.and("-Xfatal-warnings", "-deprecation")),
      compileFilesInDir("tests/neg-custom-args/fatal-warnings", defaultOptions.and("-Xfatal-warnings")),
      compileFilesInDir("tests/neg-custom-args/nowarn", defaultOptions.and("-deprecation", "-Wunused:nowarn", "-Wconf:msg=@nowarn annotation does not suppress any warnings:e")),
      compileFilesInDir("tests/neg-custom-args/erased", defaultOptions.and("-language:experimental.erasedDefinitions")),
      compileFilesInDir("tests/neg-custom-args/allow-double-bindings", allowDoubleBindings),
      compileFilesInDir("tests/neg-custom-args/allow-deep-subtypes", allowDeepSubtypes),
      compileFilesInDir("tests/neg-custom-args/no-experimental", defaultOptions.and("-Yno-experimental")),
      compileDir("tests/neg-custom-args/impl-conv", defaultOptions.and("-Xfatal-warnings", "-feature")),
      compileDir("tests/neg-custom-args/i13946", defaultOptions.and("-Xfatal-warnings", "-feature")),
      compileFile("tests/neg-custom-args/avoid-warn-deprecation.scala", defaultOptions.and("-Xfatal-warnings", "-feature")),
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
      compileFile("tests/neg-custom-args/i15575.scala", defaultOptions.and("-explain")),
      compileFile("tests/neg-custom-args/interop-polytypes.scala", allowDeepSubtypes.and("-Yexplicit-nulls")),
      compileFile("tests/neg-custom-args/conditionalWarnings.scala", allowDeepSubtypes.and("-deprecation").and("-Xfatal-warnings")),
      compileFilesInDir("tests/neg-custom-args/isInstanceOf", allowDeepSubtypes and "-Xfatal-warnings"),
      compileFile("tests/neg-custom-args/i3627.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/sourcepath/outer/nested/Test1.scala", defaultOptions.and("-sourcepath", "tests/neg-custom-args/sourcepath")),
      compileDir("tests/neg-custom-args/sourcepath2/hi", defaultOptions.and("-sourcepath", "tests/neg-custom-args/sourcepath2", "-Xfatal-warnings")),
      compileList("duplicate source", List(
        "tests/neg-custom-args/toplevel-samesource/S.scala",
        "tests/neg-custom-args/toplevel-samesource/nested/S.scala"),
        defaultOptions),
      compileFile("tests/neg-custom-args/i6300.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/infix.scala", defaultOptions.and("-source", "future", "-deprecation", "-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/missing-targetName.scala", defaultOptions.and("-Yrequire-targetName", "-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/wildcards.scala", defaultOptions.and("-source", "future", "-deprecation", "-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/indentRight.scala", defaultOptions.and("-no-indent", "-Xfatal-warnings")),
      compileDir("tests/neg-custom-args/adhoc-extension", defaultOptions.and("-source", "future", "-feature", "-Xfatal-warnings")),
      compileFile("tests/neg/i7575.scala", defaultOptions.withoutLanguageFeatures.and("-language:_")),
      compileFile("tests/neg-custom-args/kind-projector.scala", defaultOptions.and("-Ykind-projector")),
      compileFile("tests/neg-custom-args/kind-projector-underscores.scala", defaultOptions.and("-Ykind-projector:underscores")),
      compileFile("tests/neg-custom-args/typeclass-derivation2.scala", defaultOptions.and("-language:experimental.erasedDefinitions")),
      compileFile("tests/neg-custom-args/deptypes.scala", defaultOptions.and("-language:experimental.dependent")),
      compileFile("tests/neg-custom-args/matchable.scala", defaultOptions.and("-Xfatal-warnings", "-source", "future")),
      compileFile("tests/neg-custom-args/i7314.scala", defaultOptions.and("-Xfatal-warnings", "-source", "future")),
      compileFile("tests/neg-custom-args/feature-shadowing.scala", defaultOptions.and("-Xfatal-warnings", "-feature")),
      compileDir("tests/neg-custom-args/hidden-type-errors", defaultOptions.and("-explain")),
      compileFile("tests/neg-custom-args/i13026.scala", defaultOptions.and("-print-lines")),
      compileFile("tests/neg-custom-args/i13838.scala", defaultOptions.and("-Ximplicit-search-limit", "1000")),
      compileFile("tests/neg-custom-args/jdk-9-app.scala", defaultOptions.and("-release:8")),
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
      compileFile("tests/run-custom-args/typeclass-derivation1.scala", defaultOptions.without(yCheckOptions*)),
      compileFile("tests/run-custom-args/tuple-cons.scala", allowDeepSubtypes),
      compileFile("tests/run-custom-args/i5256.scala", allowDeepSubtypes),
      compileFile("tests/run-custom-args/no-useless-forwarders.scala", defaultOptions and "-Xmixin-force-forwarders:false"),
      compileFile("tests/run-custom-args/defaults-serizaliable-no-forwarders.scala", defaultOptions and "-Xmixin-force-forwarders:false"),
      compileFilesInDir("tests/run-custom-args/erased", defaultOptions.and("-language:experimental.erasedDefinitions")),
      compileFilesInDir("tests/run-custom-args/fatal-warnings", defaultOptions.and("-Xfatal-warnings")),
      compileDir("tests/run-custom-args/Xmacro-settings/simple", defaultOptions.and("-Xmacro-settings:one,two,three")),
      compileDir("tests/run-custom-args/Xmacro-settings/compileTimeEnv", defaultOptions.and("-Xmacro-settings:a,b=1,c.b.a=x.y.z=1,myLogger.level=INFO")),
      compileFilesInDir("tests/run-deep-subtype", allowDeepSubtypes),
      compileFilesInDir("tests/run", defaultOptions.and("-Ysafe-init"))
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
      compileFilesInDir("tests/new", picklingOptions),
      compileFilesInDir("tests/pos", picklingOptions, FileFilter.exclude(TestSources.posTestPicklingBlacklisted)),
      compileFilesInDir("tests/run", picklingOptions, FileFilter.exclude(TestSources.runTestPicklingBlacklisted))
    ).checkCompile()
  }

  // Explicit nulls tests
  @Test def explicitNullsNeg: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsNeg")
    aggregateTests(
      compileFilesInDir("tests/explicit-nulls/neg", explicitNullsOptions),
      compileFilesInDir("tests/explicit-nulls/neg-patmat", explicitNullsOptions and "-Xfatal-warnings"),
      compileFilesInDir("tests/explicit-nulls/unsafe-common", explicitNullsOptions),
    )
  }.checkExpectedErrors()

  @Test def explicitNullsPos: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsPos")
    aggregateTests(
      compileFilesInDir("tests/explicit-nulls/pos", explicitNullsOptions),
      compileFilesInDir("tests/explicit-nulls/pos-separate", explicitNullsOptions),
      compileFilesInDir("tests/explicit-nulls/pos-patmat", explicitNullsOptions and "-Xfatal-warnings"),
      compileFilesInDir("tests/explicit-nulls/pos-pickling", explicitNullsOptions and "-Ytest-pickler" and "-Xprint-types"),
      compileFilesInDir("tests/explicit-nulls/unsafe-common", explicitNullsOptions and "-language:unsafeNulls"),
      compileFile("tests/explicit-nulls/pos-special/i14682.scala", explicitNullsOptions and "-Ysafe-init"),
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
