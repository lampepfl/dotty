package dotty.communitybuild

import java.nio.file._
import java.io.{PrintWriter, File}
import java.nio.charset.StandardCharsets.UTF_8

lazy val communitybuildDir: Path = Paths.get(sys.props("user.dir"))

lazy val compilerVersion: String =
  val file = communitybuildDir.resolve("scala3-bootstrapped.version")
  new String(Files.readAllBytes(file), UTF_8)

lazy val compilerSupportExperimental: Boolean =
  compilerVersion.contains("SNAPSHOT") || compilerVersion.contains("NIGHTLY")

lazy val sbtPluginFilePath: String =
  // Workaround for https://github.com/sbt/sbt/issues/4395
  new File(sys.props("user.home") + "/.sbt/1.0/plugins").mkdirs()
  communitybuildDir.resolve("sbt-injected-plugins").toAbsolutePath().toString()

def log(msg: String) = println(Console.GREEN + msg + Console.RESET)

/** Executes shell command, returns false in case of error. */
def exec(projectDir: Path, binary: String, arguments: Seq[String], environment: Map[String, String]): Int =
  import collection.JavaConverters._
  val command = binary +: arguments
  log(command.mkString(" "))
  val builder = new ProcessBuilder(command: _*).directory(projectDir.toFile).inheritIO()
  builder.environment.putAll(environment.asJava)
  val process = builder.start()
  val exitCode = process.waitFor()
  exitCode


sealed trait CommunityProject:
  private var published = false

  val project: String
  val testCommand: String
  val publishCommand: String
  val docCommand: String
  val dependencies: List[CommunityProject]
  val testOnlyDependencies: () => List[CommunityProject]
  val binaryName: String
  val runCommandsArgs: List[String] = Nil
  val requiresExperimental: Boolean
  val environment: Map[String, String] = Map.empty

  final val projectDir = communitybuildDir.resolve("community-projects").resolve(project)

  final def publishDependencies(): Unit =
    dependencies.foreach(_.publish())

  /** Publish this project to the local Maven repository */
  final def publish(): Unit =
    // TODO what should this do with .requiresExperimental?
    if !published then
      publishDependencies()
      log(s"Publishing $project")
      if publishCommand eq null then
        throw RuntimeException(s"Publish command is not specified for $project. Project details:\n$this")
      val exitCode = exec(projectDir, binaryName, (runCommandsArgs :+ publishCommand), environment)
      if exitCode != 0 then
        throw RuntimeException(s"Publish command exited with code $exitCode for project $project. Project details:\n$this")
      published = true

  final def doc(): Unit =
    if this.requiresExperimental && !compilerSupportExperimental then
      log(
        s"Skipping ${this.project} - it needs experimental features unsupported in this build."
      )
      return
    publishDependencies()
    log(s"Documenting $project")
    if docCommand eq null then
      throw RuntimeException(s"Doc command is not specified for $project. Project details:\n$this")
    val exitCode = exec(projectDir, binaryName, (runCommandsArgs :+ docCommand), environment)
    if exitCode != 0 then
      throw RuntimeException(s"Doc command exited with code $exitCode for project $project. Project details:\n$this")

  final def build(): Int = exec(projectDir, binaryName, buildCommands, environment)

  final def buildCommands = runCommandsArgs :+ testCommand

end CommunityProject

final case class MillCommunityProject(
    project: String,
    baseCommand: String,
    dependencies: List[CommunityProject] = Nil,
    testOnlyDependencies: () => List[CommunityProject] = () => Nil,
    ignoreDocs: Boolean = false,
    requiresExperimental: Boolean = false,
    ) extends CommunityProject:
  override val binaryName: String = "./mill"
  override val testCommand = s"$baseCommand.test"
  override val publishCommand = s"$baseCommand.publishLocal"
  override val docCommand = null
    // uncomment once mill is released
    // if ignoreDocs then null else s"$baseCommand.docJar"
  override val runCommandsArgs = List("-i", "-D", s"dottyVersion=$compilerVersion")
  override val environment = Map("MILL_VERSION" -> "0.9.6-16-a5da34")

final case class SbtCommunityProject(
    project: String,
    sbtTestCommand: String,
    extraSbtArgs: List[String] = Nil,
    dependencies: List[CommunityProject] = Nil,
    testOnlyDependencies: () => List[CommunityProject] = () => Nil,
    sbtPublishCommand: String = null,
    sbtDocCommand: String = null,
    scalacOptions: List[String] = SbtCommunityProject.scalacOptions,
    requiresExperimental: Boolean = false,
  ) extends CommunityProject:
  override val binaryName: String = "sbt"

  private def scalacOptionsString: String =
    scalacOptions.map("\"" + _ + "\"").mkString("List(", ",", ")")

  private val baseCommand =
    "clean; set Global/logLevel := Level.Error; set Global/updateOptions ~= (_.withLatestSnapshots(false)); "
    ++ (if scalacOptions.isEmpty then "" else s"""set Global/scalacOptions ++= $scalacOptionsString;""")
    ++ s"++$compilerVersion!; "

  override val testCommand =
    """set Global/testOptions += Tests.Argument(TestFramework("munit.Framework"), "+l"); """
    ++ s"$baseCommand$sbtTestCommand"

  override val publishCommand =
    if sbtPublishCommand eq null then null else s"$baseCommand$sbtPublishCommand"

  override val docCommand =
    if sbtDocCommand eq null then null else
      val cmd = if sbtDocCommand.startsWith(";") then sbtDocCommand else s";$sbtDocCommand"
      s"$baseCommand set every useScaladoc := true; set every doc/logLevel := Level.Warn $cmd "

  override val runCommandsArgs: List[String] =
    // Run the sbt command with the compiler version and sbt plugin set in the build
    val sbtProps = Option(System.getProperty("sbt.ivy.home")) match
      case Some(ivyHome) => List(s"-Dsbt.ivy.home=$ivyHome")
      case _ => Nil
    extraSbtArgs ++ sbtProps ++ List(
      "-sbt-version", "1.6.2",
      "-Dsbt.supershell=false",
      s"-Ddotty.communitybuild.dir=$communitybuildDir",
      s"--addPluginSbtFile=$sbtPluginFilePath"
    )

object SbtCommunityProject:
  def scalacOptions = List(
    "-Xcheck-macros",
    "-Ysafe-init",
  )

object projects:

  private def forceDoc(projects: String*) =
    projects.map(project =>
      s""";set $project/Compile/doc/sources ++= ($project/Compile/doc/dotty.tools.sbtplugin.DottyPlugin.autoImport.tastyFiles).value ;$project/doc"""
    ).mkString(" ")

  private def aggregateDoc(in: String)(projects: String*) =
    val tastyFiles =
      (in +: projects).map(p => s"($p/Compile/doc/dotty.tools.sbtplugin.DottyPlugin.autoImport.tastyFiles).value").mkString(" ++ ")
    s""";set $in/Compile/doc/sources ++= file("a.scala") +: ($tastyFiles) ;$in/doc"""

  lazy val utest = MillCommunityProject(
    project = "utest",
    baseCommand = s"utest.jvm[$compilerVersion]",
    ignoreDocs = true
  )

  lazy val sourcecode = MillCommunityProject(
    project = "sourcecode",
    baseCommand = s"sourcecode.jvm[$compilerVersion]",
    ignoreDocs = true
  )

  lazy val oslib = MillCommunityProject(
    project = "os-lib",
    baseCommand = s"os.jvm[$compilerVersion]",
    dependencies = List(utest, sourcecode)
  )

  lazy val oslibWatch = MillCommunityProject(
    project = "os-lib",
    baseCommand = s"os.watch[$compilerVersion]",
    dependencies = List(utest, sourcecode),
    ignoreDocs = true
  )

  lazy val ujson = MillCommunityProject(
    project = "upickle",
    baseCommand = s"ujson.jvm[$compilerVersion]",
    dependencies = List(geny)
  )

  lazy val upickle = MillCommunityProject(
    project = "upickle",
    baseCommand = s"upickle.jvm[$compilerVersion]",
    dependencies = List(geny, utest)
  )

  lazy val upickleCore = MillCommunityProject(
    project = "upickle",
    baseCommand = s"core.jvm[$compilerVersion]",
    dependencies = List(geny, utest)
  )

  lazy val upickleImplicits = MillCommunityProject(
    project = "upickle",
    baseCommand = s"implicits.jvm[$compilerVersion]",
    dependencies = List(upickleCore, ujson)
  )

  lazy val upack = MillCommunityProject(
    project = "upickle",
    baseCommand = s"upack.jvm[$compilerVersion]",
    dependencies = List(ujson, upickleCore)
  )

  lazy val geny = MillCommunityProject(
    project = "geny",
    baseCommand = s"geny.jvm[$compilerVersion]",
    dependencies = List(utest)
  )

  lazy val fansi = MillCommunityProject(
    project = "fansi",
    baseCommand = s"fansi.jvm[$compilerVersion]",
    dependencies = List(utest, sourcecode),
    ignoreDocs = true
  )

  lazy val pprint = MillCommunityProject(
    project = "PPrint",
    baseCommand = s"pprint.jvm[$compilerVersion]",
    dependencies = List(fansi),
    ignoreDocs = true
  )

  lazy val requests = MillCommunityProject(
    project = "requests-scala",
    baseCommand = s"requests[$compilerVersion]",
    dependencies = List(geny, utest, ujson, upickleCore)
  )

  lazy val cask = MillCommunityProject(
    project = "cask",
    baseCommand = s"cask[$compilerVersion]",
    dependencies = List(utest, geny, sourcecode, pprint, upickle, upickleImplicits, upack, requests)
  )

  lazy val scas = MillCommunityProject(
    project = "scas",
    baseCommand = "scas.application",
  )

  lazy val intent = SbtCommunityProject(
    project       = "intent",
    sbtTestCommand   = "test",
    sbtDocCommand = "doc",
    requiresExperimental = true,
  )

  lazy val scalacheck = SbtCommunityProject(
    project       = "scalacheck",
    sbtTestCommand   = "jvm/test;js/test",
    sbtPublishCommand = "jvm/publishLocal;js/publishLocal",
    sbtDocCommand = forceDoc("jvm")
  )

  lazy val scalatest: SbtCommunityProject = SbtCommunityProject(
    project       = "scalatest",
    sbtTestCommand   =
      List(
        "scalacticDotty/clean; scalacticDottyJS/clean",

        // Some scalatest's tests are flaky (https://github.com/scalatest/scalatest/issues/2049)
        // so we disable them, this list is based on the one used in the Scala 2 community build
        // (https://github.com/scala/community-build/blob/2.13.x/proj/scalatest.conf).
        """set scalatestTestDotty / Test / managedSources ~= (_.filterNot(_.getName == "GeneratorSpec.scala").filterNot(_.getName == "FrameworkSuite.scala").filterNot(_.getName == "WaitersSpec.scala").filterNot(_.getName == "TestSortingReporterSpec.scala").filterNot(_.getName == "JavaFuturesSpec.scala").filterNot(_.getName == "ParallelTestExecutionSpec.scala").filterNot(_.getName == "TimeLimitsSpec.scala").filterNot(_.getName == "DispatchReporterSpec.scala").filterNot(_.getName == "TestThreadsStartingCounterSpec.scala").filterNot(_.getName == "SuiteSortingReporterSpec.scala").filterNot(_.getName == "CommonGeneratorsSpec.scala").filterNot(_.getName == "PropCheckerAssertingSpec.scala").filterNot(_.getName == "ConductorMethodsSuite.scala").filterNot(_.getName == "EventuallySpec.scala").filterNot(_.getName == "AssertionsSpec.scala").filterNot(_.getName == "DirectAssertionsSpec.scala"))""",
        """set scalacticTestDotty / Test / managedSources ~= (_.filterNot(_.getName == "NonEmptyArraySpec.scala"))""",
        """set genRegularTests4 / Test / managedSources ~= (_.filterNot(_.getName == "FrameworkSuite.scala").filterNot(_.getName == "GeneratorSpec.scala").filterNot(_.getName == "CommonGeneratorsSpec.scala").filterNot(_.getName == "ParallelTestExecutionSpec.scala").filterNot(_.getName == "DispatchReporterSpec.scala").filterNot(_.getName == "TestThreadsStartingCounterSpec.scala").filterNot(_.getName == "EventuallySpec.scala"))""",
        "scalacticTestDotty/test; scalatestTestDotty/test; scalacticDottyJS/compile; scalatestDottyJS/compile"
      ).mkString("; "),
    sbtPublishCommand = "scalacticDotty/publishLocal; scalatestDotty/publishLocal; scalacticDottyJS/publishLocal; scalatestDottyJS/publishLocal",
    sbtDocCommand = ";scalacticDotty/doc", // fails with missing type ;scalatestDotty/doc"
    // cannot take signature of (test: org.scalatest.concurrent.ConductorFixture#OneArgTest):
    // org.scalatest.Outcome
    // Problem parsing scalatest.dotty/target/scala-3.0.0-M2/src_managed/main/org/scalatest/concurrent/ConductorFixture.scala:[602..624..3843], documentation may not be generated.
    // dotty.tools.dotc.core.MissingType:
    dependencies = List(scalaXml),
    testOnlyDependencies = () => List(scalatestplusJunit, scalatestplusTestNG)
  )

  lazy val scalatestplusScalacheck = SbtCommunityProject(
    project = "scalatestplus-scalacheck",
    sbtTestCommand = "scalatestPlusScalaCheckJVM/test",
    sbtPublishCommand = "scalatestPlusScalaCheckJVM/publishLocal",
    sbtDocCommand = "scalatestPlusScalaCheckJVM/doc",
    dependencies = List(scalatest, scalacheck)
  )

  lazy val scalatestplusJunit = SbtCommunityProject(
    project           = "scalatestplus-junit",
    sbtTestCommand    = "scalatestplus-junit/test",
    sbtPublishCommand = "scalatestplus-junit/publishLocal",
    dependencies      = List(scalatest)
  )

  lazy val scalatestplusTestNG = SbtCommunityProject(
    project = "scalatestplus-testng",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
    dependencies = List(scalatest)
  )

  lazy val scalaXml = SbtCommunityProject(
    project       = "scala-xml",
    sbtTestCommand = "xml/test",
    sbtPublishCommand = "xml/publishLocal",
    sbtDocCommand = "xml/doc"
  )

  lazy val scalap = SbtCommunityProject(
    project       = "scalap",
    sbtTestCommand   = "scalap/compile",
    sbtDocCommand = "scalap/doc"
  )

  lazy val betterfiles = SbtCommunityProject(
    project       = "betterfiles",
    sbtTestCommand   = "dotty-community-build/compile",
    sbtDocCommand   = ";core/doc ;akka/doc ;shapelessScanner/doc"
  )

  lazy val scalaPB = SbtCommunityProject(
    project       = "ScalaPB",
    sbtTestCommand   = "lensesJVM3/compile; runtimeJVM3/compile; grpcRuntimeJVM3/compile; compilerPluginJVM3/compile",
    // aggregateDoc("runtimeJVM")("scalapbc", "grpcRuntime", "compilerPlugin") fails with
    // module class ScalaPbCodeGenerator$ has non-class parent: TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module protocbridge),ProtocCodeGenerator)
    // Also it seems that we do not handle correctly aggreagation projects
    // sbtDocCommand = "dotty-community-build/doc"
    sbtDocCommand = forceDoc("scalapbc", "grpcRuntime","runtimeJVM", "compilerPlugin")
  )

  lazy val minitest = SbtCommunityProject(
    project       = "minitest",
    sbtTestCommand   = "test",
    sbtDocCommand = aggregateDoc("lawsJVM")("minitestJVM"),
    dependencies = List(scalacheck)
  )

  lazy val fastparse = SbtCommunityProject(
    project       = "fastparse",
    sbtTestCommand   = "dotty-community-build/compile;dotty-community-build/test:compile",
    // Problem parsing perftests/bench2/src/perftests/PythonParse.scala:[0..18..694]
    // sbtDocCommand = "dotty-community-build/doc"
  )

  lazy val stdLib213 = SbtCommunityProject(
    project       = "stdLib213",
    extraSbtArgs  = List("-Dscala.build.compileWithDotty=true"),
    sbtTestCommand   = """set Global / fatalWarnings := false; library/compile""",
    sbtPublishCommand = """set Global / fatalWarnings := false; set library/Compile/packageDoc/publishArtifact := false; library/publishLocal""",
    // sbtDocCommand = "library/doc" // Does no compile? No idea :/
  )


  lazy val shapeless = SbtCommunityProject(
    project       = "shapeless",
    sbtTestCommand = """set deriving/scalacOptions -= "-Xfatal-warnings"; set typeable/scalacOptions -= "-Xfatal-warnings"; test""",
      // selectively disable -Xfatal-warnings due to deprecations
    sbtDocCommand = forceDoc("typeable", "deriving", "data"),
    scalacOptions = Nil // disable -Ysafe-init, due to -Xfatal-warnings
  )

  lazy val xmlInterpolator = SbtCommunityProject(
    project       = "xml-interpolator",
    sbtTestCommand   = "test",
    sbtDocCommand = "doc", // Again we've got problem with extensions
  )

  lazy val effpi = SbtCommunityProject(
    project       = "effpi",
    // We set `useEffpiPlugin := false` because we don't want to run their
    // compiler plugin since it relies on external binaries (from the model
    // checker mcrl2), however we do compile the compiler plugin.

    // We have to drop the plugin and some akka tests for now, the plugin depends on github.com/bmc/scalasti which
    // has not been updated since 2018, so no 2.13 compat. Some akka tests are dropped due to MutableBehaviour being
    // dropped in the 2.13 compatible release

    // sbtTestCommand   = "set ThisBuild / useEffpiPlugin := false; effpi/test:compile; plugin/test:compile; benchmarks/test:compile; examples/test:compile; pluginBenchmarks/test:compile",

    sbtTestCommand   = "set ThisBuild / useEffpiPlugin := false; effpi/test:compile; benchmarks/test:compile; examples/test:compile; pluginBenchmarks/test:compile",
    sbtDocCommand    = "set ThisBuild / useEffpiPlugin := false; effpi/doc; benchmarks/doc; examples/doc; pluginBenchmarks/doc",
  )

  // TODO @odersky? It got broken by #5458
  // val pdbp = test(
  //   project       = "pdbp",
  //   sbtTestCommand   = "compile",
  // )

  lazy val sconfig = SbtCommunityProject(
    project       = "sconfig",
    sbtTestCommand   = "sconfigJVM/test",
    sbtDocCommand = "sconfigJVM/doc",
    dependencies = List(scalaCollectionCompat)
  )

  lazy val zio = SbtCommunityProject(
    project = "zio",
    sbtTestCommand = """set Global / testOptions += Tests.Filter(name => !name.endsWith("ZIOSpec") && !name.endsWith("ZLayerSpec")); testJVMDotty""",
    sbtDocCommand = forceDoc("coreJVM"),
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Xcheck-macros"),
    dependencies =List(izumiReflect)
  )

  lazy val munit = SbtCommunityProject(
    project = "munit",
    sbtTestCommand  = "testsJVM/test;testsJS/test;",
    sbtPublishCommand = "munitJVM/publishLocal; munitJS/publishLocal; munitScalacheckJVM/publishLocal; munitScalacheckJS/publishLocal; junit/publishLocal",
    sbtDocCommand   = "junit/doc; munitJVM/doc",
    dependencies = List(scalacheck)
  )

  lazy val scodecBits = SbtCommunityProject(
    project          = "scodec-bits",
    sbtTestCommand   = "coreJVM/test;coreJS/test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal",
    sbtDocCommand   = "coreJVM/doc",
    dependencies = List(munit),
  )

  lazy val scodec = SbtCommunityProject(
    project          = "scodec",
    sbtTestCommand   = "unitTests/test",
    // Adds <empty> package
    sbtDocCommand   = "coreJVM/doc",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Ysafe-init"),
    dependencies = List(munit, scodecBits),
  )

  lazy val scalaParserCombinators = SbtCommunityProject(
    project          = "scala-parser-combinators",
    sbtTestCommand   = "set every versionPolicyIntention := Compatibility.None; parserCombinatorsJVM/test",
    sbtDocCommand   = forceDoc("parserCombinatorsJVM"),
  )

  lazy val dottyCpsAsync = SbtCommunityProject(
    project          = "dotty-cps-async",
    sbtTestCommand   = "test",
    // Does not compile (before reaches doc)
    // sbtDocCommand = "cpsJVM/doc",
  )

  lazy val scalaz = SbtCommunityProject(
    project          = "scalaz",
    sbtTestCommand   = "rootJVM/test",

    // sbtDocCommand = forceDoc("coreJVM"), // Fails with:
    // [error] class scalaz.Conts cannot be unpickled because no class file was found
    // [error] class scalaz.ContsT cannot be unpickled because no class file was found
    // [error] class scalaz.IndexedCont cannot be unpickled because no class file was found

    // aggregateDoc("rootJVM")("effectJVM", "iterateeJVM"), // Fails With
    // [error] Caused by: java.lang.AssertionError: assertion failed:
    // trait MonadIO has non-class parent: AppliedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module scalaz),Monad),List(TypeRef(ThisType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scalaz)),module effect),trait MonadIO)),type F)))

    // forceDoc("iterateeJVM"), // Fails with
    // [error] class scalaz.iteratee.Iteratee cannot be unpickled because no class file was found

    sbtDocCommand = forceDoc("effectJVM"),
    dependencies     = List(scalacheck)
  )

  lazy val endpoints4s = SbtCommunityProject(
    project        = "endpoints4s",
    sbtTestCommand = "json-schemaJVM/compile;algebraJVM/compile;openapiJVM/compile;http4s-server/compile;http4s-client/compile;play-server/compile;play-client/compile;akka-http-server/compile;akka-http-client/compile",
    sbtDocCommand = ";json-schemaJVM/doc ;algebraJVM/doc; openapiJVM/doc; http4s-server/doc ;http4s-client/doc ;play-server/doc ;play-client/doc ;akka-http-server/doc ;akka-http-client/doc",
  )

  lazy val catsEffect3 = SbtCommunityProject(
    project        = "cats-effect-3",
    sbtTestCommand = "ciJVM",
    sbtPublishCommand = "publishLocal",
    sbtDocCommand  = ";coreJVM/doc ;lawsJVM/doc ;kernelJVM/doc",
    dependencies   = List(cats, coop, disciplineSpecs2, scalacheck)
  )

  lazy val scalaParallelCollections = SbtCommunityProject(
    project        = "scala-parallel-collections",
    sbtTestCommand = "test",
    sbtDocCommand  = forceDoc("core"),
    dependencies   = List(scalacheck)
  )

  lazy val scalaCollectionCompat = SbtCommunityProject(
    project        = "scala-collection-compat",
    sbtTestCommand = "compat30/test",
    sbtPublishCommand = "compat30/publishLocal",
  )

  lazy val scalaJava8Compat = SbtCommunityProject(
    project        = "scala-java8-compat",
    // the fnGen subproject must be built with 2.12.x
    sbtTestCommand = s"++2.12.14; ++$compilerVersion; set fnGen/dependencyOverrides := Nil; test",
    sbtPublishCommand = s"++2.12.14; ++$compilerVersion; set fnGen/dependencyOverrides := Nil; publishLocal",
    scalacOptions = Nil // avoid passing Scala 3 options to Scala 2.12 in fnGen subproject
  )

  lazy val verify = SbtCommunityProject(
    project        = "verify",
    sbtTestCommand = "verifyJVM/test",
    sbtDocCommand = "verifyJVM/doc",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Xcheck-macros") // TODO enable -Xcheck-macros
  )

  lazy val discipline = SbtCommunityProject(
    project = "discipline",
    sbtTestCommand = "coreJVM/test;coreJS/test",
    sbtPublishCommand = "set every credentials := Nil;coreJVM/publishLocal;coreJS/publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Ysafe-init"),
    dependencies = List(scalacheck)
  )

  lazy val disciplineMunit = SbtCommunityProject(
    project = "discipline-munit",
    sbtTestCommand = "coreJVM/test;coreJS/test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal",
    dependencies = List(discipline, munit)
  )

  lazy val disciplineSpecs2 = SbtCommunityProject(
    project = "discipline-specs2",
    sbtTestCommand = "test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal",
    dependencies = List(discipline),
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Ysafe-init")
  )

  lazy val simulacrumScalafixAnnotations = SbtCommunityProject(
    project = "simulacrum-scalafix",
    sbtTestCommand = "annotation/test:compile;annotationJS/test:compile",
    sbtPublishCommand = "annotation/publishLocal;annotationJS/publishLocal",
  )

  lazy val cats = SbtCommunityProject(
    project = "cats",
    sbtTestCommand = "set Global/scalaJSStage := FastOptStage;buildJVM;validateAllJS",
    sbtPublishCommand = "catsJVM/publishLocal;catsJS/publishLocal",
    dependencies = List(discipline, disciplineMunit, scalacheck, simulacrumScalafixAnnotations),
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Ysafe-init") // disable -Ysafe-init, due to -Xfatal-warning

  )

  lazy val catsMtl = SbtCommunityProject(
    project = "cats-mtl",
    sbtTestCommand = "testsJVM/test;testsJS/test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal;lawsJVM/publishLocal;lawsJS/publishLocal",
    dependencies = List(cats, disciplineMunit)
  )

  lazy val coop = SbtCommunityProject(
    project = "coop",
    sbtTestCommand = "test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal",
    dependencies = List(cats, catsMtl)
  )

  // 'Sciss/Lucre' with its dependencies:

  lazy val scissEqual = SbtCommunityProject(
    project           = "Equal",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
    dependencies      = List(scalatest),
  )

  lazy val scissFingerTree = SbtCommunityProject(
    project           = "FingerTree",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
    dependencies      = List(scalatest),
  )

  lazy val scissLog = SbtCommunityProject(
    project           = "Log",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
  )

  lazy val scissModel = SbtCommunityProject(
    project           = "Model",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
    dependencies      = List(scalatest),
  )

  lazy val scissNumbers = SbtCommunityProject(
    project           = "Numbers",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
    dependencies      = List(scalatest),
  )

  lazy val scissSerial = SbtCommunityProject(
    project           = "Serial",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
    dependencies      = List(scalatest),
  )

  lazy val scissAsyncFile = SbtCommunityProject(
    project           = "AsyncFile",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
    dependencies      = List(scissLog, scalatest),
  )

  lazy val scissSpan = SbtCommunityProject(
    project           = "Span",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
    dependencies      = List(scissSerial, scalatest),
  )

  lazy val scalaSTM = SbtCommunityProject(
    project           = "scala-stm",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
    dependencies      = List(scalatestplusJunit),
  )

  lazy val scissLucre = SbtCommunityProject(
    project           = "Lucre",
    sbtTestCommand    = "adjunctJVM/test;baseJVM/test;confluentJVM/test;coreJVM/test;dataJVM/test;exprJVM/test;geomJVM/test;lucre-bdb/test;testsJVM/test",
    extraSbtArgs      = List("-Dde.sciss.lucre.ShortTests=true"),
    sbtPublishCommand = "adjunctJVM/publishLocal;baseJVM/publishLocal;confluentJVM/publishLocal;coreJVM/publishLocal;dataJVM/publishLocal;exprJVM/publishLocal;geomJVM/publishLocal;lucre-bdb/publishLocal",
    dependencies      = List(scalaSTM, scissAsyncFile, scissEqual, scissFingerTree, scissLog, scissModel, scissNumbers, scissSerial, scissSpan, scalatest),
  )

  lazy val izumiReflect = SbtCommunityProject(
    project = "izumi-reflect",
    sbtTestCommand = """set Global / testOptions += Tests.Filter(name => !name.endsWith("BasicDottyTest") && !name.endsWith("LightTypeTagTest")); test""",
    sbtPublishCommand = "publishLocal",
    dependencies = List(scalatest)
  )

  lazy val perspective = SbtCommunityProject(
    project = "perspective",
    // No library with easy typeclasses to verify data against exist for Dotty, so no tests yet
    // Until then I guess this mainly serves to check that it still compiles at all
    sbtTestCommand = "dottyPerspectiveExamples/compile",
    dependencies = List(cats)
  )

  lazy val akka = SbtCommunityProject(
    project = "akka",
    extraSbtArgs = List(s"-Dakka.build.scalaVersion=$compilerVersion"),
    sbtTestCommand = List(
      "set every targetSystemJdk := true",
      // selectively disable -Xfatal-warnings due to deprecations
      """set actor/Compile/scalacOptions -= "-Xfatal-warnings"""",
      """set testkit/Compile/scalacOptions -= "-Xfatal-warnings"""",
      """set actorTests/Compile/scalacOptions -= "-Xfatal-warnings"""",
      "akka-actor-tests/Test/compile",
    ).mkString("; "),
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Ysafe-init"),
    dependencies = List(scalatest, scalatestplusJunit, scalatestplusScalacheck)
  )

  lazy val monocle = SbtCommunityProject(
    project = "Monocle",
    sbtTestCommand = "coreJVM/test; macrosJVM/test; testJVM/test",
    dependencies = List(cats, munit, discipline, disciplineMunit)
  )

  lazy val protoquill = SbtCommunityProject(
    project = "protoquill",
    extraSbtArgs  = List("-Dcommunity=true", "-DcommunityRemote=true", "-Dquill.macro.stdout=true"),
    sbtTestCommand = "runCommunityBuild",
    sbtPublishCommand = "publishLocal",
    dependencies = List(scalatest),
    scalacOptions = List("-language:implicitConversions"), // disabled -Ysafe-init, due to bug in macro
  )

  lazy val onnxScala = SbtCommunityProject(
    project = "onnx-scala",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
    dependencies = List(scalatest)
  )

  lazy val playJson = SbtCommunityProject(
    project = "play-json",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
    dependencies = List(scalatest, scalatestplusScalacheck),
  )

  lazy val munitCatsEffect = SbtCommunityProject(
    project = "munit-cats-effect",
    sbtTestCommand = "ce3JVM/test; ce3JS/test",
    sbtPublishCommand = "ce3JVM/publishLocal; ce3JS/publishLocal",
    dependencies = List(munit, catsEffect3)
  )

  lazy val scalacheckEffect = SbtCommunityProject(
    project = "scalacheck-effect",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
    dependencies = List(cats, catsEffect3, munit, scalacheck)
  )

  lazy val fs2 = SbtCommunityProject(
    project = "fs2",
    sbtTestCommand = "coreJVM/test; coreJS/test",  // io/test requires JDK9+
    sbtPublishCommand = "coreJVM/publishLocal; coreJS/publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Ysafe-init"),
    dependencies = List(cats, catsEffect3, munitCatsEffect, scalacheckEffect, scodecBits)
  )

  lazy val libretto = SbtCommunityProject(
    project = "libretto",
    sbtTestCommand = "core/test; examples/compile",
    sbtPublishCommand = "core/publishLocal; examples/publishLocal",
    dependencies = List(scalatest)
  )

  lazy val jacksonModuleScala = SbtCommunityProject(
    project = "jackson-module-scala",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
    dependencies = List(scalaJava8Compat, scalatest)
  )

  lazy val specs2 = SbtCommunityProject(
    project = "specs2",
    sbtTestCommand = "core/testOnly -- exclude ci",
    sbtPublishCommand = "core/publishLocal",
    dependencies = List()
  )

  lazy val spire = SbtCommunityProject(
    project = "spire",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Xcheck-macros"),
    dependencies = List(cats, disciplineMunit)
  )

  lazy val http4s = SbtCommunityProject(
    project = "http4s",
    sbtTestCommand = "tests/test; server/test; client/test; ember-core/test; ember-server/test; ember-client/test; circe/test",
    sbtPublishCommand = "publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Ysafe-init"),
    dependencies = List(cats, catsEffect3, fs2, disciplineMunit, scalacheckEffect)
  )

  lazy val parboiled2 = SbtCommunityProject(
    project = "parboiled2",
    sbtTestCommand = "parboiledCoreJVM/test; parboiledJVM/test",
    sbtPublishCommand = "publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Xcheck-macros"),
    dependencies = List(utest, scalacheck)
  )

end projects

def allProjects = List(
  projects.utest,
  projects.sourcecode,
  projects.oslib,
  projects.oslibWatch,
  projects.ujson,
  projects.upickle,
  projects.upickleCore,
  projects.upickleImplicits,
  projects.upack,
  projects.geny,
  projects.fansi,
  projects.pprint,
  projects.requests,
  projects.cask,
  projects.scas,
  projects.intent,
  projects.scalacheck,
  projects.scalatest,
  projects.scalatestplusScalacheck,
  projects.scalatestplusJunit,
  projects.scalaXml,
  projects.scalap,
  projects.betterfiles,
  projects.scalaPB,
  projects.minitest,
  projects.fastparse,
  projects.stdLib213,
  projects.shapeless,
  projects.xmlInterpolator,
  projects.effpi,
  projects.sconfig,
  projects.zio,
  projects.munit,
  projects.scodecBits,
  projects.scodec,
  projects.scalaParserCombinators,
  projects.dottyCpsAsync,
  projects.scalaz,
  projects.endpoints4s,
  projects.catsEffect3,
  projects.scalaParallelCollections,
  projects.scalaCollectionCompat,
  projects.scalaJava8Compat,
  projects.verify,
  projects.discipline,
  projects.disciplineMunit,
  projects.disciplineSpecs2,
  projects.simulacrumScalafixAnnotations,
  projects.cats,
  projects.catsMtl,
  projects.coop,
  projects.scissEqual,
  projects.scissFingerTree,
  projects.scissLog,
  projects.scissModel,
  projects.scissNumbers,
  projects.scissSerial,
  projects.scissAsyncFile,
  projects.scissSpan,
  projects.scalaSTM,
  projects.scissLucre,
  projects.izumiReflect,
  projects.perspective,
  projects.akka,
  projects.monocle,
  projects.protoquill,
  projects.onnxScala,
  projects.playJson,
  projects.scalatestplusTestNG,
  projects.munitCatsEffect,
  projects.scalacheckEffect,
  projects.fs2,
  projects.libretto,
  projects.jacksonModuleScala,
  projects.specs2,
  projects.coop,
  projects.spire,
  projects.http4s,
  projects.parboiled2,
)

lazy val projectMap = allProjects.groupBy(_.project)
