import java.io.File
import java.nio.file._

import Process._
import Modes._
import ScaladocGeneration._
import com.jsuereth.sbtpgp.PgpKeys
import sbt.Keys._
import sbt._
import complete.DefaultParsers._
import pl.project13.scala.sbt.JmhPlugin
import pl.project13.scala.sbt.JmhPlugin.JmhKeys.Jmh
import sbt.Package.ManifestAttributes
import sbt.PublishBinPlugin.autoImport._
import sbt.plugins.SbtPlugin
import sbt.ScriptedPlugin.autoImport._
import xerial.sbt.pack.PackPlugin
import xerial.sbt.pack.PackPlugin.autoImport._
import xerial.sbt.Sonatype.autoImport._
import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbtbuildinfo.BuildInfoPlugin
import sbtbuildinfo.BuildInfoPlugin.autoImport._
import sbttastymima.TastyMiMaPlugin
import sbttastymima.TastyMiMaPlugin.autoImport._

import scala.util.Properties.isJavaAtLeast
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.linker.interface.{ModuleInitializer, StandardConfig}

object DottyJSPlugin extends AutoPlugin {
  import Build._

  object autoImport {
    val switchToESModules: StandardConfig => StandardConfig =
      config => config.withModuleKind(ModuleKind.ESModule)
  }

  val writePackageJSON = taskKey[Unit](
      "Write package.json to configure module type for Node.js")

  override def requires: Plugins = ScalaJSPlugin

  override def projectSettings: Seq[Setting[_]] = Def.settings(
    commonBootstrappedSettings,

    /* #11709 Remove the dependency on scala3-library that ScalaJSPlugin adds.
     * Instead, in this build, we use `.dependsOn` relationships to depend on
     * the appropriate, locally-defined, scala3-library-bootstrappedJS.
     */
    libraryDependencies ~= {
      _.filter(!_.name.startsWith("scala3-library_sjs1"))
    },

    // Replace the JVM JUnit dependency by the Scala.js one
    libraryDependencies ~= {
      _.filter(!_.name.startsWith("junit-interface"))
    },
    libraryDependencies +=
      ("org.scala-js" %% "scalajs-junit-test-runtime" % scalaJSVersion  % "test").cross(CrossVersion.for3Use2_13),

    // Typecheck the Scala.js IR found on the classpath
    scalaJSLinkerConfig ~= (_.withCheckIR(true)),

    Compile / jsEnvInput := (Compile / jsEnvInput).dependsOn(writePackageJSON).value,
    Test / jsEnvInput := (Test / jsEnvInput).dependsOn(writePackageJSON).value,

    writePackageJSON := {
      val packageType = scalaJSLinkerConfig.value.moduleKind match {
        case ModuleKind.NoModule       => "commonjs"
        case ModuleKind.CommonJSModule => "commonjs"
        case ModuleKind.ESModule       => "module"
      }

      val path = target.value / "package.json"

      IO.write(path, s"""{"type": "$packageType"}\n""")
    },
  )
}

object Build {
  import ScaladocConfigs._

  val referenceVersion = "3.4.0"

  val baseVersion = "3.5.0-RC1"

  // LTS or Next
  val versionLine = "Next"

  // Versions used by the vscode extension to create a new project
  // This should be the latest published releases.
  // TODO: Have the vscode extension fetch these numbers from the Internet
  // instead of hardcoding them ?
  val publishedDottyVersion = referenceVersion
  val sbtDottyVersion = "0.5.5"

  /** Version against which we check binary compatibility.
   *
   *  This must be the latest published release in the same versioning line.
   *  For example, if the next version is going to be 3.1.4, then this must be
   *  set to 3.1.3. If it is going to be 3.1.0, it must be set to the latest
   *  3.0.x release.
   */
  val previousDottyVersion = "3.4.0"

  /** Version against which we check binary compatibility. */
  val ltsDottyVersion = "3.3.0"

  object CompatMode {
    final val BinaryCompatible = 0
    final val SourceAndBinaryCompatible = 1
  }

  val compatMode = {
    val VersionRE = """^\d+\.(\d+).(\d+).*""".r
    baseVersion match {
      case VersionRE(_, "0")   => CompatMode.BinaryCompatible
      case _                   => CompatMode.SourceAndBinaryCompatible
    }
  }

  /** scala-library version required to compile Dotty.
   *
   *  Both the non-bootstrapped and bootstrapped version should match, unless
   *  we're in the process of upgrading to a new major version of
   *  scala-library.
   */
  def stdlibVersion(implicit mode: Mode): String = mode match {
    case NonBootstrapped => "2.13.12"
    case Bootstrapped => "2.13.12"
  }

  /** Version of the scala-library for which we will generate TASTy.
   *
   *  We should never use a nightly version here to release.
   *
   *  We can use nightly versions to tests the future compatibility in development.
   *  Nightly versions: https://scala-ci.typesafe.com/ui/native/scala-integration/org/scala-lang
   */
  val stdlibBootstrappedVersion = "2.13.12"

  val dottyOrganization = "org.scala-lang"
  val dottyGithubUrl = "https://github.com/scala/scala3"
  val dottyGithubRawUserContentUrl = "https://raw.githubusercontent.com/scala/scala3"


  val isRelease = sys.env.get("RELEASEBUILD") == Some("yes")

  val dottyVersion = {
    def isNightly = sys.env.get("NIGHTLYBUILD") == Some("yes")
    if (isRelease)
      baseVersion
    else if (isNightly)
      baseVersion + "-bin-" + VersionUtil.commitDate + "-" + VersionUtil.gitHash + "-NIGHTLY"
    else
      baseVersion + "-bin-SNAPSHOT"
  }
  val dottyNonBootstrappedVersion = {
    // Make sure sbt always computes the scalaBinaryVersion correctly
    val bin = if (!dottyVersion.contains("-bin")) "-bin" else ""
    dottyVersion + bin + "-nonbootstrapped"
  }

  val sbtCommunityBuildVersion = "0.1.0-SNAPSHOT"

  val agentOptions = List(
    // "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
    // "-agentpath:/home/dark/opt/yjp-2013-build-13072/bin/linux-x86-64/libyjpagent.so"
    // "-agentpath:/Applications/YourKit_Java_Profiler_2015_build_15052.app/Contents/Resources/bin/mac/libyjpagent.jnilib",
    // "-XX:+HeapDumpOnOutOfMemoryError", "-Xmx1g", "-Xss2m"
  )

  // Packages all subprojects to their jars
  val packageAll = taskKey[Map[String, String]]("Package everything needed to run tests")

  // Run tests with filter through vulpix test suite
  val testCompilation = inputKey[Unit]("runs integration test with the supplied filter")

  sealed trait Scala2Library
  // Use Scala 2 compiled library JAR
  object Scala2LibraryJar extends Scala2Library
  // Use the TASTy jar from `scala2-library-tasty` in the classpath
  // This only works with `scala3-bootstrapped/scalac` and tests in `scala3-bootstrapped`
  //
  object Scala2LibraryTasty extends Scala2Library
  // Use the TASTy jar from `scala2-library-cc-tasty` in the classpath
  // This only works with `scala3-bootstrapped/scalac` and tests in `scala3-bootstrapped`
  //
  object Scala2LibraryCCTasty extends Scala2Library

  // Set in SBT with:
  //   - `set ThisBuild/Build.scala2Library := Build.Scala2LibraryJar` (default)
  //   - `set ThisBuild/Build.scala2Library := Build.Scala2LibraryTasty`
  //   - `set ThisBuild/Build.scala2Library := Build.Scala2LibraryCCTasty`
  val scala2Library = settingKey[Scala2Library]("Choose which version of the Scala 2 library should be used")

  // Used to compile files similar to ./bin/scalac script
  val scalac = inputKey[Unit]("run the compiler using the correct classpath, or the user supplied classpath")

  // Used to run binaries similar to ./bin/scala script
  val scala = inputKey[Unit]("run compiled binary using the correct classpath, or the user supplied classpath")

  val repl = taskKey[Unit]("spawns a repl with the correct classpath")

  // Compiles the documentation and static site
  val genDocs = inputKey[Unit]("run scaladoc to generate static documentation site")

  // Settings used to configure the test language server
  val ideTestsCompilerVersion = taskKey[String]("Compiler version to use in IDE tests")
  val ideTestsCompilerArguments = taskKey[Seq[String]]("Compiler arguments to use in IDE tests")
  val ideTestsDependencyClasspath = taskKey[Seq[File]]("Dependency classpath to use in IDE tests")

  val fetchScalaJSSource = taskKey[File]("Fetch the sources of Scala.js")

  lazy val SourceDeps = config("sourcedeps")

  // Settings shared by the build (scoped in ThisBuild). Used in build.sbt
  lazy val thisBuildSettings = Def.settings(
    organization := dottyOrganization,
    organizationName := "LAMP/EPFL",
    organizationHomepage := Some(url("http://lamp.epfl.ch")),

    // Note: bench/profiles/projects.yml should be updated accordingly.
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      //"-Wconf:cat=deprecation&msg=Unsafe:s",    // example usage
      "-Xfatal-warnings",                         // -Werror in modern usage
      "-encoding", "UTF8",
      "-language:implicitConversions",
    ),

    (Compile / compile / javacOptions) ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),

    // Avoid various sbt craziness involving classloaders and parallelism
    run / fork := true,
    Test / fork := true,
    Test / parallelExecution := false,

    outputStrategy := Some(StdoutOutput),

    scala2Library := Scala2LibraryJar,

    // enable verbose exception messages for JUnit
    (Test / testOptions) += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s"),
  )

  // Settings shared globally (scoped in Global). Used in build.sbt
  lazy val globalSettings = Def.settings(
    onLoad := (Global / onLoad).value andThen { state =>
      def exists(submodule: String) = {
        val path = Paths.get(submodule)
        Files.exists(path) && {
          val fileStream = Files.list(path)
          try fileStream.iterator().hasNext
          finally fileStream.close()
        }
      }

      // Copy default configuration from .vscode-template/ unless configuration files already exist in .vscode/
      sbt.IO.copyDirectory(new File(".vscode-template/"), new File(".vscode/"), overwrite = false)

      state
    },

    // I find supershell more distracting than helpful
    useSuperShell := false,

    // Credentials to release to Sonatype
    credentials ++= (
      for {
        username <- sys.env.get("SONATYPE_USER")
        password <- sys.env.get("SONATYPE_PW")
      } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
    ).toList,
    PgpKeys.pgpPassphrase := sys.env.get("PGP_PW").map(_.toCharArray()),
    PgpKeys.useGpgPinentry := true,

    javaOptions ++= {
      val ciOptions = // propagate if this is a CI build
        sys.props.get("dotty.drone.mem") match {
          case Some(prop) => List("-Xmx" + prop)
          case _ => List()
        }
      // Do not cut off the bottom of large stack traces (default is 1024)
      "-XX:MaxJavaStackTraceDepth=1000000" :: agentOptions ::: ciOptions
    },

    excludeLintKeys ++= Set(
      // We set these settings in `commonSettings`, if a project
      // uses `commonSettings` but overrides `unmanagedSourceDirectories`,
      // sbt will complain if we don't exclude them here.
      Keys.scalaSource, Keys.javaSource
    ),

    // This is used to download nightly builds of the Scala 2 library in `scala2-library-bootstrapped`
    resolvers += "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/",
  )

  lazy val disableDocSetting =
      // This is a legacy settings, we should reevalute generating javadocs
      Compile / doc / sources := Seq()

  lazy val commonSettings = publishSettings ++ Seq(
    (Compile / scalaSource)    := baseDirectory.value / "src",
    (Test / scalaSource)       := baseDirectory.value / "test",
    (Compile / javaSource)    := baseDirectory.value / "src",
    (Test / javaSource)       := baseDirectory.value / "test",
    (Compile / resourceDirectory)    := baseDirectory.value / "resources",
    (Test / resourceDirectory)       := baseDirectory.value / "test-resources",

    // Prevent sbt from rewriting our dependencies
    scalaModuleInfo ~= (_.map(_.withOverrideScalaVersion(false))),

    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test,

    // If someone puts a source file at the root (e.g., for manual testing),
    // don't pick it up as part of any project.
    sourcesInBase := false,

    // For compatibility with Java 9+ module system;
    // without Automatic-Module-Name, the module name is derived from the jar file which is invalid because of the _3 suffix.
    Compile / packageBin / packageOptions +=
      Package.ManifestAttributes(
        "Automatic-Module-Name" -> s"${dottyOrganization.replaceAll("-",".")}.${moduleName.value.replaceAll("-",".")}"
      )
  )

  // Settings used for projects compiled only with Java
  lazy val commonJavaSettings = commonSettings ++ Seq(
    version := dottyVersion,
    scalaVersion := referenceVersion,
    // Do not append Scala versions to the generated artifacts
    crossPaths := false,
    // Do not depend on the Scala library
    autoScalaLibrary := false,
    disableDocSetting
  )

  // Settings used when compiling dotty (both non-bootstrapped and bootstrapped)
  lazy val commonDottySettings = commonSettings ++ Seq(
    // Manually set the standard library to use
    autoScalaLibrary := false,
    classpathOptions ~= (old =>
      old
        .withAutoBoot(false)      // no library on the compiler bootclasspath - we may need a more recent version
        .withFilterLibrary(false) // ...instead, we put it on the compiler classpath
    ),
  )

  lazy val commonScala2Settings = commonSettings ++ Seq(
    scalaVersion := stdlibVersion(Bootstrapped),
    moduleName ~= { _.stripSuffix("-scala2") },
    version := dottyVersion,
    target := baseDirectory.value / ".." / "out" / "scala-2" / name.value,

    disableDocSetting
  )

  // Settings used when compiling dotty with the reference compiler
  lazy val commonNonBootstrappedSettings = commonDottySettings ++ Seq(
    (Compile / unmanagedSourceDirectories) += baseDirectory.value / "src-non-bootstrapped",

    version := dottyNonBootstrappedVersion,
    scalaVersion := referenceVersion,

    disableDocSetting
  )

  private lazy val currentYear: String = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR).toString

  def scalacOptionsDocSettings(includeExternalMappings: Boolean = true) = {
    val extMap = Seq("-external-mappings:" +
        (if (includeExternalMappings) ".*scala/.*::scaladoc3::https://dotty.epfl.ch/api/," else "") +
        ".*java/.*::javadoc::https://docs.oracle.com/javase/8/docs/api/")
    Seq(
      "-skip-by-regex:.+\\.internal($|\\..+)",
      "-skip-by-regex:.+\\.impl($|\\..+)",
      "-project-logo", "docs/_assets/images/logo.svg",
      "-social-links:" +
        "github::https://github.com/scala/scala3," +
        "discord::https://discord.com/invite/scala," +
        "twitter::https://twitter.com/scala_lang",
      // contains special definitions which are "transplanted" elsewhere
      // and which therefore confuse Scaladoc when accessed from this pkg
      "-skip-by-id:scala.runtime.stdLibPatches",
      // MatchCase is a special type that represents match type cases,
      // Reflect doesn't expect to see it as a standalone definition
      // and therefore it's easier just not to document it
      "-skip-by-id:scala.runtime.MatchCase",
      "-skip-by-id:dotty.tools.tasty",
      "-skip-by-id:dotty.tools.tasty.util",
      "-project-footer", s"Copyright (c) 2002-$currentYear, LAMP/EPFL",
      "-author",
      "-groups",
      "-default-template", "static-site-main"
    ) ++ extMap
  }

  // Settings used when compiling dotty with a non-bootstrapped dotty
  lazy val commonBootstrappedSettings = commonDottySettings ++ NoBloopExport.settings ++ Seq(
    // To enable support of scaladoc and language-server projects you need to change this to true and use sbt as your build server
    bspEnabled := false,
    (Compile / unmanagedSourceDirectories) += baseDirectory.value / "src-bootstrapped",

    version := dottyVersion,
    scalaVersion := dottyNonBootstrappedVersion,

    scalaCompilerBridgeBinaryJar := {
      Some((`scala3-sbt-bridge` / Compile / packageBin).value)
    },

    // Use the same name as the non-bootstrapped projects for the artifacts.
    // Remove the `js` suffix because JS artifacts are published using their special crossVersion.
    // The order of the two `stripSuffix`es is important, so that
    // scala3-library-bootstrappedjs becomes scala3-library.
    moduleName ~= { _.stripSuffix("js").stripSuffix("-bootstrapped") },

    // Enforce that the only Scala 2 classfiles we unpickle come from scala-library
    /*
    scalacOptions ++= {
      val cp = (dependencyClasspath in `scala3-library` in Compile).value
      val scalaLib = findArtifactPath(cp, "scala-library")
      Seq("-Yscala2-unpickler", scalaLib)
    },
    */

    // sbt gets very unhappy if two projects use the same target
    target := baseDirectory.value / ".." / "out" / "bootstrap" / name.value,

    // Compile using the non-bootstrapped and non-published dotty
    managedScalaInstance := false,
    scalaInstance := {
      val externalLibraryDeps = (`scala3-library` / Compile / externalDependencyClasspath).value.map(_.data).toSet
      val externalCompilerDeps = (`scala3-compiler` / Compile / externalDependencyClasspath).value.map(_.data).toSet

      // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
      // just directories containing classfiles because sbt maintains a cache of
      // compiler instances. This cache is invalidated based on timestamps
      // however this is only implemented on jars, directories are never
      // invalidated.
      val tastyCore = (`tasty-core` / Compile / packageBin).value
      val scala3Library = (`scala3-library` / Compile / packageBin).value
      val scala3Interfaces = (`scala3-interfaces` / Compile / packageBin).value
      val scala3Compiler = (`scala3-compiler` / Compile / packageBin).value

      val libraryJars = Array(scala3Library) ++ externalLibraryDeps
      val compilerJars = Seq(tastyCore, scala3Interfaces, scala3Compiler) ++ (externalCompilerDeps -- externalLibraryDeps)

      Defaults.makeScalaInstance(
        scalaVersion.value,
        libraryJars = libraryJars,
        allCompilerJars = compilerJars,
        allDocJars = Seq.empty,
        state.value,
        scalaInstanceTopLoader.value
      )
    },
    // We cannot include scaladoc in the regular `scalaInstance` task because
    // it's a bootstrapped-only project, so we would run into a loop since we
    // need the output of that task to compile scaladoc. But we can include it
    // in the `scalaInstance` of the `doc` task which allows us to run
    // `scala3-library-bootstrapped/doc` for example.
    doc / scalaInstance := {
      val externalDeps = (LocalProject("scaladoc") / Compile / externalDependencyClasspath).value.map(_.data)
      val scalaDoc = (LocalProject("scaladoc") / Compile / packageBin).value
      val docJars = Array(scalaDoc) ++ externalDeps

      val base = scalaInstance.value
      val docScalaInstance = Defaults.makeScalaInstance(
        version = base.version,
        libraryJars = base.libraryJars,
        allCompilerJars = base.compilerJars,
        allDocJars = docJars,
        state.value,
        scalaInstanceTopLoader.value
      )
      // assert that sbt reuses the same compiler class loader
      assert(docScalaInstance.loaderCompilerOnly == base.loaderCompilerOnly)
      docScalaInstance
    },
    Compile / doc / scalacOptions ++= scalacOptionsDocSettings()
  )

  lazy val commonBenchmarkSettings = Seq(
    Jmh / bspEnabled := false,
    Jmh / run / mainClass := Some("dotty.tools.benchmarks.Bench"), // custom main for jmh:run
    javaOptions += "-DBENCH_COMPILER_CLASS_PATH=" + Attributed.data((`scala3-bootstrapped` / Compile / fullClasspath).value).mkString("", File.pathSeparator, ""),
    javaOptions += "-DBENCH_CLASS_PATH=" + Attributed.data((`scala3-library-bootstrapped` / Compile / fullClasspath).value).mkString("", File.pathSeparator, "")
  )

  lazy val commonMiMaSettings = Def.settings(
    mimaPreviousArtifacts += {
      val thisProjectID = projectID.value
      val crossedName = thisProjectID.crossVersion match {
        case cv: Disabled => thisProjectID.name
        case cv: Binary => s"${thisProjectID.name}_${cv.prefix}3${cv.suffix}"
      }
      (thisProjectID.organization % crossedName % previousDottyVersion)
    },

    mimaCheckDirection := (compatMode match {
      case CompatMode.BinaryCompatible          => "backward"
      case CompatMode.SourceAndBinaryCompatible => "both"
    }),

    mimaExcludeAnnotations += "scala.annotation.experimental",
  )

  /** Projects -------------------------------------------------------------- */

  val dottyCompilerBootstrappedRef = LocalProject("scala3-compiler-bootstrapped")

  /** External dependencies we may want to put on the compiler classpath. */
  def externalCompilerClasspathTask: Def.Initialize[Task[Def.Classpath]] =
    // Even if we're running the non-bootstrapped compiler, we want the
    // dependencies of the bootstrapped compiler since we want to put them on
    // the compiler classpath, not the JVM classpath.
    (dottyCompilerBootstrappedRef / Runtime / externalDependencyClasspath)

  // The root project:
  // - aggregates other projects so that "compile", "test", etc are run on all projects at once.
  // - publishes its own empty artifact "dotty" that depends on "scala3-library" and "scala3-compiler",
  //   this is only necessary for compatibility with sbt which currently hardcodes the "dotty" artifact name
  lazy val scala3 = project.in(file(".")).asDottyRoot(NonBootstrapped)
  lazy val `scala3-bootstrapped` = project.asDottyRoot(Bootstrapped)

  lazy val `scala3-interfaces` = project.in(file("interfaces")).
    settings(commonJavaSettings).
    settings(commonMiMaSettings).
    settings(
      versionScheme := Some("semver-spec"),
      mimaForwardIssueFilters := MiMaFilters.Interfaces.ForwardsBreakingChanges,
      mimaBackwardIssueFilters := MiMaFilters.Interfaces.BackwardsBreakingChanges,
      customMimaReportBinaryIssues("MiMaFilters.Interfaces"),
    )

  /** Find an artifact with the given `name` in `classpath` */
  def findArtifact(classpath: Def.Classpath, name: String): File = classpath
    .find(_.get(artifact.key).exists(_.name == name))
    .getOrElse(throw new MessageOnlyException(s"Artifact for $name not found in $classpath"))
    .data

  /** Like `findArtifact` but returns the absolute path of the entry as a string */
  def findArtifactPath(classpath: Def.Classpath, name: String): String =
    findArtifact(classpath, name).getAbsolutePath

  /** Insert UnsafeNulls Import after package */
  def insertUnsafeNullsImport(lines: Seq[String]): Seq[String] = {
    def recur(ls: Seq[String], foundPackage: Boolean): Seq[String] = ls match {
      case Seq(l, rest @ _*) =>
        val lt = l.trim()
        if (foundPackage) {
          if (!(lt.isEmpty || lt.startsWith("package ")))
            "import scala.language.unsafeNulls" +: ls
          else l +: recur(rest, foundPackage)
        } else {
          if (lt.startsWith("package ")) l +: recur(rest, true)
          else l +: recur(rest, foundPackage)
        }
      case _ => ls
    }
    recur(lines, false)
  }

  /** replace imports of `com.google.protobuf.*` with compiler implemented version */
  def replaceProtobuf(lines: List[String]): List[String] = {
    def recur(ls: List[String]): List[String] = ls match {
      case l :: rest =>
        val lt = l.trim()
        if (lt.isEmpty || lt.startsWith("package ") || lt.startsWith("import ")) {
          val newLine =
            if (lt.startsWith("import com.google.protobuf.")) {
              if (lt == "import com.google.protobuf.CodedInputStream") {
                "import dotty.tools.dotc.semanticdb.internal.SemanticdbInputStream as CodedInputStream"
              } else if (lt == "import com.google.protobuf.CodedOutputStream") {
                "import dotty.tools.dotc.semanticdb.internal.SemanticdbOutputStream as CodedOutputStream"
              } else {
                l
              }
            } else {
              l
            }
          newLine :: recur(rest)
        } else {
          ls // don't check rest of file
        }
      case _ => ls
    }
    recur(lines)
  }

  // Settings shared between scala3-compiler and scala3-compiler-bootstrapped
  lazy val commonDottyCompilerSettings = Seq(
       // Note: bench/profiles/projects.yml should be updated accordingly.
       Compile / scalacOptions ++= Seq("-Yexplicit-nulls", "-Ysafe-init"),

      // Use source 3.3 to avoid fatal migration warnings on scalajs-ir
      scalacOptions ++= Seq("-source", "3.3"),

      // Generate compiler.properties, used by sbt
      (Compile / resourceGenerators) += Def.task {
        import java.util._
        import java.text._
        val file = (Compile / resourceManaged).value / "compiler.properties"
        val dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss")
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"))
        val contents =                //2.11.11.v20170413-090219-8a413ba7cc
          s"""version.number=${version.value}
             |maven.version.number=${version.value}
             |git.hash=${VersionUtil.gitHash}
             |copyright.string=Copyright 2002-$currentYear, LAMP/EPFL
           """.stripMargin

        if (!(file.exists && IO.read(file) == contents)) {
          IO.write(file, contents)
        }

        Seq(file)
      }.taskValue,

      // get libraries onboard
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" % "scala-asm" % "9.6.0-scala-1", // used by the backend
        Dependencies.compilerInterface,
        "org.jline" % "jline-reader" % "3.25.1",   // used by the REPL
        "org.jline" % "jline-terminal" % "3.25.1",
        "org.jline" % "jline-terminal-jna" % "3.25.1", // needed for Windows
        ("io.get-coursier" %% "coursier" % "2.0.16" % Test).cross(CrossVersion.for3Use2_13),
      ),

      // For convenience, change the baseDirectory when running the compiler
      Compile / forkOptions := (Compile / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
      Compile / run / forkOptions := (Compile / run / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
      // And when running the tests
      Test / forkOptions := (Test / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),

      Test / test := {
        // Exclude VulpixMetaTests
        (Test / testOnly).toTask(" -- --exclude-categories=dotty.VulpixMetaTests").value
      },

      (Test / testOptions) += Tests.Argument(
        TestFrameworks.JUnit,
        "--run-listener=dotty.tools.ContextEscapeDetector",
      ),

      // Spawn new JVM in run and test

      // Add git-hash used to package the distribution to the manifest to know it in runtime and report it in REPL
      packageOptions += ManifestAttributes(("Git-Hash", VersionUtil.gitHash)),

      javaOptions ++= {
        val log = streams.value.log
        val managedSrcDir = {
          // Populate the directory
          (Compile / managedSources).value

          (Compile / sourceManaged).value
        }
        val externalDeps = externalCompilerClasspathTask.value
        val jars = packageAll.value

        def libraryPathProperty(jarName: String): Seq[String] =
          jars.get(jarName) match {
            case Some(jar) =>
              Seq(s"-Ddotty.tests.tasties.scalaLibrary=$jar")
            case None =>
              log.warn("Scala 2 library TASTy is ignored on non-bootstrapped compiler")
              Seq.empty
          }
        val scala2LibraryTasty = scala2Library.value match {
          case Scala2LibraryJar => Seq.empty
          case Scala2LibraryTasty => libraryPathProperty("scala2-library-tasty")
          case Scala2LibraryCCTasty => libraryPathProperty("scala2-library-cc-tasty")
        }

        scala2LibraryTasty ++ Seq(
          "-Ddotty.tests.dottyCompilerManagedSources=" + managedSrcDir,
          "-Ddotty.tests.classes.dottyInterfaces=" + jars("scala3-interfaces"),
          "-Ddotty.tests.classes.dottyLibrary=" + jars("scala3-library"),
          "-Ddotty.tests.classes.dottyCompiler=" + jars("scala3-compiler"),
          "-Ddotty.tests.classes.tastyCore=" + jars("tasty-core"),
          "-Ddotty.tests.classes.compilerInterface=" + findArtifactPath(externalDeps, "compiler-interface"),
          "-Ddotty.tests.classes.scalaLibrary=" + findArtifactPath(externalDeps, "scala-library"),
          "-Ddotty.tests.classes.scalaAsm=" + findArtifactPath(externalDeps, "scala-asm"),
          "-Ddotty.tests.classes.jlineTerminal=" + findArtifactPath(externalDeps, "jline-terminal"),
          "-Ddotty.tests.classes.jlineReader=" + findArtifactPath(externalDeps, "jline-reader"),
        )
      },

      javaOptions ++= Seq(
        s"-Ddotty.tools.dotc.semanticdb.test=${(ThisBuild / baseDirectory).value/"tests"/"semanticdb"}",
      ),

      testCompilation := Def.inputTaskDyn {
        val args = spaceDelimited("<arg>").parsed
        if (args.contains("--help")) {
          println(
            s"""
               |usage: testCompilation [--help] [--from-tasty] [--update-checkfiles] [--failed] [<filter>]
               |
               |By default runs tests in dotty.tools.dotc.*CompilationTests and dotty.tools.dotc.coverage.*,
               |excluding tests tagged with dotty.SlowTests.
               |
               |  --help                show this message
               |  --from-tasty          runs tests in dotty.tools.dotc.FromTastyTests
               |  --update-checkfiles   override the checkfiles that did not match with the current output
               |  --failed              re-run only failed tests
               |  <filter>              substring of the path of the tests file
               |
             """.stripMargin
          )
          (Test / testOnly).toTask(" not.a.test")
        }
        else {
          val updateCheckfile = args.contains("--update-checkfiles")
          val rerunFailed = args.contains("--failed")
          val fromTasty = args.contains("--from-tasty")
          val args1 = if (updateCheckfile | fromTasty | rerunFailed) args.filter(x => x != "--update-checkfiles" && x != "--from-tasty" && x != "--failed") else args
          val test = if (fromTasty) "dotty.tools.dotc.FromTastyTests" else "dotty.tools.dotc.*CompilationTests dotty.tools.dotc.coverage.*"
          val cmd = s" $test -- --exclude-categories=dotty.SlowTests" +
            (if (updateCheckfile) " -Ddotty.tests.updateCheckfiles=TRUE" else "") +
            (if (rerunFailed) " -Ddotty.tests.rerunFailed=TRUE" else "") +
            (if (args1.nonEmpty) " -Ddotty.tests.filter=" + args1.mkString(" ") else "")
          (Test / testOnly).toTask(cmd)
        }
      }.evaluated,

      Compile / mainClass := Some("dotty.tools.dotc.Main"),

      scala := {
        val args: List[String] = spaceDelimited("<arg>").parsed.toList
        val externalDeps = externalCompilerClasspathTask.value
        val jars = packageAll.value

        val argsWithDefaultClasspath: List[String] =
          if (args.contains("-classpath")) args
          else insertClasspathInArgs(args, (baseDirectory.value / ".." / "out" / "default-last-scalac-out.jar").getPath)

        val scalaLib = findArtifactPath(externalDeps, "scala-library")
        val dottyLib = jars("scala3-library")

        def run(args: List[String]): Unit = {
          val fullArgs = insertClasspathInArgs(args, List(".", dottyLib, scalaLib).mkString(File.pathSeparator))
          runProcess("java" :: fullArgs, wait = true)
        }

        if (args.isEmpty) {
          println("Couldn't run `scala` without args. Use `repl` to run the repl or add args to run the dotty application")
        } else if (scalaLib == "") {
          println("Couldn't find scala-library on classpath, please run using script in bin dir instead")
        } else if (args.contains("-with-compiler")) {
          val args1 = argsWithDefaultClasspath.filter(_ != "-with-compiler")
          val asm = findArtifactPath(externalDeps, "scala-asm")
          val dottyCompiler = jars("scala3-compiler")
          val dottyStaging = jars("scala3-staging")
          val dottyTastyInspector = jars("scala3-tasty-inspector")
          val dottyInterfaces = jars("scala3-interfaces")
          val tastyCore = jars("tasty-core")
          val compilerInterface = findArtifactPath(externalDeps, "compiler-interface")
          run(insertClasspathInArgs(args1, List(dottyCompiler, dottyInterfaces, asm, dottyStaging, dottyTastyInspector, tastyCore, compilerInterface).mkString(File.pathSeparator)))
        } else run(argsWithDefaultClasspath)
      },

      run := scalac.evaluated,
      scalac := Def.inputTaskDyn {
        val log = streams.value.log
        val externalDeps = externalCompilerClasspathTask.value
        val jars = packageAll.value
        val scalaLib = findArtifactPath(externalDeps, "scala-library")
        val scalaLibTastyOpt = jars.get("scala2-library-tasty")
        val dottyLib = jars("scala3-library")
        val dottyCompiler = jars("scala3-compiler")
        val args0: List[String] = spaceDelimited("<arg>").parsed.toList
        val decompile = args0.contains("-decompile")
        val printTasty = args0.contains("-print-tasty")
        val debugFromTasty = args0.contains("-Ythrough-tasty")
        val defaultOutputDirectory =
          if (printTasty || decompile || debugFromTasty || args0.contains("-d")) Nil else List("-d", (baseDirectory.value / ".." / "out" / "default-last-scalac-out.jar").getPath)
        val args = args0.filter(arg => arg != "-repl" && arg != "-decompile" &&
            arg != "-with-compiler" && arg != "-Ythrough-tasty" && arg != "-print-tasty")
        val main =
          if (decompile) "dotty.tools.dotc.decompiler.Main"
          else if (printTasty) "dotty.tools.dotc.core.tasty.TastyPrinter"
          else if (debugFromTasty) "dotty.tools.dotc.fromtasty.Debug"
          else "dotty.tools.dotc.Main"

        var extraClasspath = Seq(scalaLib, dottyLib)

        scala2Library.value match {
          case Scala2LibraryJar =>
          case Scala2LibraryTasty =>
            jars.get("scala2-library-tasty") match {
              case Some(jar) => extraClasspath :+= jar
              case None => log.warn("Scala2LibraryTasty is ignored on non-bootstrapped compiler")
            };
          case Scala2LibraryCCTasty =>
            jars.get("scala2-library-cc-tasty") match {
              case Some(jar) => extraClasspath :+= jar
              case None => log.warn("Scala2LibraryCCTasty is ignored on non-bootstrapped compiler")
            }
        }

        if (decompile && !args.contains("-classpath"))
          extraClasspath ++= Seq(".")

        if (args0.contains("-with-compiler")) {
          if (scalaVersion.value == referenceVersion) {
            log.error("-with-compiler should only be used with a bootstrapped compiler")
          }
          val dottyInterfaces = jars("scala3-interfaces")
          val dottyStaging = jars("scala3-staging")
          val dottyTastyInspector = jars("scala3-tasty-inspector")
          val tastyCore = jars("tasty-core")
          val asm = findArtifactPath(externalDeps, "scala-asm")
          val compilerInterface = findArtifactPath(externalDeps, "compiler-interface")
          extraClasspath ++= Seq(dottyCompiler, dottyInterfaces, asm, dottyStaging, dottyTastyInspector, tastyCore, compilerInterface)
        }

        val fullArgs = main :: defaultOutputDirectory ::: (if (printTasty) args else insertClasspathInArgs(args, extraClasspath.mkString(File.pathSeparator)))

        (Compile / runMain).toTask(fullArgs.mkString(" ", " ", ""))
      }.evaluated,

      /* Add the sources of scalajs-ir.
       * To guarantee that dotty can bootstrap without depending on a version
       * of scalajs-ir built with a different Scala compiler, we add its
       * sources instead of depending on the binaries.
       */
      ivyConfigurations += SourceDeps.hide,
      transitiveClassifiers := Seq("sources"),
      libraryDependencies +=
        ("org.scala-js" %% "scalajs-ir" % scalaJSVersion % "sourcedeps").cross(CrossVersion.for3Use2_13),
      (Compile / sourceGenerators) += Def.task {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val trgDir = (Compile / sourceManaged).value / "scalajs-ir-src"

        val report = updateClassifiers.value
        val scalaJSIRSourcesJar = report.select(
            configuration = configurationFilter("sourcedeps"),
            module = (_: ModuleID).name.startsWith("scalajs-ir_"),
            artifact = artifactFilter(`type` = "src")).headOption.getOrElse {
          sys.error(s"Could not fetch scalajs-ir sources")
        }

        FileFunction.cached(cacheDir / s"fetchScalaJSIRSource",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info(s"Unpacking scalajs-ir sources to $trgDir...")
          if (trgDir.exists)
            IO.delete(trgDir)
          IO.createDirectory(trgDir)
          IO.unzip(scalaJSIRSourcesJar, trgDir)

          val sjsSources = (trgDir ** "*.scala").get.toSet
          sjsSources.foreach(f => {
            val lines = IO.readLines(f)
            IO.writeLines(f, insertUnsafeNullsImport(lines))
          })
          sjsSources
        } (Set(scalaJSIRSourcesJar)).toSeq
      }.taskValue,
  )

  def insertClasspathInArgs(args: List[String], cp: String): List[String] = {
    val (beforeCp, fromCp) = args.span(_ != "-classpath")
    val classpath = fromCp.drop(1).headOption.fold(cp)(_ + File.pathSeparator + cp)
    "-classpath" :: classpath :: beforeCp ::: fromCp.drop(2)
  }

  lazy val nonBootstrappedDottyCompilerSettings = commonDottyCompilerSettings ++ Seq(
    // packageAll packages all and then returns a map with the abs location
    packageAll := Def.taskDyn { // Use a dynamic task to avoid loops when loading the settings
      Def.task {
        Map(
          "scala3-interfaces"    -> (`scala3-interfaces` / Compile / packageBin).value,
          "scala3-compiler"      -> (Compile / packageBin).value,
          "tasty-core"          -> (`tasty-core` / Compile / packageBin).value,

          // NOTE: Using scala3-library-bootstrapped here is intentional: when
          // running the compiler, we should always have the bootstrapped
          // library on the compiler classpath since the non-bootstrapped one
          // may not be binary-compatible.
          "scala3-library"       -> (`scala3-library-bootstrapped` / Compile / packageBin).value
        ).mapValues(_.getAbsolutePath)
      }
    }.value,

    (Test / testOptions) += Tests.Argument(
      TestFrameworks.JUnit,
      "--exclude-categories=dotty.BootstrappedOnlyTests",
    ),
    // increase stack size for non-bootstrapped compiler, because some code
    // is only tail-recursive after bootstrap
    (Test / javaOptions) += "-Xss2m"
  )

  lazy val bootstrappedDottyCompilerSettings = commonDottyCompilerSettings ++ Seq(
    javaOptions ++= {
      val jars = packageAll.value
      Seq(
        "-Ddotty.tests.classes.dottyStaging=" + jars("scala3-staging"),
        "-Ddotty.tests.classes.dottyTastyInspector=" + jars("scala3-tasty-inspector"),
      )
    },
    packageAll := {
      (`scala3-compiler` / packageAll).value ++ Seq(
        "scala3-compiler" -> (Compile / packageBin).value.getAbsolutePath,
        "scala3-staging"  -> (LocalProject("scala3-staging") / Compile / packageBin).value.getAbsolutePath,
        "scala3-tasty-inspector"  -> (LocalProject("scala3-tasty-inspector") / Compile / packageBin).value.getAbsolutePath,
        "tasty-core"     -> (LocalProject("tasty-core-bootstrapped") / Compile / packageBin).value.getAbsolutePath,
        "scala2-library-tasty" -> (LocalProject("scala2-library-tasty") / Compile / packageBin).value.getAbsolutePath,
        "scala2-library-cc-tasty" -> (LocalProject("scala2-library-cc-tasty") / Compile / packageBin).value.getAbsolutePath,
      )
    },

    repl := (Compile / console).value,
    Compile / console / scalacOptions := Nil, // reset so that we get stock REPL behaviour!  E.g. avoid -unchecked being enabled
  )

  def dottyCompilerSettings(implicit mode: Mode): sbt.Def.SettingsDefinition =
    if (mode == NonBootstrapped) nonBootstrappedDottyCompilerSettings else bootstrappedDottyCompilerSettings

  lazy val `scala3-compiler` = project.in(file("compiler")).asDottyCompiler(NonBootstrapped)

  lazy val Scala3CompilerCoursierTest = config("scala3CompilerCoursierTest") extend Test
  lazy val `scala3-compiler-bootstrapped` = project.in(file("compiler")).asDottyCompiler(Bootstrapped)
    .configs(Scala3CompilerCoursierTest)
    .settings(
      inConfig(Scala3CompilerCoursierTest)(Defaults.testSettings),
      Scala3CompilerCoursierTest / scalaSource := baseDirectory.value / "test-coursier",
      Scala3CompilerCoursierTest / fork := true,
      Scala3CompilerCoursierTest / envVars := Map("DOTTY_BOOTSTRAPPED_VERSION" -> dottyVersion),
      Scala3CompilerCoursierTest / unmanagedClasspath += (Scala3CompilerCoursierTest / scalaSource).value,
      Scala3CompilerCoursierTest / test := ((Scala3CompilerCoursierTest / test) dependsOn (
          publishLocal, // Had to enumarate all deps since calling `scala3-bootstrap` / publishLocal will lead to recursive dependency => stack overflow
          `scala3-interfaces` / publishLocal,
          dottyLibrary(Bootstrapped) / publishLocal,
          tastyCore(Bootstrapped) / publishLocal,
        ),
      ).value,
    )

  def dottyCompiler(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `scala3-compiler`
    case Bootstrapped => `scala3-compiler-bootstrapped`
  }

  // Settings shared between scala3-library, scala3-library-bootstrapped and scala3-library-bootstrappedJS
  lazy val dottyLibrarySettings = Seq(
    (Compile / scalacOptions) ++= Seq(
      // Needed so that the library sources are visible when `dotty.tools.dotc.core.Definitions#init` is called
      "-sourcepath", (Compile / sourceDirectories).value.map(_.getAbsolutePath).distinct.mkString(File.pathSeparator),
      "-Yexplicit-nulls",
    ),
    (Compile / doc / scalacOptions) ++= ScaladocConfigs.DefaultGenerationSettings.value.settings
  )

  lazy val `scala3-library` = project.in(file("library")).asDottyLibrary(NonBootstrapped)
  lazy val `scala3-library-bootstrapped`: Project = project.in(file("library")).asDottyLibrary(Bootstrapped)

  def dottyLibrary(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `scala3-library`
    case Bootstrapped => `scala3-library-bootstrapped`
  }

  /** The dotty standard library compiled with the Scala.js back-end, to produce
   *  the corresponding .sjsir files.
   *
   *  This artifact must be on the classpath on every "Dotty.js" project.
   *
   *  Currently, only a very small fraction of the dotty library is actually
   *  included in this project, and hence available to Dotty.js projects. More
   *  will be added in the future as things are confirmed to be supported.
   */
  lazy val `scala3-library-bootstrappedJS`: Project = project.in(file("library-js")).
    asDottyLibrary(Bootstrapped).
    enablePlugins(DottyJSPlugin).
    settings(
      libraryDependencies +=
        ("org.scala-js" %% "scalajs-library" % scalaJSVersion).cross(CrossVersion.for3Use2_13),
      Compile / unmanagedSourceDirectories ++=
        (`scala3-library-bootstrapped` / Compile / unmanagedSourceDirectories).value,

      // Configure the source maps to point to GitHub for releases
      scalacOptions ++= {
        if (isRelease) {
          val baseURI = (LocalRootProject / baseDirectory).value.toURI
          val dottyVersion = version.value
          Seq(s"-scalajs-mapSourceURI:$baseURI->$dottyGithubRawUserContentUrl/$dottyVersion/")
        } else {
          Nil
        }
      },

      // Make sure `scala3-bootstrapped/test` doesn't fail on this project for no reason
      Test / test := {},
      Test / testOnly := {},
    )

  lazy val tastyCoreSettings = Seq(
    scalacOptions += "-source:3.0-migration"
  )

  lazy val `tasty-core` = project.in(file("tasty")).asTastyCore(NonBootstrapped)
  lazy val `tasty-core-bootstrapped`: Project = project.in(file("tasty")).asTastyCore(Bootstrapped)
  lazy val `tasty-core-scala2`: Project = project.in(file("tasty")).asTastyCoreScala2

  def tastyCore(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `tasty-core`
    case Bootstrapped => `tasty-core-bootstrapped`
  }

  lazy val `scala3-staging` = project.in(file("staging")).
    withCommonSettings(Bootstrapped).
    // We want the compiler to be present in the compiler classpath when compiling this project but not
    // when compiling a project that depends on scala3-staging (see sbt-test/sbt-dotty/quoted-example-project),
    // but we always need it to be present on the JVM classpath at runtime.
    dependsOn(dottyCompiler(Bootstrapped) % "provided; compile->runtime; test->test").
    settings(
      javaOptions := (`scala3-compiler-bootstrapped` / javaOptions).value
    )

  lazy val `scala3-tasty-inspector` = project.in(file("tasty-inspector")).
    withCommonSettings(Bootstrapped).
    // We want the compiler to be present in the compiler classpath when compiling this project but not
    // when compiling a project that depends on scala3-tasty-inspector (see sbt-test/sbt-dotty/tasty-inspector-example-project),
    // but we always need it to be present on the JVM classpath at runtime.
    dependsOn(dottyCompiler(Bootstrapped) % "provided; compile->runtime; test->test").
    settings(
      javaOptions := (`scala3-compiler-bootstrapped` / javaOptions).value
    )

  /** Scala 2 library compiled by dotty using the latest published sources of the library.
   *
   *  This version of the library is not (yet) TASTy/binary compatible with the Scala 2 compiled library.
   */
  lazy val `scala2-library-bootstrapped` = project.in(file("scala2-library-bootstrapped")).
    withCommonSettings(Bootstrapped).
    dependsOn(dottyCompiler(Bootstrapped) % "provided; compile->runtime; test->test").
    settings(commonBootstrappedSettings).
    settings(scala2LibraryBootstrappedSettings).
    settings(moduleName := "scala2-library")

  /** Scala 2 library compiled by dotty using the latest published sources of the library.
   *
   *  This version of the library is not (yet) TASTy/binary compatible with the Scala 2 compiled library.
   */
  lazy val `scala2-library-cc` = project.in(file("scala2-library-cc")).
    withCommonSettings(Bootstrapped).
    dependsOn(dottyCompiler(Bootstrapped) % "provided; compile->runtime; test->test").
    settings(commonBootstrappedSettings).
    settings(scala2LibraryBootstrappedSettings).
    settings(
      moduleName := "scala2-library-cc",
      scalacOptions += "-Ycheck:all",
    )

  lazy val scala2LibraryBootstrappedSettings = Seq(
      javaOptions := (`scala3-compiler-bootstrapped` / javaOptions).value,
      Compile / scalacOptions ++= {
        Seq("-sourcepath", ((Compile/sourceManaged).value / "scala-library-src").toString)
      },
      Compile / doc / scalacOptions += "-Ydocument-synthetic-types",
      scalacOptions += "-Ycompile-scala2-library",
      scalacOptions += "-Yscala2Unpickler:never",
      scalacOptions += "-Yno-experimental",
      scalacOptions -= "-Xfatal-warnings",
      Compile / compile / logLevel := Level.Error,
      ivyConfigurations += SourceDeps.hide,
      transitiveClassifiers := Seq("sources"),
      libraryDependencies +=
        ("org.scala-lang" % "scala-library" % stdlibBootstrappedVersion % "sourcedeps"),
      (Compile / sourceGenerators) += Def.task {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val trgDir = (Compile / sourceManaged).value / "scala-library-src"

        val report = updateClassifiers.value
        val scalaLibrarySourcesJar = report.select(
            configuration = configurationFilter("sourcedeps"),
            module = (_: ModuleID).name == "scala-library",
            artifact = artifactFilter(`type` = "src")).headOption.getOrElse {
          sys.error(s"Could not fetch scala-library sources")
        }

        FileFunction.cached(cacheDir / s"fetchScalaLibrarySrc",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info(s"Unpacking scala-library sources to $trgDir...")
          if (trgDir.exists)
            IO.delete(trgDir)
          IO.createDirectory(trgDir)
          IO.unzip(scalaLibrarySourcesJar, trgDir)

          ((trgDir ** "*.scala") +++ (trgDir ** "*.java")).get.toSet
        } (Set(scalaLibrarySourcesJar)).toSeq
      }.taskValue,
      (Compile / sources) ~= (_.filterNot { file =>
        // sources from https://github.com/scala/scala/tree/2.13.x/src/library-aux
        val path = file.getPath.replace('\\', '/')
        path.endsWith("scala-library-src/scala/Any.scala") ||
        path.endsWith("scala-library-src/scala/AnyVal.scala") ||
        path.endsWith("scala-library-src/scala/AnyRef.scala") ||
        path.endsWith("scala-library-src/scala/Nothing.scala") ||
        path.endsWith("scala-library-src/scala/Null.scala") ||
        path.endsWith("scala-library-src/scala/Singleton.scala")
      }),
      (Compile / sources) := {
        val files = (Compile / sources).value
        val overwrittenSourcesDir = (Compile / scalaSource).value
        val overwrittenSources = files.flatMap(_.relativeTo(overwrittenSourcesDir)).toSet
        val reference = (Compile/sourceManaged).value / "scala-library-src"
        files.filterNot(_.relativeTo(reference).exists(overwrittenSources))
      },
      (Test / managedClasspath) ~= {
        _.filterNot(file => file.data.getName == s"scala-library-$stdlibBootstrappedVersion.jar")
      },
      mimaCheckDirection := "both",
      mimaBackwardIssueFilters := Scala2LibraryBootstrappedMiMaFilters.BackwardsBreakingChanges,
      mimaForwardIssueFilters := Scala2LibraryBootstrappedMiMaFilters.ForwardsBreakingChanges,
      customMimaReportBinaryIssues("Scala2LibraryBootstrappedMiMaFilters"),
      mimaPreviousArtifacts += "org.scala-lang" % "scala-library" % stdlibBootstrappedVersion,
      mimaExcludeAnnotations ++= Seq(
        "scala.annotation.experimental",
        "scala.annotation.specialized",
        "scala.annotation.unspecialized",
      ),
      tastyMiMaTastyQueryVersionOverride := Some("1.1.2"),
      tastyMiMaPreviousArtifacts += "org.scala-lang" % "scala-library" % stdlibBootstrappedVersion,
      tastyMiMaCurrentClasspath := {
        val javaBootCp = tastyMiMaJavaBootClasspath.value
        val classDir = (Compile / classDirectory).value.toPath()
        val cp0 = Attributed.data((Compile / fullClasspath).value).map(_.toPath())
        val cp: Seq[Path] = classDir +: (javaBootCp ++ cp0)
        (cp, classDir)
      },
      tastyMiMaConfig ~= { _.withMoreProblemFilters(TastyMiMaFilters.StdlibBootstrapped) },
      tastyMiMaReportIssues := tastyMiMaReportIssues.dependsOn(Def.task {
        val minorVersion = previousDottyVersion.split('.')(1)
        // TODO find a way around this and test in the CI
        streams.value.log.warn(
          s"""To allow TASTy-MiMa to read TASTy files generated by this version of the compile you must:
             | * Modify the TASTy version to the latest stable release (latest version supported by TASTy-MiMa) in in tasty/src/dotty/tools/tasty/TastyFormat.scala
             |   - final val MinorVersion = $minorVersion
             |   - final val ExperimentalVersion = 0
             | * Clean everything to generate a compiler with those new TASTy versions
             | * Run ${name.value}/tastyMiMaReportIssues
             |""".stripMargin)

      }).value,
      Compile / exportJars := true,
      artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
        moduleName.value + "-" + dottyVersion + "." + artifact.extension
      },
      run := {
        val log = streams.value.log
        val projectName = projectInfo.value.nameFormal
        val args: Seq[String] = spaceDelimited("<arg>").parsed
        val rootDir = (ThisBuild / baseDirectory).value
        val srcDir = (Compile / scalaSource).value.relativeTo(rootDir).get
        val reference = (Compile/sourceManaged).value.relativeTo(rootDir).get / "scala-library-src"
        args match {
          case Seq("list") =>
            log.info(s"Printing list of non-overriden files in $reference")
            reference.allPaths.get()
              .flatMap(_.relativeTo(reference))
              .filter(_.ext == "scala")
              .sorted
              .foreach(println)
          case Seq(cmd @ ("clone" | "overwrite"), files*) =>
            log.info("Cloning scala-library sources: " + files.mkString(", "))
            for (file <- files) {
              val fileRootedAtInLibraryFolder = file.stripPrefix("src/library/")
              val referenceStdlibPaths = reference / fileRootedAtInLibraryFolder
              val destination = srcDir / fileRootedAtInLibraryFolder
              if (!referenceStdlibPaths.exists) {
                log.error("Not found " + referenceStdlibPaths)
              } else if (destination.exists && cmd == "clone") {
                log.warn(s"Already exists $destination (use `overwrite` command to overwrite)")
              } else {
                val action = if (cmd == "clone") "Cloning" else "Overwriting"
                log.info(s"$action $destination")
                IO.copyFile(referenceStdlibPaths, destination)
              }
            }
          case "diff" +: rest =>
            log.info(s"Diffing ${name.value}/src with scala-library sources")
            if (rest.size > 1) {
              log.error(s"Too many arguments for $projectName/run diff")
            } else {
              val path = rest.headOption.getOrElse("")
              val fullPath = srcDir / path
              if (!fullPath.exists) {
                log.error(s"$fullPath does not exist")
              } else {
                // `--diff-filter=ACMR` is missing `D` on purpose not to show all the files that have not been overwritten.
                val command = s"git diff --diff-filter=ACMR --no-index --color=always -- $reference/$path $fullPath"
                log.info(command)
                import _root_.scala.sys.process._
                command.!
              }
            }
          case _ =>
            val projectName = projectInfo.value.nameFormal
            println(
              s"""Usage:
                |> $projectName/run list
                |  -- lists all files that are not overriden in ${name.value}/src
                |
                |> $projectName/run clone <sources>*
                |  -- clones the specified sources from the ${name.value}/src
                |  -- example: $projectName/run clone scala/Option.scala
                |
                |> $projectName/run overwrite <sources>*
                |  -- (danger) overwrites the specified sources from the ${name.value}/src
                |
                |> $projectName/run diff [path]
                |  -- shows the git diff between the reference library sources the sources used to compile $projectName
                |  -- [path] optional path in the library, eg:
                |    -- $projectName/run diff scala/Predef.scala
                |    -- $projectName/run diff scala/collection/immutable
                |""".stripMargin)
        }
      }
  )

  /** Packages the TASTy files of `scala2-library-bootstrapped` in a jar */
  lazy val `scala2-library-tasty` = project.in(file("scala2-library-tasty")).
    withCommonSettings(Bootstrapped).
    settings(
      moduleName := "scala2-library-tasty-experimental",
      exportJars := true,
      Compile / packageBin / mappings := {
        (`scala2-library-bootstrapped` / Compile / packageBin / mappings).value
          .filter(_._2.endsWith(".tasty"))
      },
    )

  /** Packages the TASTy files of `scala2-library-cc` in a jar */
  lazy val `scala2-library-cc-tasty` = project.in(file("scala2-library-cc-tasty")).
    withCommonSettings(Bootstrapped).
    settings(
      moduleName := "scala2-library-cc-tasty-experimental",
      exportJars := true,
      Compile / packageBin / mappings := {
        (`scala2-library-cc` / Compile / packageBin / mappings).value
          .filter(_._2.endsWith(".tasty"))
      },
    )

  lazy val `scala3-sbt-bridge` = project.in(file("sbt-bridge/src")).
    // We cannot depend on any bootstrapped project to compile the bridge, since the
    // bridge is needed to compile these projects.
    dependsOn(`scala3-compiler` % Provided).
    settings(commonJavaSettings).
    settings(
      description := "sbt compiler bridge for Dotty",

      Test / sources := Seq(),
      Compile / scalaSource := baseDirectory.value,
      Compile / javaSource := baseDirectory.value,
      Compile / resourceDirectory := baseDirectory.value.getParentFile / "resources",

      // Referring to the other project using a string avoids an infinite loop
      // when sbt reads the settings.
      Test / test := (LocalProject("scala3-sbt-bridge-tests") / Test / test).value,

      libraryDependencies += Dependencies.compilerInterface % Provided
    )

  // We use a separate project for the bridge tests since they can only be run
  // with the bootstrapped library on the classpath.
  lazy val `scala3-sbt-bridge-tests` = project.in(file("sbt-bridge/test")).
    dependsOn(dottyCompiler(Bootstrapped) % Test).
    dependsOn(`scala3-sbt-bridge`).
    settings(commonBootstrappedSettings).
    settings(
      Compile / sources := Seq(),
      Test / scalaSource := baseDirectory.value,
      Test / javaSource := baseDirectory.value,
      libraryDependencies += ("org.scala-sbt" %% "zinc-apiinfo" % "1.8.0" % Test).cross(CrossVersion.for3Use2_13)
    )

  lazy val `scala3-presentation-compiler` = project.in(file("presentation-compiler"))
    .asScala3PresentationCompiler(NonBootstrapped)
  lazy val `scala3-presentation-compiler-bootstrapped` = project.in(file("presentation-compiler"))
    .asScala3PresentationCompiler(Bootstrapped)

  def scala3PresentationCompiler(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `scala3-presentation-compiler`
    case Bootstrapped => `scala3-presentation-compiler-bootstrapped`
  }

  def scala3PresentationCompilerBuildInfo(implicit mode: Mode) =
    Seq(
      ideTestsDependencyClasspath := {
        val dottyLib = (dottyLibrary / Compile / classDirectory).value
        val scalaLib =
          (dottyLibrary / Compile / dependencyClasspath)
            .value
            .map(_.data)
            .filter(_.getName.matches("scala-library.*\\.jar"))
            .toList
        dottyLib :: scalaLib
        // Nil
      },
      Compile / buildInfoPackage := "dotty.tools.pc.buildinfo",
      Compile / buildInfoKeys := Seq(scalaVersion),
      Test / buildInfoPackage := "dotty.tools.pc.tests.buildinfo",
      Test / buildInfoKeys := Seq(scalaVersion, ideTestsDependencyClasspath)
    ) ++ BuildInfoPlugin.buildInfoScopedSettings(Compile) ++
      BuildInfoPlugin.buildInfoScopedSettings(Test) ++
      BuildInfoPlugin.buildInfoDefaultSettings

  lazy val presentationCompilerSettings = {
    val mtagsVersion = "1.2.2+44-42e0515a-SNAPSHOT"

    Seq(
      resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
      libraryDependencies ++= Seq(
        "org.lz4" % "lz4-java" % "1.8.0",
        "io.get-coursier" % "interface" % "1.0.18",
        "org.scalameta" % "mtags-interfaces" % mtagsVersion,
      ),
      libraryDependencies += ("org.scalameta" % "mtags-shared_2.13.12" % mtagsVersion % SourceDeps),
      ivyConfigurations += SourceDeps.hide,
      transitiveClassifiers := Seq("sources"),
      scalacOptions ++= Seq("-source", "3.3"), // To avoid fatal migration warnings
      Compile / scalacOptions ++= Seq("-Yexplicit-nulls", "-Ysafe-init"),
      Compile / sourceGenerators += Def.task {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val targetDir = (Compile/sourceManaged).value / "mtags-shared"

        val report = updateClassifiers.value
        val mtagsSharedSourceJar = report.select(
          configuration = configurationFilter("sourcedeps"),
          module = (_: ModuleID).name.startsWith("mtags-shared_"),
          artifact = artifactFilter(`type` = "src")).headOption.getOrElse {
            sys.error(s"Could not fetch mtags-shared sources")

          }
        FileFunction.cached(cacheDir / s"fetchMtagsSharedSource",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info(s"Unpacking mtags-shared sources to $targetDir...")
          if (targetDir.exists)
            IO.delete(targetDir)
          IO.createDirectory(targetDir)
          IO.unzip(mtagsSharedSourceJar, targetDir)

          val mtagsSharedSources = (targetDir ** "*.scala").get.toSet
          mtagsSharedSources.foreach(f => {
            val lines = IO.readLines(f)
            val substitutions = (replaceProtobuf(_)) andThen (insertUnsafeNullsImport(_))
            IO.writeLines(f, substitutions(lines))
          })
          mtagsSharedSources
        } (Set(mtagsSharedSourceJar)).toSeq
      }.taskValue,
    )
  }

  lazy val `scala3-language-server` = project.in(file("language-server")).
    dependsOn(dottyCompiler(Bootstrapped)).
    settings(commonBootstrappedSettings).
    settings(
      libraryDependencies ++= Seq(
        "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.6.0",
        Dependencies.`jackson-databind`
      ),
      // Work around https://github.com/eclipse/lsp4j/issues/295
      dependencyOverrides += "org.eclipse.xtend" % "org.eclipse.xtend.lib" % "2.16.0",
      javaOptions := (`scala3-compiler-bootstrapped` / javaOptions).value,
    ).
    settings(
      ideTestsCompilerVersion := (`scala3-compiler` / version).value,
      ideTestsCompilerArguments := Seq(),
      ideTestsDependencyClasspath := {
        val dottyLib = (`scala3-library-bootstrapped` / Compile / classDirectory).value
        val scalaLib =
          (`scala3-library-bootstrapped` / Compile / dependencyClasspath)
            .value
            .map(_.data)
            .filter(_.getName.matches("scala-library.*\\.jar"))
            .toList
        dottyLib :: scalaLib
      },
      Test / buildInfoKeys := Seq[BuildInfoKey](
        ideTestsCompilerVersion,
        ideTestsCompilerArguments,
        ideTestsDependencyClasspath
      ),
      Test / buildInfoPackage := "dotty.tools.languageserver.util.server",
      BuildInfoPlugin.buildInfoScopedSettings(Test),
      BuildInfoPlugin.buildInfoDefaultSettings
    )

  /** A sandbox to play with the Scala.js back-end of dotty.
   *
   *  This sandbox is compiled with dotty with support for Scala.js. It can be
   *  used like any regular Scala.js project. In particular, `fastOptJS` will
   *  produce a .js file, and `run` will run the JavaScript code with a JS VM.
   *
   *  Simply running `dotty/run -scalajs` without this sandbox is not very
   *  useful, as that would not provide the linker and JS runners.
   */
  lazy val sjsSandbox = project.in(file("sandbox/scalajs")).
    enablePlugins(DottyJSPlugin).
    dependsOn(`scala3-library-bootstrappedJS`).
    settings(
      // Required to run Scala.js tests.
      Test / fork := false,

      scalaJSUseMainModuleInitializer := true,
    )

  /** Scala.js test suite.
   *
   *  This project downloads the sources of the upstream Scala.js test suite,
   *  and tests them with the dotty Scala.js back-end. Currently, only a very
   *  small fraction of the upstream test suite is actually compiled and run.
   *  It will grow in the future, as more stuff is confirmed to be supported.
   */
  lazy val sjsJUnitTests = project.in(file("tests/sjs-junit")).
    enablePlugins(DottyJSPlugin).
    dependsOn(`scala3-library-bootstrappedJS`).
    settings(
      bspEnabled := false,
      scalacOptions --= Seq("-Xfatal-warnings", "-deprecation"),

      // Required to run Scala.js tests.
      Test / fork := false,

      fetchScalaJSSource / sourceDirectory := target.value / s"scala-js-src-$scalaJSVersion",

      fetchScalaJSSource := {
        import org.eclipse.jgit.api._
        import org.eclipse.jgit.lib._

        val s = streams.value
        val ver = scalaJSVersion
        val trgDir = (fetchScalaJSSource / sourceDirectory).value

        if (!trgDir.exists) {
          s.log.info(s"Fetching Scala.js source version $ver")
          IO.createDirectory(trgDir)
          new CloneCommand()
            .setDirectory(trgDir)
            .setURI("https://github.com/scala-js/scala-js.git")
            .setNoCheckout(true)
            .call()
        }

        // Checkout proper ref. We do this anyway so we fail if something is wrong
        val git = Git.open(trgDir)
        s.log.info(s"Checking out Scala.js source version $ver")
        git.getRepository().getConfig().setEnum("core", null, "autocrlf", CoreConfig.AutoCRLF.FALSE)
        git.checkout().setName(s"v$ver").call()

        trgDir
      },

      // We need JUnit in the Compile configuration
      libraryDependencies +=
        ("org.scala-js" %% "scalajs-junit-test-runtime" % scalaJSVersion).cross(CrossVersion.for3Use2_13),

      (Compile / sourceGenerators) += Def.task {
        import org.scalajs.linker.interface.CheckedBehavior

        val stage = scalaJSStage.value

        val linkerConfig = stage match {
          case FastOptStage => (Compile / fastOptJS / scalaJSLinkerConfig).value
          case FullOptStage => (Compile / fullOptJS / scalaJSLinkerConfig).value
        }

        val moduleKind = linkerConfig.moduleKind
        val sems = linkerConfig.semantics

        ConstantHolderGenerator.generate(
            (Compile / sourceManaged).value,
            "org.scalajs.testsuite.utils.BuildInfo",
            "scalaVersion" -> scalaVersion.value,
            "hasSourceMaps" -> false, //DottyJSPlugin.wantSourceMaps.value,
            "isNoModule" -> (moduleKind == ModuleKind.NoModule),
            "isESModule" -> (moduleKind == ModuleKind.ESModule),
            "isCommonJSModule" -> (moduleKind == ModuleKind.CommonJSModule),
            "isFullOpt" -> (stage == FullOptStage),
            "compliantAsInstanceOfs" -> (sems.asInstanceOfs == CheckedBehavior.Compliant),
            "compliantArrayIndexOutOfBounds" -> (sems.arrayIndexOutOfBounds == CheckedBehavior.Compliant),
            "compliantArrayStores" -> (sems.arrayStores == CheckedBehavior.Compliant),
            "compliantNegativeArraySizes" -> (sems.negativeArraySizes == CheckedBehavior.Compliant),
            "compliantStringIndexOutOfBounds" -> (sems.stringIndexOutOfBounds == CheckedBehavior.Compliant),
            "compliantModuleInit" -> (sems.moduleInit == CheckedBehavior.Compliant),
            "strictFloats" -> sems.strictFloats,
            "productionMode" -> sems.productionMode,
            "esVersion" -> linkerConfig.esFeatures.esVersion.edition,
            "useECMAScript2015Semantics" -> linkerConfig.esFeatures.useECMAScript2015Semantics,
        )
      }.taskValue,

      (Test / scalacOptions) += "-scalajs-genStaticForwardersForNonTopLevelObjects",

      scalaJSLinkerConfig ~= { _.withSemantics(build.TestSuiteLinkerOptions.semantics _) },
      (Test / scalaJSModuleInitializers) ++= build.TestSuiteLinkerOptions.moduleInitializers,

      // Perform Ycheck after the Scala.js-specific transformation phases
      scalacOptions += "-Ycheck:prepjsinterop,explicitJSClasses,addLocalJSFakeNews",

      Test / jsEnvInput := {
        val resourceDir = fetchScalaJSSource.value / "test-suite/js/src/test/resources"
        val f = (resourceDir / "NonNativeJSTypeTestNatives.js").toPath
        org.scalajs.jsenv.Input.Script(f) +: (Test / jsEnvInput).value
      },

      Test / unmanagedSourceDirectories ++= {
        val linkerConfig = scalaJSStage.value match {
          case FastOptStage => (Test / fastLinkJS / scalaJSLinkerConfig).value
          case FullOptStage => (Test / fullLinkJS / scalaJSLinkerConfig).value
        }

        if (linkerConfig.moduleKind != ModuleKind.NoModule && !linkerConfig.closureCompiler)
          Seq(baseDirectory.value / "test-require-multi-modules")
        else
          Nil
      },

      (Compile / managedSources) ++= {
        val dir = fetchScalaJSSource.value
        (
          (dir / "test-suite/js/src/main/scala" ** (("*.scala": FileFilter)
            -- "Typechecking*.scala" // defines a Scala 2 macro
            )).get

          ++ (dir / "junit-async/js/src/main/scala" ** "*.scala").get
        )
      },

      // A first blacklist of tests for those that do not compile or do not link
      (Test / managedSources) ++= {
        val dir = fetchScalaJSSource.value / "test-suite"

        val linkerConfig = scalaJSStage.value match {
          case FastOptStage => (Test / fastLinkJS / scalaJSLinkerConfig).value
          case FullOptStage => (Test / fullLinkJS / scalaJSLinkerConfig).value
        }

        val moduleKind = linkerConfig.moduleKind
        val hasModules = moduleKind != ModuleKind.NoModule

        def conditionally(cond: Boolean, subdir: String): Seq[File] =
          if (!cond) Nil
          else (dir / subdir ** "*.scala").get

        (
          (dir / "shared/src/test/scala" ** (("*.scala": FileFilter)
            -- "ReflectiveCallTest.scala" // uses many forms of structural calls that are not allowed in Scala 3 anymore
            -- "UTF16Test.scala" // refutable pattern match
            )).get

          ++ (dir / "shared/src/test/require-sam" ** "*.scala").get
          ++ (dir / "shared/src/test/require-jdk8" ** "*.scala").get
          ++ (dir / "shared/src/test/require-jdk7" ** "*.scala").get

          ++ (dir / "js/src/test/scala" ** (("*.scala": FileFilter)
            -- "StackTraceTest.scala" // would require `npm install source-map-support`
            -- "UnionTypeTest.scala" // requires the Scala 2 macro defined in Typechecking*.scala
            )).get

          ++ (dir / "js/src/test/require-2.12" ** "*.scala").get
          ++ (dir / "js/src/test/require-new-target" ** "*.scala").get
          ++ (dir / "js/src/test/require-sam" ** "*.scala").get
          ++ (dir / "js/src/test/scala-new-collections" ** "*.scala").get

          ++ conditionally(!hasModules, "js/src/test/require-no-modules")
          ++ conditionally(hasModules, "js/src/test/require-modules")
          ++ conditionally(hasModules && !linkerConfig.closureCompiler, "js/src/test/require-multi-modules")
          ++ conditionally(moduleKind == ModuleKind.ESModule, "js/src/test/require-dynamic-import")
          ++ conditionally(moduleKind == ModuleKind.ESModule, "js/src/test/require-esmodule")
        )
      },

      /* For some reason, in Scala 3, the implementation of IterableDefaultTest
       * resolves to `scala.collection.ArrayOps.ArrayIterator`, whose `next()`
       * method is not compliant when called past the last element on Scala.js.
       * It relies on catching an `ArrayIndexOutOfBoundsException`.
       * We have to ignore it here.
       */
      Test / testOptions := Seq(Tests.Filter(_ != "org.scalajs.testsuite.javalib.lang.IterableDefaultTest")),

      Test / managedResources ++= {
        val testDir = fetchScalaJSSource.value / "test-suite/js/src/test"

        val common = (testDir / "resources" ** "*.js").get

        val moduleSpecific = scalaJSLinkerConfig.value.moduleKind match {
          case ModuleKind.NoModule       => Nil
          case ModuleKind.CommonJSModule => (testDir / "resources-commonjs" ** "*.js").get
          case ModuleKind.ESModule       => (testDir / "resources-esmodule" ** "*.js").get
        }

        common ++ moduleSpecific
      },
    )

  lazy val sjsCompilerTests = project.in(file("sjs-compiler-tests")).
    dependsOn(`scala3-compiler` % "test->test").
    settings(
      commonNonBootstrappedSettings,

      libraryDependencies ++= Seq(
        "org.scala-js" %% "scalajs-linker" % scalaJSVersion % Test cross CrossVersion.for3Use2_13,
        "org.scala-js" %% "scalajs-env-nodejs" % "1.3.0" % Test cross CrossVersion.for3Use2_13,
      ),

      // Change the baseDirectory when running the tests
      Test / baseDirectory := baseDirectory.value.getParentFile,

      javaOptions ++= (`scala3-compiler` / javaOptions).value,
      javaOptions ++= {
        val externalJSDeps = (`scala3-library-bootstrappedJS` / Compile / externalDependencyClasspath).value
        val dottyLibraryJSJar = (`scala3-library-bootstrappedJS` / Compile / packageBin).value.getAbsolutePath

        Seq(
          "-Ddotty.tests.classes.dottyLibraryJS=" + dottyLibraryJSJar,
          "-Ddotty.tests.classes.scalaJSJavalib=" + findArtifactPath(externalJSDeps, "scalajs-javalib"),
          "-Ddotty.tests.classes.scalaJSLibrary=" + findArtifactPath(externalJSDeps, "scalajs-library_2.13"),
        )
      },
    )

  lazy val `scala3-bench` = project.in(file("bench")).asDottyBench(NonBootstrapped)
  lazy val `scala3-bench-bootstrapped` = project.in(file("bench")).asDottyBench(Bootstrapped)
  lazy val `scala3-bench-run` = project.in(file("bench-run")).asDottyBench(Bootstrapped)

  lazy val `scala3-bench-micro` = project.in(file("bench-micro"))
    .asDottyBench(Bootstrapped)
    .settings(Jmh / run / mainClass := Some("org.openjdk.jmh.Main"))

  val testcasesOutputDir = taskKey[Seq[String]]("Root directory where tests classes are generated")
  val testcasesSourceRoot = taskKey[String]("Root directory where tests sources are generated")
  val testDocumentationRoot = taskKey[String]("Root directory where tests documentation are stored")
  val generateSelfDocumentation = taskKey[Unit]("Generate example documentation")
  // Note: the two tasks below should be one, but a bug in Tasty prevents that
  val generateScalaDocumentation = inputKey[Unit]("Generate documentation for dotty lib")
  val generateStableScala3Documentation  = inputKey[Unit]("Generate documentation for stable dotty lib")
  val generateTestcasesDocumentation  = taskKey[Unit]("Generate documentation for testcases, useful for debugging tests")

  val generateReferenceDocumentation = inputKey[Unit]("Generate language reference documentation for Scala 3")

  lazy val `scaladoc-testcases` = project.in(file("scaladoc-testcases")).
    dependsOn(`scala3-compiler-bootstrapped`).
    settings(commonBootstrappedSettings)


  /**
   * Collection of projects building targets for scaladoc, these are:
   * - common - common module for javascript
   * - main - main target for default scaladoc producing html webpage
   * - contributors - not related project to any of forementioned modules. Used for presenting contributors for static site.
   *                   Made as an indepented project to be scaladoc-agnostic.
   */
  lazy val `scaladoc-js-common` = project.in(file("scaladoc-js/common")).
    enablePlugins(DottyJSPlugin).
    dependsOn(`scala3-library-bootstrappedJS`).
    settings(libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "2.8.0"))

  lazy val `scaladoc-js-main` = project.in(file("scaladoc-js/main")).
    enablePlugins(DottyJSPlugin).
    dependsOn(`scaladoc-js-common`).
    settings(
      scalaJSUseMainModuleInitializer := true,
      Test / fork := false
    )

  lazy val `scaladoc-js-contributors` = project.in(file("scaladoc-js/contributors")).
    enablePlugins(DottyJSPlugin).
    dependsOn(`scaladoc-js-common`).
    settings(
      Test / fork := false,
      scalaJSUseMainModuleInitializer := true,
      libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "2.8.0")
    )

  def generateDocumentation(configTask: Def.Initialize[Task[GenerationConfig]]) =
    Def.taskDyn {
      val config = configTask.value
      config.get[OutputDir].foreach { outDir =>
        IO.createDirectory(file(outDir.value))
      }
      val command = generateCommand(config)
      Def.task {
        (Compile / run).toTask(command).value
      }
    }

  def generateStaticAssetsTask = Def.task {
    DocumentationWebsite.generateStaticAssets(
      (`scaladoc-js-contributors` / Compile / fullOptJS).value.data,
      (`scaladoc-js-main` / Compile / fullOptJS).value.data,
      (`scaladoc-js-contributors` / Compile / baseDirectory).value / "css",
      (`scaladoc-js-common` / Compile / baseDirectory).value / "css",
      (Compile / resourceManaged).value,
    )
  }

  def bundleCSS = Def.task {
    val unmanagedResources = (Compile / resourceDirectory).value
    def createBundle(dir: File): Seq[File] = {
      val (dirs, files) = IO.listFiles(dir).toList.partition(_.isDirectory)
      val targetDir = (Compile / resourceManaged).value.toPath.resolve(unmanagedResources.toPath.relativize(dir.toPath)).toFile
      val bundleFile = targetDir / "bundle.css"
      if (bundleFile.exists) bundleFile.delete()
      files.foreach(file => IO.append(bundleFile, IO.readBytes(file)))
      bundleFile :: dirs.flatMap(createBundle)
    }

    val cssThemePath = unmanagedResources / "dotty_res" / "styles" / "theme"

    createBundle(cssThemePath)
  }

  val SourceLinksIntegrationTest = config("sourceLinksIntegrationTest") extend Test

  lazy val scaladoc = project.in(file("scaladoc")).
    configs(SourceLinksIntegrationTest).
    settings(commonBootstrappedSettings).
    dependsOn(`scala3-compiler-bootstrapped`).
    dependsOn(`scala3-tasty-inspector`).
    settings(inConfig(SourceLinksIntegrationTest)(Defaults.testSettings)).
    settings(
      SourceLinksIntegrationTest / scalaSource := baseDirectory.value / "test-source-links",
      SourceLinksIntegrationTest / test:= ((SourceLinksIntegrationTest / test) dependsOn generateScalaDocumentation.toTask("")).value,
    ).
    settings(
      Compile / resourceGenerators ++= Seq(
        generateStaticAssetsTask.taskValue,
        bundleCSS.taskValue
      ),
      libraryDependencies ++= Dependencies.flexmarkDeps ++ Seq(
        "nl.big-o" % "liqp" % "0.8.2",
        "org.jsoup" % "jsoup" % "1.17.2", // Needed to process .html files for static site
        Dependencies.`jackson-dataformat-yaml`,

        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
      ),
      Compile / mainClass := Some("dotty.tools.scaladoc.Main"),
      Compile / buildInfoKeys := Seq[BuildInfoKey](version),
      Compile / buildInfoPackage := "dotty.tools.scaladoc",
      BuildInfoPlugin.buildInfoScopedSettings(Compile),
      BuildInfoPlugin.buildInfoDefaultSettings,

      Test / test := (Test / test).dependsOn(`scaladoc-testcases` / Compile / compile).value,
      Test / testcasesOutputDir := (`scaladoc-testcases`/Compile/products).value.map(_.getAbsolutePath),
      Test / testcasesSourceRoot := ((`scaladoc-testcases` / baseDirectory).value / "src").getAbsolutePath.toString,
      run / baseDirectory := (ThisBuild / baseDirectory).value,
      generateSelfDocumentation := Def.taskDyn {
        generateDocumentation(Scaladoc)
      }.value,

      generateScalaDocumentation := Def.inputTaskDyn {
        val majorVersion = (LocalProject("scala3-library-bootstrapped") / scalaBinaryVersion).value

        val extraArgs = spaceDelimited("[<output-dir>] [--justAPI]").parsed
        val outputDirOverride = extraArgs.headOption.fold(identity[GenerationConfig](_))(newDir => {
          config: GenerationConfig => config.add(OutputDir(newDir))
        })
        val justAPIArg: Option[String] = extraArgs.drop(1).find(_ == "--justAPI")
        val justAPI = justAPIArg.fold(identity[GenerationConfig](_))(_ => {
          config: GenerationConfig => config.remove[SiteRoot]
        })
        val overrideFunc = outputDirOverride.andThen(justAPI)

        val config = Def.task {
          overrideFunc(Scala3.value)
        }

        val writeAdditionalFiles = Def.task {
          val dest = file(config.value.get[OutputDir].get.value)
          if (justAPIArg.isEmpty) {
            IO.write(dest / "versions" / "latest-nightly-base", majorVersion)
            // This file is used by GitHub Pages when the page is available in a custom domain
            IO.write(dest / "CNAME", "dotty.epfl.ch")
          }
        }

        writeAdditionalFiles.dependsOn(generateDocumentation(config))
      }.evaluated,

      generateStableScala3Documentation := Def.inputTaskDyn {
        val extraArgs = spaceDelimited("<version>").parsed
        val config = stableScala3(extraArgs.head)
        generateDocumentation(config)
      }.evaluated,

      generateTestcasesDocumentation := Def.taskDyn {
        generateDocumentation(Testcases)
      }.value,

      // Generate the Scala 3 reference documentation (published at https://docs.scala-lang.org/scala3/reference)
      generateReferenceDocumentation := Def.inputTaskDyn {
        val shouldRegenerateExpectedLinks = (Space ~> literal("--no-regenerate-expected-links")).?.parsed.isEmpty

        generateStaticAssetsTask.value

        // Move all the source files to a temporary directory and apply some changes specific to the reference documentation
        val temp = IO.createTemporaryDirectory
        IO.copyDirectory(file("docs"), temp / "docs")
        IO.delete(temp / "docs" / "_blog")

        // Overwrite the main layout and the sidebar
        IO.copyDirectory(
          file("project") / "resources" / "referenceReplacements",
          temp / "docs",
          overwrite = true
        )

        // Add redirections from previously supported URLs, for some pages
        for (name <- Seq("changed-features", "contextual", "dropped-features", "metaprogramming", "other-new-features")) {
          val path = temp / "docs" / "_docs" / "reference" / name / s"${name}.md"
          val contentLines = IO.read(path).linesIterator.to[collection.mutable.ArrayBuffer]
          contentLines.insert(1, s"redirectFrom: /${name}.html") // Add redirection
          val newContent = contentLines.mkString("\n")
          IO.write(path, newContent)
        }

        val languageReferenceConfig = Def.task {
          Scala3.value
            .add(OutputDir("scaladoc/output/reference"))
            .add(SiteRoot(s"${temp.getAbsolutePath}/docs"))
            .add(ProjectName("Scala 3 Reference"))
            .add(ProjectVersion(baseVersion))
            .remove[VersionsDictionaryUrl]
            .add(SourceLinks(List(
              s"${temp.getAbsolutePath}=github://scala/scala3/language-reference-stable"
            )))
            .withTargets(List("___fake___.scala"))
        }

        val expectedLinksRegeneration = Def.task {
          if (shouldRegenerateExpectedLinks) {
            val script = (file("project") / "scripts" / "regenerateExpectedLinks").toString
            val outputDir = languageReferenceConfig.value.get[OutputDir].get.value
            val expectedLinksFile = (file("project") / "scripts" / "expected-links" / "reference-expected-links.txt").toString
            import _root_.scala.sys.process._
            s"$script $outputDir $expectedLinksFile".!
          }
        }

        expectedLinksRegeneration.dependsOn(generateDocumentation(languageReferenceConfig))
      }.evaluated,

      Test / buildInfoKeys := Seq[BuildInfoKey](
        (Test / Build.testcasesOutputDir),
        (Test / Build.testcasesSourceRoot),
        Build.testDocumentationRoot,
      ),
      testDocumentationRoot := (baseDirectory.value / "test-documentations").getAbsolutePath,
      Test / buildInfoPackage := "dotty.tools.scaladoc.test",
      BuildInfoPlugin.buildInfoScopedSettings(Test),
    )

  // various scripted sbt tests
  lazy val `sbt-test` = project.in(file("sbt-test")).
    enablePlugins(ScriptedPlugin).
    settings(commonSettings).
    settings(
      sbtTestDirectory := baseDirectory.value,
      target := baseDirectory.value / ".." / "out" / name.value,

      // The batch mode accidentally became the default with no way to disable
      // it in sbt 1.4 (https://github.com/sbt/sbt/issues/5913#issuecomment-716003195).
      // We enable it explicitly here to make it clear that we're using it.
      scriptedBatchExecution := true,

      scriptedLaunchOpts ++= Seq(
        "-Dplugin.version=" + version.value,
        "-Dplugin.scalaVersion=" + dottyVersion,
        "-Dplugin.scala2Version=" + stdlibVersion(Bootstrapped),
        "-Dplugin.scalaJSVersion=" + scalaJSVersion,
        "-Dsbt.boot.directory=" + ((ThisBuild / baseDirectory).value / ".sbt-scripted").getAbsolutePath // Workaround sbt/sbt#3469
      ),
      // Pass along ivy home and repositories settings to sbt instances run from the tests
      scriptedLaunchOpts ++= {
        val repositoryPath = (io.Path.userHome / ".sbt" / "repositories").absolutePath
        s"-Dsbt.repository.config=$repositoryPath" ::
        ivyPaths.value.ivyHome.map("-Dsbt.ivy.home=" + _.getAbsolutePath).toList
      },
      scriptedBufferLog := true,
      scripted := scripted.dependsOn(
        (`scala3-sbt-bridge` / publishLocalBin),
        (`scala3-interfaces` / publishLocalBin),
        (`scala3-compiler-bootstrapped` / publishLocalBin),
        (`scala3-library-bootstrapped` / publishLocalBin),
        (`scala2-library-tasty` / publishLocal),
        (`scala2-library-cc-tasty` / publishLocal),
        (`scala3-library-bootstrappedJS` / publishLocalBin),
        (`tasty-core-bootstrapped` / publishLocalBin),
        (`scala3-staging` / publishLocalBin),
        (`scala3-tasty-inspector` / publishLocalBin),
        (`scaladoc` / publishLocalBin),
        (`scala3-bootstrapped` / publishLocalBin) // Needed because sbt currently hardcodes the dotty artifact
      ).evaluated
    )

  lazy val `sbt-community-build` = project.in(file("sbt-community-build")).
    enablePlugins(SbtPlugin).
    settings(commonSettings).
    settings(
      name := "sbt-community-build",
      version := sbtCommunityBuildVersion,
      organization := "ch.epfl.lamp",
      sbtTestDirectory := baseDirectory.value / "sbt-test",
      scriptedLaunchOpts ++= Seq(
        "-Dplugin.version=" + version.value,
        "-Dplugin.scalaVersion=" + dottyVersion,
        "-Dplugin.scalaJSVersion=" + scalaJSVersion,
        "-Dplugin.sbtDottyVersion=" + sbtDottyVersion,
        "-Ddotty.communitybuild.dir=" + baseDirectory.value / "target",
        "-Dsbt.boot.directory=" + ((ThisBuild / baseDirectory).value / ".sbt-scripted").getAbsolutePath // Workaround sbt/sbt#3469
      ),
      // Pass along ivy home and repositories settings to sbt instances run from the tests
      scriptedLaunchOpts ++= {
        val repositoryPath = (io.Path.userHome / ".sbt" / "repositories").absolutePath
        s"-Dsbt.repository.config=$repositoryPath" ::
        ivyPaths.value.ivyHome.map("-Dsbt.ivy.home=" + _.getAbsolutePath).toList
      },
      scriptedBufferLog := true,
      scriptedBatchExecution := true,
      scripted := scripted.dependsOn(
        (`scala3-sbt-bridge` / publishLocal),
        (`scala3-interfaces` / publishLocal),
        (`scala3-compiler-bootstrapped` / publishLocal),
        (`scala3-library-bootstrapped` / publishLocal),
        (`scala3-library-bootstrappedJS` / publishLocal),
        (`tasty-core-bootstrapped` / publishLocal),
        (`scala3-staging` / publishLocal),
        (`scala3-tasty-inspector` / publishLocal),
        (`scaladoc` / publishLocal),
        (`scala3-bootstrapped` / publishLocal)
      ).evaluated
   )

  val prepareCommunityBuild = taskKey[Unit]("Publish local the compiler and the sbt plugin. Also store the versions of the published local artefacts in two files, community-build/{scala3-bootstrapped.version,sbt-injected-plugins}.")

  lazy val `community-build` = project.in(file("community-build")).
    dependsOn(dottyLibrary(Bootstrapped)).
    settings(commonBootstrappedSettings).
    settings(
      prepareCommunityBuild := {
        (`scala3-sbt-bridge` / publishLocal).value
        (`scala3-interfaces` / publishLocal).value
        (`tasty-core-bootstrapped` / publishLocal).value
        (`scala3-library-bootstrapped` / publishLocal).value
        (`scala3-tasty-inspector` / publishLocal).value
        (`scaladoc` / publishLocal).value
        (`scala3-compiler-bootstrapped` / publishLocal).value
        (`scala3-bootstrapped` / publishLocal).value
        (`scala3-library-bootstrappedJS` / publishLocal).value
        (`sbt-community-build` / publishLocal).value
        // (publishLocal in `scala3-staging`).value
        val pluginText =
          s"""updateOptions in Global ~= (_.withLatestSnapshots(false))
             |addSbtPlugin("ch.epfl.lamp" % "sbt-community-build" % "$sbtCommunityBuildVersion")
             |addSbtPlugin("org.scala-js" % "sbt-scalajs" % "$scalaJSVersion")""".stripMargin
        IO.write(baseDirectory.value / "sbt-injected-plugins", pluginText)
        IO.write(baseDirectory.value / "scala3-bootstrapped.version", dottyVersion)
        IO.delete(baseDirectory.value / "dotty-community-build-deps")  // delete any stale deps file
      },
      (Test / testOptions) += Tests.Argument(
        TestFrameworks.JUnit,
        "--include-categories=dotty.communitybuild.TestCategory",
        "--run-listener=dotty.communitybuild.FailureSummarizer",
      ),
      Compile/run := (Compile/run).dependsOn(prepareCommunityBuild).evaluated,
      Test / testOnly := ((Test / testOnly) dependsOn prepareCommunityBuild).evaluated,
      Test / test     := ((Test / test    ) dependsOn prepareCommunityBuild).value,
      javaOptions ++= {
        // Propagate the ivy cache directory setting to the tests, which will
        // then propagate it further to the sbt instances they will spawn.
        val sbtProps = Option(System.getProperty("sbt.ivy.home")) match {
          case Some(ivyHome) =>
            Seq(s"-Dsbt.ivy.home=$ivyHome")
          case _ =>
            Seq()
        }
        sbtProps
      }
    )

  lazy val publishSettings = Seq(
    publishMavenStyle := true,
    isSnapshot := version.value.contains("SNAPSHOT"),
    publishTo := sonatypePublishToBundle.value,
    publishConfiguration ~= (_.withOverwrite(true)),
    publishLocalConfiguration ~= (_.withOverwrite(true)),
    projectID ~= {id =>
      val line = "scala.versionLine" -> versionLine
      id.withExtraAttributes(id.extraAttributes + line)
    },
    Test / publishArtifact := false,
    homepage := Some(url(dottyGithubUrl)),
    licenses += (("Apache-2.0",
      url("https://www.apache.org/licenses/LICENSE-2.0"))),
    scmInfo := Some(
      ScmInfo(
        url(dottyGithubUrl),
        "scm:git:git@github.com:scala/scala3.git"
      )
    ),
    developers := List(
      Developer(
        id = "odersky",
        name = "Martin Odersky",
        email = "martin.odersky@epfl.ch",
        url = url("https://github.com/odersky")
      ),
      Developer(
        id = "DarkDimius",
        name = "Dmitry Petrashko",
        email = "me@d-d.me",
        url = url("https://d-d.me")
      ),
      Developer(
        id = "smarter",
        name = "Guillaume Martres",
        email = "smarter@ubuntu.com",
        url = url("http://guillaume.martres.me")
      ),
      Developer(
        id = "felixmulder",
        name = "Felix Mulder",
        email = "felix.mulder@gmail.com",
        url = url("http://felixmulder.com")
      ),
      Developer(
        id = "liufengyun",
        name = "Liu Fengyun",
        email = "liu@fengy.me",
        url = url("https://fengy.me")
      ),
      Developer(
        id = "nicolasstucki",
        name = "Nicolas Stucki",
        email = "nicolas.stucki@gmail.com",
        url = url("https://github.com/nicolasstucki")
      ),
      Developer(
        id = "OlivierBlanvillain",
        name = "Olivier Blanvillain",
        email = "olivier.blanvillain@gmail.com",
        url = url("https://github.com/OlivierBlanvillain")
      ),
      Developer(
        id = "biboudis",
        name = "Aggelos Biboudis",
        email = "aggelos.biboudis@epfl.ch",
        url = url("http://biboudis.github.io")
      ),
      Developer(
        id = "allanrenucci",
        name = "Allan Renucci",
        email = "allan.renucci@gmail.com",
        url = url("https://github.com/allanrenucci")
      ),
      Developer(
        id = "Duhemm",
        name = "Martin Duhem",
        email = "martin.duhem@gmail.com",
        url = url("https://github.com/Duhemm")
      )
    )
  )

  lazy val commonDistSettings = Seq(
    packMain := Map(),
    publishArtifact := false,
    packGenerateMakefile := false,
    packExpandedClasspath := true,
    packArchiveName := "scala3-" + dottyVersion
  )

  lazy val dist = project.asDist(Bootstrapped)
    .settings(
      packResourceDir += (baseDirectory.value / "bin" -> "bin"),
    )

  private def customMimaReportBinaryIssues(issueFilterLocation: String) = mimaReportBinaryIssues := {
    mimaReportBinaryIssues.result.value match {
      case Inc(inc: Incomplete) =>
        streams.value.log.error(s"\nFilters in $issueFilterLocation are used in this check.\n ")
        throw inc
      case Value(v) => v
    }
  }

  implicit class ProjectDefinitions(val project: Project) extends AnyVal {

    // FIXME: we do not aggregate `bin` because its tests delete jars, thus breaking other tests
    def asDottyRoot(implicit mode: Mode): Project = project.withCommonSettings.
      aggregate(`scala3-interfaces`, dottyLibrary, dottyCompiler, tastyCore, `scala3-sbt-bridge`, scala3PresentationCompiler).
      bootstrappedAggregate(`scala2-library-tasty`, `scala3-language-server`, `scala3-staging`,
        `scala3-tasty-inspector`, `scala3-library-bootstrappedJS`, scaladoc).
      dependsOn(tastyCore).
      dependsOn(dottyCompiler).
      dependsOn(dottyLibrary).
      bootstrappedSettings(
        addCommandAlias("clean", ";scala3-bootstrapped/clean;scala2-library-bootstrapped/clean;scala2-library-cc/clean"),
      ).
      nonBootstrappedSettings(
        addCommandAlias("run", "scala3-compiler/run"),
        // Clean everything by default
        addCommandAlias("clean", ";scala3/clean;scala3-bootstrapped/clean;scala2-library-bootstrapped/clean;scala2-library-cc/clean"),
        // `publishLocal` on the non-bootstrapped compiler does not produce a
        // working distribution (it can't in general, since there's no guarantee
        // that the non-bootstrapped library is compatible with the
        // non-bootstrapped compiler), so publish the bootstrapped one by
        // default.
        addCommandAlias("publishLocal", "scala3-bootstrapped/publishLocal"),
        repl := (`scala3-compiler-bootstrapped` / repl).value,
        (Compile / console) := (Compile / console).dependsOn(Def.task {
          import _root_.scala.io.AnsiColor._
          val msg = "`console` uses the reference Scala version. Use `repl` instead."
          val f = "═" * (msg.length + 2)
          val box =
            s"""╔$f╗
               |║ ${BOLD}$msg$RESET ║
               |╚$f╝""".stripMargin
          streams.value.log.warn(box)
        }).value,
      ).
      settings(
        publish / skip := true
      )

    def asDottyCompiler(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(`scala3-interfaces`).
      dependsOn(dottyLibrary).
      dependsOn(tastyCore).
      settings(dottyCompilerSettings)

    def asDottyLibrary(implicit mode: Mode): Project = {
      val base =
        project.withCommonSettings.
          settings(
            versionScheme := Some("semver-spec"),
            libraryDependencies += "org.scala-lang" % "scala-library" % stdlibVersion,
            // Make sure we do not refer to experimental features outside an experimental scope.
            // In other words, disable NIGHTLY/SNAPSHOT experimental scope.
            scalacOptions += "-Yno-experimental",
          ).
          settings(dottyLibrarySettings)
      if (mode == Bootstrapped) {
        base.settings(
          (Compile/doc) := {
            // Workaround for
            // [error]    |object IArray cannot have the same name as object IArray in package scala
            // -- cannot define object member with the same name as a object member in self reference _.
            val doWork = (Compile/doc).result.value
            (Compile/doc/target).value
          },
          commonMiMaSettings,
          mimaPreviousArtifacts += {
            val thisProjectID = projectID.value
            val crossedName = thisProjectID.crossVersion match {
              case cv: Disabled => thisProjectID.name
              case cv: Binary => s"${thisProjectID.name}_${cv.prefix}3${cv.suffix}"
            }
            (thisProjectID.organization % crossedName % ltsDottyVersion)
          },
          mimaForwardIssueFilters := MiMaFilters.Scala3Library.ForwardsBreakingChanges,
          mimaBackwardIssueFilters := MiMaFilters.Scala3Library.BackwardsBreakingChanges,
          customMimaReportBinaryIssues("MiMaFilters.Scala3Library"),
        )
      } else base
    }


    def asTastyCore(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyLibrary).
      settings(tastyCoreSettings).
      settings(disableDocSetting).
      settings(
        versionScheme := Some("semver-spec"),
        if (mode == Bootstrapped) Def.settings(
          commonMiMaSettings,
          mimaForwardIssueFilters := MiMaFilters.TastyCore.ForwardsBreakingChanges,
          mimaBackwardIssueFilters := MiMaFilters.TastyCore.BackwardsBreakingChanges,
          customMimaReportBinaryIssues("MiMaFilters.TastyCore"),
        ) else {
          Nil
        }
      )

    def asTastyCoreScala2: Project = project
      .settings(commonScala2Settings)
      // need to add @annotation.internal.sharable to the classpath for compiling
      // we don't actually publish this library anywhere, so it's fine.
      // if someone depends on the sources of tasty-core in a scala 2 project,
      // they should strip the sharable annotation, or add -Ytasty-reader
      .dependsOn(dottyLibrary(NonBootstrapped) % Provided)
      .settings(
        scalacOptions += "-Ytasty-reader" // to read scala3 library
      )

    def asDottyBench(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyCompiler).
      settings(commonBenchmarkSettings).
      enablePlugins(JmhPlugin)

    def asScala3PresentationCompiler(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyCompiler, dottyLibrary).
      settings(presentationCompilerSettings).
      settings(scala3PresentationCompilerBuildInfo)

    def asDist(implicit mode: Mode): Project = project.
      enablePlugins(PackPlugin).
      withCommonSettings.
      dependsOn(`scala3-interfaces`, dottyCompiler, dottyLibrary, tastyCore, `scala3-staging`, `scala3-tasty-inspector`, scaladoc).
      settings(commonDistSettings).
      bootstrappedSettings(
        target := baseDirectory.value / "target" // override setting in commonBootstrappedSettings
      )

    def withCommonSettings(implicit mode: Mode): Project = project.settings(mode match {
      case NonBootstrapped => commonNonBootstrappedSettings
      case Bootstrapped => commonBootstrappedSettings
    })
  }
}

object ScaladocConfigs {
  import Build._
  private lazy val currentYear: String = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR).toString

  def dottyExternalMapping = ".*scala/.*::scaladoc3::https://dotty.epfl.ch/api/"
  def javaExternalMapping = ".*java/.*::javadoc::https://docs.oracle.com/javase/8/docs/api/"
  def scalaSrcLink(v: String, s: String) = s"${s}github://scala/scala/v$v#src/library"
  def dottySrcLink(v: String, sourcesPrefix: String = "", outputPrefix: String = "") =
    sys.env.get("GITHUB_SHA") match {
      case Some(sha) =>
        s"${sourcesPrefix}github://${sys.env("GITHUB_REPOSITORY")}/$sha$outputPrefix"
      case None => s"${sourcesPrefix}github://scala/scala3/$v$outputPrefix"
    }

  def defaultSourceLinks(version: String = dottyNonBootstrappedVersion, refVersion: String = dottyVersion) = Def.task {
    def stdLibVersion = stdlibVersion(NonBootstrapped)
    def srcManaged(v: String, s: String) = s"out/bootstrap/scala2-library-bootstrapped/scala-$v/src_managed/main/$s-library-src"
    SourceLinks(
      List(
        scalaSrcLink(stdLibVersion, srcManaged(version, "scala") + "="),
        dottySrcLink(refVersion, "library/src=", "#library/src"),
        dottySrcLink(refVersion),
        "docs=github://scala/scala3/main#docs"
      )
    )
  }

  lazy val DefaultGenerationSettings = Def.task {
    def projectVersion = version.value
    def socialLinks = SocialLinks(List(
      "github::https://github.com/scala/scala3",
      "discord::https://discord.com/invite/scala",
      "twitter::https://twitter.com/scala_lang",
    ))
    def projectLogo = ProjectLogo("docs/_assets/images/logo.svg")
    def skipByRegex = SkipByRegex(List(".+\\.internal($|\\..+)", ".+\\.impl($|\\..+)"))
    def skipById = SkipById(List(
      "scala.runtime.stdLibPatches",
      "scala.runtime.MatchCase",
      "dotty.tools.tasty",
      "dotty.tools.tasty.util",
    ))
    def projectFooter = ProjectFooter(s"Copyright (c) 2002-$currentYear, LAMP/EPFL")
    def defaultTemplate = DefaultTemplate("static-site-main")
    GenerationConfig(
      List(),
      ProjectVersion(projectVersion),
      GenerateInkuire(true),
      defaultSourceLinks().value,
      skipByRegex,
      skipById,
      projectLogo,
      socialLinks,
      projectFooter,
      defaultTemplate,
      Author(true),
      Groups(true),
      QuickLinks(
        List(
          "Learn::https://docs.scala-lang.org/",
          "Install::https://www.scala-lang.org/download/",
          "Playground::https://scastie.scala-lang.org",
          "Find\u00A0A\u00A0Library::https://index.scala-lang.org",
          "Community::https://www.scala-lang.org/community/",
          "Blog::https://www.scala-lang.org/blog/",
        )
      )
    )
  }

  lazy val DefaultGenerationConfig = Def.task {
    def distLocation = (dist / Compile / pack).value
    DefaultGenerationSettings.value
  }

  lazy val Scaladoc = Def.task {
    DefaultGenerationConfig.value
      .add(UseJavacp(true))
      .add(ProjectName("scaladoc"))
      .add(OutputDir("scaladoc/output/self"))
      .add(Revision(VersionUtil.gitHash))
      .add(ExternalMappings(List(dottyExternalMapping, javaExternalMapping)))
      .withTargets((Compile / classDirectory).value.getAbsolutePath :: Nil)
  }

  lazy val Testcases = Def.task {
    val tastyRoots = (Test / Build.testcasesOutputDir).value
    DefaultGenerationConfig.value
      .add(UseJavacp(true))
      .add(OutputDir("scaladoc/output/testcases"))
      .add(ProjectName("scaladoc testcases"))
      .add(Revision("main"))
      .add(SnippetCompiler(List("scaladoc-testcases/docs=compile")))
      .add(SiteRoot("scaladoc-testcases/docs"))
      .add(CommentSyntax(List(
        "scaladoc-testcases/src/example/comment-md=markdown",
        "scaladoc-testcases/src/example/comment-wiki=wiki"
      )))
      .add(ExternalMappings(List(dottyExternalMapping, javaExternalMapping)))
      .withTargets(tastyRoots)
  }

  def snippetCompilerTargets(dottyLibSrc:String) = List(
    s"$dottyLibSrc/scala=compile",
    s"$dottyLibSrc/scala/quoted=compile",
    s"$dottyLibSrc/scala/compiletime=compile",
    s"$dottyLibSrc/scala/util=compile",
    s"$dottyLibSrc/scala/util/control=compile",
  )
  lazy val Scala3 = Def.task {
    val dottyJars: Seq[java.io.File] = Seq(
      (`scala2-library-bootstrapped`/Compile/products).value,
      (`scala3-library-bootstrapped`/Compile/products).value,
      (`scala3-interfaces`/Compile/products).value,
      (`tasty-core-bootstrapped`/Compile/products).value,
    ).flatten

    val roots = dottyJars.map(_.getAbsolutePath)

    val managedSources =
      (`scala2-library-bootstrapped`/Compile/sourceManaged).value / "scala-library-src"
    val projectRoot = (ThisBuild/baseDirectory).value.toPath
    val stdLibRoot = projectRoot.relativize(managedSources.toPath.normalize())
    val docRootFile = stdLibRoot.resolve("rootdoc.txt")

    val dottyManagesSources = (`scala3-library-bootstrapped`/Compile/baseDirectory).value

    val tastyCoreSources = projectRoot.relativize((`tasty-core-bootstrapped`/Compile/scalaSource).value.toPath().normalize())

    val dottyLibRoot = projectRoot.relativize(dottyManagesSources.toPath.normalize())
    DefaultGenerationConfig.value
      .add(ProjectName("Scala 3"))
      .add(OutputDir(file("scaladoc/output/scala3").getAbsoluteFile.getAbsolutePath))
      .add(Revision("main"))
      .add(ExternalMappings(List(javaExternalMapping)))
      .add(DocRootContent(docRootFile.toString))
      .add(CommentSyntax(List(
        s"${dottyLibRoot}=markdown",
        s"${stdLibRoot}=wiki",
        s"${tastyCoreSources}=markdown",
        "wiki"
      )))
      .add(VersionsDictionaryUrl("https://scala-lang.org/api/versions.json"))
      .add(DocumentSyntheticTypes(true))
      .add(SnippetCompiler(
        snippetCompilerTargets(s"$dottyLibRoot/src") ++ List(
        "docs/_docs/reference/enums=compile"
      )))
      .add(SiteRoot("docs"))
      .add(ApiSubdirectory(true))
      .withTargets(roots)
  }

  def stableScala3(version: String) = Def.task {
    val scalaLibrarySrc = s"out/bootstrap/scala2-library-bootstrapped/scala-$version-bin-SNAPSHOT-nonbootstrapped/src_managed"
    val dottyLibrarySrc = "library/src"
    Scala3.value
      .add(defaultSourceLinks(version + "-bin-SNAPSHOT-nonbootstrapped", version).value)
      .add(ProjectVersion(version))
      .add(SnippetCompiler(snippetCompilerTargets(dottyLibrarySrc)))
      .add(CommentSyntax(List(
        s"$dottyLibrarySrc=markdown",
        s"$scalaLibrarySrc=wiki",
        "wiki"
      )))
      .add(DocRootContent(s"$scalaLibrarySrc/rootdoc.txt"))
      .withTargets(
        Seq(
          s"out/bootstrap/scala2-library-bootstrapped/scala-$version-bin-SNAPSHOT-nonbootstrapped/classes",
          s"out/bootstrap/scala3-library-bootstrapped/scala-$version-bin-SNAPSHOT-nonbootstrapped/classes",
          s"tmp/interfaces/target/classes",
          s"out/bootstrap/tasty-core-bootstrapped/scala-$version-bin-SNAPSHOT-nonbootstrapped/classes"
        )
      )
      .remove[SiteRoot]
      .remove[ApiSubdirectory]
  }
}
