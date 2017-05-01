import sbt.Keys._
import sbt._
import complete.DefaultParsers._
import java.io.{ RandomAccessFile, File }
import java.nio.channels.FileLock
import java.nio.file.Files
import scala.reflect.io.Path
import sbtassembly.AssemblyKeys.assembly

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt.Package.ManifestAttributes

import com.typesafe.sbteclipse.plugin.EclipsePlugin._

object Build {

  projectChecks()

  val scalacVersion = "2.11.11" // Do not rename, this is grepped in bin/common.

  val dottyOrganization = "ch.epfl.lamp"
  val dottyVersion = {
    val baseVersion = "0.1.1"
    val isNightly = sys.env.get("NIGHTLYBUILD") == Some("yes")
    if (isNightly)
      baseVersion + "-bin-" + VersionUtil.commitDate + "-" + VersionUtil.gitHash + "-NIGHTLY"
    else
      baseVersion + "-bin-SNAPSHOT"
  }

  val jenkinsMemLimit = List("-Xmx1500m")

  val JENKINS_BUILD = "dotty.jenkins.build"
  val DRONE_MEM = "dotty.drone.mem"

  val agentOptions = List(
    // "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
    // "-agentpath:/home/dark/opt/yjp-2013-build-13072/bin/linux-x86-64/libyjpagent.so"
    // "-agentpath:/Applications/YourKit_Java_Profiler_2015_build_15052.app/Contents/Resources/bin/mac/libyjpagent.jnilib",
    // "-XX:+HeapDumpOnOutOfMemoryError", "-Xmx1g", "-Xss2m"
  )

  // Packages all subprojects to their jars
  lazy val packageAll =
    taskKey[Map[String, String]]("Package everything needed to run tests")

  // Spawns a repl with the correct classpath
  lazy val repl = inputKey[Unit]("run the REPL with correct classpath")

  // Run tests with filter through vulpix test suite
  lazy val vulpix = inputKey[Unit]("runs integration test with the supplied filter")

  // Used to compile files similar to ./bin/dotc script
  lazy val dotc =
    inputKey[Unit]("run the compiler using the correct classpath, or the user supplied classpath")

  // Used to run binaries similar to ./bin/dotr script
  lazy val dotr =
    inputKey[Unit]("run compiled binary using the correct classpath, or the user supplied classpath")

  // Compiles the documentation and static site
  lazy val genDocs = inputKey[Unit]("run dottydoc to generate static documentation site")

  // Shorthand for compiling a docs site
  lazy val dottydoc = inputKey[Unit]("run dottydoc")

  // Used in build.sbt
  val thisBuildSettings = Seq(
    scalaVersion in Global := scalacVersion,
    version in Global := dottyVersion,
    organization in Global := dottyOrganization,
    organizationName in Global := "LAMP/EPFL",
    organizationHomepage in Global := Some(url("http://lamp.epfl.ch")),
    homepage in Global := Some(url("https://github.com/lampepfl/dotty")),

    // scalac options
    scalacOptions in Global ++= Seq(
      "-feature",
      "-deprecation",
      "-encoding", "UTF8",
      "-language:existentials,higherKinds,implicitConversions"
    ),

    javacOptions in Global ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")
  )

  // set sources to src/, tests to test/ and resources to resources/
  lazy val sourceStructure = Seq(
    scalaSource       in Compile    := baseDirectory.value / "src",
    scalaSource       in Test       := baseDirectory.value / "test",
    javaSource        in Compile    := baseDirectory.value / "src",
    javaSource        in Test       := baseDirectory.value / "test",
    resourceDirectory in Compile    := baseDirectory.value / "resources"
  )

  // Settings used by all dotty-compiled projects
  lazy val commonBootstrappedSettings = Seq(
    EclipseKeys.skipProject := true,
    scalaOrganization := dottyOrganization,
    scalaVersion := dottyVersion,
    scalaBinaryVersion := "2.11",
    scalaCompilerBridgeSource :=
      (dottyOrganization % "dotty-sbt-bridge" % scalaVersion.value % "component").sources(),

    // sbt gets very unhappy if two projects use the same target
    target := baseDirectory.value / ".." / "out" / name.value,

    // The non-bootstrapped dotty-library is not necessary when bootstrapping dotty
    autoScalaLibrary := false,
    // ...but scala-library is
    libraryDependencies += "org.scala-lang" % "scala-library" % scalacVersion
  )

  /** Projects -------------------------------------------------------------- */

  // Needed because the dotty project aggregates dotty-sbt-bridge but dotty-sbt-bridge
  // currently refers to dotty in its scripted task and "aggregate" does not take by-name
  // parameters: https://github.com/sbt/sbt/issues/2200
  lazy val dottySbtBridgeRef = LocalProject("dotty-sbt-bridge")

  // The root project:
  // - aggregates other projects so that "compile", "test", etc are run on all projects at once.
  // - publishes its own empty artifact "dotty" that depends on "dotty-library" and "dotty-compiler",
  //   this is only necessary for compatibility with sbt which currently hardcodes the "dotty" artifact name
  lazy val dotty = project.in(file(".")).
    // FIXME: we do not aggregate `bin` because its tests delete jars, thus breaking other tests
    aggregate(`dotty-interfaces`, `dotty-library`, `dotty-compiler`, `dotty-doc`, dottySbtBridgeRef,
      `scala-library`, `scala-compiler`, `scala-reflect`, scalap).
    dependsOn(`dotty-compiler`).
    settings(
      triggeredMessage in ThisBuild := Watched.clearWhenTriggered,

      addCommandAlias("run", "dotty-compiler/run") ++
      addCommandAlias("legacyTests", "dotty-compiler/testOnly dotc.tests")
    ).
    settings(publishing)

  // Meta project aggregating all bootstrapped projects
  lazy val `dotty-bootstrapped` = project.
    aggregate(`dotty-library-bootstrapped`, `dotty-compiler-bootstrapped`, `dotty-doc-bootstrapped`).
    settings(
      publishArtifact := false
    )

  lazy val `dotty-interfaces` = project.in(file("interfaces")).
    settings(sourceStructure).
    settings(
      // Do not append Scala versions to the generated artifacts
      crossPaths := false,
      // Do not depend on the Scala library
      autoScalaLibrary := false,
      // Let the sbt eclipse plugin know that this is a Java-only project
      EclipseKeys.projectFlavor := EclipseProjectFlavor.Java,
      //Remove javac invalid options in Compile doc
      javacOptions in (Compile, doc) --= Seq("-Xlint:unchecked", "-Xlint:deprecation")
    ).
    settings(publishing)

  // Settings shared between dotty-doc and dotty-doc-bootstrapped
  lazy val dottyDocSettings = Seq(
    baseDirectory in (Compile, run) := baseDirectory.value / "..",
    baseDirectory in (Test, run) := baseDirectory.value,

    connectInput in run := true,
    outputStrategy := Some(StdoutOutput),

    javaOptions ++= (javaOptions in `dotty-compiler`).value,
    fork in run := true,
    fork in Test := true,
    parallelExecution in Test := false,

    genDocs := Def.inputTaskDyn {
      val dottyLib = (packageAll in `dotty-compiler`).value("dotty-library")
      val dottyInterfaces = (packageAll in `dotty-compiler`).value("dotty-interfaces")
      val otherDeps = (dependencyClasspath in Compile).value.map(_.data).mkString(":")
      val sources =
        (unmanagedSources in (Compile, compile)).value ++
          (unmanagedSources in (`dotty-compiler`, Compile)).value
      val args: Seq[String] = Seq(
        "-siteroot", "docs",
        "-project", "Dotty",
        "-classpath", s"$dottyLib:$dottyInterfaces:$otherDeps"
      )
        (runMain in Compile).toTask(
          s""" dotty.tools.dottydoc.Main ${args.mkString(" ")} ${sources.mkString(" ")}"""
        )
    }.evaluated,

    dottydoc := Def.inputTaskDyn {
      val args: Seq[String] = spaceDelimited("<arg>").parsed
      val dottyLib = (packageAll in `dotty-compiler`).value("dotty-library")
      val dottyInterfaces = (packageAll in `dotty-compiler`).value("dotty-interfaces")
      val otherDeps = (dependencyClasspath in Compile).value.map(_.data).mkString(":")
      val cp: Seq[String] = Seq("-classpath", s"$dottyLib:$dottyInterfaces:$otherDeps")
        (runMain in Compile).toTask(s""" dotty.tools.dottydoc.Main ${cp.mkString(" ")} """ + args.mkString(" "))
    }.evaluated,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "com.vladsch.flexmark" % "flexmark" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-gfm-tasklist" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-gfm-tables" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-autolink" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-anchorlink" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-emoji" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-gfm-strikethrough" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-yaml-front-matter" % "0.11.1",
      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.8.6",
      "nl.big-o" % "liqp" % "0.6.7"
    )
  )

  lazy val `dotty-doc` = project.in(file("doc-tool")).
    dependsOn(`dotty-compiler`, `dotty-compiler` % "test->test").
    settings(sourceStructure).
    settings(dottyDocSettings).
    settings(publishing)

  lazy val `dotty-doc-bootstrapped` = project.in(file("doc-tool")).
    dependsOn(`dotty-compiler-bootstrapped`, `dotty-compiler-bootstrapped` % "test->test").
    settings(sourceStructure).
    settings(commonBootstrappedSettings).
    settings(dottyDocSettings)


  lazy val `dotty-bot` = project.in(file("bot")).
    settings(sourceStructure).
    settings(
      resourceDirectory in Test := baseDirectory.value / "test" / "resources",

      // specify main and ignore tests when assembling
      mainClass in assembly := Some("dotty.tools.bot.Main"),
      test in assembly := {},

      libraryDependencies ++= {
        val circeVersion = "0.7.0"
        val http4sVersion = "0.15.3"
        Seq(
          "com.novocode" % "junit-interface" % "0.11" % "test",
          "io.circe" %% "circe-generic" % circeVersion,
          "io.circe" %% "circe-parser" % circeVersion,
          "ch.qos.logback" % "logback-classic" % "1.1.7",
          "org.http4s" %% "http4s-dsl" % http4sVersion,
          "org.http4s" %% "http4s-blaze-server" % http4sVersion,
          "org.http4s" %% "http4s-blaze-client" % http4sVersion,
          "org.http4s" %% "http4s-circe" % http4sVersion
        )
      }
    )

  // Settings shared between dotty-compiler and dotty-compiler-bootstrapped
  lazy val dottyCompilerSettings = Seq(
      // The scala-backend folder is a git submodule that contains a fork of the Scala 2.11
      // compiler developed at https://github.com/lampepfl/scala/tree/sharing-backend.
      // We do not compile the whole submodule, only the part of the Scala 2.11 GenBCode backend
      // that we reuse for dotty.
      // See http://dotty.epfl.ch/docs/contributing/backend.html for more information.
      //
      // NOTE: We link (or copy if symbolic links are not supported) these sources in
      // the current project using `sourceGenerators` instead of simply
      // referencing them using `unmanagedSourceDirectories` because the latter
      // breaks some IDEs.
      sourceGenerators in Compile += Def.task {
        val outputDir = (sourceManaged in Compile).value

        val submoduleCompilerDir = baseDirectory.value / ".." / "scala-backend" / "src" / "compiler"
        val backendDir = submoduleCompilerDir / "scala" / "tools" / "nsc" / "backend"
        val allScalaFiles = GlobFilter("*.scala")

        // NOTE: Keep these exclusions synchronized with the ones in the tests (CompilationTests.scala)
        val files = ((backendDir *
          (allScalaFiles - "JavaPlatform.scala" - "Platform.scala" - "ScalaPrimitives.scala")) +++
         (backendDir / "jvm") *
          (allScalaFiles - "BCodeICodeCommon.scala" - "GenASM.scala" - "GenBCode.scala" - "ScalacBackendInterface.scala")
        ).get

        val pairs = files.pair(sbt.Path.rebase(submoduleCompilerDir, outputDir))

        try {
          pairs.foreach { case (src, dst) =>
            sbt.IO.createDirectory(dst.getParentFile)
            if (!dst.exists)
              Files.createSymbolicLink(/*link = */ dst.toPath, /*existing = */src.toPath)
          }
        } catch {
          case e: UnsupportedOperationException =>
            // If the OS doesn't support symbolic links, copy the directory instead.
            sbt.IO.copy(pairs, overwrite = true, preserveLastModified = true)
        }

        pairs.map(_._2)
      }.taskValue,

      // Used by the backend
      libraryDependencies += "org.scala-lang.modules" % "scala-asm" % "5.1.0-scala-2",

      // set system in/out for repl
      connectInput in run := true,
      outputStrategy := Some(StdoutOutput),

      // Generate compiler.properties, used by sbt
      resourceGenerators in Compile += Def.task {
        val file = (resourceManaged in Compile).value / "compiler.properties"
        val contents = s"version.number=${version.value}"

        if (!(file.exists && IO.read(file) == contents)) {
          IO.write(file, contents)
        }

        Seq(file)
      }.taskValue,

      // include sources in eclipse (downloads source code for all dependencies)
      //http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
      com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys.withSource := true,

      // get libraries onboard
      resolvers += Resolver.typesafeIvyRepo("releases"), // For org.scala-sbt:interface
      libraryDependencies ++= Seq("org.scala-sbt" % "interface" % sbtVersion.value,
                                  "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
                                  "com.novocode" % "junit-interface" % "0.11" % "test",
                                  "org.scala-lang" % "scala-reflect" % scalacVersion,
                                  "org.scala-lang" % "scala-library" % scalacVersion % "test"),

      // enable improved incremental compilation algorithm
      incOptions := incOptions.value.withNameHashing(true),

      // For convenience, change the baseDirectory when running the compiler
      baseDirectory in (Compile, run) := baseDirectory.value / "..",
      // .. but not when running test
      baseDirectory in (Test, run) := baseDirectory.value,

      repl := Def.inputTaskDyn {
        val args: Seq[String] = spaceDelimited("<arg>").parsed
        val dottyLib = packageAll.value("dotty-library")
        (runMain in Compile).toTask(
          s" dotty.tools.dotc.repl.Main -classpath $dottyLib " + args.mkString(" ")
        )
      }.evaluated,

      test in Test := {
        // Exclude legacy tests by default
        (testOnly in Test).toTask(" -- --exclude-categories=java.lang.Exception").value
      },

      vulpix := Def.inputTaskDyn {
        val args: Seq[String] = spaceDelimited("<arg>").parsed
        val cmd = " dotty.tools.dotc.CompilationTests" + {
          if (args.nonEmpty) " -- -Ddotty.tests.filter=" + args.mkString(" ")
          else ""
        }
        (testOnly in Test).toTask(cmd)
      }.evaluated,

      // Override run to be able to run compiled classfiles
      dotr := {
        val args: Seq[String] = spaceDelimited("<arg>").parsed
        val java: String = Process("which" :: "java" :: Nil).!!
        val attList = (dependencyClasspath in Runtime).value
        val _  = packageAll.value
        val scalaLib = attList
          .map(_.data.getAbsolutePath)
          .find(_.contains("scala-library"))
          .toList.mkString(":")

        if (java == "")
          println("Couldn't find java executable on path, please install java to a default location")
        else if (scalaLib == "") {
          println("Couldn't find scala-library on classpath, please run using script in bin dir instead")
        } else {
          val dottyLib = packageAll.value("dotty-library")
          s"""$java -classpath .:$dottyLib:$scalaLib ${args.mkString(" ")}""".!
        }
      },
      run := Def.inputTaskDyn {
        val dottyLib = packageAll.value("dotty-library")
        val args: Seq[String] = spaceDelimited("<arg>").parsed

        val fullArgs = args.span(_ != "-classpath") match {
          case (beforeCp, Nil) => beforeCp ++ ("-classpath" :: dottyLib :: Nil)
          case (beforeCp, rest) => beforeCp ++ rest
        }

        (runMain in Compile).toTask(
          s" dotty.tools.dotc.Main " + fullArgs.mkString(" ")
        )
      }.evaluated,
      dotc := run.evaluated,

      // enable verbose exception messages for JUnit
      testOptions in Test += Tests.Argument(
        TestFrameworks.JUnit, "-a", "-v",
        "--run-listener=dotty.tools.ContextEscapeDetector"
      ),

      /* Add the sources of scalajs-ir.
       * To guarantee that dotty can bootstrap without depending on a version
       * of scalajs-ir built with a different Scala compiler, we add its
       * sources instead of depending on the binaries.
       */
      //TODO: disabling until moved to separate project
      //ivyConfigurations += config("sourcedeps").hide,
      //libraryDependencies +=
      //  "org.scala-js" %% "scalajs-ir" % scalaJSVersion % "sourcedeps",
      //sourceGenerators in Compile += Def.task {
      //  val s = streams.value
      //  val cacheDir = s.cacheDirectory
      //  val trgDir = (sourceManaged in Compile).value / "scalajs-ir-src"

      //  val report = updateClassifiers.value
      //  val scalaJSIRSourcesJar = report.select(
      //      configuration = Set("sourcedeps"),
      //      module = (_: ModuleID).name.startsWith("scalajs-ir_"),
      //      artifact = artifactFilter(`type` = "src")).headOption.getOrElse {
      //    sys.error(s"Could not fetch scalajs-ir sources")
      //  }

      //  FileFunction.cached(cacheDir / s"fetchScalaJSIRSource",
      //      FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
      //    s.log.info(s"Unpacking scalajs-ir sources to $trgDir...")
      //    if (trgDir.exists)
      //      IO.delete(trgDir)
      //    IO.createDirectory(trgDir)
      //    IO.unzip(scalaJSIRSourcesJar, trgDir)
      //    (trgDir ** "*.scala").get.toSet
      //  } (Set(scalaJSIRSourcesJar)).toSeq
      //}.taskValue,

      // Spawn new JVM in run and test
      fork in run := true,
      fork in Test := true,
      parallelExecution in Test := false,

      // Add git-hash used to package the distribution to the manifest to know it in runtime and report it in REPL
      packageOptions += ManifestAttributes(("Git-Hash", VersionUtil.gitHash)),

      // http://grokbase.com/t/gg/simple-build-tool/135ke5y90p/sbt-setting-jvm-boot-paramaters-for-scala
      // packageAll should always be run before tests
      javaOptions ++= {
        val attList = (dependencyClasspath in Runtime).value
        val pA = packageAll.value

        // put needed dependencies on classpath:
        val path = for {
          file <- attList.map(_.data)
          path = file.getAbsolutePath
          // FIXME: when we snip the cord, this should go bye-bye
          if path.contains("scala-library") ||
            // FIXME: currently needed for tests referencing scalac internals
            path.contains("scala-reflect") ||
            // FIXME: should go away when xml literal parsing is removed
            path.contains("scala-xml") ||
            // used for tests that compile dotty
            path.contains("scala-asm") ||
            // needed for the xsbti interface
            path.contains("org.scala-sbt/interface/")
        } yield "-Xbootclasspath/p:" + path

        val ci_build = // propagate if this is a ci build
          if (sys.props.isDefinedAt(JENKINS_BUILD))
            List(s"-D$JENKINS_BUILD=${sys.props(JENKINS_BUILD)}") ::: jenkinsMemLimit
          else if (sys.props.isDefinedAt(DRONE_MEM))
            List("-Xmx" + sys.props(DRONE_MEM))
          else List()

        val tuning =
          if (sys.props.isDefinedAt("Oshort"))
            // Optimize for short-running applications, see https://github.com/lampepfl/dotty/issues/222
            List("-XX:+TieredCompilation", "-XX:TieredStopAtLevel=1")
          else List()

        val jars = List(
          "-Ddotty.tests.classes.interfaces=" + pA("dotty-interfaces"),
          "-Ddotty.tests.classes.library=" + pA("dotty-library"),
          "-Ddotty.tests.classes.compiler=" + pA("dotty-compiler")
        )

        jars ::: tuning ::: agentOptions ::: ci_build ::: path.toList
      }
  )

  lazy val `dotty-compiler` = project.in(file("compiler")).
    dependsOn(`dotty-interfaces`).
    settings(sourceStructure).
    settings(dottyCompilerSettings).
    settings(
      // Disable scaladoc generation, it's way too slow and we'll replace it
      // by dottydoc anyway. We still publish an empty -javadoc.jar to make
      // sonatype happy.
      sources in (Compile, doc) := Seq(),

      // packageAll packages all and then returns a map with the abs location
      packageAll := {
        Map(
          "dotty-interfaces" -> (packageBin in (`dotty-interfaces`, Compile)).value,
          "dotty-compiler" -> (packageBin in Compile).value,
          "dotty-library" -> (packageBin in (`dotty-library-bootstrapped`, Compile)).value,
          "dotty-compiler-test" -> (packageBin in Test).value
        ) map { case (k, v) => (k, v.getAbsolutePath) }
      }
    ).
    settings(publishing)

  lazy val `dotty-compiler-bootstrapped` = project.in(file("compiler")).
    dependsOn(`dotty-library-bootstrapped`).
    settings(sourceStructure).
    settings(commonBootstrappedSettings).
    settings(dottyCompilerSettings).
    settings(
      // Used instead of "dependsOn(`dotty-interfaces`)" because the latter breaks sbt somehow
      libraryDependencies += scalaOrganization.value % "dotty-interfaces" % version.value,

      packageAll := {
        (packageAll in `dotty-compiler`).value ++ Seq(
          ("dotty-compiler" -> (packageBin in Compile).value.getAbsolutePath),
          ("dotty-library" -> (packageBin in (`dotty-library-bootstrapped`, Compile)).value.getAbsolutePath)
        )
      }
    )

  /* Contains unit tests for the scripts */
  lazy val `dotty-bin-tests` = project.in(file("bin")).
    settings(sourceStructure).
    settings(
      publishArtifact := false,
      parallelExecution in Test := false,
      libraryDependencies +=
        "com.novocode" % "junit-interface" % "0.11" % "test"
    )

  // Settings shared between dotty-library and dotty-library-bootstrapped
  lazy val dottyLibrarySettings = Seq(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalacVersion,
        "org.scala-lang" % "scala-library" % scalacVersion,
        "com.novocode" % "junit-interface" % "0.11" % "test"
      )
  )

  lazy val `dotty-library` = project.in(file("library")).
    settings(unmanagedSourceDirectories in Compile += baseDirectory.value / "src_scalac").
    settings(sourceStructure).
    settings(dottyLibrarySettings).
    settings(publishing)

  lazy val `dotty-library-bootstrapped` = project.in(file("library")).
    settings(unmanagedSourceDirectories in Compile += baseDirectory.value / "src_dotc").
    settings(sourceStructure).
    settings(commonBootstrappedSettings).
    settings(dottyLibrarySettings)

  // until sbt/sbt#2402 is fixed (https://github.com/sbt/sbt/issues/2402)
  lazy val cleanSbtBridge = TaskKey[Unit]("cleanSbtBridge", "delete dotty-sbt-bridge cache")

  lazy val `dotty-sbt-bridge` = project.in(file("sbt-bridge")).
    dependsOn(`dotty-compiler`).
    dependsOn(`dotty-library`).
    settings(sourceStructure).
    settings(
      cleanSbtBridge := {
        val dottySbtBridgeVersion = version.value
        val dottyVersion = (version in `dotty-compiler`).value
        val classVersion = System.getProperty("java.class.version")

        val sbtV = sbtVersion.value
        val sbtOrg = "org.scala-sbt"
        val sbtScalaVersion = "2.10.6"

        val home = System.getProperty("user.home")
        val org = organization.value
        val artifact = moduleName.value

        IO.delete(file(home) / ".ivy2" / "cache" / sbtOrg / s"$org-$artifact-$dottySbtBridgeVersion-bin_${dottyVersion}__$classVersion")
        IO.delete(file(home) / ".sbt"  / "boot" / s"scala-$sbtScalaVersion" / sbtOrg / "sbt" / sbtV / s"$org-$artifact-$dottySbtBridgeVersion-bin_${dottyVersion}__$classVersion")
      },
      publishLocal := (publishLocal.dependsOn(cleanSbtBridge)).value,
      description := "sbt compiler bridge for Dotty",
      resolvers += Resolver.typesafeIvyRepo("releases"), // For org.scala-sbt stuff
      libraryDependencies ++= Seq(
        "org.scala-sbt" % "interface" % sbtVersion.value,
        "org.scala-sbt" % "api" % sbtVersion.value % "test",
        "org.specs2" %% "specs2" % "2.3.11" % "test"
      ),
      // The sources should be published with crossPaths := false since they
      // need to be compiled by the project using the bridge.
      crossPaths := false,

      // Don't publish any binaries for the bridge because of the above
      publishArtifact in (Compile, packageBin) := false,

      fork in Test := true,
      parallelExecution in Test := false
    ).
    settings(ScriptedPlugin.scriptedSettings: _*).
    settings(
      ScriptedPlugin.sbtTestDirectory := baseDirectory.value / "sbt-test",
      ScriptedPlugin.scriptedLaunchOpts := Seq("-Xmx1024m"),
      ScriptedPlugin.scriptedBufferLog := false,
      ScriptedPlugin.scripted := {
        val x1 = (publishLocal in `dotty-interfaces`).value
        val x2 = (publishLocal in `dotty-compiler`).value
        val x3 = (publishLocal in `dotty-library-bootstrapped`).value
        val x4 = (publishLocal in dotty).value // Needed because sbt currently hardcodes the dotty artifact
        ScriptedPlugin.scriptedTask.evaluated
      }
      // TODO: Use this instead of manually copying DottyInjectedPlugin.scala
      // everywhere once https://github.com/sbt/sbt/issues/2601 gets fixed.
      /*,
      ScriptedPlugin.scriptedPrescripted := { f =>
        IO.write(inj, """
import sbt._
import Keys._

object DottyInjectedPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  override val projectSettings = Seq(
    scalaVersion := "0.1.1-bin-SNAPSHOT",
    scalaOrganization := "ch.epfl.lamp",
    scalacOptions += "-language:Scala2",
    scalaBinaryVersion  := "2.11",
    autoScalaLibrary := false,
    libraryDependencies ++= Seq("org.scala-lang" % "scala-library" % "2.11.5"),
    scalaCompilerBridgeSource := ("ch.epfl.lamp" % "dotty-sbt-bridge" % scalaVersion.value % "component").sources()
  )
}
""")
      }
      */
    ).
    settings(publishing)

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
    enablePlugins(ScalaJSPlugin).
    settings(sourceStructure).
    settings(
      /* Remove the Scala.js compiler plugin for scalac, and enable the
       * Scala.js back-end of dotty instead.
       */
      libraryDependencies ~= { deps =>
        deps.filterNot(_.name.startsWith("scalajs-compiler"))
      },
      scalacOptions += "-scalajs",

      // The main class cannot be found automatically due to the empty inc.Analysis
      mainClass in Compile := Some("hello.world"),

      // While developing the Scala.js back-end, it is very useful to see the trees dotc gives us
      scalacOptions += "-Xprint:labelDef",

      /* Debug-friendly Scala.js optimizer options.
       * In particular, typecheck the Scala.js IR found on the classpath.
       */
      scalaJSOptimizerOptions ~= {
        _.withCheckScalaJSIR(true).withParallel(false)
      }
    ).
    settings(compileWithDottySettings).
    settings(inConfig(Compile)(Seq(
      /* Make sure jsDependencyManifest runs after compile, otherwise compile
       * might remove the entire directory afterwards.
       */
      jsDependencyManifest := jsDependencyManifest.dependsOn(compile).value
    )))

  lazy val `dotty-bench` = project.in(file("bench")).
    dependsOn(`dotty-compiler` % "compile->test").
    settings(sourceStructure).
    settings(
      baseDirectory in (Test,run) := (baseDirectory in `dotty-compiler`).value,

      libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6" % Test,

      fork in Test := true,
      parallelExecution in Test := false,

      // http://grokbase.com/t/gg/simple-build-tool/135ke5y90p/sbt-setting-jvm-boot-paramaters-for-scala
      javaOptions ++= {
        val attList = (dependencyClasspath in Runtime).value
        val bin = (packageBin in Compile).value

        // put the Scala {library, reflect, compiler} in the classpath
        val path = for {
          file <- attList.map(_.data)
          path = file.getAbsolutePath
          prefix = if (path.endsWith(".jar")) "p" else "a"
        } yield "-Xbootclasspath/" + prefix + ":" + path
        // dotty itself needs to be in the bootclasspath
        val fullpath = ("-Xbootclasspath/a:" + bin) :: path.toList
        // System.err.println("BOOTPATH: " + fullpath)

        val ci_build = // propagate if this is a ci build
          if (sys.props.isDefinedAt(JENKINS_BUILD))
            List(s"-D$JENKINS_BUILD=${sys.props(JENKINS_BUILD)}")
          else if (sys.props.isDefinedAt(DRONE_MEM))
            List("-Xmx" + sys.props(DRONE_MEM))
          else
            List()
        val res = agentOptions ::: ci_build ::: fullpath
        println("Running with javaOptions: " + res)
        res
      }
    )


  // Dummy scala-library artefact. This is useful because sbt projects
  // automatically depend on scalaOrganization.value % "scala-library" % scalaVersion.value
  lazy val `scala-library` = project.
    dependsOn(`dotty-library`).
    settings(
      crossPaths := false
    ).
    settings(publishing)

  // sbt >= 0.13.12 will automatically rewrite transitive dependencies on
  // any version in any organization of scala{-library,-compiler,-reflect,p}
  // to have organization `scalaOrganization` and version `scalaVersion`
  // (see https://github.com/sbt/sbt/pull/2634).
  // This means that we need to provide dummy artefacts for these projects,
  // otherwise users will get compilation errors if they happen to transitively
  // depend on one of these projects.
  lazy val `scala-compiler` = project.
    settings(
      crossPaths := false
    ).
    settings(publishing)
  lazy val `scala-reflect` = project.
    settings(
      crossPaths := false,
      libraryDependencies := Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
    ).
    settings(publishing)
  lazy val scalap = project.
    settings(
      crossPaths := false,
      libraryDependencies := Seq("org.scala-lang" % "scalap" % scalaVersion.value)
    ).
    settings(publishing)

   lazy val publishing = Seq(
     publishMavenStyle := true,
     publishArtifact := true,
     isSnapshot := version.value.contains("SNAPSHOT"),
     publishTo := {
       val nexus = "https://oss.sonatype.org/"
       if (isSnapshot.value)
         Some("snapshots" at nexus + "content/repositories/snapshots")
       else
         Some("releases"  at nexus + "service/local/staging/deploy/maven2")
     },
     publishArtifact in Test := false,
     homepage := Some(url("https://github.com/lampepfl/dotty")),
     licenses += ("BSD New",
       url("https://github.com/lampepfl/dotty/blob/master/LICENSE.md")),
     scmInfo := Some(
       ScmInfo(
         url("https://github.com/lampepfl/dotty"),
         "scm:git:git@github.com:lampepfl/dotty.git"
       )
     ),
     pomExtra := (
       <developers>
         <developer>
           <id>odersky</id>
           <name>Martin Odersky</name>
           <email>martin.odersky@epfl.ch</email>
           <url>https://github.com/odersky</url>
         </developer>
         <developer>
           <id>DarkDimius</id>
           <name>Dmitry Petrashko</name>
           <email>me@d-d.me</email>
           <url>https://d-d.me</url>
         </developer>
         <developer>
           <id>smarter</id>
           <name>Guillaume Martres</name>
           <email>smarter@ubuntu.com</email>
           <url>http://guillaume.martres.me</url>
         </developer>
         <developer>
           <id>felixmulder</id>
           <name>Felix Mulder</name>
           <email>felix.mulder@gmail.com</email>
           <url>http://felixmulder.com</url>
         </developer>
         <developer>
           <id>liufengyun</id>
           <name>Liu Fengyun</name>
           <email>liufengyun@chaos-lab.com</email>
           <url>http://chaos-lab.com</url>
         </developer>
       </developers>
     )
   )

  // Compile with dotty
  lazy val compileWithDottySettings = {
    inConfig(Compile)(inTask(compile)(Defaults.runnerTask) ++ Seq(
      // Compile with dotty
      fork in compile := true,

      compile := {
        val inputs = (compileInputs in compile).value
        import inputs.config._

        val s = streams.value
        val logger = s.log
        val cacheDir = s.cacheDirectory

        // Discover classpaths

        def cpToString(cp: Seq[File]) =
          cp.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

        val compilerCp = Attributed.data((fullClasspath in (`dotty-compiler`, Compile)).value)
        val cpStr = cpToString(classpath ++ compilerCp)

        // List all my dependencies (recompile if any of these changes)

        val allMyDependencies = classpath filterNot (_ == classesDirectory) flatMap { cpFile =>
          if (cpFile.isDirectory) (cpFile ** "*.class").get
          else Seq(cpFile)
        }

        // Compile

        val cachedCompile = FileFunction.cached(cacheDir / "compile",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>

          logger.info(
              "Compiling %d Scala sources to %s..." format (
              sources.size, classesDirectory))

          if (classesDirectory.exists)
            IO.delete(classesDirectory)
          IO.createDirectory(classesDirectory)

          val sourcesArgs = sources.map(_.getAbsolutePath()).toList

          /* run.run() below in doCompile() will emit a call to its
           * logger.info("Running dotty.tools.dotc.Main [...]")
           * which we do not want to see. We use this patched logger to
           * filter out that particular message.
           */
          val patchedLogger = new Logger {
            def log(level: Level.Value, message: => String) = {
              val msg = message
              if (level != Level.Info ||
                  !msg.startsWith("Running dotty.tools.dotc.Main"))
                logger.log(level, msg)
            }
            def success(message: => String) = logger.success(message)
            def trace(t: => Throwable) = logger.trace(t)
          }

          def doCompile(sourcesArgs: List[String]): Unit = {
            val run = (runner in compile).value
            run.run("dotty.tools.dotc.Main", compilerCp,
                "-classpath" :: cpStr ::
                "-d" :: classesDirectory.getAbsolutePath() ::
                options ++:
                sourcesArgs,
                patchedLogger) foreach sys.error
          }

          // Work around the Windows limitation on command line length.
          val isWindows =
            System.getProperty("os.name").toLowerCase().indexOf("win") >= 0
          if ((fork in compile).value && isWindows &&
              (sourcesArgs.map(_.length).sum > 1536)) {
            IO.withTemporaryFile("sourcesargs", ".txt") { sourceListFile =>
              IO.writeLines(sourceListFile, sourcesArgs)
              doCompile(List("@"+sourceListFile.getAbsolutePath()))
            }
          } else {
            doCompile(sourcesArgs)
          }

          // Output is all files in classesDirectory
          (classesDirectory ** AllPassFilter).get.toSet
        }

        cachedCompile((sources ++ allMyDependencies).toSet)

        // We do not have dependency analysis when compiling externally
        sbt.inc.Analysis.Empty
      }
    ))
  }

  private def projectChecks(): Unit = {
    val scalaScala = new File("scala-scala")
    if (!scalaScala.exists()) {
      println(
        s"""[WARNING] Missing `dotty/scala-scala` library
           |You can clone the library with:
           |  > git clone -b dotty-library https://github.com/DarkDimius/scala.git ${scalaScala.getAbsolutePath}
        """.stripMargin)
    }
  }
}
