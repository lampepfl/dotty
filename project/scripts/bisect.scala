//> using jvm 17 // Maximal JDK version which can be used with all Scala 3 versions, can be overriden via command line arguments '--jvm=21'
/*
This script will bisect a problem with the compiler based on success/failure of the validation script passed as an argument.
It starts with a fast bisection on released nightly builds.
Then it will bisect the commits between the last nightly that worked and the first nightly that failed.
Look at the `usageMessage` below for more details.
*/


import sys.process._
import scala.io.Source
import java.io.File
import java.nio.file.attribute.PosixFilePermissions
import java.nio.charset.StandardCharsets
import java.nio.file.Files

val usageMessage = """
  |Usage:
  |  > scala-cli project/scripts/bisect.scala -- [<bisect-options>] <validation-command>
  |
  |The <validation-command> should be one of:
  |* compile <arg1> <arg2> ...
  |* run <arg1> <arg2> ...
  |* <custom-validation-script-path>
  |
  |The arguments for 'compile' and 'run' should be paths to the source file(s) and optionally additional options passed directly to scala-cli.
  |
  |A custom validation script should be executable and accept a single parameter, which will be the scala version to validate.
  |Look at bisect-cli-example.sh and bisect-expect-example.exp for reference.
  |If you want to use one of the example scripts - use a copy of the file instead of modifying it in place because that might mess up the checkout.
  |
  |The optional <bisect-options> may be any combination of:
  |* --dry-run
  |    Don't try to bisect - just make sure the validation command works correctly
  |
  |* --releases <releases-range>
  |    Bisect only releases from the given range (defaults to all releases).
  |    The range format is <first>...<last>, where both <first> and <last> are optional, e.g.
  |    * 3.1.0-RC1-bin-20210827-427d313-NIGHTLY...3.2.1-RC1-bin-20220716-bb9c8ff-NIGHTLY
  |    * 3.2.1-RC1-bin-20220620-de3a82c-NIGHTLY...
  |    * ...3.3.0-RC1-bin-20221124-e25362d-NIGHTLY
  |    The ranges are treated as inclusive.
  |
  |* --bootstrapped
  |    Publish locally and test a bootstrapped compiler rather than a nonboostrapped one.
  |
  |* --should-fail
  |    Expect the validation command to fail rather that succeed. This can be used e.g. to find out when some illegal code started to compile.
  |
  |Warning: The bisect script should not be run multiple times in parallel because of a potential race condition while publishing artifacts locally.

""".stripMargin

@main def run(args: String*): Unit =
  val scriptOptions =
    try ScriptOptions.fromArgs(args)
    catch
      case _ =>
        sys.error(s"Wrong script parameters.\n${usageMessage}")

  val validationScript = scriptOptions.validationCommand.validationScript
  val releases = Releases.fromRange(scriptOptions.releasesRange)
  val releaseBisect = ReleaseBisect(validationScript, shouldFail = scriptOptions.shouldFail, releases)

  releaseBisect.verifyEdgeReleases()

  if (!scriptOptions.dryRun) then
    val (lastGoodRelease, firstBadRelease) = releaseBisect.bisectedGoodAndBadReleases()
    println(s"Last good release: ${lastGoodRelease.version}")
    println(s"First bad release: ${firstBadRelease.version}")
    println("\nFinished bisecting releases\n")

    val commitBisect = CommitBisect(validationScript, shouldFail = scriptOptions.shouldFail, bootstrapped = scriptOptions.bootstrapped, lastGoodRelease.hash, firstBadRelease.hash)
    commitBisect.bisect()


case class ScriptOptions(validationCommand: ValidationCommand, dryRun: Boolean, bootstrapped: Boolean, releasesRange: ReleasesRange, shouldFail: Boolean)
object ScriptOptions:
  def fromArgs(args: Seq[String]) =
    val defaultOptions = ScriptOptions(
      validationCommand = null,
      dryRun = false,
      bootstrapped = false,
      ReleasesRange(first = None, last = None),
      shouldFail = false
    )
    parseArgs(args, defaultOptions)

  private def parseArgs(args: Seq[String], options: ScriptOptions): ScriptOptions =
    args match
      case "--dry-run" :: argsRest => parseArgs(argsRest, options.copy(dryRun = true))
      case "--bootstrapped" :: argsRest => parseArgs(argsRest, options.copy(bootstrapped = true))
      case "--releases" :: argsRest =>
        val range = ReleasesRange.tryParse(argsRest.head).get
        parseArgs(argsRest.tail, options.copy(releasesRange = range))
      case "--should-fail" :: argsRest => parseArgs(argsRest, options.copy(shouldFail = true))
      case _ =>
        val command = ValidationCommand.fromArgs(args)
        options.copy(validationCommand = command)

enum ValidationCommand:
  case Compile(args: Seq[String])
  case Run(args: Seq[String])
  case CustomValidationScript(scriptFile: File)

  def validationScript: File = this match
    case Compile(args) =>
      ValidationScript.tmpScalaCliScript(command = "compile", args)
    case Run(args) =>
      ValidationScript.tmpScalaCliScript(command = "run", args)
    case CustomValidationScript(scriptFile) =>
      ValidationScript.copiedFrom(scriptFile)

object ValidationCommand:
  def fromArgs(args: Seq[String]) = args match
    case Seq("compile", commandArgs*) => Compile(commandArgs)
    case Seq("run", commandArgs*) => Run(commandArgs)
    case Seq(path) => CustomValidationScript(new File(path))


object ValidationScript:
  def copiedFrom(file: File): File =
    val fileContent = scala.io.Source.fromFile(file).mkString
    tmpScript(fileContent)

  def tmpScalaCliScript(command: String, args: Seq[String]): File = tmpScript(s"""
    |#!/usr/bin/env bash
    |export JAVA_HOME=${sys.props("java.home")}
    |scala-cli ${command} -S "$$1" --server=false ${args.mkString(" ")}
    |""".stripMargin
  )

  private def tmpScript(content: String): File =
    val executableAttr = PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwxr-xr-x"))
    val tmpPath = Files.createTempFile("scala-bisect-validator", "", executableAttr)
    val tmpFile = tmpPath.toFile

    print(s"Bisecting with validation script: ${tmpPath.toAbsolutePath}\n")
    print("#####################################\n")
    print(s"${content}\n\n")
    print("#####################################\n\n")

    tmpFile.deleteOnExit()
    Files.write(tmpPath, content.getBytes(StandardCharsets.UTF_8))
    tmpFile


case class ReleasesRange(first: Option[String], last: Option[String]):
  def filter(releases: Seq[Release]) =
    def releaseIndex(version: String): Int =
      val index = releases.indexWhere(_.version == version)
      assert(index > 0, s"${version} matches no nightly compiler release")
      index

    val startIdx = first.map(releaseIndex(_)).getOrElse(0)
    val endIdx = last.map(releaseIndex(_) + 1).getOrElse(releases.length)
    val filtered = releases.slice(startIdx, endIdx).toVector
    assert(filtered.nonEmpty, "No matching releases")
    filtered

object ReleasesRange:
  def all = ReleasesRange(None, None)
  def tryParse(range: String): Option[ReleasesRange] = range match
    case s"${first}...${last}" => Some(ReleasesRange(
      Some(first).filter(_.nonEmpty),
      Some(last).filter(_.nonEmpty)
    ))
    case _ => None

class Releases(val releases: Vector[Release])

object Releases:
  lazy val allReleases: Vector[Release] =
    val re = raw"<version>(.+-bin-\d{8}-\w{7}-NIGHTLY)</version>".r
    val xml = io.Source.fromURL(
      "https://repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/maven-metadata.xml"
    )
    re.findAllMatchIn(xml.mkString)
      .flatMap{ m => Option(m.group(1)).map(Release.apply) }
      .toVector

  def fromRange(range: ReleasesRange): Vector[Release] = range.filter(allReleases)

case class Release(version: String):
  private val re = raw".+-bin-(\d{8})-(\w{7})-NIGHTLY".r
  def date: String =
    version match
      case re(date, _) => date
      case _ => sys.error(s"Could not extract date from release name: $version")
  def hash: String =
    version match
      case re(_, hash) => hash
      case _ => sys.error(s"Could not extract hash from release name: $version")

  override def toString: String = version


class ReleaseBisect(validationScript: File, shouldFail: Boolean, allReleases: Vector[Release]):
  assert(allReleases.length > 1, "Need at least 2 releases to bisect")

  private val isGoodReleaseCache = collection.mutable.Map.empty[Release, Boolean]

  def verifyEdgeReleases(): Unit =
    println(s"Verifying the first release: ${allReleases.head.version}")
    assert(isGoodRelease(allReleases.head), s"The evaluation script unexpectedly failed for the first checked release")
    println(s"Verifying the last release: ${allReleases.last.version}")
    assert(!isGoodRelease(allReleases.last), s"The evaluation script unexpectedly succeeded for the last checked release")

  def bisectedGoodAndBadReleases(): (Release, Release) =
    val firstBadRelease = bisect(allReleases)
    assert(!isGoodRelease(firstBadRelease), s"Bisection error: the 'first bad release' ${firstBadRelease.version} is not a bad release")
    val lastGoodRelease = firstBadRelease.previous
    assert(isGoodRelease(lastGoodRelease), s"Bisection error: the 'last good release' ${lastGoodRelease.version} is not a good release")
    (lastGoodRelease, firstBadRelease)

  extension (release: Release) private def previous: Release =
    val idx = allReleases.indexOf(release)
    allReleases(idx - 1)

  private def bisect(releases: Vector[Release]): Release =
    if releases.length == 2 then
      if isGoodRelease(releases.head) then releases.last
      else releases.head
    else
      val mid = releases(releases.length / 2)
      if isGoodRelease(mid) then bisect(releases.drop(releases.length / 2))
      else bisect(releases.take(releases.length / 2 + 1))

  private def isGoodRelease(release: Release): Boolean =
    isGoodReleaseCache.getOrElseUpdate(release, {
      println(s"Testing ${release.version}")
      val result = Seq(validationScript.getAbsolutePath, release.version).!
      val isGood = if(shouldFail) result != 0 else result == 0 // invert the process status if failure was expected
      println(s"Test result: ${release.version} is a ${if isGood then "good" else "bad"} release\n")
      isGood
    })

class CommitBisect(validationScript: File, shouldFail: Boolean, bootstrapped: Boolean, lastGoodHash: String, fistBadHash: String):
  def bisect(): Unit =
    println(s"Starting bisecting commits $lastGoodHash..$fistBadHash\n")
    val scala3CompilerProject = if bootstrapped then "scala3-compiler-bootstrapped" else "scala3-compiler"
    val scala3Project = if bootstrapped then "scala3-bootstrapped" else "scala3"
    val validationCommandStatusModifier = if shouldFail then "! " else "" // invert the process status if failure was expected
    val bisectRunScript = raw"""
      |scalaVersion=$$(sbt "print ${scala3CompilerProject}/version" | tail -n1)
      |rm -rf out
      |export JAVA_HOME=${sys.props("java.home")}
      |(sbt "clean; set every doc := new File(\"unused\"); set scaladoc/Compile/resourceGenerators := (\`${scala3Project}\`/Compile/resourceGenerators).value; ${scala3Project}/publishLocal" \
      |  || (echo "Failed to build compiler, skip $$scalaVersion"; git bisect skip) \
      |) && ${validationCommandStatusModifier}${validationScript.getAbsolutePath} "$$scalaVersion"
    """.stripMargin
    "git bisect start".!
    s"git bisect bad $fistBadHash".!
    s"git bisect good $lastGoodHash".!
    Seq("git", "bisect", "run", "sh", "-c", bisectRunScript).!
    s"git bisect reset".!
