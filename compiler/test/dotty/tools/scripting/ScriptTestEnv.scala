package dotty
package tools
package scripting

import scala.language.unsafeNulls

import java.io.File
import java.nio.file.{Path, Paths, Files}

import dotty.tools.dotc.config.Properties.*

import scala.sys.process.*
import scala.jdk.CollectionConverters.*

/**
 * Common Code for supporting scripting tests.
 * To override the path to the bash executable, set TEST_BASH=<path-to-bash.exe>
 * To specify where `dist/target/pack/bin` resides, set TEST_CWD=<working-directory>
 * Test scripts run in a bash env, so paths are converted to forward slash via .norm.
 */
object ScriptTestEnv {
  def osname: String = sys.props("os.name").toLowerCase
  def psep: String = sys.props("path.separator")
  def userDir: String = sys.props("user.dir").norm
  def testCwd = envOrElse("TEST_CWD", "").norm // optional working directory TEST_CWD
  def verbose = envOrElse("VERBOSE", "").nonEmpty

  def whichJava: String = whichExe("java")
  def whichBash: String = whichExe("bash")

  lazy val workingDirectory: String = {
    val dirstr = if testCwd.nonEmpty then
      if verbose then printf("TEST_CWD set to [%s]\n", testCwd)
      testCwd
    else
      userDir // userDir, if TEST_CWD not set

    // issue warning if things don't look right
    val test = Paths.get(s"$dirstr/dist/target/pack/bin").normalize
    if !test.isDirectory then
      printf("warning: not found below working directory: %s\n", test.norm)

    if verbose then printf("working directory is [%s]\n", dirstr)
    dirstr
  }

  def envPath: String = envOrElse("PATH", "")
  // remove duplicate entries in path
  def supplementedPath: String = s"dist/target/pack/bin$psep$envJavaHome/bin$psep$envScalaHome/bin$psep$envPath".norm
  def adjustedPathEntries: List[String] = supplementedPath.norm.split(psep).toList.distinct
  def adjustedPath: String = adjustedPathEntries.mkString(psep)
  def envPathEntries: List[String] = envPath.split(psep).toList.distinct

  def bashExe: String = envOrElse("TEST_BASH", whichBash)

  def unameExe = which("uname")
  def ostypeFull = if unameExe.nonEmpty then exec(unameExe).mkString else ""
  def ostype = ostypeFull.toLowerCase.takeWhile{ cc => cc >= 'a' && cc <='z' || cc >= 'A' && cc <= 'Z' }

  def cygwin = ostype == "cygwin"
  def mingw = ostype == "mingw"
  def msys = ostype == "msys"
  def winshell: Boolean = cygwin || mingw || msys

  def which(str: String) =
    var out = ""
    // must not use adjusted path here! (causes recursive call / stack overflow)
    envPathEntries.find { entry =>
      val it = Paths.get(entry).toAbsolutePath.normalize
      it.toFile.isDirectory && {
        var testpath = s"$it/$str".norm
        val test = Paths.get(testpath)
        if test.toFile.exists then
          out = testpath
          true
        else
          val test = Paths.get(s"$it/$str.exe".norm)
          if test.toFile.exists then
            out = testpath
            true
          else
            false
        }
      }
    out

  def whichExe(basename: String): String =
    val exeName = if (osname.toLowerCase.startsWith("windows")) s"$basename.exe" else basename
    which(exeName)

  /* returned values are:
   * validTest: Boolean   - false if permissions problems occur, true otherwise
   * exitVal:   Int       - the conventional return error code, where zero implies "no errors".
   * stdout: Seq[String]  - the lines captured from STDOUT
   * stderr: Seq[String]  - the lines captured from STDERR
   */
  def bashCommand(cmdstr: String, additionalEnvPairs: List[(String, String)] = Nil): (Boolean, Int, Seq[String], Seq[String]) = {
    var (stdout, stderr) = (List.empty[String], List.empty[String])
    if bashExe.toFile.exists then
      def q = "\""
      printf("bashCmd: %s -c %s\n", bashExe, s"$q$cmdstr$q")
      val cmd = Seq(bashExe, "-c", cmdstr)
      val envPairs = testEnvPairs ++ additionalEnvPairs
      val proc = Process(cmd, None, envPairs *)
      val exitVal = proc ! ProcessLogger (
        (out: String) => stdout ::= out,
        (err: String) => stderr ::= err
      )
      // a misconfigured environment (e.g., script is not executable) can prevent script execution
      val validTest = !stderr.exists(_.contains("Permission denied"))
      if ! validTest then
        System.err.printf("\nunable to execute script, return value is %d\n", exitVal)
        stderr.foreach { System.err.printf("stderr [%s]\n", _) }

      (validTest, exitVal, stdout.reverse, stderr.reverse)
    else
      (false, -1, Nil, Nil)
  }

  def execCmd(command: String, options: String *): Seq[String] =
    val cmd = (command :: options.toList).toSeq
    for {
      line <- Process(cmd).lazyLines_!
    } yield line


  def packBinDir = "dist/target/pack/bin"
  def packLibDir = "dist/target/pack/lib"
  def packBinScalaExists: Boolean = Files.exists(Paths.get(s"$packBinDir/scala"))

  def listJars(dir: String): List[File] =
    val packlibDir = Paths.get(dir).toFile
    if packlibDir.isDirectory then
      packlibDir.listFiles.toList.filter { _.getName.endsWith(".jar") }
    else
      Nil

  // script output expected as "<tag>: <value>"
  def findTaggedLine(tag: String, lines: Seq[String]): String =
    lines.map { stripColors(_) }.find { _.startsWith(tag) } match
      case None =>
        lines.foreach { System.err.printf("line[%s]\n", _) }
        sys.error(s"no $tag: found in script output")
      case Some(cwd) => cwd.dropWhile( _ != ' ').trim // discard tag

  def stripColors(line:String): String =
    // ESC has be seen in the wild replaced by "\u2190"
    // Also, BOM marker appears as ï»¿
    lazy val colorsRegex = "(\u001b|\u2190)\\[[0-9;]*m|ï»¿".r
    colorsRegex.replaceAllIn(line,"")

  def exec(cmd: String *): Seq[String] = Process(cmd).lazyLines_!.toList

  def script2jar(scriptFile: File) =
    val jarName = s"${scriptFile.getName.dropExtension}.jar"
    File(scriptFile.getParent, jarName)

  def showScriptUnderTest(scriptFile: File): Unit =
    printf("===> test script name [%s]\n", scriptFile.getName)

  def callExecutableJar(script: File, jar: File, scriptArgs: Array[String] = Array.empty[String]) = {
    import scala.sys.process._
    val cmd = Array("java", s"-Dscript.path=${script.getName}", "-jar", jar.absPath)
      ++ scriptArgs
    Process(cmd).lazyLines_!.foreach { println }
  }

  ////////////////////////////////////////////////////////////////////////////////

  def createArgsFile(): String =
    val utfCharset = java.nio.charset.StandardCharsets.UTF_8.name
    val path = Files.createTempFile("scriptingTest", ".args")
    val text = s"-classpath ${workingDirectory.absPath}"
    Files.write(path, text.getBytes(utfCharset))
    path.toFile.getAbsolutePath.norm

  def fixHome(s: String): String =
    s.startsWith("~") match {
      case false => s
      case true => s.replaceFirst("~", userHome)
    }

  extension(s: String) {
    def norm: String = s.replace('\\', '/') // bash expects forward slash
    def noDrive = if s.secondChar == ":" then s.drop(2).norm else s.norm
    def toPath: Path = Paths.get(fixHome(s)) // .toAbsolutePath
    def toFile: File = File(s)
    def absPath: String = s.toFile.absPath
    def relpath: String = s.norm.replaceFirst(s"${cwd.norm}/","")
    def isFile: Boolean = s.toFile.isFile
    def isDirectory: Boolean = s.toFile.isDirectory
    def exists: Boolean = s.toFile.exists
    def name: String = s.toFile.getName
    def getName: String = s.toFile.getName
    def dropExtension: String = s.reverse.dropWhile(_ != '.').drop(1).reverse
    def parent(up: Int): String = s.norm.split("/").reverse.drop(up).reverse.mkString("/")
    def secondChar: String = s.take(2).drop(1).mkString("")
  }

  extension(p: Path) {
    def norm: String = p.normalize.toString.replace('\\', '/')
    def noDrive = p.norm match {
      case str if str.drop(1).take(1) == ":" => str.drop(2)
      case str => str
    }
    def name: String = p.toFile.getName
    def relpath: Path = cwd.relativize(p).normalize
    def files: Seq[File] = p.toFile.files
    def parent: String = norm.replaceAll("/[^/]*$", "")

    // convert to absolute path relative to cwd.
    def absPath: String = if (p.isAbsolute) p.norm else Paths.get(userDir, p.norm).norm

    def isDir: Boolean = Files.isDirectory(p)
    def isDirectory: Boolean = p.toFile.isDirectory
    def isFile: Boolean = p.toFile.isFile

    def toUrl: String = Paths.get(absPath).toUri.toURL.toString

    // Used to be an extension on String
    // Treat norm paths with a leading '/' as absolute (Windows java.io.File#isAbsolute treats them as relative)
    //@annotation.nowarn // hidden by Path#isAbsolute
    //def isAbsolute = p.norm.startsWith("/") || (isWin && p.norm.secondChar == ":")
  }

  extension(f: File) {
    def name = f.getName
    def norm: String = f.toPath.normalize.norm
    def absPath: String = f.getAbsolutePath.norm
    def relpath: Path = f.toPath.relpath
    def files: Seq[File] = f.listFiles.toList
    def parentDir: Path = f.toPath.getParent
  }

  lazy val cwd: Path = Paths.get(".").toAbsolutePath.normalize

  lazy val (scalacPath: String, scalaPath: String) = {
    val scalac = s"$workingDirectory/dist/target/pack/bin/scalac".toPath.normalize
    val scala = s"$workingDirectory/dist/target/pack/bin/scala".toPath.normalize
    (scalac.norm, scala.norm)
  }


  // use optional TEST_BASH if defined, otherwise, bash must be in PATH

  // envScalaHome is:
  //    dist/target/pack, if present
  //    else, SCALA_HOME if defined
  //    else, not defined
  lazy val envScalaHome =
    printf("scalacPath: %s\n", scalacPath.norm)
    if scalacPath.isFile then scalacPath.replaceAll("/bin/scalac", "")
    else envOrElse("SCALA_HOME", "not-found").norm

  lazy val javaParent: String = whichJava.parent(2)
  lazy val envJavaHome: String = envOrElse("JAVA_HOME", javaParent)
  lazy val cyghome = envOrElse("CYGWIN", "")
  lazy val msyshome = envOrElse("MSYS", "")

  // remove xtrace, if present, add :igncr: if not present
  lazy val shellopts: String = {
    val value: String = envOrElse("SHELLOPTS", "braceexpand:hashall:igncr:ignoreeof:monitor:vi")
    val list: List[String] = value.split(":").toList
    val minlist = list.filter {
      case "igncr" | "xtrace" => false
      case _ => true
    }
    if isWin then
      "igncr" :: minlist
    else
      minlist
  }.mkString(":")

  lazy val testEnvPairs = {
    val pairs = List(
      ("JAVA_HOME", envJavaHome),
      ("SCALA_HOME", envScalaHome),
      ("PATH", adjustedPath),
      ("CYGWIN", cyghome),
      ("MSYS", msyshome),
      ("SHELLOPTS", shellopts),
    ).filter { case (name, valu) => valu.nonEmpty }
    if verbose then for (k, v) <- pairs do printf("%s : %s\n", k ,v)
    pairs
  }

  // if unable to execute bash commands, this prevents invalid tests from failing
  lazy val passInvalidTests = envOrElse("PASS_INVALID_TESTS", "").nonEmpty

  def verifyValid(validTest: Boolean): Boolean =
      // !validTest implies unable to execute scripts via bash (e.g., permissions, or bash not found, etc.)
    if !validTest && !passInvalidTests then
      assert(validTest == true, s"unable to call script via bash -c")

    validTest
}
