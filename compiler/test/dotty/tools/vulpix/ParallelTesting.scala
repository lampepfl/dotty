package dotty
package tools
package vulpix

import scala.language.unsafeNulls

import java.io.{File => JFile, IOException, PrintStream, ByteArrayOutputStream}
import java.lang.System.{lineSeparator => EOL}
import java.net.URL
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, NoSuchFileException, Path, Paths}
import java.nio.charset.{Charset, StandardCharsets}
import java.text.SimpleDateFormat
import java.util.{HashMap, Timer, TimerTask}
import java.util.concurrent.{ExecutionException, TimeUnit, TimeoutException, Executors => JExecutors}

import scala.collection.mutable
import scala.io.{Codec, Source}
import scala.jdk.CollectionConverters.*
import scala.util.{Random, Try, Failure => TryFailure, Success => TrySuccess, Using}
import scala.util.control.NonFatal
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer

import dotc.{Compiler, Driver}
import dotc.core.Contexts.*
import dotc.decompiler
import dotc.report
import dotc.interfaces.Diagnostic.ERROR
import dotc.reporting.{Reporter, TestReporter}
import dotc.reporting.Diagnostic
import dotc.config.Config
import dotc.util.{DiffUtil, SourceFile, SourcePosition, Spans, NoSourcePosition}
import io.AbstractFile
import dotty.tools.vulpix.TestConfiguration.defaultOptions

/** A parallel testing suite whose goal is to integrate nicely with JUnit
 *
 *  This trait can be mixed in to offer parallel testing to compile runs. When
 *  using this, you should be running your JUnit tests **sequentially**, as the
 *  test suite itself runs with a high level of concurrency.
 */
trait ParallelTesting extends RunnerOrchestration { self =>
  import ParallelTesting._

  /** If the running environment supports an interactive terminal, each `Test`
   *  will be run with a progress bar and real time feedback
   */
  def isInteractive: Boolean

  /** A list of strings which is used to filter which tests to run, if `Nil` will run
   *  all tests. All absolute paths that contain any of the substrings in `testFilter`
   *  will be run
   */
  def testFilter: List[String]

  /** Tests should override the checkfiles with the current output */
  def updateCheckFiles: Boolean

  /** Contains a list of failed tests to run, if list is empty no tests will run */
  def failedTests: Option[List[String]]

  /** A test source whose files or directory of files is to be compiled
   *  in a specific way defined by the `Test`
   */
  sealed trait TestSource { self =>
    def name: String
    def outDir: JFile
    def flags: TestFlags
    def sourceFiles: Array[JFile]

    def runClassPath: String = outDir.getPath + JFile.pathSeparator + flags.runClassPath

    def title: String = self match {
      case self: JointCompilationSource =>
        if (self.files.length > 1) name
        else self.files.head.getPath

      case self: SeparateCompilationSource =>
        self.dir.getPath
    }

    /** Adds the flags specified in `newFlags0` if they do not already exist */
    def withFlags(newFlags0: String*) = {
      val newFlags = newFlags0.toArray
      if (!flags.options.containsSlice(newFlags)) self match {
        case self: JointCompilationSource =>
          self.copy(flags = flags.and(newFlags*))
        case self: SeparateCompilationSource =>
          self.copy(flags = flags.and(newFlags*))
      }
      else self
    }

    def withoutFlags(flags1: String*): TestSource = self match {
      case self: JointCompilationSource =>
        self.copy(flags = flags.without(flags1*))
      case self: SeparateCompilationSource =>
        self.copy(flags = flags.without(flags1*))
    }

    lazy val allToolArgs: ToolArgs =
      toolArgsFor(sourceFiles.toList.map(_.toPath), getCharsetFromEncodingOpt(flags))

    /** Generate the instructions to redo the test from the command line */
    def buildInstructions(errors: Int, warnings: Int): String = {
      val sb = new StringBuilder
      val maxLen = 80
      var lineLen = 0
      val delimiter = " "

      sb.append(
        s"""|
            |Test '$title' compiled with $errors error(s) and $warnings warning(s),
            |the test can be reproduced by running from SBT (prefix it with ./bin/ if you
            |want to run from the command line):""".stripMargin
      )
      sb.append("\n\nscalac ")
      flags.all.foreach { arg =>
        if (lineLen > maxLen) {
          sb.append(delimiter)
          lineLen = 4
        }
        sb.append(arg)
        lineLen += arg.length
        sb += ' '
      }

      self match {
        case source: JointCompilationSource => {
          source.sourceFiles.map(_.getPath).foreach { path =>
            sb.append(delimiter)
            sb += '\''
            sb.append(path)
            sb += '\''
            sb += ' '
          }
          sb.toString + "\n\n"
        }
        case self: SeparateCompilationSource => { // TODO: this won't work when using other versions of compiler
          val command = sb.toString
          val fsb = new StringBuilder(command)
          self.compilationGroups.foreach { (_, files) =>
            files.map(_.getPath).foreach { path =>
              fsb.append(delimiter)
              lineLen = 8
              fsb += '\''
              fsb.append(path)
              fsb += '\''
              fsb += ' '
            }
            fsb.append("\n\n")
            fsb.append(command)
          }
          fsb.toString + "\n\n"
        }
      }
    }
  }

  private sealed trait FromTastyCompilationMode
  private case object NotFromTasty extends FromTastyCompilationMode
  private case object FromTasty extends FromTastyCompilationMode
  private case object FromBestEffortTasty extends FromTastyCompilationMode
  private case class WithBestEffortTasty(bestEffortDir: JFile) extends FromTastyCompilationMode

  /** A group of files that may all be compiled together, with the same flags
   *  and output directory
   */
  private case class JointCompilationSource(
    name: String,
    files: Array[JFile],
    flags: TestFlags,
    outDir: JFile,
    fromTasty: FromTastyCompilationMode = NotFromTasty,
    decompilation: Boolean = false
  ) extends TestSource {
    def sourceFiles: Array[JFile] = files.filter(isSourceFile)

    override def toString() = sourceFiles match { case Array(f) => f.getPath case _ => outDir.getPath }
  }

  /** A test source whose files will be compiled separately according to their
   *  suffix `_X`
   */
  case class SeparateCompilationSource(
    name: String,
    dir: JFile,
    flags: TestFlags,
    outDir: JFile
  ) extends TestSource {
    case class Group(ordinal: Int, compiler: String)

    lazy val compilationGroups: List[(Group, Array[JFile])] =
      val Compiler = """c([\d\.]+)""".r
      val Ordinal = """(\d+)""".r
      def groupFor(file: JFile): Group =
        val groupSuffix = file.getName.dropWhile(_ != '_').stripSuffix(".scala").stripSuffix(".java")
        val groupSuffixParts = groupSuffix.split("_")
        val ordinal = groupSuffixParts.collectFirst { case Ordinal(n) => n.toInt }.getOrElse(Int.MinValue)
        val compiler = groupSuffixParts.collectFirst { case Compiler(c) => c }.getOrElse("")
        Group(ordinal, compiler)

      dir.listFiles
        .filter(isSourceFile)
        .groupBy(groupFor)
        .toList
        .sortBy { (g, _) => (g.ordinal, g.compiler) }
        .map { (g, f) => (g, f.sorted) }

    def sourceFiles = compilationGroups.map(_._2).flatten.toArray
  }

  protected def shouldSkipTestSource(testSource: TestSource): Boolean = false

  protected def shouldReRun(testSource: TestSource): Boolean =
    failedTests.forall(rerun => testSource match {
      case JointCompilationSource(_, files, _, _, _, _) =>
        rerun.exists(filter => files.exists(file => file.getPath.contains(filter)))
      case SeparateCompilationSource(_, dir, _, _) =>
        rerun.exists(dir.getPath.contains)
    })

  private trait CompilationLogic { this: Test =>
    def suppressErrors = false

    /**
     * Compiles the test source.
     * @return The reporters containing the results of all the compilation runs for this test source.
     */
    private final def compileTestSource(testSource: TestSource): Try[List[TestReporter]] =
      Try(testSource match {
        case testSource @ JointCompilationSource(name, files, flags, outDir, fromTasty, decompilation) =>
          val reporter = fromTasty match
            case NotFromTasty => compile(testSource.sourceFiles, flags, outDir)
            case FromTasty => compileFromTasty(flags, outDir)
            case FromBestEffortTasty => compileFromBestEffortTasty(flags, outDir)
            case WithBestEffortTasty(bestEffortDir) => compileWithBestEffortTasty(testSource.sourceFiles, bestEffortDir, flags, outDir)
          List(reporter)

        case testSource @ SeparateCompilationSource(_, dir, flags, outDir) =>
          testSource.compilationGroups.map { (group, files) =>
            if group.compiler.isEmpty then
              compile(files, flags, outDir)
            else
              compileWithOtherCompiler(group.compiler, files, flags, outDir)
          }
      })

    final def countErrorsAndWarnings(reporters: Seq[TestReporter]): (Int, Int) =
      reporters.foldLeft((0, 0)) { case ((err, warn), r) => (err + r.errorCount, warn + r.warningCount) }

    final def countErrors  (reporters: Seq[TestReporter]) = countErrorsAndWarnings(reporters)._1
    final def countWarnings(reporters: Seq[TestReporter]) = countErrorsAndWarnings(reporters)._2
    final def reporterFailed(r: TestReporter) = r.errorCount > 0

    /**
     * For a given test source, returns a check file against which the result of the test run
     * should be compared. Is used by implementations of this trait.
     */
    final def checkFile(testSource: TestSource): Option[JFile] = (testSource match {
      case ts: JointCompilationSource =>
        ts.files.collectFirst {
          case f if !f.isDirectory =>
            new JFile(f.getPath.replaceFirst("\\.(scala|java)$", ".check"))
        }
      case ts: SeparateCompilationSource =>
        Option(new JFile(ts.dir.getPath + ".check"))
    }).filter(_.exists)

    /**
     * Checks if the given actual lines are the same as the ones in the check file.
     * If not, fails the test.
     */
    final def diffTest(testSource: TestSource, checkFile: JFile, actual: List[String], reporters: Seq[TestReporter], logger: LoggedRunnable) = {
      for (msg <- FileDiff.check(testSource.title, actual, checkFile.getPath)) {
        onFailure(testSource, reporters, logger, Some(msg))

        if (updateCheckFiles) {
          FileDiff.dump(checkFile.toPath.toString, actual)
          echo("Updated checkfile: " + checkFile.getPath)
        } else {
          val outFile = checkFile.toPath.resolveSibling(s"${checkFile.toPath.getFileName}.out").toString
          FileDiff.dump(outFile, actual)
          echo(FileDiff.diffMessage(checkFile.getPath, outFile))
        }
      }
    }

    /** Entry point: runs the test */
    final def encapsulatedCompilation(testSource: TestSource) = new LoggedRunnable { self =>
      def checkTestSource(): Unit = tryCompile(testSource) {
        val reportersOrCrash = compileTestSource(testSource)
        onComplete(testSource, reportersOrCrash, self)
        registerCompletion()
      }
    }

    /** This callback is executed once the compilation of this test source finished */
    private final def onComplete(testSource: TestSource, reportersOrCrash: Try[Seq[TestReporter]], logger: LoggedRunnable): Unit =
      try
        reportersOrCrash match
          case TryFailure(exn) => onFailure(testSource, Nil, logger, Some(s"Fatal compiler crash when compiling: ${testSource.title}:\n${exn.getMessage}${exn.getStackTrace.map("\n\tat " + _).mkString}"))
          case TrySuccess(reporters) if !reporters.exists(_.skipped) =>
            maybeFailureMessage(testSource, reporters) match {
              case Some(msg) => onFailure(testSource, reporters, logger, Option(msg).filter(_.nonEmpty))
              case None => onSuccess(testSource, reporters, logger)
            }
          case _ =>
      catch case ex: Throwable =>
        echo(s"Exception thrown onComplete (probably by a reporter) in $testSource: ${ex.getClass}")
        Try(ex.printStackTrace())
          .recover{ _ =>
            val trace = ex.getStackTrace.map(_.toString) // compute this first in case getStackTrace throws an exception
            echo(s"${ex.getClass.getName} message could not be printed due to an exception while computing the message.")
            if trace.nonEmpty then trace.foreach(echo) else echo(s"${ex.getClass.getName} stack trace is empty.")
          }
          .getOrElse(echo(s"${ex.getClass.getName} stack trace could not be printed due to an exception while printing the stack trace."))
        failTestSource(testSource)

    /**
     * Based on the reporters obtained after the compilation, determines if this test has failed.
     * If it has, returns a Some with an error message. Otherwise, returns None.
     * As the conditions of failure are different for different test types, this method should be
     * overridden by the concrete implementations of this trait.
     */
    def maybeFailureMessage(testSource: TestSource, reporters: Seq[TestReporter]): Option[String] =
      if (reporters.exists(reporterFailed)) Some(s"Compilation failed for: '${testSource.title}'")
      else None

    /**
     * If the test has compiled successfully, this callback will be called. You can still fail the test from this callback.
     */
    def onSuccess(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable): Unit = ()

    /**
     * If the test failed to compile or the compiler crashed, this callback will be called.
     */
    def onFailure(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable, message: Option[String]): Unit = {
      message.foreach(echo)
      reporters.filter(reporterFailed).foreach(logger.logReporterContents)
      logBuildInstructions(testSource, reporters)
      failTestSource(testSource)
    }
  }

  /** Each `Test` takes the `testSources` and performs the compilation and assertions
   *  according to the implementing class "neg", "run" or "pos".
   */
  private class Test(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit val summaryReport: SummaryReporting) extends CompilationLogic { test =>

    import summaryReport._

    protected final val realStdout = System.out
    protected final val realStderr = System.err

    /** A runnable that logs its contents in a buffer */
    trait LoggedRunnable extends Runnable {
      /** Instances of `LoggedRunnable` implement this method instead of the
       *  `run` method
       */
      def checkTestSource(): Unit

      private val logBuffer = mutable.ArrayBuffer.empty[String]
      def log(msg: String): Unit = logBuffer.append(msg)

      def logReporterContents(reporter: TestReporter): Unit =
        reporter.messages.foreach(log)

      def echo(msg: String): Unit = {
        log(msg)
        test.echo(msg)
      }

      final def run(): Unit = {
        checkTestSource()
        summaryReport.echoToLog(logBuffer.iterator)
      }
    }

    /** All testSources left after filtering out */
    private val filteredSources =
      val filteredByName =
        if (testFilter.isEmpty) testSources
        else testSources.filter {
          case JointCompilationSource(_, files, _, _, _, _) =>
            testFilter.exists(filter => files.exists(file => file.getPath.contains(filter)))
          case SeparateCompilationSource(_, dir, _, _) =>
            testFilter.exists(dir.getPath.contains)
        }
      filteredByName.filterNot(shouldSkipTestSource(_)).filter(shouldReRun(_))

    /** Total amount of test sources being compiled by this test */
    val sourceCount = filteredSources.length

    private var _testSourcesCompleted = 0
    private def testSourcesCompleted: Int = _testSourcesCompleted

    /** Complete the current compilation with the amount of errors encountered */
    protected final def registerCompletion() = synchronized {
      _testSourcesCompleted += 1
    }

    sealed trait Failure
    case class JavaCompilationFailure(reason: String) extends Failure
    case class TimeoutFailure(title: String) extends Failure
    case object Generic extends Failure

    private var _failures = Set.empty[Failure]
    private var _failureCount = 0

    /** Fail the current test */
    protected final def fail(failure: Failure = Generic): Unit = synchronized {
      _failures = _failures + failure
      _failureCount = _failureCount + 1
    }
    def didFail: Boolean = _failureCount != 0

    /** A set of the different failures */
    def failureReasons: Set[Failure] = _failures

    /** Number of failed tests */
    def failureCount: Int = _failureCount

    private var _skipCount = 0
    protected final def registerSkip(): Unit = synchronized { _skipCount += 1 }
    def skipCount: Int = _skipCount

    protected def logBuildInstructions(testSource: TestSource, reporters: Seq[TestReporter]) = {
      val (errCount, warnCount) = countErrorsAndWarnings(reporters)
      val errorMsg = testSource.buildInstructions(errCount, warnCount)
      addFailureInstruction(errorMsg)
    }

    /** Instructions on how to reproduce failed test source compilations */
    private val reproduceInstructions = mutable.ArrayBuffer.empty[String]
    protected final def addFailureInstruction(ins: String): Unit =
      synchronized { reproduceInstructions.append(ins) }

    /** The test sources that failed according to the implementing subclass */
    private val failedTestSources = mutable.ArrayBuffer.empty[FailedTestInfo]
    protected final def failTestSource(testSource: TestSource, reason: Failure = Generic) = synchronized {
      val extra = reason match {
        case TimeoutFailure(title) => s", test '$title' timed out"
        case JavaCompilationFailure(msg) => s", java test sources failed to compile with: \n$msg"
        case Generic => ""
      }
      failedTestSources.append(FailedTestInfo(testSource.title, s" failed" + extra))
      fail(reason)
    }

    /** Prints to `System.err` if we're not suppressing all output */
    protected def echo(msg: String): Unit = if (!suppressAllOutput) {
      // pad right so that output is at least as large as progress bar line
      val paddingRight = " " * math.max(0, 80 - msg.length)
      realStderr.println(msg + paddingRight)
    }

    /** Print a progress bar for the current `Test` */
    private def updateProgressMonitor(start: Long): Unit =
      if testSourcesCompleted < sourceCount then
        realStdout.print(s"\r${makeProgressBar(start)}")

    private def finishProgressMonitor(start: Long): Unit =
      realStdout.println(s"\r${makeProgressBar(start)}")

    private def makeProgressBar(start: Long): String =
      val tCompiled = testSourcesCompleted
      val timestamp = (System.currentTimeMillis - start) / 1000
      val progress = (tCompiled.toDouble / sourceCount * 40).toInt
      val past = "=" * math.max(progress - 1, 0)
      val curr = if progress > 0 then ">" else ""
      val next = " " * (40 - progress)
      s"[$past$curr$next] completed ($tCompiled/$sourceCount, $failureCount failed, ${timestamp}s)"

    /** Wrapper function to make sure that the compiler itself did not crash -
     *  if it did, the test should automatically fail.
     */
    protected def tryCompile(testSource: TestSource)(op: => Unit): Unit =
      try op
      catch
        case e: Throwable =>
          // if an exception is thrown during compilation, the complete test
          // run should fail
          failTestSource(testSource)
          e.printStackTrace()
          registerCompletion()
          throw e

    protected def compile(files0: Array[JFile], flags0: TestFlags, targetDir: JFile): TestReporter = {
      import scala.util.Properties.*

      def flattenFiles(f: JFile): Array[JFile] =
        if (f.isDirectory) f.listFiles.flatMap(flattenFiles)
        else Array(f)

      val files: Array[JFile] = files0.flatMap(flattenFiles)

      val toolArgs = toolArgsFor(files.toList.map(_.toPath), getCharsetFromEncodingOpt(flags0))

      val spec = raw"(\d+)(\+)?".r
      val testIsFiltered = toolArgs.get(ToolName.Test) match
        case Some("-jvm" :: spec(n, more) :: Nil) =>
          if more == "+" then isJavaAtLeast(n) else javaSpecVersion == n
        case Some(args) => throw new IllegalStateException(args.mkString("unknown test option: ", ", ", ""))
        case None => true

      def scalacOptions = toolArgs.getOrElse(ToolName.Scalac, Nil)
      def javacOptions  = toolArgs.getOrElse(ToolName.Javac, Nil)

      val flags = flags0
        .and(scalacOptions*)
        .and("-d", targetDir.getPath)
        .withClasspath(targetDir.getPath)

      def waitForJudiciously(process: Process): Int =
        try process.waitFor()
        catch case _: InterruptedException =>
          try if process.waitFor(5L, TimeUnit.MINUTES) then process.exitValue() else -2
          finally Thread.currentThread.interrupt()

      def compileWithJavac(fs: Array[String]) = if (fs.nonEmpty) {
        val fullArgs = Array(
          "-encoding", StandardCharsets.UTF_8.name,
        ) ++ flags.javacFlags ++ javacOptions++ fs

        val process = Runtime.getRuntime.exec("javac" +: fullArgs)
        val output = Source.fromInputStream(process.getErrorStream).mkString

        if waitForJudiciously(process) != 0 then Some(output)
        else None
      } else None

      val reporter = mkReporter

      val driver =
        if (times == 1) new Driver
        else new Driver {
          private def ntimes(n: Int)(op: Int => Reporter): Reporter =
            (1 to n).foldLeft(emptyReporter) ((_, i) => op(i))

          override def doCompile(comp: Compiler, files: List[AbstractFile])(using Context) =
            ntimes(times) { run =>
              val start = System.nanoTime()
              val rep = super.doCompile(comp, files)
              report.echo(s"\ntime run $run: ${(System.nanoTime - start) / 1000000}ms")
              rep
            }
        }

      val allArgs = flags.all

      if testIsFiltered then
        // If a test contains a Java file that cannot be parsed by Dotty's Java source parser, its
        // name must contain the string "JAVA_ONLY".
        val dottyFiles = files.filterNot(_.getName.contains("JAVA_ONLY")).map(_.getPath)
        driver.process(allArgs ++ dottyFiles, reporter = reporter)

        // todo a better mechanism than ONLY. test: -scala-only?
        val javaFiles = files.filter(_.getName.endsWith(".java")).filterNot(_.getName.contains("SCALA_ONLY")).map(_.getPath)
        val javaErrors = compileWithJavac(javaFiles)

        if (javaErrors.isDefined) {
          echo(s"\njava compilation failed: \n${ javaErrors.get }")
          fail(failure = JavaCompilationFailure(javaErrors.get))
        }
      else
        registerSkip()
        reporter.setSkip()
      end if

      reporter
    }

    private def parseErrors(errorsText: String, compilerVersion: String, pageWidth: Int) =
      val errorPattern = """^.*Error: (.*\.scala):(\d+):(\d+).*""".r
      val brokenClassPattern = """^class file (.*) is broken.*""".r
      val warnPattern = """^.*Warning: (.*\.scala):(\d+):(\d+).*""".r
      val summaryPattern = """\d+ (?:warning|error)s? found""".r
      val indent = "    "
      var diagnostics = List.empty[Diagnostic.Error]
      def barLine(start: Boolean) = s"$indent${if start then "╭" else "╰"}${"┄" * pageWidth}${if start then "╮" else "╯"}\n"
      def errorLine(line: String) = s"$indent┆${String.format(s"%-${pageWidth}s", stripAnsi(line))}┆\n"
      def stripAnsi(str: String): String = str.replaceAll("\u001b\\[\\d+m", "")
      def addToLast(str: String): Unit =
        diagnostics match
          case head :: tail =>
            diagnostics = Diagnostic.Error(s"${head.msg.message}$str", head.pos) :: tail
          case Nil =>
      var inError = false
      for line <- errorsText.linesIterator do
        line match
          case error @ warnPattern(filePath, line, column) =>
            inError = false
          case error @ errorPattern(filePath, line, column) =>
            inError = true
            val lineNum = line.toInt
            val columnNum = column.toInt
            val abstractFile = AbstractFile.getFile(filePath)
            val sourceFile = SourceFile(abstractFile, Codec.UTF8)
            val offset = sourceFile.lineToOffset(lineNum - 1) + columnNum - 1
            val span = Spans.Span(offset)
            val sourcePos = SourcePosition(sourceFile, span)
            addToLast(barLine(start = false))
            diagnostics ::= Diagnostic.Error(s"Compilation of $filePath with Scala $compilerVersion failed at line: $line, column: $column.\nFull error output:\n${barLine(start = true)}${errorLine(error)}", sourcePos)
          case error @ brokenClassPattern(filePath) =>
            inError = true
            diagnostics ::= Diagnostic.Error(s"$error\nFull error output:\n${barLine(start = true)}${errorLine(error)}", NoSourcePosition)
          case summaryPattern() => // Ignored
          case line if inError => addToLast(errorLine(line))
          case _ =>
      addToLast(barLine(start = false))
      diagnostics.reverse

    protected def compileWithOtherCompiler(compiler: String, files: Array[JFile], flags: TestFlags, targetDir: JFile): TestReporter =
      def artifactClasspath(organizationName: String, moduleName: String) =
        import coursier._
        val dep = Dependency(
          Module(
            Organization(organizationName),
            ModuleName(moduleName),
            attributes = Map.empty
          ),
          version = compiler
        )
        Fetch()
          .addDependencies(dep)
          .run()
          .mkString(JFile.pathSeparator)

      val pageWidth = TestConfiguration.pageWidth - 20

      val fileArgs = files.map(_.getAbsolutePath)

      def scala2Command(): Array[String] = {
        assert(!flags.options.contains("-scalajs"),
          "Compilation tests with Scala.js on Scala 2 are not supported.\nThis test can be skipped using the `// scalajs: --skip` tag")
        val stdlibClasspath = artifactClasspath("org.scala-lang", "scala-library")
        val scalacClasspath = artifactClasspath("org.scala-lang", "scala-compiler")
        val flagsArgs = flags
          .copy(options = Array.empty, defaultClassPath = stdlibClasspath)
          .withClasspath(targetDir.getPath)
          .and("-d", targetDir.getPath)
          .all
        val scalacCommand = Array("java", "-cp", scalacClasspath, "scala.tools.nsc.Main")
        scalacCommand ++ flagsArgs ++ fileArgs
      }

      def scala3Command(): Array[String] = {
        val stdlibClasspath = artifactClasspath("org.scala-lang", "scala3-library_3")
        val scalacClasspath = artifactClasspath("org.scala-lang", "scala3-compiler_3")
        val flagsArgs = flags
          .copy(defaultClassPath = stdlibClasspath)
          .withClasspath(targetDir.getPath)
          .and("-d", targetDir.getPath)
          .and("-pagewidth", pageWidth.toString)
          .all
        val scalacCommand = Array("java", "-cp", scalacClasspath, "dotty.tools.dotc.Main")
        scalacCommand ++ flagsArgs ++ fileArgs
      }

      val command = if compiler.startsWith("2") then scala2Command() else scala3Command()
      val process = Runtime.getRuntime.exec(command)

      val reporter = mkReporter
      val errorsText = Source.fromInputStream(process.getErrorStream).mkString
      if process.waitFor() != 0 then
        val diagnostics = parseErrors(errorsText, compiler, pageWidth)
        diagnostics.foreach { diag =>
          val context = (new ContextBase).initialCtx
          reporter.report(diag)(using context)
        }

      reporter

    protected def compileFromBestEffortTasty(flags0: TestFlags, targetDir: JFile): TestReporter = {
      val classes = flattenFiles(targetDir).filter(isBestEffortTastyFile).map(_.toString)
      val flags = flags0 and "-from-tasty" and "-Ywith-best-effort-tasty"
      val reporter = mkReporter
      val driver = new Driver

      driver.process(flags.all ++ classes, reporter = reporter)

      reporter
    }

    protected def compileWithBestEffortTasty(files0: Array[JFile], bestEffortDir: JFile, flags0: TestFlags, targetDir: JFile): TestReporter = {
      val flags = flags0
        .and("-Ywith-best-effort-tasty")
        .and("-d", targetDir.getPath)
      val reporter = mkReporter
      val driver = new Driver

      val args = Array("-classpath", flags.defaultClassPath + JFile.pathSeparator + bestEffortDir.toString) ++ flags.options

      driver.process(args ++ files0.map(_.toString), reporter = reporter)

      reporter
    }

    protected def compileFromTasty(flags0: TestFlags, targetDir: JFile): TestReporter = {
      val tastyOutput = new JFile(targetDir.getPath + "_from-tasty")
      tastyOutput.mkdir()
      val flags = flags0 and ("-d", tastyOutput.getPath) and "-from-tasty"

      val classes = flattenFiles(targetDir).filter(isTastyFile).map(_.toString)

      val reporter = mkReporter

      val driver = new Driver

      driver.process(flags.all ++ classes, reporter = reporter)

      reporter
    }

    private def mkLogLevel = if suppressErrors || suppressAllOutput then ERROR + 1 else ERROR
    private def mkReporter = TestReporter.reporter(realStdout, logLevel = mkLogLevel)

    protected def diffCheckfile(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable) =
      checkFile(testSource).foreach(diffTest(testSource, _, reporterOutputLines(reporters), reporters, logger))

    private def reporterOutputLines(reporters: Seq[TestReporter]): List[String] =
      reporters.flatMap(_.consoleOutput.split("\n")).toList

    private[ParallelTesting] def executeTestSuite(): this.type = {
      assert(testSourcesCompleted == 0, "not allowed to re-use a `CompileRun`")
      if filteredSources.nonEmpty then
        val pool = JExecutors.newWorkStealingPool(threadLimit.getOrElse(Runtime.getRuntime.availableProcessors()))
        val timer = new Timer()
        val logProgress = isInteractive && !suppressAllOutput
        val start = System.currentTimeMillis()
        if logProgress then
          timer.schedule((() => updateProgressMonitor(start)): TimerTask, 100/*ms*/, 200/*ms*/)

        val eventualResults = for target <- filteredSources yield
          pool.submit(encapsulatedCompilation(target))

        pool.shutdown()

        if !pool.awaitTermination(20, TimeUnit.MINUTES) then
          val remaining = new ListBuffer[TestSource]
          for (src, res) <- filteredSources.lazyZip(eventualResults) do
            if !res.isDone then
              remaining += src

          pool.shutdownNow()
          System.setOut(realStdout)
          System.setErr(realStderr)
          throw new TimeoutException(s"Compiling targets timed out, remaining targets: ${remaining.mkString(", ")}")
        end if

        for fut <- eventualResults do
          try fut.get()
          catch
          case ee: ExecutionException if ee.getCause.isInstanceOf[InterruptedException] =>
            System.err.println("Interrupted (probably running after shutdown)")
            ee.printStackTrace()
          case ex: Exception =>
            System.err.println(ex.getMessage)
            ex.printStackTrace()

        if logProgress then
          timer.cancel()
          finishProgressMonitor(start)

        if didFail then
          reportFailed()
          failedTestSources.toSet.foreach(addFailedTest)
          reproduceInstructions.foreach(addReproduceInstruction)
        else reportPassed()
      else echo {
        testFilter match
          case _ :: _ => s"""No files matched "${testFilter.mkString(",")}" in test"""
          case _      => "No tests available under target - erroneous test?"
      }

      this
    }

    /** Returns all files in directory or the file if not a directory */
    private def flattenFiles(f: JFile): Array[JFile] =
      if (f.isDirectory) f.listFiles.flatMap(flattenFiles)
      else Array(f)
  }

  private final class PosTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput)

  private final class WarnTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput):
    override def suppressErrors = true
    override def onSuccess(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable): Unit =
      diffCheckfile(testSource, reporters, logger)

    override def maybeFailureMessage(testSource: TestSource, reporters: Seq[TestReporter]): Option[String] =
      lazy val (map, expCount) = getWarnMapAndExpectedCount(testSource.sourceFiles.toIndexedSeq)
      lazy val obtCount = reporters.foldLeft(0)(_ + _.warningCount)
      lazy val (expected, unexpected) = getMissingExpectedWarnings(map, reporters.iterator.flatMap(_.diagnostics))
      lazy val diagnostics = reporters.flatMap(_.diagnostics.toSeq.sortBy(_.pos.line).map(e => s" at ${e.pos.line + 1}: ${e.message}"))
      def showLines(title: String, lines: Seq[String]) = if lines.isEmpty then "" else title + lines.mkString("\n", "\n", "")
      def hasMissingAnnotations = expected.nonEmpty || unexpected.nonEmpty
      def showDiagnostics = showLines("-> following the diagnostics:", diagnostics)
      Option:
        if reporters.exists(_.errorCount > 0) then
          s"""Compilation failed for: ${testSource.title}
             |$showDiagnostics
             |""".stripMargin.trim.linesIterator.mkString("\n", "\n", "")
        else if expCount != obtCount then
          s"""|Wrong number of warnings encountered when compiling $testSource
              |expected: $expCount, actual: $obtCount
              |${showLines("Unfulfilled expectations:", expected)}
              |${showLines("Unexpected warnings:", unexpected)}
              |$showDiagnostics
              |""".stripMargin.trim.linesIterator.mkString("\n", "\n", "")
        else if hasMissingAnnotations then s"\nWarnings found on incorrect row numbers when compiling $testSource\n$showDiagnostics"
        else if !map.isEmpty then s"\nExpected warnings(s) have {<warning position>=<unreported warning>}: $map"
        else null
    end maybeFailureMessage

    def getWarnMapAndExpectedCount(files: Seq[JFile]): (HashMap[String, Integer], Int) =
      val comment = raw"//( *)(nopos-)?warn".r
      val map = new HashMap[String, Integer]()
      var count = 0
      def bump(key: String): Unit =
        map.get(key) match
          case null => map.put(key, 1)
          case n    => map.put(key, n+1)
        count += 1
      files.filter(isSourceFile).foreach { file =>
        Using(Source.fromFile(file, StandardCharsets.UTF_8.name)) { source =>
          source.getLines.zipWithIndex.foreach { case (line, lineNbr) =>
            comment.findAllMatchIn(line).foreach { m =>
              m.group(2) match
                case "nopos-" =>
                  bump("nopos")
                case _ => bump(s"${file.getPath}:${lineNbr+1}")
            }
          }
        }.get
      }
      (map, count)

    def getMissingExpectedWarnings(map: HashMap[String, Integer], reporterWarnings: Iterator[Diagnostic]): (List[String], List[String]) =
      val unexpected, unpositioned = ListBuffer.empty[String]
      def relativize(path: String): String = path.split(JFile.separatorChar).dropWhile(_ != "tests").mkString(JFile.separator)
      def seenAt(key: String): Boolean =
        map.get(key) match
          case null => false
          case 1 => map.remove(key) ; true
          case n => map.put(key, n - 1) ; true
      def sawDiagnostic(d: Diagnostic): Unit =
        val srcpos = d.pos.nonInlined
        if srcpos.exists then
          val key = s"${relativize(srcpos.source.file.toString())}:${srcpos.line + 1}"
          if !seenAt(key) then unexpected += key
        else
          if(!seenAt("nopos")) unpositioned += relativize(srcpos.source.file.toString())

      reporterWarnings.foreach(sawDiagnostic)

      (map.asScala.keys.toList, (unexpected ++ unpositioned).toList)
    end getMissingExpectedWarnings

  private final class RewriteTest(testSources: List[TestSource], checkFiles: Map[JFile, JFile], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput) {
    private def verifyOutput(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable) = {
      testSource.sourceFiles.foreach { file =>
        if checkFiles.contains(file) then
          val checkFile = checkFiles(file)
          val actual = {
            val source = Source.fromFile(file, StandardCharsets.UTF_8.name)
            try source.getLines().toList
            finally source.close()
          }
          diffTest(testSource, checkFile, actual, reporters, logger)
      }

      // check that the rewritten code compiles
      new CompilationTest(testSource).checkCompile()
    }

    override def onSuccess(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable) =
      verifyOutput(testSource, reporters, logger)
  }

  private final class RunTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput) {
    private var didAddNoRunWarning = false
    private def addNoRunWarning() = if (!didAddNoRunWarning) {
      didAddNoRunWarning = true
      summaryReport.addStartingMessage {
        """|WARNING
           |-------
           |Run tests were only compiled, not run - this is due to the `dotty.tests.norun`
           |property being set
           |""".stripMargin
      }
    }

    private def verifyOutput(checkFile: Option[JFile], dir: JFile, testSource: TestSource, warnings: Int, reporters: Seq[TestReporter], logger: LoggedRunnable) = {
      if Properties.testsNoRun then addNoRunWarning()
      else runMain(testSource.runClassPath, testSource.allToolArgs) match {
        case Success(output) => checkFile match {
          case Some(file) if file.exists => diffTest(testSource, file, output.linesIterator.toList, reporters, logger)
          case _ =>
        }
        case Failure(output) =>
          if output == "" then
            echo(s"Test '${testSource.title}' failed with no output")
          else
            echo(s"Test '${testSource.title}' failed with output:")
            echo(output)
          failTestSource(testSource)
        case Timeout =>
          echo("failed because test " + testSource.title + " timed out")
          failTestSource(testSource, TimeoutFailure(testSource.title))
      }
    }

    override def onSuccess(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable) =
      verifyOutput(checkFile(testSource), testSource.outDir, testSource, countWarnings(reporters), reporters, logger)
  }

  private final class NegTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput) {
    override def suppressErrors = true

    override def maybeFailureMessage(testSource: TestSource, reporters: Seq[TestReporter]): Option[String] =
      lazy val (errorMap, expectedErrors) = getErrorMapAndExpectedCount(testSource.sourceFiles.toIndexedSeq)
      lazy val actualErrors = reporters.foldLeft(0)(_ + _.errorCount)
      lazy val (expected, unexpected) = getMissingExpectedErrors(errorMap, reporters.iterator.flatMap(_.errors))
      def hasMissingAnnotations = expected.nonEmpty || unexpected.nonEmpty
      def showErrors = "-> following the errors:\n" +
        reporters.flatMap(_.allErrors.sortBy(_.pos.line).map(e => s"${e.pos.line + 1}: ${e.message}")).mkString(" at ", "\n at ", "")

      Option {
        if actualErrors == 0 then s"\nNo errors found when compiling neg test $testSource"
        else if expectedErrors == 0 then s"\nNo errors expected/defined in $testSource -- use // error or // nopos-error"
        else if expectedErrors != actualErrors then
          s"""|Wrong number of errors encountered when compiling $testSource
              |expected: $expectedErrors, actual: $actualErrors
              |${expected.mkString("Unfulfilled expectations:\n", "\n", "")}
              |${unexpected.mkString("Unexpected errors:\n", "\n", "")}
              |$showErrors
              |""".stripMargin.trim.linesIterator.mkString("\n", "\n", "")
        else if hasMissingAnnotations then s"\nErrors found on incorrect row numbers when compiling $testSource\n$showErrors"
        else if !errorMap.isEmpty then s"\nExpected error(s) have {<error position>=<unreported error>}: $errorMap"
        else null
      }
    end maybeFailureMessage

    override def onSuccess(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable): Unit =
      diffCheckfile(testSource, reporters, logger)

    // In neg-tests we allow two or three types of error annotations.
    // Normally, `// error` must be annotated on the correct line number.
    // `// nopos-error` allows for an error reported without a position.
    // `// anypos-error` allows for an error reported with a position that can't be annotated in the check file.
    //
    // We collect these in a map `"file:row" -> numberOfErrors`, for
    // nopos and anypos errors we save them in `"file" -> numberOfNoPosErrors`
    def getErrorMapAndExpectedCount(files: Seq[JFile]): (HashMap[String, Integer], Int) =
      val comment = raw"//( *)(nopos-|anypos-)?error".r
      val errorMap = new HashMap[String, Integer]()
      var expectedErrors = 0
      def bump(key: String): Unit =
        errorMap.get(key) match
          case null => errorMap.put(key, 1)
          case n => errorMap.put(key, n+1)
        expectedErrors += 1
      files.filter(isSourceFile).foreach { file =>
        Using(Source.fromFile(file, StandardCharsets.UTF_8.name)) { source =>
          source.getLines.zipWithIndex.foreach { case (line, lineNbr) =>
            comment.findAllMatchIn(line).foreach { m =>
              m.group(2) match
                case prefix if m.group(1).isEmpty =>
                  val what = Option(prefix).getOrElse("")
                  echo(s"Warning: ${file.getCanonicalPath}:${lineNbr}: found `//${what}error` but expected `// ${what}error`, skipping comment")
                case "nopos-" => bump("nopos")
                case "anypos-" => bump("anypos")
                case _ => bump(s"${file.getPath}:${lineNbr+1}")
            }
          }
        }.get
      }
      (errorMap, expectedErrors)
    end getErrorMapAndExpectedCount

    // return unfulfilled expected errors and unexpected diagnostics
    def getMissingExpectedErrors(errorMap: HashMap[String, Integer], reporterErrors: Iterator[Diagnostic]): (List[String], List[String]) =
      val unexpected, unpositioned = ListBuffer.empty[String]
      // For some reason, absolute paths leak from the compiler itself...
      def relativize(path: String): String = path.split(JFile.separatorChar).dropWhile(_ != "tests").mkString(JFile.separator)
      def seenAt(key: String): Boolean =
        errorMap.get(key) match
          case null => false
          case 1 => errorMap.remove(key) ; true
          case n => errorMap.put(key, n - 1) ; true
      def sawDiagnostic(d: Diagnostic): Unit =
        d.pos.nonInlined match
          case srcpos if srcpos.exists =>
            val key = s"${relativize(srcpos.source.file.toString)}:${srcpos.line + 1}"
            if !seenAt(key) then unexpected += key
          case srcpos =>
            if !seenAt("nopos") then unpositioned += relativize(srcpos.source.file.toString)

      reporterErrors.foreach(sawDiagnostic)

      errorMap.get("anypos") match
        case n if n == unexpected.size => errorMap.remove("anypos") ; unexpected.clear()
        case _ =>

      (errorMap.asScala.keys.toList, (unexpected ++ unpositioned).toList)
    end getMissingExpectedErrors
  }

  private final class NoCrashTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput) {
    override def suppressErrors = true
    override def maybeFailureMessage(testSource: TestSource, reporters: Seq[TestReporter]): Option[String] = None
  }

  private final class NoBestEffortErrorsTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput) {
    override def suppressErrors = true
    override def maybeFailureMessage(testSource: TestSource, reporters: Seq[TestReporter]): Option[String] =
      val unsucceffulBestEffortErrorMsg = "Unsuccessful best-effort compilation."
      val failedBestEffortCompilation: Seq[TestReporter] =
        reporters.collect{
          case testReporter if testReporter.errors.exists(_.msg.message.startsWith(unsucceffulBestEffortErrorMsg)) =>
            testReporter
        }
      if !failedBestEffortCompilation.isEmpty then
        Some(failedBestEffortCompilation.flatMap(_.consoleOutput.split("\n")).mkString("\n"))
      else
        None
  }


  /** The `CompilationTest` is the main interface to `ParallelTesting`, it
   *  can be instantiated via one of the following methods:
   *
   *  - `compileFile`
   *  - `compileDir`
   *  - `compileList`
   *  - `compileFilesInDir`
   *  - `compileShallowFilesInDir`
   *
   *  Each compilation test can then be turned into either a "pos", "neg" or
   *  "run" test:
   *
   *  ```
   *  compileFile("tests/pos/i1103.scala", opts).pos()
   *  ```
   *
   *  These tests can be customized before calling one of the execution
   *  methods, for instance:
   *
   *  ```
   *  compileFile("tests/pos/i1103.scala", opts).times(2).verbose.pos()
   *  ```
   *
   *  Which would compile `i1103.scala` twice with the verbose flag as a "pos"
   *  test.
   *
   *  pos tests
   *  =========
   *  Pos tests verify that the compiler is able to compile the given
   *  `TestSource`s and that they generate no errors or exceptions during
   *  compilation
   *
   *  neg tests
   *  =========
   *  Neg tests are expected to generate a certain amount of errors - but not
   *  crash the compiler. In each `.scala` file, you specify the line on which
   *  the error will be generated, e.g:
   *
   *  ```
   *  val x: String = 1 // error
   *  ```
   *
   *  if a line generates multiple errors, you need to annotate it multiple
   *  times. For a line that generates two errors:
   *
   *  ```
   *  val y: String = { val y1: String = 1; 2 } // error // error
   *  ```
   *
   *  Certain errors have no position, if you need to check these annotate the
   *  file anywhere with `// nopos-error`
   *
   *  run tests
   *  =========
   *  Run tests are a superset of pos tests, they both verify compilation and
   *  that the compiler does not crash. In addition, run tests verify that the
   *  tests are able to run as expected.
   *
   *  Run tests need to have the following form:
   *
   *  ```
   *  object Test {
   *    def main(args: Array[String]): Unit = ()
   *  }
   *  ```
   *
   *  This is because the runner instantiates the `Test` class and calls the
   *  main method.
   *
   *  Other definitions are allowed in the same file, but the file needs to at
   *  least have the `Test` object with a `main` method.
   *
   *  To verify output you may use `.check` files. These files should share the
   *  name of the file or directory that they are testing. For instance:
   *
   *  ```none
   *  .
   *  └── tests
   *      ├── i1513.scala
   *      └── i1513.check
   *  ```
   *
   *  If you are testing a directory under separate compilation, you would
   *  have:
   *
   *  ```none
   *  .
   *  └── tests
   *      ├── myTestDir
   *      │   ├── T_1.scala
   *      │   ├── T_2.scala
   *      │   └── T_3.scala
   *      └── myTestDir.check
   *  ```
   *
   *  In the above example, `i1513.scala` and one of the files `T_X.scala`
   *  would contain a `Test` object with a main method.
   *
   *  Composing tests
   *  ===============
   *  Since this is a parallel test suite, it is essential to be able to
   *  compose tests to take advantage of the concurrency. This is done using
   *  `aggregateTests` in the companion, which will ensure that aggregation is allowed.
   */
  final class CompilationTest private (
    private[ParallelTesting] val targets: List[TestSource],
    private[ParallelTesting] val times: Int,
    private[ParallelTesting] val shouldDelete: Boolean,
    private[ParallelTesting] val threadLimit: Option[Int],
    private[ParallelTesting] val shouldFail: Boolean,
    private[ParallelTesting] val shouldSuppressOutput: Boolean
  ) {
    import org.junit.Assert.fail

    def this(target: TestSource) =
      this(List(target), 1, true, None, false, false)

    def this(targets: List[TestSource]) =
      this(targets, 1, true, None, false, false)

    def copy(targets: List[TestSource],
      times: Int = times,
      shouldDelete: Boolean = shouldDelete,
      threadLimit: Option[Int] = threadLimit,
      shouldFail: Boolean = shouldFail,
      shouldSuppressOutput: Boolean = shouldSuppressOutput): CompilationTest =
        CompilationTest(targets, times, shouldDelete, threadLimit, shouldFail, shouldSuppressOutput)

    /** Creates a "pos" test run, which makes sure that all tests pass
     *  compilation without generating errors and that they do not crash the
     *  compiler
     */
    def checkCompile()(implicit summaryReport: SummaryReporting): this.type =
      checkPass(new PosTest(targets, times, threadLimit, shouldFail || shouldSuppressOutput), "Pos")

    def checkWarnings()(implicit summaryReport: SummaryReporting): this.type =
      checkPass(new WarnTest(targets, times, threadLimit, shouldFail || shouldSuppressOutput), "Warn")

    /** Creates a "neg" test run, which makes sure that each test manages successful
     *  best-effort compilation, without any errors related to pickling/unpickling
     *  of betasty files.
     */
    def checkNoBestEffortError()(implicit summaryReport: SummaryReporting): this.type = {
      val test = new NoBestEffortErrorsTest(targets, times, threadLimit, shouldFail || shouldSuppressOutput).executeTestSuite()

      cleanup()

      if (test.didFail) {
        fail("Best-effort test should not have shown a \"Unsuccessful best-effort compilation\" error, but did")
      }

      this
    }

    /** Creates a "neg" test run, which makes sure that each test generates the
     *  correct number of errors at the correct positions. It also makes sure
     *  that none of these tests crashes the compiler.
     */
    def checkExpectedErrors()(implicit summaryReport: SummaryReporting): this.type =
      val test = new NegTest(targets, times, threadLimit, shouldSuppressOutput).executeTestSuite()

      cleanup()

      if shouldFail && !test.didFail && test.skipCount == 0 then
        fail(s"Neg test shouldn't have failed, but did. Reasons:\n${ reasonsForFailure(test) }")
      else if !shouldFail && test.didFail then
        fail("Neg test should have failed, but did not")

      this
    end checkExpectedErrors

    /** Creates a "fuzzy" test run, which makes sure that each test compiles (or not) without crashing */
    def checkNoCrash()(implicit summaryReport: SummaryReporting): this.type =
      checkFail(new NoCrashTest(targets, times, threadLimit, shouldSuppressOutput), "Fuzzy")

    /** Creates a "run" test run, which is a superset of "pos". In addition to
     *  making sure that all tests pass compilation and that they do not crash
     *  the compiler; it also makes sure that all tests can run with the
     *  expected output
     */
    def checkRuns()(implicit summaryReport: SummaryReporting): this.type =
      checkPass(new RunTest(targets, times, threadLimit, shouldFail || shouldSuppressOutput), "Run")

    /** Tests `-rewrite`, which makes sure that the rewritten files still compile
     *  and agree with the expected result (if specified).
     *
     *  Check files are only supported for joint compilation sources.
     */
    def checkRewrites()(implicit summaryReport: SummaryReporting): this.type = {
      // use the original check file, to simplify update of check files
      var checkFileMap = Map.empty[JFile, JFile]

      // copy source file to targets, as they will be changed
      val copiedTargets = targets.map {
        case target @ JointCompilationSource(_, files, _, outDir, _, _) =>
          val files2 = files.map { f =>
            val dest = copyToDir(outDir, f)
            val checkFile = new JFile(f.getPath.replaceFirst("\\.scala$", ".check"))
            if (checkFile.exists) checkFileMap = checkFileMap.updated(dest, checkFile)
            dest
          }
          target.copy(files = files2)
        case target @ SeparateCompilationSource(_, dir, _, outDir) =>
          target.copy(dir = copyToDir(outDir, dir))
      }

      val test = new RewriteTest(copiedTargets, checkFileMap, times, threadLimit, shouldFail || shouldSuppressOutput)

      checkFail(test, "Rewrite")
    }

    private def checkPass(test: Test, desc: String): this.type =
      test.executeTestSuite()

      cleanup()

      if !shouldFail && test.didFail then
        fail(s"$desc test failed, but should not, reasons:\n${reasonsForFailure(test)}")
      else if shouldFail && !test.didFail && test.skipCount == 0 then
        fail(s"$desc test should have failed, but didn't")

      this

    private def checkFail(test: Test, desc: String): this.type =
      test.executeTestSuite()

      cleanup()

      if shouldFail && !test.didFail && test.skipCount == 0 then
        fail(s"$desc test shouldn't have failed, but did. Reasons:\n${reasonsForFailure(test)}")
      else if !shouldFail && test.didFail then
        fail(s"$desc test failed")

      this

    /** Deletes output directories and files */
    private def cleanup(): this.type = {
      if (shouldDelete) delete()
      this
    }

    /** Extract `Failure` set and render from `Test` */
    private def reasonsForFailure(test: Test): String = {
      val failureReport =
        if test.failureCount == 0 then ""
        else s"encountered ${test.failureCount} test failure(s):\n"

      failureReport + test.failureReasons.collect {
        case test.TimeoutFailure(title) =>
          s"  - test '$title' timed out"
        case test.JavaCompilationFailure(msg) =>
          s"  - java compilation failed with:\n${ msg.linesIterator.map("      " + _).mkString("\n") }"
        case test.Generic =>
           "  - generic failure (see test output)"
      }.mkString("\n")
    }

    /** Copies `file` to `dir` - taking into account if `file` is a directory,
     *  and if so copying recursively
     */
    private def copyToDir(dir: JFile, file: JFile): JFile = {
      val target = Paths.get(dir.getPath, file.getName)
      Files.copy(file.toPath, target, REPLACE_EXISTING)
      if (file.isDirectory) file.listFiles.map(copyToDir(target.toFile, _))
      target.toFile
    }

    /** Builds a `CompilationTest` which performs the compilation `i` times on
     *  each target
     */
    def times(i: Int): CompilationTest =
      new CompilationTest(targets, i, shouldDelete, threadLimit, shouldFail, shouldSuppressOutput)

    /** Builds a `Compilationtest` which passes the verbose flag and logs the
     *  classpath
     */
    def verbose: CompilationTest = new CompilationTest(
      targets.map(t => t.withFlags("-verbose", "-Ylog-classpath")),
      times, shouldDelete, threadLimit, shouldFail, shouldSuppressOutput
    )

    /** Builds a `CompilationTest` which keeps the generated output files
     *
     *  This is needed for tests like `tastyBootstrap` which relies on first
     *  compiling a certain part of the project and then compiling a second
     *  part which depends on the first
     */
    def keepOutput: CompilationTest =
      new CompilationTest(targets, times, false, threadLimit, shouldFail, shouldSuppressOutput)

    /** Builds a `CompilationTest` with a limited level of concurrency with
     *  maximum `i` threads
     */
    def limitThreads(i: Int): CompilationTest =
      new CompilationTest(targets, times, shouldDelete, Some(i), shouldFail, shouldSuppressOutput)

    /** Builds a `CompilationTest` where the executed test is expected to fail
     *
     *  This behaviour is mainly needed for the tests that test the test suite.
     */
    def expectFailure: CompilationTest =
      new CompilationTest(targets, times, shouldDelete, threadLimit, true, shouldSuppressOutput)

    /** Builds a `CompilationTest` where all output is suppressed
     *
     *  This behaviour is mainly needed for the tests that test the test suite.
     */
    def suppressAllOutput: CompilationTest =
      new CompilationTest(targets, times, shouldDelete, threadLimit, shouldFail, true)

    /** Delete all output files generated by this `CompilationTest` */
    def delete(): Unit = targets.foreach(t => delete(t.outDir))

    private def delete(file: JFile): Unit = {
      if (file.isDirectory) file.listFiles.foreach(delete)
      try Files.delete(file.toPath)
      catch {
        case _: NoSuchFileException => // already deleted, everything's fine
      }
    }
  }

  object CompilationTest:

    /** Compose test targets from `tests`
     *
     *  It does this, only if all the tests are mutally compatible.
     *  Otherwise it throws an `IllegalArgumentException`.
     *
     *  Grouping tests together like this allows us to take advantage of the
     *  concurrency offered by this test suite, as each call to an executing
     *  method (`pos()` / `checkExpectedErrors()`/ `run()`) will spin up a thread pool with the
     *  maximum allowed level of concurrency. Doing this for only a few targets
     *  does not yield any real benefit over sequential compilation.
     *
     *  As such, each `CompilationTest` should contain as many targets as
     *  possible.
     */
    def aggregateTests(tests: CompilationTest*): CompilationTest =
      assert(tests.nonEmpty)
      def aggregate(test1: CompilationTest, test2: CompilationTest) =
        require(test1.times == test2.times, "can't combine tests that are meant to be benchmark compiled")
        require(test1.shouldDelete == test2.shouldDelete, "can't combine tests that differ on deleting output")
        require(test1.shouldFail == test2.shouldFail, "can't combine tests that have different expectations on outcome")
        require(test1.shouldSuppressOutput == test2.shouldSuppressOutput, "can't combine tests that both suppress and don't suppress output")
        test1.copy(test1.targets ++ test2.targets) // what if thread limit differs? currently threads are limited on aggregate only
      tests.reduce(aggregate)
  end CompilationTest

  /** Create out directory for directory `d` */
  def createOutputDirsForDir(d: JFile, sourceDir: JFile, outDir: String): JFile = {
    val targetDir = new JFile(outDir + s"${sourceDir.getName}/${d.getName}")
    targetDir.mkdirs()
    targetDir
  }

  /** Create out directory for `file` */
  private def createOutputDirsForFile(file: JFile, sourceDir: JFile, outDir: String): JFile = {
    val uniqueSubdir = file.getName.substring(0, file.getName.lastIndexOf('.'))
    val targetDir = new JFile(outDir + s"${sourceDir.getName}${JFile.separatorChar}$uniqueSubdir")
    targetDir.mkdirs()
    targetDir
  }

  /** Make sure that directory string is as expected */
  private def checkRequirements(f: String, sourceDir: JFile, outDir: String): Unit = {
    require(sourceDir.isDirectory && sourceDir.exists, "passed non-directory to `compileFilesInDir`: " + sourceDir)
    require(outDir.last == JFile.separatorChar, "please specify an `outDir` with a trailing file separator")
  }

  /** Separates directories from files and returns them as `(dirs, files)` */
  private def compilationTargets(sourceDir: JFile, fileFilter: FileFilter = FileFilter.NoFilter): (List[JFile], List[JFile]) =
    sourceDir.listFiles.foldLeft((List.empty[JFile], List.empty[JFile])) { case ((dirs, files), f) =>
      if (!fileFilter.accept(f.getName)) (dirs, files)
      else if (f.isDirectory) (f :: dirs, files)
      else if (isSourceFile(f)) (dirs, f :: files)
      else (dirs, files)
    }

  /** Compiles a single file from the string path `f` using the supplied flags */
  def compileFile(f: String, flags: TestFlags)(implicit testGroup: TestGroup): CompilationTest = {
    val sourceFile = new JFile(f)
    val parent = sourceFile.getParentFile
    val outDir =
      defaultOutputDir + testGroup + JFile.separator +
      sourceFile.getName.substring(0, sourceFile.getName.lastIndexOf('.')) + JFile.separator

    require(
      sourceFile.exists && !sourceFile.isDirectory &&
      (parent ne null) && parent.exists && parent.isDirectory,
      s"Source file: $f, didn't exist"
    )

    val target = JointCompilationSource(
      testGroup.name,
      Array(sourceFile),
      flags,
      createOutputDirsForFile(sourceFile, parent, outDir)
    )
    new CompilationTest(target)
  }

  /** Compiles a directory `f` using the supplied `flags`. This method does
   *  deep compilation, that is - it compiles all files and subdirectories
   *  contained within the directory `f`.
   *
   *  By default, files are compiled in alphabetical order. An optional seed
   *  can be used for randomization.
   */
  def compileDir(f: String, flags: TestFlags, randomOrder: Option[Int] = None, recursive: Boolean = true)(implicit testGroup: TestGroup): CompilationTest = {
    val outDir = defaultOutputDir + testGroup + JFile.separator
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    def flatten(f: JFile): Array[JFile] =
      if (f.isDirectory) {
        val files = f.listFiles
        if (recursive) files.flatMap(flatten) else files
      }
      else Array(f)

    // Sort files either alphabetically or randomly using the provided seed:
    val sortedFiles = flatten(sourceDir).sorted
    val randomized  = randomOrder match {
      case None       => sortedFiles
      case Some(seed) => new Random(seed).shuffle(sortedFiles.toList).toArray
    }

    // Directories in which to compile all containing files with `flags`:
    val targetDir = new JFile(outDir + JFile.separator + sourceDir.getName + JFile.separator)
    targetDir.mkdirs()

    val target = JointCompilationSource(s"compiling '$f' in test '$testGroup'", randomized, flags, targetDir)
    new CompilationTest(target)
  }

  /** Compiles all `files` together as a single compilation run. It is given a
   *  `testName` since files can be in separate directories and or be otherwise
   *  dissociated
   */
  def compileList(testName: String, files: List[String], flags: TestFlags)(implicit testGroup: TestGroup): CompilationTest = {
    val outDir = defaultOutputDir + testGroup + JFile.separator + testName + JFile.separator

    // Directories in which to compile all containing files with `flags`:
    val targetDir = new JFile(outDir)
    targetDir.mkdirs()
    assert(targetDir.exists, s"couldn't create target directory: $targetDir")

    val target = JointCompilationSource(s"$testName from $testGroup", files.map(new JFile(_)).toArray, flags, targetDir)

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(target)
  }

  /** This function compiles the files and folders contained within directory
   *  `f` in a specific way.
   *
   *  - Each file is compiled separately as a single compilation run
   *  - Each directory is compiled as a `SeparateCompilationTarget`, in this
   *    target all files are grouped according to the file suffix `_X` where `X`
   *    is a number. These groups are then ordered in ascending order based on
   *    the value of `X` and each group is compiled one after the other.
   *
   *  For this function to work as expected, we use the same convention for
   *  directory layout as the old partest. That is:
   *
   *  - Single files can have an associated check-file with the same name (but
   *    with file extension `.check`)
   *  - Directories can have an associated check-file, where the check file has
   *    the same name as the directory (with the file extension `.check`)
   */
  def compileFilesInDir(f: String, flags: TestFlags, fileFilter: FileFilter = FileFilter.NoFilter)(implicit testGroup: TestGroup): CompilationTest = {
    val outDir = defaultOutputDir + testGroup + JFile.separator
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    val (dirs, files) = compilationTargets(sourceDir, fileFilter)

    val isPicklerTest = flags.options.contains("-Ytest-pickler")
    def picklerDirFilter(source: SeparateCompilationSource): Boolean = {
      // Pickler tests stop after pickler not producing class/tasty files. The second part of the compilation
      // will not be able to compile due to the missing artifacts from the first part.
      !isPicklerTest || source.compilationGroups.length == 1
    }
    val targets =
      files.map(f => JointCompilationSource(testGroup.name, Array(f), flags, createOutputDirsForFile(f, sourceDir, outDir))) ++
      dirs.map { dir => SeparateCompilationSource(testGroup.name, dir, flags, createOutputDirsForDir(dir, sourceDir, outDir)) }.filter(picklerDirFilter)

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(targets)
  }

  /** This function compiles the files and folders contained within directory
   *  `f` in a specific way. Once compiled, they are recompiled/run from tasty as sources.
   *
   *  - Each file is compiled separately as a single compilation run
   *  - Each directory is compiled as a `SeparateCompilationTarget`, in this
   *    target all files are grouped according to the file suffix `_X` where `X`
   *    is a number. These groups are then ordered in ascending order based on
   *    the value of `X` and each group is compiled one after the other.
   *
   *  For this function to work as expected, we use the same convention for
   *  directory layout as the old partest. That is:
   *
   *  - Single files can have an associated check-file with the same name (but
   *    with file extension `.check`)
   *  - Directories can have an associated check-file, where the check file has
   *    the same name as the directory (with the file extension `.check`)
   *
   *  Tests in the first part of the tuple must be executed before the second.
   *  Both testsRequires explicit delete().
   */
  def compileTastyInDir(f: String, flags0: TestFlags, fromTastyFilter: FileFilter)(
      implicit testGroup: TestGroup): TastyCompilationTest = {
    val outDir = defaultOutputDir + testGroup + JFile.separator
    val flags = flags0 and "-Yretain-trees"
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    val (dirs, files) = compilationTargets(sourceDir, fromTastyFilter)

    val filteredFiles = testFilter match
      case _ :: _ => files.filter(f => testFilter.exists(f.getPath.contains))
      case _      => Nil

    class JointCompilationSourceFromTasty(
       name: String,
       file: JFile,
       flags: TestFlags,
       outDir: JFile,
       fromTasty: Boolean = false,
    ) extends JointCompilationSource(name, Array(file), flags, outDir, if (fromTasty) FromTasty else NotFromTasty) {

      override def buildInstructions(errors: Int, warnings: Int): String = {
        val runOrPos = if (file.getPath.startsWith(s"tests${JFile.separator}run${JFile.separator}")) "run" else "pos"
        val listName = if (fromTasty) "from-tasty" else "decompilation"
        s"""|
            |Test '$title' compiled with $errors error(s) and $warnings warning(s),
            |the test can be reproduced by running:
            |
            |  sbt "testCompilation --from-tasty $file"
            |
            |This tests can be disabled by adding `${file.getName}` to `compiler${JFile.separator}test${JFile.separator}dotc${JFile.separator}$runOrPos-$listName.blacklist`
            |
            |""".stripMargin
      }

    }

    val targets = filteredFiles.map { f =>
      val classpath = createOutputDirsForFile(f, sourceDir, outDir)
      new JointCompilationSourceFromTasty(testGroup.name, f, flags.withClasspath(classpath.getPath), classpath, fromTasty = true)
    }
    // TODO add SeparateCompilationSource from tasty?

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    val generateClassFiles = compileFilesInDir(f, flags0, fromTastyFilter)

    new TastyCompilationTest(
      generateClassFiles.keepOutput,
      new CompilationTest(targets).keepOutput,
      shouldDelete = true
    )
  }

  /** A two step compilation test for best effort compilation pickling and unpickling.
   *
   *  First, erroring neg test files are compiled with the `-Ybest-effort` option.
   *  If successful, then the produced Best Effort TASTy is re-compiled with
   *  '-Ywith-best-effort-tasty' to test the TastyReader for Best Effort TASTy.
   */
  def compileBestEffortTastyInDir(f: String, flags: TestFlags, picklingFilter: FileFilter, unpicklingFilter: FileFilter)(
      implicit testGroup: TestGroup): BestEffortCompilationTest = {
    val bestEffortFlag = "-Ybest-effort"
    val semanticDbFlag = "-Xsemanticdb"
    assert(!flags.options.contains(bestEffortFlag), "Best effort compilation flag should not be added manually")

    val outDir = defaultOutputDir + testGroup + JFile.separator
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    val (dirsStep1, filteredPicklingFiles) = compilationTargets(sourceDir, picklingFilter)
    val (dirsStep2, filteredUnpicklingFiles) = compilationTargets(sourceDir, unpicklingFilter)

    class BestEffortCompilation(
      name: String,
      file: JFile,
      flags: TestFlags,
      outputDir: JFile
    ) extends JointCompilationSource(name, Array(file), flags.and(bestEffortFlag).and(semanticDbFlag), outputDir) {
      override def buildInstructions(errors: Int, warnings: Int): String = {
        s"""|
            |Test '$title' compiled with a compiler crash,
            |the test can be reproduced by running:
            |
            |  sbt "scalac $bestEffortFlag $semanticDbFlag $file"
            |
            |These tests can be disabled by adding `${file.getName}` to `compiler${JFile.separator}test${JFile.separator}dotc${JFile.separator}neg-best-effort-pickling.blacklist`
            |""".stripMargin
      }
    }

    class CompilationFromBestEffortTasty(
       name: String,
       file: JFile,
       flags: TestFlags,
       bestEffortDir: JFile,
    ) extends JointCompilationSource(name, Array(file), flags, bestEffortDir, fromTasty = FromBestEffortTasty) {

      override def buildInstructions(errors: Int, warnings: Int): String = {
        def beTastyFiles(file: JFile): Array[JFile] =
          file.listFiles.flatMap { innerFile =>
            if (innerFile.isDirectory) beTastyFiles(innerFile)
            else if (isBestEffortTastyFile(innerFile)) Array(innerFile)
            else Array.empty[JFile]
          }
        val beTastyFilesString = beTastyFiles(bestEffortDir).mkString(" ")
        s"""|
            |Test '$title' compiled with a compiler crash,
            |the test can be reproduced by running:
            |
            |  sbt "scalac -Ybest-effort $file"
            |  sbt "scalac --from-tasty -Ywith-best-effort-tasty $beTastyFilesString"
            |
            |These tests can be disabled by adding `${file.getName}` to `compiler${JFile.separator}test${JFile.separator}dotc${JFile.separator}neg-best-effort-unpickling.blacklist`
            |
            |""".stripMargin
      }
    }

    val (bestEffortTargets, targetAndBestEffortDirs) =
      filteredPicklingFiles.map { f =>
        val outputDir = createOutputDirsForFile(f, sourceDir, outDir)
        val bestEffortDir = new JFile(outputDir, s"META-INF${JFile.separator}best-effort")
        (
          BestEffortCompilation(testGroup.name, f, flags, outputDir),
          (f, bestEffortDir)
        )
      }.unzip
    val (_, bestEffortDirs) = targetAndBestEffortDirs.unzip
    val fileToBestEffortDirMap = targetAndBestEffortDirs.toMap

    val picklingSet = filteredPicklingFiles.toSet
    val fromTastyTargets =
      filteredUnpicklingFiles.filter(picklingSet.contains(_)).map { f =>
        val bestEffortDir = fileToBestEffortDirMap(f)
        new CompilationFromBestEffortTasty(testGroup.name, f, flags, bestEffortDir)
      }

    new BestEffortCompilationTest(
      new CompilationTest(bestEffortTargets).keepOutput,
      new CompilationTest(fromTastyTargets).keepOutput,
      bestEffortDirs,
      shouldDelete = true
    )
  }

  /** A two step integration test for best effort compilation.
   *
   *  Directories found in the directory `f` represent separate tests and must contain
   *  the 'err' and 'main' directories. First the (erroring) contents of the 'err'
   *  directory are compiled with the `Ybest-effort` option.
   *  Then, are the contents of 'main' are compiled with the previous best effort directory
   *  on the classpath using the option `-Ywith-best-effort-tasty`.
   */
  def compileBestEffortIntegration(f: String, flags: TestFlags)(implicit testGroup: TestGroup) = {
    val bestEffortFlag = "-Ybest-effort"
    val semanticDbFlag = "-Xsemanticdb"
    val withBetastyFlag = "-Ywith-best-effort-tasty"
    val sourceDir = new JFile(f)
    val dirs = sourceDir.listFiles.toList
    assert(dirs.forall(_.isDirectory), s"All files in $f have to be directories.")

    val (step1Targets, step2Targets, bestEffortDirs) = dirs.map { dir =>
      val step1SourceDir = new JFile(dir, "err")
      val step2SourceDir = new JFile(dir, "main")

      val step1SourceFiles = step1SourceDir.listFiles
      val step2SourceFiles = step2SourceDir.listFiles

      val outDir = defaultOutputDir + testGroup + JFile.separator + dir.getName().toString + JFile.separator

      val step1OutDir = createOutputDirsForDir(step1SourceDir, step1SourceDir, outDir)
      val step2OutDir = createOutputDirsForDir(step2SourceDir, step2SourceDir, outDir)

      val step1Compilation = JointCompilationSource(
        testGroup.name, step1SourceFiles, flags.and(bestEffortFlag).and(semanticDbFlag), step1OutDir, fromTasty = NotFromTasty
      )

      val bestEffortDir = new JFile(step1OutDir, s"META-INF${JFile.separator}best-effort")

      val step2Compilation = JointCompilationSource(
        testGroup.name, step2SourceFiles, flags.and(withBetastyFlag).and(semanticDbFlag), step2OutDir, fromTasty = WithBestEffortTasty(bestEffortDir)
      )
      (step1Compilation, step2Compilation, bestEffortDir)
    }.unzip3

    BestEffortCompilationTest(
      new CompilationTest(step1Targets).keepOutput,
      new CompilationTest(step2Targets).keepOutput,
      bestEffortDirs,
      true
    )
  }


  class TastyCompilationTest(step1: CompilationTest, step2: CompilationTest, shouldDelete: Boolean)(implicit testGroup: TestGroup) {

    def keepOutput: TastyCompilationTest =
      new TastyCompilationTest(step1, step2, shouldDelete)

    def checkCompile()(implicit summaryReport: SummaryReporting): this.type = {
      step1.checkCompile() // Compile all files to generate the class files with tasty
      step2.checkCompile() // Compile from tasty

      if (shouldDelete)
        CompilationTest.aggregateTests(step1, step2).delete()

      this
    }

    def checkRuns()(implicit summaryReport: SummaryReporting): this.type = {
      step1.checkCompile() // Compile all files to generate the class files with tasty
      step2.checkRuns() // Compile from tasty

      if (shouldDelete)
        CompilationTest.aggregateTests(step1, step2).delete()

      this
    }
  }

  class BestEffortCompilationTest(step1: CompilationTest, step2: CompilationTest, bestEffortDirs: List[JFile], shouldDelete: Boolean)(implicit testGroup: TestGroup) {

    def checkNoCrash()(implicit summaryReport: SummaryReporting): this.type = {
      step1.checkNoBestEffortError() // Compile all files to generate the class files with best effort tasty
      step2.checkNoBestEffortError() // Compile with best effort tasty

      if (shouldDelete) {
        CompilationTest.aggregateTests(step1, step2).delete()
        def delete(file: JFile): Unit = {
          if (file.isDirectory) file.listFiles.foreach(delete)
          try Files.delete(file.toPath)
          catch {
            case _: NoSuchFileException => // already deleted, everything's fine
          }
        }
        bestEffortDirs.foreach(t => delete(t))
      }

      this
    }

    def noCrashWithCompilingDependencies()(implicit summaryReport: SummaryReporting): this.type = {
      step1.checkNoBestEffortError() // Compile all files to generate the class files with best effort tasty
      step2.checkCompile() // Compile with best effort tasty

      this
    }
  }

  /** This function behaves similar to `compileFilesInDir` but it ignores
   *  sub-directories and as such, does **not** perform separate compilation
   *  tests.
   */
  def compileShallowFilesInDir(f: String, flags: TestFlags)(implicit testGroup: TestGroup): CompilationTest = {
    val outDir = defaultOutputDir + testGroup + JFile.separator
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    val (_, files) = compilationTargets(sourceDir)

    val targets = files.map { file =>
      JointCompilationSource(testGroup.name, Array(file), flags, createOutputDirsForFile(file, sourceDir, outDir))
    }

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(targets)
  }

  private def getCharsetFromEncodingOpt(flags: TestFlags) =
    flags.options.sliding(2).collectFirst {
      case Array("-encoding", encoding) => Charset.forName(encoding)
    }.getOrElse(StandardCharsets.UTF_8)
}

object ParallelTesting {

  def defaultOutputDir: String = "out"+JFile.separator

  def isSourceFile(f: JFile): Boolean = {
    val name = f.getName
    name.endsWith(".scala") || name.endsWith(".java")
  }

  def isTastyFile(f: JFile): Boolean =
    f.getName.endsWith(".tasty")

  def isBestEffortTastyFile(f: JFile): Boolean =
    f.getName.endsWith(".betasty")

}
