package dotty.tools.dotc

import dotty.tools.FatalError
import config.CompilerCommand
import core.Comments.{ContextDoc, ContextDocstrings}
import core.Contexts.*
import core.{MacroClassLoader, TypeError}
import dotty.tools.dotc.ast.Positioned
import dotty.tools.io.{AbstractFile, FileExtension}
import reporting.*
import core.Decorators.*
import config.Feature

import scala.util.control.NonFatal
import fromtasty.{TASTYCompiler, TastyFileUtil}

/** Run the Dotty compiler.
 *
 *  Extending this class lets you customize many aspect of the compilation
 *  process, but in most cases you only need to call [[process]] on the
 *  existing object [[Main]].
 */
class Driver {

  protected def newCompiler(using Context): Compiler =
    if (ctx.settings.fromTasty.value) new TASTYCompiler
    else new Compiler

  protected def emptyReporter: Reporter = new StoreReporter(null)

  protected def doCompile(compiler: Compiler, files: List[AbstractFile])(using Context): Reporter =
    if files.nonEmpty then
      var runOrNull = ctx.run
      try
        val run = compiler.newRun
        runOrNull = run
        run.compile(files)
        finish(compiler, run)
      catch
        case ex: FatalError =>
          report.error(ex.getMessage.nn) // signals that we should fail compilation.
        case ex: TypeError if !runOrNull.enrichedErrorMessage =>
          println(runOrNull.enrichErrorMessage(s"${ex.toMessage} while compiling ${files.map(_.path).mkString(", ")}"))
          throw ex
        case ex: Throwable if !runOrNull.enrichedErrorMessage =>
          println(runOrNull.enrichErrorMessage(s"Exception while compiling ${files.map(_.path).mkString(", ")}"))
          throw ex
    ctx.reporter

  protected def finish(compiler: Compiler, run: Run)(using Context): Unit =
    run.printSummary()
    if !ctx.reporter.errorsReported && run.suspendedUnits.nonEmpty then
      val suspendedUnits = run.suspendedUnits.toList
      if (ctx.settings.XprintSuspension.value)
        report.echo(i"compiling suspended $suspendedUnits%, %")
      val run1 = compiler.newRun
      run1.compileSuspendedUnits(suspendedUnits)
      finish(compiler, run1)(using MacroClassLoader.init(ctx.fresh))

  protected def initCtx: Context = (new ContextBase).initialCtx

  protected def sourcesRequired: Boolean = true

  protected def command: CompilerCommand = ScalacCommand

  /** Setup context with initialized settings from CLI arguments, then check if there are any settings that
   *  would change the default behaviour of the compiler.
   *
   *  @return If there is no setting like `-help` preventing us from continuing compilation,
   *  this method returns a list of files to compile and an updated Context.
   *  If compilation should be interrupted, this method returns None.
   */
  def setup(args: Array[String], rootCtx: Context): Option[(List[AbstractFile], Context)] = {
    val ictx = rootCtx.fresh
    val summary = command.distill(args, ictx.settings)(ictx.settingsState)(using ictx)
    ictx.setSettings(summary.sstate)
    Feature.checkExperimentalSettings(using ictx)
    MacroClassLoader.init(ictx)
    Positioned.init(using ictx)

    inContext(ictx) {
      if !ctx.settings.YdropComments.value || ctx.settings.YreadComments.value then
        ictx.setProperty(ContextDoc, new ContextDocstrings)
      val fileNamesOrNone = command.checkUsage(summary, sourcesRequired)(using ctx.settings)(using ctx.settingsState)
      fileNamesOrNone.map { fileNames =>
        val files = fileNames.map(ctx.getFile)
        (files, fromTastySetup(files))
      }
    }
  }

  /** Setup extra classpath of tasty and jar files */
  protected def fromTastySetup(files: List[AbstractFile])(using Context): Context =
    if ctx.settings.fromTasty.value then
      val newEntries: List[String] = files
        .flatMap { file =>
          if !file.exists then
            report.error(em"File does not exist: ${file.path}")
            None
          else file.ext match
            case FileExtension.Jar => Some(file.path)
            case FileExtension.Tasty =>
              TastyFileUtil.getClassPath(file) match
                case Some(classpath) => Some(classpath)
                case _ =>
                  report.error(em"Could not load classname from: ${file.path}")
                  None
            case _ =>
              report.error(em"File extension is not `tasty` or `jar`: ${file.path}")
              None
        }
        .distinct
      val ctx1 = ctx.fresh
      val fullClassPath =
        (newEntries :+ ctx.settings.classpath.value).mkString(java.io.File.pathSeparator.nn)
      ctx1.setSetting(ctx1.settings.classpath, fullClassPath)
    else ctx

  /** Entry point to the compiler that can be conveniently used with Java reflection.
   *
   *  This entry point can easily be used without depending on the `dotty` package,
   *  you only need to depend on `dotty-interfaces` and call this method using
   *  reflection. This allows you to write code that will work against multiple
   *  versions of dotty without recompilation.
   *
   *  The trade-off is that you can only pass a SimpleReporter to this method
   *  and not a normal Reporter which is more powerful.
   *
   *  Usage example: [[https://github.com/scala/scala3/tree/master/compiler/test/dotty/tools/dotc/InterfaceEntryPointTest.scala]]
   *
   *  @param args       Arguments to pass to the compiler.
   *  @param simple     Used to log errors, warnings, and info messages.
   *                    The default reporter is used if this is `null`.
   *  @param callback   Used to execute custom code during the compilation
   *                    process. No callbacks will be executed if this is `null`.
   *  @return
   */
  final def process(args: Array[String], simple: interfaces.SimpleReporter | Null,
    callback: interfaces.CompilerCallback | Null): interfaces.ReporterResult = {
    val reporter = if (simple == null) null else Reporter.fromSimpleReporter(simple)
    process(args, reporter, callback)
  }

  /** Principal entry point to the compiler.
   *
   *  Usage example: [[https://github.com/scala/scala3/tree/master/compiler/test/dotty/tools/dotc/EntryPointsTest.scala.disabled]]
   *  in method `runCompiler`
   *
   *  @param args       Arguments to pass to the compiler.
   *  @param reporter   Used to log errors, warnings, and info messages.
   *                    The default reporter is used if this is `null`.
   *  @param callback   Used to execute custom code during the compilation
   *                    process. No callbacks will be executed if this is `null`.
   *  @return           The `Reporter` used. Use `Reporter#hasErrors` to check
   *                    if compilation succeeded.
   */
  final def process(args: Array[String], reporter: Reporter | Null = null,
    callback: interfaces.CompilerCallback | Null = null): Reporter = {
    val compileCtx = initCtx.fresh
    if (reporter != null)
      compileCtx.setReporter(reporter)
    if (callback != null)
      compileCtx.setCompilerCallback(callback)
    process(args, compileCtx)
  }

  /** Entry point to the compiler with no optional arguments.
   *
   *  This overload is provided for compatibility reasons: the
   *  `RawCompiler` of sbt expects this method to exist and calls
   *  it using reflection. Keeping it means that we can change
   *  the other overloads without worrying about breaking compatibility
   *  with sbt.
   */
  def process(args: Array[String]): Reporter =
    process(args, null: Reporter | Null, null: interfaces.CompilerCallback | Null)

  /** Entry point to the compiler using a custom `Context`.
   *
   *  In most cases, you do not need a custom `Context` and should
   *  instead use one of the other overloads of `process`. However,
   *  the other overloads cannot be overridden, instead you
   *  should override this one which they call internally.
   *
   *  Usage example: [[https://github.com/scala/scala3/tree/master/compiler/test/dotty/tools/dotc/EntryPointsTest.scala.disabled]]
   *  in method `runCompilerWithContext`
   *
   *  @param args       Arguments to pass to the compiler.
   *  @param rootCtx    The root Context to use.
   *  @return           The `Reporter` used. Use `Reporter#hasErrors` to check
   *                    if compilation succeeded.
   */
  def process(args: Array[String], rootCtx: Context): Reporter = {
    setup(args, rootCtx) match
      case Some((files, compileCtx)) =>
        doCompile(newCompiler(using compileCtx), files)(using compileCtx)
      case None =>
        rootCtx.reporter
  }

  def main(args: Array[String]): Unit = {
    // Preload scala.util.control.NonFatal. Otherwise, when trying to catch a StackOverflowError,
    // we may try to load it but fail with another StackOverflowError and lose the original exception,
    // see <https://groups.google.com/forum/#!topic/scala-user/kte6nak-zPM>.
    val _ = NonFatal
    sys.exit(if (process(args).hasErrors) 1 else 0)
  }
}
