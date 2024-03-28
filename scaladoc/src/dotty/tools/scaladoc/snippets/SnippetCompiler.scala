package dotty.tools.scaladoc
package snippets

import dotty.tools.io.{AbstractFile, VirtualDirectory}
import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.config.Settings.Setting._
import dotty.tools.dotc.interfaces.{ SourcePosition => ISourcePosition }
import dotty.tools.dotc.ast.Trees.Tree
import dotty.tools.dotc.interfaces.{SourceFile => ISourceFile}
import dotty.tools.dotc.reporting.{ Diagnostic, StoreReporter }
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.{ Compiler, Run }
import dotty.tools.io.{AbstractFile, VirtualDirectory}
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.util.Spans._
import dotty.tools.dotc.interfaces.Diagnostic._
import dotty.tools.dotc.util.{ SourcePosition, NoSourcePosition, SourceFile, NoSource }

import scala.util.{ Try, Success, Failure }

class SnippetCompiler(
  val snippetCompilerSettings: Seq[SnippetCompilerSetting[?]],
  target: AbstractFile = new VirtualDirectory("(memory)")
):
  object SnippetDriver extends Driver:
    val currentCtx =
      val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions).addMode(Mode.Interactive)
      rootCtx.setSetting(rootCtx.settings.experimental, true)
      rootCtx.setSetting(rootCtx.settings.YretainTrees, true)
      rootCtx.setSetting(rootCtx.settings.YcookComments, true)
      rootCtx.setSetting(rootCtx.settings.YreadComments, true)
      rootCtx.setSetting(rootCtx.settings.color, "never")
      rootCtx.setSetting(rootCtx.settings.XimportSuggestionTimeout, 0)

      val ctx = setup(Array(""), rootCtx) match
        case Some((_, ctx)) =>
          ctx
        case None => rootCtx
      val res = snippetCompilerSettings.foldLeft(ctx.fresh) { (ctx, setting) =>
        ctx.setSetting(setting.setting, setting.value)
      }
      res.initialize()(using res)
      res

  private val scala3Compiler = new Compiler

  private def newRun(using ctx: Context): Run = scala3Compiler.newRun

  private def nullableMessage(msgOrNull: String): String =
    if (msgOrNull == null) "" else msgOrNull

  private def createReportMessage(wrappedSnippet: WrappedSnippet, arg: SnippetCompilerArg, diagnostics: Seq[Diagnostic], sourceFile: SourceFile): Seq[SnippetCompilerMessage] = {
    val line = wrappedSnippet.outerLineOffset
    val column = wrappedSnippet.outerColumnOffset
    val innerLineOffset = wrappedSnippet.innerLineOffset
    val innerColumnOffset = wrappedSnippet.innerColumnOffset
    val infos = diagnostics.toSeq.sortBy(_.pos.source.path)
    val errorMessages = infos.map {
      case diagnostic if diagnostic.position.isPresent =>
        val diagPos = diagnostic.position.get match
          case s: SourcePosition => s
          case _ => NoSourcePosition
        val offsetFromLine = sourceFile match
          case NoSource => 0
          case sf: SourceFile => sf.lineToOffset(diagPos.line + line - innerLineOffset)
        val offsetFromColumn = diagPos.column + column - innerColumnOffset
        val span = Span(offsetFromLine + offsetFromColumn, offsetFromLine + offsetFromColumn)
        val pos = Some(
          Position(dotty.tools.dotc.util.SourcePosition(sourceFile, span), diagPos.line - innerLineOffset)
        )
        val dmsg = Try(diagnostic.message) match {
          case Success(msg) => msg
          case Failure(ex) => ex.getMessage
        }
        val msg = nullableMessage(dmsg)
        val level = MessageLevel.fromOrdinal(diagnostic.level)
        SnippetCompilerMessage(pos, msg, level)
      case d =>
        val level = MessageLevel.fromOrdinal(d.level)
        SnippetCompilerMessage(None, nullableMessage(d.message), level)
    }
    errorMessages
  }

  private def additionalMessages(wrappedSnippet: WrappedSnippet, arg: SnippetCompilerArg, sourceFile: SourceFile, context: Context): Seq[SnippetCompilerMessage] = {
      Option.when(arg.flag == SCFlags.Fail && !context.reporter.hasErrors)(
        SnippetCompilerMessage(None, "Snippet should not compile but compiled successfully", MessageLevel.Error)
      ).toList
  }

  private def isSuccessful(arg: SnippetCompilerArg, context: Context): Boolean = {
    if arg.flag == SCFlags.Fail then context.reporter.hasErrors
    else !context.reporter.hasErrors
  }

  def compile(
    wrappedSnippet: WrappedSnippet,
    arg: SnippetCompilerArg,
    sourceFile: SourceFile
  ): SnippetCompilationResult = {
    val context = SnippetDriver.currentCtx.fresh
      .setSetting(
        SnippetDriver.currentCtx.settings.outputDir,
        target
      )
      .setReporter(new StoreReporter)
    val run = newRun(using context)
    run.compileFromStrings(List(wrappedSnippet.snippet))

    val messages =
      createReportMessage(wrappedSnippet, arg, context.reporter.pendingMessages(using context), sourceFile) ++
      additionalMessages(wrappedSnippet, arg, sourceFile, context)

    val t = Option.when(!context.reporter.hasErrors)(target)
    SnippetCompilationResult(wrappedSnippet, isSuccessful(arg, context), t, messages)
  }
