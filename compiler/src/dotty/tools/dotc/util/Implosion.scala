package dotty.tools
package dotc
package util

import core.*
import Contexts.*, Flags.*, ContextOps.*, Symbols.*, Decorators.*
import reporting.*
import util.{SourcePosition, NoSourcePosition, SrcPos}
import ast.*

import scala.util.control.ControlThrowable
import java.io.{PrintStream, PrintWriter, StringWriter}
import scala.util.control.NoStackTrace



/**
  * An `Error` that indicates that the compiler has crashed.
  * This is used to capture and report important information from the context when the compiler crashes.
  * An `Implosion` should only be caught at the edge of the compiler to prevent the crash propagating to the caller of the compiler.
  * 
  * No handling is required 
  *
  * @param cause The exception that caused the crash.
  */
class Implosion private (val cause: Throwable) extends ControlThrowable with NoStackTrace:
  import scala.language.unsafeNulls
  override def toString: String = cause.toString()
  override def getCause(): Throwable = cause.getCause()
  override def getStackTrace(): Array[StackTraceElement] = cause.getStackTrace()
  override def printStackTrace(s: PrintStream): Unit = cause.printStackTrace(s)
  override def printStackTrace(s: PrintWriter): Unit = cause.printStackTrace(s)



object Implosion :

  def apply(cause: Throwable)(using ctx: Context): Nothing = 
    if cause.isInstanceOf[ControlThrowable] then 
      //not rethrown because `implode` should always crash the compiler
      report.error("A `ControlThrowable` was used to crash the compiler", ctx.tree.sourcePos) 

    val msg =  enrichErrorMessage(cause)
    println("----------------------------------------------------------")
    println(msg)
    println("----------------------------------------------------------")
    report.error(msg, ctx.tree.sourcePos)
    throw new Implosion(cause)

  private def enrichErrorMessage(cause: Throwable)(using Context): String = 
    // record state as we go along so we can report something more than the errorMessage if there's an error while reporting
    val blackBox = new StringBuilder()
    
    extension [A](a: A)
      transparent inline def record: A = 
        blackBox.append(a).append("\n")
        a    


    """|Error during error reporting, black box recording:
       |An unhandled exception was thrown in the compiler. Please file a crash
       |report here: https://github.com/lampepfl/dotty/issues/new/choose)""".stripMargin.record
      
    val errorMessage = cause.toString().record

    val writer = new StringWriter()
    cause.printStackTrace(new PrintWriter(writer))

    val stackTrace = writer.toString().record

    try {
      def formatExplain(pairs: List[(String, Any)]) = 
        pairs.map((k, v) => f"$k%20s: $v").mkString("\n")


      val settings = ctx.settings.userSetSettings(ctx.settingsState).sortBy(_.name).record
      val tree     = ctx.tree.record
      val sym      = tree.symbol.record
      val pos      = tree.sourcePos.record
      val path     = pos.source.path.record
      val site     = ctx.outersIterator.map(_.owner).filter(sym => !sym.exists || sym.isClass || sym.is(Method)).next().record

      import untpd.*
      extension (tree: Tree) def summaryString: String = tree match
        case Literal(const)     => s"Literal($const)"
        case Ident(name)        => s"Ident(${name.decode})"
        case Select(qual, name) => s"Select(${qual.summaryString}, ${name.decode})"
        case tree: NameTree     => (if tree.isType then "type " else "") + tree.name.decode
        case tree               => s"${tree.className}${if tree.symbol.exists then s"(${tree.symbol})" else ""}"

      "info1:".record
      val info1 = formatExplain(List(
        "while compiling".record   -> ctx.compilationUnit.record,
        "during phase".record      -> ctx.phase.prevMega.record,
        "mode".record              -> ctx.mode.record,
        "library version".record   -> scala.util.Properties.versionString.record,
        "compiler version".record  -> dotty.tools.dotc.config.Properties.versionString.record,
        "settings".record          -> settings.map(s => if s.value == "" then s"${s.name} \"\"" else s"${s.name} ${s.value}").mkString(" ").record,
      ))
      "symbolInfos:".record
      val symbolInfos = if sym eq NoSymbol then List("symbol".record -> sym.record) else List(
        "symbol".record             -> sym.showLocated.record,
        "symbol definition".record  -> s"${sym.showDcl} (a ${sym.className})".record,
        //"???" -> ??? //to try out black box
        "symbol package".record     -> sym.enclosingPackageClass.fullName.record,
        "symbol owners".record      -> sym.showExtendedLocation.record
      )
      "info2:".record
      val info2 = formatExplain(List(
        "tree".record               -> tree.summaryString,
        "tree position".record      -> (if pos.exists then s"$path:${pos.line + 1}:${pos.column}" else s"$path:<unknown>"),
        "tree type".record          -> tree.typeOpt.show.record,
      ) ::: symbolInfos.record ::: List(
        "call site".record          -> s"${site.showLocated} in ${site.enclosingPackageClass}".record
      ))
      
      s"""
      |  An unhandled exception was thrown in the compiler. Please file a crash
      |  report here: https://github.com/lampepfl/dotty/issues/new/choose
      |  with the following information:
      |
      |$errorMessage
      |
      |$stackTrace
      |
      |$info1
      |
      |$info2
      |
      |
      |""".stripMargin
    } catch case _: Throwable => 
        // don't introduce new errors trying to report errors, so swallow exceptions and fall back to the blackBox recording
        blackBox.toString()  
