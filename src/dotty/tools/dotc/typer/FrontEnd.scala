package dotty.tools.dotc
package typer

import core._
import Phases._
import Contexts._
import Symbols._
import dotty.tools.dotc.parsing.JavaParsers.JavaParser
import parsing.Parsers.Parser
import config.Config
import config.Printers.{typr, default}
import util.Stats._
import scala.util.control.NonFatal
import ast.Trees._
import util.FreshNameCreator

class FrontEnd extends Phase {

  override def phaseName = "frontend"
  override def isTyper = true
  import ast.tpd

  def monitor(doing: String)(body: => Unit)(implicit ctx: Context) =
    try body
    catch {
      case NonFatal(ex) =>
        ctx.echo(s"exception occurred while $doing ${ctx.compilationUnit}")
        throw ex
    }

  def parse(implicit ctx: Context) = monitor("parsing") {
    val unit = ctx.compilationUnit
    unit.untpdTree =
      if (unit.isJava) new JavaParser(unit.source).parse()
      else new Parser(unit.source).parse()
    val printer = if (ctx.settings.Xprint.value.contains("parser")) default else typr
    printer.println("parsed:\n" + unit.untpdTree.show)
    if (Config.checkPositions) 
      unit.untpdTree.checkPos(nonOverlapping = !unit.isJava && !ctx.reporter.hasErrors)
  }

  def enterSyms(implicit ctx: Context) = monitor("indexing") {
    val unit = ctx.compilationUnit
    ctx.typer.index(unit.untpdTree)
    typr.println("entered: " + unit.source)
  }

  def typeCheck(implicit ctx: Context) = monitor("typechecking") {
    val unit = ctx.compilationUnit
    unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
    typr.println("typed: " + unit.source)
    record("retainedUntypedTrees", unit.untpdTree.treeSize)
    record("retainedTypedTrees", unit.tpdTree.treeSize)
  }

  private def firstTopLevelDef(trees: List[tpd.Tree])(implicit ctx: Context): Symbol = trees match {
    case PackageDef(_, defs) :: _    => firstTopLevelDef(defs)
    case Import(_, _) :: defs        => firstTopLevelDef(defs)
    case (tree @ TypeDef(_, _)) :: _ => tree.symbol
    case _ => NoSymbol
  }

  protected def discardAfterTyper(unit: CompilationUnit)(implicit ctx: Context) =
    unit.isJava || firstTopLevelDef(unit.tpdTree :: Nil).isPrimitiveValueClass

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val unitContexts = for (unit <- units) yield
      ctx.fresh.setCompilationUnit(unit).setFreshNames(new FreshNameCreator.Default)
    unitContexts foreach (parse(_))
    println(s"total trees created after parsing: ${ast.Trees.ntrees}")
    record("parsedTrees", ast.Trees.ntrees)
    unitContexts foreach (enterSyms(_))
    unitContexts foreach (typeCheck(_))
    println(s"total trees created after typing: ${ast.Trees.ntrees}")
    println(s"retained parse trees: ${nodesStat(unitContexts.map(ctx => ctx.compilationUnit.untpdTree))}")
    println(s"retained typed trees: ${nodesStat(unitContexts.map(ctx => ctx.compilationUnit.tpdTree))}")
    record("totalTrees", ast.Trees.ntrees)
    unitContexts.map(_.compilationUnit).filterNot(discardAfterTyper)
  }

  override def run(implicit ctx: Context): Unit = {
    parse
    enterSyms
    typeCheck
  }
}
