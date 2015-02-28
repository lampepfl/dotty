package dotty.tools
package dotc

import core._
import Contexts._, Periods._, Symbols._, Phases._, Decorators._
import dotty.tools.dotc.transform.TreeTransforms.TreeTransformer
import io.PlainFile
import util.{SourceFile, NoSource, Stats, SimpleMap}
import reporting.Reporter
import transform.TreeChecker
import java.io.{BufferedWriter, OutputStreamWriter}
import scala.reflect.io.VirtualFile

class Run(comp: Compiler)(implicit ctx: Context) {

  assert(comp.phases.last.last.id <= Periods.MaxPossiblePhaseId)
  assert(ctx.runId <= Periods.MaxPossibleRunId)

  var units: List[CompilationUnit] = _

  def getSource(fileName: String): SourceFile = {
    val f = new PlainFile(fileName)
    if (f.exists) new SourceFile(f)
    else {
      ctx.error(s"not found: $fileName")
      NoSource
    }
  }

  def compile(fileNames: List[String]): Unit = {
    val sources = fileNames map getSource
    compileSources(sources)
  }

  /** TODO: There's a fundamental design problem here: We assmble phases using `squash`
   *  when we first build the compiler. But we modify them with -Yskip, -Ystop
   *  on each run. That modification needs to either trasnform the tree structure,
   *  or we need to assmeble phases on each run, and take -Yskip, -Ystop into
   *  account. I think the latter would be preferable.
   */
  def compileSources(sources: List[SourceFile]) = Stats.monitorHeartBeat {
    if (sources forall (_.exists)) {
      val phases = ctx.squashPhases(ctx.phasePlan,
        ctx.settings.Yskip.value, ctx.settings.YstopBefore.value, ctx.settings.YstopAfter.value, ctx.settings.Ycheck.value)
      ctx.usePhases(phases)
      units = sources map (new CompilationUnit(_))
      var stopped = false
      for (phase <- ctx.allPhases) {
        stopped |= phase.stopOnError && ctx.reporter.hasErrors
        if (!stopped) {
          if (ctx.settings.verbose.value) println(s"[$phase]")
          units = phase.runOn(units)
          def foreachUnit(op: Context => Unit)(implicit ctx: Context): Unit =
            for (unit <- units) op(ctx.fresh.setPhase(phase.next).setCompilationUnit(unit))
          if (ctx.settings.Xprint.value.containsPhase(phase))
            foreachUnit(printTree)
        }
      }
    }
  }

  private def printTree(ctx: Context) = {
    val unit = ctx.compilationUnit
    val prevPhase = ctx.phase.prev // can be a mini-phase
    val squashedPhase = ctx.squashed(prevPhase)
    val tree = if (prevPhase.untypedResult) unit.untpdTree else unit.tpdTree

    println(s"result of $unit after ${squashedPhase}:")
    println(tree.show(ctx))
  }

  def compile(sourceCode: String): Unit = {
    val virtualFile = new VirtualFile(sourceCode) // use source code as name as it's used for equals
    val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8")) // buffering is still advised by javadoc
    writer.write(sourceCode)
    writer.close()
    compileSources(List(new SourceFile(virtualFile)))
  }

  /** Print summary; return # of errors encountered */
  def printSummary(): Reporter = {
    ctx.runInfo.printMaxConstraint()
    val r = ctx.typerState.reporter
    r.printSummary
    r
  }
}