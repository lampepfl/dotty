package dotty.tools
package dotc

import core.*
import Contexts.*
import SymDenotations.ClassDenotation
import Symbols.*
import Comments.Comment
import util.{FreshNameCreator, SourceFile, NoSource}
import util.Spans.Span
import ast.{tpd, untpd}
import tpd.{Tree, TreeTraverser}
import ast.Trees.{Import, Ident}
import typer.Nullables
import core.Decorators.*
import config.{SourceVersion, Feature}
import StdNames.nme
import scala.annotation.internal.sharable
import scala.util.control.NoStackTrace
import transform.MacroAnnotations

class CompilationUnit protected (val source: SourceFile, val info: CompilationUnitInfo | Null) {

  override def toString: String = source.toString

  var untpdTree: untpd.Tree = untpd.EmptyTree

  var tpdTree: tpd.Tree = tpd.EmptyTree

  /** Is this the compilation unit of a Java file */
  def isJava: Boolean = source.file.ext.isJava

  /** Is this the compilation unit of a Java file, or TASTy derived from a Java file */
  def typedAsJava =
    val ext = source.file.ext
    ext.isJavaOrTasty && (ext.isJava || tastyInfo.exists(_.attributes.isJava))

  def tastyInfo: Option[TastyInfo] =
    val local = info
    if local == null then None else local.tastyInfo


  /** The source version for this unit, as determined by a language import */
  var sourceVersion: Option[SourceVersion] = None

  /** Pickled TASTY binaries, indexed by class. */
  var pickled: Map[ClassSymbol, () => Array[Byte]] = Map()

  /** The fresh name creator for the current unit.
   *  FIXME(#7661): This is not fine-grained enough to enable reproducible builds,
   *  see https://github.com/scala/scala/commit/f50ec3c866263448d803139e119b33afb04ec2bc
   */
  val freshNames: FreshNameCreator = new FreshNameCreator.Default

  /** Will be set to `true` if there are inline call that must be inlined after typer.
   *  The information is used in phase `Inlining` in order to avoid traversing trees that need no transformations.
   */
  var needsInlining: Boolean = false

  var hasMacroAnnotations: Boolean = false

  /** Set to `true` if inliner added anonymous mirrors that need to be completed */
  var needsMirrorSupport: Boolean = false

  /** Will be set to `true` if contains `Quote`.
   *  The information is used in phase `Staging`/`Splicing`/`PickleQuotes` in order to avoid traversing trees that need no transformations.
   */
  var needsStaging: Boolean = false

  /** Will be set to true if the unit contains a captureChecking language import */
  var needsCaptureChecking: Boolean = false

  /** Will be set to true if the unit contains a pureFunctions language import */
  var knowsPureFuns: Boolean = false

  var suspended: Boolean = false
  var suspendedAtInliningPhase: Boolean = false

  /** Can this compilation unit be suspended */
  def isSuspendable: Boolean = true

  /** List of all comments present in this compilation unit */
  var comments: List[Comment] = Nil

  /** This is used to record dependencies to invalidate during incremental
   *  compilation, but only if `ctx.runZincPhases` is true.
   */
  val depRecorder: sbt.DependencyRecorder = sbt.DependencyRecorder()

  /** Suspends the compilation unit by thowing a SuspendException
   *  and recording the suspended compilation unit
   */
  def suspend()(using Context): Nothing =
    assert(isSuspendable)
    // Clear references to symbols that may become stale. No need to call
    // `depRecorder.sendToZinc()` since all compilation phases will be rerun
    // when this unit is unsuspended.
    depRecorder.clear()
    if !suspended then
      if ctx.settings.YnoSuspendedUnits.value then
        report.error(i"Compilation unit suspended $this (-Yno-suspended-units is set)")
      else
        if (ctx.settings.XprintSuspension.value)
          report.echo(i"suspended: $this")
        suspended = true
        ctx.run.nn.suspendedUnits += this
        if ctx.phase == Phases.inliningPhase then
          suspendedAtInliningPhase = true
    throw CompilationUnit.SuspendException()

  private var myAssignmentSpans: Map[Int, List[Span]] | Null = null

  /** A map from (name-) offsets of all local variables in this compilation unit
   *  that can be tracked for being not null to the list of spans of assignments
   *  to these variables.
   */
  def assignmentSpans(using Context): Map[Int, List[Span]] =
    if myAssignmentSpans == null then myAssignmentSpans = Nullables.assignmentSpans
    myAssignmentSpans.nn
}

@sharable object NoCompilationUnit extends CompilationUnit(NoSource, info = null) {

  override def isJava: Boolean = false

  override def suspend()(using Context): Nothing =
    throw CompilationUnit.SuspendException()

  override def assignmentSpans(using Context): Map[Int, List[Span]] = Map.empty
}

object CompilationUnit {

  class SuspendException extends Exception with NoStackTrace

  /** Make a compilation unit for top class `clsd` with the contents of the `unpickled` tree */
  def apply(clsd: ClassDenotation, unpickled: Tree, forceTrees: Boolean)(using Context): CompilationUnit =
    val compilationUnitInfo = clsd.symbol.compilationUnitInfo.nn
    val file = compilationUnitInfo.associatedFile
    apply(SourceFile(file, Array.empty[Char]), unpickled, forceTrees, compilationUnitInfo)

  /** Make a compilation unit, given picked bytes and unpickled tree */
  def apply(source: SourceFile, unpickled: Tree, forceTrees: Boolean, info: CompilationUnitInfo)(using Context): CompilationUnit = {
    assert(!unpickled.isEmpty, unpickled)
    val unit1 = new CompilationUnit(source, info)
    unit1.tpdTree = unpickled
    if (forceTrees) {
      val force = new Force
      force.traverse(unit1.tpdTree)
      unit1.needsStaging = force.containsQuote
      unit1.needsInlining = force.containsInline
      unit1.hasMacroAnnotations = force.containsMacroAnnotation
    }
    unit1
  }

  /** Create a compilation unit corresponding to `source`.
   *  If `mustExist` is true, this will fail if `source` does not exist.
   */
  def apply(source: SourceFile, mustExist: Boolean = true)(using Context): CompilationUnit = {
    val src =
      if (!mustExist)
        source
      else if (source.file.isDirectory) {
        report.error(em"expected file, received directory '${source.file.path}'")
        NoSource
      }
      else if (!source.file.exists) {
        report.error(em"source file not found: ${source.file.path}")
        NoSource
      }
      else source
    val info = if src.exists then CompilationUnitInfo(src.file) else null
    new CompilationUnit(src, info)
  }

  /** Force the tree to be loaded */
  private class Force extends TreeTraverser {
    var containsQuote = false
    var containsInline = false
    var containsCaptureChecking = false
    var containsMacroAnnotation = false
    def traverse(tree: Tree)(using Context): Unit = {
      if tree.symbol.is(Flags.Inline) then
        containsInline = true
      tree match
        case _: tpd.Quote =>
          containsQuote = true
        case tree: tpd.Apply if tree.symbol == defn.QuotedTypeModule_of =>
          containsQuote = true
        case Import(qual, selectors) =>
          tpd.languageImport(qual) match
            case Some(prefix) =>
              for case untpd.ImportSelector(untpd.Ident(imported), untpd.EmptyTree, _) <- selectors do
                Feature.handleGlobalLanguageImport(prefix, imported)
            case _ =>
        case _ =>
      for annot <- tree.symbol.annotations do
        if MacroAnnotations.isMacroAnnotation(annot) then
          ctx.compilationUnit.hasMacroAnnotations = true
      traverseChildren(tree)
    }
  }
}
