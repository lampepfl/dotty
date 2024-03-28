package dotty.tools
package dotc
package transform

import core.*
import Symbols.*, Types.*, Contexts.*, Flags.*, Decorators.*, reporting.*
import util.SrcPos
import config.{ScalaVersion, NoScalaVersion, Feature, ScalaRelease}
import MegaPhase.MiniPhase
import scala.util.{Failure, Success}
import ast.tpd

class CrossVersionChecks extends MiniPhase:
  import tpd.*
  import CrossVersionChecks.*

  override def phaseName: String = CrossVersionChecks.name

  override def description: String = CrossVersionChecks.description

  // Note: if a symbol has both @deprecated and @migration annotations and both
  // warnings are enabled, only the first one checked here will be emitted.
  // I assume that's a consequence of some code trying to avoid noise by suppressing
  // warnings after the first, but I think it'd be better if we didn't have to
  // arbitrarily choose one as more important than the other.
  private def checkUndesiredProperties(sym: Symbol, pos: SrcPos)(using Context): Unit =
    checkRef(sym, pos)

    val xMigrationValue = ctx.settings.Xmigration.value
    if xMigrationValue != NoScalaVersion then
      checkMigration(sym, pos, xMigrationValue)
  end checkUndesiredProperties

  private def checkExperimentalAnnots(sym: Symbol)(using Context): Unit =
    if sym.exists && !sym.isInExperimentalScope then
      for annot <- sym.annotations if annot.symbol.isExperimental do
        Feature.checkExperimentalDef(annot.symbol, annot.tree)

  /** If @migration is present (indicating that the symbol has changed semantics between versions),
   *  emit a warning.
   */
  private def checkMigration(sym: Symbol, pos: SrcPos, xMigrationValue: ScalaVersion)(using Context): Unit =
    for annot <- sym.getAnnotation(defn.MigrationAnnot) do
      val migrationVersion = ScalaVersion.parse(annot.argumentConstant(1).get.stringValue)
      migrationVersion match
        case Success(symVersion) if xMigrationValue < symVersion =>
          val msg = annot.argumentConstant(0).get.stringValue
          report.warning(SymbolChangedSemanticsInVersion(sym, symVersion, msg), pos)
        case Failure(ex) =>
          report.warning(SymbolHasUnparsableVersionNumber(sym, ex.getMessage.nn), pos)
        case _ =>

  /** Check that a deprecated val or def does not override a
   *  concrete, non-deprecated method.  If it does, then
   *  deprecation is meaningless.
   */
  private def checkDeprecatedOvers(tree: Tree)(using Context): Unit = {
    val symbol = tree.symbol
    if (symbol.isDeprecated) {
      val concrOvers =
        symbol.allOverriddenSymbols.filter(sym =>
          !sym.isDeprecated && !sym.is(Deferred))
      if (!concrOvers.isEmpty)
        report.deprecationWarning(
          em"""$symbol overrides concrete, non-deprecated definition(s):
              |    ${concrOvers.map(_.name).mkString(", ")}""",
          tree.srcPos)
    }
  }

  /** ??? */
  def checkDeprecatedInheritance(parents: List[Tree])(using Context): Unit = {
    for parent <- parents
        psym = parent.tpe.classSymbol
        annot <- psym.getAnnotation(defn.DeprecatedInheritanceAnnot)
        if !skipWarning(psym)
    do
      val msg = annot.argumentConstantString(0).map(msg => s": $msg").getOrElse("")
      val since = annot.argumentConstantString(1).map(version => s" (since: $version)").getOrElse("")
      report.deprecationWarning(em"inheritance from $psym is deprecated$since$msg", parent.srcPos)
  }

  override def transformValDef(tree: ValDef)(using Context): ValDef =
    checkDeprecatedOvers(tree)
    checkExperimentalAnnots(tree.symbol)
    tree

  override def transformDefDef(tree: DefDef)(using Context): DefDef =
    checkDeprecatedOvers(tree)
    checkExperimentalAnnots(tree.symbol)
    tree

  override def transformTypeDef(tree: TypeDef)(using Context): TypeDef =
    // TODO do we need to check checkDeprecatedOvers(tree)?
    checkExperimentalAnnots(tree.symbol)
    tree

  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree =
    checkDeprecatedInheritance(tree.parents)
    tree

  override def transformIdent(tree: Ident)(using Context): Ident = {
    checkUndesiredProperties(tree.symbol, tree.srcPos)
    tree
  }

  override def transformSelect(tree: Select)(using Context): Select = {
    checkUndesiredProperties(tree.symbol, tree.srcPos)
    tree
  }

  override def transformNew(tree: New)(using Context): New = {
    checkUndesiredProperties(tree.tpe.typeSymbol, tree.srcPos)
    tree
  }

  override def transformTypeTree(tree: TypeTree)(using Context): TypeTree = {
    val tpe = tree.tpe
    tpe.foreachPart {
      case TypeRef(_, sym: Symbol)  =>
        if tree.span.isSourceDerived then
          checkDeprecatedRef(sym, tree.srcPos)
        checkExperimentalRef(sym, tree.srcPos)
      case TermRef(_, sym: Symbol)  =>
        if tree.span.isSourceDerived then
          checkDeprecatedRef(sym, tree.srcPos)
        checkExperimentalRef(sym, tree.srcPos)
      case _ =>
    }
    tree
  }

  override def transformOther(tree: Tree)(using Context): Tree =
    val inPackage = ctx.owner.is(Package) || ctx.owner.isPackageObject
    if !(inPackage && tree.isInstanceOf[ImportOrExport] && Feature.isExperimentalEnabledByImport) then
      tree.foreachSubTree { // Find references in type trees and imports
        case tree: Ident => transformIdent(tree)
        case tree: Select => transformSelect(tree)
        case tree: TypeTree => transformTypeTree(tree)
        case _ =>
      }
    tree

end CrossVersionChecks

object CrossVersionChecks:
  val name: String = "crossVersionChecks"
  val description: String = "check issues related to deprecated and experimental"

  /** Check that a reference to an experimental definition with symbol `sym` meets cross-version constraints
   *  for `@deprecated` and `@experimental`.
   */
  def checkRef(sym: Symbol, pos: SrcPos)(using Context): Unit =
    checkDeprecatedRef(sym, pos)
    checkExperimentalRef(sym, pos)

  /** Check that a reference to an experimental definition with symbol `sym` is only
   *  used in an experimental scope
   */
  private[CrossVersionChecks] def checkExperimentalRef(sym: Symbol, pos: SrcPos)(using Context): Unit =
    if sym.isExperimental && !ctx.owner.isInExperimentalScope then
      Feature.checkExperimentalDef(sym, pos)

  /** If @deprecated is present, and the point of reference is not enclosed
   *  in either a deprecated member or a scala bridge method, issue a warning.
   */
  private[CrossVersionChecks] def checkDeprecatedRef(sym: Symbol, pos: SrcPos)(using Context): Unit =

    // Also check for deprecation of the companion class for synthetic methods
    val toCheck = sym :: (if sym.isAllOf(SyntheticMethod) then sym.owner.companionClass :: Nil else Nil)
    for sym <- toCheck; annot <- sym.getAnnotation(defn.DeprecatedAnnot) do
      if !skipWarning(sym) then
        val msg = annot.argumentConstant(0).map(": " + _.stringValue).getOrElse("")
        val since = annot.argumentConstant(1).map(" since " + _.stringValue).getOrElse("")
        report.deprecationWarning(em"${sym.showLocated} is deprecated${since}${msg}", pos)

  /** Skip warnings for synthetic members of case classes during declaration and
   *  scan the chain of outer declaring scopes from the current context
   *  a deprecation warning will be skipped if one the following holds
   *  for a given declaring scope:
   *  - the symbol associated with the scope is also deprecated.
   *  - if and only if `sym` is an enum case, the scope is either
   *    a module that declares `sym`, or the companion class of the
   *    module that declares `sym`.
   */
  private def skipWarning(sym: Symbol)(using Context): Boolean =

    /** is the owner an enum or its companion and also the owner of sym */
    def isEnumOwner(owner: Symbol)(using Context) =
      // pre: sym is an enumcase
      if owner.isEnumClass then owner.companionClass eq sym.owner
      else if owner.is(ModuleClass) && owner.companionClass.isEnumClass then owner eq sym.owner
      else false

    def isDeprecatedOrEnum(owner: Symbol)(using Context) =
      // pre: sym is an enumcase
      owner.isDeprecated || isEnumOwner(owner)

    (ctx.owner.is(Synthetic) && sym.is(CaseClass))
      || ctx.owner.ownersIterator.exists(if sym.isEnumCase then isDeprecatedOrEnum else _.isDeprecated)
  end skipWarning
