package dotty.tools.repl.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme


/** This phase collects and transforms top-level Import trees to handle definition shadowing.
 *
 *  This is used by repl to handle new run contexts and allowing
 *  definitions to be shadowed by imports in the same run.
 *
 *  Import transformation is necessary for excluding its members when they are shadowed in the same run.
 *  This is done by finding all members defined after the Import clause calculating
 *  their intersection with available members from selectors including renaming.
 *
 *  This step is necessary for proper new run initialization since we need to import the previous run
 *  into Context. It is accomplished in the following order:
 *    1. Previous wrapper object for a given run
 *    2. Previous imports for a given run
 *
 *  This phase uses typed trees thus after the Typer phase.
 */
class CollectTopLevelImports extends Phase {
  import tpd._

  def phaseName: String = "collectTopLevelImports"

  private var gatheredImports: List[Import] = _

  def imports: List[Import] = gatheredImports

  def run(using Context): Unit =
    val PackageDef(_, _ :: TypeDef(_, rhs: Template) :: _) = ctx.compilationUnit.tpdTree: @unchecked
    gatheredImports = transformTopLevelImports(rhs.body)

  /** Transforms top-level imports to exclude intersecting members declared after the Import clause.
   *  To properly handle imports such as: `import A.f; def f = 3`  consequently making sure that original selectors are
   *  filtered to eliminate potential duplications that would result in compilation error.
   *
   *  Transformed imports of which selectors were all shadowed will be ignored in the future runs.
   */
  private def transformTopLevelImports(trees: List[Tree])(using Context): List[Import] =
    val definitions = collectTopLevelMemberDefs(trees)

    trees.collect {
      case tree @ Import(expr, selectors) =>
        val definitionsAfterImport = definitions.filter(_._2 > tree.endPos.end).map(_._1)

        val importedNames: List[Name] = (if selectors.exists(_.isWildcard) then
          val allImportTypeMembers = expr.tpe.allMembers.map(_.name)
          val nonWildcardSelectors = selectors.filter(_.isWildcard)
          val renamedMembers = nonWildcardSelectors.map(_.imported.name)
          nonWildcardSelectors.map(_.rename) ++ allImportTypeMembers.filterNot(renamedMembers.contains)
        else
          selectors.map(_.rename)
        )

        val shadowedMembers = importedNames.intersect(definitionsAfterImport)
        val adjustedSelectors = shadowedMembers.map(collidingMember => {
          untpd.ImportSelector(untpd.Ident(collidingMember), untpd.Ident(nme.WILDCARD))
        })

        val remainingSelectors = selectors.filterNot(importSelector => {
            shadowedMembers.contains(importSelector.rename)
        })

        if remainingSelectors.isEmpty then
          None
        else
          Some(Import(expr, adjustedSelectors ++ remainingSelectors))
    }.flatten

  private def collectTopLevelMemberDefs(trees: List[Tree])(using Context): List[(Name, Int)] =
    trees.collect {
      case tree: ValDef  => tree.name -> tree.endPos.end
      case tree: DefDef  => tree.name -> tree.endPos.end
      case tree: TypeDef => tree.name -> tree.endPos.end
    }

}

