package dotty.tools.dotc
package transform
package init

import MegaPhase._
import ast.tpd
import core.Contexts._

/** Set the `defTree` property of symbols */
class SetDefTree extends MiniPhase {
  import tpd._

  override val phaseName: String = SetDefTree.name
  override val runsAfter = Set(Pickler.name)

  override def isEnabled(using Context): Boolean =
    super.isEnabled && ctx.settings.YcheckInit.value

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    val ctx2 = ctx.fresh.setSetting(ctx.settings.YretainTrees, true)
    super.runOn(units)(using ctx2)
  }

  override def transformValDef(tree: ValDef)(using Context): Tree = tree.setDefTree

  override def transformDefDef(tree: DefDef)(using Context): Tree = tree.setDefTree

  override def transformTypeDef(tree: TypeDef)(using Context): Tree = tree.setDefTree
}

object SetDefTree {
  val name: String = "SetDefTree"
}
