package dotty.tools.dotc
package transform

import core.*
import Phases.*
import ast.Trees.*
import Contexts.*

/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
abstract class MacroTransform extends Phase {

  import ast.tpd.*

  override def isRunnable(using Context) = super.isRunnable && !ctx.usedBestEffortTasty

  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    unit.tpdTree = atPhase(transformPhase)(newTransformer.transform(unit.tpdTree))
  }

  protected def newTransformer(using Context): Transformer

  /** The phase in which the transformation should be run.
   *  By default this is the phase given by the this macro transformer,
   *  but it could be overridden to be the phase following that one.
   */
  protected def transformPhase(using Context): Phase = this

  class Transformer extends TreeMapWithPreciseStatContexts(cpy = cpyBetweenPhases):

    protected def localCtx(tree: Tree)(using Context): FreshContext =
      ctx.fresh.setTree(tree).setOwner(localOwner(tree))

    override def transform(tree: Tree)(using Context): Tree =
      try
        tree match {
          case EmptyValDef =>
            tree
          case _: PackageDef | _: MemberDef =>
            super.transform(tree)(using localCtx(tree))
          case impl @ Template(constr, _, self, _) =>
            cpy.Template(tree)(
              transformSub(constr),
              transform(impl.parents)(using ctx.superCallContext),
              Nil,
              transformSelf(self),
              transformStats(impl.body, tree.symbol))
          case _ =>
            super.transform(tree)
        }
      catch {
        case ex: TypeError =>
          report.error(ex, tree.srcPos)
          tree
      }

    def transformSelf(vd: ValDef)(using Context): ValDef =
      cpy.ValDef(vd)(tpt = transform(vd.tpt))
  end Transformer
}
