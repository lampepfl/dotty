package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Flags._
import core.Constants._
import core.StdNames._
import core.Decorators._
import core.TypeErasure.isErasedType
import core.Phases.Phase
import typer._
import typer.ErrorReporting._
import reporting.ThrowingReporter
import ast.Trees._
import ast.{tpd, untpd}
import util.SourcePosition
import java.lang.AssertionError

/** Run by -Ycheck option after a given phase, this class retypes all syntax trees
 *  and verifies that the type of each tree node so obtained conforms to the type found in the tree node.
 *  It also performs the following checks:
 *
 *   - The owner of each definition is the same as the owner of the current typing context.
 *   - Ident nodes do not refer to a denotation that would need a select to be accessible
 *     (see tpd.needsSelect).
 *   - After typer, identifiers and select nodes refer to terms only (all types should be
 *     represented as TypeTrees then).
 */
class TreeChecker {
  import ast.tpd._

  private def previousPhases(phases: List[Phase])(implicit ctx: Context): List[Phase] = phases match {
    case (phase: TreeTransformer) :: phases1 =>
      val subPhases = phase.transformations.map(_.phase)
      val previousSubPhases = previousPhases(subPhases.toList)
      if (previousSubPhases.length == subPhases.length) previousSubPhases ::: previousPhases(phases1)
      else previousSubPhases
    case phase :: phases1 if phase ne ctx.phase =>
      phase :: previousPhases(phases1)
    case _ =>
      Nil
  }
  def check(phasesToRun: Seq[Phase], ctx: Context) = {
    println(s"checking ${ctx.compilationUnit} after phase ${ctx.phase.prev}")
    val checkingCtx = ctx.fresh
      .setTyperState(ctx.typerState.withReporter(new ThrowingReporter(ctx.typerState.reporter)))
    val checker = new Checker(previousPhases(phasesToRun.toList)(ctx))
    checker.typedExpr(ctx.compilationUnit.tpdTree)(checkingCtx)
  }

  class Checker(phasesToCheck: Seq[Phase]) extends ReTyper {
    override def typed(tree: untpd.Tree, pt: Type)(implicit ctx: Context) = {
      val res = tree match {
        case _: untpd.UnApply =>
          // can't recheck patterns
          tree.asInstanceOf[tpd.Tree]
        case _: untpd.TypedSplice | _: untpd.Thicket | _: EmptyValDef[_] =>
          super.typed(tree)
        case _ if tree.isType =>
          promote(tree)
        case _ =>
          val tree1 = super.typed(tree, pt)
          def isSubType(tp1: Type, tp2: Type) =
            (tp1 eq tp2) || // accept NoType / NoType
            (tp1 <:< tp2)
          def divergenceMsg(tp1: Type, tp2: Type) =
            s"""Types differ
               |Original type : ${tree.typeOpt.show}
               |After checking: ${tree1.tpe.show}
               |Original tree : ${tree.show}
               |After checking: ${tree1.show}
               |Why different :
             """.stripMargin + core.TypeComparer.explained((tp1 <:< tp2)(_))
          assert(isSubType(tree1.tpe, tree.typeOpt), divergenceMsg(tree1.tpe, tree.typeOpt))
          tree1
        }
      phasesToCheck.foreach(_.checkPostCondition(res))
      res
    }

    override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.isTerm || !ctx.isAfterTyper, tree.show + " at " + ctx.phase)
      assert(tree.isType || !needsSelect(tree.tpe), i"bad type ${tree.tpe} for $tree")
      super.typedIdent(tree, pt)
    }

    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.isTerm || !ctx.isAfterTyper, tree.show + " at " + ctx.phase)
      super.typedSelect(tree, pt)
    }

    private def checkOwner(tree: untpd.Tree)(implicit ctx: Context): Unit = {
      def ownerMatches(symOwner: Symbol, ctxOwner: Symbol): Boolean =
        symOwner == ctxOwner ||
          ctxOwner.isWeakOwner && (!(ctxOwner is Method | Lazy | Mutable) || (ctxOwner is Label)) &&
          ownerMatches(symOwner, ctxOwner.owner)
      if(!ownerMatches(tree.symbol.owner, ctx.owner)) {
        assert(ownerMatches(tree.symbol.owner, ctx.owner),
          i"bad owner; ${tree.symbol} has owner ${tree.symbol.owner}, expected was ${ctx.owner}\n" +
          i"owner chain = ${tree.symbol.ownersIterator.toList}%, %, ctxOwners = ${ctx.outersIterator.map(_.owner).toList}%, %")
      }
    }

    override def typedClassDef(cdef: untpd.TypeDef, cls: ClassSymbol)(implicit ctx: Context) = {
      val TypeDef(_, _, impl @ Template(constr, _, _, _)) = cdef
      checkOwner(impl)
      checkOwner(impl.constr)
      super.typedClassDef(cdef, cls)
    }

    /** Check that all defined symbols have legal owners.
     *  An owner is legal if it is either the same as the context's owner
     *  or there's an owner chain of valdefs starting at the context's owner and
     *  reaching up to the symbol's owner. The reason for this relaxed matching
     *  is that we should be able to pull out an expression as an initializer
     *  of a helper value without having to do a change owner traversal of the expression.
     */
    override def typedStats(trees: List[untpd.Tree], exprOwner: Symbol)(implicit ctx: Context): List[Tree] = {
      for (tree <- trees) tree match {
        case tree: untpd.DefTree => checkOwner(tree)
        case _: untpd.Thicket => assert(false, "unexpanded thicket in statement sequence")
        case _ =>
      }
      super.typedStats(trees, exprOwner)
    }

    override def adapt(tree: Tree, pt: Type, original: untpd.Tree = untpd.EmptyTree)(implicit ctx: Context) = {
      def isPrimaryConstructorReturn =
        ctx.owner.isPrimaryConstructor && pt.isRef(ctx.owner.owner) && tree.tpe.isRef(defn.UnitClass)
      if (ctx.mode.isExpr && !isPrimaryConstructorReturn)
        assert(tree.tpe <:< pt,
            s"error at ${sourcePos(tree.pos)}\n" +
            err.typeMismatchStr(tree.tpe, pt))
      tree
    }
  }
}

object TreeChecker extends TreeChecker