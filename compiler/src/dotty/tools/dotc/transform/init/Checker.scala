package dotty.tools.dotc
package transform
package init


import dotty.tools.dotc._
import ast.tpd

import dotty.tools.dotc.core._
import Contexts._
import Types._
import Symbols._
import StdNames._

import dotty.tools.dotc.transform._
import Phases._


import scala.collection.mutable
import util.EqHashMap


class Checker extends Phase {
  import tpd._

  val phaseName = "initChecker"

  private val semantic = new Semantic

  override val runsAfter = Set(Pickler.name)

  override def isEnabled(using Context): Boolean =
    super.isEnabled && ctx.settings.YcheckInit.value

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    units.foreach { unit => traverser.traverse(unit.tpdTree) }
    super.runOn(units)

  val traverser = new TreeTraverser {
    override def traverse(tree: Tree)(using Context): Unit =
      traverseChildren(tree)
      tree match {
        case tdef: MemberDef =>
          // self-type annotation ValDef has no symbol
          if tdef.name != nme.WILDCARD then
            tdef.symbol.defTree = tree
        case _ =>
      }
  }

  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    unit.tpdTree.foreachSubTree {
      case tdef: TypeDef if tdef.isClassDef =>
        transformTypeDef(tdef)

      case _ =>
    }
  }


  private def transformTypeDef(tree: TypeDef)(using Context): tpd.Tree = {
    val cls = tree.symbol.asClass
    val instantiable: Boolean =
      cls.is(Flags.Module) ||
      !cls.isOneOf(Flags.AbstractOrTrait) && {
        // see `Checking.checkInstantiable` in typer
        val tp = cls.appliedRef
        val stp = SkolemType(tp)
        val selfType = cls.givenSelfType.asSeenFrom(stp, cls)
        !selfType.exists || stp <:< selfType
      }

    // A concrete class may not be instantiated if the self type is not satisfied
    if (instantiable && cls.enclosingPackageClass != defn.StdLibPatchesPackage.moduleClass) {
      import semantic._
      val tpl = tree.rhs.asInstanceOf[Template]
      val thisRef = ThisRef(cls).ensureExists

      val paramValues = tpl.constr.termParamss.flatten.map(param => param.symbol -> Hot).toMap

      // A wrapper for eval that uses two-cache method to compute fix-point after evaluating a class body
      def fixPointEval(expr: Tree, thisV: Addr, klass: ClassSymbol, inputCache: evalCache, outputCache: evalCache): Result = {
        import semantic.Heap._
        given Promoted = Promoted.empty
        given Trace = Trace.empty
        given Env = Env(paramValues)
        given (evalCache, evalCache) = (inputCache, outputCache)
        val res = eval(expr, thisV, klass)
        if !res.errors.isEmpty then res
        else if inputCache.equal(outputCache) then
          inputCache.commitEvalCache(thisV)
          res
        else
          thisV.emptyField()
          fixPointEval(expr, thisV, klass, outputCache, new evalCache)
      }

      val res = fixPointEval(tpl, thisRef, cls, new evalCache, new evalCache)
      res.errors.foreach(_.issue)
    }

    tree
  }
}
