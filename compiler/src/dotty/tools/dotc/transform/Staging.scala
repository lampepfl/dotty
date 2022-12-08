package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.util.SrcPos
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.transform.TreeMapWithStages._



/** Checks that the Phase Consistency Principle (PCP) holds and heals types.
 *
 *  Type healing consists in transforming a phase inconsistent type `T` into `${ implicitly[Type[T]] }`.
 */
class Staging extends MacroTransform {
  import tpd._

  override def phaseName: String = Staging.name

  override def description: String = Staging.description

  override def runsAfter: Set[String] = Set(Inlining.name)

  override def allowsImplicitSearch: Boolean = true

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    if (ctx.phase <= splicingPhase) {
      // Recheck that PCP holds but do not heal any inconsistent types as they should already have been heald
      tree match {
        case PackageDef(pid, _) if tree.symbol.owner == defn.RootClass =>
          val stagingCtx = freshStagingContext
          val checker = new PCPCheckAndHeal(stagingCtx) {
            override protected def tryHeal(sym: Symbol, tp: TypeRef, pos: SrcPos)(using Context): TypeRef = {
              def symStr =
                if (sym.is(ModuleClass)) sym.sourceModule.show
                else i"${sym.name}.this"
              val errMsg = s"\nin ${ctx.owner.fullName}"
              assert(
                ctx.owner.hasAnnotation(defn.QuotedRuntime_SplicedTypeAnnot) ||
                (sym.isType && levelOf(sym) > 0),
                em"""access to $symStr from wrong staging level:
                    | - the definition is at level ${levelOf(sym)},
                    | - but the access is at level $level.$errMsg""")

              tp
            }
          }
          checker.transform(tree)
        case _ =>
      }

      tree.tpe match {
        case tpe @ TypeRef(prefix, _) if tpe.typeSymbol.isTypeSplice =>
          // Type splices must have a know term ref, usually to an implicit argument
          // This is mostly intended to catch `quoted.Type[T]#splice` types which should just be `T`
          assert(prefix.isInstanceOf[TermRef] || prefix.isInstanceOf[ThisType], prefix)
        case _ =>
          // OK
      }
    }

  override def run(using Context): Unit =
    if (ctx.compilationUnit.needsStaging) super.run(using freshStagingContext)

  protected def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      new PCPCheckAndHeal(ctx).transform(tree)
  }
}


object Staging {
  val name: String = "staging"
  val description: String = "check staging levels and heal staged types"
}
