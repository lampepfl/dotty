package dotty.tools.dotc
package core

import Periods.*
import SymDenotations.*
import Contexts.*
import Types.*
import Symbols.*
import Denotations.*
import Phases.*

object DenotTransformers {

  /** A transformer group contains a sequence of transformers,
   *  ordered by the phase where they apply. Transformers are added
   *  to a group via `install`.
   */

  /** A transformer transforms denotations at a given phase */
  trait DenotTransformer extends Phase {

    /** The last phase during which the transformed denotations are valid */
    def lastPhaseId(using Context): Int = ctx.base.nextDenotTransformerId(id + 1)

    /** The validity period of the transformed denotations in the given context */
    def validFor(using Context): Period =
      Period(ctx.runId, id + 1, lastPhaseId)

    /** The transformation method */
    def transform(ref: SingleDenotation)(using Context): SingleDenotation

    override def isRunnable(using Context) = super.isRunnable && !ctx.usedBestEffortTasty
  }

  /** A transformer that only transforms the info field of denotations */
  trait InfoTransformer extends DenotTransformer {

    def transformInfo(tp: Type, sym: Symbol)(using Context): Type

    def transform(ref: SingleDenotation)(using Context): SingleDenotation = {
      val sym = ref.symbol
      if (sym.exists && !infoMayChange(sym)) ref
      else {
        val info1 = transformInfo(ref.info, ref.symbol)
        if (info1 eq ref.info) ref
        else ref match {
          case ref: SymDenotation =>
            ref.copySymDenotation(info = info1).copyCaches(ref, ctx.phase.next)
          case _ =>
            ref.derivedSingleDenotation(ref.symbol, info1)
        }
      }
    }

    /** Denotations with a symbol where `infoMayChange` is false are guaranteed to be
     *  unaffected by this transform, so `transformInfo` need not be run. This
     *  can save time, and more importantly, can help avoid forcing symbol completers.
     */
    protected def infoMayChange(sym: Symbol)(using Context): Boolean = true
  }

  /** A transformer that only transforms SymDenotations.
   *  Note: Infos of non-sym denotations are left as is. So the transformer should
   *  be used before erasure only if this is not a problem. After erasure, all
   *  denotations are SymDenotations, so SymTransformers can be used freely.
   */
  trait SymTransformer extends DenotTransformer {

    def transformSym(sym: SymDenotation)(using Context): SymDenotation

    def transform(ref: SingleDenotation)(using Context): SingleDenotation = ref match {
      case ref: SymDenotation => transformSym(ref)
      case _ => ref
    }
  }

  /** A `DenotTransformer` trait that has the identity as its `transform` method.
   *  You might want to inherit from this trait so that new denotations can be
   *  installed using `installAfter` and `enteredAfter` at the end of the phase.
   */
  trait IdentityDenotTransformer extends DenotTransformer {
    def transform(ref: SingleDenotation)(using Context): SingleDenotation = ref
  }
}
