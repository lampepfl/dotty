package dotty.tools.dotc
package core

import Periods._
import SymDenotations._
import Contexts._
import Types._
import Denotations._
import Phases._
import java.lang.AssertionError
import dotty.tools.dotc.util.DotClass

object DenotTransformers {

  /** A transformer group contains a sequence of transformers,
   *  ordered by the phase where they apply. Transformers are added
   *  to a group via `install`.
   */

  /** A transformer transforms denotations at a given phase */
  trait DenotTransformer extends Phase {

    /** The last phase during which the transformed denotations are valid */
    def lastPhaseId(implicit ctx: Context) = ctx.nextTransformerId(id + 1)

    /** The validity period of the transformer in the given context */
    def validFor(implicit ctx: Context): Period =
      Period(ctx.runId, id, lastPhaseId)

    /** The transformation method */
    def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation
  }
}
