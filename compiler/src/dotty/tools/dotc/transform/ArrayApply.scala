package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts._
import Symbols._
import Types._
import StdNames._
import ast.Trees._
import dotty.tools.dotc.ast.tpd
import util.Lst; // import Lst.::
import util.Lst.{NIL, +:, toLst}

import scala.reflect.ClassTag


/** This phase rewrites calls to `Array.apply` to a direct instantiation of the array in the bytecode.
 *
 *  Transforms `scala.Array.apply([....])` and `scala.Array.apply(..., [....])` into `[...]`
 */
class ArrayApply extends MiniPhase {
  import tpd._

  override def phaseName: String = "arrayApply"

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    if (tree.symbol.name == nme.apply && tree.symbol.owner == defn.ArrayModule.moduleClass) // Is `Array.apply`
      tree.args match {
        case Lst(StripAscription(Apply(wrapRefArrayMeth, Lst(seqLit: tpd.JavaSeqLiteral))), ct)
            if defn.WrapArrayMethods().contains(wrapRefArrayMeth.symbol) && elideClassTag(ct) =>
          seqLit

        case Lst(elem0, StripAscription(Apply(wrapRefArrayMeth, Lst(seqLit: tpd.JavaSeqLiteral))))
            if defn.WrapArrayMethods().contains(wrapRefArrayMeth.symbol) =>
          tpd.JavaSeqLiteral(elem0 :: seqLit.elems, seqLit.elemtpt)

        case _ =>
          tree
      }

    else tree

  /** Only optimize when classtag if it is one of
   *  - `ClassTag.apply(classOf[XYZ])`
   *  - `ClassTag.apply(java.lang.XYZ.Type)` for boxed primitives `XYZ``
   *  - `ClassTag.XYZ` for primitive types
   */
  private def elideClassTag(ct: Tree)(using Context): Boolean = ct match {
    case Apply(_, Lst(rc)) if ct.symbol == defn.ClassTagModule_apply =>
      rc match {
        case _: Literal => true // ClassTag.apply(classOf[XYZ])
        case rc: RefTree if rc.name == nme.TYPE_ =>
          // ClassTag.apply(java.lang.XYZ.Type)
          defn.ScalaBoxedClasses().contains(rc.symbol.maybeOwner.companionClass)
        case _ => false
      }
    case Apply(ctm: RefTree, _) if ctm.symbol.maybeOwner.companionModule == defn.ClassTagModule =>
      // ClassTag.XYZ
      nme.ScalaValueNames.contains(ctm.name)
    case _ => false
  }

  object StripAscription {
    def unapply(tree: Tree)(using Context): Some[Tree] = tree match {
      case Typed(expr, _) => unapply(expr)
      case _ => Some(tree)
    }
  }
}
