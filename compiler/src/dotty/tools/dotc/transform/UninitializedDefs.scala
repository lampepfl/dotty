package dotty.tools.dotc
package transform

import core._
import Contexts._
import Flags._
import Symbols._
import MegaPhase.MiniPhase
import StdNames.nme
import ast.tpd

/** This phase replaces `compiletime.uninitialized` on the right hand side of a mutable field definition by `_`.
 *  This avoids a
 *  ```scala
 *  "@compileTimeOnly("`uninitialized` can only be used as the right hand side of a mutable field definition")`
 *  ```
 *  error in Erasure and communicates to Constructors that the variable does not have an initializer.
 *
 *  @syntax markdown
 */
class UninitializedDefs extends MiniPhase:
  import tpd._

  override def phaseName: String = UninitializedDefs.name

  override def description: String = UninitializedDefs.description

  override def transformValDef(tree: ValDef)(using Context): Tree =
    if !hasUninitializedRHS(tree) then tree
    else cpy.ValDef(tree)(rhs = cpy.Ident(tree.rhs)(nme.WILDCARD).withType(tree.tpt.tpe))

  private def hasUninitializedRHS(tree: ValOrDefDef)(using Context): Boolean =
    def recur(rhs: Tree): Boolean = rhs match
      case rhs: RefTree =>
        rhs.symbol == defn.Compiletime_uninitialized
        && tree.symbol.is(Mutable) && tree.symbol.owner.isClass
      case closureDef(ddef) if tree.tpt.tpe.dealias.isContextFunctionType =>
        recur(ddef.rhs)
      case _ =>
        false
    recur(tree.rhs)

end UninitializedDefs

object UninitializedDefs:
  val name: String = "uninitialized"
  val description: String = "eliminates `compiletime.uninitialized`"
end UninitializedDefs
