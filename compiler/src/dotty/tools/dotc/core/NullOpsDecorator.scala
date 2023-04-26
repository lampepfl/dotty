package dotty.tools.dotc
package core

import Contexts._
import Types._

/** Defines operations on nullable types and tree. */
object NullOpsDecorator:

  extension (self: Type)
    def stripFlexible(using Context): Type = {
      self match {
        case FlexibleType(tp) => tp
        case _ => self
      }
    }
    /** Syntactically strips the nullability from this type.
     *  If the type is `T1 | ... | Tn`, and `Ti` references to `Null`,
     *  then return `T1 | ... | Ti-1 | Ti+1 | ... | Tn`.
     *  If this type isn't (syntactically) nullable, then returns the type unchanged.
     *  The type will not be changed if explicit-nulls is not enabled.
     */
    def stripNull(using Context): Type = {
      def strip(tp: Type): Type =
        val tpWiden = tp.widenDealias
        val tpStripped = tpWiden match {
          case tp @ OrType(lhs, rhs) =>
            val llhs = strip(lhs)
            val rrhs = strip(rhs)
            if rrhs.isNullType then llhs
            else if llhs.isNullType then rrhs
            else tp.derivedOrType(llhs, rrhs)
          case tp @ AndType(tp1, tp2) =>
            // We cannot `tp.derivedAndType(strip(tp1), strip(tp2))` directly,
            // since `stripNull((A | Null) & B)` would produce the wrong
            // result `(A & B) | Null`.
            val tp1s = strip(tp1)
            val tp2s = strip(tp2)
            if (tp1s ne tp1) && (tp2s ne tp2) then
              tp.derivedAndType(tp1s, tp2s)
            else tp
          case tp @ FlexibleType(tp1) => strip(tp1)
          case tp @ TypeBounds(lo, hi) =>
            tp.derivedTypeBounds(strip(lo), strip(hi))
          case tp => tp
        }
        if tpStripped ne tpWiden then tpStripped else tp

      if ctx.explicitNulls then strip(self) else self
    }

    /** Is self (after widening and dealiasing) a type of the form `T | Null`? */
    def isNullableUnion(using Context): Boolean = {
      val stripped = self.stripNull
      stripped ne self
    }
  end extension

  import ast.tpd._

  extension (self: Tree)
    // cast the type of the tree to a non-nullable type
    def castToNonNullable(using Context): Tree = self.typeOpt match {
      case OrNull(tp) => self.cast(tp)
      case _ => self
    }
end NullOpsDecorator
