package dotty.tools.dotc
package core

import Annotations._
import Contexts._
import Flags._
import Symbols._
import Types._
import transform.SymUtils._

/** Defines operations on nullable types and tree. */
object NullOpsDecorator:

  extension (self: Type)
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
          case tp @ TypeBounds(lo, hi) =>
            tp.derivedTypeBounds(strip(lo), strip(hi))
          case tp => tp
        }
        if tpStripped ne tpWiden then tpStripped else tp

      if ctx.explicitNulls then strip(self) else self
    }

    /** Strips `|Null` from the return type of a Java method,
     *  replacing it with a `@CanEqualNull` annotation
     */
    def replaceOrNull(using Context): Type =
      // Since this method should only be called on types from Java,
      // handling these cases is enough.
      def recur(tp: Type): Type = tp match
        case tp @ OrType(lhs, rhs) if rhs.isNullType =>
          AnnotatedType(recur(lhs), Annotation(defn.CanEqualNullAnnot))
        case tp: AndOrType =>
          tp.derivedAndOrType(recur(tp.tp1), recur(tp.tp2))
        case tp @ AppliedType(tycon, targs) =>
          tp.derivedAppliedType(tycon, targs.map(recur))
        case mptp: MethodOrPoly =>
          mptp.derivedLambdaType(resType = recur(mptp.resType))
        case _ => tp
      if ctx.explicitNulls then recur(self) else self

    /** Is self (after widening and dealiasing) a type of the form `T | Null`? */
    def isNullableUnion(using Context): Boolean = {
      val stripped = self.stripNull
      stripped ne self
    }
  end extension

  import ast.tpd._

  extension (tree: Tree)

    // cast the type of the tree to a non-nullable type
    def castToNonNullable(using Context): Tree = tree.typeOpt match
      case OrNull(tp) => tree.cast(tp)
      case _          => tree

    def tryToCastToCanEqualNull(using Context): Tree =
      // return the tree directly if not at Typer phase
      if !(ctx.explicitNulls && ctx.phase.isTyper) then return tree

      val sym = tree.symbol
      val tp = tree.tpe

      if !ctx.mode.is(Mode.UnsafeJavaReturn)
        || !sym.is(JavaDefined)
        || sym.isNoValue
        || !sym.isTerm
        || tp.isError then
        return tree

      tree match
        case _: Apply if sym.is(Method) =>
          val tp2 = tp.replaceOrNull
          if tp ne tp2 then
            tree.cast(tp2)
          else tree
        case _: Select | _: Ident if !sym.is(Method) =>
          val tpw = tp.widen
          val tp2 = tpw.replaceOrNull
          if tpw ne tp2 then
            tree.cast(tp2)
          else tree
        case _ => tree

end NullOpsDecorator
