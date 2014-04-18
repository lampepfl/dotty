package dotty.tools.dotc
package transform

import TreeTransforms._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Constants._
import core.StdNames._
import core.transform.Erasure.isUnboundedGeneric
import typer.ErrorReporting._
import ast.Trees._
import Erasure.Boxing.box

/** This transform normalizes type tests and type casts,
 *  also replacing type tests with singleton argument type with refference equality check
 *  Any remaining type tests
 *   - use the object methods $isInstanceOf and $asInstanceOf
 *   - have a reference type as receiver
 *   - can be translated directly to machine instructions
 */
class TypeTestsCasts extends TreeTransform {
  import ast.tpd._

  override def name: String = "typeTestsCasts"

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = ctx.traceIndented(s"transforming ${tree.show}", show = true) {
    tree.fun match {
      case fun @ Select(qual, selector) =>
        val sym = tree.symbol

        def isPrimitive(tp: Type) = tp.classSymbol.isPrimitiveValueClass

        def derivedTree(qual1: Tree, sym: Symbol, tp: Type) =
          cpy.TypeApply(tree, Select(qual1, sym) withPos qual.pos, List(TypeTree(tp)))

        def qualCls = qual.tpe.classSymbol

        def transformIsInstanceOf(expr:Tree, argType: Type): Tree = {
          if (expr.tpe <:< argType)
            Literal(Constant(true)) withPos tree.pos
          else if (qualCls.isPrimitiveValueClass) {
            val argCls = argType.classSymbol
            if (argCls.isPrimitiveValueClass) Literal(Constant(qualCls == argCls))
            else errorTree(tree, "isInstanceOf cannot test if value types are references")
          }
          else argType.dealias match {
            case _: SingletonType =>
              val cmpOp = if (argType derivesFrom defn.AnyValClass) defn.Any_equals else defn.Object_eq
              Apply(Select(expr, cmpOp), singleton(argType) :: Nil)
            case AndType(tp1, tp2) =>
              evalOnce(expr) { fun =>
                val erased1 = transformIsInstanceOf(fun, tp1)
                val erased2 = transformIsInstanceOf(fun, tp2)
                erased1 match {
                  case Literal(Constant(true)) => erased2
                  case _ =>
                    erased2 match {
                      case Literal(Constant(true)) => erased1
                      case _ => mkAnd(erased1, erased2)
                    }
                }
              }
            case defn.MultiArrayType(elem, ndims) if isUnboundedGeneric(elem) =>
              def isArrayTest(arg: Tree) =
                runtimeCall(nme.isArray, arg :: Literal(Constant(ndims)) :: Nil)
              if (ndims == 1) isArrayTest(qual)
              else evalOnce(qual) { qual1 =>
                mkAnd(derivedTree(qual1, defn.Any_isInstanceOf, qual1.tpe), isArrayTest(qual1))
              }
            case _ =>
              derivedTree(expr, defn.Any_isInstanceOf, argType)
          }
        }

        def transformAsInstanceOf(argType: Type): Tree = {
          if (qual.tpe <:< argType)
            Typed(qual, tree.args.head)
          else if (qualCls.isPrimitiveValueClass) {
            val argCls = argType.classSymbol
            if (argCls.isPrimitiveValueClass) primitiveConversion(qual, argCls)
            else derivedTree(box(qual), defn.Any_asInstanceOf, argType)
          }
          else
            derivedTree(qual, defn.Any_asInstanceOf, argType)
        }

        if (sym eq defn.Any_isInstanceOf)
          transformIsInstanceOf(qual, tree.args.head.tpe)
        else if (defn.asInstanceOfMethods contains sym)
          transformAsInstanceOf(tree.args.head.tpe)
        else tree

      case _ =>
        tree
    }
  }
}