package dotty.tools
package dotc
package transform

import core._
import Contexts._, Symbols._, Types._, Annotations._, Constants._, Phases._
import StdNames.nme
import ast.untpd
import ast.tpd._
import config.Config

object ContextFunctionResults:

  /** Annotate methods that have context function result types directly matched by context
   *  closures on their right-hand side. Parameters to such closures will be integrated
   *  as additional method parameters in erasure.
   *
   *  A @ContextResultCount(n) annotation means that the method's result type
   *  consists of a string of `n` nested context closures.
   */
  def annotateContextResults(mdef: DefDef)(using Context): Unit =
    def contextResultCount(rhs: Tree, tp: Type): Int = tp match
      case ContextFunctionType(_, resTpe, _) =>
        rhs match
          case closureDef(meth) => 1 + contextResultCount(meth.rhs, resTpe)
          case _ => 0
      case _ => 0

    val meth = mdef.symbol

    // Disable context result annotations for anonymous functions
    // and for implementations of PolyFunction
    def disabled =
      meth.isAnonymousFunction
      || meth.name == nme.apply
          && meth.owner.isAnonymousClass
          && meth.owner.info.parents.exists(_.isRef(defn.PolyFunctionClass))

    val count = contextResultCount(mdef.rhs, mdef.tpt.tpe)

    if Config.flattenContextFunctionResults && count != 0 && !disabled then
      val countAnnot = Annotation(defn.ContextResultCountAnnot, Literal(Constant(count)), mdef.symbol.span)
      mdef.symbol.addAnnotation(countAnnot)
  end annotateContextResults

  /** The argument of a ContextResultCount annotation, or 0 if none exists.
   *  See PostTyper#annotateContextResults.
   */
  def contextResultCount(sym: Symbol)(using Context): Int =
    sym.getAnnotation(defn.ContextResultCountAnnot) match
      case Some(annot) =>
        val ast.Trees.Literal(Constant(crCount: Int)) :: Nil = annot.arguments: @unchecked
        crCount
      case none => 0

  /** True iff `ContextResultCount` is not zero and all context functions in the result
   *  type are erased.
   */
  def contextResultsAreErased(sym: Symbol)(using Context): Boolean =
    def allErased(tp: Type): Boolean = tp.dealias match
      case ContextFunctionType(_, resTpe, isErased) => isErased && allErased(resTpe)
      case _ => true
    contextResultCount(sym) > 0 && allErased(sym.info.finalResultType)

  /** Turn the first `crCount` context function types in the result type of `tp`
   *  into the curried method types.
   */
  def integrateContextResults(tp: Type, crCount: Int)(using Context): Type =
    if crCount == 0 then tp
    else tp match
      case ExprType(rt) =>
        integrateContextResults(rt, crCount)
      case tp: MethodOrPoly =>
        tp.derivedLambdaType(resType = integrateContextResults(tp.resType, crCount))
      case ContextFunctionType(argTypes, resType, isErased) =>
        val methodType: MethodTypeCompanion =
          if isErased then ErasedMethodType else MethodType
        methodType(argTypes, integrateContextResults(resType, crCount - 1))

  /** The total number of parameters of method `sym`, not counting
   *  erased parameters, but including context result parameters.
   */
  def totalParamCount(sym: Symbol)(using Context): Int =

    def contextParamCount(tp: Type, crCount: Int): Int =
      if crCount == 0 then 0
      else
        val ContextFunctionType(params, resTpe, isErased) = tp: @unchecked
        val rest = contextParamCount(resTpe, crCount - 1)
        if isErased then rest else params.length + rest

    def normalParamCount(tp: Type): Int = tp.widenExpr.stripPoly match
      case mt @ MethodType(pnames) =>
        val rest = normalParamCount(mt.resType)
        if mt.isErasedMethod then rest else pnames.length + rest
      case _ => contextParamCount(tp, contextResultCount(sym))

    normalParamCount(sym.info)
  end totalParamCount

  /** The `depth` levels nested context function type in the result type of `meth` */
  def contextFunctionResultTypeAfter(meth: Symbol, depth: Int)(using Context) =
    def recur(tp: Type, n: Int): Type =
      if n == 0 then tp
      else tp match
        case ContextFunctionType(_, resTpe, _) => recur(resTpe, n - 1)
    recur(meth.info.finalResultType, depth)

  /** Should selection `tree` be eliminated since it refers to an `apply`
   *  node of a context function type whose parameters will end up being
   *  integrated in the  preceding method?
   *  @param `n` the select nodes seen in previous recursive iterations of this method
   */
  def integrateSelect(tree: untpd.Tree, n: Int = 0)(using Context): Boolean =
    if ctx.erasedTypes then
      atPhase(erasurePhase)(integrateSelect(tree, n))
    else tree match
      case Select(qual, name) =>
        if name == nme.apply && defn.isContextFunctionClass(tree.symbol.maybeOwner) then
          integrateSelect(qual, n + 1)
        else
          n > 0 && contextResultCount(tree.symbol) >= n
      case Ident(name) =>
        n > 0 && contextResultCount(tree.symbol) >= n
      case Apply(fn, args) =>
        integrateSelect(fn, n)
      case TypeApply(fn, _) =>
        integrateSelect(fn, n)
      case Block(_, expr) =>
        integrateSelect(expr, n)
      case Inlined(_, _, expr) =>
        integrateSelect(expr, n)
      case _ =>
        false

end ContextFunctionResults