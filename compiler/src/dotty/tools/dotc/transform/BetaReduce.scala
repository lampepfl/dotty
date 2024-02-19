package dotty.tools
package dotc
package transform

import core._
import Flags._
import MegaPhase._
import Symbols._, Contexts._, Types._, Decorators._
import StdNames.nme
import ast.TreeTypeMap

import scala.collection.mutable.ListBuffer

/** Rewrite an application
 *
 *    (([X1, ..., Xm] => (x1, ..., xn) => b): T)[T1, ..., Tm](y1, ..., yn)
 *
 *  where
 *
 *    - all yi are pure references without a prefix
 *    - the closure can also be contextual or erased, but cannot be a SAM type
 *    - the type parameters Xi and type arguments Ti are optional
 *    - the type ascription ...: T is optional
 *
 *  to
 *
 *    [xi := yi]b
 *
 *  This is more limited than beta reduction in inlining since it only works for simple variables `yi`.
 *  It is more general since it also works for type-ascripted closures.
 *
 *  A typical use case is eliminating redundant closures for blackbox macros that
 *  return context functions. See i6375.scala.
 */
class BetaReduce extends MiniPhase:
  import ast.tpd._

  override def phaseName: String = BetaReduce.name

  override def description: String = BetaReduce.description

  override def transformApply(app: Apply)(using Context): Tree =
    val app1 = BetaReduce(app)
    if app1 ne app then report.log(i"beta reduce $app -> $app1")
    app1

object BetaReduce:
  import ast.tpd._

  val name: String = "betaReduce"
  val description: String = "reduce closure applications"

  /** Rewrite an application
   *
   *    ((x1, ..., xn) => b)(e1, ..., en)
   *
   *  to
   *
   *    val/def x1 = e1; ...; val/def xn = en; b
   *
   *  where `def` is used for call-by-name parameters. However, we shortcut any NoPrefix
   *  refs among the ei's directly without creating an intermediate binding.
   *
   *  Similarly, rewrites type applications
   *
   *    ([X1, ..., Xm] => (x1, ..., xn) => b).apply[T1, .., Tm](e1, ..., en)
   *
   *  to
   *
   *    type X1 = T1; ...; type Xm = Tm;val/def x1 = e1; ...; val/def xn = en; b
   *
   *  This beta-reduction preserves the integrity of `Inlined` tree nodes.
   */
  def apply(tree: Tree)(using Context): Tree =
    val bindingsBuf = new ListBuffer[DefTree]
    def recur(fn: Tree, argss: List[List[Tree]]): Option[Tree] = fn match
      case Block((ddef : DefDef) :: Nil, closure: Closure) if ddef.symbol == closure.meth.symbol =>
        Some(reduceApplication(ddef, argss, bindingsBuf))
      case Block((TypeDef(_, template: Template)) :: Nil, Typed(Apply(Select(New(_), _), _), _)) if template.constr.rhs.isEmpty =>
        template.body match
          case (ddef: DefDef) :: Nil => Some(reduceApplication(ddef, argss, bindingsBuf))
          case _ => None
      case Block(stats, expr) if stats.forall(isPureBinding) =>
        recur(expr, argss).map(cpy.Block(fn)(stats, _))
      case fn @ Inlined(call, bindings, expr) if bindings.forall(isPureBinding) =>
        recur(expr, argss).map(cpy.Inlined(fn)(call, bindings, _))
      case Typed(expr, tpt) =>
        recur(expr, argss)
      case TypeApply(Select(expr, nme.asInstanceOfPM), List(tpt)) =>
        recur(expr, argss)
      case _ => None
    tree match
      case Apply(Select(fn, nme.apply), args) if defn.isFunctionNType(fn.tpe) =>
        recur(fn, List(args)) match
          case Some(reduced) =>
            seq(bindingsBuf.result(), reduced).withSpan(tree.span)
          case None =>
            tree
      case Apply(TypeApply(Select(fn, nme.apply), targs), args) if fn.tpe.typeSymbol eq dotc.core.Symbols.defn.PolyFunctionClass =>
        recur(fn, List(targs, args)) match
          case Some(reduced) =>
            seq(bindingsBuf.result(), reduced).withSpan(tree.span)
          case None =>
            tree
      case _ =>
        tree

  /** Beta-reduces a call to `ddef` with arguments `args` and registers new bindings */
  def reduceApplication(ddef: DefDef, argss: List[List[Tree]], bindings: ListBuffer[DefTree])(using Context): Tree =
    val (targs, args) = argss.flatten.partition(_.isType)
    val tparams = ddef.leadingTypeParams
    val vparams = ddef.termParamss.flatten

    val targSyms =
      for (targ, tparam) <- targs.zip(tparams) yield
        targ.tpe.dealias match
          case ref @ TypeRef(NoPrefix, _) =>
            ref.symbol
          case _ =>
            val binding = TypeDef(newSymbol(ctx.owner, tparam.name, EmptyFlags, TypeAlias(targ.tpe), coord = targ.span)).withSpan(targ.span)
            bindings += binding
            binding.symbol

    val argSyms =
      for (arg, param) <- args.zip(vparams) yield
        arg.tpe.dealias match
          case ref @ TermRef(NoPrefix, _) if isPurePath(arg) =>
            ref.symbol
          case _ =>
            val flags = Synthetic | (param.symbol.flags & Erased)
            val tpe =
              if arg.tpe.isBottomType then param.tpe.widenTermRefExpr
              else if arg.tpe.dealias.isInstanceOf[ConstantType] then arg.tpe.dealias
              else arg.tpe.widen
            val binding = ValDef(newSymbol(ctx.owner, param.name, flags, tpe, coord = arg.span), arg).withSpan(arg.span)
            if !(tpe.isInstanceOf[ConstantType] && isPureExpr(arg)) then
              bindings += binding
            binding.symbol

    val expansion = TreeTypeMap(
      oldOwners = ddef.symbol :: Nil,
      newOwners = ctx.owner :: Nil,
      substFrom = (tparams ::: vparams).map(_.symbol),
      substTo = targSyms ::: argSyms
    ).transform(ddef.rhs)

    val expansion1 = new TreeMap {
      override def transform(tree: Tree)(using Context) = tree.tpe.widenTermRefExpr match
        case ConstantType(const) if isPureExpr(tree) => cpy.Literal(tree)(const)
        case _ => super.transform(tree)
    }.transform(expansion)

    expansion1
