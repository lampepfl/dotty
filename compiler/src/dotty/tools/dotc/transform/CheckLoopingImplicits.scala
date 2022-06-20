package dotty.tools.dotc
package transform

import core.*
import MegaPhase.MiniPhase
import Contexts.*, Types.*, Symbols.*, SymDenotations.*, Flags.*
import ast.*
import Decorators.*


object CheckLoopingImplicits:
  val name: String = "checkLoopingImplicits"
  val description: String = "check that implicit defs do not call themselves in an infinite loop"

/** Checks that implicit defs do not call themselves in an infinite loop */
class CheckLoopingImplicits extends MiniPhase:
  thisPhase =>
  import tpd._

  override def phaseName: String = CheckLoopingImplicits.name

  override def description: String = CheckLoopingImplicits.description

  override def transformValDef(mdef: ValDef)(using Context): Tree =
    transform(mdef)

  override def transformDefDef(mdef: DefDef)(using Context): Tree =
    transform(mdef)

  def transform(mdef: ValOrDefDef)(using Context): Tree =
    val sym = mdef.symbol

    def checkNotSelfRef(t: RefTree) =
      if t.symbol eq sym then
          report.warning(
              em"""Infinite loop in function body
                  |${mdef.rhs}""",
              mdef.rhs.srcPos
            )

    def checkNotLooping(t: Tree): Unit = t match
      case t: Ident =>
        checkNotSelfRef(t)
      case t @ Select(qual, _) =>
        checkNotSelfRef(t)
        checkNotLooping(qual)
      case Apply(fn, args) =>
        checkNotLooping(fn)
        fn.tpe.widen match
          case mt: MethodType
               // Boolean && and || aren't defined with by-name parameters
               // and therefore their type isn't an ExprType, so we exempt them by symbol name
               if t.symbol != defn.Boolean_&& && t.symbol != defn.Boolean_|| =>
            args.lazyZip(mt.paramInfos).foreach { (arg, pinfo) =>
              if !pinfo.isInstanceOf[ExprType] then checkNotLooping(arg)
            }
          case _ =>
      case TypeApply(fn, _) =>
        checkNotLooping(fn)
      case Block(stats, expr) =>
        stats.foreach(checkNotLooping)
        checkNotLooping(expr)
      case Typed(expr, _) =>
        checkNotLooping(expr)
      case Assign(lhs, rhs) =>
        checkNotLooping(lhs)
        checkNotLooping(rhs)
      case If(cond, _, _) =>
        checkNotLooping(cond)
      case Match(selector, _) =>
        checkNotLooping(selector)
      case Labeled(_, expr) =>
        checkNotLooping(expr)
      case Return(expr, _) =>
        checkNotLooping(expr)
      case WhileDo(cond, _) =>
        checkNotLooping(cond)
      case Try(block, _, finalizer) =>
        checkNotLooping(block)
        checkNotLooping(finalizer)
      case SeqLiteral(elems, _) =>
        elems.foreach(checkNotLooping)
      case t: ValDef =>
        checkNotLooping(t.rhs)
      case _ =>

    if sym.isOneOf(GivenOrImplicit | Lazy | ExtensionMethod) || mdef.isInstanceOf[ValDef] then
      checkNotLooping(mdef.rhs)
    mdef
  end transform
end CheckLoopingImplicits
