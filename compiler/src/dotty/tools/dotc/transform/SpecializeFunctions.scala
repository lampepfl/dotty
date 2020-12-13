package dotty.tools.dotc
package transform

import ast.Trees._, ast.tpd, core._
import Contexts._, Types._, Decorators._, Symbols._, DenotTransformers._
import SymDenotations._, Scopes._, StdNames._, NameOps._, Names._
import MegaPhase.MiniPhase

import scala.collection.mutable

/** Specializes classes that inherit from `FunctionN` where there exists a
 *  specialized form.
 */
class SpecializeFunctions extends MiniPhase {
  import ast.tpd._
  val phaseName = "specializeFunctions"
  override def runsAfter = Set(ElimByName.name)

  override def isEnabled(using Context): Boolean =
    !ctx.settings.scalajs.value

  /** Create forwarders from the generic applys to the specialized ones.
   */
  override def transformDefDef(ddef: DefDef)(using Context) = {
    if ddef.name != nme.apply
       || ddef.vparamss.length != 1
       || ddef.vparamss.head.length > 2
       || !ctx.owner.isClass
    then
      return ddef

    val sym = ddef.symbol
    val cls = ctx.owner.asClass

    var specName: Name = null

    def isSpecializable = {
      val paramTypes = ddef.vparamss.head.map(_.symbol.info)
      val retType = sym.info.finalResultType
      specName = nme.apply.specializedFunction(retType, paramTypes)
      defn.isSpecializableFunction(cls, paramTypes, retType)
    }

    if (sym.is(Flags.Deferred) || !isSpecializable) return ddef

    val specializedApply = newSymbol(
        cls,
        specName,
        sym.flags | Flags.Synthetic,
        sym.info
      ).entered

    val specializedDecl =
      DefDef(specializedApply.asTerm, vparamss => {
        ddef.rhs
          .changeOwner(ddef.symbol, specializedApply)
          .subst(ddef.vparamss.head.map(_.symbol), vparamss.head.map(_.symbol))
      })

    // create a forwarding to the specialized apply
    val args = ddef.vparamss.head.map(vparam => ref(vparam.symbol))
    val rhs = This(cls).select(specializedApply).appliedToArgs(args)
    val ddef1 = cpy.DefDef(ddef)(rhs = rhs)
    Thicket(ddef1, specializedDecl)
  }

  /** Dispatch to specialized `apply`s in user code when available */
  override def transformApply(tree: Apply)(using Context) =
    tree match {
      case Apply(fun: NameTree, args) if fun.name == nme.apply && args.size <= 3 =>
        val argTypes = fun.tpe.widen.firstParamTypes.map(_.widenSingleton.dealias)
        val retType  = tree.tpe.widenSingleton.dealias
        val isSpecializable =
          defn.isSpecializableFunction(
            fun.symbol.owner.asClass,
            argTypes,
            retType
          )

        if (!isSpecializable || argTypes.exists(_.isInstanceOf[ExprType])) return tree

        val specializedApply = nme.apply.specializedFunction(retType, argTypes)
        val newSel = fun match {
          case Select(qual, _) =>
            qual.select(specializedApply)
          case _ =>
            (fun.tpe: @unchecked) match {
              case TermRef(prefix: ThisType, name) =>
                tpd.This(prefix.cls).select(specializedApply)
              case TermRef(prefix: NamedType, name) =>
                tpd.ref(prefix).select(specializedApply)
            }
        }

        newSel.appliedToArgs(args)

      case _ => tree
    }

  private def derivesFromFn012(cls: ClassSymbol)(using Context): Boolean =
    cls.baseClasses.exists { p =>
      p == defn.Function0 || p == defn.Function1 || p == defn.Function2
    }
}
