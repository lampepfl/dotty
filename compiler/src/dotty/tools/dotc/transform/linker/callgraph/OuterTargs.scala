package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{RefinedType, Type, TypeAccumulator, TypeAlias, TypeType}
import dotty.tools.dotc.transform.linker.types.ClosureType

object OuterTargs {
  val empty = new OuterTargs(Map.empty)

  def parentRefinements(tp: Type)(implicit ctx: Context): OuterTargs = {
    new TypeAccumulator[OuterTargs]() {
      def apply(x: OuterTargs, tp: Type): OuterTargs = tp match {
        case t: RefinedType =>
          val member = t.parent.member(t.refinedName).symbol
          val parent = member.owner
          val nList = x.add(parent, t.refinedName, t.refinedInfo)
          apply(nList, t.parent)
        case t: ClosureType =>
          apply(x, t.u)
        case _ =>
          foldOver(x, tp)
      }
    }.apply(OuterTargs.empty, tp)
  }
}

final class OuterTargs(val mp: Map[Symbol, Map[Name, Type]]) extends AnyVal {

  def nonEmpty: Boolean = mp.nonEmpty

  def add(parent: Symbol, tp: Type)(implicit ctx: Context): OuterTargs = {
    assert(!parent.isClass || tp.isInstanceOf[TypeType], tp)
    this.add(parent, tp.typeSymbol.name, tp)
  }

  def add(parent: Symbol, name: Name, tp: Type)(implicit ctx: Context): OuterTargs = {
    val tp1 = if (parent.isClass && !tp.isInstanceOf[TypeType]) TypeAlias(tp) else tp
    val old = mp.getOrElse(parent, Map.empty)
    new OuterTargs(mp.updated(parent, old + (name -> tp1)))
  }

  def addAll(parent: Symbol, names: List[Name], tps: List[Type])(implicit ctx: Context): OuterTargs =
    (names zip tps).foldLeft(this)((x, nameType) => x.add(parent, nameType._1, nameType._2))

  def ++(other: OuterTargs)(implicit ctx: Context): OuterTargs = {
    other.mp.foldLeft(this) { (x, y) =>
      y._2.foldLeft(x: OuterTargs)((x: OuterTargs, z: (Name, Type)) => x.add(y._1, z._1, z._2))
    }
  }

  def combine(environment: OuterTargs)(implicit ctx: Context): OuterTargs = {
    val subst = new SubstituteByParentMap(environment)
    val newMap = mp.map(x => (x._1, x._2.map(x => (x._1, subst.apply(x._2)))))
    new OuterTargs(newMap)
  }

  override def toString: String = s"OuterTargs($mp)"
}
