package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Types.Type

class TypeWithContext(val tp: Type, val outerTargs: OuterTargs) {

  override def equals(obj: Any): Boolean = obj match {
    case obj: TypeWithContext => tp == obj.tp && outerTargs == obj.outerTargs
    case _ => false
  }

  override def hashCode(): Int = tp.hashCode() // ^ outerTargs.mp.hashCode()

  override def toString: String = s"TypeWithContext($tp, $outerTargs)"
}
