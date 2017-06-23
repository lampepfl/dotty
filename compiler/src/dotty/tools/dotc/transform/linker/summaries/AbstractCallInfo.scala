package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.TermSymbol
import dotty.tools.dotc.core.Types.{MethodType, PolyType, TermRef, Type, TypeBounds}
import dotty.tools.dotc.transform.linker.types.ClosureType
import dotty.tools.sharable

abstract class AbstractCallInfo {

  final val id: Int = AbstractCallInfo.nextId()

  /** This is type of method, that includes full type of receiver, eg: TermRef(receiver, Method) */
  val call: TermRef

  /** Type arguments at call site */
  val targs: List[Type]

  /** Type of the arguments at call site */
  val argumentsPassed: List[Type]

  def callSymbol(implicit ctx: Context): TermSymbol = call.normalizedPrefix match {
    case t: ClosureType => t.meth.meth.symbol.asTerm
    case _ => call.termSymbol.asTerm
  }
}

object AbstractCallInfo {

  def assertions(callInfo: AbstractCallInfo)(implicit ctx: Context): Unit = {
    callInfo.call.widenDealias match {
      case t: PolyType => assert(t.paramNames.size == callInfo.targs.size)
      case t: MethodType => assert(t.paramNamess.flatten.size == callInfo.argumentsPassed.size)
      case _ =>
    }
    callInfo.argumentsPassed.foreach(arg => assert(arg.widen.isValueType, arg))
    callInfo.targs.foreach(targ => assert(!targ.isInstanceOf[TypeBounds], targ))
  }

  @sharable private var lastId = 0

  private[AbstractCallInfo] def nextId(): Int = {
    lastId += 1
    lastId
  }

}
