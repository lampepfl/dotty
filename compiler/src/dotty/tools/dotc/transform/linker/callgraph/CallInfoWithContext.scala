package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.linker.summaries.{AbstractCallInfo, CallInfo}
import dotty.tools.dotc.transform.linker.types._

import scala.collection.mutable

class CallInfoWithContext private (val call: TermRef, val targs: List[Type], val argumentsPassed: List[Type],
    val outerTargs: OuterTargs, val parent: Option[CallInfoWithContext], val callee: Option[CallInfo])
    extends AbstractCallInfo {

  private lazy val outEdges = mutable.HashMap[CallInfo, List[CallInfoWithContext]]().withDefault(x => Nil)

  def outEdgesIterator: Iterator[(CallInfo, List[CallInfoWithContext])] = outEdges.iterator

  def getOutEdges(callSite: CallInfo): List[CallInfoWithContext] = outEdges(callSite)

  def addOutEdges(callSite: CallInfo, edges: Traversable[CallInfoWithContext]): Unit = {
    var es = outEdges(callSite)
    for (e <- edges) {
      if (!es.contains(e))
        es = e :: es
    }
    outEdges(callSite) = es
  }

  def addOutEdges(callSite: CallInfo, e: CallInfoWithContext): Unit = {
    val es = outEdges(callSite)
    if (!es.contains(e))
      outEdges(callSite) = e :: es
  }

  def edgeCount: Int =
    outEdges.values.foldLeft(0)(_ + _.size)

  def source: Option[CallInfo] = callee.flatMap(_.source)

  def isOnJavaAllocatedType: Boolean = call.prefix.isInstanceOf[JavaAllocatedType]

  override def equals(obj: Any): Boolean = obj match {
    case obj: CallInfoWithContext =>
      call == obj.call && targs == obj.targs && argumentsPassed == obj.argumentsPassed && outerTargs == obj.outerTargs
    case _ => false
  }

  override def hashCode(): Int = call.hashCode ^ targs.hashCode ^ argumentsPassed.hashCode ^ outerTargs.mp.hashCode

  override def toString(): String = s"CallInfoWithContext($call, $targs, $argumentsPassed, $outerTargs, $parent, $callee)"
}

object CallInfoWithContext {

  def apply(call: TermRef, targs: List[Type], argumentsPassed: List[Type], outerTargs: OuterTargs,
      parent: Option[CallInfoWithContext], callee: Option[CallInfo])(implicit ctx: Context): CallInfoWithContext = {
    val callInfo = new CallInfoWithContext(call, targs, argumentsPassed, outerTargs, parent, callee)
    AbstractCallInfo.assertions(callInfo)
    callInfo
  }

}
