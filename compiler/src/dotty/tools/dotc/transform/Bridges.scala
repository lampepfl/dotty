package dotty.tools
package dotc
package transform

import core._
import Symbols._, Types._, Contexts._, Decorators._, Flags._, Scopes._, Phases._
import DenotTransformers._
import ast.untpd
import collection.{mutable, immutable}
import util.SrcPos
import ContextFunctionResults.{contextResultCount, contextFunctionResultTypeAfter}
import StdNames.nme
import Constants.Constant
import TypeErasure.transformInfo
import Erasure.Boxing.adaptClosure

/** A helper class for generating bridge methods in class `root`. */
class Bridges(root: ClassSymbol, thisPhase: DenotTransformer)(using Context) {
  import ast.tpd._

  assert(ctx.phase == erasurePhase.next)
  private val preErasureCtx = ctx.withPhase(erasurePhase)
  private lazy val elimErasedCtx = ctx.withPhase(elimErasedValueTypePhase.next)

  private class BridgesCursor(using Context) extends OverridingPairs.Cursor(root) {

    override def isSubParent(parent: Symbol, bc: Symbol)(using Context) =
      true
      	// Never consider a bridge if there is a superclass that would contain it
      	// See run/t2857.scala for a test that would break with a VerifyError otherwise.

    /** Only use the superclass of `root` as a parent class. This means
     *  overriding pairs that have a common implementation in a trait parent
     *  are also counted. The only exception is generation of bridges for traits,
     *  where we want to be able to deduplicate bridges already defined in parents.
     */
    override lazy val parents =
      if(root.is(Trait)) super.parents
      else Array(root.superClass)

    override def exclude(sym: Symbol) =
      !sym.isOneOf(MethodOrModule) || sym.isAllOf(Module | JavaDefined) || super.exclude(sym)

    override def canBeHandledByParent(sym1: Symbol, sym2: Symbol, parent: Symbol): Boolean =
      OverridingPairs.isOverridingPair(sym1, sym2, parent.thisType)
  }

  val site = root.thisType

  private var toBeRemoved = immutable.Set[Symbol]()
  private val bridges = mutable.ListBuffer[Tree]()
  private val bridgesScope = newScope
  private val bridgeTarget = MutableSymbolMap[Symbol]()

  def bridgePosFor(member: Symbol): SrcPos =
    (if (member.owner == root && member.span.exists) member else root).srcPos

  /** Add a bridge between `member` and `other`, where `member` overrides `other`
   *  before erasure, if the following conditions are satisfied.
   *
   *   - `member` and `other` have different signatures
   *   - there is not yet a bridge with the same name and signature in `root`.
   *
   *  The bridge has the erased info of `other` and forwards to `member`.
   *  Additionally, if `member` and `other` do have the same signature,
   *  but not the same type after erasure and before elimErasedValueTypes
   *  issue an error: A bridge would be needed yet it would clash with the member itself.
   *  See neg/i1905.scala
   */
  private def addBridgeIfNeeded(member: Symbol, other: Symbol) = {
    def bridgeExists =
      bridgesScope.lookupAll(member.name).exists(bridge =>
        bridgeTarget(bridge) == member && bridge.signature == other.signature)
    def info(sym: Symbol)(using Context) = sym.info
    def desc(sym: Symbol)= {
      val infoStr = info(sym)(using preErasureCtx) match {
        case ExprType(info) => i": $info"
        case info => info.show
      }
      i"$sym$infoStr in ${sym.owner}"
    }
    if (member.signature == other.signature) {
      if (!member.info.matches(other.info))
        report.error(em"""bridge generated for member ${desc(member)}
                      |which overrides ${desc(other)}
                      |clashes with definition of the member itself; both have erased type ${info(member)(using elimErasedCtx)}."""",
                  bridgePosFor(member))
    }
    else if !inContext(preErasureCtx)(site.memberInfo(member).matches(site.memberInfo(other))) then
      // Neither symbol signatures nor pre-erasure types seen from root match; this means
      // according to Scala 2 semantics there is no override.
      // A bridge might introduce a classcast exception.
      // Example where this was observed: run/i12828a.scala and MapView in stdlib213
      report.log(i"suppress bridge in $root for ${member} in ${member.owner} and ${other.showLocated} since member infos ${site.memberInfo(member)} and ${site.memberInfo(other)} do not match")
    else if !bridgeExists then
      addBridge(member, other)
  }

  /** Generate bridge between `member` and `other`
   */
  private def addBridge(member: Symbol, other: Symbol) = {
    val bridge = other.copy(
      owner = root,
      flags = (member.flags | Method | Bridge | Artifact) &~
        (Accessor | ParamAccessor | CaseAccessor | Deferred | Lazy | Module),
      coord = bridgePosFor(member).span).enteredAfter(thisPhase).asTerm

    report.debuglog(
      i"""generating bridge from ${other.showLocated}: ${other.info}
             |to ${member.showLocated}: ${member.info} @ ${member.span}
             |bridge: ${bridge.showLocated} with flags: ${bridge.flagsString}""")

    bridgeTarget(bridge) = member
    bridgesScope.enter(bridge)

    if (other.owner == root) {
      root.delete(other)
      toBeRemoved += other
    }

    val memberCount = contextResultCount(member)

    /** Eta expand application `ref(args)` as needed.
     *  To do this correctly, we have to look at the member's original pre-erasure
     *  type and figure out which context function types in its result are
     *  not yet instantiated.
     */
    def etaExpand(ref: Tree, args: List[Tree])(using Context): Tree =
      def expand(args: List[Tree], tp: Type, n: Int)(using Context): Tree =
        if n <= 0 then
          assert(ctx.typer.isInstanceOf[Erasure.Typer])
          ctx.typer.typed(untpd.cpy.Apply(ref)(ref, args), member.info.finalResultType)
        else
          val defn.ContextFunctionType(argTypes, resType, isErased) = tp: @unchecked
          val anonFun = newAnonFun(ctx.owner,
            MethodType(if isErased then Nil else argTypes, resType),
            coord = ctx.owner.coord)
          anonFun.info = transformInfo(anonFun, anonFun.info)

          def lambdaBody(refss: List[List[Tree]]) =
            val refs :: Nil = refss: @unchecked
            val expandedRefs = refs.map(_.withSpan(ctx.owner.span.endPos)) match
              case (bunchedParam @ Ident(nme.ALLARGS)) :: Nil =>
                argTypes.indices.toList.map(n =>
                  bunchedParam
                    .select(nme.primitive.arrayApply)
                    .appliedTo(Literal(Constant(n))))
              case refs1 => refs1
            expand(args ::: expandedRefs, resType, n - 1)(using ctx.withOwner(anonFun))

          val unadapted = Closure(anonFun, lambdaBody)
          cpy.Block(unadapted)(unadapted.stats,
            adaptClosure(unadapted.expr.asInstanceOf[Closure]))
      end expand

      val otherCount = contextResultCount(other)
      val start = contextFunctionResultTypeAfter(member, otherCount)(using preErasureCtx)
      expand(args, start, memberCount - otherCount)(using ctx.withOwner(bridge))
    end etaExpand

    def bridgeRhs(argss: List[List[Tree]]) =
      assert(argss.tail.isEmpty)
      val ref = This(root).select(member)
      if member.info.isParameterless then ref // can happen if `member` is a module
      else if memberCount == 0 then ref.appliedToTermArgs(argss.head)
      else etaExpand(ref, argss.head)

    bridges += DefDef(bridge, bridgeRhs(_).withSpan(bridge.span))
  }

  /** Add all necessary bridges to template statements `stats`, and remove at the same
   *  time deferred methods in `stats` that are replaced by a bridge with the same signature.
   */
  def add(stats: List[untpd.Tree]): List[untpd.Tree] =
    // When adding bridges to traits ignore non-public methods
    // see `DottyBackendTests.invocationReceivers`
    val opc = inContext(preErasureCtx) { new BridgesCursor }
    while opc.hasNext do
      if
        !opc.overriding.is(Deferred) &&
        (!root.is(Trait) || opc.overridden.isPublic)
      then
        addBridgeIfNeeded(opc.overriding, opc.overridden)
      opc.next()
    if bridges.isEmpty then stats
    else stats.filterNot(stat => toBeRemoved contains stat.symbol) ::: bridges.toList
}
