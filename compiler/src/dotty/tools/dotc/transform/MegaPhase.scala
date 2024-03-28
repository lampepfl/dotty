package dotty.tools
package dotc
package transform

import scala.compiletime.uninitialized

import core.*
import Contexts.*, Phases.*, Symbols.*, Decorators.*
import Flags.PackageVal
import staging.StagingLevel.*

/** A MegaPhase combines a number of mini-phases which are all executed in
 *  a single tree traversal.
 *
 *  This is an evolution of the previous "TreeTransformers.scala", which was written by @DarkDimius and
 *  is described in his thesis.
 */
object MegaPhase {
  import ast.tpd.*

  /** The base class of tree transforms. For each kind of tree K, there are
   *  two methods which can be overridden:
   *
   *  prepareForK: return a new Context which gets passed to the node and its children
   *  transformK // transform node of type K
   *
   *  There are also prepare/transform hooks for
   *
   *   - Stats: to prepare/transform a statement sequence in a block, template, or package def,
   *   - Unit : to prepare/transform a whole compilation unit
   *   - Other: to prepare/transform a tree that does not have a specific prepare/transform
   *     method pair.
   */
  abstract class MiniPhase extends Phase {

    private[MegaPhase] var superPhase: MegaPhase = uninitialized
    private[MegaPhase] var idxInGroup: Int = uninitialized

    /** List of names of phases that should have finished their processing of all compilation units
     *  before this phase starts
     */
    def runsAfterGroupsOf: Set[String] = Set.empty

    final override def relaxedTyping: Boolean = superPhase.relaxedTyping

    /** If set, use relaxed typing for all phases in group */
    def relaxedTypingInGroup: Boolean = false

    val cpy: TypedTreeCopier = cpyBetweenPhases

    def prepareForIdent(tree: Ident)(using Context): Context = ctx
    def prepareForSelect(tree: Select)(using Context): Context = ctx
    def prepareForThis(tree: This)(using Context): Context = ctx
    def prepareForSuper(tree: Super)(using Context): Context = ctx
    def prepareForApply(tree: Apply)(using Context): Context = ctx
    def prepareForTypeApply(tree: TypeApply)(using Context): Context = ctx
    def prepareForLiteral(tree: Literal)(using Context): Context = ctx
    def prepareForNew(tree: New)(using Context): Context = ctx
    def prepareForTyped(tree: Typed)(using Context): Context = ctx
    def prepareForAssign(tree: Assign)(using Context): Context = ctx
    def prepareForBlock(tree: Block)(using Context): Context = ctx
    def prepareForIf(tree: If)(using Context): Context = ctx
    def prepareForClosure(tree: Closure)(using Context): Context = ctx
    def prepareForMatch(tree: Match)(using Context): Context = ctx
    def prepareForCaseDef(tree: CaseDef)(using Context): Context = ctx
    def prepareForLabeled(tree: Labeled)(using Context): Context = ctx
    def prepareForReturn(tree: Return)(using Context): Context = ctx
    def prepareForWhileDo(tree: WhileDo)(using Context): Context = ctx
    def prepareForTry(tree: Try)(using Context): Context = ctx
    def prepareForSeqLiteral(tree: SeqLiteral)(using Context): Context = ctx
    def prepareForInlined(tree: Inlined)(using Context): Context = ctx
    def prepareForQuote(tree: Quote)(using Context): Context = ctx
    def prepareForSplice(tree: Splice)(using Context): Context = ctx
    def prepareForTypeTree(tree: TypeTree)(using Context): Context = ctx
    def prepareForBind(tree: Bind)(using Context): Context = ctx
    def prepareForAlternative(tree: Alternative)(using Context): Context = ctx
    def prepareForUnApply(tree: UnApply)(using Context): Context = ctx
    def prepareForValDef(tree: ValDef)(using Context): Context = ctx
    def prepareForDefDef(tree: DefDef)(using Context): Context = ctx
    def prepareForTypeDef(tree: TypeDef)(using Context): Context = ctx
    def prepareForTemplate(tree: Template)(using Context): Context = ctx
    def prepareForPackageDef(tree: PackageDef)(using Context): Context = ctx
    def prepareForStats(trees: List[Tree])(using Context): Context = ctx
    def prepareForUnit(tree: Tree)(using Context): Context = ctx
    def prepareForOther(tree: Tree)(using Context): Context = ctx

    def transformIdent(tree: Ident)(using Context): Tree = tree
    def transformSelect(tree: Select)(using Context): Tree = tree
    def transformThis(tree: This)(using Context): Tree = tree
    def transformSuper(tree: Super)(using Context): Tree = tree
    def transformApply(tree: Apply)(using Context): Tree = tree
    def transformTypeApply(tree: TypeApply)(using Context): Tree = tree
    def transformLiteral(tree: Literal)(using Context): Tree = tree
    def transformNew(tree: New)(using Context): Tree = tree
    def transformTyped(tree: Typed)(using Context): Tree = tree
    def transformAssign(tree: Assign)(using Context): Tree = tree
    def transformBlock(tree: Block)(using Context): Tree = tree
    def transformIf(tree: If)(using Context): Tree = tree
    def transformClosure(tree: Closure)(using Context): Tree = tree
    def transformMatch(tree: Match)(using Context): Tree = tree
    def transformCaseDef(tree: CaseDef)(using Context): Tree = tree
    def transformLabeled(tree: Labeled)(using Context): Tree = tree
    def transformReturn(tree: Return)(using Context): Tree = tree
    def transformWhileDo(tree: WhileDo)(using Context): Tree = tree
    def transformTry(tree: Try)(using Context): Tree = tree
    def transformSeqLiteral(tree: SeqLiteral)(using Context): Tree = tree
    def transformInlined(tree: Inlined)(using Context): Tree = tree
    def transformQuote(tree: Quote)(using Context): Tree = tree
    def transformSplice(tree: Splice)(using Context): Tree = tree
    def transformTypeTree(tree: TypeTree)(using Context): Tree = tree
    def transformBind(tree: Bind)(using Context): Tree = tree
    def transformAlternative(tree: Alternative)(using Context): Tree = tree
    def transformUnApply(tree: UnApply)(using Context): Tree = tree
    def transformValDef(tree: ValDef)(using Context): Tree = tree
    def transformDefDef(tree: DefDef)(using Context): Tree = tree
    def transformTypeDef(tree: TypeDef)(using Context): Tree = tree
    def transformTemplate(tree: Template)(using Context): Tree = tree
    def transformPackageDef(tree: PackageDef)(using Context): Tree = tree
    def transformStats(trees: List[Tree])(using Context): List[Tree] = trees
    def transformUnit(tree: Tree)(using Context): Tree = tree
    def transformOther(tree: Tree)(using Context): Tree = tree

    /** Transform tree using all transforms of current group (including this one) */
    def transformAllDeep(tree: Tree)(using Context): Tree =
      superPhase.transformTree(tree, 0)

    /** Transform tree using all transforms following the current one in this group */
    def transformFollowingDeep(tree: Tree)(using Context): Tree =
      superPhase.transformTree(tree, idxInGroup + 1)

    /** Transform single node using all transforms following the current one in this group */
    def transformFollowing(tree: Tree)(using Context): Tree =
      superPhase.transformNode(tree, idxInGroup + 1)

    protected def singletonGroup: MegaPhase = new MegaPhase(Array(this))

    override def run(using Context): Unit =
      singletonGroup.run

    override def isRunnable(using Context): Boolean = super.isRunnable && !ctx.usedBestEffortTasty
  }
}
import MegaPhase.*

class MegaPhase(val miniPhases: Array[MiniPhase]) extends Phase {
  import ast.tpd.*

  override val phaseName: String =
    if (miniPhases.length == 1) miniPhases(0).phaseName
    else miniPhases.map(_.phaseName).mkString("MegaPhase{", ", ", "}")

  /** Used in progress reporting to avoid super long phase names, also the precision is not so important here */
  lazy val shortPhaseName: String =
    if (miniPhases.length == 1) miniPhases(0).phaseName
    else
      s"MegaPhase{${miniPhases.head.phaseName},...,${miniPhases.last.phaseName}}"

  private var relaxedTypingCache: Boolean = uninitialized
  private var relaxedTypingKnown = false

  override final def relaxedTyping: Boolean = {
    if (!relaxedTypingKnown) {
      relaxedTypingCache = miniPhases.exists(_.relaxedTypingInGroup)
      relaxedTypingKnown = true
    }
    relaxedTypingCache
  }

  override def isRunnable(using Context): Boolean = super.isRunnable && !ctx.usedBestEffortTasty

  private val cpy: TypedTreeCopier = cpyBetweenPhases

  /** Transform node using all phases in this group that have idxInGroup >= start */
  def transformNode(tree: Tree, start: Int)(using Context): Tree = {
    def goNamed(tree: Tree, start: Int) =
      try
        tree match {
          case tree: Ident => goIdent(tree, start)
          case tree: Select => goSelect(tree, start)
          case tree: ValDef => goValDef(tree, start)
          case tree: DefDef => goDefDef(tree, start)
          case tree: TypeDef => goTypeDef(tree, start)
          case tree: Labeled => goLabeled(tree, start)
          case tree: Bind => goBind(tree, start)
          case _ => goOther(tree, start)
        }
      catch {
        case ex: TypeError =>
          report.error(ex, tree.srcPos)
          tree
      }
    def goUnnamed(tree: Tree, start: Int) =
      try
        tree match {
          case tree: Apply => goApply(tree, start)
          case tree: TypeTree => goTypeTree(tree, start)
          case tree: Thicket =>
            cpy.Thicket(tree)(tree.trees.mapConserve(transformNode(_, start)))
          case tree: This => goThis(tree, start)
          case tree: Literal => goLiteral(tree, start)
          case tree: Block => goBlock(tree, start)
          case tree: TypeApply => goTypeApply(tree, start)
          case tree: If => goIf(tree, start)
          case tree: New => goNew(tree, start)
          case tree: Typed => goTyped(tree, start)
          case tree: CaseDef => goCaseDef(tree, start)
          case tree: Closure => goClosure(tree, start)
          case tree: Assign => goAssign(tree, start)
          case tree: SeqLiteral => goSeqLiteral(tree, start)
          case tree: Super => goSuper(tree, start)
          case tree: Template => goTemplate(tree, start)
          case tree: Match => goMatch(tree, start)
          case tree: UnApply => goUnApply(tree, start)
          case tree: PackageDef => goPackageDef(tree, start)
          case tree: Try => goTry(tree, start)
          case tree: Inlined => goInlined(tree, start)
          case tree: Return => goReturn(tree, start)
          case tree: WhileDo => goWhileDo(tree, start)
          case tree: Alternative => goAlternative(tree, start)
          case tree => goOther(tree, start)
        }
      catch {
        case ex: TypeError =>
          report.error(ex, tree.srcPos)
          tree
      }
    if (tree.isInstanceOf[NameTree]) goNamed(tree, start) else goUnnamed(tree, start)
  }

  /** Transform full tree using all phases in this group that have idxInGroup >= start */
  def transformTree(tree: Tree, start: Int)(using Context): Tree = {

    inline def inLocalContext[T](inline op: Context ?=> T)(using Context): T =
      val sym = tree.symbol
      runWithOwner(if (sym.is(PackageVal)) sym.moduleClass else sym)(op)

    def transformNamed(tree: Tree, start: Int, outerCtx: Context): Tree = tree match {
      case tree: Ident =>
        inContext(prepIdent(tree, start)(using outerCtx)) {
          goIdent(tree, start)
        }
      case tree: Select =>
        inContext(prepSelect(tree, start)(using outerCtx)) {
          val qual = transformTree(tree.qualifier, start)
          goSelect(cpy.Select(tree)(qual, tree.name), start)
        }
      case tree: ValDef =>
        inContext(prepValDef(tree, start)(using outerCtx)) {
          def mapValDef(using Context) = {
            val tpt = transformTree(tree.tpt, start)
            val rhs = transformTree(tree.rhs, start)
            cpy.ValDef(tree)(tree.name, tpt, rhs)
          }
          if tree.isEmpty then tree
          else goValDef(
            if tree.symbol.exists then inLocalContext(mapValDef) else mapValDef,
            start)
        }
      case tree: DefDef =>
        inContext(prepDefDef(tree, start)(using outerCtx)) {
          def mapDefDef(using Context) = {
            val paramss = tree.paramss.mapConserve(transformSpecificTrees(_, start))
              .asInstanceOf[List[ParamClause]]
            val tpt = transformTree(tree.tpt, start)
            val rhs = transformTree(tree.rhs, start)
            cpy.DefDef(tree)(tree.name, paramss, tpt, rhs)
          }
          goDefDef(inLocalContext(mapDefDef), start)
        }
      case tree: TypeDef =>
        inContext(prepTypeDef(tree, start)(using outerCtx)) {
          val rhs = inLocalContext(transformTree(tree.rhs, start))
          goTypeDef(cpy.TypeDef(tree)(tree.name, rhs), start)
        }
      case tree: Labeled =>
        inContext(prepLabeled(tree, start)(using outerCtx)) {
          val bind = transformTree(tree.bind, start).asInstanceOf[Bind]
          val expr = transformTree(tree.expr, start)
          goLabeled(cpy.Labeled(tree)(bind, expr), start)
        }
      case tree: Bind =>
        inContext(prepBind(tree, start)(using outerCtx)) {
          val body = transformTree(tree.body, start)
          goBind(cpy.Bind(tree)(tree.name, body), start)
        }
      case _ =>
        inContext(prepOther(tree, start)(using outerCtx)) {
          goOther(tree, start)
        }
    }

    def transformUnnamed(tree: Tree, start: Int, outerCtx: Context): Tree = tree match {
      case tree: Apply =>
        inContext(prepApply(tree, start)(using outerCtx)) {
          val fun = transformTree(tree.fun, start)
          val args = transformTrees(tree.args, start)
          goApply(cpy.Apply(tree)(fun, args), start)
        }
      case tree: TypeTree =>
        inContext(prepTypeTree(tree, start)(using outerCtx)) {
          goTypeTree(tree, start)
        }
      case tree: Thicket =>
        cpy.Thicket(tree)(transformTrees(tree.trees, start))
      case tree: This =>
        inContext(prepThis(tree, start)(using outerCtx)) {
          goThis(tree, start)
        }
      case tree: Literal =>
        inContext(prepLiteral(tree, start)(using outerCtx)) {
          goLiteral(tree, start)
        }
      case tree: Block =>
        inContext(prepBlock(tree, start)(using outerCtx)) {
          transformBlock(tree, start)
        }
      case tree: TypeApply =>
        inContext(prepTypeApply(tree, start)(using outerCtx)) {
          val fun = transformTree(tree.fun, start)
          val args = transformTrees(tree.args, start)
          goTypeApply(cpy.TypeApply(tree)(fun, args), start)
        }
      case tree: If =>
        inContext(prepIf(tree, start)(using outerCtx)) {
          val cond = transformTree(tree.cond, start)
          val thenp = transformTree(tree.thenp, start)
          val elsep = transformTree(tree.elsep, start)
          goIf(cpy.If(tree)(cond, thenp, elsep), start)
        }
      case tree: New =>
        inContext(prepNew(tree, start)(using outerCtx)) {
          val tpt = transformTree(tree.tpt, start)
          goNew(cpy.New(tree)(tpt), start)
        }
      case tree: Typed =>
        inContext(prepTyped(tree, start)(using outerCtx)) {
          val expr = transformTree(tree.expr, start)
          val tpt = transformTree(tree.tpt, start)
          goTyped(cpy.Typed(tree)(expr, tpt), start)
        }
      case tree: CaseDef =>
        inContext(prepCaseDef(tree, start)(using outerCtx)) {
          val pat = withMode(Mode.Pattern)(transformTree(tree.pat, start))
          val guard = transformTree(tree.guard, start)
          val body = transformTree(tree.body, start)
          goCaseDef(cpy.CaseDef(tree)(pat, guard, body), start)
        }
      case tree: Closure =>
        inContext(prepClosure(tree, start)(using outerCtx)) {
          val env = transformTrees(tree.env, start)
          val meth = transformTree(tree.meth, start)
          val tpt = transformTree(tree.tpt, start)
          goClosure(cpy.Closure(tree)(env, meth, tpt), start)
        }
      case tree: Assign =>
        inContext(prepAssign(tree, start)(using outerCtx)) {
          val lhs = transformTree(tree.lhs, start)
          val rhs = transformTree(tree.rhs, start)
          goAssign(cpy.Assign(tree)(lhs, rhs), start)
        }
      case tree: SeqLiteral =>
        inContext(prepSeqLiteral(tree, start)(using outerCtx)) {
          val elems = transformTrees(tree.elems, start)
          val elemtpt = transformTree(tree.elemtpt, start)
          goSeqLiteral(cpy.SeqLiteral(tree)(elems, elemtpt), start)
        }
      case tree: Super =>
        inContext(prepSuper(tree, start)(using outerCtx)) {
          goSuper(tree, start)
        }
      case tree: Template =>
        inContext(prepTemplate(tree, start)(using outerCtx)) {
          val constr = transformSpecificTree(tree.constr, start)
          val parents = transformTrees(tree.parents, start)(using ctx.superCallContext)
          val self = transformSpecificTree(tree.self, start)
          val body = transformStats(tree.body, tree.symbol, start)
          goTemplate(cpy.Template(tree)(constr, parents, Nil, self, body), start)
        }
      case tree: Match =>
        inContext(prepMatch(tree, start)(using outerCtx)) {
          val selector = transformTree(tree.selector, start)
          val cases = transformSpecificTrees(tree.cases, start)
          goMatch(cpy.Match(tree)(selector, cases), start)
        }
      case tree: UnApply =>
        inContext(prepUnApply(tree, start)(using outerCtx)) {
          val fun = transformTree(tree.fun, start)
          val implicits = transformTrees(tree.implicits, start)
          val patterns = transformTrees(tree.patterns, start)
          goUnApply(cpy.UnApply(tree)(fun, implicits, patterns), start)
        }
      case tree: PackageDef =>
        inContext(prepPackageDef(tree, start)(using outerCtx)) {
          def mapPackage(using Context) = {
            val pid = transformSpecificTree(tree.pid, start)
            val stats = transformStats(tree.stats, tree.symbol, start)
            cpy.PackageDef(tree)(pid, stats)
          }
          goPackageDef(inLocalContext(mapPackage), start)
        }
      case tree: Try =>
        inContext(prepTry(tree, start)(using outerCtx)) {
          val expr = transformTree(tree.expr, start)
          val cases = transformSpecificTrees(tree.cases, start)
          val finalizer = transformTree(tree.finalizer, start)
          goTry(cpy.Try(tree)(expr, cases, finalizer), start)
        }
      case tree: Inlined =>
        inContext(prepInlined(tree, start)(using outerCtx)) {
          val bindings = transformSpecificTrees(tree.bindings, start)
          val expansion = transformTree(tree.expansion, start)(using inlineContext(tree))
          goInlined(cpy.Inlined(tree)(tree.call, bindings, expansion), start)
        }
      case tree: Quote =>
        inContext(prepQuote(tree, start)(using outerCtx)) {
          val body = transformTree(tree.body, start)(using quoteContext)
          goQuote(cpy.Quote(tree)(body, Nil), start)
        }
      case tree: Splice =>
        inContext(prepSplice(tree, start)(using outerCtx)) {
          val expr = transformTree(tree.expr, start)(using spliceContext)
          goSplice(cpy.Splice(tree)(expr), start)
        }
      case tree: Return =>
        inContext(prepReturn(tree, start)(using outerCtx)) {
          val expr = transformTree(tree.expr, start)
          goReturn(cpy.Return(tree)(expr, tree.from), start)
            // don't transform `tree.from`, as this is not a normal ident, but
            // a pointer to the enclosing method.
        }
      case tree: WhileDo =>
        inContext(prepWhileDo(tree, start)(using outerCtx)) {
          val cond = transformTree(tree.cond, start)
          val body = transformTree(tree.body, start)
          goWhileDo(cpy.WhileDo(tree)(cond, body), start)
        }
      case tree: Alternative =>
        inContext(prepAlternative(tree, start)(using outerCtx)) {
          val trees = transformTrees(tree.trees, start)
          goAlternative(cpy.Alternative(tree)(trees), start)
        }
      case tree =>
        inContext(prepOther(tree, start)(using outerCtx)) {
          goOther(tree, start)
        }
    }

    // try
      if (tree.source != ctx.source && tree.source.exists)
        transformTree(tree, start)(using ctx.withSource(tree.source))
      else if (tree.isInstanceOf[NameTree])
        transformNamed(tree, start, ctx)
      else
        transformUnnamed(tree, start, ctx)
    // catch case ex: AssertionError =>
    //  println(i"error while transforming $tree")
    //  throw ex
  }

  def transformSpecificTree[T <: Tree](tree: T, start: Int)(using Context): T =
    transformTree(tree, start).asInstanceOf[T]

  def transformStats(trees: List[Tree], exprOwner: Symbol, start: Int)(using Context): List[Tree] =
    val nestedCtx = prepStats(trees, start)
    val trees1 = trees.mapStatements(exprOwner, transformTree(_, start), stats1 => stats1)(using nestedCtx)
    goStats(trees1, start)(using nestedCtx)

  def transformBlock(tree: Block, start: Int)(using Context): Tree =
    val nestedCtx = prepStats(tree.stats, start)
    val block1 = tree.stats.mapStatements(ctx.owner,
      transformTree(_, start),
      stats1 => ctx ?=> {
        val stats2 = goStats(stats1, start)(using nestedCtx)
        val expr2 = transformTree(tree.expr, start)
        cpy.Block(tree)(stats2, expr2)
      })(using nestedCtx)
    goBlock(block1, start)

  def transformUnit(tree: Tree)(using Context): Tree = {
    val nestedCtx = prepUnit(tree, 0)
    val tree1 = transformTree(tree, 0)(using nestedCtx)
    goUnit(tree1, 0)(using nestedCtx)
  }

  def transformTrees(trees: List[Tree], start: Int)(using Context): List[Tree] =
    trees.flattenedMapConserve(transformTree(_, start))

  def transformSpecificTrees[T <: Tree](trees: List[T], start: Int)(using Context): List[T] =
    transformTrees(trees, start).asInstanceOf[List[T]]

  override def run(using Context): Unit =
    ctx.compilationUnit.tpdTree =
      atPhase(miniPhases.last.next)(transformUnit(ctx.compilationUnit.tpdTree))

  // Initialization code

  /** Class#getDeclaredMethods is slow, so we cache its output */
  private val clsMethodsCache = new java.util.IdentityHashMap[Class[?], Array[java.lang.reflect.Method | Null]]

  /** Does `phase` contain a redefinition of method `name`?
   *  (which is a method of MiniPhase)
   */
  private def defines(phase: MiniPhase, name: String) = {
    def hasRedefinedMethod(cls: Class[?]): Boolean =
      if (cls.eq(classOf[MiniPhase])) false
      else {
        var clsMethods = clsMethodsCache.get(cls)
        if (clsMethods == null) {
          clsMethods = cls.getDeclaredMethods
          clsMethodsCache.put(cls, clsMethods)
        }
        clsMethods.nn.exists(_.nn.getName == name) ||
        hasRedefinedMethod(cls.getSuperclass.nn)
      }
    hasRedefinedMethod(phase.getClass)
  }

  private def newNxArray = new Array[MiniPhase | Null](miniPhases.length + 1)
  private val emptyNxArray = newNxArray

  private def init(methName: String): Array[MiniPhase | Null] = {
    var nx: Array[MiniPhase | Null] = emptyNxArray
    for (idx <- miniPhases.length - 1 to 0 by -1) {
      val subPhase = miniPhases(idx)
      if (defines(subPhase, methName)) {
        if (nx eq emptyNxArray) nx = newNxArray
        nx(idx) = subPhase
      }
      else if (nx ne emptyNxArray) nx(idx) = nx(idx + 1)
    }
    nx
  }

  private val nxIdentPrepPhase = init("prepareForIdent")
  private val nxIdentTransPhase = init("transformIdent")
  private val nxSelectPrepPhase = init("prepareForSelect")
  private val nxSelectTransPhase = init("transformSelect")
  private val nxThisPrepPhase = init("prepareForThis")
  private val nxThisTransPhase = init("transformThis")
  private val nxSuperPrepPhase = init("prepareForSuper")
  private val nxSuperTransPhase = init("transformSuper")
  private val nxApplyPrepPhase = init("prepareForApply")
  private val nxApplyTransPhase = init("transformApply")
  private val nxTypeApplyPrepPhase = init("prepareForTypeApply")
  private val nxTypeApplyTransPhase = init("transformTypeApply")
  private val nxLiteralPrepPhase = init("prepareForLiteral")
  private val nxLiteralTransPhase = init("transformLiteral")
  private val nxNewPrepPhase = init("prepareForNew")
  private val nxNewTransPhase = init("transformNew")
  private val nxTypedPrepPhase = init("prepareForTyped")
  private val nxTypedTransPhase = init("transformTyped")
  private val nxAssignPrepPhase = init("prepareForAssign")
  private val nxAssignTransPhase = init("transformAssign")
  private val nxBlockPrepPhase = init("prepareForBlock")
  private val nxBlockTransPhase = init("transformBlock")
  private val nxIfPrepPhase = init("prepareForIf")
  private val nxIfTransPhase = init("transformIf")
  private val nxClosurePrepPhase = init("prepareForClosure")
  private val nxClosureTransPhase = init("transformClosure")
  private val nxMatchPrepPhase = init("prepareForMatch")
  private val nxMatchTransPhase = init("transformMatch")
  private val nxCaseDefPrepPhase = init("prepareForCaseDef")
  private val nxCaseDefTransPhase = init("transformCaseDef")
  private val nxLabeledPrepPhase = init("prepareForLabeled")
  private val nxLabeledTransPhase = init("transformLabeled")
  private val nxReturnPrepPhase = init("prepareForReturn")
  private val nxReturnTransPhase = init("transformReturn")
  private val nxWhileDoPrepPhase = init("prepareForWhileDo")
  private val nxWhileDoTransPhase = init("transformWhileDo")
  private val nxTryPrepPhase = init("prepareForTry")
  private val nxTryTransPhase = init("transformTry")
  private val nxSeqLiteralPrepPhase = init("prepareForSeqLiteral")
  private val nxSeqLiteralTransPhase = init("transformSeqLiteral")
  private val nxInlinedPrepPhase = init("prepareForInlined")
  private val nxInlinedTransPhase = init("transformInlined")
  private val nxQuotePrepPhase = init("prepareForQuote")
  private val nxQuoteTransPhase = init("transformQuote")
  private val nxSplicePrepPhase = init("prepareForPrep")
  private val nxSpliceTransPhase = init("transformSplice")
  private val nxTypeTreePrepPhase = init("prepareForTypeTree")
  private val nxTypeTreeTransPhase = init("transformTypeTree")
  private val nxBindPrepPhase = init("prepareForBind")
  private val nxBindTransPhase = init("transformBind")
  private val nxAlternativePrepPhase = init("prepareForAlternative")
  private val nxAlternativeTransPhase = init("transformAlternative")
  private val nxUnApplyPrepPhase = init("prepareForUnApply")
  private val nxUnApplyTransPhase = init("transformUnApply")
  private val nxValDefPrepPhase = init("prepareForValDef")
  private val nxValDefTransPhase = init("transformValDef")
  private val nxDefDefPrepPhase = init("prepareForDefDef")
  private val nxDefDefTransPhase = init("transformDefDef")
  private val nxTypeDefPrepPhase = init("prepareForTypeDef")
  private val nxTypeDefTransPhase = init("transformTypeDef")
  private val nxTemplatePrepPhase = init("prepareForTemplate")
  private val nxTemplateTransPhase = init("transformTemplate")
  private val nxPackageDefPrepPhase = init("prepareForPackageDef")
  private val nxPackageDefTransPhase = init("transformPackageDef")
  private val nxStatsPrepPhase = init("prepareForStats")
  private val nxStatsTransPhase = init("transformStats")
  private val nxUnitPrepPhase = init("prepareForUnit")
  private val nxUnitTransPhase = init("transformUnit")
  private val nxOtherPrepPhase = init("prepareForOther")
  private val nxOtherTransPhase = init("transformOther")

  for ((phase, idx) <- miniPhases.zipWithIndex) {
    phase.superPhase = this
    phase.idxInGroup = idx
  }

  // Boilerplate snippets

  def prepIdent(tree: Ident, start: Int)(using Context): Context = {
    val phase = nxIdentPrepPhase(start)
    if (phase == null) ctx
    else prepIdent(tree, phase.idxInGroup + 1)(using phase.prepareForIdent(tree))
  }

  def goIdent(tree: Ident, start: Int)(using Context): Tree = {
    val phase = nxIdentTransPhase(start)
    if (phase == null) tree
    else phase.transformIdent(tree) match {
      case tree1: Ident => goIdent(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepSelect(tree: Select, start: Int)(using Context): Context = {
    val phase = nxSelectPrepPhase(start)
    if (phase == null) ctx
    else prepSelect(tree, phase.idxInGroup + 1)(using phase.prepareForSelect(tree))
  }

  def goSelect(tree: Select, start: Int)(using Context): Tree = {
    val phase = nxSelectTransPhase(start)
    if (phase == null) tree
    else phase.transformSelect(tree) match {
      case tree1: Select => goSelect(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepThis(tree: This, start: Int)(using Context): Context = {
    val phase = nxThisPrepPhase(start)
    if (phase == null) ctx
    else prepThis(tree, phase.idxInGroup + 1)(using phase.prepareForThis(tree))
  }

  def goThis(tree: This, start: Int)(using Context): Tree = {
    val phase = nxThisTransPhase(start)
    if (phase == null) tree
    else phase.transformThis(tree) match {
      case tree1: This => goThis(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepSuper(tree: Super, start: Int)(using Context): Context = {
    val phase = nxSuperPrepPhase(start)
    if (phase == null) ctx
    else prepSuper(tree, phase.idxInGroup + 1)(using phase.prepareForSuper(tree))
  }

  def goSuper(tree: Super, start: Int)(using Context): Tree = {
    val phase = nxSuperTransPhase(start)
    if (phase == null) tree
    else phase.transformSuper(tree) match {
      case tree1: Super => goSuper(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepApply(tree: Apply, start: Int)(using Context): Context = {
    val phase = nxApplyPrepPhase(start)
    if (phase == null) ctx
    else prepApply(tree, phase.idxInGroup + 1)(using phase.prepareForApply(tree))
  }

  def goApply(tree: Apply, start: Int)(using Context): Tree = {
    val phase = nxApplyTransPhase(start)
    if (phase == null) tree
    else phase.transformApply(tree) match {
      case tree1: Apply => goApply(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTypeApply(tree: TypeApply, start: Int)(using Context): Context = {
    val phase = nxTypeApplyPrepPhase(start)
    if (phase == null) ctx
    else prepTypeApply(tree, phase.idxInGroup + 1)(using phase.prepareForTypeApply(tree))
  }

  def goTypeApply(tree: TypeApply, start: Int)(using Context): Tree = {
    val phase = nxTypeApplyTransPhase(start)
    if (phase == null) tree
    else phase.transformTypeApply(tree) match {
      case tree1: TypeApply => goTypeApply(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepLiteral(tree: Literal, start: Int)(using Context): Context = {
    val phase = nxLiteralPrepPhase(start)
    if (phase == null) ctx
    else prepLiteral(tree, phase.idxInGroup + 1)(using phase.prepareForLiteral(tree))
  }

  def goLiteral(tree: Literal, start: Int)(using Context): Tree = {
    val phase = nxLiteralTransPhase(start)
    if (phase == null) tree
    else phase.transformLiteral(tree) match {
      case tree1: Literal => goLiteral(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepNew(tree: New, start: Int)(using Context): Context = {
    val phase = nxNewPrepPhase(start)
    if (phase == null) ctx
    else prepNew(tree, phase.idxInGroup + 1)(using phase.prepareForNew(tree))
  }

  def goNew(tree: New, start: Int)(using Context): Tree = {
    val phase = nxNewTransPhase(start)
    if (phase == null) tree
    else phase.transformNew(tree) match {
      case tree1: New => goNew(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTyped(tree: Typed, start: Int)(using Context): Context = {
    val phase = nxTypedPrepPhase(start)
    if (phase == null) ctx
    else prepTyped(tree, phase.idxInGroup + 1)(using phase.prepareForTyped(tree))
  }

  def goTyped(tree: Typed, start: Int)(using Context): Tree = {
    val phase = nxTypedTransPhase(start)
    if (phase == null) tree
    else phase.transformTyped(tree) match {
      case tree1: Typed => goTyped(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepAssign(tree: Assign, start: Int)(using Context): Context = {
    val phase = nxAssignPrepPhase(start)
    if (phase == null) ctx
    else prepAssign(tree, phase.idxInGroup + 1)(using phase.prepareForAssign(tree))
  }

  def goAssign(tree: Assign, start: Int)(using Context): Tree = {
    val phase = nxAssignTransPhase(start)
    if (phase == null) tree
    else phase.transformAssign(tree) match {
      case tree1: Assign => goAssign(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepBlock(tree: Block, start: Int)(using Context): Context = {
    val phase = nxBlockPrepPhase(start)
    if (phase == null) ctx
    else prepBlock(tree, phase.idxInGroup + 1)(using phase.prepareForBlock(tree))
  }

  def goBlock(tree: Block, start: Int)(using Context): Tree = {
    val phase = nxBlockTransPhase(start)
    if (phase == null) tree
    else phase.transformBlock(tree) match {
      case tree1: Block => goBlock(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepIf(tree: If, start: Int)(using Context): Context = {
    val phase = nxIfPrepPhase(start)
    if (phase == null) ctx
    else prepIf(tree, phase.idxInGroup + 1)(using phase.prepareForIf(tree))
  }

  def goIf(tree: If, start: Int)(using Context): Tree = {
    val phase = nxIfTransPhase(start)
    if (phase == null) tree
    else phase.transformIf(tree) match {
      case tree1: If => goIf(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepClosure(tree: Closure, start: Int)(using Context): Context = {
    val phase = nxClosurePrepPhase(start)
    if (phase == null) ctx
    else prepClosure(tree, phase.idxInGroup + 1)(using phase.prepareForClosure(tree))
  }

  def goClosure(tree: Closure, start: Int)(using Context): Tree = {
    val phase = nxClosureTransPhase(start)
    if (phase == null) tree
    else phase.transformClosure(tree) match {
      case tree1: Closure => goClosure(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepMatch(tree: Match, start: Int)(using Context): Context = {
    val phase = nxMatchPrepPhase(start)
    if (phase == null) ctx
    else prepMatch(tree, phase.idxInGroup + 1)(using phase.prepareForMatch(tree))
  }

  def goMatch(tree: Match, start: Int)(using Context): Tree = {
    val phase = nxMatchTransPhase(start)
    if (phase == null) tree
    else phase.transformMatch(tree) match {
      case tree1: Match => goMatch(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepCaseDef(tree: CaseDef, start: Int)(using Context): Context = {
    val phase = nxCaseDefPrepPhase(start)
    if (phase == null) ctx
    else prepCaseDef(tree, phase.idxInGroup + 1)(using phase.prepareForCaseDef(tree))
  }

  def goCaseDef(tree: CaseDef, start: Int)(using Context): Tree = {
    val phase = nxCaseDefTransPhase(start)
    if (phase == null) tree
    else phase.transformCaseDef(tree) match {
      case tree1: CaseDef => goCaseDef(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepLabeled(tree: Labeled, start: Int)(using Context): Context = {
    val phase = nxLabeledPrepPhase(start)
    if (phase == null) ctx
    else prepLabeled(tree, phase.idxInGroup + 1)(using phase.prepareForLabeled(tree))
  }

  def goLabeled(tree: Labeled, start: Int)(using Context): Tree = {
    val phase = nxLabeledTransPhase(start)
    if (phase == null) tree
    else phase.transformLabeled(tree) match {
      case tree1: Labeled => goLabeled(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepReturn(tree: Return, start: Int)(using Context): Context = {
    val phase = nxReturnPrepPhase(start)
    if (phase == null) ctx
    else prepReturn(tree, phase.idxInGroup + 1)(using phase.prepareForReturn(tree))
  }

  def goReturn(tree: Return, start: Int)(using Context): Tree = {
    val phase = nxReturnTransPhase(start)
    if (phase == null) tree
    else phase.transformReturn(tree) match {
      case tree1: Return => goReturn(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepWhileDo(tree: WhileDo, start: Int)(using Context): Context = {
    val phase = nxWhileDoPrepPhase(start)
    if (phase == null) ctx
    else prepWhileDo(tree, phase.idxInGroup + 1)(using phase.prepareForWhileDo(tree))
  }

  def goWhileDo(tree: WhileDo, start: Int)(using Context): Tree = {
    val phase = nxWhileDoTransPhase(start)
    if (phase == null) tree
    else phase.transformWhileDo(tree) match {
      case tree1: WhileDo => goWhileDo(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTry(tree: Try, start: Int)(using Context): Context = {
    val phase = nxTryPrepPhase(start)
    if (phase == null) ctx
    else prepTry(tree, phase.idxInGroup + 1)(using phase.prepareForTry(tree))
  }

  def goTry(tree: Try, start: Int)(using Context): Tree = {
    val phase = nxTryTransPhase(start)
    if (phase == null) tree
    else phase.transformTry(tree) match {
      case tree1: Try => goTry(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepSeqLiteral(tree: SeqLiteral, start: Int)(using Context): Context = {
    val phase = nxSeqLiteralPrepPhase(start)
    if (phase == null) ctx
    else prepSeqLiteral(tree, phase.idxInGroup + 1)(using phase.prepareForSeqLiteral(tree))
  }

  def goSeqLiteral(tree: SeqLiteral, start: Int)(using Context): Tree = {
    val phase = nxSeqLiteralTransPhase(start)
    if (phase == null) tree
    else phase.transformSeqLiteral(tree) match {
      case tree1: SeqLiteral => goSeqLiteral(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepInlined(tree: Inlined, start: Int)(using Context): Context = {
    val phase = nxInlinedPrepPhase(start)
    if (phase == null) ctx
    else prepInlined(tree, phase.idxInGroup + 1)(using phase.prepareForInlined(tree))
  }

  def goInlined(tree: Inlined, start: Int)(using Context): Tree = {
    val phase = nxInlinedTransPhase(start)
    if (phase == null) tree
    else phase.transformInlined(tree) match {
      case tree1: Inlined => goInlined(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepQuote(tree: Quote, start: Int)(using Context): Context = {
    val phase = nxQuotePrepPhase(start)
    if (phase == null) ctx
    else prepQuote(tree, phase.idxInGroup + 1)(using phase.prepareForQuote(tree))
  }

  def goQuote(tree: Quote, start: Int)(using Context): Tree = {
    val phase = nxQuoteTransPhase(start)
    if (phase == null) tree
    else phase.transformQuote(tree) match {
      case tree1: Quote => goQuote(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepSplice(tree: Splice, start: Int)(using Context): Context = {
    val phase = nxSplicePrepPhase(start)
    if (phase == null) ctx
    else prepSplice(tree, phase.idxInGroup + 1)(using phase.prepareForSplice(tree))
  }

  def goSplice(tree: Splice, start: Int)(using Context): Tree = {
    val phase = nxSpliceTransPhase(start)
    if (phase == null) tree
    else phase.transformSplice(tree) match {
      case tree1: Splice => goSplice(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTypeTree(tree: TypeTree, start: Int)(using Context): Context = {
    val phase = nxTypeTreePrepPhase(start)
    if (phase == null) ctx
    else prepTypeTree(tree, phase.idxInGroup + 1)(using phase.prepareForTypeTree(tree))
  }

  def goTypeTree(tree: TypeTree, start: Int)(using Context): Tree = {
    val phase = nxTypeTreeTransPhase(start)
    if (phase == null) tree
    else phase.transformTypeTree(tree) match {
      case tree1: TypeTree => goTypeTree(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepBind(tree: Bind, start: Int)(using Context): Context = {
    val phase = nxBindPrepPhase(start)
    if (phase == null) ctx
    else prepBind(tree, phase.idxInGroup + 1)(using phase.prepareForBind(tree))
  }

  def goBind(tree: Bind, start: Int)(using Context): Tree = {
    val phase = nxBindTransPhase(start)
    if (phase == null) tree
    else phase.transformBind(tree) match {
      case tree1: Bind => goBind(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepAlternative(tree: Alternative, start: Int)(using Context): Context = {
    val phase = nxAlternativePrepPhase(start)
    if (phase == null) ctx
    else prepAlternative(tree, phase.idxInGroup + 1)(using phase.prepareForAlternative(tree))
  }

  def goAlternative(tree: Alternative, start: Int)(using Context): Tree = {
    val phase = nxAlternativeTransPhase(start)
    if (phase == null) tree
    else phase.transformAlternative(tree) match {
      case tree1: Alternative => goAlternative(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepUnApply(tree: UnApply, start: Int)(using Context): Context = {
    val phase = nxUnApplyPrepPhase(start)
    if (phase == null) ctx
    else prepUnApply(tree, phase.idxInGroup + 1)(using phase.prepareForUnApply(tree))
  }

  def goUnApply(tree: UnApply, start: Int)(using Context): Tree = {
    val phase = nxUnApplyTransPhase(start)
    if (phase == null) tree
    else phase.transformUnApply(tree) match {
      case tree1: UnApply => goUnApply(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepValDef(tree: ValDef, start: Int)(using Context): Context = {
    val phase = nxValDefPrepPhase(start)
    if (phase == null) ctx
    else prepValDef(tree, phase.idxInGroup + 1)(using phase.prepareForValDef(tree))
  }

  def goValDef(tree: ValDef, start: Int)(using Context): Tree = {
    val phase = nxValDefTransPhase(start)
    if (phase == null) tree
    else phase.transformValDef(tree) match {
      case tree1: ValDef => goValDef(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepDefDef(tree: DefDef, start: Int)(using Context): Context = {
    val phase = nxDefDefPrepPhase(start)
    if (phase == null) ctx
    else prepDefDef(tree, phase.idxInGroup + 1)(using phase.prepareForDefDef(tree))
  }

  def goDefDef(tree: DefDef, start: Int)(using Context): Tree = {
    val phase = nxDefDefTransPhase(start)
    if (phase == null) tree
    else phase.transformDefDef(tree) match {
      case tree1: DefDef => goDefDef(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTypeDef(tree: TypeDef, start: Int)(using Context): Context = {
    val phase = nxTypeDefPrepPhase(start)
    if (phase == null) ctx
    else prepTypeDef(tree, phase.idxInGroup + 1)(using phase.prepareForTypeDef(tree))
  }

  def goTypeDef(tree: TypeDef, start: Int)(using Context): Tree = {
    val phase = nxTypeDefTransPhase(start)
    if (phase == null) tree
    else phase.transformTypeDef(tree) match {
      case tree1: TypeDef => goTypeDef(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTemplate(tree: Template, start: Int)(using Context): Context = {
    val phase = nxTemplatePrepPhase(start)
    if (phase == null) ctx
    else prepTemplate(tree, phase.idxInGroup + 1)(using phase.prepareForTemplate(tree))
  }

  def goTemplate(tree: Template, start: Int)(using Context): Tree = {
    val phase = nxTemplateTransPhase(start)
    if (phase == null) tree
    else phase.transformTemplate(tree) match {
      case tree1: Template => goTemplate(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepPackageDef(tree: PackageDef, start: Int)(using Context): Context = {
    val phase = nxPackageDefPrepPhase(start)
    if (phase == null) ctx
    else prepPackageDef(tree, phase.idxInGroup + 1)(using phase.prepareForPackageDef(tree))
  }

  def goPackageDef(tree: PackageDef, start: Int)(using Context): Tree = {
    val phase = nxPackageDefTransPhase(start)
    if (phase == null) tree
    else phase.transformPackageDef(tree) match {
      case tree1: PackageDef => goPackageDef(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepStats(trees: List[Tree], start: Int)(using Context): Context = {
    val phase = nxStatsPrepPhase(start)
    if (phase == null) ctx
    else prepStats(trees, phase.idxInGroup + 1)(using phase.prepareForStats(trees))
  }

  def goStats(trees: List[Tree], start: Int)(using Context): List[Tree] = {
    val phase = nxStatsTransPhase(start)
    if (phase == null) trees
    else goStats(phase.transformStats(trees), phase.idxInGroup + 1)
  }

  def prepUnit(tree: Tree, start: Int)(using Context): Context = {
    val phase = nxUnitPrepPhase(start)
    if (phase == null) ctx
    else prepUnit(tree, phase.idxInGroup + 1)(using phase.prepareForUnit(tree))
  }

  def goUnit(tree: Tree, start: Int)(using Context): Tree = {
    val phase = nxUnitTransPhase(start)
    if (phase == null) tree
    else goUnit(phase.transformUnit(tree), phase.idxInGroup + 1)
  }

  def prepOther(tree: Tree, start: Int)(using Context): Context = {
    val phase = nxOtherPrepPhase(start)
    if (phase == null) ctx
    else prepOther(tree, phase.idxInGroup + 1)(using phase.prepareForOther(tree))
  }

  def goOther(tree: Tree, start: Int)(using Context): Tree = {
    val phase = nxOtherTransPhase(start)
    if (phase == null) tree
    else goOther(phase.transformOther(tree), phase.idxInGroup + 1)
  }
}
