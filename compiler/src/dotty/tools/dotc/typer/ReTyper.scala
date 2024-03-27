package dotty.tools.dotc
package typer

import core.*
import Contexts.*
import Types.*
import Symbols.*
import StdNames.*
import Decorators.*
import typer.ProtoTypes.*
import ast.{tpd, untpd}
import scala.util.control.NonFatal
import util.Spans.Span
import Nullables.*
import staging.StagingLevel.*

/** A version of Typer that keeps all symbols defined and referenced in a
 *  previously typed tree.
 *
 *  All definition nodes keep their symbols. All leaf nodes for idents, selects,
 *  and TypeTrees keep their types. Indexing is a no-op.
 *
 *  Otherwise, everything is as in Typer.
 */
class ReTyper(nestingLevel: Int = 0) extends Typer(nestingLevel) with ReChecking {
  import tpd.*

  private def assertTyped(tree: untpd.Tree)(using Context): Unit =
    assert(tree.hasType, i"$tree ${tree.getClass} ${tree.uniqueId}")

  /** Checks that the given tree has been typed */
  protected def promote(tree: untpd.Tree)(using Context): tree.ThisTree[Type] = {
    assertTyped(tree)
    tree.withType(tree.typeOpt)
  }

  override def typedIdent(tree: untpd.Ident, pt: Type)(using Context): Tree =
    promote(tree)

  override def typedSelect(tree: untpd.Select, pt: Type)(using Context): Tree = {
    assertTyped(tree)
    val qual1 = withoutMode(Mode.Pattern)(typed(tree.qualifier, AnySelectionProto))
    untpd.cpy.Select(tree)(qual1, tree.name).withType(tree.typeOpt)
  }

  override def typedLiteral(tree: untpd.Literal)(implicit ctc: Context): Tree =
    promote(tree)

  override def typedThis(tree: untpd.This)(using Context): Tree =
    promote(tree)

  override def typedSuper(tree: untpd.Super, pt: Type)(using Context): Tree =
    promote(tree)

  override def typedImport(tree: untpd.Import)(using Context): Tree =
    promote(tree)

  override def typedTyped(tree: untpd.Typed, pt: Type)(using Context): Tree = {
    assertTyped(tree)

    val tpt1 = checkSimpleKinded(typedType(tree.tpt))
    val expr1 = tree.expr match {
      case id: untpd.Ident if (ctx.mode is Mode.Pattern) && untpd.isVarPattern(id) && (id.name == nme.WILDCARD || id.name == nme.WILDCARD_STAR) =>
        tree.expr.withType(tpt1.tpe)
      case _ => typed(tree.expr)
    }
    val result = untpd.cpy.Typed(tree)(expr1, tpt1).withType(tree.typeOpt)
    if ctx.mode.isExpr then result.withNotNullInfo(expr1.notNullInfo) else result
  }

  override def typedTypeTree(tree: untpd.TypeTree, pt: Type)(using Context): TypeTree =
    promote(tree)

  override def typedRefinedTypeTree(tree: untpd.RefinedTypeTree)(using Context): TypTree =
    promote(TypeTree(tree.typeOpt).withSpan(tree.span))

  override def typedExport(exp: untpd.Export)(using Context): Export =
    promote(exp)

  override def typedBind(tree: untpd.Bind, pt: Type)(using Context): Bind = {
    assertTyped(tree)
    val body1 = typed(tree.body, pt)
    untpd.cpy.Bind(tree)(tree.name, body1).withType(tree.typeOpt)
  }

  override def typedUnApply(tree: untpd.UnApply, selType: Type)(using Context): UnApply = {
    val fun1 =
      // retract PatternOrTypeBits like in typedExpr
      withoutMode(Mode.PatternOrTypeBits)(typedUnadapted(tree.fun, AnyFunctionProto))
    val implicits1 = tree.implicits.map(typedExpr(_))
    val patterns1 = tree.patterns.mapconserve(pat => typed(pat, pat.typeOpt))
    untpd.cpy.UnApply(tree)(fun1, implicits1, patterns1).withType(tree.typeOpt)
  }

  override def typedUnApply(tree: untpd.Apply, selType: Type)(using Context): Tree =
    typedApply(tree, selType)

  override def typedInlined(tree: untpd.Inlined, pt: Type)(using Context): Tree = {
    val (bindings1, exprCtx) = typedBlockStats(tree.bindings)
    val expansion1 = typed(tree.expansion, pt)(using inlineContext(promote(tree))(using exprCtx))
    untpd.cpy.Inlined(tree)(tree.call, bindings1.asInstanceOf[List[MemberDef]], expansion1)
      .withType(avoidingType(expansion1, bindings1))
  }

  override def typedQuote(tree: untpd.Quote, pt: Type)(using Context): Tree =
    assertTyped(tree)
    val body1 = typed(tree.body, promote(tree).bodyType)(using quoteContext)
    for tag <- tree.tags do assertTyped(tag)
    untpd.cpy.Quote(tree)(body1, tree.tags).withType(tree.typeOpt)

  override def typedSplice(tree: untpd.Splice, pt: Type)(using Context): Tree =
    assertTyped(tree)
    val exprType = // Expr[T]
        defn.QuotedExprClass.typeRef.appliedTo(tree.typeOpt)
    val quoteType = // Quotes ?=> Expr[T]
      defn.FunctionType(1, isContextual = true)
        .appliedTo(defn.QuotesClass.typeRef, exprType)
    val expr1 = typed(tree.expr, quoteType)(using spliceContext)
    untpd.cpy.Splice(tree)(expr1).withType(tree.typeOpt)

  override def typedQuotePattern(tree: untpd.QuotePattern, pt: Type)(using Context): Tree =
    assertTyped(tree)
    val bindings1 = tree.bindings.map(typed(_))
    val bodyCtx = quoteContext
      .retractMode(Mode.Pattern)
      .addMode(if tree.body.isType then Mode.QuotedTypePattern else Mode.QuotedExprPattern)
    val body1 = typed(tree.body, promote(tree).bodyType)(using bodyCtx)
    val quotes1 = typed(tree.quotes, defn.QuotesClass.typeRef)
    untpd.cpy.QuotePattern(tree)(bindings1, body1, quotes1).withType(tree.typeOpt)

  override def typedSplicePattern(tree: untpd.SplicePattern, pt: Type)(using Context): Tree =
    assertTyped(tree)
    val args1 = tree.args.mapconserve(typedExpr(_))
    val patternTpe =
      if args1.isEmpty then tree.typeOpt
      else defn.FunctionType(args1.size).appliedTo(args1.map(_.tpe) :+ tree.typeOpt)
    val bodyCtx = spliceContext.addMode(Mode.Pattern).retractMode(Mode.QuotedPatternBits)
    val body1 = typed(tree.body, defn.QuotedExprClass.typeRef.appliedTo(patternTpe))(using bodyCtx)
    val args = tree.args.mapconserve(typedExpr(_))
    untpd.cpy.SplicePattern(tree)(body1, args1).withType(tree.typeOpt)

  override def typedHole(tree: untpd.Hole, pt: Type)(using Context): Tree =
    promote(tree)

  override def localDummy(cls: ClassSymbol, impl: untpd.Template)(using Context): Symbol = impl.symbol

  override def retrieveSym(tree: untpd.Tree)(using Context): Symbol = tree.symbol
  override def symbolOfTree(tree: untpd.Tree)(using Context): Symbol = tree.symbol

  override def localTyper(sym: Symbol): Typer = this

  override def index(trees: List[untpd.Tree])(using Context): Context = ctx

  override def tryInsertApplyOrImplicit(tree: Tree, pt: ProtoType, locked: TypeVars)(fallBack: => Tree)(using Context): Tree =
    fallBack

  override def completeAnnotations(mdef: untpd.MemberDef, sym: Symbol)(using Context): Unit = ()

  override def ensureConstrCall(cls: ClassSymbol, parent: Tree, psym: Symbol)(using Context): Tree =
    parent

  override def handleUnexpectedFunType(tree: untpd.Apply, fun: Tree)(using Context): Tree = fun.tpe match {
    case mt: MethodType =>
      val args: List[Tree] = tree.args.zipWithConserve(mt.paramInfos)(typedExpr)
      assignType(untpd.cpy.Apply(tree)(fun, args), fun, args)
    case _ =>
      super.handleUnexpectedFunType(tree, fun)
  }

  override def addCanThrowCapabilities(expr: untpd.Tree, cases: List[CaseDef])(using Context): untpd.Tree =
    expr

  override def typedUnadapted(tree: untpd.Tree, pt: Type, locked: TypeVars)(using Context): Tree =
    try super.typedUnadapted(tree, pt, locked)
    catch case NonFatal(ex) if ctx.phase != Phases.typerPhase && ctx.phase != Phases.inliningPhase && !ctx.run.enrichedErrorMessage =>
      val treeStr = tree.show(using ctx.withPhase(ctx.phase.prev.megaPhase))
      println(ctx.run.enrichErrorMessage(s"exception while retyping $treeStr of class ${tree.className} # ${tree.uniqueId}"))
      throw ex

  override def inlineExpansion(mdef: DefDef)(using Context): List[Tree] = mdef :: Nil

  override def inferView(from: Tree, to: Type)(using Context): Implicits.SearchResult =
    Implicits.NoMatchingImplicitsFailure
  override def checkCanEqual(ltp: Type, rtp: Type, span: Span)(using Context): Unit = ()

  override def widenEnumCase(tree: Tree, pt: Type)(using Context): Tree = tree

  override protected def addAccessorDefs(cls: Symbol, body: List[Tree])(using Context): List[Tree] = body
  override protected def checkEqualityEvidence(tree: tpd.Tree, pt: Type)(using Context): Unit = ()
  override protected def matchingApply(methType: MethodOrPoly, pt: FunProto)(using Context): Boolean = true
  override protected def typedScala2MacroBody(call: untpd.Tree)(using Context): Tree = promote(call)
  override protected def migrate[T](migration: => T, disabled: => T = ()): T = disabled
}
