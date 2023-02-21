package dotty.tools.dotc
package typer

import core._
import Contexts._
import Types._
import Symbols._
import StdNames._
import Decorators._
import typer.ProtoTypes._
import ast.{tpd, untpd}
import scala.util.control.NonFatal
import util.Spans.Span
import Nullables._

/** A version of Typer that keeps all symbols defined and referenced in a
 *  previously typed tree.
 *
 *  All definition nodes keep their symbols. All leaf nodes for idents, selects,
 *  and TypeTrees keep their types. Indexing is a no-op.
 *
 *  Otherwise, everything is as in Typer.
 */
class ReTyper(nestingLevel: Int = 0) extends Typer(nestingLevel) with ReChecking {
  import tpd._

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
    catch {
      case NonFatal(ex) if ctx.phase != Phases.typerPhase && ctx.phase != Phases.inliningPhase =>
        ctx.lateImplode(
          Exception(i"exception while typing $tree of class ${tree.getClass} # ${tree.uniqueId}", ex)
        )
        
    }

  override def inlineExpansion(mdef: DefDef)(using Context): List[Tree] = mdef :: Nil

  override def inferView(from: Tree, to: Type)(using Context): Implicits.SearchResult =
    Implicits.NoMatchingImplicitsFailure
  override def checkCanEqual(ltp: Type, rtp: Type, span: Span)(using Context): Unit = ()

  override def widenEnumCase(tree: Tree, pt: Type)(using Context): Tree = tree

  override protected def addAccessorDefs(cls: Symbol, body: List[Tree])(using Context): List[Tree] = body
  override protected def checkEqualityEvidence(tree: tpd.Tree, pt: Type)(using Context): Unit = ()
  override protected def matchingApply(methType: MethodOrPoly, pt: FunProto)(using Context): Boolean = true
  override protected def typedScala2MacroBody(call: untpd.Tree)(using Context): Tree = promote(call)
}
