package dotty.tools.dotc
package transform

import core.*
import Names.*
import dotty.tools.dotc.transform.MegaPhase.*
import ast.untpd
import Flags.*
import Types.*
import Constants.Constant
import Contexts.*
import Symbols.*
import Decorators.*
import scala.collection.mutable
import DenotTransformers.*
import NameOps.*
import NameKinds.OuterSelectName
import StdNames.*
import config.Feature
import inlines.Inlines.inInlineMethod

object FirstTransform {
  val name: String = "firstTransform"
  val description: String = "some transformations to put trees into a canonical form"
}

/** The first tree transform
 *   - eliminates some kinds of trees: Imports other than language imports,
 *     Exports, NamedArgs, type trees other than TypeTree
 *   - stubs out native methods
 *   - eliminates self tree in Template and self symbol in ClassInfo
 *   - collapses all type trees to trees of class TypeTree
 *   - converts idempotent expressions with constant types
 *   - drops branches of ifs using the rules
 *          if (true) A else B    ==> A
 *          if (false) A else B   ==> B
 */
class FirstTransform extends MiniPhase with InfoTransformer { thisPhase =>
  import ast.tpd.*

  override def phaseName: String = FirstTransform.name

  override def description: String = FirstTransform.description

  /** eliminate self symbol in ClassInfo */
  override def transformInfo(tp: Type, sym: Symbol)(using Context): Type = tp match {
    case tp @ ClassInfo(_, _, _, _, self: Symbol) =>
      tp.derivedClassInfo(selfInfo = self.info)
    case _ =>
      tp
  }

  override protected def infoMayChange(sym: Symbol)(using Context): Boolean = sym.isClass

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    tree match {
      case Select(qual, name) if !name.is(OuterSelectName) && tree.symbol.exists =>
        val qualTpe = qual.tpe
        assert(
          qualTpe.isErasedValueType || qualTpe.derivesFrom(tree.symbol.owner) ||
            tree.symbol.is(JavaStatic) && qualTpe.derivesFrom(tree.symbol.enclosingClass),
          i"non member selection of ${tree.symbol.showLocated} from ${qualTpe} in $tree")
      case _: TypeTree =>
      case _: Export | _: NamedArg | _: TypTree =>
        assert(false, i"illegal tree: $tree")
      case _ =>
    }

  /** Reorder statements so that module classes always come after their companion classes */
  private def reorderAndComplete(stats: List[Tree])(using Context): List[Tree] = {
    val moduleClassDefs, singleClassDefs = mutable.Map[Name, Tree]()

    /* Returns the result of reordering stats and prepending revPrefix in reverse order to it.
     * The result of reorder is equivalent to reorder(stats, revPrefix) = revPrefix.reverse ::: reorder(stats, Nil).
     * This implementation is tail recursive as long as the element is not a module TypeDef.
     */
    def reorder(stats: List[Tree], revPrefix: List[Tree]): List[Tree] = stats match {
      case (stat: TypeDef) :: stats1 if stat.symbol.isClass =>
        if (stat.symbol.is(Flags.Module)) {
          def pushOnTop(xs: List[Tree], ys: List[Tree]): List[Tree] =
            xs.foldLeft(ys)((ys, x) => x :: ys)
          moduleClassDefs += (stat.name -> stat)
          singleClassDefs -= stat.name.stripModuleClassSuffix
          val stats1r = reorder(stats1, Nil)
          pushOnTop(revPrefix, if (moduleClassDefs contains stat.name) stat :: stats1r else stats1r)
        }
        else
          reorder(
            stats1,
            moduleClassDefs remove stat.name.moduleClassName match {
              case Some(mcdef) =>
                mcdef :: stat :: revPrefix
              case None =>
                singleClassDefs += (stat.name -> stat)
                stat :: revPrefix
            }
          )
      case stat :: stats1 => reorder(stats1, stat :: revPrefix)
      case Nil => revPrefix.reverse
    }

    reorder(stats, Nil)
  }

  /** Eliminate self in Template
   *  Under captureChecking, we keep the self type `S` around in a type definition
   *
   *     private[this] type $this = S
   *
   *  This is so that the type can be checked for well-formedness in the CaptureCheck phase.
   */
  override def transformTemplate(impl: Template)(using Context): Tree =
    impl.self match
      case self: ValDef if !self.tpt.isEmpty && Feature.ccEnabled =>
        val tsym = newSymbol(ctx.owner, tpnme.SELF, PrivateLocal, TypeAlias(self.tpt.tpe))
        val tdef = untpd.cpy.TypeDef(self)(tpnme.SELF, self.tpt).withType(tsym.typeRef)
        cpy.Template(impl)(self = EmptyValDef, body = tdef :: impl.body)
      case _ =>
        cpy.Template(impl)(self = EmptyValDef)

  override def transformDefDef(ddef: DefDef)(using Context): Tree =
    val meth = ddef.symbol.asTerm
    if meth.hasAnnotation(defn.NativeAnnot) then
      meth.resetFlag(Deferred)
      DefDef(meth, _ =>
        ref(defn.Sys_error.termRef).withSpan(ddef.span)
          .appliedTo(Literal(Constant(s"native method stub"))))
    else ddef

  override def transformStats(trees: List[Tree])(using Context): List[Tree] =
    ast.Trees.flatten(atPhase(thisPhase.next)(reorderAndComplete(trees)))

  private object collectBinders extends TreeAccumulator[List[Ident]] {
    def apply(annots: List[Ident], t: Tree)(using Context): List[Ident] = t match {
      case t @ Bind(_, body) =>
        val annot = untpd.Ident(tpnme.BOUNDTYPE_ANNOT).withType(t.symbol.typeRef)
        apply(annot :: annots, body)
      case _ =>
        foldOver(annots, t)
    }
  }

  /** Replace type tree `t` of type `T` with `TypeTree(T)`, but record all
   *  nested Bind nodes in annotations. These are interpreted in TreeTypeMaps
   *  so that bound symbols can be properly copied.
   */
  private def toTypeTree(tree: Tree)(using Context) = {
    val binders = collectBinders.apply(Nil, tree)
    val result: Tree = TypeTree(tree.tpe).withSpan(tree.span)
    binders.foldLeft(result)(Annotated(_, _))
  }

  override def transformOther(tree: Tree)(using Context): Tree = tree match {
    case tree: Export => EmptyTree
    case tree: NamedArg => transformAllDeep(tree.arg)
    case tree => if (tree.isType) toTypeTree(tree) else tree
  }

  override def transformIdent(tree: Ident)(using Context): Tree =
    if (tree.isType) {
      toTypeTree(tree)
    } else if (tree.name != nme.WILDCARD) {
      // We constant-fold all idents except wildcards.
      // AFAIK, constant-foldable wildcard idents can only occur in patterns, for instance as `case _: "a"`.
      // Constant-folding that would result in `case "a": "a"`, which changes the meaning of the pattern.
      // Note that we _do_ want to constant-fold idents in patterns that _aren't_ wildcards -
      // for example, @switch annotation needs to see inlined literals and not indirect references.
      constToLiteral(tree)
    } else tree

  override def transformSelect(tree: Select)(using Context): Tree =
    if (tree.isType) toTypeTree(tree) else constToLiteral(tree)

  override def transformTypeApply(tree: TypeApply)(using Context): Tree =
    constToLiteral(tree)
  
  override def transformApply(tree: Apply)(using Context): Tree =
    constToLiteral(foldCondition(tree))

  override def transformTyped(tree: Typed)(using Context): Tree =
    // Singleton type cases (such as `case _: "a"`) are constant-foldable.
    // We avoid constant-folding those as doing so would change the meaning of the pattern (see transformIdent).
    if (!ctx.mode.is(Mode.Pattern)) constToLiteral(tree) else tree

  override def transformBlock(tree: Block)(using Context): Tree =
    constToLiteral(tree)

  override def transformIf(tree: If)(using Context): Tree =
    tree.cond.tpe match {
      case ConstantType(Constant(c: Boolean)) if isPureExpr(tree.cond) =>
        if (c) tree.thenp else tree.elsep
      case _ => tree
    }

  /** Perform one of the following simplification if applicable:
   *
   *      true  && y   ==>  y
   *      false && y   ==>  false
   *      true  || y   ==>  true
   *      false || y   ==>  y
   */
  private def foldCondition(tree: Apply)(using Context) = tree.fun match {
    case Select(x @ Literal(Constant(c: Boolean)), op) =>
      tree.args match {
        case y :: Nil if y.tpe.widen.isRef(defn.BooleanClass) =>
          op match {
            case nme.ZAND => if (c) y else x
            case nme.ZOR  => if (c) x else y
            case _ => tree
          }
        case _ => tree
      }
    case _ => tree
  }

  // invariants: all modules have companion objects
  // all types are TypeTrees
  // all this types are explicit
}
