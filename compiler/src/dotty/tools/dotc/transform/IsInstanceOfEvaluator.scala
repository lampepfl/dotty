package dotty.tools.dotc
package transform

import dotty.tools.dotc.util.Positions._
import TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import core._
import Contexts.Context, Types._, Constants._, Decorators._, Symbols._
import TypeUtils._, TypeErasure._, Flags._, TypeApplications._
import reporting.diagnostic.messages._

/** Implements partial evaluation of `sc.isInstanceOf[Sel]` according to:
 *
 *  | Sel\sc      | trait                      | class                      | final class      |
 *  | ----------: | :------------------------: | :------------------------: | :--------------: |
 *  | trait       |               ?            |               ?            | statically known |
 *  | class       |               ?            | false if classes unrelated | statically known |
 *  | final class | false if classes unrelated | false if classes unrelated | statically known |
 *
 *  This is a generalized solution to raising an error on unreachable match
 *  cases and warnings on other statically known results of `isInstanceOf`.
 *
 *  This phase also warns if the erased type parameter of a parameterized type
 *  is used in a match where it would be erased to `Object` or if the
 *  typeparameters are removed. Both of these cases could cause surprising
 *  behavior for the users.
 *
 *  Steps taken:
 *
 *  1. `evalTypeApply` will establish the matrix and choose the appropriate
 *     handling for the case:
 *      - Sel/sc is a value class or scrutinee is `Any`
 *      - `handleStaticallyKnown`
 *      - `falseIfUnrelated` with `scrutinee <:< selector`
 *      - `handleFalseUnrelated`
 *      - leave as is (`happens`)
 *  2. Rewrite according to steps taken in 1
 */
class IsInstanceOfEvaluator extends MiniPhaseTransform { thisTransformer =>

  import dotty.tools.dotc.ast.tpd._

  val phaseName = "isInstanceOfEvaluator"

  /** Transforms a [TypeApply](dotty.tools.dotc.ast.Trees.TypeApply) in order to
   *  evaluate an `isInstanceOf` check according to the rules defined above.
   */
  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val defn = ctx.definitions

    /** Handles the four cases of statically known `isInstanceOf`s and gives
     *  the correct warnings, or an error if statically known to be false in
     *  match
     */
    def handleStaticallyKnown(select: Select, scrutinee: Type, selector: Type, inMatch: Boolean, pos: Position): Tree = {
      val scrutineeSubSelector = scrutinee <:< selector
      if (!scrutineeSubSelector && inMatch) {
        ctx.error(
          s"this case is unreachable due to `${selector.show}` not being a subclass of `${scrutinee.show}`",
          Position(pos.start - 5, pos.end - 5)
        )
        rewrite(select, to = false)
      } else if (!scrutineeSubSelector && !inMatch) {
        ctx.warning(
          s"this will always yield false since `${scrutinee.show}` is not a subclass of `${selector.show}` (will be optimized away)",
          pos
        )
        rewrite(select, to = false)
      } else if (scrutineeSubSelector && !inMatch) {
        ctx.warning(
          s"this will always yield true if the scrutinee is non-null, since `${scrutinee.show}` is a subclass of `${selector.show}` (will be optimized away)",
          pos
        )
        rewrite(select, to = true)
      } else /* if (scrutineeSubSelector && inMatch) */ rewrite(select, to = true)
    }

    /** Rewrites cases with unrelated types */
    def handleFalseUnrelated(select: Select, scrutinee: Type, selector: Type, inMatch: Boolean) =
      if (inMatch) {
        ctx.error(
          s"will never match since `${selector.show}` is not a subclass of `${scrutinee.show}`",
          Position(select.pos.start - 5, select.pos.end - 5)
        )
        rewrite(select, to = false)
      } else {
        ctx.warning(
          s"will always yield false since `${scrutinee.show}` is not a subclass of `${selector.show}`",
          select.pos
        )
        rewrite(select, to = false)
      }

    /** Rewrites the select to a boolean if `to` is false or if the qualifier
     *  is a value class.
     *
     *  If `to` is set to true and the qualifier is not a primitive, the
     *  instanceOf is replaced by a null check, since:
     *
     *  `scrutinee.isInstanceOf[Selector]` if `scrutinee eq null`
     */
    def rewrite(tree: Select, to: Boolean): Tree =
      if (!to || !tree.qualifier.tpe.widen.derivesFrom(defn.AnyRefAlias)) {
        val literal = Literal(Constant(to))
        if (!isPureExpr(tree.qualifier)) Block(List(tree.qualifier), literal)
        else literal
      } else
        Apply(tree.qualifier.select(defn.Object_ne), List(Literal(Constant(null))))

    /** Attempts to rewrite TypeApply to either `scrutinee ne null` or a
     *  constant
     */
    def evalTypeApply(tree: TypeApply): Tree =
      if (tree.symbol != defn.Any_isInstanceOf) tree
      else tree.fun match {
        case s: Select => {
          val scrutinee = erasure(s.qualifier.tpe.widen)
          val selector  = erasure(tree.args.head.tpe.widen)

          val scTrait = scrutinee.typeSymbol is Trait
          val scClass =
            scrutinee.typeSymbol.isClass &&
            !(scrutinee.typeSymbol is Trait) &&
            !(scrutinee.typeSymbol is Module)

          val scClassNonFinal = scClass && !(scrutinee.typeSymbol is Final)
          val scFinalClass    = scClass && (scrutinee.typeSymbol is Final)

          val selTrait = selector.typeSymbol is Trait
          val selClass =
            selector.typeSymbol.isClass &&
            !(selector.typeSymbol is Trait) &&
            !(selector.typeSymbol is Module)

          val selClassNonFinal = selClass && !(selector.typeSymbol is Final)
          val selFinalClass    = selClass && (selector.typeSymbol is Final)

          // Check if the selector's potential type parameters will be erased, and if so warn
          val selTypeParam = tree.args.head.tpe.widen match {
            case tp @ AppliedType(_, args @ (arg :: _)) =>
              // If the type is `Array[X]` where `X` is a primitive value
              // class. In the future, when we have a solid implementation of
              // Arrays of value classes, we might be able to relax this check.
              val anyValArray = tp.isRef(defn.ArrayClass) && arg.typeSymbol.isPrimitiveValueClass
              // param is: Any | AnyRef | java.lang.Object
              val topType = defn.ObjectType <:< arg
              // has @unchecked annotation to suppress warnings
              val hasUncheckedAnnot = arg.hasAnnotation(defn.UncheckedAnnot)

              // Shouldn't warn when matching on a subclass with underscore
              // params or type binding
              val matchingUnderscoresOrTypeBindings = args.forall(_ match {
                case tr: TypeRef =>
                  tr.symbol.is(BindDefinedType)
                case TypeBounds(lo, hi) =>
                  (lo eq defn.NothingType) && (hi eq defn.AnyType)
                case _ => false
              }) && selector <:< scrutinee

              // we don't want to warn when matching on `List` from `Seq` e.g:
              // (xs: Seq[Int]) match { case xs: List[Int] => ??? }
              val matchingSeqToList = {
                val hasSameTypeArgs = s.qualifier.tpe.widen match {
                  case AppliedType(_, scrutArg :: Nil) =>
                    (scrutArg eq arg) || arg <:< scrutArg
                  case _ => false
                }

                scrutinee.isRef(defn.SeqClass) &&
                tp.isRef(defn.ListClass) &&
                hasSameTypeArgs
              }

              val shouldWarn =
                !topType && !hasUncheckedAnnot &&
                !matchingUnderscoresOrTypeBindings && !matchingSeqToList &&
                !anyValArray

              if (shouldWarn) ctx.uncheckedWarning(
                ErasedType(hl"""|Since type parameters are erased, you should not match on them in
                                |${"match"} expressions."""),
                tree.pos
              )
              true
            case _ =>
              if (tree.args.head.symbol.is(TypeParam)) {
                ctx.uncheckedWarning(
                  ErasedType(
                    hl"""|`${tree.args.head.tpe}` will be erased to `${selector}`. Which means that the specified
                         |behavior could be different during runtime."""
                  ),
                  tree.pos
                )
                true
              }
              else false
          }

          // Cases ---------------------------------
          val valueClassesOrAny =
            ValueClasses.isDerivedValueClass(scrutinee.typeSymbol) ||
            ValueClasses.isDerivedValueClass(selector.typeSymbol)  ||
            scrutinee == defn.ObjectType

          val knownStatically = scFinalClass

          val falseIfUnrelated =
            (scClassNonFinal && selClassNonFinal) ||
            (scClassNonFinal && selFinalClass)    ||
            (scTrait && selFinalClass)

          val happens =
            (scClassNonFinal && selClassNonFinal) ||
            (scTrait && selClassNonFinal)         ||
            (scTrait && selTrait)

          val inMatch = s.qualifier.symbol is Case
            // FIXME: This will misclassify case objects! We need to find another way to characterize
            // isInstanceOfs generated by matches.
            // Probably the most robust way is to use another symbol for the isInstanceOf method.

          if (selTypeParam || valueClassesOrAny) tree
          else if (knownStatically)
            handleStaticallyKnown(s, scrutinee, selector, inMatch, tree.pos)
          else if (falseIfUnrelated && scrutinee <:< selector)
            // scrutinee is a subtype of the selector, safe to rewrite
            rewrite(s, to = true)
          else if (falseIfUnrelated && !(selector <:< scrutinee))
            // selector and scrutinee are unrelated
            handleFalseUnrelated(s, scrutinee, selector, inMatch)
          else if (happens) tree
          else tree
        }

        case _ => tree
      }

    evalTypeApply(tree)
  }
}
