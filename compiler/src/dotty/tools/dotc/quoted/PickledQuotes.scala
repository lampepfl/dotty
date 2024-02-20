package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.config.Printers.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.tasty.{ PositionPickler, TastyPickler, TastyPrinter, TreePickler, Attributes }
import dotty.tools.dotc.core.tasty.DottyUnpickler
import dotty.tools.dotc.core.tasty.TreeUnpickler.UnpickleMode
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.Message

import scala.quoted.Quotes
import scala.quoted.runtime.impl.*

import scala.collection.mutable

import QuoteUtils.*
import dotty.tools.io.NoAbstractFile

object PickledQuotes {
  import tpd.*

  /** Pickle the tree of the quote into strings */
  def pickleQuote(tree: Tree)(using Context): List[String] =
    if (ctx.reporter.hasErrors) Nil
    else {
      assert(!tree.isInstanceOf[Hole]) // Should not be pickled as it represents `'{$x}` which should be optimized to `x`
      val pickled = pickle(tree)
      TastyString.pickle(pickled)
    }

  /** Transform the expression into its fully spliced Tree */
  def quotedExprToTree[T](expr: quoted.Expr[T])(using Context): Tree = {
    val expr1 = expr.asInstanceOf[ExprImpl]
    ScopeException.checkInCorrectScope(expr1.scope, SpliceScope.getCurrent, expr1.tree, "Expr")
    changeOwnerOfTree(expr1.tree, ctx.owner)
  }

  /** Transform the expression into its fully spliced TypeTree */
  def quotedTypeToTree(tpe: quoted.Type[?])(using Context): Tree = {
    val tpe1 = tpe.asInstanceOf[TypeImpl]
    ScopeException.checkInCorrectScope(tpe1.scope, SpliceScope.getCurrent, tpe1.typeTree, "Type")
    changeOwnerOfTree(tpe1.typeTree, ctx.owner)
  }

  /** `typeHole`/`types` argument of `QuoteUnpickler.{unpickleExpr,unpickleExprV2,unpickleType,unpickleTypeV2}` */
  enum TypeHole:
    /** `termHole` argument of `QuoteUnpickler.{unpickleExpr, unpickleType}`.
     *  From code compiled with Scala 3.0.x and 3.1.x.
     *  Note: For `unpickleType` it will always be `null`.
     */
    case V1(evalHole: Null | ((Int, Seq[scala.quoted.Type[?]]) => scala.quoted.Type[?]))
    /** `termHole` argument of `QuoteUnpickler.unpickleExprV2`
     *  From code compiled with Scala 3.2.0+
     */
    case V2(types: Null | Seq[scala.quoted.Type[?]])

    def isEmpty: Boolean = this match
      case V1(evalHole) => evalHole == null
      case V2(types) => types == null

  enum ExprHole:
    /** `termHole` argument of `QuoteUnpickler.{unpickleExpr, unpickleType}`.
     *  From code compiled with Scala 3.0.x and 3.1.x.
     *  Note: For `unpickleType` it will always be `null`.
     */
    case V1(evalHole: Null | ((Int, Seq[ExprHole.ArgV1], scala.quoted.Quotes) => scala.quoted.Expr[?]))
    /** `termHole` argument of `QuoteUnpickler.unpickleExprV2`
     *  From code compiled with Scala 3.2.0+
     */
    case V2(evalHole: Null | ((Int, Seq[ExprHole.ArgV2], scala.quoted.Quotes) => scala.quoted.Expr[?]))

  object ExprHole:
    type ArgV1 = scala.quoted.Type[?] | (Quotes ?=> scala.quoted.Expr[Any])
    type ArgV2 = scala.quoted.Type[?] | scala.quoted.Expr[Any]

  /** Unpickle the tree contained in the TastyExpr */
  def unpickleTerm(pickled: String | List[String], typeHole: TypeHole, termHole: ExprHole)(using Context): Tree = {
    withMode(Mode.ReadPositions)(unpickle(pickled, isType = false)) match
      case tree @ Inlined(call, Nil, expansion) =>
        val inlineCtx = inlineContext(tree)
        val expansion1 = spliceTypes(expansion, typeHole)(using inlineCtx)
        val expansion2 = spliceTerms(expansion1, typeHole, termHole)(using inlineCtx)
        cpy.Inlined(tree)(call, Nil, expansion2)
  }


  /** Unpickle the tree contained in the TastyType */
  def unpickleTypeTree(pickled: String | List[String], typeHole: TypeHole)(using Context): Tree = {
    val unpickled = withMode(Mode.ReadPositions)(unpickle(pickled, isType = true))
    spliceTypes(unpickled, typeHole)
  }

  /** Replace all term holes with the spliced terms */
  private def spliceTerms(tree: Tree, typeHole: TypeHole, termHole: ExprHole)(using Context): Tree = {
    def evaluateHoles = new TreeMapWithPreciseStatContexts {
      override def transform(tree: tpd.Tree)(using Context): tpd.Tree = tree match {
        case Hole(isTerm, idx, args, _) =>
          inContext(SpliceScope.contextWithNewSpliceScope(tree.sourcePos)) {
            if isTerm then
              val quotedExpr = termHole match
                case ExprHole.V1(evalHole) =>
                  evalHole.nn.apply(idx, reifyExprHoleV1Args(args), QuotesImpl())
                case ExprHole.V2(evalHole) =>
                  evalHole.nn.apply(idx, reifyExprHoleV2Args(args), QuotesImpl())

              val filled = PickledQuotes.quotedExprToTree(quotedExpr)

              // We need to make sure a hole is created with the source file of the surrounding context, even if
              // it filled with contents a different source file.
              if filled.source == ctx.source then filled
              else filled.cloneIn(ctx.source).withSpan(tree.span)
            else
              // For backwards compatibility with 3.0.x and 3.1.x
              // In 3.2.0+ all these holes are handled by `spliceTypes` before we call `spliceTerms`.
              //
              // Replaces type holes generated by PickleQuotes (non-spliced types).
              // These are types defined in a quote and used at the same level in a nested quote.
              val TypeHole.V1(evalHole) = typeHole: @unchecked
              val quotedType = evalHole.nn.apply(idx, reifyTypeHoleArgs(args))
              PickledQuotes.quotedTypeToTree(quotedType)
          }
        case tree =>
          if tree.isDef then
            tree.symbol.annotations = tree.symbol.annotations.map {
              annot => annot.derivedAnnotation(transform(annot.tree))
            }
          end if

         val tree1 = super.transform(tree)
         tree1.withType(mapAnnots(tree1.tpe))
      }

      // Evaluate holes in type annotations
      private val mapAnnots = new TypeMap {
        override def apply(tp: Type): Type = {
            tp match
              case tp @ AnnotatedType(underlying, annot) =>
                val underlying1 = this(underlying)
                derivedAnnotatedType(tp, underlying1, annot.derivedAnnotation(transform(annot.tree)))
              case _ => mapOver(tp)
        }
      }
    }
    val tree1 = termHole match
      case ExprHole.V2(null) => tree
      case _ => evaluateHoles.transform(tree)
    quotePickling.println(i"**** evaluated quote\n$tree1")
    tree1
  }

  /** Replace all type holes generated with the spliced types */
  private def spliceTypes(tree: Tree, typeHole: TypeHole)(using Context): Tree = {
    if typeHole.isEmpty then tree
    else tree match
      case Block(stat :: rest, expr1) if stat.symbol.hasAnnotation(defn.QuotedRuntime_SplicedTypeAnnot) =>
        val typeSpliceMap = (stat :: rest).iterator.map {
          case tdef: TypeDef =>
            assert(tdef.symbol.hasAnnotation(defn.QuotedRuntime_SplicedTypeAnnot))
            val tree = typeHole match
              case TypeHole.V1(evalHole) =>
                tdef.rhs match
                  case TypeBoundsTree(_, Hole(_, idx, args, _), _) =>
                    // To keep for backwards compatibility. In some older version holes where created in the bounds.
                    val quotedType = evalHole.nn.apply(idx, reifyTypeHoleArgs(args))
                    PickledQuotes.quotedTypeToTree(quotedType)
                  case TypeBoundsTree(_, tpt, _) =>
                    // To keep for backwards compatibility. In some older version we missed the creation of some holes.
                    tpt
              case TypeHole.V2(types) =>
                val Hole(_, idx, _, _) = tdef.rhs: @unchecked
                PickledQuotes.quotedTypeToTree(types.nn.apply(idx))
            (tdef.symbol, tree.tpe)
        }.toMap
        class ReplaceSplicedTyped extends TypeMap() {
          override def apply(tp: Type): Type = tp match {
            case tp: ClassInfo =>
              tp.derivedClassInfo(declaredParents = tp.declaredParents.map(apply))
            case tp: TypeRef =>
              typeSpliceMap.get(tp.symbol) match
                case Some(t) if tp.typeSymbol.hasAnnotation(defn.QuotedRuntime_SplicedTypeAnnot) => mapOver(t)
                case _ => mapOver(tp)
            case _ =>
              mapOver(tp)
          }
        }
        val expansion2 = new TreeTypeMap(new ReplaceSplicedTyped).transform(expr1)
        quotePickling.println(i"**** typed quote\n${expansion2.show}")
        expansion2
      case _ =>
        tree
  }

  def reifyTypeHoleArgs(args: List[Tree])(using Context): List[scala.quoted.Type[?]] =
    args.map(arg => new TypeImpl(arg, SpliceScope.getCurrent))

  def reifyExprHoleV1Args(args: List[Tree])(using Context): List[ExprHole.ArgV1] =
    args.map { arg =>
      if arg.isTerm then (q: Quotes) ?=> new ExprImpl(arg, SpliceScope.getCurrent)
      else new TypeImpl(arg, SpliceScope.getCurrent)
    }

  def reifyExprHoleV2Args(args: List[Tree])(using Context): List[ExprHole.ArgV2] =
    args.map { arg =>
      if arg.isTerm then new ExprImpl(arg, SpliceScope.getCurrent)
      else new TypeImpl(arg, SpliceScope.getCurrent)
    }

  // TASTY picklingtests/pos/quoteTest.scala

  /** Pickle tree into it's TASTY bytes s*/
  private def pickle(tree: Tree)(using Context): Array[Byte] = {
    quotePickling.println(i"**** pickling quote of\n$tree")
    val pickler = new TastyPickler(defn.RootClass, isBestEffortTasty = false)
    val treePkl = new TreePickler(pickler, Attributes.empty)
    treePkl.pickle(tree :: Nil)
    treePkl.compactify()
    if tree.span.exists then
      val positionWarnings = new mutable.ListBuffer[Message]()
      val reference = ctx.settings.sourceroot.value
      PositionPickler.picklePositions(pickler, treePkl.buf.addrOfTree, treePkl.treeAnnots, reference,
        ctx.compilationUnit.source, tree :: Nil, positionWarnings)
      positionWarnings.foreach(report.warning(_))

    val pickled = pickler.assembleParts()
    quotePickling.println(s"**** pickled quote\n${TastyPrinter.showContents(pickled, ctx.settings.color.value == "never", isBestEffortTasty = false)}")
    pickled
  }

  /** Unpickle TASTY bytes into it's tree */
  private def unpickle(pickled: String | List[String], isType: Boolean)(using Context): Tree = {
    QuotesCache.getTree(pickled) match
      case Some(tree) =>
        quotePickling.println(s"**** Using cached quote for TASTY\n$tree")
        treeOwner(tree) match
          case Some(owner) =>
            // Copy the cached tree to make sure the all definitions are unique.
            TreeTypeMap(oldOwners = List(owner), newOwners = List(owner)).apply(tree)
          case _ =>
            tree

      case _ =>
        val bytes = pickled match
          case pickled: String => TastyString.unpickle(pickled)
          case pickled: List[String] => TastyString.unpickle(pickled)

        val unpicklingContext =
          if ctx.owner.isClass then
            // When a quote is unpickled with a Quotes context that that has a class `spliceOwner`
            // we need to use a dummy owner to unpickle it. Otherwise any definitions defined
            // in the quoted block would be accidentally entered in the class.
            // When splicing this expression, this owner is replaced with the correct owner (see `quotedExprToTree` and `quotedTypeToTree` above).
            // On the other hand, if the expression is used as a reflect term, the user must call `changeOwner` (same as with other expressions used within a nested owner).
            // `-Xcheck-macros` will check for inconsistent owners and provide the users hints on how to improve them.
            //
            // Quotes context that that has a class `spliceOwner` can come from a macro annotation
            // or a user setting it explicitly using `Symbol.asQuotes`.
            ctx.withOwner(newSymbol(ctx.owner, "$quoteOwnedByClass$".toTermName, Private, defn.AnyType, NoSymbol))
            else ctx

        inContext(unpicklingContext) {

          quotePickling.println(s"**** unpickling quote from TASTY\n${TastyPrinter.showContents(bytes, ctx.settings.color.value == "never", isBestEffortTasty = false)}")

          val mode = if (isType) UnpickleMode.TypeTree else UnpickleMode.Term
          val unpickler = new DottyUnpickler(NoAbstractFile, bytes, isBestEffortTasty = false, mode)
          unpickler.enter(Set.empty)

          val tree = unpickler.tree
          QuotesCache(pickled) = tree

          // Make sure trees and positions are fully loaded
          tree.foreachSubTree(identity)

          quotePickling.println(i"**** unpickled quote\n$tree")

          tree
        }

  }

}
