package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.config.Printers._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.tasty.TreePickler.Hole
import dotty.tools.dotc.core.tasty.{ PositionPickler, TastyPickler, TastyPrinter }
import dotty.tools.dotc.core.tasty.DottyUnpickler
import dotty.tools.dotc.core.tasty.TreeUnpickler.UnpickleMode
import dotty.tools.dotc.report

import dotty.tools.tasty.TastyString

import scala.reflect.ClassTag

import scala.internal.quoted.Unpickler._
import scala.quoted.QuoteContext
import scala.collection.mutable
import dotty.tools.dotc.util
import util.Lst; // import Lst.::
import util.Lst.{NIL, +:, toLst}

object PickledQuotes {
  import tpd._

  /** Pickle the tree of the quote into strings */
  def pickleQuote(tree: Tree)(using Context): PickledQuote =
    if (ctx.reporter.hasErrors) Nil
    else {
      assert(!tree.isInstanceOf[Hole]) // Should not be pickled as it represents `'{$x}` which should be optimized to `x`
      val pickled = pickle(tree)
      TastyString.pickle(pickled)
    }

  /** Transform the expression into its fully spliced Tree */
  def quotedExprToTree[T](expr: quoted.Expr[T])(using Context): Tree = {
    val expr1 = expr.asInstanceOf[scala.internal.quoted.Expr[Tree]]
    QuoteContextImpl.checkScopeId(expr1.scopeId)
    healOwner(expr1.tree)
  }

  /** Transform the expression into its fully spliced TypeTree */
  def quotedTypeToTree(tpe: quoted.Type[?])(using Context): Tree = {
    val tpe1 = tpe.asInstanceOf[scala.internal.quoted.Type[Tree]]
    QuoteContextImpl.checkScopeId(tpe1.scopeId)
    healOwner(tpe1.typeTree)
  }

  /** Unpickle the tree contained in the TastyExpr */
  def unpickleExpr(tasty: PickledQuote, splices: PickledArgs)(using Context): Tree = {
    val tastyBytes = TastyString.unpickle(tasty)
    val unpickled = withMode(Mode.ReadPositions)(
      unpickle(tastyBytes, splices, isType = false))
    val Inlined(call, NIL, expansion) = unpickled
    val inlineCtx = inlineContext(call)
    val expansion1 = spliceTypes(expansion, splices)(using inlineCtx)
    val expansion2 = spliceTerms(expansion1, splices)(using inlineCtx)
    cpy.Inlined(unpickled)(call, NIL, expansion2)
  }

  /** Unpickle the tree contained in the TastyType */
  def unpickleType(tasty: PickledQuote, args: PickledArgs)(using Context): Tree = {
    val tastyBytes = TastyString.unpickle(tasty)
    val unpickled = withMode(Mode.ReadPositions)(
      unpickle(tastyBytes, args, isType = true))
    spliceTypes(unpickled, args)
  }

  /** Replace all term holes with the spliced terms */
  private def spliceTerms(tree: Tree, splices: PickledArgs)(using Context): Tree = {
    val evaluateHoles = new TreeMap {
      override def transform(tree: tpd.Tree)(using Context): tpd.Tree = tree match {
        case Hole(isTerm, idx, args) =>
          val reifiedArgs = args.map { arg =>
            if (arg.isTerm) (using qctx: QuoteContext) => new scala.internal.quoted.Expr(arg, QuoteContextImpl.scopeId)
            else new scala.internal.quoted.Type(arg, QuoteContextImpl.scopeId)
          }
          if isTerm then
            val splice1 = splices(idx).asInstanceOf[Seq[Any] => QuoteContext ?=> quoted.Expr[?]]
            val quotedExpr = splice1(reifiedArgs)(using dotty.tools.dotc.quoted.QuoteContextImpl())
            val filled = PickledQuotes.quotedExprToTree(quotedExpr)

            // We need to make sure a hole is created with the source file of the surrounding context, even if
            // it filled with contents a different source file.
            if filled.source == ctx.source then filled
            else filled.cloneIn(ctx.source).withSpan(tree.span)
          else
            // Replaces type holes generated by ReifyQuotes (non-spliced types).
            // These are types defined in a quote and used at the same level in a nested quote.
            val quotedType = splices(idx).asInstanceOf[Seq[Any] => quoted.Type[?]](reifiedArgs)
            PickledQuotes.quotedTypeToTree(quotedType)
        case tree: Select =>
          // Retain selected members
          val qual = transform(tree.qualifier)
          qual.select(tree.symbol).withSpan(tree.span)

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
    val tree1 = evaluateHoles.transform(tree)
    quotePickling.println(i"**** evaluated quote\n$tree1")
    tree1
  }

  /** Replace all type holes generated with the spliced types */
  private def spliceTypes(tree: Tree, splices: PickledArgs)(using Context): Tree = {
    tree match
      case Block(stats, expr1) if stats.nonEmpty && stats.head.symbol.hasAnnotation(defn.InternalQuoted_QuoteTypeTagAnnot) =>
        val typeSpliceMap = stats.iterator.map {
          case tdef: TypeDef =>
            assert(tdef.symbol.hasAnnotation(defn.InternalQuoted_QuoteTypeTagAnnot))
            val tree = tdef.rhs match
              case TypeBoundsTree(_, Hole(_, idx, args), _) =>
                val quotedType = splices(idx).asInstanceOf[Seq[Any] => quoted.Type[?]](args)
                PickledQuotes.quotedTypeToTree(quotedType)
              case TypeBoundsTree(_, tpt, _) =>
                tpt
            (tdef.symbol, tree.tpe)
        }.toMap
        class ReplaceSplicedTyped extends TypeMap() {
          override def apply(tp: Type): Type = tp match {
            case tp: ClassInfo =>
              tp.derivedClassInfo(classParents = tp.classParents.map(apply))
            case tp: TypeRef =>
              typeSpliceMap.get(tp.symbol) match
                case Some(t) if tp.typeSymbol.hasAnnotation(defn.InternalQuoted_QuoteTypeTagAnnot) => mapOver(t)
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

  // TASTY picklingtests/pos/quoteTest.scala

  /** Pickle tree into it's TASTY bytes s*/
  private def pickle(tree: Tree)(using Context): Array[Byte] = {
    quotePickling.println(i"**** pickling quote of\n$tree")
    val pickler = new TastyPickler(defn.RootClass)
    val treePkl = pickler.treePkl
    treePkl.pickle(tree :: Nil)
    treePkl.compactify()
    if tree.span.exists then
      val positionWarnings = new mutable.ListBuffer[String]()
      new PositionPickler(pickler, treePkl.buf.addrOfTree, treePkl.treeAnnots)
        .picklePositions(tree :: Nil, positionWarnings)
      positionWarnings.foreach(report.warning(_))

    val pickled = pickler.assembleParts()
    quotePickling.println(s"**** pickled quote\n${new TastyPrinter(pickled).printContents()}")
    pickled
  }

  /** Unpickle TASTY bytes into it's tree */
  private def unpickle(bytes: Array[Byte], splices: Seq[Any], isType: Boolean)(using Context): Tree = {
    quotePickling.println(s"**** unpickling quote from TASTY\n${new TastyPrinter(bytes).printContents()}")

    val mode = if (isType) UnpickleMode.TypeTree else UnpickleMode.Term
    val unpickler = new DottyUnpickler(bytes, mode)
    unpickler.enter(Set.empty)

    val tree = unpickler.tree

    // Make sure trees and positions are fully loaded
    new TreeTraverser {
      def traverse(tree: Tree)(using Context): Unit = traverseChildren(tree)
    }.traverse(tree)

    quotePickling.println(i"**** unpickled quote\n$tree")
    tree
  }

  /** Make sure that the owner of this tree is `ctx.owner` */
  def healOwner(tree: Tree)(using Context): Tree = {
    val getCurrentOwner = new TreeAccumulator[Option[Symbol]] {
      def apply(x: Option[Symbol], tree: tpd.Tree)(using Context): Option[Symbol] =
        if (x.isDefined) x
        else tree match {
          case tree: DefTree => Some(tree.symbol.owner)
          case _ => foldOver(x, tree)
        }
    }
    getCurrentOwner(None, tree) match {
      case Some(owner) if owner != ctx.owner => tree.changeOwner(owner, ctx.owner)
      case _ => tree
    }
  }
}
