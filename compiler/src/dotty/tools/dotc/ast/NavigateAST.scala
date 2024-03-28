package dotty.tools.dotc
package ast

import core.Contexts.*
import core.Decorators.*
import util.Spans.*
import Trees.{Closure, MemberDef, DefTree, WithLazyFields}
import dotty.tools.dotc.core.Types.AnnotatedType
import dotty.tools.dotc.core.Types.ImportType
import dotty.tools.dotc.core.Types.Type

/** Utility functions to go from typed to untyped ASTs */
// TODO: Handle trees with mixed source files
object NavigateAST {

  /** The untyped tree corresponding to typed tree `tree` in the compilation
   *  unit specified by `ctx`
   */
  def toUntyped(tree: tpd.Tree)(using Context): untpd.Tree =
    untypedPath(tree, exactMatch = true) match {
      case (utree: untpd.Tree) :: _ =>
        utree
      case _ =>
        val loosePath = untypedPath(tree, exactMatch = false)
        throw new
          Error(i"""no untyped tree for $tree, pos = ${tree.sourcePos}
                   |best matching path =\n$loosePath%\n====\n%
                   |path positions = ${loosePath.map(_.sourcePos)}""")
    }

  /** The reverse path of untyped trees starting with a tree that closest matches
   *  `tree` and ending in the untyped tree at the root of the compilation unit
   *  specified by `ctx`.
   *  @param exactMatch    If `true`, the path must start with a node that exactly
   *                       matches `tree`, or `Nil` is returned.
   *                       If `false` the path might start with a node enclosing
   *                       the logical position of `tree`.
   *  Note: A complication concerns member definitions. ValDefs and DefDefs
   *  have after desugaring a position that spans just the name of the symbol being
   *  defined and nothing else. So we look instead for an untyped tree approximating the
   *  envelope of the definition, and declare success if we find another DefTree.
   */
  def untypedPath(tree: tpd.Tree, exactMatch: Boolean = false)(using Context): List[Positioned] =
    tree match {
      case tree: MemberDef[?] =>
        untypedPath(tree.span) match {
          case path @ (last: DefTree[?]) :: _ => path
          case path if !exactMatch => path
          case _ => Nil
        }
      case _ =>
        untypedPath(tree.span) match {
          case (path @ last :: _) if last.span == tree.span || !exactMatch => path
          case _ => Nil
        }
    }

  /** The reverse part of the untyped root of the compilation unit of `ctx` to
   *  the given `span`.
   */
  def untypedPath(span: Span)(using Context): List[Positioned] =
    pathTo(span, List(ctx.compilationUnit.untpdTree))


  /** The reverse path from any node in `from` to the node that closest encloses `span`,
   *  or `Nil` if no such path exists. If a non-empty path is returned it starts with
   *  the node closest enclosing `span` and ends with one of the nodes in `from`.
   *
   *  @param skipZeroExtent  If true, skip over zero-extent nodes in the search. These nodes
   *                         do not correspond to code the user wrote since their start and
   *                         end point are the same, so this is useful when trying to reconcile
   *                         nodes with source code.
   */
  def pathTo(span: Span, from: List[Positioned], skipZeroExtent: Boolean = false)(using Context): List[Positioned] = {
    def childPath(it: Iterator[Any], path: List[Positioned]): List[Positioned] = {
      var bestFit: List[Positioned] = path
      while (it.hasNext) {
        val path1 = it.next() match {
          case p: Positioned if !p.isInstanceOf[Closure[?]] => singlePath(p, path)
          case m: untpd.Modifiers => childPath(m.productIterator, path)
          case xs: List[?] => childPath(xs.iterator, path)
          case _ => path
        }
        if ((path1 ne path) &&
            ((bestFit eq path) ||
             bestFit.head.span != path1.head.span &&
             bestFit.head.span.contains(path1.head.span)))
          bestFit = path1
      }
      bestFit
    }
    /*
     * Annotations trees are located in the Type
     */
    def unpackAnnotations(t: Type, path: List[Positioned]): List[Positioned] =
      t match {
        case ann: AnnotatedType =>
            unpackAnnotations(ann.parent, childPath(ann.annot.tree.productIterator, path))
        case imp: ImportType =>
          childPath(imp.expr.productIterator, path)
        case other =>
          path
    }
    def singlePath(p: Positioned, path: List[Positioned]): List[Positioned] =
      if (p.span.exists && !(skipZeroExtent && p.span.isZeroExtent) && p.span.contains(span)) {
        // FIXME: We shouldn't be manually forcing trees here, we should replace
        // our usage of `productIterator` by something in `Positioned` that takes
        // care of low-level details like this for us.
        p match
          case p: WithLazyFields => p.forceFields()
          case _ =>
        val iterator = p match
          case defdef: DefTree[?] =>
            p.productIterator ++ defdef.mods.productIterator
          case _ =>
            p.productIterator
        childPath(iterator, p :: path)
      }
      else {
        p match {
          case t: untpd.TypeTree => unpackAnnotations(t.typeOpt, path)
          case _ => path
        }
      }
    childPath(from.iterator, Nil)
  }
}
