package dotty.tools
package dotc
package ast

import util.Spans._
import util.{SourceFile, NoSource, SourcePosition}
import core.Contexts.{Context, SourceInfo}
import core.Decorators._
import core.Flags.{JavaDefined, Extension}
import core.StdNames.nme
import annotation.transientParam
import annotation.internal.sharable

/** A base class for things that have positions (currently: modifiers and trees)
 */
abstract class Positioned(implicit @transientParam src: SourceInfo) extends Product with Cloneable {

  /** A unique identifier. Among other things, used for determining the source file
   *  component of the position.
   */
  private var myUniqueId: Int = -1
  private[this] var mySpan: Span = NoSpan

  /** The span part of the item's position */
  def span: Span = mySpan

  /** item's id */
  def uniqueId: Int = myUniqueId

  def source: SourceFile = SourceFile.fromId(uniqueId)
  def sourcePos(implicit ctx: Context): SourcePosition = source.atSpan(span)

  //setId(initialSource(src).nextId)
  setPos(initialSpan(src), source)

  protected def setId(id: Int): Unit = {
    myUniqueId = id
    //assert(id != 2067, getClass)
  }

  /** Destructively update `mySpan` to given span and potentially update `id` so that
   *  it refers to `file`. Also, set any missing positions in children.
   */
  protected def setPos(span: Span, file: SourceFile): Unit = {
    setOnePos(span, file)
    if (span.exists) setChildPositions(span.toSynthetic, file)
  }

  /** A positioned item like this one with given `span`.
   *  If the positioned item is source-derived, a clone is returned.
   *  If the positioned item is synthetic, the position is updated
   *  destructively and the item itself is returned.
   */
  def withSpan(span: Span): this.type = {
    val ownSpan = this.span
    val newpd: this.type =
      if (span == ownSpan || ownSpan.isSynthetic) this else cloneIn(source)
    newpd.setPos(span, source)
    newpd
  }

  def withPosOf(posd: Positioned): this.type = {
    val ownSpan = this.span
    val newpd: this.type =
      if ((posd.source `eq` source) && posd.span == ownSpan || ownSpan.isSynthetic) this
      else cloneIn(posd.source)
    newpd.setPos(posd.span, posd.source)
    newpd
  }

  def withSourcePos(sourcePos: SourcePosition): this.type = {
    val ownSpan = this.span
    val newpd: this.type =
      if ((sourcePos.source `eq` source) && sourcePos.span == ownSpan || ownSpan.isSynthetic) this
      else cloneIn(sourcePos.source)
    newpd.setPos(sourcePos.span, sourcePos.source)
    newpd
  }

  /** Set span of this tree only, without updating children spans.
   *  Called from Unpickler when entering positions.
   */
  private[dotc] def setOnePos(span: Span, file: SourceFile = this.source): Unit = {
    if (file `ne` this.source) setId(file.nextId)
    mySpan = span
  }

  /** If any children of this node do not have spans,
   *  fit their spans between the spans of the known subtrees
   *  and transitively visit their children.
   *  The method is likely time-critical because it is invoked on any node
   *  we create, so we want to avoid object allocations in the common case.
   *  The method is naturally expressed as two mutually (tail-)recursive
   *  functions, one which computes the next element to consider or terminates if there
   *  is none and the other which propagates the span information to that element.
   *  But since mutual tail recursion is not supported in Scala, we express it instead
   *  as a while loop with a termination by return in the middle.
   */
  private def setChildPositions(span: Span, file: SourceFile): Unit = {
    var n = productArity                    // subnodes are analyzed right to left
    var elems: List[Any] = Nil              // children in lists still to be considered, from right to left
    var end = span.end                      // the last defined offset, fill in spans up to this offset
    var outstanding: List[Positioned] = Nil // nodes that need their spans filled once a start offset
                                            // is known, from left to right.
    def fillIn(ps: List[Positioned], start: Int, end: Int): Unit = ps match {
      case p :: ps1 =>
        // If a tree has no span or a zero-extent span, it should be
        // synthetic. We can preserve this invariant by always setting a
        // zero-extent span for these trees here.
        if (!p.span.exists || p.span.isZeroExtent) {
          p.setPos(Span(start, start), file)
          fillIn(ps1, start, end)
        } else {
          p.setPos(Span(start, end), file)
          fillIn(ps1, end, end)
        }
      case nil =>
    }
    while (true) {
      var nextChild: Any = null // the next child to be considered
      if (elems.nonEmpty) {
        nextChild = elems.head
        elems = elems.tail
      }
      else if (n > 0) {
        n = n - 1
        nextChild = productElement(n)
      }
      else {
        fillIn(outstanding, span.start, end)
        return
      }
      nextChild match {
        case p: Positioned =>
          if (p.span.exists) {
            fillIn(outstanding, p.span.end, end)
            outstanding = Nil
            end = p.span.start
          }
          else outstanding = p :: outstanding
        case m: untpd.Modifiers =>
          if (m.mods.nonEmpty || m.annotations.nonEmpty)
            elems = elems ::: m.mods.reverse ::: m.annotations.reverse
        case xs: List[_] =>
          elems = elems ::: xs.reverse
        case _ =>
      }
    }
  }

  /** Clone this node but assign it a fresh id which marks it as a node in `file`. */
  protected def cloneIn(file: SourceFile): this.type = {
    val newpd: this.type = clone.asInstanceOf[this.type]
    newpd.setId(file.nextId)
    newpd
  }

  /** The initial, synthetic span. This is usually the union of all positioned children's spans.
   */
  def initialSpan(si: SourceInfo): Span = {

    def include(span1: Span, p2: Positioned): Span = {
      val span2 = p2.span
      if (span2.exists) {
        var src = if (uniqueId == -1) NoSource else source
        val src2 = p2.source
        if (src `eq` src2) span1.union(span2)
        else if (!src.exists) {
          setId(src2.nextId)
          if (span1.exists) initialSpan(si) // we might have some mis-classified children; re-run everything
          else span2
        }
        else span1 // sources differ: ignore child span
      }
      else span1
    }

    def includeAll(span: Span, xs: List[_]): Span = xs match {
      case Nil => span
      case (p: Positioned) :: xs1 => includeAll(include(span, p), xs1)
      case (xs0: List[_]) :: xs1 => includeAll(includeAll(span, xs0), xs1)
      case _ :: xs1 => includeAll(span, xs1)
    }

    val limit = relevantElemCount
    var n = 0
    var span = NoSpan
    while (n < limit) {
      productElement(n) match {
        case p: Positioned =>
          span = include(span, p)
        case m: untpd.Modifiers =>
          span = includeAll(includeAll(span, m.mods), m.annotations)
        case xs: ::[_] =>
          span = includeAll(span, xs)
        case _ =>
      }
      n += 1
    }
    if (uniqueId == -1) setId(si.source.nextId)
    span.toSynthetic
  }

  /** How many elements to consider when computing the span.
   *  Normally: all, overridden in Inlined.
   */
  def relevantElemCount = productArity

  def contains(that: Positioned): Boolean = {
    def isParent(x: Any): Boolean = x match {
      case x: Positioned =>
        x.contains(that)
      case m: untpd.Modifiers =>
        m.mods.exists(isParent) || m.annotations.exists(isParent)
      case xs: List[_] =>
        xs.exists(isParent)
      case _ =>
        false
    }
    (this eq that) ||
      (this.span contains that.span) && {
        var n = productArity
        var found = false
        while (!found && n > 0) {
          n -= 1
          found = isParent(productElement(n))
        }
        found
      }
  }

  /** Check that all positioned items in this tree satisfy the following conditions:
   *  - Parent spans contain child spans
   *  - If item is a non-empty tree, it has a position
   */
  def checkPos(nonOverlapping: Boolean)(implicit ctx: Context): Unit = try {
    import untpd._
    var lastPositioned: Positioned = null
    var lastSpan = NoSpan
    def check(p: Any): Unit = p match {
      case p: Positioned =>
        assert(span contains p.span,
          s"""position error, parent span does not contain child span
             |parent      = $this,
             |parent span = $span,
             |child       = $p,
             |child span  = ${p.span}""".stripMargin)
        p match {
          case tree: Tree if !tree.isEmpty =>
            assert(tree.span.exists,
              s"position error: position not set for $tree # ${tree.uniqueId}")
          case _ =>
        }
        if (nonOverlapping) {
          this match {
            case _: XMLBlock =>
              // FIXME: Trees generated by the XML parser do not satisfy `checkPos`
            case _: WildcardFunction
            if lastPositioned.isInstanceOf[ValDef] && !p.isInstanceOf[ValDef] =>
              // ignore transition from last wildcard parameter to body
            case _ =>
              assert(!lastSpan.exists || !p.span.exists || lastSpan.end <= p.span.start,
                s"""position error, child positions overlap or in wrong order
                   |parent         = $this
                   |1st child      = $lastPositioned
                   |1st child span = $lastSpan
                   |2nd child      = $p
                   |2nd child span = ${p.span}""".stripMargin)
          }
          lastPositioned = p
          lastSpan = p.span
        }
        p.checkPos(nonOverlapping)
      case m: untpd.Modifiers =>
        m.annotations.foreach(check)
        m.mods.foreach(check)
      case xs: List[_] =>
        xs.foreach(check)
      case _ =>
    }
    this match {
      case tree: DefDef if tree.name == nme.CONSTRUCTOR && tree.mods.is(JavaDefined) =>
        // Special treatment for constructors coming from Java:
        // Leave out tparams, they are copied with wrong positions from parent class
        check(tree.mods)
        check(tree.vparamss)
      case tree: DefDef if tree.mods.is(Extension) =>
        tree.vparamss match {
          case vparams1 :: vparams2 :: rest if !isLeftAssoc(tree.name) =>
            check(vparams2)
            check(tree.tparams)
            check(vparams1)
            check(rest)
          case vparams1 :: rest =>
            check(vparams1)
            check(tree.tparams)
            check(rest)
          case _ =>
            check(tree.tparams)
        }
        check(tree.tpt)
        check(tree.rhs)
      case _ =>
        val end = productArity
        var n = 0
        while (n < end) {
          check(productElement(n))
          n += 1
        }
    }
  } catch {
    case ex: AssertionError =>
      println(i"error while checking $this")
      throw ex
  }
}
