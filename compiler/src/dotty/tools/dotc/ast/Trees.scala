package dotty.tools
package dotc
package ast

import core.*
import Types.*, Names.*, NameOps.*, Flags.*, util.Spans.*, Contexts.*, Constants.*
import typer.{ ConstFold, ProtoTypes }
import SymDenotations.*, Symbols.*, Denotations.*, StdNames.*, Comments.*
import collection.mutable.ListBuffer
import printing.Printer
import printing.Texts.Text
import util.{Stats, Attachment, Property, SourceFile, NoSource, SrcPos, SourcePosition}
import config.Config
import config.Printers.overload
import annotation.internal.sharable
import annotation.unchecked.uncheckedVariance
import annotation.constructorOnly
import compiletime.uninitialized
import Decorators.*
import staging.StagingLevel.*

object Trees {

  type Untyped = Type | Null

  /** The total number of created tree nodes, maintained if Stats.enabled */
  @sharable var ntrees: Int = 0

  /** Property key for trees with documentation strings attached */
  val DocComment: Property.StickyKey[Comments.Comment] = Property.StickyKey()

  /** Property key for backquoted identifiers and definitions */
  val Backquoted: Property.StickyKey[Unit] = Property.StickyKey()

  val SyntheticUnit: Property.StickyKey[Unit] = Property.StickyKey()

  /** Trees take a parameter indicating what the type of their `tpe` field
   *  is. Two choices: `Type` or `Untyped`.
   *  Untyped trees have type `Tree[Untyped]`.
   *
   *  Tree typing uses a copy-on-write implementation:
   *
   *   - You can never observe a `tpe` which is `null` (throws an exception)
   *   - So when creating a typed tree with `withType` we can re-use
   *     the existing tree transparently, assigning its `tpe` field.
   *   - It is impossible to embed untyped trees in typed ones.
   *   - Typed trees can be embedded in untyped ones provided they are rooted
   *     in a TypedSplice node.
   *   - Type checking an untyped tree should remove all embedded `TypedSplice`
   *     nodes.
   */
  abstract class Tree[+T <: Untyped](implicit @constructorOnly src: SourceFile)
  extends Positioned, SrcPos, Product, Attachment.Container, printing.Showable {

    if (Stats.enabled) ntrees += 1

    /** The type  constructor at the root of the tree */
    type ThisTree[T <: Untyped] <: Tree[T]

    protected var myTpe: T @uncheckedVariance = uninitialized

    /** Destructively set the type of the tree. This should be called only when it is known that
     *  it is safe under sharing to do so. One use-case is in the withType method below
     *  which implements copy-on-write. Another use-case is in method interpolateAndAdapt in Typer,
     *  where we overwrite with a simplified version of the type itself.
     */
    private[dotc] def overwriteType(tpe: T @uncheckedVariance): Unit =
      myTpe = tpe

    /** The type of the tree. In case of an untyped tree,
     *   an UnAssignedTypeException is thrown. (Overridden by empty trees)
     */
    final def tpe: T =
      if myTpe == null then throw UnAssignedTypeException(this)
      myTpe.uncheckedNN

    /** Copy `tpe` attribute from tree `from` into this tree, independently
     *  whether it is null or not.
    final def copyAttr[U <: Untyped](from: Tree[U]): ThisTree[T] = {
      val t1 = this.withSpan(from.span)
      val t2 =
        if (from.myTpe != null) t1.withType(from.myTpe.asInstanceOf[Type])
        else t1
      t2.asInstanceOf[ThisTree[T]]
    }
     */

    /** Return a typed tree that's isomorphic to this tree, but has given
     *  type. (Overridden by empty trees)
     */
    def withType(tpe: Type)(using Context): ThisTree[Type] = {
      if (tpe.isInstanceOf[ErrorType])
        assert(!Config.checkUnreportedErrors ||
               ctx.reporter.errorsReported ||
               ctx.settings.YshowPrintErrors.value
                 // under -Yshow-print-errors, errors might arise during printing, but they do not count as reported
              )
      else if (Config.checkTreesConsistent)
        checkChildrenTyped(productIterator)
      withTypeUnchecked(tpe)
    }

    /** Check that typed trees don't refer to untyped ones, except if
     *   - the parent tree is an import, or
     *   - the child tree is an identifier, or
     *   - errors were reported
     */
    private def checkChildrenTyped(it: Iterator[Any])(using Context): Unit =
      if (!this.isInstanceOf[Import[?]])
        while (it.hasNext)
          it.next() match {
            case x: Ident[?] => // untyped idents are used in a number of places in typed trees
            case x: Tree[?] =>
              assert(x.hasType || ctx.reporter.errorsReported,
                     s"$this has untyped child $x")
            case xs: List[?] => checkChildrenTyped(xs.iterator)
            case _ =>
          }

    def withTypeUnchecked(tpe: Type): ThisTree[Type] = {
      val tree =
        (if (myTpe == null ||
          (myTpe.asInstanceOf[AnyRef] eq tpe.asInstanceOf[AnyRef])) this
         else cloneIn(source)).asInstanceOf[Tree[Type]]
      tree overwriteType tpe
      tree.asInstanceOf[ThisTree[Type]]
    }

    /** Does the tree have its type field set? Note: this operation is not
     *  referentially transparent, because it can observe the withType
     *  modifications. Should be used only in special circumstances (we
     *  need it for printing trees with optional type info).
     */
    final def hasType: Boolean = myTpe != null

    final def typeOpt: Type = myTpe match
      case tp: Type => tp
      case null => NoType

    /** The denotation referred to by this tree.
     *  Defined for `DenotingTree`s and `ProxyTree`s, NoDenotation for other
     *  kinds of trees
     */
    def denot(using Context): Denotation = NoDenotation

    /** Shorthand for `denot.symbol`. */
    final def symbol(using Context): Symbol = denot.symbol

    /** Does this tree represent a type? */
    def isType: Boolean = false

    /** Does this tree represent a term? */
    def isTerm: Boolean = false

    /** Is this a legal part of a pattern which is not at the same time a term? */
    def isPattern: Boolean = false

    /** Does this tree define a new symbol that is not defined elsewhere? */
    def isDef: Boolean = false

    /** Is this tree either the empty tree or the empty ValDef or an empty type ident? */
    def isEmpty: Boolean = false

    /** Convert tree to a list. Gives a singleton list, except
     *  for thickets which return their element trees.
     */
    def toList: List[Tree[T]] = this :: Nil

    /** if this tree is the empty tree, the alternative, else this tree */
    inline def orElse[U >: T <: Untyped](inline that: Tree[U]): Tree[U] =
      if (this eq genericEmptyTree) that else this

    /** The number of nodes in this tree */
    def treeSize: Int = {
      var s = 1
      def addSize(elem: Any): Unit = elem match {
        case t: Tree[?] => s += t.treeSize
        case ts: List[?] => ts foreach addSize
        case _ =>
      }
      productIterator foreach addSize
      s
    }

    /** If this is a thicket, perform `op` on each of its trees
     *  otherwise, perform `op` ion tree itself.
     */
    def foreachInThicket(op: Tree[T] => Unit): Unit = op(this)

    override def toText(printer: Printer): Text = printer.toText(this)

    def sameTree(that: Tree[?]): Boolean = {
      def isSame(x: Any, y: Any): Boolean =
        x.asInstanceOf[AnyRef].eq(y.asInstanceOf[AnyRef]) || {
          x match {
            case x: Tree[?] =>
              y match {
                case y: Tree[?] => x.sameTree(y)
                case _ => false
              }
            case x: List[?] =>
              y match {
                case y: List[?] => x.corresponds(y)(isSame)
                case _ => false
              }
            case _ =>
              false
          }
        }
      this.getClass == that.getClass && {
        val it1 = this.productIterator
        val it2 = that.productIterator
        it1.corresponds(it2)(isSame)
      }
    }

    override def hashCode(): Int = System.identityHashCode(this)
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  }

  class UnAssignedTypeException[T <: Untyped](tree: Tree[T]) extends RuntimeException {
    override def getMessage: String = s"type of $tree is not assigned"
  }

  type LazyTree[+T <: Untyped] = Tree[T] | Lazy[Tree[T]]
  type LazyTreeList[+T <: Untyped] = List[Tree[T]] | Lazy[List[Tree[T]]]

  // ------ Categories of trees -----------------------------------

  /** Instances of this class are trees for which isType is definitely true.
   *  Note that some trees have isType = true without being TypTrees (e.g. Ident, Annotated)
   */
  trait TypTree[+T <: Untyped] extends Tree[T] {
    type ThisTree[+T <: Untyped] <: TypTree[T]
    override def isType: Boolean = true
  }

  /** Instances of this class are trees for which isTerm is definitely true.
   *  Note that some trees have isTerm = true without being TermTrees (e.g. Ident, Annotated)
   */
  trait TermTree[+T <: Untyped] extends Tree[T] {
    type ThisTree[+T <: Untyped] <: TermTree[T]
    override def isTerm: Boolean = true
  }

  /** Instances of this class are trees which are not terms but are legal
   *  parts of patterns.
   */
  trait PatternTree[+T <: Untyped] extends Tree[T] {
    type ThisTree[+T <: Untyped] <: PatternTree[T]
    override def isPattern: Boolean = true
  }

  /** Tree's denotation can be derived from its type */
  abstract class DenotingTree[+T <: Untyped](implicit @constructorOnly src: SourceFile) extends Tree[T] {
    type ThisTree[+T <: Untyped] <: DenotingTree[T]
    override def denot(using Context): Denotation = typeOpt.stripped match
      case tpe: NamedType => tpe.denot
      case tpe: ThisType => tpe.cls.denot
      case _ => NoDenotation
  }

  /** Tree's denot/isType/isTerm properties come from a subtree
   *  identified by `forwardTo`.
   */
  abstract class ProxyTree[+T <: Untyped](implicit @constructorOnly src: SourceFile)  extends Tree[T] {
    type ThisTree[+T <: Untyped] <: ProxyTree[T]
    def forwardTo: Tree[T]
    override def denot(using Context): Denotation = forwardTo.denot
    override def isTerm: Boolean = forwardTo.isTerm
    override def isType: Boolean = forwardTo.isType
  }

  /** Tree has a name */
  abstract class NameTree[+T <: Untyped](implicit @constructorOnly src: SourceFile) extends DenotingTree[T] {
    type ThisTree[+T <: Untyped] <: NameTree[T]
    def name: Name
  }

  /** Tree refers by name to a denotation */
  abstract class RefTree[+T <: Untyped](implicit @constructorOnly src: SourceFile) extends NameTree[T] {
    type ThisTree[+T <: Untyped] <: RefTree[T]
    def qualifier: Tree[T]
    override def isType: Boolean = name.isTypeName
    override def isTerm: Boolean = name.isTermName
  }

  /** Tree defines a new symbol */
  trait DefTree[+T <: Untyped] extends DenotingTree[T] {
    type ThisTree[+T <: Untyped] <: DefTree[T]

    private var myMods: untpd.Modifiers | Null = uninitialized

    private[dotc] def rawMods: untpd.Modifiers =
      if (myMods == null) untpd.EmptyModifiers else myMods.uncheckedNN

    def withAnnotations(annots: List[untpd.Tree]): ThisTree[Untyped] = withMods(rawMods.withAnnotations(annots))

    def withMods(mods: untpd.Modifiers): ThisTree[Untyped] = {
      val tree = if (myMods == null || (myMods == mods)) this else cloneIn(source)
      tree.setMods(mods)
      tree.asInstanceOf[ThisTree[Untyped]]
    }

    def withFlags(flags: FlagSet): ThisTree[Untyped] = withMods(untpd.Modifiers(flags))
    def withAddedFlags(flags: FlagSet): ThisTree[Untyped] = withMods(rawMods | flags)

    /** Destructively update modifiers. To be used with care. */
    def setMods(mods: untpd.Modifiers): Unit = myMods = mods

    override def isDef: Boolean = true
    def namedType: NamedType = tpe.asInstanceOf[NamedType]
  }

  extension (mdef: untpd.DefTree) def mods: untpd.Modifiers = mdef.rawMods

  sealed trait WithEndMarker[+T <: Untyped]:
    self: PackageDef[T] | NamedDefTree[T] =>

    import WithEndMarker.*

    final def endSpan(using Context): Span =
      if hasEndMarker then
        val realName = srcName.stripModuleClassSuffix.lastPart
        span.withStart(span.end - realName.length)
      else
        NoSpan

    /** The name in source code that represents this construct,
     *  and is the name that the user must write to create a valid
     *  end marker.
     *  e.g. a constructor definition is terminated in the source
     *  code by `end this`, so it's `srcName` should return `this`.
     */
    protected def srcName(using Context): Name

    final def withEndMarker(): self.type =
      self.withAttachment(HasEndMarker, ())

    final def withEndMarker(copyFrom: WithEndMarker[?]): self.type =
      if copyFrom.hasEndMarker then
        this.withEndMarker()
      else
        this

    final def dropEndMarker(): self.type =
      self.removeAttachment(HasEndMarker)
      this

    protected def hasEndMarker: Boolean = self.hasAttachment(HasEndMarker)

  object WithEndMarker:
    /** Property key that signals the tree was terminated
     *  with an `end` marker in the source code
     */
    private val HasEndMarker: Property.StickyKey[Unit] = Property.StickyKey()

  end WithEndMarker

  abstract class NamedDefTree[+T <: Untyped](implicit @constructorOnly src: SourceFile)
  extends NameTree[T] with DefTree[T] with WithEndMarker[T] {
    type ThisTree[+T <: Untyped] <: NamedDefTree[T]

    protected def srcName(using Context): Name =
      if name == nme.CONSTRUCTOR then nme.this_
      else if symbol.isPackageObject then symbol.owner.name
      else name

    /** The position of the name defined by this definition.
     *  This is a point position if the definition is synthetic, or a range position
     *  if the definition comes from source.
     *  It might also be that the definition does not have a position (for instance when synthesized by
     *  a calling chain from `viewExists`), in that case the return position is NoSpan.
     *  Overridden in Bind
     */
    def nameSpan(using Context): Span =
      if (span.exists) {
        val point = span.point
        if (rawMods.is(Synthetic) || span.isSynthetic || name.toTermName == nme.ERROR) Span(point)
        else {
          val realName = srcName.stripModuleClassSuffix.lastPart
          Span(point, point + realName.length, point)
        }
      }
      else span

    /** The source position of the name defined by this definition.
     *  This is a point position if the definition is synthetic, or a range position
     *  if the definition comes from source.
     */
    def namePos(using Context): SourcePosition = source.atSpan(nameSpan)
  }

  /** Tree defines a new symbol and carries modifiers.
   *  The position of a MemberDef contains only the defined identifier or pattern.
   *  The envelope of a MemberDef contains the whole definition and has its point
   *  on the opening keyword (or the next token after that if keyword is missing).
   */
  abstract class MemberDef[+T <: Untyped](implicit @constructorOnly src: SourceFile) extends NamedDefTree[T] {
    type ThisTree[+T <: Untyped] <: MemberDef[T]

    def rawComment: Option[Comment] = getAttachment(DocComment)

    def setComment(comment: Option[Comment]): this.type = {
      comment.map(putAttachment(DocComment, _))
      this
    }

    def name: Name
  }

  /** A ValDef or DefDef tree */
  abstract class ValOrDefDef[+T <: Untyped](implicit @constructorOnly src: SourceFile) extends MemberDef[T], WithLazyFields {
    type ThisTree[+T <: Untyped] <: ValOrDefDef[T]
    def name: TermName
    def tpt: Tree[T]
    def unforcedRhs: LazyTree[T]
    def rhs(using Context): Tree[T]
  }

  trait ValOrTypeDef[+T <: Untyped] extends MemberDef[T]:
    type ThisTree[+T <: Untyped] <: ValOrTypeDef[T]

  type ParamClause[T <: Untyped] = List[ValDef[T]] | List[TypeDef[T]]

  // ----------- Tree case classes ------------------------------------

  /** name */
  case class Ident[+T <: Untyped] private[ast] (name: Name)(implicit @constructorOnly src: SourceFile)
    extends RefTree[T] {
    type ThisTree[+T <: Untyped] = Ident[T]
    def qualifier: Tree[T] = genericEmptyTree

    def isBackquoted: Boolean = hasAttachment(Backquoted)
  }

  class SearchFailureIdent[+T <: Untyped] private[ast] (name: Name, expl: => String)(implicit @constructorOnly src: SourceFile)
    extends Ident[T](name) {
    def explanation = expl
    override def toString: String = s"SearchFailureIdent($explanation)"
  }

  /** qualifier.name, or qualifier#name, if qualifier is a type */
  case class Select[+T <: Untyped] private[ast] (qualifier: Tree[T], name: Name)(implicit @constructorOnly src: SourceFile)
    extends RefTree[T] {
    type ThisTree[+T <: Untyped] = Select[T]

    override def denot(using Context): Denotation = typeOpt match
      case ConstantType(_) if ConstFold.foldedUnops.contains(name) =>
        // Recover the denotation of a constant-folded selection
        qualifier.typeOpt.member(name).atSignature(Signature.NotAMethod, name)
      case _ =>
        super.denot

    def nameSpan(using Context): Span =
      if span.exists then
        val point = span.point
        if name.toTermName == nme.ERROR then
          Span(point)
        else if qualifier.span.exists && qualifier.span.start > span.point then // right associative
          val realName = name.stripModuleClassSuffix.lastPart
          Span(span.start, span.start + realName.length, point)
        else
          Span(point, span.end, point)
      else span
  }

  class SelectWithSig[+T <: Untyped] private[ast] (qualifier: Tree[T], name: Name, val sig: Signature)(implicit @constructorOnly src: SourceFile)
    extends Select[T](qualifier, name) {
    override def toString: String = s"SelectWithSig($qualifier, $name, $sig)"
  }

  /** qual.this */
  case class This[+T <: Untyped] private[ast] (qual: untpd.Ident)(implicit @constructorOnly src: SourceFile)
    extends DenotingTree[T] with TermTree[T] {
    type ThisTree[+T <: Untyped] = This[T]
    // Denotation of a This tree is always the underlying class; needs correction for modules.
    override def denot(using Context): Denotation =
      typeOpt match {
        case tpe @ TermRef(pre, _) if tpe.symbol.is(Module) =>
          tpe.symbol.moduleClass.denot.asSeenFrom(pre)
        case _ =>
          super.denot
      }
  }

  /** C.super[mix], where qual = C.this */
  case class Super[+T <: Untyped] private[ast] (qual: Tree[T], mix: untpd.Ident)(implicit @constructorOnly src: SourceFile)
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[+T <: Untyped] = Super[T]
    def forwardTo: Tree[T] = qual
  }

  abstract class GenericApply[+T <: Untyped](implicit @constructorOnly src: SourceFile) extends ProxyTree[T] with TermTree[T] {
    type ThisTree[+T <: Untyped] <: GenericApply[T]
    val fun: Tree[T]
    val args: List[Tree[T]]
    def forwardTo: Tree[T] = fun
  }

  object GenericApply:
    def unapply[T <: Untyped](tree: Tree[T]): Option[(Tree[T], List[Tree[T]])] = tree match
      case tree: GenericApply[T] => Some((tree.fun, tree.args))
      case _ => None

  /** The kind of application */
  enum ApplyKind:
    case Regular      // r.f(x)
    case Using        // r.f(using x)
    case InfixTuple   // r f (x1, ..., xN) where N != 1;  needs to be treated specially for an error message in typedApply

  /** fun(args) */
  case class Apply[+T <: Untyped] private[ast] (fun: Tree[T], args: List[Tree[T]])(implicit @constructorOnly src: SourceFile)
    extends GenericApply[T] {
    type ThisTree[+T <: Untyped] = Apply[T]

    def setApplyKind(kind: ApplyKind) =
      putAttachment(untpd.KindOfApply, kind)
      this

    /** The kind of this application. Works reliably only for untyped trees; typed trees
     *  are under no obligation to update it correctly.
     */
    def applyKind: ApplyKind =
      attachmentOrElse(untpd.KindOfApply, ApplyKind.Regular)
  }

  /** fun[args] */
  case class TypeApply[+T <: Untyped] private[ast] (fun: Tree[T], args: List[Tree[T]])(implicit @constructorOnly src: SourceFile)
    extends GenericApply[T] {
    type ThisTree[+T <: Untyped] = TypeApply[T]
  }

  /** const */
  case class Literal[+T <: Untyped] private[ast] (const: Constant)(implicit @constructorOnly src: SourceFile)
    extends Tree[T] with TermTree[T] {
    type ThisTree[+T <: Untyped] = Literal[T]
  }

  /** new tpt, but no constructor call */
  case class New[+T <: Untyped] private[ast] (tpt: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends Tree[T] with TermTree[T] {
    type ThisTree[+T <: Untyped] = New[T]
  }

  /** expr : tpt */
  case class Typed[+T <: Untyped] private[ast] (expr: Tree[T], tpt: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[+T <: Untyped] = Typed[T]
    def forwardTo: Tree[T] = expr
  }

  /** name = arg, in a parameter list */
  case class NamedArg[+T <: Untyped] private[ast] (name: Name, arg: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends Tree[T] {
    type ThisTree[+T <: Untyped] = NamedArg[T]
  }

  /** name = arg, outside a parameter list */
  case class Assign[+T <: Untyped] private[ast] (lhs: Tree[T], rhs: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends TermTree[T] {
    type ThisTree[+T <: Untyped] = Assign[T]
  }

  /** { stats; expr } */
  case class Block[+T <: Untyped] private[ast] (stats: List[Tree[T]], expr: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends Tree[T] {
    type ThisTree[+T <: Untyped] = Block[T]
    override def isType: Boolean = expr.isType
    override def isTerm: Boolean = !isType // this will classify empty trees as terms, which is necessary
  }

  /** if cond then thenp else elsep */
  case class If[+T <: Untyped] private[ast] (cond: Tree[T], thenp: Tree[T], elsep: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends TermTree[T] {
    type ThisTree[+T <: Untyped] = If[T]
    def isInline = false
  }
  class InlineIf[+T <: Untyped] private[ast] (cond: Tree[T], thenp: Tree[T], elsep: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends If(cond, thenp, elsep) {
    override def isInline = true
    override def toString = s"InlineIf($cond, $thenp, $elsep)"
  }

  /** A closure with an environment and a reference to a method.
   *  @param env    The captured parameters of the closure
   *  @param meth   A ref tree that refers to the method of the closure.
   *                The first (env.length) parameters of that method are filled
   *                with env values.
   *  @param tpt    Either EmptyTree or a TypeTree. If tpt is EmptyTree the type
   *                of the closure is a function type, otherwise it is the type
   *                given in `tpt`, which must be a SAM type.
   */
  case class Closure[+T <: Untyped] private[ast] (env: List[Tree[T]], meth: Tree[T], tpt: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends TermTree[T] {
    type ThisTree[+T <: Untyped] = Closure[T]
  }

  /** selector match { cases } */
  case class Match[+T <: Untyped] private[ast] (selector: Tree[T], cases: List[CaseDef[T]])(implicit @constructorOnly src: SourceFile)
    extends TermTree[T] {
    type ThisTree[+T <: Untyped] = Match[T]
    def isInline = false
  }
  class InlineMatch[+T <: Untyped] private[ast] (selector: Tree[T], cases: List[CaseDef[T]])(implicit @constructorOnly src: SourceFile)
    extends Match(selector, cases) {
    override def isInline = true
    override def toString = s"InlineMatch($selector, $cases)"
  }

  /** case pat if guard => body */
  case class CaseDef[+T <: Untyped] private[ast] (pat: Tree[T], guard: Tree[T], body: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends Tree[T] {
    type ThisTree[+T <: Untyped] = CaseDef[T]
  }

  /** label[tpt]: { expr } */
  case class Labeled[+T <: Untyped] private[ast] (bind: Bind[T], expr: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends NameTree[T] {
    type ThisTree[+T <: Untyped] = Labeled[T]
    def name: Name = bind.name
  }

  /** return expr
   *  where `from` refers to the method or label from which the return takes place
   *  After program transformations this is not necessarily the enclosing method, because
   *  closures can intervene.
   */
  case class Return[+T <: Untyped] private[ast] (expr: Tree[T], from: Tree[T] = genericEmptyTree)(implicit @constructorOnly src: SourceFile)
    extends TermTree[T] {
    type ThisTree[+T <: Untyped] = Return[T]
  }

  /** while (cond) { body } */
  case class WhileDo[+T <: Untyped] private[ast] (cond: Tree[T], body: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends TermTree[T] {
    type ThisTree[+T <: Untyped] = WhileDo[T]
  }

  /** try block catch cases finally finalizer */
  case class Try[+T <: Untyped] private[ast] (expr: Tree[T], cases: List[CaseDef[T]], finalizer: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends TermTree[T] {
    type ThisTree[+T <: Untyped] = Try[T]
  }

  /** Seq(elems)
   *  @param  tpt  The element type of the sequence.
   */
  case class SeqLiteral[+T <: Untyped] private[ast] (elems: List[Tree[T]], elemtpt: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends Tree[T] {
    type ThisTree[+T <: Untyped] = SeqLiteral[T]
  }

  /** Array(elems) */
  class JavaSeqLiteral[+T <: Untyped] private[ast] (elems: List[Tree[T]], elemtpt: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends SeqLiteral(elems, elemtpt) {
    override def toString: String = s"JavaSeqLiteral($elems, $elemtpt)"
  }

  /** A tree representing inlined code.
   *
   *  @param  call      Info about the original call that was inlined
   *                    Until PostTyper, this is the full call, afterwards only
   *                    a reference to the toplevel class from which the call was inlined.
   *  @param  bindings  Bindings for proxies to be used in the inlined code
   *  @param  expansion The inlined tree, minus bindings.
   *
   *  The full inlined code is equivalent to
   *
   *      { bindings; expansion }
   *
   *  The reason to keep `bindings` separate is because they are typed in a
   *  different context: `bindings` represent the arguments to the inlined
   *  call, whereas `expansion` represents the body of the inlined function.
   */
  case class Inlined[+T <: Untyped] private[ast] (call: tpd.Tree, bindings: List[MemberDef[T]], expansion: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends Tree[T] {

    def inlinedFromOuterScope: Boolean = call.isEmpty

    type ThisTree[+T <: Untyped] = Inlined[T]
    override def isTerm = expansion.isTerm
    override def isType = expansion.isType
  }

  /** A tree representing a quote `'{ body }` or `'[ body ]`.
   *  `Quote`s are created by the `Parser`. In typer they can be typed as a
   *  `Quote` with a known `tpt` or desugared and typed as a quote pattern.
   *
   *  `Quotes` are checked and transformed in the `staging`, `splicing` and `pickleQuotes`
   *  phases. After `pickleQuotes` phase, the only quotes that exist are in `inline`
   *  methods. These are dropped when we remove the inline method implementations.
   *
   *  Type quotes `'[body]` from the parser are typed into `QuotePattern`s when type checking.
   *  TASTy files will not contain type quotes. Type quotes are used again in the `staging`
   *  phase to represent the reification of `Type.of[T]]`.
   *
   *  Type tags `tags` are always empty before the `staging` phase. Tags for stage inconsistent
   *  types are added in the `staging` phase to level 0 quotes. Tags for types that refer to
   *  definitions in an outer quote are added in the `splicing` phase
   *
   *  @param  body  The tree that was quoted
   *  @param  tags  Term references to instances of `Type[T]` for `T`s that are used in the quote
   */
  case class Quote[+T <: Untyped] private[ast] (body: Tree[T], tags: List[Tree[T]])(implicit @constructorOnly src: SourceFile)
    extends TermTree[T] {
    type ThisTree[+T <: Untyped] = Quote[T]

    /** Is this a type quote `'[tpe]' */
    def isTypeQuote = body.isType

    /** Set the type of the body of the quote */
    def withBodyType(tpe: Type)(using Context): Quote[Type] =
      val exprType = // `Expr[T]` or `Type[T]`
        if body.isTerm then defn.QuotedExprClass.typeRef.appliedTo(tpe)
        else defn.QuotedTypeClass.typeRef.appliedTo(tpe)
      val quoteType = // `Quotes ?=> Expr[T]` or `Quotes ?=> Type[T]`
        defn.FunctionType(1, isContextual = true)
          .appliedTo(defn.QuotesClass.typeRef, exprType)
      withType(quoteType)
  }

  /** A tree representing a splice `${ expr }`
   *
   *  `Splice`s are created by the `Parser`. In typer they can be typed as a
   *  `Splice` with a known `tpt` or desugared and typed as a quote pattern holes.
   *
   *  `Splice` are checked and transformed in the `staging` and `splicing` phases.
   *  After `splicing` phase, the only splices that exist are in `inline`
   *  methods. These are dropped when we remove the inline method implementations.
   *
   *  @param expr The tree that was spliced
   */
  case class Splice[+T <: Untyped] private[ast] (expr: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends TermTree[T] {
    type ThisTree[+T <: Untyped] = Splice[T]
  }

  /** A tree representing a quote pattern `'{ type binding1; ...; body }` or `'[ type binding1; ...; body ]`.
   *  `QuotePattern`s are created the type checker when typing an `untpd.Quote` in a pattern context.
   *
   *  `QuotePattern`s are checked are encoded into `unapply`s  in the `staging` phase.
   *
   *   The `bindings` contain the list of quote pattern type variable definitions (`Bind`s) in the oreder in
   *   which they are defined in the source.
   *
   *   @param  bindings  Type variable definitions (`Bind` tree)
   *   @param  body      Quoted pattern (without type variable definitions)
   *   @param  quotes    A reference to the given `Quotes` instance in scope
   */
  case class QuotePattern[+T <: Untyped] private[ast] (bindings: List[Tree[T]], body: Tree[T], quotes: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends PatternTree[T] {
    type ThisTree[+T <: Untyped] = QuotePattern[T]
  }

  /** A tree representing a pattern splice `${ pattern }`, `$ident` or `$ident(args*)` in a quote pattern.
   *
   *  Parser will only create `${ pattern }` and `$ident`, hence they will not have args.
   *  While typing, the `$ident(args*)` the args are identified and desugared into a `SplicePattern`
   *  containing them.
   *
   *  `SplicePattern` can only be contained within a `QuotePattern`.
   *
   *  @param body  The tree that was spliced
   *  @param args  The arguments of the splice (the HOAS arguments)
   */
  case class SplicePattern[+T <: Untyped] private[ast] (body: Tree[T], args: List[Tree[T]])(implicit @constructorOnly src: SourceFile)
    extends TermTree[T] {
    type ThisTree[+T <: Untyped] = SplicePattern[T]
  }

  /** A type tree that represents an existing or inferred type */
  case class TypeTree[+T <: Untyped]()(implicit @constructorOnly src: SourceFile)
    extends DenotingTree[T] with TypTree[T] {
    type ThisTree[+T <: Untyped] <: TypeTree[T]
    override def isEmpty: Boolean = !hasType
    override def toString: String =
      s"TypeTree${if (hasType) s"[$typeOpt]" else ""}"
  }

  /** Tree that replaces a level 1 splices in pickled (level 0) quotes.
   *  It is only used when pickling quotes (will never be in a TASTy file).
   *
   *  @param isTerm If this hole is a term, otherwise it is a type hole.
   *  @param idx The index of the hole in it's enclosing level 0 quote.
   *  @param args The arguments of the splice to compute its content
   *  @param content Lambda that computes the content of the hole. This tree is empty when in a quote pickle.
   */
  case class Hole[+T <: Untyped](override val isTerm: Boolean, idx: Int, args: List[Tree[T]], content: Tree[T])(implicit @constructorOnly src: SourceFile) extends Tree[T] {
    type ThisTree[+T <: Untyped] <: Hole[T]
    override def isType: Boolean = !isTerm
  }

  /** A type tree whose type is inferred. These trees appear in two contexts
   *    - as an argument of a TypeApply. In that case its type is always a TypeVar
   *    - as a (result-)type of an inferred ValDef or DefDef.
   *  Every TypeVar is created as the type of one InferredTypeTree.
   */
  class InferredTypeTree[+T <: Untyped](implicit @constructorOnly src: SourceFile) extends TypeTree[T]:
    type ThisTree[+T <: Untyped] <: InferredTypeTree[T]

  /** ref.type */
  case class SingletonTypeTree[+T <: Untyped] private[ast] (ref: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends DenotingTree[T] with TypTree[T] {
    type ThisTree[+T <: Untyped] = SingletonTypeTree[T]
  }

  /** tpt { refinements } */
  case class RefinedTypeTree[+T <: Untyped] private[ast] (tpt: Tree[T], refinements: List[Tree[T]])(implicit @constructorOnly src: SourceFile)
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[+T <: Untyped] = RefinedTypeTree[T]
    def forwardTo: Tree[T] = tpt
  }

  /** tpt[args] */
  case class AppliedTypeTree[+T <: Untyped] private[ast] (tpt: Tree[T], args: List[Tree[T]])(implicit @constructorOnly src: SourceFile)
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[+T <: Untyped] = AppliedTypeTree[T]
    def forwardTo: Tree[T] = tpt
  }

  /** [typeparams] -> tpt
   *
   *  Note: the type of such a tree is not necessarily a `HKTypeLambda`, it can
   *  also be a `TypeBounds` where the upper bound is an `HKTypeLambda`, and the
   *  lower bound is either a reference to `Nothing` or an `HKTypeLambda`,
   *  this happens because these trees are typed by `HKTypeLambda#fromParams` which
   *  makes sure to move bounds outside of the type lambda itself to simplify their
   *  handling in the compiler.
   *
   *  You may ask: why not normalize the trees too? That way,
   *
   *      LambdaTypeTree(X, TypeBoundsTree(A, B))
   *
   *  would become,
   *
   *      TypeBoundsTree(LambdaTypeTree(X, A), LambdaTypeTree(X, B))
   *
   *  which would maintain consistency between a tree and its type. The problem
   *  with this definition is that the same tree `X` appears twice, therefore
   *  we'd have to create two symbols for it which makes it harder to relate the
   *  source code written by the user with the trees used by the compiler (for
   *  example, to make "find all references" work in the IDE).
   */
  case class LambdaTypeTree[+T <: Untyped] private[ast] (tparams: List[TypeDef[T]], body: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends TypTree[T] {
    type ThisTree[+T <: Untyped] = LambdaTypeTree[T]
  }

  case class TermLambdaTypeTree[+T <: Untyped] private[ast] (params: List[ValDef[T]], body: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends TypTree[T] {
    type ThisTree[+T <: Untyped] = TermLambdaTypeTree[T]
  }

  /** [bound] selector match { cases } */
  case class MatchTypeTree[+T <: Untyped] private[ast] (bound: Tree[T], selector: Tree[T], cases: List[CaseDef[T]])(implicit @constructorOnly src: SourceFile)
    extends TypTree[T] {
    type ThisTree[+T <: Untyped] = MatchTypeTree[T]
  }

  /** => T */
  case class ByNameTypeTree[+T <: Untyped] private[ast] (result: Tree[T])(implicit @constructorOnly src: SourceFile)
  extends TypTree[T] {
    type ThisTree[+T <: Untyped] = ByNameTypeTree[T]
  }

  /** >: lo <: hi
   *  >: lo <: hi = alias  for RHS of bounded opaque type
   */
  case class TypeBoundsTree[+T <: Untyped] private[ast] (lo: Tree[T], hi: Tree[T], alias: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends TypTree[T] {
    type ThisTree[+T <: Untyped] = TypeBoundsTree[T]
  }

  /** name @ body */
  case class Bind[+T <: Untyped] private[ast] (name: Name, body: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends NamedDefTree[T] with PatternTree[T] {
    type ThisTree[+T <: Untyped] = Bind[T]
    override def isType: Boolean = name.isTypeName
    override def isTerm: Boolean = name.isTermName

    override def nameSpan(using Context): Span =
      if span.exists then Span(span.start, span.start + name.toString.length) else span
  }

  /** tree_1 | ... | tree_n */
  case class Alternative[+T <: Untyped] private[ast] (trees: List[Tree[T]])(implicit @constructorOnly src: SourceFile)
    extends PatternTree[T] {
    type ThisTree[+T <: Untyped] = Alternative[T]
  }

  /** The typed translation of `extractor(patterns)` in a pattern. The translation has the following
   *  components:
   *
   *  @param fun       is `extractor.unapply` (or, for backwards compatibility, `extractor.unapplySeq`)
   *                   possibly with type parameters
   *  @param implicits Any implicit parameters passed to the unapply after the selector
   *  @param patterns  The argument patterns in the pattern match.
   *
   *  It is typed with same type as first `fun` argument
   *  Given a match selector `sel` a pattern UnApply(fun, implicits, patterns) is roughly translated as follows
   *
   *    val result = fun(sel)(implicits)
   *    if (result.isDefined) "match patterns against result"
   */
  case class UnApply[+T <: Untyped] private[ast] (fun: Tree[T], implicits: List[Tree[T]], patterns: List[Tree[T]])(implicit @constructorOnly src: SourceFile)
    extends ProxyTree[T] with PatternTree[T] {
    type ThisTree[+T <: Untyped] = UnApply[T]
    def forwardTo = fun
  }

  /** mods val name: tpt = rhs */
  case class ValDef[+T <: Untyped] private[ast] (name: TermName, tpt: Tree[T], private var preRhs: LazyTree[T])(implicit @constructorOnly src: SourceFile)
    extends ValOrDefDef[T], ValOrTypeDef[T] {
    type ThisTree[+T <: Untyped] = ValDef[T]
    assert(isEmpty || (tpt ne genericEmptyTree))

    def unforcedRhs: LazyTree[T] = preRhs
    def forceFields()(using Context): Unit = preRhs = force(preRhs)
    def rhs(using Context): Tree[T] = { forceFields(); preRhs.asInstanceOf[Tree[T]] }
  }

  /** mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs */
  case class DefDef[+T <: Untyped] private[ast] (name: TermName,
      paramss: List[ParamClause[T]], tpt: Tree[T], private var preRhs: LazyTree[T])(implicit @constructorOnly src: SourceFile)
    extends ValOrDefDef[T] {
    type ThisTree[+T <: Untyped] = DefDef[T]
    assert(tpt ne genericEmptyTree)

    def unforcedRhs: LazyTree[T] = preRhs
    def forceFields()(using Context): Unit = preRhs = force(preRhs)
    def rhs(using Context): Tree[T] = { forceFields(); preRhs.asInstanceOf[Tree[T]] }

    def leadingTypeParams(using Context): List[TypeDef[T]] = paramss match
      case (tparams @ (tparam: TypeDef[?]) :: _) :: _ => tparams.asInstanceOf[List[TypeDef[T]]]
      case _ => Nil

    def trailingParamss(using Context): List[ParamClause[T]] = paramss match
      case ((tparam: TypeDef[?]) :: _) :: paramss1 => paramss1
      case _ => paramss

    def termParamss(using Context): List[List[ValDef[T]]] =
      (if ctx.erasedTypes then paramss else untpd.termParamssIn(paramss))
        .asInstanceOf[List[List[ValDef[T]]]]
  }

  /** mods class name template     or
   *  mods trait name template     or
   *  mods type name = rhs   or
   *  mods type name >: lo <: hi,          if rhs = TypeBoundsTree(lo, hi)      or
   *  mods type name >: lo <: hi = rhs     if rhs = TypeBoundsTree(lo, hi, alias) and opaque in mods
   */
  case class TypeDef[+T <: Untyped] private[ast] (name: TypeName, rhs: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends MemberDef[T], ValOrTypeDef[T] {
    type ThisTree[+T <: Untyped] = TypeDef[T]

    /** Is this a definition of a class? */
    def isClassDef: Boolean = rhs.isInstanceOf[Template[?]]

    def isBackquoted: Boolean = hasAttachment(Backquoted)
  }

  /** extends parents { self => body }
   *  @param preParentsOrDerived A list of parents followed by a list of derived classes,
   *                             if this is of class untpd.DerivingTemplate.
   *                             Typed templates only have parents.
   */
  case class Template[+T <: Untyped] private[ast] (constr: DefDef[T], private var preParentsOrDerived: LazyTreeList[T], self: ValDef[T], private var preBody: LazyTreeList[T])(implicit @constructorOnly src: SourceFile)
    extends DefTree[T] with WithLazyFields {
    type ThisTree[+T <: Untyped] = Template[T]

    def forceFields()(using Context): Unit =
      preParentsOrDerived = force(preParentsOrDerived)
      preBody = force(preBody)

    def unforcedBody: LazyTreeList[T] = preBody
    def body(using Context): List[Tree[T]] = { forceFields(); preBody.asInstanceOf[List[Tree[T]]] }
    def parentsOrDerived(using Context): List[Tree[T]] = { forceFields(); preParentsOrDerived.asInstanceOf[List[Tree[T]]] }

    def parents(using Context): List[Tree[T]] = parentsOrDerived // overridden by DerivingTemplate
    def derived: List[untpd.Tree] = Nil // overridden by DerivingTemplate
  }


  abstract class ImportOrExport[+T <: Untyped](implicit @constructorOnly src: SourceFile)
    extends DenotingTree[T] {
    type ThisTree[+T <: Untyped] <: ImportOrExport[T]
    val expr: Tree[T]
    val selectors: List[untpd.ImportSelector]
  }

  /** import expr.selectors
   *  where a selector is either an untyped `Ident`, `name` or
   *  an untyped thicket consisting of `name` and `rename`.
   */
  case class Import[+T <: Untyped] private[ast] (expr: Tree[T], selectors: List[untpd.ImportSelector])(implicit @constructorOnly src: SourceFile)
    extends ImportOrExport[T] {
    type ThisTree[+T <: Untyped] = Import[T]
  }

  /** export expr.selectors
   *  where a selector is either an untyped `Ident`, `name` or
   *  an untyped thicket consisting of `name` and `rename`.
   */
  case class Export[+T <: Untyped] private[ast] (expr: Tree[T], selectors: List[untpd.ImportSelector])(implicit @constructorOnly src: SourceFile)
    extends ImportOrExport[T] {
      type ThisTree[+T <: Untyped] = Export[T]
  }

  /** package pid { stats } */
  case class PackageDef[+T <: Untyped] private[ast] (pid: RefTree[T], stats: List[Tree[T]])(implicit @constructorOnly src: SourceFile)
    extends ProxyTree[T] with WithEndMarker[T] {
    type ThisTree[+T <: Untyped] = PackageDef[T]
    def forwardTo: RefTree[T] = pid
    protected def srcName(using Context): Name = pid.name
  }

  /** arg @annot */
  case class Annotated[+T <: Untyped] private[ast] (arg: Tree[T], annot: Tree[T])(implicit @constructorOnly src: SourceFile)
    extends ProxyTree[T] {
    type ThisTree[+T <: Untyped] = Annotated[T]
    def forwardTo: Tree[T] = arg
  }

  trait WithoutTypeOrPos[+T <: Untyped] extends Tree[T] {
    override def withTypeUnchecked(tpe: Type): ThisTree[Type] = this.asInstanceOf[ThisTree[Type]]
    override def span: Span = NoSpan
    override def span_=(span: Span): Unit = {}
  }

  /** Temporary class that results from translation of ModuleDefs
   *  (and possibly other statements).
   *  The contained trees will be integrated when transformed with
   *  a `transform(List[Tree])` call.
   */
  case class Thicket[+T <: Untyped](trees: List[Tree[T]])(implicit @constructorOnly src: SourceFile)
    extends Tree[T] with WithoutTypeOrPos[T] {
    myTpe = NoType.asInstanceOf[T]
    type ThisTree[+T <: Untyped] = Thicket[T]

    def mapElems[U >: T <: Untyped](op: Tree[T] => Tree[U]): Thicket[U] = {
      val newTrees = trees.mapConserve(op)
      if (trees eq newTrees)
        this
      else
        Thicket[U](newTrees)(source).asInstanceOf[this.type]
    }

    override def foreachInThicket(op: Tree[T] => Unit): Unit =
      trees foreach (_.foreachInThicket(op))

    override def isEmpty: Boolean = trees.isEmpty
    override def toList: List[Tree[T]] = flatten(trees)
    override def toString: String = if (isEmpty) "EmptyTree" else "Thicket(" + trees.mkString(", ") + ")"
    override def span: Span =
      def combine(s: Span, ts: List[Tree[T]]): Span = ts match
        case t :: ts1 => combine(s.union(t.span), ts1)
        case nil => s
      combine(NoSpan, trees)

    override def withSpan(span: Span): this.type =
      mapElems(_.withSpan(span)).asInstanceOf[this.type]
  }

  class EmptyTree[T <: Untyped] extends Thicket(Nil)(NoSource) {
    // assert(uniqueId != 1492)
    override def withSpan(span: Span) = throw AssertionError("Cannot change span of EmptyTree")
  }

  class EmptyValDef[T <: Untyped] extends ValDef[T](
    nme.WILDCARD, genericEmptyTree[T], genericEmptyTree[T])(NoSource) with WithoutTypeOrPos[T] {
    myTpe = NoType.asInstanceOf[T]
    setMods(untpd.Modifiers(PrivateLocal))
    override def isEmpty: Boolean = true
    override def withSpan(span: Span) = throw AssertionError("Cannot change span of EmptyValDef")
  }

  @sharable val theEmptyTree = new EmptyTree[Type]()
  @sharable val theEmptyValDef = new EmptyValDef[Type]()

  def genericEmptyValDef[T <: Untyped]: ValDef[T]       = theEmptyValDef.asInstanceOf[ValDef[T]]
  def genericEmptyTree[T <: Untyped]: Thicket[T]        = theEmptyTree.asInstanceOf[Thicket[T]]

  def flatten[T <: Untyped](trees: List[Tree[T]]): List[Tree[T]] = {
    def recur(buf: ListBuffer[Tree[T]] | Null, remaining: List[Tree[T]]): ListBuffer[Tree[T]] | Null =
      remaining match {
        case Thicket(elems) :: remaining1 =>
          var buf1 = buf
          if (buf1 == null) {
            buf1 = new ListBuffer[Tree[T]]
            var scanned = trees
            while (scanned `ne` remaining) {
              buf1 += scanned.head
              scanned = scanned.tail
            }
          }
          recur(recur(buf1, elems), remaining1)
        case tree :: remaining1 =>
          if (buf != null) buf += tree
          recur(buf, remaining1)
        case nil =>
          buf
      }
    val buf = recur(null, trees)
    if (buf != null) buf.toList else trees
  }

  // ----- Lazy trees and tree sequences

  /** A base trait for lazy tree fields.
   *  These can be instantiated with Lazy instances which
   *  can delay tree construction until the field is first demanded.
   */
  trait Lazy[+T <: AnyRef]:
    def complete(using Context): T

  /** A tree that can have a lazy fields.
   *  Such fields are variables of type `T | Lazy[T]`, for some tyope `T`.
   */
  trait WithLazyFields:

    /** If `x` is lazy, computes the underlying value */
    protected def force[T <: AnyRef](x: T | Lazy[T])(using Context): T = x match
      case x: Lazy[T] @unchecked => x.complete
      case x: T @unchecked => x

    /** Assigns all lazy fields their underlying non-lazy value. */
    def forceFields()(using Context): Unit

  end WithLazyFields

  // ----- Generic Tree Instances, inherited from `tpt` and `untpd`.

  abstract class Instance[T <: Untyped] { inst =>

    type Tree = Trees.Tree[T]
    type TypTree = Trees.TypTree[T]
    type TermTree = Trees.TermTree[T]
    type PatternTree = Trees.PatternTree[T]
    type DenotingTree = Trees.DenotingTree[T]
    type ProxyTree = Trees.ProxyTree[T]
    type NameTree = Trees.NameTree[T]
    type RefTree = Trees.RefTree[T]
    type DefTree = Trees.DefTree[T]
    type NamedDefTree = Trees.NamedDefTree[T]
    type MemberDef = Trees.MemberDef[T]
    type ValOrDefDef = Trees.ValOrDefDef[T]
    type ValOrTypeDef = Trees.ValOrTypeDef[T]
    type LazyTree = Trees.LazyTree[T]
    type LazyTreeList = Trees.LazyTreeList[T]
    type ParamClause = Trees.ParamClause[T]

    type Ident = Trees.Ident[T]
    type SearchFailureIdent = Trees.SearchFailureIdent[T]
    type Select = Trees.Select[T]
    type SelectWithSig = Trees.SelectWithSig[T]
    type This = Trees.This[T]
    type Super = Trees.Super[T]
    type Apply = Trees.Apply[T]
    type TypeApply = Trees.TypeApply[T]
    type GenericApply = Trees.GenericApply[T]
    type Literal = Trees.Literal[T]
    type New = Trees.New[T]
    type Typed = Trees.Typed[T]
    type NamedArg = Trees.NamedArg[T]
    type Assign = Trees.Assign[T]
    type Block = Trees.Block[T]
    type If = Trees.If[T]
    type InlineIf = Trees.InlineIf[T]
    type Closure = Trees.Closure[T]
    type Match = Trees.Match[T]
    type InlineMatch = Trees.InlineMatch[T]
    type CaseDef = Trees.CaseDef[T]
    type Labeled = Trees.Labeled[T]
    type Return = Trees.Return[T]
    type WhileDo = Trees.WhileDo[T]
    type Try = Trees.Try[T]
    type SeqLiteral = Trees.SeqLiteral[T]
    type JavaSeqLiteral = Trees.JavaSeqLiteral[T]
    type Inlined = Trees.Inlined[T]
    type Quote = Trees.Quote[T]
    type Splice = Trees.Splice[T]
    type QuotePattern = Trees.QuotePattern[T]
    type SplicePattern = Trees.SplicePattern[T]
    type TypeTree = Trees.TypeTree[T]
    type InferredTypeTree = Trees.InferredTypeTree[T]
    type SingletonTypeTree = Trees.SingletonTypeTree[T]
    type RefinedTypeTree = Trees.RefinedTypeTree[T]
    type AppliedTypeTree = Trees.AppliedTypeTree[T]
    type LambdaTypeTree = Trees.LambdaTypeTree[T]
    type TermLambdaTypeTree = Trees.TermLambdaTypeTree[T]
    type MatchTypeTree = Trees.MatchTypeTree[T]
    type ByNameTypeTree = Trees.ByNameTypeTree[T]
    type TypeBoundsTree = Trees.TypeBoundsTree[T]
    type Bind = Trees.Bind[T]
    type Alternative = Trees.Alternative[T]
    type UnApply = Trees.UnApply[T]
    type ValDef = Trees.ValDef[T]
    type DefDef = Trees.DefDef[T]
    type TypeDef = Trees.TypeDef[T]
    type Template = Trees.Template[T]
    type Import = Trees.Import[T]
    type Export = Trees.Export[T]
    type ImportOrExport = Trees.ImportOrExport[T]
    type PackageDef = Trees.PackageDef[T]
    type Annotated = Trees.Annotated[T]
    type Thicket = Trees.Thicket[T]

    type Hole = Trees.Hole[T]

    @sharable val EmptyTree: Thicket = genericEmptyTree
    @sharable val EmptyValDef: ValDef = genericEmptyValDef

    // ----- Auxiliary creation methods ------------------

    def Thicket(): Thicket = EmptyTree
    def Thicket(x1: Tree, x2: Tree)(implicit src: SourceFile): Thicket = new Thicket(x1 :: x2 :: Nil)
    def Thicket(x1: Tree, x2: Tree, x3: Tree)(implicit src: SourceFile): Thicket = new Thicket(x1 :: x2 :: x3 :: Nil)
    def Thicket(xs: List[Tree])(implicit src: SourceFile) = new Thicket(xs)

    def flatTree(xs: List[Tree])(implicit src: SourceFile): Tree = flatten(xs) match {
      case x :: Nil => x
      case ys => Thicket(ys)
    }

    // ----- Helper classes for copying, transforming, accumulating -----------------

    val cpy: TreeCopier

    /** A class for copying trees. The copy methods avoid creating a new tree
     *  If all arguments stay the same.
     *
     * Note: Some of the copy methods take a context.
     * These are exactly those methods that are overridden in TypedTreeCopier
     * so that they selectively retype themselves. Retyping needs a context.
     */
    abstract class TreeCopier {
      protected def postProcess(tree: Tree, copied: untpd.Tree): copied.ThisTree[T]
      protected def postProcess(tree: Tree, copied: untpd.MemberDef): copied.ThisTree[T]

      /** Soucre of the copied tree */
      protected def sourceFile(tree: Tree): SourceFile = tree.source

      protected def finalize(tree: Tree, copied: untpd.Tree): copied.ThisTree[T] =
        Stats.record(s"TreeCopier.finalize/${tree.getClass == copied.getClass}")
        postProcess(tree, copied.withSpan(tree.span).withAttachmentsFrom(tree))

      protected def finalize(tree: Tree, copied: untpd.MemberDef): copied.ThisTree[T] =
        Stats.record(s"TreeCopier.finalize/${tree.getClass == copied.getClass}")
        postProcess(tree, copied.withSpan(tree.span).withAttachmentsFrom(tree))

      def Ident(tree: Tree)(name: Name)(using Context): Ident = tree match {
        case tree: Ident if name == tree.name => tree
        case _ => finalize(tree, untpd.Ident(name)(sourceFile(tree)))
      }
      def Select(tree: Tree)(qualifier: Tree, name: Name)(using Context): Select = tree match {
        case tree: SelectWithSig =>
          if ((qualifier eq tree.qualifier) && (name == tree.name)) tree
          else finalize(tree, SelectWithSig(qualifier, name, tree.sig)(sourceFile(tree)))
        case tree: Select if (qualifier eq tree.qualifier) && (name == tree.name) => tree
        case _ => finalize(tree, untpd.Select(qualifier, name)(sourceFile(tree)))
      }
      /** Copy Ident or Select trees */
      def Ref(tree: RefTree)(name: Name)(using Context): RefTree = tree match {
        case Ident(_) => Ident(tree)(name)
        case Select(qual, _) => Select(tree)(qual, name)
      }
      def This(tree: Tree)(qual: untpd.Ident)(using Context): This = tree match {
        case tree: This if (qual eq tree.qual) => tree
        case _ => finalize(tree, untpd.This(qual)(sourceFile(tree)))
      }
      def Super(tree: Tree)(qual: Tree, mix: untpd.Ident)(using Context): Super = tree match {
        case tree: Super if (qual eq tree.qual) && (mix eq tree.mix) => tree
        case _ => finalize(tree, untpd.Super(qual, mix)(sourceFile(tree)))
      }
      def Apply(tree: Tree)(fun: Tree, args: List[Tree])(using Context): Apply = tree match {
        case tree: Apply if (fun eq tree.fun) && (args eq tree.args) => tree
        case _ => finalize(tree, untpd.Apply(fun, args)(sourceFile(tree)))
            //.ensuring(res => res.uniqueId != 2213, s"source = $tree, ${tree.uniqueId}, ${tree.span}")
      }
      def TypeApply(tree: Tree)(fun: Tree, args: List[Tree])(using Context): TypeApply = tree match {
        case tree: TypeApply if (fun eq tree.fun) && (args eq tree.args) => tree
        case _ => finalize(tree, untpd.TypeApply(fun, args)(sourceFile(tree)))
      }
      def Literal(tree: Tree)(const: Constant)(using Context): Literal = tree match {
        case tree: Literal if const == tree.const => tree
        case _ => finalize(tree, untpd.Literal(const)(sourceFile(tree)))
      }
      def New(tree: Tree)(tpt: Tree)(using Context): New = tree match {
        case tree: New if (tpt eq tree.tpt) => tree
        case _ => finalize(tree, untpd.New(tpt)(sourceFile(tree)))
      }
      def Typed(tree: Tree)(expr: Tree, tpt: Tree)(using Context): Typed = tree match {
        case tree: Typed if (expr eq tree.expr) && (tpt eq tree.tpt) => tree
        case tree => finalize(tree, untpd.Typed(expr, tpt)(sourceFile(tree)))
      }
      def NamedArg(tree: Tree)(name: Name, arg: Tree)(using Context): NamedArg = tree match {
        case tree: NamedArg if (name == tree.name) && (arg eq tree.arg) => tree
        case _ => finalize(tree, untpd.NamedArg(name, arg)(sourceFile(tree)))
      }
      def Assign(tree: Tree)(lhs: Tree, rhs: Tree)(using Context): Assign = tree match {
        case tree: Assign if (lhs eq tree.lhs) && (rhs eq tree.rhs) => tree
        case _ => finalize(tree, untpd.Assign(lhs, rhs)(sourceFile(tree)))
      }
      def Block(tree: Tree)(stats: List[Tree], expr: Tree)(using Context): Block = tree match {
        case tree: Block if (stats eq tree.stats) && (expr eq tree.expr) => tree
        case _ => finalize(tree, untpd.Block(stats, expr)(sourceFile(tree)))
      }
      def If(tree: Tree)(cond: Tree, thenp: Tree, elsep: Tree)(using Context): If = tree match {
        case tree: If if (cond eq tree.cond) && (thenp eq tree.thenp) && (elsep eq tree.elsep) => tree
        case tree: InlineIf => finalize(tree, untpd.InlineIf(cond, thenp, elsep)(sourceFile(tree)))
        case _ => finalize(tree, untpd.If(cond, thenp, elsep)(sourceFile(tree)))
      }
      def Closure(tree: Tree)(env: List[Tree], meth: Tree, tpt: Tree)(using Context): Closure = tree match {
        case tree: Closure if (env eq tree.env) && (meth eq tree.meth) && (tpt eq tree.tpt) => tree
        case _ => finalize(tree, untpd.Closure(env, meth, tpt)(sourceFile(tree)))
      }
      def Match(tree: Tree)(selector: Tree, cases: List[CaseDef])(using Context): Match = tree match {
        case tree: Match if (selector eq tree.selector) && (cases eq tree.cases) => tree
        case tree: InlineMatch => finalize(tree, untpd.InlineMatch(selector, cases)(sourceFile(tree)))
        case _ => finalize(tree, untpd.Match(selector, cases)(sourceFile(tree)))
      }
      def CaseDef(tree: Tree)(pat: Tree, guard: Tree, body: Tree)(using Context): CaseDef = tree match {
        case tree: CaseDef if (pat eq tree.pat) && (guard eq tree.guard) && (body eq tree.body) => tree
        case _ => finalize(tree, untpd.CaseDef(pat, guard, body)(sourceFile(tree)))
      }
      def Labeled(tree: Tree)(bind: Bind, expr: Tree)(using Context): Labeled = tree match {
        case tree: Labeled if (bind eq tree.bind) && (expr eq tree.expr) => tree
        case _ => finalize(tree, untpd.Labeled(bind, expr)(sourceFile(tree)))
      }
      def Return(tree: Tree)(expr: Tree, from: Tree)(using Context): Return = tree match {
        case tree: Return if (expr eq tree.expr) && (from eq tree.from) => tree
        case _ => finalize(tree, untpd.Return(expr, from)(sourceFile(tree)))
      }
      def WhileDo(tree: Tree)(cond: Tree, body: Tree)(using Context): WhileDo = tree match {
        case tree: WhileDo if (cond eq tree.cond) && (body eq tree.body) => tree
        case _ => finalize(tree, untpd.WhileDo(cond, body)(sourceFile(tree)))
      }
      def Try(tree: Tree)(expr: Tree, cases: List[CaseDef], finalizer: Tree)(using Context): Try = tree match {
        case tree: Try if (expr eq tree.expr) && (cases eq tree.cases) && (finalizer eq tree.finalizer) => tree
        case _ => finalize(tree, untpd.Try(expr, cases, finalizer)(sourceFile(tree)))
      }
      def SeqLiteral(tree: Tree)(elems: List[Tree], elemtpt: Tree)(using Context): SeqLiteral = tree match {
        case tree: JavaSeqLiteral =>
          if ((elems eq tree.elems) && (elemtpt eq tree.elemtpt)) tree
          else finalize(tree, untpd.JavaSeqLiteral(elems, elemtpt))
        case tree: SeqLiteral if (elems eq tree.elems) && (elemtpt eq tree.elemtpt) => tree
        case _ => finalize(tree, untpd.SeqLiteral(elems, elemtpt)(sourceFile(tree)))
      }
      // Positions of trees are automatically pushed down except when we reach an Inlined tree. Therefore, we
      // make sure the new expansion has a position by copying the one of the original Inlined tree.
      def Inlined(tree: Inlined)(call: tpd.Tree, bindings: List[MemberDef], expansion: Tree)(using Context): Inlined =
        if (call eq tree.call) && (bindings eq tree.bindings) && (expansion eq tree.expansion) then tree
        else
          // Copy the span from the original Inlined tree if the new expansion doesn't have a span.
          val expansionWithSpan =
            if expansion.span.exists then expansion
            else expansion.withSpan(tree.expansion.span)
          finalize(tree, untpd.Inlined(call, bindings, expansionWithSpan)(sourceFile(tree)))

      def Quote(tree: Tree)(body: Tree, tags: List[Tree])(using Context): Quote = tree match {
        case tree: Quote if (body eq tree.body) && (tags eq tree.tags) => tree
        case _ => finalize(tree, untpd.Quote(body, tags)(sourceFile(tree)))
      }
      def Splice(tree: Tree)(expr: Tree)(using Context): Splice = tree match {
        case tree: Splice if (expr eq tree.expr) => tree
        case _ => finalize(tree, untpd.Splice(expr)(sourceFile(tree)))
      }
      def QuotePattern(tree: Tree)(bindings: List[Tree], body: Tree, quotes: Tree)(using Context): QuotePattern = tree match {
        case tree: QuotePattern if (bindings eq tree.bindings) && (body eq tree.body) && (quotes eq tree.quotes) => tree
        case _ => finalize(tree, untpd.QuotePattern(bindings, body, quotes)(sourceFile(tree)))
      }
      def SplicePattern(tree: Tree)(body: Tree, args: List[Tree])(using Context): SplicePattern = tree match {
        case tree: SplicePattern if (body eq tree.body) && (args eq tree.args) => tree
        case _ => finalize(tree, untpd.SplicePattern(body, args)(sourceFile(tree)))
      }
      def SingletonTypeTree(tree: Tree)(ref: Tree)(using Context): SingletonTypeTree = tree match {
        case tree: SingletonTypeTree if (ref eq tree.ref) => tree
        case _ => finalize(tree, untpd.SingletonTypeTree(ref)(sourceFile(tree)))
      }
      def RefinedTypeTree(tree: Tree)(tpt: Tree, refinements: List[Tree])(using Context): RefinedTypeTree = tree match {
        case tree: RefinedTypeTree if (tpt eq tree.tpt) && (refinements eq tree.refinements) => tree
        case _ => finalize(tree, untpd.RefinedTypeTree(tpt, refinements)(sourceFile(tree)))
      }
      def AppliedTypeTree(tree: Tree)(tpt: Tree, args: List[Tree])(using Context): AppliedTypeTree = tree match {
        case tree: AppliedTypeTree if (tpt eq tree.tpt) && (args eq tree.args) => tree
        case _ => finalize(tree, untpd.AppliedTypeTree(tpt, args)(sourceFile(tree)))
      }
      def LambdaTypeTree(tree: Tree)(tparams: List[TypeDef], body: Tree)(using Context): LambdaTypeTree = tree match {
        case tree: LambdaTypeTree if (tparams eq tree.tparams) && (body eq tree.body) => tree
        case _ => finalize(tree, untpd.LambdaTypeTree(tparams, body)(sourceFile(tree)))
      }
      def TermLambdaTypeTree(tree: Tree)(params: List[ValDef], body: Tree)(using Context): TermLambdaTypeTree = tree match {
        case tree: TermLambdaTypeTree if (params eq tree.params) && (body eq tree.body) => tree
        case _ => finalize(tree, untpd.TermLambdaTypeTree(params, body)(sourceFile(tree)))
      }
      def MatchTypeTree(tree: Tree)(bound: Tree, selector: Tree, cases: List[CaseDef])(using Context): MatchTypeTree = tree match {
        case tree: MatchTypeTree if (bound eq tree.bound) && (selector eq tree.selector) && (cases eq tree.cases) => tree
        case _ => finalize(tree, untpd.MatchTypeTree(bound, selector, cases)(sourceFile(tree)))
      }
      def ByNameTypeTree(tree: Tree)(result: Tree)(using Context): ByNameTypeTree = tree match {
        case tree: ByNameTypeTree if (result eq tree.result) => tree
        case _ => finalize(tree, untpd.ByNameTypeTree(result)(sourceFile(tree)))
      }
      def TypeBoundsTree(tree: Tree)(lo: Tree, hi: Tree, alias: Tree)(using Context): TypeBoundsTree = tree match {
        case tree: TypeBoundsTree if (lo eq tree.lo) && (hi eq tree.hi) && (alias eq tree.alias) => tree
        case _ => finalize(tree, untpd.TypeBoundsTree(lo, hi, alias)(sourceFile(tree)))
      }
      def Bind(tree: Tree)(name: Name, body: Tree)(using Context): Bind = tree match {
        case tree: Bind if (name eq tree.name) && (body eq tree.body) => tree
        case _ => finalize(tree, untpd.Bind(name, body)(sourceFile(tree)))
      }
      def Alternative(tree: Tree)(trees: List[Tree])(using Context): Alternative = tree match {
        case tree: Alternative if (trees eq tree.trees) => tree
        case _ => finalize(tree, untpd.Alternative(trees)(sourceFile(tree)))
      }
      def UnApply(tree: Tree)(fun: Tree, implicits: List[Tree], patterns: List[Tree])(using Context): UnApply = tree match {
        case tree: UnApply if (fun eq tree.fun) && (implicits eq tree.implicits) && (patterns eq tree.patterns) => tree
        case _ => finalize(tree, untpd.UnApply(fun, implicits, patterns)(sourceFile(tree)))
      }
      def ValDef(tree: Tree)(name: TermName, tpt: Tree, rhs: LazyTree)(using Context): ValDef = tree match {
        case tree: ValDef if (name == tree.name) && (tpt eq tree.tpt) && (rhs eq tree.unforcedRhs) => tree
        case _ => finalize(tree, untpd.ValDef(name, tpt, rhs)(sourceFile(tree)))
      }
      def DefDef(tree: Tree)(name: TermName, paramss: List[ParamClause], tpt: Tree, rhs: LazyTree)(using Context): DefDef = tree match {
        case tree: DefDef if (name == tree.name) && (paramss eq tree.paramss) && (tpt eq tree.tpt) && (rhs eq tree.unforcedRhs) => tree
        case _ => finalize(tree, untpd.DefDef(name, paramss, tpt, rhs)(sourceFile(tree)))
      }
      def TypeDef(tree: Tree)(name: TypeName, rhs: Tree)(using Context): TypeDef = tree match {
        case tree: TypeDef if (name == tree.name) && (rhs eq tree.rhs) => tree
        case _ => finalize(tree, untpd.TypeDef(name, rhs)(sourceFile(tree)))
      }
      def Template(tree: Tree)(constr: DefDef, parents: List[Tree], derived: List[untpd.Tree], self: ValDef, body: LazyTreeList)(using Context): Template = tree match {
        case tree: Template if (constr eq tree.constr) && (parents eq tree.parents) && (derived eq tree.derived) && (self eq tree.self) && (body eq tree.unforcedBody) => tree
        case tree => finalize(tree, untpd.Template(constr, parents, derived, self, body)(sourceFile(tree)))
      }
      def Import(tree: Tree)(expr: Tree, selectors: List[untpd.ImportSelector])(using Context): Import = tree match {
        case tree: Import if (expr eq tree.expr) && (selectors eq tree.selectors) => tree
        case _ => finalize(tree, untpd.Import(expr, selectors)(sourceFile(tree)))
      }
      def Export(tree: Tree)(expr: Tree, selectors: List[untpd.ImportSelector])(using Context): Export = tree match {
        case tree: Export if (expr eq tree.expr) && (selectors eq tree.selectors) => tree
        case _ => finalize(tree, untpd.Export(expr, selectors)(sourceFile(tree)))
      }
      def PackageDef(tree: Tree)(pid: RefTree, stats: List[Tree])(using Context): PackageDef = tree match {
        case tree: PackageDef if (pid eq tree.pid) && (stats eq tree.stats) => tree
        case _ => finalize(tree, untpd.PackageDef(pid, stats)(sourceFile(tree)))
      }
      def Annotated(tree: Tree)(arg: Tree, annot: Tree)(using Context): Annotated = tree match {
        case tree: Annotated if (arg eq tree.arg) && (annot eq tree.annot) => tree
        case _ => finalize(tree, untpd.Annotated(arg, annot)(sourceFile(tree)))
      }
      def Thicket(tree: Tree)(trees: List[Tree])(using Context): Thicket = tree match {
        case tree: Thicket if (trees eq tree.trees) => tree
        case _ => finalize(tree, untpd.Thicket(trees)(sourceFile(tree)))
      }
      def Hole(tree: Tree)(isTerm: Boolean, idx: Int, args: List[Tree], content: Tree)(using Context): Hole = tree match {
        case tree: Hole if isTerm == tree.isTerm && idx == tree.idx && args.eq(tree.args) && content.eq(tree.content) && content.eq(tree.content) => tree
        case _ => finalize(tree, untpd.Hole(isTerm, idx, args, content)(sourceFile(tree)))
      }

      // Copier methods with default arguments; these demand that the original tree
      // is of the same class as the copy. We only include trees with more than 2 elements here.
      def If(tree: If)(cond: Tree = tree.cond, thenp: Tree = tree.thenp, elsep: Tree = tree.elsep)(using Context): If =
        If(tree: Tree)(cond, thenp, elsep)
      def Closure(tree: Closure)(env: List[Tree] = tree.env, meth: Tree = tree.meth, tpt: Tree = tree.tpt)(using Context): Closure =
        Closure(tree: Tree)(env, meth, tpt)
      def CaseDef(tree: CaseDef)(pat: Tree = tree.pat, guard: Tree = tree.guard, body: Tree = tree.body)(using Context): CaseDef =
        CaseDef(tree: Tree)(pat, guard, body)
      def Try(tree: Try)(expr: Tree = tree.expr, cases: List[CaseDef] = tree.cases, finalizer: Tree = tree.finalizer)(using Context): Try =
        Try(tree: Tree)(expr, cases, finalizer)
      def UnApply(tree: UnApply)(fun: Tree = tree.fun, implicits: List[Tree] = tree.implicits, patterns: List[Tree] = tree.patterns)(using Context): UnApply =
        UnApply(tree: Tree)(fun, implicits, patterns)
      def ValDef(tree: ValDef)(name: TermName = tree.name, tpt: Tree = tree.tpt, rhs: LazyTree = tree.unforcedRhs)(using Context): ValDef =
        ValDef(tree: Tree)(name, tpt, rhs)
      def DefDef(tree: DefDef)(name: TermName = tree.name, paramss: List[ParamClause] = tree.paramss, tpt: Tree = tree.tpt, rhs: LazyTree = tree.unforcedRhs)(using Context): DefDef =
        DefDef(tree: Tree)(name, paramss, tpt, rhs)
      def TypeDef(tree: TypeDef)(name: TypeName = tree.name, rhs: Tree = tree.rhs)(using Context): TypeDef =
        TypeDef(tree: Tree)(name, rhs)
      def Template(tree: Template)(using Context)(constr: DefDef = tree.constr, parents: List[Tree] = tree.parents, derived: List[untpd.Tree] = tree.derived, self: ValDef = tree.self, body: LazyTreeList = tree.unforcedBody): Template =
        Template(tree: Tree)(constr, parents, derived, self, body)
      def Hole(tree: Hole)(isTerm: Boolean = tree.isTerm, idx: Int = tree.idx, args: List[Tree] = tree.args, content: Tree = tree.content)(using Context): Hole =
        Hole(tree: Tree)(isTerm, idx, args, content)

    }

    /** Hook to indicate that a transform of some subtree should be skipped */
    protected def skipTransform(tree: Tree)(using Context): Boolean = false

    /** For untyped trees, this is just the identity.
     *  For typed trees, a context derived form `ctx` that records `call` as the
     *  innermost enclosing call for which the inlined version is currently
     *  processed.
     */
    protected def inlineContext(tree: Inlined)(using Context): Context = ctx

    /** The context to use when mapping or accumulating over a tree */
    def localCtx(tree: Tree)(using Context): Context

    /** The context to use when transforming a tree.
      * It ensures that the source is correct, and that the local context is used if
      * that's necessary for transforming the whole tree.
      * TODO: ensure transform is always called with the correct context as argument
      * @see https://github.com/lampepfl/dotty/pull/13880#discussion_r836395977
      */
    def transformCtx(tree: Tree)(using Context): Context =
      val sourced =
        if tree.source.exists && tree.source != ctx.source
        then ctx.withSource(tree.source)
        else ctx
      tree match
        case t: (MemberDef | PackageDef | LambdaTypeTree | TermLambdaTypeTree) =>
          localCtx(t)(using sourced)
        case _ =>
          sourced

    abstract class TreeMap(val cpy: TreeCopier = inst.cpy) { self =>
      def transform(tree: Tree)(using Context): Tree = {
        inContext(transformCtx(tree)) {
          Stats.record(s"TreeMap.transform/$getClass")
          if (skipTransform(tree)) tree
          else tree match {
            case Ident(name) =>
              tree
            case Select(qualifier, name) =>
              cpy.Select(tree)(transform(qualifier), name)
            case This(qual) =>
              tree
            case Super(qual, mix) =>
              cpy.Super(tree)(transform(qual), mix)
            case Apply(fun, args) =>
              cpy.Apply(tree)(transform(fun), transform(args))
            case TypeApply(fun, args) =>
              cpy.TypeApply(tree)(transform(fun), transform(args))
            case Literal(const) =>
              tree
            case New(tpt) =>
              cpy.New(tree)(transform(tpt))
            case Typed(expr, tpt) =>
              cpy.Typed(tree)(transform(expr), transform(tpt))
            case NamedArg(name, arg) =>
              cpy.NamedArg(tree)(name, transform(arg))
            case Assign(lhs, rhs) =>
              cpy.Assign(tree)(transform(lhs), transform(rhs))
            case blk: Block =>
              transformBlock(blk)
            case If(cond, thenp, elsep) =>
              cpy.If(tree)(transform(cond), transform(thenp), transform(elsep))
            case Closure(env, meth, tpt) =>
              cpy.Closure(tree)(transform(env), transform(meth), transform(tpt))
            case Match(selector, cases) =>
              cpy.Match(tree)(transform(selector), transformSub(cases))
            case CaseDef(pat, guard, body) =>
              cpy.CaseDef(tree)(transform(pat), transform(guard), transform(body))
            case Labeled(bind, expr) =>
              cpy.Labeled(tree)(transformSub(bind), transform(expr))
            case Return(expr, from) =>
              cpy.Return(tree)(transform(expr), transformSub(from))
            case WhileDo(cond, body) =>
              cpy.WhileDo(tree)(transform(cond), transform(body))
            case Try(block, cases, finalizer) =>
              cpy.Try(tree)(transform(block), transformSub(cases), transform(finalizer))
            case SeqLiteral(elems, elemtpt) =>
              cpy.SeqLiteral(tree)(transform(elems), transform(elemtpt))
            case tree @ Inlined(call, bindings, expansion) =>
              cpy.Inlined(tree)(call, transformSub(bindings), transform(expansion)(using inlineContext(tree)))
            case TypeTree() =>
              tree
            case SingletonTypeTree(ref) =>
              cpy.SingletonTypeTree(tree)(transform(ref))
            case RefinedTypeTree(tpt, refinements) =>
              cpy.RefinedTypeTree(tree)(transform(tpt), transformSub(refinements))
            case AppliedTypeTree(tpt, args) =>
              cpy.AppliedTypeTree(tree)(transform(tpt), transform(args))
            case LambdaTypeTree(tparams, body) =>
              cpy.LambdaTypeTree(tree)(transformSub(tparams), transform(body))
            case TermLambdaTypeTree(params, body) =>
              cpy.TermLambdaTypeTree(tree)(transformSub(params), transform(body))
            case MatchTypeTree(bound, selector, cases) =>
              cpy.MatchTypeTree(tree)(transform(bound), transform(selector), transformSub(cases))
            case ByNameTypeTree(result) =>
              cpy.ByNameTypeTree(tree)(transform(result))
            case TypeBoundsTree(lo, hi, alias) =>
              cpy.TypeBoundsTree(tree)(transform(lo), transform(hi), transform(alias))
            case Bind(name, body) =>
              cpy.Bind(tree)(name, transform(body))
            case Alternative(trees) =>
              cpy.Alternative(tree)(transform(trees))
            case UnApply(fun, implicits, patterns) =>
              cpy.UnApply(tree)(transform(fun), transform(implicits), transform(patterns))
            case EmptyValDef =>
              tree
            case tree @ ValDef(name, tpt, _) =>
              val tpt1 = transform(tpt)
              val rhs1 = transform(tree.rhs)
              cpy.ValDef(tree)(name, tpt1, rhs1)
            case tree @ DefDef(name, paramss, tpt, _) =>
              cpy.DefDef(tree)(name, transformParamss(paramss), transform(tpt), transform(tree.rhs))
            case tree @ TypeDef(name, rhs) =>
              cpy.TypeDef(tree)(name, transform(rhs))
            case tree @ Template(constr, parents, self, _) if tree.derived.isEmpty =>
              // Currently we do not have cases where we expect `tree.derived` to contain trees for typed trees.
              // If it is the case we will fall in `transformMoreCases` and throw an exception there.
              // In the future we might keep the `derived` clause after typing, in that case we might want to start handling it here.
              cpy.Template(tree)(transformSub(constr), transform(tree.parents), Nil, transformSub(self), transformStats(tree.body, tree.symbol))
            case Import(expr, selectors) =>
              cpy.Import(tree)(transform(expr), selectors)
            case Export(expr, selectors) =>
              cpy.Export(tree)(transform(expr), selectors)
            case PackageDef(pid, stats) =>
              cpy.PackageDef(tree)(transformSub(pid), transformStats(stats, ctx.owner))
            case Annotated(arg, annot) =>
              cpy.Annotated(tree)(transform(arg), transform(annot))
            case Thicket(trees) =>
              val trees1 = transform(trees)
              if (trees1 eq trees) tree else Thicket(trees1)
            case Quote(body, tags) =>
              cpy.Quote(tree)(transform(body)(using quoteContext), transform(tags))
            case tree @ Splice(expr) =>
              cpy.Splice(tree)(transform(expr)(using spliceContext))
            case tree @ QuotePattern(bindings, body, quotes) =>
              cpy.QuotePattern(tree)(transform(bindings), transform(body)(using quoteContext), transform(quotes))
            case tree @ SplicePattern(body, args) =>
              cpy.SplicePattern(tree)(transform(body)(using spliceContext), transform(args))
            case tree @ Hole(isTerm, idx, args, content) =>
              cpy.Hole(tree)(isTerm, idx, transform(args), transform(content))
            case _ =>
              transformMoreCases(tree)
          }
        }
      }

      def transformStats(trees: List[Tree], exprOwner: Symbol)(using Context): List[Tree] =
        transform(trees)
      def transformBlock(blk: Block)(using Context): Block =
        cpy.Block(blk)(transformStats(blk.stats, ctx.owner), transform(blk.expr))
      def transform(trees: List[Tree])(using Context): List[Tree] =
        flatten(trees mapConserve (transform(_)))
      def transformSub[Tr <: Tree](tree: Tr)(using Context): Tr =
        transform(tree).asInstanceOf[Tr]
      def transformSub[Tr <: Tree](trees: List[Tr])(using Context): List[Tr] =
        transform(trees).asInstanceOf[List[Tr]]
      def transformParams(params: ParamClause)(using Context): ParamClause =
        transform(params).asInstanceOf[ParamClause]
      def transformParamss(paramss: List[ParamClause])(using Context): List[ParamClause] =
        paramss.mapConserve(transformParams)

      protected def transformMoreCases(tree: Tree)(using Context): Tree = {
        assert(ctx.reporter.errorsReported)
        tree
      }
    }

    abstract class TreeAccumulator[X] { self =>
      // Ties the knot of the traversal: call `foldOver(x, tree))` to dive in the `tree` node.
      def apply(x: X, tree: Tree)(using Context): X

      def apply(x: X, trees: List[Tree])(using Context): X =
        def fold(x: X, trees: List[Tree]): X = trees match
          case tree :: rest => fold(apply(x, tree), rest)
          case Nil => x
        fold(x, trees)

      def foldOver(x: X, tree: Tree)(using Context): X =
        if (tree.source != ctx.source && tree.source.exists)
          foldOver(x, tree)(using ctx.withSource(tree.source))
        else {
          Stats.record(s"TreeAccumulator.foldOver/$getClass")
          tree match {
            case Ident(name) =>
              x
            case Select(qualifier, name) =>
              this(x, qualifier)
            case This(qual) =>
              x
            case Super(qual, mix) =>
              this(x, qual)
            case Apply(fun, args) =>
              this(this(x, fun), args)
            case TypeApply(fun, args) =>
              this(this(x, fun), args)
            case Literal(const) =>
              x
            case New(tpt) =>
              this(x, tpt)
            case Typed(expr, tpt) =>
              this(this(x, expr), tpt)
            case NamedArg(name, arg) =>
              this(x, arg)
            case Assign(lhs, rhs) =>
              this(this(x, lhs), rhs)
            case Block(stats, expr) =>
              this(this(x, stats), expr)
            case If(cond, thenp, elsep) =>
              this(this(this(x, cond), thenp), elsep)
            case Closure(env, meth, tpt) =>
              this(this(this(x, env), meth), tpt)
            case Match(selector, cases) =>
              this(this(x, selector), cases)
            case CaseDef(pat, guard, body) =>
              this(this(this(x, pat), guard), body)
            case Labeled(bind, expr) =>
              this(this(x, bind), expr)
            case Return(expr, from) =>
              this(this(x, expr), from)
            case WhileDo(cond, body) =>
              this(this(x, cond), body)
            case Try(block, handler, finalizer) =>
              this(this(this(x, block), handler), finalizer)
            case SeqLiteral(elems, elemtpt) =>
              this(this(x, elems), elemtpt)
            case tree @ Inlined(call, bindings, expansion) =>
              this(this(x, bindings), expansion)(using inlineContext(tree))
            case TypeTree() =>
              x
            case SingletonTypeTree(ref) =>
              this(x, ref)
            case RefinedTypeTree(tpt, refinements) =>
              this(this(x, tpt), refinements)
            case AppliedTypeTree(tpt, args) =>
              this(this(x, tpt), args)
            case LambdaTypeTree(tparams, body) =>
              inContext(localCtx(tree)) {
                this(this(x, tparams), body)
              }
            case TermLambdaTypeTree(params, body) =>
              inContext(localCtx(tree)) {
                this(this(x, params), body)
              }
            case MatchTypeTree(bound, selector, cases) =>
              this(this(this(x, bound), selector), cases)
            case ByNameTypeTree(result) =>
              this(x, result)
            case TypeBoundsTree(lo, hi, alias) =>
              this(this(this(x, lo), hi), alias)
            case Bind(name, body) =>
              this(x, body)
            case Alternative(trees) =>
              this(x, trees)
            case UnApply(fun, implicits, patterns) =>
              this(this(this(x, fun), implicits), patterns)
            case tree @ ValDef(_, tpt, _) =>
              inContext(localCtx(tree)) {
                this(this(x, tpt), tree.rhs)
              }
            case tree @ DefDef(_, paramss, tpt, _) =>
              inContext(localCtx(tree)) {
                this(this(paramss.foldLeft(x)(apply), tpt), tree.rhs)
              }
            case TypeDef(_, rhs) =>
              inContext(localCtx(tree)) {
                this(x, rhs)
              }
            case tree @ Template(constr, _, self, _) if tree.derived.isEmpty =>
              this(this(this(this(x, constr), tree.parents), self), tree.body)
            case Import(expr, _) =>
              this(x, expr)
            case Export(expr, _) =>
              this(x, expr)
            case PackageDef(pid, stats) =>
              this(this(x, pid), stats)(using localCtx(tree))
            case Annotated(arg, annot) =>
              this(this(x, arg), annot)
            case Thicket(ts) =>
              this(x, ts)
            case Quote(body, tags) =>
              this(this(x, body)(using quoteContext), tags)
            case Splice(expr) =>
              this(x, expr)(using spliceContext)
            case QuotePattern(bindings, body, quotes) =>
              this(this(this(x, bindings), body)(using quoteContext), quotes)
            case SplicePattern(body, args) =>
              this(this(x, body)(using spliceContext), args)
            case Hole(_, _, args, content) =>
              this(this(x, args), content)
            case _ =>
              foldMoreCases(x, tree)
          }
        }

      def foldMoreCases(x: X, tree: Tree)(using Context): X = {
        assert(ctx.reporter.hasUnreportedErrors
                || ctx.reporter.errorsReported
                || ctx.mode.is(Mode.Interactive), tree)
          // In interactive mode, errors might come from previous runs.
          // In case of errors it may be that typed trees point to untyped ones.
          // The IDE can still traverse inside such trees, either in the run where errors
          // are reported, or in subsequent ones.
        x
      }
    }

    abstract class TreeTraverser extends TreeAccumulator[Unit] {
      def traverse(tree: Tree)(using Context): Unit
      def traverse(trees: List[Tree])(using Context) = apply((), trees)
      def apply(x: Unit, tree: Tree)(using Context): Unit = traverse(tree)
      protected def traverseChildren(tree: Tree)(using Context): Unit = foldOver((), tree)
    }

    /** Fold `f` over all tree nodes, in depth-first, prefix order */
    class DeepFolder[X](f: (X, Tree) => X) extends TreeAccumulator[X] {
      def apply(x: X, tree: Tree)(using Context): X = foldOver(f(x, tree), tree)
    }

    /** Fold `f` over all tree nodes, in depth-first, prefix order, but don't visit
     *  subtrees where `f` returns a different result for the root, i.e. `f(x, root) ne x`.
     */
    class ShallowFolder[X](f: (X, Tree) => X) extends TreeAccumulator[X] {
      def apply(x: X, tree: Tree)(using Context): X = {
        val x1 = f(x, tree)
        if (x1.asInstanceOf[AnyRef] ne x.asInstanceOf[AnyRef]) x1
        else foldOver(x1, tree)
      }
    }

    def rename(tree: NameTree, newName: Name)(using Context): tree.ThisTree[T] = {
      tree match {
        case tree: Ident => cpy.Ident(tree)(newName)
        case tree: Select => cpy.Select(tree)(tree.qualifier, newName)
        case tree: Bind => cpy.Bind(tree)(newName, tree.body)
        case tree: ValDef => cpy.ValDef(tree)(name = newName.asTermName)
        case tree: DefDef => cpy.DefDef(tree)(name = newName.asTermName)
        case tree: TypeDef => cpy.TypeDef(tree)(name = newName.asTypeName)
      }
    }.asInstanceOf[tree.ThisTree[T]]

    object TypeDefs:
      def unapply(xs: List[Tree]): Option[List[TypeDef]] = xs match
        case (x: TypeDef) :: _ => Some(xs.asInstanceOf[List[TypeDef]])
        case _ => None

    object ValDefs:
      def unapply(xs: List[Tree]): Option[List[ValDef]] = xs match
        case Nil => Some(Nil)
        case (x: ValDef) :: _ => Some(xs.asInstanceOf[List[ValDef]])
        case _ => None

    def termParamssIn(paramss: List[ParamClause]): List[List[ValDef]] = paramss match
      case ValDefs(vparams) :: paramss1 =>
        val paramss2 = termParamssIn(paramss1)
        if paramss2 eq paramss1 then paramss.asInstanceOf[List[List[ValDef]]]
        else vparams :: paramss2
      case _ :: paramss1 =>
        termParamssIn(paramss1)
      case nil =>
        Nil

    /** If `tparams` is non-empty, add it to the left `paramss`, merging
     *  it with a leading type parameter list of `paramss`, if one exists.
     */
    def joinParams(tparams: List[TypeDef], paramss: List[ParamClause]): List[ParamClause] =
      if tparams.isEmpty then paramss
      else paramss match
        case TypeDefs(tparams1) :: paramss1 => (tparams ++ tparams1) :: paramss1
        case _ => tparams :: paramss

    def isTermOnly(paramss: List[ParamClause]): Boolean = paramss match
      case Nil => true
      case params :: paramss1 =>
        params match
          case (param: untpd.TypeDef) :: _ => false
          case _ => isTermOnly(paramss1)

    def asTermOnly(paramss: List[ParamClause]): List[List[ValDef]] =
      assert(isTermOnly(paramss))
      paramss.asInstanceOf[List[List[ValDef]]]

    /** Delegate to FunProto or FunProtoTyped depending on whether the prefix is `untpd` or `tpd`. */
    protected def FunProto(args: List[Tree], resType: Type)(using Context): ProtoTypes.FunProto

    /** Construct the application `$receiver.$method[$targs]($args)` using overloading resolution
     *  to find a matching overload of `$method` if necessary.
     *  This is useful when overloading resolution needs to be performed in a phase after typer.
     *  Note that this will not perform any kind of implicit search.
     *
     *  @param expectedType  An expected type of the application used to guide overloading resolution
     */
    def applyOverloaded(
        receiver: tpd.Tree, method: TermName, args: List[Tree], targs: List[Type],
        expectedType: Type)(using parentCtx: Context): tpd.Tree = {
      given ctx: Context = parentCtx.retractMode(Mode.ImplicitsEnabled)
      import dotty.tools.dotc.ast.tpd.TreeOps

      val typer = ctx.typer
      val proto = FunProto(args, expectedType)
      val denot = receiver.tpe.member(method)
      if !denot.exists then
        overload.println(i"members = ${receiver.tpe.decls}")
        report.error(em"no member $receiver . $method", receiver.srcPos)
      val selected =
        if (denot.isOverloaded) {
          def typeParamCount(tp: Type) = tp.widen match {
            case tp: PolyType => tp.paramInfos.length
            case _ => 0
          }
          val allAlts = denot.alternatives
            .map(denot => TermRef(receiver.tpe, denot.symbol))
            .filter(tr => typeParamCount(tr) == targs.length)
            .filter { _.widen match {
              case MethodTpe(_, _, x: MethodType) => !x.isImplicitMethod
              case _ => true
            }}
          val alternatives = ctx.typer.resolveOverloaded(allAlts, proto)
          assert(alternatives.size == 1,
            i"${if (alternatives.isEmpty) "no" else "multiple"} overloads available for " +
              i"$method on ${receiver.tpe.widenDealiasKeepAnnots} with targs: $targs%, %; args: $args%, %; expectedType: $expectedType." +
              i"all alternatives: ${allAlts.map(_.symbol.showDcl).mkString(", ")}\n" +
              i"matching alternatives: ${alternatives.map(_.symbol.showDcl).mkString(", ")}.") // this is parsed from bytecode tree. there's nothing user can do about it
            alternatives.head
        }
        else TermRef(receiver.tpe, denot.symbol)
      val fun = receiver.select(selected).appliedToTypes(targs)

      val apply = untpd.Apply(fun, args)
      typer.ApplyTo(apply, fun, selected, proto, expectedType)
    }


    def resolveConstructor(atp: Type, args: List[Tree])(using Context): tpd.Tree = {
      val targs = atp.argTypes
      withoutMode(Mode.PatternOrTypeBits) {
        applyOverloaded(tpd.New(atp.typeConstructor), nme.CONSTRUCTOR, args, targs, atp)
      }
    }
  }
}
