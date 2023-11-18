package dotty.tools
package dotc
package ast

import core.*
import Types.*, Contexts.*, Constants.*, Names.*, Flags.*
import dotty.tools.dotc.typer.ProtoTypes
import Symbols.*, StdNames.*, Trees.*
import util.{Property, SourceFile, NoSource}
import util.Spans.Span
import annotation.constructorOnly
import annotation.internal.sharable
import Decorators.*

object untpd extends Trees.Instance[Untyped] with UntypedTreeInfo {

  // ----- Tree cases that exist in untyped form only ------------------

  abstract class OpTree(implicit @constructorOnly src: SourceFile) extends Tree {
    def op: Ident
    override def isTerm: Boolean = op.isTerm
    override def isType: Boolean = op.isType
  }

  /** A typed subtree of an untyped tree needs to be wrapped in a TypedSplice
   *  @param owner               The current owner at the time the tree was defined
   *  @param isExtensionReceiver The splice was created from the receiver `e` in an extension
   *                             method call `e.f(...)`
   */
  abstract case class TypedSplice(splice: tpd.Tree)(val owner: Symbol, val isExtensionReceiver: Boolean)(implicit @constructorOnly src: SourceFile) extends ProxyTree {
    def forwardTo: tpd.Tree = splice
    override def toString =
      def ext = if isExtensionReceiver then ", isExtensionReceiver = true" else ""
      s"TypedSplice($splice$ext)"
  }

  object TypedSplice {
    def apply(tree: tpd.Tree, isExtensionReceiver: Boolean = false)(using Context): TypedSplice =
      new TypedSplice(tree)(ctx.owner, isExtensionReceiver) {}
  }

  /** mods object name impl */
  case class ModuleDef(name: TermName, impl: Template)(implicit @constructorOnly src: SourceFile)
    extends MemberDef {
    type ThisTree[+T <: Untyped] <: Trees.NameTree[T] & Trees.MemberDef[T] & ModuleDef
    def withName(name: Name)(using Context): ModuleDef = cpy.ModuleDef(this)(name.toTermName, impl)
  }

  /** An untyped template with a derives clause. Derived parents are added to the end
   *  of the `parents` list. `derivedCount` keeps track of how many there are.
   *  This representation was chosen because it balances two concerns:
   *   - maximize overlap between DerivingTemplate and Template for code streamlining
   *   - keep invariant that elements of untyped trees align with source positions
   */
  class DerivingTemplate(constr: DefDef, parentsOrDerived: List[Tree], self: ValDef, preBody: LazyTreeList, derivedCount: Int)(implicit @constructorOnly src: SourceFile)
  extends Template(constr, parentsOrDerived, self, preBody) {
    private val myParents = parentsOrDerived.dropRight(derivedCount)
    override def parents(using Context) = myParents
    override val derived = parentsOrDerived.takeRight(derivedCount)
  }

  case class ParsedTry(expr: Tree, handler: Tree, finalizer: Tree)(implicit @constructorOnly src: SourceFile) extends TermTree

  case class SymbolLit(str: String)(implicit @constructorOnly src: SourceFile) extends TermTree

  /** An interpolated string
   *  @param segments  a list of two element tickets consisting of string literal and argument tree,
   *                   possibly with a simple string literal as last element of the list
   */
  case class InterpolatedString(id: TermName, segments: List[Tree])(implicit @constructorOnly src: SourceFile)
    extends TermTree

  /** A function type or closure */
  case class Function(args: List[Tree], body: Tree)(implicit @constructorOnly src: SourceFile) extends Tree {
    override def isTerm: Boolean = body.isTerm
    override def isType: Boolean = body.isType
  }

  /** A function type or closure with `implicit` or `given` modifiers and information on which parameters are `erased` */
  class FunctionWithMods(args: List[Tree], body: Tree, val mods: Modifiers, val erasedParams: List[Boolean])(implicit @constructorOnly src: SourceFile)
    extends Function(args, body) {
      assert(args.length == erasedParams.length)

      def hasErasedParams = erasedParams.contains(true)
    }

  /** A polymorphic function type */
  case class PolyFunction(targs: List[Tree], body: Tree)(implicit @constructorOnly src: SourceFile) extends Tree {
    override def isTerm = body.isTerm
    override def isType = body.isType
  }

  /** A function created from a wildcard expression
   *  @param  placeholderParams  a list of definitions of synthetic parameters.
   *  @param  body               the function body where wildcards are replaced by
   *                             references to synthetic parameters.
   *  This is equivalent to Function, except that forms a special case for the overlapping
   *  positions tests.
   */
  class WildcardFunction(placeholderParams: List[ValDef], body: Tree)(implicit @constructorOnly src: SourceFile)
    extends Function(placeholderParams, body)

  case class InfixOp(left: Tree, op: Ident, right: Tree)(implicit @constructorOnly src: SourceFile) extends OpTree
  case class PostfixOp(od: Tree, op: Ident)(implicit @constructorOnly src: SourceFile) extends OpTree
  case class PrefixOp(op: Ident, od: Tree)(implicit @constructorOnly src: SourceFile) extends OpTree
  case class Parens(t: Tree)(implicit @constructorOnly src: SourceFile) extends ProxyTree {
    def forwardTo: Tree = t
  }
  case class Tuple(trees: List[Tree])(implicit @constructorOnly src: SourceFile) extends Tree {
    override def isTerm: Boolean = trees.isEmpty || trees.head.isTerm
    override def isType: Boolean = !isTerm
  }
  case class Throw(expr: Tree)(implicit @constructorOnly src: SourceFile) extends TermTree
  case class ForYield(enums: List[Tree], expr: Tree)(implicit @constructorOnly src: SourceFile) extends TermTree
  case class ForDo(enums: List[Tree], body: Tree)(implicit @constructorOnly src: SourceFile) extends TermTree
  case class GenFrom(pat: Tree, expr: Tree, checkMode: GenCheckMode)(implicit @constructorOnly src: SourceFile) extends Tree
  case class GenAlias(pat: Tree, expr: Tree)(implicit @constructorOnly src: SourceFile) extends Tree
  case class ContextBounds(bounds: TypeBoundsTree, cxBounds: List[Tree])(implicit @constructorOnly src: SourceFile) extends TypTree
  case class PatDef(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree)(implicit @constructorOnly src: SourceFile) extends DefTree
  case class ExtMethods(paramss: List[ParamClause], methods: List[Tree])(implicit @constructorOnly src: SourceFile) extends Tree
  case class MacroTree(expr: Tree)(implicit @constructorOnly src: SourceFile) extends Tree

  case class ImportSelector(imported: Ident, renamed: Tree = EmptyTree, bound: Tree = EmptyTree)(implicit @constructorOnly src: SourceFile) extends Tree {
    // TODO: Make bound a typed tree?

    /** It's a `given` selector */
    val isGiven: Boolean = imported.name.isEmpty

    /** It's a `given` or `_` selector */
    val isWildcard: Boolean = isGiven || imported.name == nme.WILDCARD

    /** The imported name, EmptyTermName if it's a given selector */
    val name: TermName = imported.name.asInstanceOf[TermName]

    /** The renamed part (which might be `_`), if present, or `name`, if missing */
    val rename: TermName = renamed match
      case Ident(rename: TermName) => rename
      case _ => name

    def isUnimport = rename == nme.WILDCARD
  }

  case class Number(digits: String, kind: NumberKind)(implicit @constructorOnly src: SourceFile) extends TermTree

  enum NumberKind {
    case Whole(radix: Int)
    case Decimal
    case Floating
  }

  /** {x1, ..., xN} T   (only relevant under captureChecking)
   *  Created when parsing function types so that capture set and result type
   *  is combined in a single node.
   */
  case class CapturesAndResult(refs: List[Tree], parent: Tree)(implicit @constructorOnly src: SourceFile) extends TypTree

  /** A type tree appearing somewhere in the untyped DefDef of a lambda, it will be typed using `tpFun`.
   *
   *  @param isResult  Is this the result type of the lambda? This is handled specially in `Namer#valOrDefDefSig`.
   *  @param tpFun     Compute the type of the type tree given the parameters of the lambda.
   *                   A lambda has at most one type parameter list followed by exactly one term parameter list.
   *
   *  Note: This is only used briefly in Typer and does not need the copy/transform/fold infrastructure.
   */
  case class InLambdaTypeTree(isResult: Boolean, tpFun: (List[TypeSymbol], List[TermSymbol]) => Type)(implicit @constructorOnly src: SourceFile) extends Tree

  @sharable object EmptyTypeIdent extends Ident(tpnme.EMPTY)(NoSource) with WithoutTypeOrPos[Untyped] {
    override def isEmpty: Boolean = true
  }

  def WildcardTypeBoundsTree()(using src: SourceFile): TypeBoundsTree = TypeBoundsTree(EmptyTree, EmptyTree, EmptyTree)
  object WildcardTypeBoundsTree:
    def unapply(tree: untpd.Tree): Boolean = tree match
      case TypeBoundsTree(EmptyTree, EmptyTree, _) => true
      case _ => false


  /** A block generated by the XML parser, only treated specially by
   *  `Positioned#checkPos` */
  class XMLBlock(stats: List[Tree], expr: Tree)(implicit @constructorOnly src: SourceFile) extends Block(stats, expr)

  /** An enum to control checking or filtering of patterns in GenFrom trees */
  enum GenCheckMode {
    case Ignore       // neither filter nor check since filtering was done before
    case Check        // check that pattern is irrefutable
    case CheckAndFilter // both check and filter (transitional period starting with 3.2)
    case FilterNow    // filter out non-matching elements if we are not in 3.2 or later
    case FilterAlways // filter out non-matching elements since pattern is prefixed by `case`
  }

  // ----- Modifiers -----------------------------------------------------
  /** Mod is intended to record syntactic information about modifiers, it's
    * NOT a replacement of FlagSet.
    *
    * For any query about semantic information, check `flags` instead.
    */
  sealed abstract class Mod(val flags: FlagSet)(implicit @constructorOnly src: SourceFile)
  extends Positioned

  object Mod {
    case class Private()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Private)

    case class Protected()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Protected)

    case class Var()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Mutable)

    case class Implicit()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Implicit)

    case class Given()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Given)

    case class Erased()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Erased)

    case class Final()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Final)

    case class Sealed()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Sealed)

    case class Opaque()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Opaque)

    case class Open()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Open)

    case class Override()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Override)

    case class Abstract()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Abstract)

    case class Lazy()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Lazy)

    case class Inline()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Inline)

    case class Transparent()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Transparent)

    case class Infix()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Infix)

    case class Tracked()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Tracked)

    /** Used under pureFunctions to mark impure function types `A => B` in `FunctionWithMods` */
    case class Impure()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Impure)
  }

  /** Modifiers and annotations for definitions
   *
   *  @param flags          The set flags
   *  @param privateWithin  If a private or protected has is followed by a
   *                        qualifier [q], the name q, "" as a typename otherwise.
   *  @param annotations    The annotations preceding the modifiers
   */
  case class Modifiers (
    flags: FlagSet = EmptyFlags,
    privateWithin: TypeName = tpnme.EMPTY,
    annotations: List[Tree] = Nil,
    mods: List[Mod] = Nil) {

    def is(flag: Flag): Boolean = flags.is(flag)
    def is(flag: Flag, butNot: FlagSet): Boolean = flags.is(flag, butNot = butNot)
    def isOneOf(fs: FlagSet): Boolean = flags.isOneOf(fs)
    def isOneOf(fs: FlagSet, butNot: FlagSet): Boolean = flags.isOneOf(fs, butNot = butNot)
    def isAllOf(fc: FlagSet): Boolean = flags.isAllOf(fc)

    def | (fs: FlagSet): Modifiers = withFlags(flags | fs)
    def & (fs: FlagSet): Modifiers = withFlags(flags & fs)
    def &~(fs: FlagSet): Modifiers = withFlags(flags &~ fs)

    def toTypeFlags: Modifiers = withFlags(flags.toTypeFlags)
    def toTermFlags: Modifiers = withFlags(flags.toTermFlags)

    def withFlags(flags: FlagSet): Modifiers =
      if (this.flags == flags) this
      else copy(flags = flags)

    def withoutFlags(flags: FlagSet): Modifiers =
      if (this.isOneOf(flags))
        Modifiers(this.flags &~ flags, this.privateWithin, this.annotations, this.mods.filterNot(_.flags.isOneOf(flags)))
      else this

    def withAddedMod(mod: Mod): Modifiers =
      if (mods.exists(_ eq mod)) this
      else withMods(mods :+ mod)

    private def compatible(flags1: FlagSet, flags2: FlagSet): Boolean =
      flags1.isEmpty || flags2.isEmpty
      || flags1.isTermFlags && flags2.isTermFlags
      || flags1.isTypeFlags && flags2.isTypeFlags

    /** Add `flags` to thos modifier set, checking that there are no type/term conflicts.
     *  If there are conflicts, issue an error and return the modifiers consisting of
     *  the added flags only. The reason to do it this way is that the added flags usually
     *  describe the core of a construct whereas the existing set are the modifiers
     *  given in the source.
     */
    def withAddedFlags(flags: FlagSet, span: Span)(using Context): Modifiers =
      if this.flags.isAllOf(flags) then this
      else if compatible(this.flags, flags) then this | flags
      else
        val what = if flags.isTermFlags then "values" else "types"
        report.error(em"${(flags & ModifierFlags).flagsString} $what cannot be ${this.flags.flagsString}", ctx.source.atSpan(span))
        Modifiers(flags)

    /** Modifiers with given list of Mods. It is checked that
     *  all modifiers are already accounted for in `flags` and `privateWithin`.
     */
    def withMods(ms: List[Mod]): Modifiers =
      if (mods eq ms) this
      else {
        if (ms.nonEmpty)
          for (m <- ms)
            assert(flags.isAllOf(m.flags)
                || m.isInstanceOf[Mod.Private] && !privateWithin.isEmpty
                || (m.isInstanceOf[Mod.Abstract] || m.isInstanceOf[Mod.Override]) && flags.is(AbsOverride),
                s"unaccounted modifier: $m in $this with flags ${flags.flagsString} when adding $ms")
        copy(mods = ms)
      }

    def withAddedAnnotation(annot: Tree): Modifiers =
      if (annotations.exists(_ eq annot)) this
      else withAnnotations(annotations :+ annot)

    def withAnnotations(annots: List[Tree]): Modifiers =
      if (annots eq annotations) this
      else copy(annotations = annots)

    def withPrivateWithin(pw: TypeName): Modifiers =
      if (pw.isEmpty) this
      else copy(privateWithin = pw)

    def hasFlags: Boolean = flags != EmptyFlags
    def hasAnnotations: Boolean = annotations.nonEmpty
    def hasPrivateWithin: Boolean = privateWithin != tpnme.EMPTY
    def hasMod(cls: Class[?]) = mods.exists(_.getClass == cls)

    private def isEnum = is(Enum, butNot = JavaDefined)

    def isEnumCase: Boolean = isEnum && is(Case)
    def isEnumClass: Boolean = isEnum && !is(Case)
  }

  @sharable val EmptyModifiers: Modifiers = Modifiers()

  // ----- TypeTrees that refer to other tree's symbols -------------------

  /** A type tree that gets its type from some other tree's symbol. Enters the
   *  type tree in the References attachment of the `from` tree as a side effect.
   */
  abstract class DerivedTypeTree(implicit @constructorOnly src: SourceFile) extends TypeTree {

    private var myWatched: Tree = EmptyTree

    /** The watched tree; used only for printing */
    def watched: Tree = myWatched

    /** Install the derived type tree as a dependency on `original` */
    def watching(original: DefTree): this.type = {
      myWatched = original
      val existing = original.attachmentOrElse(References, Nil)
      original.putAttachment(References, this :: existing)
      this
    }

    /** Install the derived type tree as a dependency on `sym` */
    def watching(sym: Symbol): this.type = withAttachment(OriginalSymbol, sym)

    /** A hook to ensure that all necessary symbols are completed so that
     *  OriginalSymbol attachments are propagated to this tree
     */
    def ensureCompletions(using Context): Unit = ()

    /** The method that computes the tree with the derived type */
    def derivedTree(originalSym: Symbol)(using Context): tpd.Tree
  }

  /** Property key containing TypeTrees whose type is computed
   *  from the symbol in this type. These type trees have marker trees
   *  TypeRefOfSym or InfoOfSym as their originals.
   */
  val References: Property.Key[List[DerivedTypeTree]] = Property.Key()

  /** Property key for TypeTrees marked with TypeRefOfSym or InfoOfSym
   *  which contains the symbol of the original tree from which this
   *  TypeTree is derived.
   */
  val OriginalSymbol: Property.Key[Symbol] = Property.Key()

  /** Property key for contextual Apply trees of the form `fn given arg` */
  val KindOfApply: Property.StickyKey[ApplyKind] = Property.StickyKey()

  // ------ Creation methods for untyped only -----------------

  def Ident(name: Name)(implicit src: SourceFile): Ident = new Ident(name)
  def SearchFailureIdent(name: Name, explanation: => String)(implicit src: SourceFile): SearchFailureIdent = new SearchFailureIdent(name, explanation)
  def Select(qualifier: Tree, name: Name)(implicit src: SourceFile): Select = new Select(qualifier, name)
  def SelectWithSig(qualifier: Tree, name: Name, sig: Signature)(implicit src: SourceFile): Select = new SelectWithSig(qualifier, name, sig)
  def This(qual: Ident)(implicit src: SourceFile): This = new This(qual)
  def Super(qual: Tree, mix: Ident)(implicit src: SourceFile): Super = new Super(qual, mix)
  def Apply(fun: Tree, args: List[Tree])(implicit src: SourceFile): Apply = new Apply(fun, args)
  def TypeApply(fun: Tree, args: List[Tree])(implicit src: SourceFile): TypeApply = new TypeApply(fun, args)
  def Literal(const: Constant)(implicit src: SourceFile): Literal = new Literal(const)
  def New(tpt: Tree)(implicit src: SourceFile): New = new New(tpt)
  def Typed(expr: Tree, tpt: Tree)(implicit src: SourceFile): Typed = new Typed(expr, tpt)
  def NamedArg(name: Name, arg: Tree)(implicit src: SourceFile): NamedArg = new NamedArg(name, arg)
  def Assign(lhs: Tree, rhs: Tree)(implicit src: SourceFile): Assign = new Assign(lhs, rhs)
  def Block(stats: List[Tree], expr: Tree)(implicit src: SourceFile): Block = new Block(stats, expr)
  def If(cond: Tree, thenp: Tree, elsep: Tree)(implicit src: SourceFile): If = new If(cond, thenp, elsep)
  def InlineIf(cond: Tree, thenp: Tree, elsep: Tree)(implicit src: SourceFile): If = new InlineIf(cond, thenp, elsep)
  def Closure(env: List[Tree], meth: Tree, tpt: Tree)(implicit src: SourceFile): Closure = new Closure(env, meth, tpt)
  def Match(selector: Tree, cases: List[CaseDef])(implicit src: SourceFile): Match = new Match(selector, cases)
  def InlineMatch(selector: Tree, cases: List[CaseDef])(implicit src: SourceFile): Match = new InlineMatch(selector, cases)
  def CaseDef(pat: Tree, guard: Tree, body: Tree)(implicit src: SourceFile): CaseDef = new CaseDef(pat, guard, body)
  def Labeled(bind: Bind, expr: Tree)(implicit src: SourceFile): Labeled = new Labeled(bind, expr)
  def Return(expr: Tree, from: Tree)(implicit src: SourceFile): Return = new Return(expr, from)
  def WhileDo(cond: Tree, body: Tree)(implicit src: SourceFile): WhileDo = new WhileDo(cond, body)
  def Try(expr: Tree, cases: List[CaseDef], finalizer: Tree)(implicit src: SourceFile): Try = new Try(expr, cases, finalizer)
  def SeqLiteral(elems: List[Tree], elemtpt: Tree)(implicit src: SourceFile): SeqLiteral = new SeqLiteral(elems, elemtpt)
  def JavaSeqLiteral(elems: List[Tree], elemtpt: Tree)(implicit src: SourceFile): JavaSeqLiteral = new JavaSeqLiteral(elems, elemtpt)
  def Inlined(call: tpd.Tree, bindings: List[MemberDef], expansion: Tree)(implicit src: SourceFile): Inlined = new Inlined(call, bindings, expansion)
  def Quote(body: Tree, tags: List[Tree])(implicit src: SourceFile): Quote = new Quote(body, tags)
  def Splice(expr: Tree)(implicit src: SourceFile): Splice = new Splice(expr)
  def QuotePattern(bindings: List[Tree], body: Tree, quotes: Tree)(implicit src: SourceFile): QuotePattern = new QuotePattern(bindings, body, quotes)
  def SplicePattern(body: Tree, args: List[Tree])(implicit src: SourceFile): SplicePattern = new SplicePattern(body, args)
  def TypeTree()(implicit src: SourceFile): TypeTree = new TypeTree()
  def InferredTypeTree()(implicit src: SourceFile): TypeTree = new InferredTypeTree()
  def SingletonTypeTree(ref: Tree)(implicit src: SourceFile): SingletonTypeTree = new SingletonTypeTree(ref)
  def RefinedTypeTree(tpt: Tree, refinements: List[Tree])(implicit src: SourceFile): RefinedTypeTree = new RefinedTypeTree(tpt, refinements)
  def AppliedTypeTree(tpt: Tree, args: List[Tree])(implicit src: SourceFile): AppliedTypeTree = new AppliedTypeTree(tpt, args)
  def LambdaTypeTree(tparams: List[TypeDef], body: Tree)(implicit src: SourceFile): LambdaTypeTree = new LambdaTypeTree(tparams, body)
  def TermLambdaTypeTree(params: List[ValDef], body: Tree)(implicit src: SourceFile): TermLambdaTypeTree = new TermLambdaTypeTree(params, body)
  def MatchTypeTree(bound: Tree, selector: Tree, cases: List[CaseDef])(implicit src: SourceFile): MatchTypeTree = new MatchTypeTree(bound, selector, cases)
  def ByNameTypeTree(result: Tree)(implicit src: SourceFile): ByNameTypeTree = new ByNameTypeTree(result)
  def TypeBoundsTree(lo: Tree, hi: Tree, alias: Tree = EmptyTree)(implicit src: SourceFile): TypeBoundsTree = new TypeBoundsTree(lo, hi, alias)
  def Bind(name: Name, body: Tree)(implicit src: SourceFile): Bind = new Bind(name, body)
  def Alternative(trees: List[Tree])(implicit src: SourceFile): Alternative = new Alternative(trees)
  def UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree])(implicit src: SourceFile): UnApply = new UnApply(fun, implicits, patterns)
  def ValDef(name: TermName, tpt: Tree, rhs: LazyTree)(implicit src: SourceFile): ValDef = new ValDef(name, tpt, rhs)
  def DefDef(name: TermName, paramss: List[ParamClause], tpt: Tree, rhs: LazyTree)(implicit src: SourceFile): DefDef = new DefDef(name, paramss, tpt, rhs)
  def TypeDef(name: TypeName, rhs: Tree)(implicit src: SourceFile): TypeDef = new TypeDef(name, rhs)
  def Template(constr: DefDef, parents: List[Tree], derived: List[Tree], self: ValDef, body: LazyTreeList)(implicit src: SourceFile): Template =
    if (derived.isEmpty) new Template(constr, parents, self, body)
    else new DerivingTemplate(constr, parents ++ derived, self, body, derived.length)
  def Template(constr: DefDef, parents: LazyTreeList, self: ValDef, body: LazyTreeList)(implicit src: SourceFile): Template =
    new Template(constr, parents, self, body)
  def Import(expr: Tree, selectors: List[ImportSelector])(implicit src: SourceFile): Import = new Import(expr, selectors)
  def Export(expr: Tree, selectors: List[ImportSelector])(implicit src: SourceFile): Export = new Export(expr, selectors)
  def PackageDef(pid: RefTree, stats: List[Tree])(implicit src: SourceFile): PackageDef = new PackageDef(pid, stats)
  def Annotated(arg: Tree, annot: Tree)(implicit src: SourceFile): Annotated = new Annotated(arg, annot)
  def Hole(isTerm: Boolean, idx: Int, args: List[Tree], content: Tree)(implicit src: SourceFile): Hole = new Hole(isTerm, idx, args, content)

  // ------ Additional creation methods for untyped only -----------------

  /**     new T(args1)...(args_n)
   *  ==>
   *      new T.<init>[Ts](args1)...(args_n)
   *
   *  where `Ts` are the class type arguments of `T` or its class type alias.
   *  Note: we also keep any type arguments as parts of `T`. This is necessary to allow
   *  navigation into these arguments from the IDE, and to do the right thing in
   *  PrepareInlineable.
   */
  def New(tpt: Tree, argss: List[List[Tree]])(using Context): Tree =
    ensureApplied(argss.foldLeft(makeNew(tpt))(Apply(_, _)))

  /** A new expression with constrictor and possibly type arguments. See
   *  `New(tpt, argss)` for details.
   */
  def makeNew(tpt: Tree)(using Context): Tree = {
    val (tycon, targs) = tpt match {
      case AppliedTypeTree(tycon, targs) =>
        (tycon, targs)
      case TypedSplice(tpt1: tpd.Tree) =>
        val argTypes = tpt1.tpe.dealias.argTypesLo
        def wrap(tpe: Type) = TypeTree(tpe).withSpan(tpt.span)
        (tpt, argTypes.map(wrap))
      case _ =>
        (tpt, Nil)
    }
    val nu: Tree = Select(New(tycon), nme.CONSTRUCTOR)
    if (targs.nonEmpty) TypeApply(nu, targs) else nu
  }

  def Block(stat: Tree, expr: Tree)(implicit src: SourceFile): Block =
    Block(stat :: Nil, expr)

  def Apply(fn: Tree, arg: Tree)(implicit src: SourceFile): Apply =
    Apply(fn, arg :: Nil)

  def ensureApplied(tpt: Tree)(implicit src: SourceFile): Tree = tpt match {
    case _: Apply => tpt
    case _ => Apply(tpt, Nil)
  }

  def AppliedTypeTree(tpt: Tree, arg: Tree)(implicit src: SourceFile): AppliedTypeTree =
    AppliedTypeTree(tpt, arg :: Nil)

  def TypeTree(tpe: Type)(using Context): TypedSplice =
    TypedSplice(TypeTree().withTypeUnchecked(tpe))

  def InferredTypeTree(tpe: Type)(using Context): TypedSplice =
    TypedSplice(new InferredTypeTree().withTypeUnchecked(tpe))

  def unitLiteral(implicit src: SourceFile): Literal = Literal(Constant(())).withAttachment(SyntheticUnit, ())

  def ref(tp: NamedType)(using Context): Tree =
    TypedSplice(tpd.ref(tp))

  def ref(sym: Symbol)(using Context): Tree =
    TypedSplice(tpd.ref(sym))

  def rawRef(tp: NamedType)(using Context): Tree =
    if tp.typeParams.isEmpty then ref(tp)
    else AppliedTypeTree(ref(tp), tp.typeParams.map(_ => WildcardTypeBoundsTree()))

  def rootDot(name: Name)(implicit src: SourceFile): Select = Select(Ident(nme.ROOTPKG), name)
  def scalaDot(name: Name)(implicit src: SourceFile): Select = Select(rootDot(nme.scala), name)
  def scalaAnnotationDot(name: Name)(using SourceFile): Select = Select(scalaDot(nme.annotation), name)
  def scalaRuntimeDot(name: Name)(using SourceFile): Select = Select(scalaDot(nme.runtime), name)
  def scalaUnit(implicit src: SourceFile): Select = scalaDot(tpnme.Unit)
  def scalaAny(implicit src: SourceFile): Select = scalaDot(tpnme.Any)

  def captureRoot(using Context): Select =
    Select(scalaDot(nme.caps), nme.CAPTURE_ROOT)

  def captureRootIn(using Context): Select =
    Select(scalaDot(nme.caps), nme.capIn)

  def makeRetaining(parent: Tree, refs: List[Tree], annotName: TypeName)(using Context): Annotated =
    Annotated(parent, New(scalaAnnotationDot(annotName), List(refs)))

  def makeConstructor(tparams: List[TypeDef], vparamss: List[List[ValDef]], rhs: Tree = EmptyTree)(using Context): DefDef =
    DefDef(nme.CONSTRUCTOR, joinParams(tparams, vparamss), TypeTree(), rhs)

  def emptyConstructor(using Context): DefDef =
    makeConstructor(Nil, Nil)

  def makeSelfDef(name: TermName, tpt: Tree)(using Context): ValDef =
    ValDef(name, tpt, EmptyTree).withFlags(PrivateLocal)

  def makeTupleOrParens(ts: List[Tree])(using Context): Tree = ts match {
    case t :: Nil => Parens(t)
    case _ => Tuple(ts)
  }

  def makeTuple(ts: List[Tree])(using Context): Tree = ts match {
    case t :: Nil => t
    case _ => Tuple(ts)
  }

  def makeAndType(left: Tree, right: Tree)(using Context): AppliedTypeTree =
    AppliedTypeTree(ref(defn.andType.typeRef), left :: right :: Nil)

  def makeParameter(pname: TermName, tpe: Tree, mods: Modifiers, isBackquoted: Boolean = false)(using Context): ValDef = {
    val vdef = ValDef(pname, tpe, EmptyTree)
    if (isBackquoted) vdef.pushAttachment(Backquoted, ())
    vdef.withMods(mods | Param)
  }

  def makeSyntheticParameter(n: Int = 1, tpt: Tree | Null = null, flags: FlagSet = SyntheticTermParam)(using Context): ValDef =
    ValDef(nme.syntheticParamName(n), if (tpt == null) TypeTree() else tpt, EmptyTree)
      .withFlags(flags)

  def isInto(t: Tree)(using Context): Boolean = t match
    case PrefixOp(Ident(tpnme.into), _) => true
    case Function(_, res) => isInto(res)
    case Parens(t) => isInto(t)
    case _ => false

  def lambdaAbstract(params: List[ValDef] | List[TypeDef], tpt: Tree)(using Context): Tree =
    params match
      case Nil               => tpt
      case (vd: ValDef) :: _ => TermLambdaTypeTree(params.asInstanceOf[List[ValDef]], tpt)
      case _                 => LambdaTypeTree(params.asInstanceOf[List[TypeDef]], tpt)

  def lambdaAbstractAll(paramss: List[List[ValDef] | List[TypeDef]], tpt: Tree)(using Context): Tree =
    paramss.foldRight(tpt)(lambdaAbstract)

  /** A reference to given definition. If definition is a repeated
   *  parameter, the reference will be a repeated argument.
   */
  def refOfDef(tree: MemberDef)(using Context): Tree = tree match {
    case ValDef(_, PostfixOp(_, Ident(tpnme.raw.STAR)), _) => repeated(Ident(tree.name))
    case _ => Ident(tree.name)
  }

  /** A repeated argument such as `arg: _*` */
  def repeated(arg: Tree)(using Context): Typed = Typed(arg, Ident(tpnme.WILDCARD_STAR))


// --------- Copier/Transformer/Accumulator classes for untyped trees -----

  def localCtx(tree: Tree)(using Context): Context = ctx

  override val cpy: UntypedTreeCopier = UntypedTreeCopier()

  class UntypedTreeCopier extends TreeCopier {

    def postProcess(tree: Tree, copied: Tree): copied.ThisTree[Untyped] =
      copied.asInstanceOf[copied.ThisTree[Untyped]]

    def postProcess(tree: Tree, copied: MemberDef): copied.ThisTree[Untyped] = {
      tree match {
        case tree: MemberDef => copied.withMods(tree.rawMods)
        case _ => copied
      }
    }.asInstanceOf[copied.ThisTree[Untyped]]

    def ModuleDef(tree: Tree)(name: TermName, impl: Template)(using Context): ModuleDef = tree match {
      case tree: ModuleDef if (name eq tree.name) && (impl eq tree.impl) => tree
      case _ => finalize(tree, untpd.ModuleDef(name, impl)(tree.source))
    }
    def ParsedTry(tree: Tree)(expr: Tree, handler: Tree, finalizer: Tree)(using Context): TermTree = tree match {
      case tree: ParsedTry if (expr eq tree.expr) && (handler eq tree.handler) && (finalizer eq tree.finalizer) => tree
      case _ => finalize(tree, untpd.ParsedTry(expr, handler, finalizer)(tree.source))
    }
    def SymbolLit(tree: Tree)(str: String)(using Context): TermTree = tree match {
      case tree: SymbolLit if str == tree.str => tree
      case _ => finalize(tree, untpd.SymbolLit(str)(tree.source))
    }
    def InterpolatedString(tree: Tree)(id: TermName, segments: List[Tree])(using Context): TermTree = tree match {
      case tree: InterpolatedString if (id eq tree.id) && (segments eq tree.segments) => tree
      case _ => finalize(tree, untpd.InterpolatedString(id, segments)(tree.source))
    }
    def Function(tree: Tree)(args: List[Tree], body: Tree)(using Context): Tree = tree match {
      case tree: Function if (args eq tree.args) && (body eq tree.body) => tree
      case _ =>
        val tree1 = tree match
          case tree: FunctionWithMods => untpd.FunctionWithMods(args, body, tree.mods, tree.erasedParams)(using tree.source)
          case _ => untpd.Function(args, body)(using tree.source)
        finalize(tree, tree1)
    }
    def PolyFunction(tree: Tree)(targs: List[Tree], body: Tree)(using Context): Tree = tree match {
      case tree: PolyFunction if (targs eq tree.targs) && (body eq tree.body) => tree
      case _ => finalize(tree, untpd.PolyFunction(targs, body)(tree.source))
    }
    def InfixOp(tree: Tree)(left: Tree, op: Ident, right: Tree)(using Context): Tree = tree match {
      case tree: InfixOp if (left eq tree.left) && (op eq tree.op) && (right eq tree.right) => tree
      case _ => finalize(tree, untpd.InfixOp(left, op, right)(tree.source))
    }
    def PostfixOp(tree: Tree)(od: Tree, op: Ident)(using Context): Tree = tree match {
      case tree: PostfixOp if (od eq tree.od) && (op eq tree.op) => tree
      case _ => finalize(tree, untpd.PostfixOp(od, op)(tree.source))
    }
    def PrefixOp(tree: Tree)(op: Ident, od: Tree)(using Context): Tree = tree match {
      case tree: PrefixOp if (op eq tree.op) && (od eq tree.od) => tree
      case _ => finalize(tree, untpd.PrefixOp(op, od)(tree.source))
    }
    def Parens(tree: Tree)(t: Tree)(using Context): ProxyTree = tree match {
      case tree: Parens if t eq tree.t => tree
      case _ => finalize(tree, untpd.Parens(t)(tree.source))
    }
    def Tuple(tree: Tree)(trees: List[Tree])(using Context): Tree = tree match {
      case tree: Tuple if trees eq tree.trees => tree
      case _ => finalize(tree, untpd.Tuple(trees)(tree.source))
    }
    def Throw(tree: Tree)(expr: Tree)(using Context): TermTree = tree match {
      case tree: Throw if expr eq tree.expr => tree
      case _ => finalize(tree, untpd.Throw(expr)(tree.source))
    }
    def ForYield(tree: Tree)(enums: List[Tree], expr: Tree)(using Context): TermTree = tree match {
      case tree: ForYield if (enums eq tree.enums) && (expr eq tree.expr) => tree
      case _ => finalize(tree, untpd.ForYield(enums, expr)(tree.source))
    }
    def ForDo(tree: Tree)(enums: List[Tree], body: Tree)(using Context): TermTree = tree match {
      case tree: ForDo if (enums eq tree.enums) && (body eq tree.body) => tree
      case _ => finalize(tree, untpd.ForDo(enums, body)(tree.source))
    }
    def GenFrom(tree: Tree)(pat: Tree, expr: Tree, checkMode: GenCheckMode)(using Context): Tree = tree match {
      case tree: GenFrom if (pat eq tree.pat) && (expr eq tree.expr) && (checkMode == tree.checkMode) => tree
      case _ => finalize(tree, untpd.GenFrom(pat, expr, checkMode)(tree.source))
    }
    def GenAlias(tree: Tree)(pat: Tree, expr: Tree)(using Context): Tree = tree match {
      case tree: GenAlias if (pat eq tree.pat) && (expr eq tree.expr) => tree
      case _ => finalize(tree, untpd.GenAlias(pat, expr)(tree.source))
    }
    def ContextBounds(tree: Tree)(bounds: TypeBoundsTree, cxBounds: List[Tree])(using Context): TypTree = tree match {
      case tree: ContextBounds if (bounds eq tree.bounds) && (cxBounds eq tree.cxBounds) => tree
      case _ => finalize(tree, untpd.ContextBounds(bounds, cxBounds)(tree.source))
    }
    def PatDef(tree: Tree)(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree)(using Context): Tree = tree match {
      case tree: PatDef if (mods eq tree.mods) && (pats eq tree.pats) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
      case _ => finalize(tree, untpd.PatDef(mods, pats, tpt, rhs)(tree.source))
    }
    def ExtMethods(tree: Tree)(paramss: List[ParamClause], methods: List[Tree])(using Context): Tree = tree match
      case tree: ExtMethods if (paramss eq tree.paramss) && (methods == tree.methods) => tree
      case _ => finalize(tree, untpd.ExtMethods(paramss, methods)(tree.source))
    def ImportSelector(tree: Tree)(imported: Ident, renamed: Tree, bound: Tree)(using Context): Tree = tree match {
      case tree: ImportSelector if (imported eq tree.imported) && (renamed eq tree.renamed) && (bound eq tree.bound) => tree
      case _ => finalize(tree, untpd.ImportSelector(imported, renamed, bound)(tree.source))
    }
    def Number(tree: Tree)(digits: String, kind: NumberKind)(using Context): Tree = tree match {
      case tree: Number if (digits == tree.digits) && (kind == tree.kind) => tree
      case _ => finalize(tree, untpd.Number(digits, kind))
    }
    def CapturesAndResult(tree: Tree)(refs: List[Tree], parent: Tree)(using Context): Tree = tree match
      case tree: CapturesAndResult if (refs eq tree.refs) && (parent eq tree.parent) => tree
      case _ => finalize(tree, untpd.CapturesAndResult(refs, parent))

    def TypedSplice(tree: Tree)(splice: tpd.Tree)(using Context): ProxyTree = tree match {
      case tree: TypedSplice if splice `eq` tree.splice => tree
      case _ => finalize(tree, untpd.TypedSplice(splice)(using ctx))
    }
    def MacroTree(tree: Tree)(expr: Tree)(using Context): Tree = tree match {
      case tree: MacroTree if expr `eq` tree.expr => tree
      case _ => finalize(tree, untpd.MacroTree(expr)(tree.source))
    }
  }

  abstract class UntypedTreeMap(cpy: UntypedTreeCopier = untpd.cpy) extends TreeMap(cpy) {
    override def transformMoreCases(tree: Tree)(using Context): Tree = tree match {
      case ModuleDef(name, impl) =>
        cpy.ModuleDef(tree)(name, transformSub(impl))
      case tree: DerivingTemplate =>
        cpy.Template(tree)(transformSub(tree.constr), transform(tree.parents),
          transform(tree.derived), transformSub(tree.self), transformStats(tree.body, tree.symbol))
      case ParsedTry(expr, handler, finalizer) =>
        cpy.ParsedTry(tree)(transform(expr), transform(handler), transform(finalizer))
      case SymbolLit(str) =>
        cpy.SymbolLit(tree)(str)
      case InterpolatedString(id, segments) =>
        cpy.InterpolatedString(tree)(id, segments.mapConserve(transform))
      case Function(args, body) =>
        cpy.Function(tree)(transform(args), transform(body))
      case PolyFunction(targs, body) =>
        cpy.PolyFunction(tree)(transform(targs), transform(body))
      case InfixOp(left, op, right) =>
        cpy.InfixOp(tree)(transform(left), op, transform(right))
      case PostfixOp(od, op) =>
        cpy.PostfixOp(tree)(transform(od), op)
      case PrefixOp(op, od) =>
        cpy.PrefixOp(tree)(op, transform(od))
      case Parens(t) =>
        cpy.Parens(tree)(transform(t))
      case Tuple(trees) =>
        cpy.Tuple(tree)(transform(trees))
      case Throw(expr) =>
        cpy.Throw(tree)(transform(expr))
      case ForYield(enums, expr) =>
        cpy.ForYield(tree)(transform(enums), transform(expr))
      case ForDo(enums, body) =>
        cpy.ForDo(tree)(transform(enums), transform(body))
      case GenFrom(pat, expr, checkMode) =>
        cpy.GenFrom(tree)(transform(pat), transform(expr), checkMode)
      case GenAlias(pat, expr) =>
        cpy.GenAlias(tree)(transform(pat), transform(expr))
      case ContextBounds(bounds, cxBounds) =>
        cpy.ContextBounds(tree)(transformSub(bounds), transform(cxBounds))
      case PatDef(mods, pats, tpt, rhs) =>
        cpy.PatDef(tree)(mods, transform(pats), transform(tpt), transform(rhs))
      case ExtMethods(paramss, methods) =>
        cpy.ExtMethods(tree)(transformParamss(paramss), transformSub(methods))
      case ImportSelector(imported, renamed, bound) =>
        cpy.ImportSelector(tree)(transformSub(imported), transform(renamed), transform(bound))
      case Number(_, _) | TypedSplice(_) =>
        tree
      case MacroTree(expr) =>
        cpy.MacroTree(tree)(transform(expr))
      case CapturesAndResult(refs, parent) =>
        cpy.CapturesAndResult(tree)(transform(refs), transform(parent))
      case _ =>
        super.transformMoreCases(tree)
    }
  }

  abstract class UntypedTreeAccumulator[X] extends TreeAccumulator[X] { self =>
    override def foldMoreCases(x: X, tree: Tree)(using Context): X = tree match {
      case ModuleDef(name, impl) =>
        this(x, impl)
      case tree: DerivingTemplate =>
        this(this(this(this(this(x, tree.constr), tree.parents), tree.derived), tree.self), tree.body)
      case ParsedTry(expr, handler, finalizer) =>
        this(this(this(x, expr), handler), finalizer)
      case SymbolLit(str) =>
        x
      case InterpolatedString(id, segments) =>
        this(x, segments)
      case Function(args, body) =>
        this(this(x, args), body)
      case PolyFunction(targs, body) =>
        this(this(x, targs), body)
      case InfixOp(left, op, right) =>
        this(this(this(x, left), op), right)
      case PostfixOp(od, op) =>
        this(this(x, od), op)
      case PrefixOp(op, od) =>
        this(this(x, op), od)
      case Parens(t) =>
        this(x, t)
      case Tuple(trees) =>
        this(x, trees)
      case Throw(expr) =>
        this(x, expr)
      case ForYield(enums, expr) =>
        this(this(x, enums), expr)
      case ForDo(enums, body) =>
        this(this(x, enums), body)
      case GenFrom(pat, expr, _) =>
        this(this(x, pat), expr)
      case GenAlias(pat, expr) =>
        this(this(x, pat), expr)
      case ContextBounds(bounds, cxBounds) =>
        this(this(x, bounds), cxBounds)
      case PatDef(mods, pats, tpt, rhs) =>
        this(this(this(x, pats), tpt), rhs)
      case ExtMethods(paramss, methods) =>
        this(paramss.foldLeft(x)(apply), methods)
      case ImportSelector(imported, renamed, bound) =>
        this(this(this(x, imported), renamed), bound)
      case Number(_, _) =>
        x
      case TypedSplice(splice) =>
        this(x, splice)
      case MacroTree(expr) =>
        this(x, expr)
      case CapturesAndResult(refs, parent) =>
        this(this(x, refs), parent)
      case _ =>
        super.foldMoreCases(x, tree)
    }
  }

  abstract class UntypedTreeTraverser extends UntypedTreeAccumulator[Unit] {
    def traverse(tree: Tree)(using Context): Unit
    def apply(x: Unit, tree: Tree)(using Context): Unit = traverse(tree)
    protected def traverseChildren(tree: Tree)(using Context): Unit = foldOver((), tree)
  }

  /** Fold `f` over all tree nodes, in depth-first, prefix order */
  class UntypedDeepFolder[X](f: (X, Tree) => X) extends UntypedTreeAccumulator[X] {
    def apply(x: X, tree: Tree)(using Context): X = foldOver(f(x, tree), tree)
  }

  /** Is there a subtree of this tree that satisfies predicate `p`? */
  extension (tree: Tree) def existsSubTree(p: Tree => Boolean)(using Context): Boolean = {
    val acc = new UntypedTreeAccumulator[Boolean] {
      def apply(x: Boolean, t: Tree)(using Context) = x || p(t) || foldOver(x, t)
    }
    acc(false, tree)
  }

  protected def FunProto(args: List[Tree], resType: Type)(using Context) =
    ProtoTypes.FunProto(args, resType)(ctx.typer, ApplyKind.Regular)
}
