package dotty.tools
package dotc
package core

import Periods.*
import Names.*
import Scopes.*
import Flags.*
import Decorators.*
import Contexts.*
import Phases.*
import SymDenotations.*
import Denotations.*
import printing.Texts.*
import printing.Printer
import Types.*
import util.Spans.*
import DenotTransformers.*
import StdNames.*
import NameOps.*
import NameKinds.LazyImplicitName
import ast.*, tpd.*
import Constants.Constant
import Variances.Variance
import reporting.Message
import collection.mutable
import io.AbstractFile
import util.{SourceFile, NoSource, Property, SourcePosition, SrcPos, EqHashMap}
import scala.annotation.internal.sharable
import config.Printers.typr
import dotty.tools.dotc.classpath.FileUtils.isScalaBinary

import scala.compiletime.uninitialized
import dotty.tools.tasty.TastyVersion

object Symbols extends SymUtils {

  implicit def eqSymbol: CanEqual[Symbol, Symbol] = CanEqual.derived

  /** Tree attachment containing the identifiers in a tree as a sorted array */
  val Ids: Property.Key[Array[String]] = new Property.Key

  /** A Symbol represents a Scala definition/declaration or a package.
   *  @param myCoord The coordinates of the symbol (a position or an index)
   *  @param id     A unique identifier of the symbol (unique per ContextBase)
   */
  class Symbol private[Symbols] (private var myCoord: Coord, val id: Int, val nestingLevel: Int)
    extends Designator, ParamInfo, SrcPos, printing.Showable {

    type ThisName <: Name

    //assert(id != 723)

    def coord: Coord = myCoord

    /** Set the coordinate of this class, this is only useful when the coordinate is
     *  not known at symbol creation. This is the case for root symbols
     *  unpickled from TASTY.
     *
     *  @pre coord == NoCoord
     */
    private[core] def coord_=(c: Coord): Unit = {
      // assert(myCoord == NoCoord)
        // This assertion fails for CommentPickling test.
        // TODO: figure out what's wrong in the setup of CommentPicklingTest and re-enable assertion.
      myCoord = c
    }

    private var myDefTree: Tree | Null = null

    /** The tree defining the symbol at pickler time, EmptyTree if none was retained */
    def defTree: Tree =
      if (myDefTree == null) tpd.EmptyTree else myDefTree.nn

    /** Set defining tree if this symbol retains its definition tree */
    def defTree_=(tree: Tree)(using Context): Unit =
      if (retainsDefTree) myDefTree = tree

    /** Does this symbol retain its definition tree?
     *  A good policy for this needs to balance costs and benefits, where
     *  costs are mainly memory leaks, in particular across runs.
     */
    def retainsDefTree(using Context): Boolean =
      ctx.settings.YretainTrees.value ||
      denot.owner.isTerm ||                // no risk of leaking memory after a run for these
      denot.isOneOf(InlineOrProxy) ||      // need to keep inline info
      ctx.settings.YcheckInit.value ||     // initialization check
      ctx.settings.YcheckInitGlobal.value

    /** The last denotation of this symbol */
    private var lastDenot: SymDenotation = uninitialized
    private var checkedPeriod: Period = Nowhere

    private[core] def invalidateDenotCache(): Unit = { checkedPeriod = Nowhere }

    /** Set the denotation of this symbol
     *  `denot` should always be initialized when a new Symbol is created.
     */
    private[core] def denot_=(d: SymDenotation): Unit = {
      util.Stats.record("Symbol.denot_=")
      lastDenot = d
      checkedPeriod = Nowhere
    }

    /** The current denotation of this symbol */
    final def denot(using Context): SymDenotation = {
      util.Stats.record("Symbol.denot")
      if checkedPeriod.code == ctx.period.code then lastDenot
      else computeDenot(lastDenot)
    }

    private def computeDenot(lastd: SymDenotation)(using Context): SymDenotation = {
      // Written that way so that it comes in at 32 bytes and is therefore inlineable for
      // the JIT (reputedly, cutoff is at 35 bytes)
      util.Stats.record("Symbol.computeDenot")
      val now = ctx.period
      checkedPeriod = now
      if lastd.validFor.contains(now) then lastd else recomputeDenot(lastd)
    }

    /** Overridden in NoSymbol */
    protected def recomputeDenot(lastd: SymDenotation)(using Context): SymDenotation = {
      util.Stats.record("Symbol.recomputeDenot")
      val newd = lastd.current.asInstanceOf[SymDenotation]
      if newd.exists || lastd.initial.validFor.firstPhaseId <= ctx.phaseId then
        lastDenot = newd
      else
        // We are trying to bring forward a symbol that is defined only at a later phase
        // (typically, a nested Java class, invisible before erasure).
        // In that case, keep lastDenot as it was and set the checked period to lastDenot's
        // previous validity, which means we will try another bring forward when the symbol
        // is referenced at a later phase. Otherwise we'd get stuck on NoDenotation here.
        // See #15562 and test i15562b in ReplCompilerTests
        checkedPeriod = lastd.validFor
      newd
    }

    /** The original denotation of this symbol, without forcing anything */
    final def originDenotation: SymDenotation =
      lastDenot.initial

    /** The last known denotation of this symbol, without going through `current` */
    final def lastKnownDenotation: SymDenotation =
      lastDenot

    private[dotc] def defRunId: RunId =
      lastDenot.validFor.runId

    private inline def associatedFileMatches(inline filter: AbstractFile => Boolean)(using Context): Boolean =
      try
        val file = associatedFile
        file != null && filter(file)
      catch case ex: StaleSymbol =>
        // can happen for constructor proxy companions. Test case is pos-macros/i9484.
        false

    /** Does this symbol come from a currently compiled source file? */
    final def isDefinedInCurrentRun(using Context): Boolean =
      span.exists && defRunId == ctx.runId && associatedFileMatches(ctx.run.nn.files.contains)

    /** Is this symbol valid in the current run and has an associated file that is
      * not a binary file. e.g. This will return true for
      * symbols defined by the user in a prior run of the REPL, that are still valid.
      */
    final def isDefinedInSource(using Context): Boolean =
      span.exists && isValidInCurrentRun && associatedFileMatches(!_.isScalaBinary)

    /** Is symbol valid in current run? */
    final def isValidInCurrentRun(using Context): Boolean =
      (lastDenot.validFor.runId == ctx.runId || stillValid(lastDenot)) &&
      (lastDenot.symbol eq this)
        // the last condition is needed because under ctx.staleOK overwritten
        // members keep denotations pointing to the new symbol, so the validity
        // periods check out OK. But once a package member is overridden it is not longer
        // valid. If the option would be removed, the check would be no longer needed.

    final def isTerm(using Context): Boolean =
      (if (defRunId == ctx.runId) lastDenot else denot).isTerm
    final def isType(using Context): Boolean =
      (if (defRunId == ctx.runId) lastDenot else denot).isType
    final def asTerm(using Context): TermSymbol = {
      assert(isTerm, s"asTerm called on not-a-Term $this" );
      asInstanceOf[TermSymbol]
    }
    final def asType(using Context): TypeSymbol = {
      assert(isType, s"asType called on not-a-Type $this");
      asInstanceOf[TypeSymbol]
    }

    final def isClass: Boolean = isInstanceOf[ClassSymbol]
    final def asClass: ClassSymbol = asInstanceOf[ClassSymbol]

    /** Test whether symbol is private. This
     *  conservatively returns `false` if symbol does not yet have a denotation, or denotation
     *  is a class that is not yet read.
     */
    final def isPrivate(using Context): Boolean =
      lastDenot.flagsUNSAFE.is(Private)

    /** Is the symbol a pattern bound symbol?
     */
    final def isPatternBound(using Context): Boolean =
      !isClass && this.is(Case, butNot = Enum | Module)

    /** The symbol's signature if it is completed or a method, NotAMethod otherwise. */
    final def signature(using Context): Signature =
      if lastDenot.uncheckedNN.isCompleted || lastDenot.uncheckedNN.is(Method) then
        denot.signature
      else
        Signature.NotAMethod

    /** Special cased here, because it may be used on naked symbols in substituters */
    final def isStatic(using Context): Boolean =
      lastDenot.initial.isStatic

    /** This symbol entered into owner's scope (owner must be a class). */
    final def entered(using Context): this.type = {
      if (this.owner.isClass) {
        this.owner.asClass.enter(this)
        if (this.is(Module)) this.owner.asClass.enter(this.moduleClass)
      }
      this
    }

    /** Enter this symbol in its class owner after given `phase`. Create a fresh
     *  denotation for its owner class if the class does not already have one
     *  that starts being valid after `phase`.
     *  @pre  Symbol is a class member
     */
    def enteredAfter(phase: DenotTransformer)(using Context): this.type =
      if ctx.phaseId != phase.next.id then
        atPhase(phase.next)(enteredAfter(phase))
      else this.owner match {
        case owner: ClassSymbol =>
          if (owner.is(Package)) {
            denot.validFor |= InitialPeriod
            if (this.is(Module)) this.moduleClass.validFor |= InitialPeriod
          }
          else owner.ensureFreshScopeAfter(phase)
          assert(isPrivate || phase.changesMembers, i"$this entered in $owner at undeclared phase $phase")
          entered
        case _ => this
      }

    /** Remove symbol from scope of owning class */
    final def drop()(using Context): Unit = {
      this.owner.asClass.delete(this)
      if (this.is(Module)) this.owner.asClass.delete(this.moduleClass)
    }

    /** Remove symbol from scope of owning class after given `phase`. Create a fresh
     *  denotation for its owner class if the class does not already have one that starts being valid after `phase`.
     *  @pre  Symbol is a class member
     */
    def dropAfter(phase: DenotTransformer)(using Context): Unit =
      if ctx.phaseId != phase.next.id then
        atPhase(phase.next)(dropAfter(phase))
      else {
        assert (!this.owner.is(Package))
        this.owner.asClass.ensureFreshScopeAfter(phase)
        assert(isPrivate || phase.changesMembers, i"$this deleted in ${this.owner} at undeclared phase $phase")
        drop()
      }

    /** This symbol, if it exists, otherwise the result of evaluating `that` */
    inline def orElse(inline that: Symbol)(using Context): Symbol =
      if (this.exists) this else that

    /** If this symbol satisfies predicate `p` this symbol, otherwise `NoSymbol` */
    def filter(p: Symbol => Boolean): Symbol = if (p(this)) this else NoSymbol

    /** The current name of this symbol */
    final def name(using Context): ThisName = denot.name.asInstanceOf[ThisName]

    /** The source or class file from which this class or
     *  the class containing this symbol was generated, null if not applicable.
     *  Note that the returned classfile might be from the top-level class
     *  containing this symbol instead of the directly enclosing class.
     */
    def associatedFile(using Context): AbstractFile | Null =
      val compUnitInfo = compilationUnitInfo
      if compUnitInfo == null then (null: AbstractFile | Null)
      else compUnitInfo.associatedFile

    /** The compilation unit info (associated file, tasty versions, ...).
     *  Note that the returned CompilationUnitInfo might be from the top-level class
     *  containing this symbol instead of the directly enclosing class.
     *  Overridden in ClassSymbol
     */
    def compilationUnitInfo(using Context): CompilationUnitInfo | Null =
      lastDenot.topLevelClass.compilationUnitInfo

    /** The info of the TASTy from which this symbol was loaded, None if not applicable. */
    def tastyInfo(using Context): Option[TastyInfo] =
      val compUnitInfo = compilationUnitInfo
      if compUnitInfo == null then None
      else compUnitInfo.tastyInfo

    /** The class file from which this class was generated, null if not applicable. */
    final def binaryFile(using Context): AbstractFile | Null = {
      val file = associatedFile
      if file != null && file.isScalaBinary then file else null
    }

    /** A trap to avoid calling x.symbol on something that is already a symbol.
     *  This would be expanded to `toDenot(x).symbol` which is guaraneteed to be
     *  the same as `x`.
     *  With the given setup, all such calls will give implicit-not found errors
     */
    final def symbol(implicit ev: DontUseSymbolOnSymbol): Nothing = unsupported("symbol")

    final def source(using Context): SourceFile = {
      def valid(src: SourceFile): SourceFile =
        if (src.exists && !src.file.isScalaBinary) src
        else NoSource

      if (!denot.exists) NoSource
      else
        valid(defTree.source) match {
          case NoSource =>
            valid(denot.owner.source) match {
              case NoSource =>
                this match {
                  case cls: ClassSymbol      => valid(cls.sourceOfClass)
                  case _ if denot.is(Module) => valid(denot.moduleClass.source)
                  case _ => NoSource
                }
              case src => src
            }
          case src => src
        }
    }

    /** A symbol related to `sym` that is defined in source code.
     *
     *  @see [[interactive.Interactive.enclosingSourceSymbols]]
     */
    @annotation.tailrec final def sourceSymbol(using Context): Symbol =
      if (!denot.exists)
        this
      else if (denot.is(ModuleVal))
        this.moduleClass.sourceSymbol // The module val always has a zero-extent position
      else if denot.is(ExportedType) then
        denot.info.dropAlias.finalResultType.typeConstructor match
          case tp: NamedType => tp.symbol.sourceSymbol
          case _             => this
      else if denot.is(ExportedTerm) then
        val root = denot.maybeOwner match
          case cls: ClassSymbol => cls.rootTreeContaining(name.toString)
          case _                => EmptyTree
        val targets = root.collectSubTrees:
          case tree: DefDef if tree.symbol == denot.symbol => methPart(tree.rhs).tpe
        targets.match
          case (tp: NamedType) :: _ => tp.symbol.sourceSymbol
          case _                    => this
      else if (denot.is(Synthetic)) {
        val linked = denot.linkedClass
        if (linked.exists && !linked.is(Synthetic))
          linked
        else
          denot.owner.sourceSymbol
      }
      else if (denot.isPrimaryConstructor)
        denot.owner.sourceSymbol
      else this

    /** The position of this symbol, or NoSpan if the symbol was not loaded
     *  from source or from TASTY. This is always a zero-extent position.
     */
    final def span: Span = if (coord.isSpan) coord.toSpan else NoSpan

    final def sourcePos(using Context): SourcePosition = {
      val src = source
      (if (src.exists) src else ctx.source).atSpan(span)
    }

    /** This positioned item, widened to `SrcPos`. Used to make clear we only need the
     *  position, typically for error reporting.
     */
    final def srcPos: SrcPos = this

    // ParamInfo types and methods
    def isTypeParam(using Context): Boolean = denot.is(TypeParam)
    def paramName(using Context): ThisName = name.asInstanceOf[ThisName]
    def paramInfo(using Context): Type = denot.info
    def paramInfoAsSeenFrom(pre: Type)(using Context): Type = pre.memberInfo(this)
    def paramInfoOrCompleter(using Context): Type = denot.infoOrCompleter
    def paramVariance(using Context): Variance = denot.variance
    def paramRef(using Context): TypeRef = denot.typeRef

    /** Copy a symbol, overriding selective fields.
     *  Note that `coord` and `compilationUnitInfo` will be set from the fields in `owner`, not
     *  the fields in `sym`. */
    def copy(using Context)(
        owner: Symbol = this.owner,
        name: ThisName = name,
        flags: FlagSet = this.flags,
        info: Type = this.info,
        privateWithin: Symbol = this.privateWithin,
        coord: Coord = NoCoord, // Can be `= owner.coord` once we bootstrap
        compUnitInfo: CompilationUnitInfo | Null = null // Can be `= owner.associatedFile` once we bootstrap
    ): Symbol = {
      val coord1 = if (coord == NoCoord) owner.coord else coord
      val compilationUnitInfo1 = if (compilationUnitInfo == null) owner.compilationUnitInfo else compilationUnitInfo


      if isClass then
        newClassSymbol(owner, name.asTypeName, flags, _ => info, privateWithin, coord1, compilationUnitInfo1)
      else
        newSymbol(owner, name, flags, info, privateWithin, coord1)
    }

// -------- Printing --------------------------------------------------------

    /** The prefix string to be used when displaying this symbol without denotation */
    protected def prefixString: String = "Symbol"

    override def toString: String =
      lastDenot.toString // + "#" + id // !!! DEBUG

    def toText(printer: Printer): Text = printer.toText(this)

    def showLocated(using Context): String = ctx.printer.locatedText(this).show
    def showExtendedLocation(using Context): String = ctx.printer.extendedLocationText(this).show
    def showDcl(using Context): String = ctx.printer.dclText(this).show
    def showKind(using Context): String = ctx.printer.kindString(this)
    def showName(using Context): String = ctx.printer.nameString(this)
    def showFullName(using Context): String = ctx.printer.fullNameString(this)

    override def hashCode(): Int = id // for debugging.
  }

  type TermSymbol = Symbol { type ThisName = TermName }
  type TypeSymbol = Symbol { type ThisName = TypeName }

  class ClassSymbol private[Symbols] (coord: Coord, val compUnitInfo: CompilationUnitInfo | Null, id: Int, nestingLevel: Int)
    extends Symbol(coord, id, nestingLevel) {

    type ThisName = TypeName

    type TreeOrProvider = tpd.TreeProvider | tpd.Tree

    private var myTree: TreeOrProvider = tpd.EmptyTree

    /** If this is a top-level class and `-Yretain-trees` (or `-from-tasty`) is set.
      * Returns the TypeDef tree (possibly wrapped inside PackageDefs) for this class, otherwise EmptyTree.
      * This will force the info of the class.
      */
    def rootTree(using Context): Tree = rootTreeContaining("")

    /** Same as `tree` but load tree only if `id == ""` or the tree might contain `id`.
     *  For Tasty trees this means consulting whether the name table defines `id`.
     *  For already loaded trees, we maintain the referenced ids in an attachment.
     */
    def rootTreeContaining(id: String)(using Context): Tree = {
      denot.infoOrCompleter match {
        case _: NoCompleter =>
        case _ => denot.ensureCompleted()
      }
      myTree match {
        case fn: TreeProvider =>
          if (id.isEmpty || fn.mightContain(id)) {
            val tree = fn.tree
            myTree = tree
            tree
          }
          else tpd.EmptyTree
        case tree: Tree @ unchecked =>
          if (id.isEmpty || mightContain(tree, id)) tree else tpd.EmptyTree
      }
    }

    def rootTreeOrProvider: TreeOrProvider = myTree

    private[dotc] def rootTreeOrProvider_=(t: TreeOrProvider)(using Context): Unit =
      myTree = t

    private def mightContain(tree: Tree, id: String)(using Context): Boolean = {
      val ids = tree.getAttachment(Ids) match {
        case Some(ids) => ids
        case None =>
          val idSet = mutable.SortedSet[String]()
          tree.foreachSubTree {
            case tree: tpd.NameTree if tree.name.toTermName.isInstanceOf[SimpleName] =>
              idSet += tree.name.toString
            case _ =>
          }
          val ids = idSet.toArray
          tree.putAttachment(Ids, ids)
          ids
      }
      ids.binarySearch(id) >= 0
    }

    /** The source or class file from which this class was generated, null if not applicable. */
    override def compilationUnitInfo(using Context): CompilationUnitInfo | Null =
      if compUnitInfo != null || this.is(Package) || this.owner.is(Package) then compUnitInfo
      else super.compilationUnitInfo

    private var mySource: SourceFile = NoSource

    final def sourceOfClass(using Context): SourceFile = {
      if !mySource.exists && !denot.is(Package) then
        // this allows sources to be added in annotations after `sourceOfClass` is first called
        val file = associatedFile
        if file != null && !file.isScalaBinary then
          mySource = ctx.getSource(file)
        else
          mySource = defn.patchSource(this)
          if !mySource.exists then
            val compUnitInfo = compilationUnitInfo
            if compUnitInfo != null then
              compUnitInfo.tastyInfo.flatMap(_.attributes.sourceFile) match
                case Some(path) => mySource = ctx.getSource(path)
                case _ =>
          if !mySource.exists then
            mySource = atPhaseNoLater(flattenPhase) {
              denot.topLevelClass.unforcedAnnotation(defn.SourceFileAnnot) match
                case Some(sourceAnnot) => sourceAnnot.argumentConstant(0) match
                  case Some(Constant(path: String)) => ctx.getSource(path)
                  case none => NoSource
                case none => NoSource
            }
      mySource
    }

    final def classDenot(using Context): ClassDenotation =
      denot.asInstanceOf[ClassDenotation]

    override protected def prefixString: String = "ClassSymbol"
  }

  @sharable object NoSymbol extends Symbol(NoCoord, 0, 0) {
    override def compilationUnitInfo(using Context): CompilationUnitInfo | Null = CompilationUnitInfo(NoSource.file)
    override def recomputeDenot(lastd: SymDenotation)(using Context): SymDenotation = NoDenotation
  }

  NoDenotation // force it in order to set `denot` field of NoSymbol

  /** Makes all denotation operations available on symbols */
  implicit def toDenot(sym: Symbol)(using Context): SymDenotation = sym.denot

  /** Makes all class denotation operations available on class symbols */
  implicit def toClassDenot(cls: ClassSymbol)(using Context): ClassDenotation = cls.classDenot

  /** The Definitions object */
  def defn(using Context): Definitions = ctx.definitions

  /** The current class */
  def currentClass(using Context): ClassSymbol = ctx.owner.enclosingClass.asClass

  type MutableSymbolMap[T] = EqHashMap[Symbol, T]
  def MutableSymbolMap[T](): EqHashMap[Symbol, T] = EqHashMap[Symbol, T]()
  def MutableSymbolMap[T](initialCapacity: Int): EqHashMap[Symbol, T] = EqHashMap[Symbol, T](initialCapacity)

// ---- Symbol creation methods ----------------------------------

  /** Create a symbol from its fields (info may be lazy) */
  def newSymbol[N <: Name](using Context)(
      owner: Symbol,
      name: N,
      flags: FlagSet,
      info: Type,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      nestingLevel: Int = ctx.nestingLevel): Symbol { type ThisName = N } = {
    val sym = new Symbol(coord, ctx.base.nextSymId, nestingLevel).asInstanceOf[Symbol { type ThisName = N }]
    val denot = SymDenotation(sym, owner, name, flags, info, privateWithin)
    sym.denot = denot
    sym
  }

  /** Create a class symbol from its non-info fields and a function
   *  producing its info (the produced info may be lazy).
   */
  def newClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      infoFn: ClassSymbol => Type,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      compUnitInfo: CompilationUnitInfo | Null = null)(using Context): ClassSymbol
  = {
    val cls = new ClassSymbol(coord, compUnitInfo, ctx.base.nextSymId, ctx.nestingLevel)
    val denot = SymDenotation(cls, owner, name, flags, infoFn(cls), privateWithin)
    cls.denot = denot
    cls
  }

  /** Create a class symbol from its non-info fields and the fields of its info. */
  def newCompleteClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      parents: List[TypeRef],
      decls: Scope,
      selfInfo: Type = NoType,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      compUnitInfo: CompilationUnitInfo | Null = null)(using Context): ClassSymbol =
    newClassSymbol(
        owner, name, flags,
        ClassInfo(owner.thisType, _, parents, decls, selfInfo),
        privateWithin, coord, compUnitInfo)

  /** Same as `newCompleteClassSymbol` except that `parents` can be a list of arbitrary
   *  types which get normalized into type refs and parameter bindings.
   */
  def newNormalizedClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      parentTypes: List[Type],
      selfInfo: Type = NoType,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      compUnitInfo: CompilationUnitInfo | Null = null)(using Context): ClassSymbol = {
    def completer = new LazyType {
      def complete(denot: SymDenotation)(using Context): Unit = {
        val cls = denot.asClass.classSymbol
        val decls = newScope
        denot.info = ClassInfo(owner.thisType, cls, parentTypes.map(_.dealias), decls, selfInfo)
      }
    }
    newClassSymbol(owner, name, flags, completer, privateWithin, coord, compUnitInfo)
  }

  def newRefinedClassSymbol(coord: Coord = NoCoord)(using Context): ClassSymbol =
    newCompleteClassSymbol(ctx.owner, tpnme.REFINE_CLASS, NonMember, parents = Nil, newScope, coord = coord)

  /** Create a module symbol with associated module class
   *  from its non-info fields and a function producing the info
   *  of the module class (this info may be lazy).
   */
  def newModuleSymbol(
      owner: Symbol,
      name: TermName,
      modFlags: FlagSet,
      clsFlags: FlagSet,
      infoFn: (TermSymbol, ClassSymbol) => Type, // typically a ModuleClassCompleterWithDecls
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      compUnitInfo: CompilationUnitInfo | Null = null)(using Context): TermSymbol
  = {
    val base = owner.thisType
    val modclsFlags = clsFlags | ModuleClassCreationFlags
    val modclsName = name.toTypeName.adjustIfModuleClass(modclsFlags)
    val module = newSymbol(
      owner, name, modFlags | ModuleValCreationFlags, NoCompleter, privateWithin, coord)
    val modcls = newClassSymbol(
      owner, modclsName, modclsFlags, infoFn(module, _), privateWithin, coord, compUnitInfo)
    module.info =
      if (modcls.isCompleted) TypeRef(owner.thisType, modcls)
      else new ModuleCompleter(modcls)
    module
  }

  /** Create a module symbol with associated module class
   *  from its non-info fields and the fields of the module class info.
   *  @param flags  The combined flags of the module and the module class
   *                These are masked with RetainedModuleValFlags/RetainedModuleClassFlags.
   */
  def newCompleteModuleSymbol(
      owner: Symbol,
      name: TermName,
      modFlags: FlagSet,
      clsFlags: FlagSet,
      parents: List[TypeRef],
      decls: Scope,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      compUnitInfo: CompilationUnitInfo | Null = null)(using Context): TermSymbol =
    newModuleSymbol(
        owner, name, modFlags, clsFlags,
        (module, modcls) => ClassInfo(
          owner.thisType, modcls, parents, decls, TermRef(owner.thisType, module)),
        privateWithin, coord, compUnitInfo)

  /** Same as `newCompleteModuleSymbol` except that `parents` can be a list of arbitrary
   *  types which get normalized into type refs and parameter bindings.
   */
  def newNormalizedModuleSymbol(
      owner: Symbol,
      name: TermName,
      modFlags: FlagSet,
      clsFlags: FlagSet,
      parentTypes: List[Type],
      decls: Scope,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      compUnitInfo: CompilationUnitInfo | Null = null)(using Context): TermSymbol = {
    def completer(module: Symbol) = new LazyType {
      def complete(denot: SymDenotation)(using Context): Unit = {
        val cls = denot.asClass.classSymbol
        val decls = newScope
        denot.info = ClassInfo(owner.thisType, cls, parentTypes.map(_.dealias), decls, TermRef(owner.thisType, module))
      }
    }
    newModuleSymbol(
        owner, name, modFlags, clsFlags,
        (module, modcls) => completer(module),
        privateWithin, coord, compUnitInfo)
  }

  /** Create a package symbol with associated package class
   *  from its non-info fields and a lazy type for loading the package's members.
   */
  def newPackageSymbol(
      owner: Symbol,
      name: TermName,
      infoFn: (TermSymbol, ClassSymbol) => LazyType)(using Context): TermSymbol =
    newModuleSymbol(owner, name, PackageCreationFlags, PackageCreationFlags, infoFn)
  /** Create a package symbol with associated package class
   *  from its non-info fields its member scope.
   */
  def newCompletePackageSymbol(
      owner: Symbol,
      name: TermName,
      modFlags: FlagSet = EmptyFlags,
      clsFlags: FlagSet = EmptyFlags,
      decls: Scope = newScope(0))(using Context): TermSymbol =
    newCompleteModuleSymbol(
      owner, name,
      modFlags | PackageCreationFlags, clsFlags | PackageCreationFlags,
      Nil, decls)

  /** Define a new symbol associated with a Bind or pattern wildcard and, by default, make it gadt narrowable. */
  def newPatternBoundSymbol(
      name: Name,
      info: Type,
      span: Span,
      addToGadt: Boolean = true,
      flags: FlagSet = EmptyFlags)(using Context): Symbol = {
    val sym = newSymbol(ctx.owner, name, Case | flags, info, coord = span)
    if (addToGadt && name.isTypeName) ctx.gadtState.addToConstraint(sym)
    sym
  }

  /** Create a stub symbol that will issue a missing reference error
   *  when attempted to be completed.
   */
  def newStubSymbol(owner: Symbol, name: Name, compUnitInfo: CompilationUnitInfo | Null = null)(using Context): Symbol = {
    def stubCompleter = new StubInfo()
    val normalizedOwner = if (owner.is(ModuleVal)) owner.moduleClass else owner
    typr.println(s"creating stub for ${name.show}, owner = ${normalizedOwner.denot.debugString}, compilation unit = $compUnitInfo")
    typr.println(s"decls = ${normalizedOwner.unforcedDecls.toList.map(_.debugString).mkString("\n  ")}") // !!! DEBUG
    //if (base.settings.debug.value) throw new Error()
    val stub = name match {
      case name: TermName =>
        newModuleSymbol(normalizedOwner, name, EmptyFlags, EmptyFlags, stubCompleter, compUnitInfo = compUnitInfo)
      case name: TypeName =>
        newClassSymbol(normalizedOwner, name, EmptyFlags, stubCompleter, compUnitInfo = compUnitInfo)
    }
    stub
  }

  /** Create the local template dummy of given class `cls`.
   *  In a template
   *
   *     trait T { val fld: Int; { val x: int = 2 }; val fld2 = { val y = 2; y }}
   *
   *  the owner of `x` is the local dummy of the template. The owner of the local
   *  dummy is then the class of the template itself. By contrast, the owner of `y`
   *  would be `fld2`. There is a single local dummy per template.
   */
  def newLocalDummy(cls: Symbol, coord: Coord = NoCoord)(using Context): TermSymbol =
    newSymbol(cls, nme.localDummyName(cls), NonMember, NoType)

  /** Create an import symbol pointing back to given qualifier `expr`. */
  def newImportSymbol(owner: Symbol, expr: Tree, coord: Coord = NoCoord)(using Context): TermSymbol =
    newImportSymbol(owner, ImportType(expr), coord = coord)

  /** Create an import symbol with given `info`. */
  def newImportSymbol(owner: Symbol, info: Type, coord: Coord)(using Context): TermSymbol =
    newSymbol(owner, nme.IMPORT, Synthetic | NonMember, info, coord = coord)

  /** Create a class constructor symbol for given class `cls`. */
  def newConstructor(
      cls: ClassSymbol,
      flags: FlagSet,
      paramNames: List[TermName],
      paramTypes: List[Type],
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord)(using Context): TermSymbol =
    newSymbol(cls, nme.CONSTRUCTOR, flags | Method, MethodType(paramNames, paramTypes, cls.typeRef), privateWithin, coord)

  /** Create an anonymous function symbol */
  def newAnonFun(owner: Symbol, info: Type, coord: Coord = NoCoord)(using Context): TermSymbol =
    newSymbol(owner, nme.ANON_FUN, Synthetic | Method, info, coord = coord)

  /** Create an empty default constructor symbol for given class `cls`. */
  def newDefaultConstructor(cls: ClassSymbol)(using Context): TermSymbol =
    newConstructor(cls, EmptyFlags, Nil, Nil)

  def newLazyImplicit(info: Type, coord: Coord = NoCoord)(using Context): TermSymbol =
    newSymbol(ctx.owner, LazyImplicitName.fresh(), EmptyFlags, info, coord = coord)

  /** Create a symbol representing a selftype declaration for class `cls`. */
  def newSelfSym(
      cls: ClassSymbol,
      name: TermName = nme.WILDCARD,
      selfInfo: Type = NoType)(using Context): TermSymbol =
    newSymbol(cls, name, SelfSymFlags, selfInfo.orElse(cls.classInfo.selfType), coord = cls.coord)

  /** Create new type parameters with given owner, names, and flags.
   *  @param boundsFn  A function that, given type refs to the newly created
   *                   parameters returns a list of their bounds.
   */
  def newTypeParams(
    owner: Symbol,
    names: List[TypeName],
    flags: FlagSet,
    boundsFn: List[TypeRef] => List[Type])(using Context): List[TypeSymbol] = {

    val tparamBuf = new mutable.ListBuffer[TypeSymbol]
    val trefBuf = new mutable.ListBuffer[TypeRef]
    for (name <- names) {
      val tparam = newSymbol(
        owner, name, flags | owner.typeParamCreationFlags, NoType, coord = owner.coord)
      tparamBuf += tparam
      trefBuf += TypeRef(owner.thisType, tparam)
    }
    val tparams = tparamBuf.toList
    val bounds = boundsFn(trefBuf.toList)
    for (tparam, bound) <- tparams.lazyZip(bounds) do
      tparam.info = bound
    tparams
  }

  /** Create a new skolem symbol. This is not the same as SkolemType, even though the
   *  motivation (create a singleton referencing to a type) is similar.
   */
  def newSkolem(tp: Type)(using Context): TermSymbol =
    newSymbol(defn.RootClass, nme.SKOLEM, SyntheticArtifact | NonMember | Permanent, tp)

  def newErrorSymbol(owner: Symbol, name: Name, msg: Message)(using Context): Symbol = {
    val errType = ErrorType(msg)
    newSymbol(owner, name, SyntheticArtifact,
        if (name.isTypeName) TypeAlias(errType) else errType)
  }

  /** Map given symbols, subjecting their attributes to the mappings
   *  defined in the given TreeTypeMap `ttmap`.
   *  Cross symbol references are brought over from originals to copies.
   *  Do not copy any symbols if all attributes of all symbols stay the same.
   */
  def mapSymbols(originals: List[Symbol], ttmap: TreeTypeMap, mapAlways: Boolean = false)(using Context): List[Symbol] =
    if (originals.forall(sym =>
        (ttmap.mapType(sym.info) eq sym.info) &&
        !(ttmap.oldOwners contains sym.owner)) && !mapAlways)
      originals
    else {
      val copies: List[Symbol] = for (original <- originals) yield
        val odenot = original.denot
        original.copy(
          owner = ttmap.mapOwner(odenot.owner),
          flags = odenot.flags &~ Touched,
          info = NoCompleter,
          privateWithin = ttmap.mapOwner(odenot.privateWithin),
          coord = original.coord)
      val ttmap1 = ttmap.withSubstitution(originals, copies)
      originals.lazyZip(copies) foreach { (original, copy) =>
        val odenot = original.denot
        val completer = new LazyType:

          def complete(denot: SymDenotation)(using Context): Unit =

            val oinfo = original.info match
              case ClassInfo(pre, _, parents, decls, selfInfo) =>
                assert(original.isClass)
                val parents1 = parents.mapConserve(ttmap.mapType)
                val otypeParams = original.typeParams
                if otypeParams.isEmpty then
                  ClassInfo(pre, copy.asClass, parents1, decls.cloneScope, selfInfo)
                else
                  // copy type params, enter other definitions unchanged
                  // type parameters need to be copied early, since other type
                  // computations depend on them.
                  val decls1 = newScope
                  val newTypeParams = mapSymbols(original.typeParams, ttmap1, mapAlways = true)
                  newTypeParams.foreach(decls1.enter)
                  for sym <- decls do if !sym.is(TypeParam) then decls1.enter(sym)
                  val parents2 = parents1.map(_.substSym(otypeParams, newTypeParams))
                  val selfInfo1 = selfInfo match
                    case selfInfo: Type => selfInfo.substSym(otypeParams, newTypeParams)
                    case _ => selfInfo
                  ClassInfo(pre, copy.asClass, parents2, decls1, selfInfo1)
              case oinfo => oinfo

            denot.info = oinfo // needed as otherwise we won't be able to go from Sym -> parents & etc
                               // Note that this is a hack, but hack commonly used in Dotty
                               // The same thing is done by other completers all the time
            denot.info = ttmap1.mapType(oinfo)
            denot.annotations = odenot.annotations.mapConserve(ttmap1.apply)

        end completer

        copy.info = completer
        copy.denot match
          case cd: ClassDenotation =>
            cd.registeredCompanion = original.registeredCompanion.subst(originals, copies)
          case _ =>
      }

      copies.foreach(_.ensureCompleted()) // avoid memory leak

      // Update Child annotations of classes encountered previously to new values
      // if some child is among the mapped symbols
      for orig <- ttmap1.substFrom do
        if orig.is(Sealed) && orig.children.exists(originals.contains) then
          val sealedCopy = orig.subst(ttmap1.substFrom, ttmap1.substTo)
          sealedCopy.annotations = sealedCopy.annotations.mapConserve(ttmap1.apply)

      copies
    }

  /** Matches lists of term symbols, including the empty list.
   *  All symbols in the list are assumed to be of the same kind.
   */
  object TermSymbols:
    def unapply(xs: List[Symbol])(using Context): Option[List[TermSymbol]] = xs match
      case (x: Symbol) :: _ if x.isType => None
      case _ => Some(xs.asInstanceOf[List[TermSymbol]])

  /** Matches lists of type symbols, excluding the empty list.
   *  All symbols in the list are assumed to be of the same kind.
   */
  object TypeSymbols:
    def unapply(xs: List[Symbol])(using Context): Option[List[TypeSymbol]] = xs match
      case (x: Symbol) :: _ if x.isType => Some(xs.asInstanceOf[List[TypeSymbol]])
      case _ => None

  type DontUseSymbolOnSymbol

// ----- Locating predefined symbols ----------------------------------------

  def requiredPackage(path: PreName)(using Context): TermSymbol = {
    val name = path.toTermName
    staticRef(name, isPackage = true).requiredSymbol("package", name)(_.is(Package)).asTerm
  }

  def requiredPackageRef(path: PreName)(using Context): TermRef = requiredPackage(path).termRef

  def requiredClass(path: PreName)(using Context): ClassSymbol = {
    val name = path.toTypeName
    staticRef(name).requiredSymbol("class", name)(_.isClass) match {
      case cls: ClassSymbol => cls
      case sym => defn.AnyClass
    }
  }

  def requiredClassRef(path: PreName)(using Context): TypeRef = requiredClass(path).typeRef

  /** Get ClassSymbol if class is either defined in current compilation run
   *  or present on classpath. Returns NoSymbol otherwise.
   */
  def getClassIfDefined(path: PreName)(using Context): Symbol =
    staticRef(path.toTypeName, generateStubs = false)
      .disambiguate(_.isClass).symbol

  /** Get a List of ClassSymbols which are either defined in current compilation
   *  run or present on classpath.
   */
  def getClassesIfDefined(paths: List[PreName])(using Context): List[ClassSymbol] =
    paths.map(getClassIfDefined).filter(_.exists).map(_.asInstanceOf[ClassSymbol])

  /** Get ClassSymbol if package is either defined in current compilation run
   *  or present on classpath. Returns NoSymbol otherwise.
   */
  def getPackageClassIfDefined(path: PreName)(using Context): Symbol =
    staticRef(path.toTypeName, isPackage = true, generateStubs = false)
      .disambiguate(_.is(PackageClass)).symbol

  def requiredModule(path: PreName)(using Context): TermSymbol = {
    val name = path.toTermName
    staticRef(name).requiredSymbol("object", name)(_.is(Module)).asTerm
  }

  /** Get module symbol if the module is either defined in current compilation run
   *  or present on classpath. Returns NoSymbol otherwise.
   */
  def getModuleIfDefined(path: PreName)(using Context): Symbol =
    staticRef(path.toTermName, generateStubs = false)
      .disambiguate(_.is(Module)).symbol

  def requiredModuleRef(path: PreName)(using Context): TermRef = requiredModule(path).termRef

  def requiredMethod(path: PreName)(using Context): TermSymbol = {
    val name = path.toTermName
    staticRef(name).requiredSymbol("method", name)(_.is(Method)).asTerm
  }

  def requiredMethodRef(path: PreName)(using Context): TermRef = requiredMethod(path).termRef
}
