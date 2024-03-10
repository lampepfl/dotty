package dotty.tools
package dotc
package core

import interfaces.CompilerCallback
import Decorators.*
import Periods.*
import Names.*
import Phases.*
import Types.*
import Symbols.*
import Scopes.*
import Uniques.*
import ast.Trees.*
import ast.untpd
import util.{NoSource, SimpleIdentityMap, SourceFile, HashSet, ReusableInstance}
import typer.{Implicits, ImportInfo, SearchHistory, SearchRoot, TypeAssigner, Typer, Nullables}
import inlines.Inliner
import Nullables.*
import Implicits.ContextualImplicits
import config.Settings.*
import config.Config
import reporting.*
import io.{AbstractFile, NoAbstractFile, PlainFile, Path}
import scala.io.Codec
import collection.mutable
import printing.*
import config.{JavaPlatform, SJSPlatform, Platform, ScalaSettings}
import classfile.ReusableDataReader
import StdNames.nme
import compiletime.uninitialized

import scala.annotation.internal.sharable

import DenotTransformers.DenotTransformer
import dotty.tools.dotc.profile.Profiler
import dotty.tools.dotc.sbt.interfaces.{IncrementalCallback, ProgressCallback}
import util.Property.Key
import util.Store
import plugins.*
import java.util.concurrent.atomic.AtomicInteger
import java.nio.file.InvalidPathException

object Contexts {

  private val (compilerCallbackLoc,  store1) = Store.empty.newLocation[CompilerCallback]()
  private val (incCallbackLoc,       store2) = store1.newLocation[IncrementalCallback | Null]()
  private val (printerFnLoc,         store3) = store2.newLocation[Context => Printer](new RefinedPrinter(_))
  private val (settingsStateLoc,     store4) = store3.newLocation[SettingsState]()
  private val (compilationUnitLoc,   store5) = store4.newLocation[CompilationUnit]()
  private val (runLoc,               store6) = store5.newLocation[Run | Null]()
  private val (profilerLoc,          store7) = store6.newLocation[Profiler]()
  private val (notNullInfosLoc,      store8) = store7.newLocation[List[NotNullInfo]]()
  private val (importInfoLoc,        store9) = store8.newLocation[ImportInfo | Null]()
  private val (typeAssignerLoc,     store10) = store9.newLocation[TypeAssigner](TypeAssigner)
  private val (progressCallbackLoc, store11) = store10.newLocation[ProgressCallback | Null]()

  private val initialStore = store11

  /** The current context */
  inline def ctx(using ctx: Context): Context = ctx

  /** Run `op` with given context */
  inline def inContext[T](c: Context)(inline op: Context ?=> T): T =
    op(using c)

  /** Execute `op` at given period */
  inline def atPeriod[T](pd: Period)(inline op: Context ?=> T)(using Context): T =
    op(using ctx.fresh.setPeriod(pd))

  /** Execute `op` at given phase id */
  inline def atPhase[T](pid: PhaseId)(inline op: Context ?=> T)(using Context): T =
    op(using ctx.withPhase(pid))

  /** Execute `op` at given phase */
  inline def atPhase[T](phase: Phase)(inline op: Context ?=> T)(using Context): T =
    op(using ctx.withPhase(phase))

  inline def atNextPhase[T](inline op: Context ?=> T)(using Context): T =
    atPhase(ctx.phase.next)(op)

  /** Execute `op` at the current phase if it's before the first transform phase,
   *  otherwise at the last phase before the first transform phase.
   *
   *  Note: this should be used instead of `atPhaseNoLater(ctx.picklerPhase)`
   *  because the later won't work if the `Pickler` phase is not present (for example,
   *  when using `QuoteCompiler`).
   */
  inline def atPhaseBeforeTransforms[T](inline op: Context ?=> T)(using Context): T =
    atPhaseNoLater(firstTransformPhase.prev)(op)

  inline def atPhaseNoLater[T](limit: Phase)(inline op: Context ?=> T)(using Context): T =
    op(using if !limit.exists || ctx.phase <= limit then ctx else ctx.withPhase(limit))

  inline def atPhaseNoEarlier[T](limit: Phase)(inline op: Context ?=> T)(using Context): T =
    op(using if !limit.exists || limit <= ctx.phase then ctx else ctx.withPhase(limit))

  inline private def inMode[T](mode: Mode)(inline op: Context ?=> T)(using ctx: Context): T =
    op(using if mode != ctx.mode then ctx.fresh.setMode(mode) else ctx)

  inline def withMode[T](mode: Mode)(inline op: Context ?=> T)(using ctx: Context): T =
    inMode(ctx.mode | mode)(op)

  inline def withoutMode[T](mode: Mode)(inline op: Context ?=> T)(using ctx: Context): T =
    inMode(ctx.mode &~ mode)(op)

  /** A context is passed basically everywhere in dotc.
   *  This is convenient but carries the risk of captured contexts in
   *  objects that turn into space leaks. To combat this risk, here are some
   *  conventions to follow:
   *
   *    - Never let an implicit context be an argument of a class whose instances
   *      live longer than the context.
   *    - Classes that need contexts for their initialization take an explicit parameter
   *      named `initctx`. They pass initctx to all positions where it is needed
   *      (and these positions should all be part of the intialization sequence of the class).
   *    - Classes that need contexts that survive initialization are instead passed
   *      a "condensed context", typically named `cctx` (or they create one). Condensed contexts
   *      just add some basic information to the context base without the
   *      risk of capturing complete trees.
   *    - To make sure these rules are kept, it would be good to do a sanity
   *      check using bytecode inspection with javap or scalap: Keep track
   *      of all class fields of type context; allow them only in whitelisted
   *      classes (which should be short-lived).
   */
  abstract class Context(val base: ContextBase) { thiscontext =>

    protected given Context = this

    def outer: Context

    /** All outer contexts, ending in `base.initialCtx` and then `NoContext` */
    def outersIterator: Iterator[Context] = new Iterator[Context] {
      var current = thiscontext
      def hasNext = current != NoContext
      def next = { val c = current; current = current.outer; c }
    }

    def period: Period
    def mode: Mode
    def owner: Symbol
    def tree: Tree[?]
    def scope: Scope
    def typerState: TyperState
    def gadt: GadtConstraint = gadtState.gadt
    def gadtState: GadtState
    def searchHistory: SearchHistory
    def source: SourceFile

    /** A map in which more contextual properties can be stored
     *  Typically used for attributes that are read and written only in special situations.
     */
    def moreProperties: Map[Key[Any], Any]

    def property[T](key: Key[T]): Option[T] =
      moreProperties.get(key).asInstanceOf[Option[T]]

    /** A store that can be used by sub-components.
     *  Typically used for attributes that are defined only once per compilation unit.
     *  Access to store entries is much faster than access to properties, and only
     *  slightly slower than a normal field access would be.
     */
    def store: Store

    /** The compiler callback implementation, or null if no callback will be called. */
    def compilerCallback: CompilerCallback = store(compilerCallbackLoc)

    /** The Zinc callback implementation if we are run from Zinc, null otherwise */
    def incCallback: IncrementalCallback | Null = store(incCallbackLoc)

    /** Run `op` if there exists an incremental callback */
    inline def withIncCallback(inline op: IncrementalCallback => Unit): Unit =
      val local = incCallback
      if local != null then op(local)

    def runZincPhases: Boolean =
      def forceRun = settings.YdumpSbtInc.value || settings.YforceSbtPhases.value
      val local = incCallback
      local != null && local.enabled || forceRun

    /** The Zinc compile progress callback implementation if we are run from Zinc, null otherwise */
    def progressCallback: ProgressCallback | Null = store(progressCallbackLoc)

    /** Run `op` if there exists a Zinc progress callback */
    inline def withProgressCallback(inline op: ProgressCallback => Unit): Unit =
      val local = progressCallback
      if local != null then op(local)

    /** The current plain printer */
    def printerFn: Context => Printer = store(printerFnLoc)

    /** A function creating a printer */
    def printer: Printer =
      val pr = printerFn(this)
      if this.settings.YplainPrinter.value then pr.plain else pr

    /** The current settings values */
    def settingsState: SettingsState = store(settingsStateLoc)

    /** The current compilation unit */
    def compilationUnit: CompilationUnit = store(compilationUnitLoc)

    /** The current compiler-run */
    def run: Run | Null = store(runLoc)

    /**  The current compiler-run profiler */
    def profiler: Profiler = store(profilerLoc)

    /** The paths currently known to be not null */
    def notNullInfos: List[NotNullInfo] = store(notNullInfosLoc)

    /** The currently active import info */
    def importInfo: ImportInfo | Null = store(importInfoLoc)

    /** The current type assigner or typer */
    def typeAssigner: TypeAssigner = store(typeAssignerLoc)

    /** The new implicit references that are introduced by this scope */
    private var implicitsCache: ContextualImplicits | Null = null
    def implicits: ContextualImplicits = {
      if (implicitsCache == null)
        implicitsCache = {
          val implicitRefs: List[ImplicitRef] =
            if (isClassDefContext)
              try owner.thisType.implicitMembers
              catch {
                case ex: CyclicReference => Nil
              }
            else if (isImportContext) importInfo.nn.importedImplicits
            else if (isNonEmptyScopeContext) scope.implicitDecls
            else Nil
          val outerImplicits =
            if (isImportContext && importInfo.nn.unimported.exists)
              outer.implicits exclude importInfo.nn.unimported
            else
              outer.implicits
          if (implicitRefs.isEmpty) outerImplicits
          else new ContextualImplicits(implicitRefs, outerImplicits, isImportContext)(this)
        }
      implicitsCache.nn
    }

    /** Either the current scope, or, if the current context owner is a class,
     *  the declarations of the current class.
     */
    def effectiveScope(using Context): Scope =
      val myOwner: Symbol | Null = owner
      if myOwner != null && myOwner.isClass then myOwner.asClass.unforcedDecls
      else scope

    def nestingLevel: Int = effectiveScope.nestingLevel

    /** Sourcefile corresponding to given abstract file, memoized */
    def getSource(file: AbstractFile, codec: => Codec = Codec(settings.encoding.value)) = {
      util.Stats.record("Context.getSource")
      base.sources.getOrElseUpdate(file, SourceFile(file, codec))
    }

    /** SourceFile with given path name, memoized */
    def getSource(path: TermName): SourceFile = getFile(path) match
      case NoAbstractFile => NoSource
      case file => getSource(file)

    /** SourceFile with given path, memoized */
    def getSource(path: String): SourceFile = getSource(path.toTermName)

    /** AbstraFile with given path name, memoized */
    def getFile(name: TermName): AbstractFile = base.files.get(name) match
      case Some(file) =>
        file
      case None =>
        try
          val file = new PlainFile(Path(name.toString))
          base.files(name) = file
          file
        catch
          case ex: InvalidPathException =>
            report.error(em"invalid file path: ${ex.getMessage}")
            NoAbstractFile

    /** AbstractFile with given path, memoized */
    def getFile(name: String): AbstractFile = getFile(name.toTermName)

    private var related: SimpleIdentityMap[Phase | SourceFile, Context] | Null = null

    private def lookup(key: Phase | SourceFile): Context | Null =
      util.Stats.record("Context.related.lookup")
      if related == null then
        related = SimpleIdentityMap.empty
        null
      else
        related.nn(key)

    private def withPhase(phase: Phase, pid: PhaseId): Context =
      util.Stats.record("Context.withPhase")
      val curId = phaseId
      if curId == pid then
        this
      else
        var ctx1 = lookup(phase)
        if ctx1 == null then
          util.Stats.record("Context.withPhase.new")
          ctx1 = fresh.setPhase(pid)
          related = related.nn.updated(phase, ctx1)
        ctx1

    final def withPhase(phase: Phase): Context = withPhase(phase, phase.id)
    final def withPhase(pid: PhaseId): Context = withPhase(base.phases(pid), pid)

    final def withSource(source: SourceFile): Context =
      util.Stats.record("Context.withSource")
      if this.source eq source then
        this
      else
        var ctx1 = lookup(source)
        if ctx1 == null then
          util.Stats.record("Context.withSource.new")
          val ctx2 = fresh.setSource(source)
          if ctx2.compilationUnit eq NoCompilationUnit then
            // `source` might correspond to a file not necessarily
            // in the current project (e.g. when inlining library code),
            // so set `mustExist` to false.
            ctx2.setCompilationUnit(CompilationUnit(source, mustExist = false))
          ctx1 = ctx2
          related = related.nn.updated(source, ctx2)
        ctx1

    // `creationTrace`-related code. To enable, uncomment the code below and the
    // call to `setCreationTrace()` in this file.
    /*
    /** If -Ydebug is on, the top of the stack trace where this context
     *  was created, otherwise `null`.
     */
    private var creationTrace: Array[StackTraceElement] = uninitialized

    private def setCreationTrace() =
      creationTrace = (new Throwable).getStackTrace().take(20)

    /** Print all enclosing context's creation stacktraces */
    def printCreationTraces() = {
      println("=== context creation trace =======")
      for (ctx <- outersIterator) {
        println(s">>>>>>>>> $ctx")
        if (ctx.creationTrace != null) println(ctx.creationTrace.mkString("\n"))
      }
      println("=== end context creation trace ===")
    }
    */

    /** The current reporter */
    def reporter: Reporter = typerState.reporter

    final def phase: Phase = base.phases(period.firstPhaseId)
    final def runId = period.runId
    final def phaseId = period.phaseId

    final def lastPhaseId = base.phases.length - 1

    /** Does current phase use an erased types interpretation? */
    final def erasedTypes = phase.erasedTypes

    /** Are we in a Java compilation unit? */
    final def isJava: Boolean = compilationUnit.isJava

    /** Is current phase after TyperPhase? */
    final def isAfterTyper = base.isAfterTyper(phase)
    final def isTyper = base.isTyper(phase)

    /** Is this a context for the members of a class definition? */
    def isClassDefContext: Boolean =
      owner.isClass && (owner ne outer.owner)

    /** Is this a context that introduces an import clause? */
    def isImportContext: Boolean =
      (this ne NoContext)
      && (outer ne NoContext)
      && (this.importInfo nen outer.importInfo)

    /** Is this a context that introduces a non-empty scope? */
    def isNonEmptyScopeContext: Boolean =
      (this.scope ne outer.scope) && !this.scope.isEmpty

    /** Is this a context for typechecking an inlined body? */
    def isInlineContext: Boolean =
      typer.isInstanceOf[Inliner#InlineTyper]

    /** The next outer context whose tree is a template or package definition
     *  Note: Currently unused
    def enclTemplate: Context = {
      var c = this
      while (c != NoContext && !c.tree.isInstanceOf[Template[?]] && !c.tree.isInstanceOf[PackageDef[?]])
        c = c.outer
      c
    }*/

    /** The context for a supercall. This context is used for elaborating
     *  the parents of a class and their arguments.
     *  The context is computed from the current class context. It has
     *
     *  - as owner: The primary constructor of the class
     *  - as outer context: The context enclosing the class context
     *  - as scope: The parameter accessors in the class context
     *
     *  The reasons for this peculiar choice of attributes are as follows:
     *
     *  - The constructor must be the owner, because that's where any local methods or closures
     *    should go.
     *  - The context may not see any class members (inherited or defined), and should
     *    instead see definitions defined in the outer context which might be shadowed by
     *    such class members. That's why the outer context must be the outer context of the class.
     *  - At the same time the context should see the parameter accessors of the current class,
     *    that's why they get added to the local scope. An alternative would have been to have the
     *    context see the constructor parameters instead, but then we'd need a final substitution step
     *    from constructor parameters to class parameter accessors.
     */
    def superCallContext: Context = {
      val locals = newScopeWith(owner.typeParams ++ owner.asClass.paramAccessors*)
      superOrThisCallContext(owner.primaryConstructor, locals)
    }

    /** The context for the arguments of a this(...) constructor call.
     *  The context is computed from the local auxiliary constructor context.
     *  It has
     *
     *   - as owner: The auxiliary constructor
     *   - as outer context: The context enclosing the enclosing class context
     *   - as scope: The parameters of the auxiliary constructor.
     */
    def thisCallArgContext: Context = {
      val constrCtx = outersIterator.dropWhile(_.outer.owner == owner).next()
      superOrThisCallContext(owner, constrCtx.scope)
        .setTyperState(typerState)
        .setGadtState(gadtState)
        .fresh
        .setScope(this.scope)
    }

    /** The super- or this-call context with given owner and locals. */
    private def superOrThisCallContext(owner: Symbol, locals: Scope): FreshContext = {
      var classCtx = outersIterator.dropWhile(!_.isClassDefContext).next()
      classCtx.outer.fresh.setOwner(owner)
        .setScope(locals)
        .setMode(classCtx.mode)
    }

    /** The context of expression `expr` seen as a member of a statement sequence */
    def exprContext(stat: Tree[?], exprOwner: Symbol): Context =
      if (exprOwner == this.owner) this
      else if (untpd.isSuperConstrCall(stat) && this.owner.isClass) superCallContext
      else fresh.setOwner(exprOwner)

    /** A new context that summarizes an import statement */
    def importContext(imp: Import[?], sym: Symbol): FreshContext =
       fresh.setImportInfo(ImportInfo(sym, imp.selectors, imp.expr))

    /** Is the debug option set? */
    def debug: Boolean = base.settings.Ydebug.value

    /** Is the verbose option set? */
    def verbose: Boolean = base.settings.verbose.value

    /** Should use colors when printing? */
    def useColors: Boolean =
      base.settings.color.value == "always"

    def withColors: FreshContext =
      fresh.setSetting(ctx.settings.color, "always")

    def withoutColors: FreshContext =
      fresh.setSetting(ctx.settings.color, "never")

    /** Is the explicit nulls option set? */
    def explicitNulls: Boolean = base.settings.YexplicitNulls.value

    /** Is the flexible types option set? */
    def flexibleTypes: Boolean = base.settings.YexplicitNulls.value && !base.settings.YnoFlexibleTypes.value

    /** A fresh clone of this context embedded in this context. */
    def fresh: FreshContext = freshOver(this)

    /** A fresh clone of this context embedded in the specified `outer` context. */
    def freshOver(outer: Context): FreshContext =
      FreshContext(base).init(outer, this).setTyperState(this.typerState)

    final def withOwner(owner: Symbol): Context =
      if (owner ne this.owner) fresh.setOwner(owner) else this

    final def withTyperState(typerState: TyperState): Context =
      if typerState ne this.typerState then fresh.setTyperState(typerState) else this

    final def withUncommittedTyperState: Context =
      withTyperState(typerState.uncommittedAncestor)

    final def withProperty[T](key: Key[T], value: Option[T]): Context =
      if (property(key) == value) this
      else value match {
        case Some(v) => fresh.setProperty(key, v)
        case None => fresh.dropProperty(key)
      }

    def typer: Typer = this.typeAssigner match {
      case typer: Typer => typer
      case _ => new Typer
    }

    override def toString: String =
      def iinfo(using Context) =
        val info = ctx.importInfo
        if (info == null) "" else i"${info.selectors}%, %"
      def cinfo(using Context) =
        val core = s"  owner = ${ctx.owner}, scope = ${ctx.scope}, import = $iinfo"
        if (ctx ne NoContext) && (ctx.implicits ne ctx.outer.implicits) then
          s"$core, implicits = ${ctx.implicits}"
        else
          core
      s"""Context(
         |${outersIterator.map(ctx => cinfo(using ctx)).mkString("\n\n")})""".stripMargin

    def settings: ScalaSettings            = base.settings
    def definitions: Definitions           = base.definitions
    def platform: Platform                 = base.platform
    def pendingUnderlying: util.HashSet[Type]      = base.pendingUnderlying
    def uniqueNamedTypes: Uniques.NamedTypeUniques = base.uniqueNamedTypes
    def uniques: util.WeakHashSet[Type]            = base.uniques

    def initialize()(using Context): Unit = base.initialize()

    protected def resetCaches(): Unit =
      implicitsCache = null
      related = null

    /** Reuse this context as a fresh context nested inside `outer`
     *  But keep the typerstate, this one has to be set explicitly if needed.
     */
    def reuseIn(outer: Context): this.type
  }

  /** A condensed context provides only a small memory footprint over
   *  a Context base, and therefore can be stored without problems in
   *  long-lived objects.
  abstract class CondensedContext extends Context {
    override def condensed = this
  }
  */

  /** A fresh context allows selective modification
   *  of its attributes using the with... methods.
   */
  class FreshContext(base: ContextBase) extends Context(base) {
    util.Stats.record("Context.fresh")

    private var _outer: Context = uninitialized
    def outer: Context = _outer

    private var _period: Period = uninitialized
    final def period: Period = _period

    private var _mode: Mode = uninitialized
    final def mode: Mode = _mode

    private var _owner: Symbol = uninitialized
    final def owner: Symbol = _owner

    private var _tree: Tree[?] = uninitialized
    final def tree: Tree[?] = _tree

    private var _scope: Scope = uninitialized
    final def scope: Scope = _scope

    private var _typerState: TyperState = uninitialized
    final def typerState: TyperState = _typerState

    private var _gadtState: GadtState = uninitialized
    final def gadtState: GadtState = _gadtState

    private var _searchHistory: SearchHistory = uninitialized
    final def searchHistory: SearchHistory = _searchHistory

    private var _source: SourceFile = uninitialized
    final def source: SourceFile = _source

    private var _moreProperties: Map[Key[Any], Any] = uninitialized
    final def moreProperties: Map[Key[Any], Any] = _moreProperties

    private var _store: Store = uninitialized
    final def store: Store = _store

   /** Initialize all context fields, except typerState, which has to be set separately
     *  @param  outer   The outer context
     *  @param  origin  The context from which fields are copied
     */
    private[Contexts] def init(outer: Context, origin: Context): this.type = {
      _outer = outer
      _period = origin.period
      _mode = origin.mode
      _owner = origin.owner
      _tree = origin.tree
      _scope = origin.scope
      _gadtState = origin.gadtState
      _searchHistory = origin.searchHistory
      _source = origin.source
      _moreProperties = origin.moreProperties
      _store = origin.store
      this
    }

    def reuseIn(outer: Context): this.type =
      resetCaches()
      init(outer, outer)

    def setPeriod(period: Period): this.type =
      util.Stats.record("Context.setPeriod")
      //assert(period.firstPhaseId == period.lastPhaseId, period)
      this._period = period
      this

    def setMode(mode: Mode): this.type =
      util.Stats.record("Context.setMode")
      this._mode = mode
      this

    def setOwner(owner: Symbol): this.type =
      util.Stats.record("Context.setOwner")
      assert(owner != NoSymbol)
      this._owner = owner
      this

    def setTree(tree: Tree[?]): this.type =
      util.Stats.record("Context.setTree")
      this._tree = tree
      this

    def setScope(scope: Scope): this.type =
      this._scope = scope
      this

    def setNewScope: this.type =
      util.Stats.record("Context.setScope")
      this._scope = newScope
      this

    def setTyperState(typerState: TyperState): this.type =
      this._typerState = typerState
      this
    def setNewTyperState(): this.type =
      setTyperState(typerState.fresh(committable = true))
    def setExploreTyperState(): this.type =
      setTyperState(typerState.fresh(committable = false))
    def setReporter(reporter: Reporter): this.type =
      setTyperState(typerState.fresh().setReporter(reporter))

    def setTyper(typer: Typer): this.type =
      this._scope = typer.scope
      setTypeAssigner(typer)

    def setGadtState(gadtState: GadtState): this.type =
      util.Stats.record("Context.setGadtState")
      this._gadtState = gadtState
      this
    def setFreshGADTBounds: this.type =
      setGadtState(gadtState.fresh)

    def setSearchHistory(searchHistory: SearchHistory): this.type =
      util.Stats.record("Context.setSearchHistory")
      this._searchHistory = searchHistory
      this

    def setSource(source: SourceFile): this.type =
      util.Stats.record("Context.setSource")
      this._source = source
      this

    private def setMoreProperties(moreProperties: Map[Key[Any], Any]): this.type =
      util.Stats.record("Context.setMoreProperties")
      this._moreProperties = moreProperties
      this

    private def setStore(store: Store): this.type =
      util.Stats.record("Context.setStore")
      this._store = store
      this

    def setCompilationUnit(compilationUnit: CompilationUnit): this.type = {
      setSource(compilationUnit.source)
      updateStore(compilationUnitLoc, compilationUnit)
    }

    def setCompilerCallback(callback: CompilerCallback): this.type = updateStore(compilerCallbackLoc, callback)
    def setIncCallback(callback: IncrementalCallback): this.type = updateStore(incCallbackLoc, callback)
    def setProgressCallback(callback: ProgressCallback): this.type = updateStore(progressCallbackLoc, callback)
    def setPrinterFn(printer: Context => Printer): this.type = updateStore(printerFnLoc, printer)
    def setSettings(settingsState: SettingsState): this.type = updateStore(settingsStateLoc, settingsState)
    def setRun(run: Run | Null): this.type = updateStore(runLoc, run)
    def setProfiler(profiler: Profiler): this.type = updateStore(profilerLoc, profiler)
    def setNotNullInfos(notNullInfos: List[NotNullInfo]): this.type = updateStore(notNullInfosLoc, notNullInfos)
    def setImportInfo(importInfo: ImportInfo): this.type =
      importInfo.mentionsFeature(nme.unsafeNulls) match
        case Some(true) =>
          setMode(this.mode &~ Mode.SafeNulls)
        case Some(false) if ctx.settings.YexplicitNulls.value =>
          setMode(this.mode | Mode.SafeNulls)
        case _ =>
      updateStore(importInfoLoc, importInfo)
    def setTypeAssigner(typeAssigner: TypeAssigner): this.type = updateStore(typeAssignerLoc, typeAssigner)

    def setProperty[T](key: Key[T], value: T): this.type =
      setMoreProperties(moreProperties.updated(key, value))

    def dropProperty(key: Key[?]): this.type =
      setMoreProperties(moreProperties - key)

    def addLocation[T](initial: T): Store.Location[T] = {
      val (loc, store1) = store.newLocation(initial)
      setStore(store1)
      loc
    }

    def addLocation[T](): Store.Location[T] = {
      val (loc, store1) = store.newLocation[T]()
      setStore(store1)
      loc
    }

    def updateStore[T](loc: Store.Location[T], value: T): this.type =
      setStore(store.updated(loc, value))

    def setPhase(pid: PhaseId): this.type = setPeriod(Period(runId, pid))
    def setPhase(phase: Phase): this.type = setPeriod(Period(runId, phase.start, phase.end))

    def setSetting[T](setting: Setting[T], value: T): this.type =
      setSettings(setting.updateIn(settingsState, value))

    def setDebug: this.type = setSetting(base.settings.Ydebug, true)
  }

  object FreshContext:
    /** Defines an initial context with given context base and possible settings. */
    def initial(base: ContextBase, settingsGroup: SettingGroup): Context =
      val c = new FreshContext(base)
      c._outer = NoContext
      c._period = InitialPeriod
      c._mode = Mode.None
      c._typerState = TyperState.initialState()
      c._owner = NoSymbol
      c._tree = untpd.EmptyTree
      c._moreProperties = Map(MessageLimiter -> DefaultMessageLimiter())
      c._scope = EmptyScope
      c._source = NoSource
      c._store = initialStore
          .updated(settingsStateLoc, settingsGroup.defaultState)
          .updated(notNullInfosLoc, Nil)
          .updated(compilationUnitLoc, NoCompilationUnit)
      c._searchHistory = new SearchRoot
      c._gadtState = GadtState(GadtConstraint.empty)
      c
  end FreshContext

  extension (c: Context)
    def addNotNullInfo(info: NotNullInfo) =
      c.withNotNullInfos(c.notNullInfos.extendWith(info))

    def addNotNullRefs(refs: Set[TermRef]) =
      c.addNotNullInfo(NotNullInfo(refs, Set()))

    def withNotNullInfos(infos: List[NotNullInfo]): Context =
      if c.notNullInfos eq infos then c else c.fresh.setNotNullInfos(infos)

    def relaxedOverrideContext: Context =
      c.withModeBits(c.mode &~ Mode.SafeNulls | Mode.RelaxedOverriding)

  // TODO: Fix issue when converting ModeChanges and FreshModeChanges to extension givens
  extension (c: Context) {
    final def withModeBits(mode: Mode): Context =
      if (mode != c.mode) c.fresh.setMode(mode) else c

    final def addMode(mode: Mode): Context = withModeBits(c.mode | mode)
    final def retractMode(mode: Mode): Context = withModeBits(c.mode &~ mode)
  }

  extension (c: FreshContext) {
    final def addMode(mode: Mode): c.type = c.setMode(c.mode | mode)
    final def retractMode(mode: Mode): c.type = c.setMode(c.mode &~ mode)
  }

  /** Run `op` with a pool-allocated context that has an ExporeTyperState. */
  inline def explore[T](inline op: Context ?=> T)(using Context): T =
    exploreInFreshCtx(op)

  /** Run `op` with a pool-allocated FreshContext that has an ExporeTyperState. */
  inline def exploreInFreshCtx[T](inline op: FreshContext ?=> T)(using Context): T =
    val pool = ctx.base.exploreContextPool
    val nestedCtx = pool.next()
    try op(using nestedCtx)
    finally
      nestedCtx.typerState.reporter.asInstanceOf[ExploringReporter].reset()
      pool.free()

  /** Run `op` with a pool-allocated context that has a fresh typer state.
   *  Commit the typer state if `commit` applied to `op`'s result returns true.
   */
  inline def withFreshTyperState[T](inline op: Context ?=> T, inline commit: T => Context ?=> Boolean)(using Context): T =
    val pool = ctx.base.freshTSContextPool
    val nestedCtx = pool.next()
    try
      val result = op(using nestedCtx)
      if commit(result)(using nestedCtx) then
        nestedCtx.typerState.commit()
        nestedCtx.typerState.setCommittable(true)
      result
    finally
      pool.free()

  /** Run `op` with a pool-allocated context that has the given `owner`. */
  inline def runWithOwner[T](owner: Symbol)(inline op: Context ?=> T)(using Context): T =
    if Config.reuseOwnerContexts then
      val pool = ctx.base.generalContextPool
      try op(using pool.next().setOwner(owner).setTyperState(ctx.typerState))
      finally pool.free()
    else
      op(using ctx.fresh.setOwner(owner))

  /** The type comparer of the kind created by `maker` to be used.
   *  This is the currently active type comparer CMP if
   *   - CMP is associated with the current context, and
   *   - CMP is of the kind created by maker or maker creates a plain type comparer.
   *  Note: plain TypeComparers always take on the kind of the outer comparer if they are in the same context.
   *  In other words: tracking or explaining is a sticky property in the same context.
   */
  def comparer(using Context): TypeComparer =
    util.Stats.record("comparing")
    val base = ctx.base
    if base.comparersInUse > 0
       && (base.comparers(base.comparersInUse - 1).comparerContext eq ctx)
    then
      base.comparers(base.comparersInUse - 1).currentInstance
    else
      val result =
        if base.comparersInUse < base.comparers.size then
          base.comparers(base.comparersInUse)
        else
          val result = TypeComparer(ctx)
          base.comparers += result
          result
      base.comparersInUse += 1
      result.init(ctx)
      result

  inline def comparing[T](inline op: TypeComparer => T)(using Context): T =
    util.Stats.record("comparing")
    val saved = ctx.base.comparersInUse
    try op(comparer)
    finally ctx.base.comparersInUse = saved
  end comparing

  @sharable val NoContext: Context = new FreshContext((null: ContextBase | Null).uncheckedNN) {
    override val implicits: ContextualImplicits = new ContextualImplicits(Nil, null, false)(this: @unchecked)
    setSource(NoSource)
  }

  /** A context base defines state and associated methods that exist once per
   *  compiler run.
   */
  class ContextBase extends ContextState
                       with Phases.PhasesBase
                       with Plugins {

    val settings: ScalaSettings = ScalaSettings

    /** The initial context */
    val initialCtx: Context = FreshContext.initial(this: @unchecked, settings)

    /** The platform, initialized by `initPlatform()`. */
    private var _platform: Platform | Null = uninitialized

    /** The platform */
    def platform: Platform = {
      val p = _platform
      if p == null then
        throw new IllegalStateException(
            "initialize() must be called before accessing platform")
      p
    }

    protected def newPlatform(using Context): Platform =
      if (settings.scalajs.value) new SJSPlatform
      else new JavaPlatform

    /** The loader that loads the members of _root_ */
    def rootLoader(root: TermSymbol)(using Context): SymbolLoader = platform.rootLoader(root)

    /** The standard definitions */
    val definitions: Definitions = new Definitions

    // Set up some phases to get started */
    usePhases(List(SomePhase))

    /** Initializes the `ContextBase` with a starting context.
     *  This initializes the `platform` and the `definitions`.
     */
    def initialize()(using Context): Unit = {
      _platform = newPlatform
      definitions.init()
    }

    def fusedContaining(p: Phase): Phase =
      allPhases.find(_.period.containsPhaseId(p.id)).getOrElse(NoPhase)
  }

  class ContextPool:
    protected def fresh()(using Context): FreshContext =
      FreshContext(ctx.base).init(ctx, ctx)

    private var inUse: Int = 0
    private var pool = new mutable.ArrayBuffer[FreshContext]

    def next()(using Context): FreshContext =
      val base = ctx.base
      import base.*
      val nestedCtx =
        if inUse < pool.size then
          pool(inUse).reuseIn(ctx)
        else
          val c = fresh()
          pool += c
          c
      inUse += 1
      nestedCtx

    final def free(): Unit =
      inUse -= 1
  end ContextPool

  class TSContextPool extends ContextPool:
    override def next()(using Context) =
      val nextCtx = super.next()
      nextCtx.typerState.init(ctx.typerState, ctx.typerState.constraint)
      nextCtx

  class FreshTSContextPool extends TSContextPool:
    override protected def fresh()(using Context) =
      super.fresh().setTyperState(ctx.typerState.fresh(committable = true))

  class ExploreContextPool extends TSContextPool:
    override protected def fresh()(using Context) =
      val ts = TyperState()
        .setReporter(ExploringReporter())
        .setCommittable(false)
      super.fresh().setTyperState(ts)

  /** The essential mutable state of a context base, collected into a common class */
  class ContextState {
    // Symbols state

    /** Counter for unique symbol ids */
    private var _nextSymId: Int = 0
    def nextSymId: Int = { _nextSymId += 1; _nextSymId }

    /** Sources and Files that were loaded */
    val sources: util.HashMap[AbstractFile, SourceFile] = util.HashMap[AbstractFile, SourceFile]()
    val files: util.HashMap[TermName, AbstractFile] = util.HashMap()

    // Types state
    /** A table for hash consing unique types */
    private[core] val uniques: Uniques = Uniques()

    /** A table for hash consing unique applied types */
    private[dotc] val uniqueAppliedTypes: AppliedUniques = AppliedUniques()

    /** A table for hash consing unique named types */
    private[core] val uniqueNamedTypes: NamedTypeUniques = NamedTypeUniques()

    var emptyTypeBounds: TypeBounds | Null = null
    var emptyWildcardBounds: WildcardType | Null = null

    /** Number of findMember calls on stack */
    private[core] var findMemberCount: Int = 0

    /** List of names which have a findMemberCall on stack,
     *  after Config.LogPendingFindMemberThreshold is reached.
     */
    private[core] var pendingMemberSearches: List[Name] = Nil

    /** The number of recursive invocation of underlying on a NamedType
     *  during a controlled operation.
     */
    private[core] var underlyingRecursions: Int = 0

    /** The set of named types on which a currently active invocation
     *  of underlying during a controlled operation exists. */
    private[core] val pendingUnderlying: util.HashSet[Type] = util.HashSet[Type]()

    /** A map from ErrorType to associated message. We use this map
     *  instead of storing messages directly in ErrorTypes in order
     *  to avoid space leaks - the message usually captures a context.
     */
    private[core] val errorTypeMsg: mutable.Map[Types.ErrorType, Message] = mutable.Map()

    // Phases state

    private[core] var phasesPlan: List[List[Phase]] = uninitialized

    /** Phases by id */
    private[dotc] var phases: Array[Phase] = uninitialized

    /** Phases with consecutive Transforms grouped into a single phase, Empty array if fusion is disabled */
    private[core] var fusedPhases: Array[Phase] = Array.empty[Phase]

    /** Next denotation transformer id */
    private[core] var nextDenotTransformerId: Array[Int] = uninitialized

    private[core] var denotTransformers: Array[DenotTransformer] = uninitialized

    /** Flag to suppress inlining, set after overflow */
    private[dotc] var stopInlining: Boolean = false

    /** Cached -Yno-double-bindings setting. This is accessed from `setDenot`, which
     *  is fairly hot, so we don't want to lookup the setting each time it is called.
     */
    private[dotc] var checkNoDoubleBindings = false

    /** A variable that records that some error was reported in a globally committable context.
     *  The error will not necessarlily be emitted, since it could still be that
     *  the enclosing context will be aborted. The variable is used as a smoke test
     *  to turn off assertions that might be wrong if the program is erroneous. To
     *  just test for `ctx.reporter.errorsReported` is not always enough, since it
     *  could be that the context in which the assertion is tested is a completer context
     *  that's different from the context where the error was reported. See i13218.scala
     *  for a test.
     */
    private[dotc] var errorsToBeReported = false

    // Reporters state
    private[dotc] var indent: Int = 0

    protected[dotc] val indentTab: String = "  "

    val exploreContextPool = ExploreContextPool()
    val freshTSContextPool = FreshTSContextPool()
    val generalContextPool = ContextPool()

    private[Contexts] val comparers = new mutable.ArrayBuffer[TypeComparer]
    private[Contexts] var comparersInUse: Int = 0

    private var charArray = new Array[Char](256)

    private[core] val reusableDataReader = ReusableInstance(new ReusableDataReader())

    private[dotc] var wConfCache: (List[String], WConf) = uninitialized

    def sharedCharArray(len: Int): Array[Char] =
      while len > charArray.length do
        charArray = new Array[Char](charArray.length * 2)
      charArray

    def reset(): Unit =
      uniques.clear()
      uniqueAppliedTypes.clear()
      uniqueNamedTypes.clear()
      emptyTypeBounds = null
      emptyWildcardBounds = null
      errorsToBeReported = false
      errorTypeMsg.clear()
      sources.clear()
      files.clear()
      comparers.clear()  // forces re-evaluation of top and bottom classes in TypeComparer
      comparersInUse = 0

    // Test that access is single threaded

    /** The thread on which `checkSingleThreaded was invoked last */
    @sharable private var thread: Thread | Null = null

    /** Check that we are on the same thread as before */
    def checkSingleThreaded(): Unit =
      if (thread == null) thread = Thread.currentThread()
      else assert(thread == Thread.currentThread(), "illegal multithreaded access to ContextBase")
  }
}
