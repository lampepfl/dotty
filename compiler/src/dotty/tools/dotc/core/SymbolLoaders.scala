package dotty.tools
package dotc
package core

import java.io.{IOException, File}
import java.nio.channels.ClosedByInterruptException

import scala.util.control.NonFatal

import dotty.tools.dotc.classpath.FileUtils.hasTastyExtension
import dotty.tools.io.{ ClassPath, ClassRepresentation, AbstractFile }
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions

import Contexts.*, Symbols.*, Flags.*, SymDenotations.*, Types.*, Scopes.*, Names.*
import NameOps.*
import StdNames.*
import classfile.{ClassfileParser, ClassfileTastyUUIDParser}
import Decorators.*

import util.Stats
import reporting.trace

import ast.desugar

import parsing.JavaParsers.OutlineJavaParser
import parsing.Parsers.OutlineParser
import dotty.tools.tasty.{TastyHeaderUnpickler, UnpickleException, UnpicklerConfig, TastyVersion}
import dotty.tools.dotc.core.tasty.TastyUnpickler

object SymbolLoaders {
  import ast.untpd.*

  /** A marker trait for a completer that replaces the original
   *  Symbol loader for an unpickled root.
   */
  trait SecondCompleter

  private def enterNew(
      owner: Symbol, member: Symbol,
      completer: SymbolLoader, scope: Scope = EmptyScope)(using Context): Symbol = {
    val comesFromScan =
      completer.isInstanceOf[SourcefileLoader]
    assert(comesFromScan || scope.lookup(member.name) == NoSymbol,
           s"${owner.fullName}.${member.name} already has a symbol")
    owner.asClass.enter(member, scope)
    member
  }

  /** Enter class with given `name` into scope of `owner`.
   */
  def enterClass(
      owner: Symbol, name: PreName, completer: SymbolLoader,
      flags: FlagSet = EmptyFlags, scope: Scope = EmptyScope)(using Context): Symbol = {
    val cls = newClassSymbol(owner, name.toTypeName.unmangleClassName.decode, flags, completer, compUnitInfo = completer.compilationUnitInfo)
    enterNew(owner, cls, completer, scope)
  }

  /** Enter module with given `name` into scope of `owner`.
   */
  def enterModule(
      owner: Symbol, name: PreName, completer: SymbolLoader,
      modFlags: FlagSet = EmptyFlags, clsFlags: FlagSet = EmptyFlags, scope: Scope = EmptyScope)(using Context): Symbol = {
    val module = newModuleSymbol(
      owner, name.toTermName.decode, modFlags, clsFlags,
      (module, _) => completer.proxy.withDecls(newScope).withSourceModule(module),
      compUnitInfo = completer.compilationUnitInfo)
    enterNew(owner, module, completer, scope)
    enterNew(owner, module.moduleClass, completer, scope)
  }

  /** Enter package with given `name` into scope of `owner`
   *  and give them `completer` as type.
   */
  def enterPackage(owner: Symbol, pname: TermName, completer: (TermSymbol, ClassSymbol) => PackageLoader)(using Context): Symbol = {
    val preExisting = owner.info.decls lookup pname
    if (preExisting != NoSymbol)
      // Some jars (often, obfuscated ones) include a package and
      // object with the same name. Rather than render them unusable,
      // offer a setting to resolve the conflict one way or the other.
      // This was motivated by the desire to use YourKit probes, which
      // require yjp.jar at runtime. See SI-2089.
      if (ctx.settings.YtermConflict.value == "package" || ctx.mode.is(Mode.Interactive)) {
        report.warning(
          s"Resolving package/object name conflict in favor of package ${preExisting.fullName}. The object will be inaccessible.")
        owner.asClass.delete(preExisting)
      }
      else if (ctx.settings.YtermConflict.value == "object") {
        report.warning(
          s"Resolving package/object name conflict in favor of object ${preExisting.fullName}.  The package will be inaccessible.")
        return NoSymbol
      }
      else
        throw TypeError(
          em"""$owner contains object and package with same name: $pname
             |one of them needs to be removed from classpath""")
    newModuleSymbol(owner, pname, PackageCreationFlags, PackageCreationFlags,
      completer).entered
  }

  /** Enter class and module with given `name` into scope of `owner`
   *  and give them `completer` as type.
   */
  def enterClassAndModule(
      owner: Symbol, name: PreName, completer: SymbolLoader,
      flags: FlagSet = EmptyFlags, scope: Scope = EmptyScope)(using Context): Unit = {
    val clazz = enterClass(owner, name, completer, flags, scope)
    val module = enterModule(
      owner, name, completer,
      modFlags = flags.toTermFlags & RetainedModuleValFlags,
      clsFlags = flags.toTypeFlags & RetainedModuleClassFlags,
      scope = scope)
  }

  /** Enter all toplevel classes and objects in file `src` into package `owner`, provided
   *  they are in the right package. Issue a warning if a class or object is in the wrong
   *  package, i.e. if the file path differs from the declared package clause.
   *
   *  All entered symbols are given a source completer of `src` as info.
   */
  def enterToplevelsFromSource(
      owner: Symbol, name: PreName, src: AbstractFile,
      scope: Scope = EmptyScope)(using Context): Unit =
    if src.exists && !src.isDirectory then
      val completer = new SourcefileLoader(src)
      val filePath = owner.ownersIterator.takeWhile(!_.isRoot).map(_.name.toTermName).toList

      def addPrefix(pid: RefTree, path: List[TermName]): List[TermName] = pid match {
        case Ident(name: TermName) => name :: path
        case Select(qual: RefTree, name: TermName) => name :: addPrefix(qual, path)
        case _ => path
      }

      def enterScanned(unit: CompilationUnit)(using Context) = {

        def checkPathMatches(path: List[TermName], what: String, tree: NameTree): Boolean = {
          val ok = filePath == path
          if (!ok)
            report.warning(i"""$what ${tree.name} is in the wrong directory.
                           |It was declared to be in package ${path.reverse.mkString(".")}
                           |But it is found in directory     ${filePath.reverse.mkString(File.separator.nn)}""",
              tree.srcPos.focus)
          ok
        }

        /** Run the subset of desugaring necessary to record the correct symbols */
        def simpleDesugar(tree: Tree): Tree = tree match
          case tree: PackageDef =>
            desugar.packageDef(tree)
          case tree: ModuleDef =>
            desugar.packageModuleDef(tree)
          case _ =>
            tree

        def traverse(tree: Tree, path: List[TermName]): Unit = simpleDesugar(tree) match {
          case tree @ PackageDef(pid, body) =>
            val path1 = addPrefix(pid, path)
            for (stat <- body) traverse(stat, path1)
          case tree: TypeDef if tree.isClassDef =>
            if (checkPathMatches(path, "class", tree))
              // It might be a case class or implicit class,
              // so enter class and module to be on the safe side
              enterClassAndModule(owner, tree.name, completer, scope = scope)
          case tree: ModuleDef =>
            if (checkPathMatches(path, "object", tree))
              enterModule(owner, tree.name, completer, scope = scope)
          case _ =>
        }

        traverse(
          if (unit.isJava) new OutlineJavaParser(unit.source).parse()
          else new OutlineParser(unit.source).parse(),
          Nil)
      }

      val unit = CompilationUnit(ctx.getSource(src))
      enterScanned(unit)(using ctx.fresh.setCompilationUnit(unit))

  /** The package objects of scala and scala.reflect should always
   *  be loaded in binary if classfiles are available, even if sourcefiles
   *  are newer. Late-compiling these objects from source leads to compilation
   *  order issues.
   *  Note: We do a name-base comparison here because the method is called before we even
   *  have ReflectPackage defined.
   */
  def binaryOnly(owner: Symbol, name: TermName)(using Context): Boolean =
    name == nme.PACKAGEkw &&
      (owner.name == nme.scala || owner.name == nme.reflect && owner.owner.name == nme.scala)

  /** Initialize toplevel class and module symbols in `owner` from class path representation `classRep`
   */
  def initializeFromClassPath(owner: Symbol, classRep: ClassRepresentation)(using Context): Unit =
    ((classRep.binary, classRep.source): @unchecked) match {
      case (Some(bin), Some(src)) if needCompile(bin, src) && !binaryOnly(owner, nameOf(classRep)) =>
        if (ctx.settings.verbose.value) report.inform("[symloader] picked up newer source file for " + src.path)
        enterToplevelsFromSource(owner, nameOf(classRep), src)
      case (None, Some(src)) =>
        if (ctx.settings.verbose.value) report.inform("[symloader] no class or tasty, picked up source file for " + src.path)
        enterToplevelsFromSource(owner, nameOf(classRep), src)
      case (Some(bin), _) =>
        val completer =
          if bin.hasTastyExtension then ctx.platform.newTastyLoader(bin)
          else ctx.platform.newClassLoader(bin)
        enterClassAndModule(owner, nameOf(classRep), completer)
    }

  def needCompile(bin: AbstractFile, src: AbstractFile): Boolean =
    src.lastModified >= bin.lastModified

  private def nameOf(classRep: ClassRepresentation)(using Context): TermName =
    classRep.fileName.sliceToTermName(0, classRep.nameLength)

  /** Load contents of a package
   */
  class PackageLoader(_sourceModule: TermSymbol, classPath: ClassPath) extends SymbolLoader {
    def compilationUnitInfo: CompilationUnitInfo | Null = null
    override def sourceModule(using Context): TermSymbol = _sourceModule
    def description(using Context): String = "package loader " + sourceModule.fullName

    private var enterFlatClasses: Option[() => Context ?=> Unit] = None

    Stats.record("package scopes")

    /** The scope of a package. This is different from a normal scope
  	 *  in that names of scope entries are kept in mangled form.
  	 */
    final class PackageScope extends MutableScope(0) {
      override def newScopeEntry(name: Name, sym: Symbol)(using Context): ScopeEntry =
        super.newScopeEntry(name.mangled, sym)

      override def lookupEntry(name: Name)(using Context): ScopeEntry | Null = {
        val mangled = name.mangled
        val e = super.lookupEntry(mangled)
        if (e != null) e
        else if (isFlatName(mangled.toSimpleName) && enterFlatClasses.isDefined) {
          Stats.record("package scopes with flatnames entered")
          enterFlatClasses.get()
          lookupEntry(name)
        }
        else e
      }

      override def ensureComplete()(using Context): Unit =
        for (enter <- enterFlatClasses) enter()

      override def newScopeLikeThis(): PackageScope = new PackageScope
    }

    private[core] val currentDecls: MutableScope = new PackageScope()

    private def isFlatName(name: SimpleName): Boolean = {
      val idx = name.lastIndexOf('$', name.length - 2)
      idx >= 0 &&
      (idx + str.TOPLEVEL_SUFFIX.length + 1 != name.length || !name.endsWith(str.TOPLEVEL_SUFFIX))
    }

    /** Name of class contains `$`, excepted names ending in `$package` */
    def hasFlatName(classRep: ClassRepresentation): Boolean = {
      val name = classRep.name
      val idx = name.lastIndexOf('$', name.length - 2)
      idx >= 0 &&
      (idx + str.TOPLEVEL_SUFFIX.length + 1 != name.length || !name.endsWith(str.TOPLEVEL_SUFFIX))
    }

    def maybeModuleClass(classRep: ClassRepresentation): Boolean = classRep.name.last == '$'

    private def enterClasses(root: SymDenotation, packageName: String, flat: Boolean)(using Context) = {
      def isAbsent(classRep: ClassRepresentation) =
        !root.unforcedDecls.lookup(classRep.name.toTypeName).exists

      if (!root.isRoot) {
        val classReps = classPath.list(packageName).classesAndSources

        for (classRep <- classReps)
          if (!maybeModuleClass(classRep) && hasFlatName(classRep) == flat &&
            (!flat || isAbsent(classRep))) // on 2nd enter of flat names, check that the name has not been entered before
            initializeFromClassPath(root.symbol, classRep)
        for (classRep <- classReps)
          if (maybeModuleClass(classRep) && hasFlatName(classRep) == flat &&
              isAbsent(classRep))
            initializeFromClassPath(root.symbol, classRep)
      }
    }

    def doComplete(root: SymDenotation)(using Context): Unit = {
      assert(root is PackageClass, root)
      val pre = root.owner.thisType
      root.info = ClassInfo(pre, root.symbol.asClass, Nil, currentDecls, pre select sourceModule)
      if (!sourceModule.isCompleted)
        sourceModule.completer.complete(sourceModule)

      val packageName = if (root.isEffectiveRoot) "" else root.symbol.javaClassName

      enterFlatClasses = Some { () =>
        enterFlatClasses = None
        inContext(ctx){enterClasses(root, packageName, flat = true)}
      }
      enterClasses(root, packageName, flat = false)
      if (!root.isEmptyPackage)
        for (pkg <- classPath.packages(packageName)) {
          val fullName = pkg.name
          val name =
            if (packageName.isEmpty) fullName
            else fullName.substring(packageName.length + 1).nn

          enterPackage(root.symbol, name.toTermName,
            (module, modcls) => new PackageLoader(module, classPath))
        }
    }
  }
}

/** A lazy type that completes itself by calling parameter doComplete.
 *  Any linked modules/classes or module classes are also initialized.
 */
abstract class SymbolLoader extends LazyType { self =>
  /** Load source or class file for `root`, return */
  def doComplete(root: SymDenotation)(using Context): Unit

  def compilationUnitInfo: CompilationUnitInfo | Null

  /** Description of the resource (ClassPath, AbstractFile)
   *  being processed by this loader
   */
  def description(using Context): String

  /** A proxy to this loader that keeps the doComplete operation
   *  but provides fresh slots for scope/sourceModule/moduleClass
   */
  def proxy: SymbolLoader = new SymbolLoader {
    export self.{doComplete, compilationUnitInfo}
    def description(using Context): String = s"proxy to ${self.description}"
  }

  override def complete(root: SymDenotation)(using Context): Unit = {
    def signalError(ex: Exception): Unit = {
      if (ctx.debug) ex.printStackTrace()
      val msg = ex.getMessage()
      report.error(
        if msg == null then em"i/o error while loading ${root.name}"
        else em"""error while loading ${root.name},
                 |$msg""")
    }
    try {
      val start = System.currentTimeMillis
      trace.onDebug("loading") {
        doComplete(root)
      }
      report.informTime("loaded " + description, start)
    }
    catch {
      case ex: InterruptedException =>
        throw ex
      case ex: ClosedByInterruptException =>
        throw new InterruptedException
      case ex: IOException =>
        signalError(ex)
      case NonFatal(ex: TypeError) =>
        println(s"exception caught when loading $root: ${ex.toMessage}")
        throw ex
      case NonFatal(ex) =>
        println(s"exception caught when loading $root: $ex")
        throw ex
    }
    finally {
      def postProcess(denot: SymDenotation, other: Symbol) =
        if !denot.isCompleted &&
           !denot.completer.isInstanceOf[SymbolLoaders.SecondCompleter] then
          if denot.is(ModuleClass) && NamerOps.needsConstructorProxies(other) then
            NamerOps.makeConstructorCompanion(denot.sourceModule.asTerm, other.asClass)
            denot.resetFlag(Touched)
          else
            denot.markAbsent()

      val other = if root.isRoot then NoSymbol else root.scalacLinkedClass
      postProcess(root, other)
      if (!root.isRoot)
        postProcess(other, root.symbol)
    }
  }

  protected def rootDenots(rootDenot: ClassDenotation)(using Context): (ClassDenotation, ClassDenotation) = {
    val linkedDenot = rootDenot.scalacLinkedClass.denot match {
      case d: ClassDenotation => d
      case d =>
        // this can happen if the companion if shadowed by a val or type
        // in a package object; in this case, we make up some dummy denotation
        // as a stand in for loading.
        // An example for this situation is scala.reflect.Manifest, which exists
        // as a class in scala.reflect and as a val in scala.reflect.package.
        if (rootDenot.is(ModuleClass))
          newClassSymbol(
            rootDenot.owner, rootDenot.name.stripModuleClassSuffix.asTypeName, Synthetic,
              _ => NoType).classDenot
        else
          newModuleSymbol(
            rootDenot.owner, rootDenot.name.toTermName, Synthetic, Synthetic,
            (module, _) => NoLoader().withDecls(newScope).withSourceModule(module))
            .moduleClass.denot.asClass
    }
    if (rootDenot.is(ModuleClass)) (linkedDenot, rootDenot)
    else (rootDenot, linkedDenot)
  }
}

class ClassfileLoader(val classfile: AbstractFile) extends SymbolLoader {

  def compilationUnitInfo: CompilationUnitInfo | Null = CompilationUnitInfo(classfile)


  def description(using Context): String = "class file " + classfile.toString

  override def doComplete(root: SymDenotation)(using Context): Unit =
    val (classRoot, moduleRoot) = rootDenots(root.asClass)
    val classfileParser = new ClassfileParser(classfile, classRoot, moduleRoot)(ctx)
    classfileParser.run()
}

class TastyLoader(val tastyFile: AbstractFile) extends SymbolLoader {

  private val unpickler: tasty.DottyUnpickler =
    handleUnpicklingExceptions:
      val tastyBytes = tastyFile.toByteArray
      new tasty.DottyUnpickler(tastyFile, tastyBytes) // reads header and name table

  val compilationUnitInfo: CompilationUnitInfo | Null = unpickler.compilationUnitInfo

  def description(using Context): String = "TASTy file " + tastyFile.toString

  override def doComplete(root: SymDenotation)(using Context): Unit =
    handleUnpicklingExceptions:
      checkTastyUUID()
      val (classRoot, moduleRoot) = rootDenots(root.asClass)
      unpickler.enter(roots = Set(classRoot, moduleRoot, moduleRoot.sourceModule))(using ctx.withSource(util.NoSource))
      if mayLoadTreesFromTasty then
        classRoot.classSymbol.rootTreeOrProvider = unpickler
        moduleRoot.classSymbol.rootTreeOrProvider = unpickler

  private def handleUnpicklingExceptions[T](thunk: =>T): T =
    try thunk
    catch case e: RuntimeException =>
      val message = e match
        case e: UnpickleException =>
          s"""TASTy file ${tastyFile.canonicalPath} could not be read, failing with:
            |  ${Option(e.getMessage).getOrElse("")}""".stripMargin
        case _ =>
          s"""TASTy file ${tastyFile.canonicalPath} is broken, reading aborted with ${e.getClass}
            |  ${Option(e.getMessage).getOrElse("")}""".stripMargin
      throw IOException(message, e)


  private def checkTastyUUID()(using Context): Unit =
    val classfile =
      val className = tastyFile.name.stripSuffix(".tasty")
      tastyFile.resolveSibling(className + ".class")
    if classfile != null then
      val tastyUUID = unpickler.unpickler.header.uuid
      new ClassfileTastyUUIDParser(classfile)(ctx).checkTastyUUID(tastyUUID)
    else
      // This will be the case in any of our tests that compile with `-Youtput-only-tasty`
      report.inform(s"No classfiles found for $tastyFile when checking TASTy UUID")

  private def mayLoadTreesFromTasty(using Context): Boolean =
    ctx.settings.YretainTrees.value || ctx.settings.fromTasty.value
}

class SourcefileLoader(val srcfile: AbstractFile) extends SymbolLoader {
  def description(using Context): String = "source file " + srcfile.toString
  def compilationUnitInfo: CompilationUnitInfo | Null = CompilationUnitInfo(srcfile)
  def doComplete(root: SymDenotation)(using Context): Unit =
    ctx.run.nn.lateCompile(srcfile, typeCheck = ctx.settings.YretainTrees.value)
}

/** A NoCompleter which is also a SymbolLoader. */
class NoLoader extends SymbolLoader with NoCompleter {
  def description(using Context): String = "NoLoader"
  def compilationUnitInfo: CompilationUnitInfo | Null = null
  override def complete(root: SymDenotation)(using Context): Unit =
    super[NoCompleter].complete(root)
  def doComplete(root: SymDenotation)(using Context): Unit =
    unsupported("doComplete")
}
