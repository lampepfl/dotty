package dotty.tools.dotc
package sbt

import scala.language.unsafeNulls

import java.io.File
import java.util.{Arrays, EnumSet}

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.classpath.FileUtils.{isTasty, isClass}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Denotations.StaleSymbol
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.util.{SrcPos, NoSourcePosition}
import dotty.tools.io
import dotty.tools.io.{AbstractFile, PlainFile, ZipArchive}
import xsbti.UseScope
import xsbti.api.DependencyContext
import xsbti.api.DependencyContext._

import scala.collection.{Set, mutable}


/** This phase sends information on classes' dependencies to sbt via callbacks.
 *
 *  This is used by sbt for incremental recompilation. Briefly, when a file
 *  changes sbt will recompile it, if its API has changed (determined by what
 *  `ExtractAPI` sent) then sbt will determine which reverse-dependencies
 *  (determined by what `ExtractDependencies` sent) of the API have to be
 *  recompiled depending on what changed.
 *
 *  See the documentation of `ExtractDependenciesCollector`, `ExtractAPI`,
 *  `ExtractAPICollector` and
 *  http://www.scala-sbt.org/0.13/docs/Understanding-Recompilation.html for more
 *  information on how sbt incremental compilation works.
 *
 *  The following flags affect this phase:
 *   -Yforce-sbt-phases
 *   -Ydump-sbt-inc
 *
 *  @see ExtractAPI
 */
class ExtractDependencies extends Phase {
  import ExtractDependencies._

  override def phaseName: String = ExtractDependencies.name

  override def description: String = ExtractDependencies.description

  override def isRunnable(using Context): Boolean = {
    def forceRun = ctx.settings.YdumpSbtInc.value || ctx.settings.YforceSbtPhases.value
    super.isRunnable && (ctx.sbtCallback != null || forceRun)
  }

  // Check no needed. Does not transform trees
  override def isCheckable: Boolean = false

  // This phase should be run directly after `Frontend`, if it is run after
  // `PostTyper`, some dependencies will be lost because trees get simplified.
  // See the scripted test `constants` for an example where this matters.
  // TODO: Add a `Phase#runsBefore` method ?

  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    val collector = new ExtractDependenciesCollector
    collector.traverse(unit.tpdTree)

    if (ctx.settings.YdumpSbtInc.value) {
      val deps = collector.dependencies.map(_.toString).toArray[Object]
      val names = collector.usedNames.map { case (clazz, names) => s"$clazz: $names" }.toArray[Object]
      Arrays.sort(deps)
      Arrays.sort(names)

      val pw = io.File(unit.source.file.jpath).changeExtension("inc").toFile.printWriter()
      // val pw = Console.out
      try {
        pw.println("Used Names:")
        pw.println("===========")
        names.foreach(pw.println)
        pw.println()
        pw.println("Dependencies:")
        pw.println("=============")
        deps.foreach(pw.println)
      } finally pw.close()
    }

    if (ctx.sbtCallback != null) {
      collector.usedNames.foreach {
        case (clazz, usedNames) =>
          val className = classNameAsString(clazz)
          usedNames.names.foreach {
            case (usedName, scopes) =>
              ctx.sbtCallback.usedName(className, usedName.toString, scopes)
          }
      }

      collector.dependencies.foreach(recordDependency)
    }
  }

  /*
   * Handles dependency on given symbol by trying to figure out if represents a term
   * that is coming from either source code (not necessarily compiled in this compilation
   * run) or from class file and calls respective callback method.
   */
  def recordDependency(dep: ClassDependency)(using Context): Unit = {
    val fromClassName = classNameAsString(dep.from)
    val sourceFile = ctx.compilationUnit.source.file.file

    def binaryDependency(file: File, binaryClassName: String) =
      ctx.sbtCallback.binaryDependency(file, binaryClassName, fromClassName, sourceFile, dep.context)

    def processExternalDependency(depFile: AbstractFile, binaryClassName: String) = {
      depFile match {
        case ze: ZipArchive#Entry => // The dependency comes from a JAR
          ze.underlyingSource match
            case Some(zip) if zip.file != null =>
              binaryDependency(zip.file, binaryClassName)
            case _ =>
        case pf: PlainFile => // The dependency comes from a class file
          // FIXME: pf.file is null for classfiles coming from the modulepath
          // (handled by JrtClassPath) because they cannot be represented as
          // java.io.File, since the `binaryDependency` callback must take a
          // java.io.File, this means that we cannot record dependencies coming
          // from the modulepath. For now this isn't a big deal since we only
          // support having the standard Java library on the modulepath.
          if pf.file != null then
            binaryDependency(pf.file, binaryClassName)
        case _ =>
          internalError(s"Ignoring dependency $depFile of unknown class ${depFile.getClass}}", dep.from.srcPos)
      }
    }

    val depFile = dep.to.associatedFile
    if (depFile != null) {
      // Cannot ignore inheritance relationship coming from the same source (see sbt/zinc#417)
      def allowLocal = dep.context == DependencyByInheritance || dep.context == LocalDependencyByInheritance
      val depClassFile =
        if depFile.isClass then depFile
        else depFile.resolveSibling(dep.to.binaryClassName + ".class")
      if (depClassFile != null) {
        // Dependency is external -- source is undefined
        processExternalDependency(depClassFile, dep.to.binaryClassName)
      } else if (allowLocal || depFile.file != sourceFile) {
        // We cannot ignore dependencies coming from the same source file because
        // the dependency info needs to propagate. See source-dependencies/trait-trait-211.
        val toClassName = classNameAsString(dep.to)
        ctx.sbtCallback.classDependency(toClassName, fromClassName, dep.context)
      }
    }
  }
}

object ExtractDependencies {
  val name: String = "sbt-deps"
  val description: String = "sends information on classes' dependencies to sbt"

  def classNameAsString(sym: Symbol)(using Context): String =
    sym.fullName.stripModuleClassSuffix.toString

  /** Report an internal error in incremental compilation. */
  def internalError(msg: => String, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    report.error(em"Internal error in the incremental compiler while compiling ${ctx.compilationUnit.source}: $msg", pos)
}

private case class ClassDependency(from: Symbol, to: Symbol, context: DependencyContext)

/** An object that maintain the set of used names from within a class */
private final class UsedNamesInClass {
  private val _names = new mutable.HashMap[Name, EnumSet[UseScope]]
  def names: collection.Map[Name, EnumSet[UseScope]] = _names

  def update(name: Name, scope: UseScope): Unit = {
    val scopes = _names.getOrElseUpdate(name, EnumSet.noneOf(classOf[UseScope]))
    scopes.add(scope)
  }

  override def toString(): String = {
    val builder = new StringBuilder
    names.foreach { case (name, scopes) =>
      builder.append(name.mangledString)
      builder.append(" in [")
      scopes.forEach(scope => builder.append(scope.toString))
      builder.append("]")
      builder.append(", ")
    }
    builder.toString()
  }
}

/** Extract the dependency information of a compilation unit.
 *
 *  To understand why we track the used names see the section "Name hashing
 *  algorithm" in http://www.scala-sbt.org/0.13/docs/Understanding-Recompilation.html
 *  To understand why we need to track dependencies introduced by inheritance
 *  specially, see the subsection "Dependencies introduced by member reference and
 *  inheritance" in the "Name hashing algorithm" section.
 */
private class ExtractDependenciesCollector extends tpd.TreeTraverser { thisTreeTraverser =>
  import tpd._

  private val _usedNames = new mutable.HashMap[Symbol, UsedNamesInClass]
  private val _dependencies = new mutable.HashSet[ClassDependency]

  /** The names used in this class, this does not include names which are only
   *  defined and not referenced.
   */
  def usedNames: collection.Map[Symbol, UsedNamesInClass] = _usedNames

  /** The set of class dependencies from this compilation unit.
   */
  def dependencies: Set[ClassDependency] = _dependencies

  /** Top level import dependencies are registered as coming from a first top level
   *  class/trait/object declared in the compilation unit. If none exists, issue warning.
   */
  private var _responsibleForImports: Symbol = _
  private def responsibleForImports(using Context) = {
    def firstClassOrModule(tree: Tree) = {
      val acc = new TreeAccumulator[Symbol] {
        def apply(x: Symbol, t: Tree)(using Context) =
          t match {
            case typeDef: TypeDef =>
              typeDef.symbol
            case other =>
              foldOver(x, other)
          }
      }
      acc(NoSymbol, tree)
    }

    if (_responsibleForImports == null) {
      val tree = ctx.compilationUnit.tpdTree
      _responsibleForImports = firstClassOrModule(tree)
      if (!_responsibleForImports.exists)
          report.warning("""|No class, trait or object is defined in the compilation unit.
                         |The incremental compiler cannot record the dependency information in such case.
                         |Some errors like unused import referring to a non-existent class might not be reported.
                         |""".stripMargin, tree.sourcePos)
    }
    _responsibleForImports
  }

  private var lastOwner: Symbol = _
  private var lastDepSource: Symbol = _

  /**
   * Resolves dependency source (that is, the closest non-local enclosing
   * class from a given `ctx.owner`
   */
  private def resolveDependencySource(using Context): Symbol = {
    def nonLocalEnclosingClass = {
      var clazz = ctx.owner.enclosingClass
      var owner = clazz

      while (!owner.is(PackageClass)) {
        if (owner.isTerm) {
          clazz = owner.enclosingClass
          owner = clazz
        } else {
          owner = owner.owner
        }
      }
      clazz
    }

    if (lastOwner != ctx.owner) {
      lastOwner = ctx.owner
      val source = nonLocalEnclosingClass
      lastDepSource = if (source.is(PackageClass)) responsibleForImports else source
    }

    lastDepSource
  }

  private def addUsedName(fromClass: Symbol, name: Name, scope: UseScope): Unit = {
    val usedName = _usedNames.getOrElseUpdate(fromClass, new UsedNamesInClass)
    usedName.update(name, scope)
  }

  private def addUsedName(name: Name, scope: UseScope)(using Context): Unit = {
    val fromClass = resolveDependencySource
    if (fromClass.exists) { // can happen when visiting imports
      assert(fromClass.isClass)
      addUsedName(fromClass, name, scope)
    }
  }

  private def addMemberRefDependency(sym: Symbol)(using Context): Unit =
    if (!ignoreDependency(sym)) {
      val enclOrModuleClass = if (sym.is(ModuleVal)) sym.moduleClass else sym.enclosingClass
      assert(enclOrModuleClass.isClass, s"$enclOrModuleClass, $sym")

      val fromClass = resolveDependencySource
      if (fromClass.exists) { // can happen when visiting imports
        assert(fromClass.isClass)

        addUsedName(fromClass, sym.zincMangledName, UseScope.Default)
        // packages have class symbol. Only record them as used names but not dependency
        if (!sym.is(Package)) {
          _dependencies += ClassDependency(fromClass, enclOrModuleClass, DependencyByMemberRef)
        }
      }
    }

  private def addInheritanceDependencies(tree: Closure)(using Context): Unit =
    // If the tpt is empty, this is a non-SAM lambda, so no need to register
    // an inheritance relationship.
    if !tree.tpt.isEmpty then
      val from = resolveDependencySource
      _dependencies += ClassDependency(from, tree.tpt.tpe.classSymbol, LocalDependencyByInheritance)

  private def addInheritanceDependencies(tree: Template)(using Context): Unit =
    if (tree.parents.nonEmpty) {
      val depContext = depContextOf(tree.symbol.owner)
      val from = resolveDependencySource
      for parent <- tree.parents do
        _dependencies += ClassDependency(from, parent.tpe.classSymbol, depContext)
    }

  private def depContextOf(cls: Symbol)(using Context): DependencyContext =
    if cls.isLocal then LocalDependencyByInheritance
    else DependencyByInheritance

  private def ignoreDependency(sym: Symbol)(using Context) =
    try
      !sym.exists ||
      sym.isAbsent(canForce = false) || // ignore dependencies that have a symbol but do not exist.
                                        // e.g. java.lang.Object companion object
      sym.isEffectiveRoot ||
      sym.isAnonymousFunction ||
      sym.isAnonymousClass
    catch case ex: StaleSymbol =>
      // can happen for constructor proxies. Test case is pos-macros/i13532.
      true


  /** Traverse the tree of a source file and record the dependencies and used names which
   *  can be retrieved using `dependencies` and`usedNames`.
   */
  override def traverse(tree: Tree)(using Context): Unit = try {
    tree match {
      case Match(selector, _) =>
        addPatMatDependency(selector.tpe)
      case Import(expr, selectors) =>
        def lookupImported(name: Name) =
          expr.tpe.member(name).symbol
        def addImported(name: Name) = {
          // importing a name means importing both a term and a type (if they exist)
          addMemberRefDependency(lookupImported(name.toTermName))
          addMemberRefDependency(lookupImported(name.toTypeName))
        }
        for sel <- selectors if !sel.isWildcard do
          addImported(sel.name)
          if sel.rename != sel.name then
            addUsedName(sel.rename, UseScope.Default)
      case exp @ Export(expr, selectors) =>
        val dep = expr.tpe.classSymbol
        if dep.exists && selectors.exists(_.isWildcard) then
          // If an export is a wildcard, that means that the enclosing class
          // has forwarders to all the applicable signatures in `dep`,
          // those forwarders will cause member/type ref dependencies to be
          // recorded. However, if `dep` adds more members with new names,
          // there has been no record that the enclosing class needs to
          // recompile to capture the new members. We add an
          // inheritance dependency in the presence of wildcard exports
          // to ensure all new members of `dep` are forwarded to.
          val depContext = depContextOf(ctx.owner.lexicallyEnclosingClass)
          val from = resolveDependencySource
          _dependencies += ClassDependency(from, dep, depContext)
      case t: TypeTree =>
        addTypeDependency(t.tpe)
      case ref: RefTree =>
        addMemberRefDependency(ref.symbol)
        addTypeDependency(ref.tpe)
      case t: Closure =>
        addInheritanceDependencies(t)
      case t: Template =>
        addInheritanceDependencies(t)
      case _ =>
    }

    tree match {
      case Inlined(call, _, _) if !call.isEmpty =>
        // The inlined call is normally ignored by TreeTraverser but we need to
        // record it as a dependency
        traverse(call)
      case vd: ValDef if vd.symbol.is(ModuleVal) =>
        // Don't visit module val
      case t: Template if t.symbol.owner.is(ModuleClass) =>
        // Don't visit self type of module class
        traverse(t.constr)
        t.parents.foreach(traverse)
        t.body.foreach(traverse)
      case _ =>
        traverseChildren(tree)
    }
  } catch {
    case ex: AssertionError =>
      println(i"asserted failed while traversing $tree")
      throw ex
  }

  /** Traverse a used type and record all the dependencies we need to keep track
   *  of for incremental recompilation.
   *
   *  As a motivating example, given a type `T` defined as:
   *
   *    type T >: L <: H
   *    type L <: A1
   *    type H <: B1
   *    class A1 extends A0
   *    class B1 extends B0
   *
   *  We need to record a dependency on `T`, `L`, `H`, `A1`, `B1`. This is
   *  necessary because the API representation that `ExtractAPI` produces for
   *  `T` just refers to the strings "L" and "H", it does not contain their API
   *  representation. Therefore, the name hash of `T` does not change if for
   *  example the definition of `L` changes.
   *
   *  We do not need to keep track of superclasses like `A0` and `B0` because
   *  the API representation of a class (and therefore its name hash) already
   *  contains all necessary information on superclasses.
   *
   *  A natural question to ask is: Since traversing all referenced types to
   *  find all these names is costly, why not change the API representation
   *  produced by `ExtractAPI` to contain that information? This way the name
   *  hash of `T` would change if any of the types it depends on change, and we
   *  would only need to record a dependency on `T`. Unfortunately there is no
   *  simple answer to the question "what does T depend on?" because it depends
   *  on the prefix and `ExtractAPI` does not compute types as seen from every
   *  possible prefix, the documentation of `ExtractAPI` explains why.
   *
   *  The tests in sbt `types-in-used-names-a`, `types-in-used-names-b`,
   *  `as-seen-from-a` and `as-seen-from-b` rely on this.
   */
  private abstract class TypeDependencyTraverser(using Context) extends TypeTraverser() {
    protected def addDependency(symbol: Symbol): Unit

    // Avoid cycles by remembering both the types (testcase:
    // tests/run/enum-values.scala) and the symbols of named types (testcase:
    // tests/pos-java-interop/i13575) we've seen before.
    val seen = new mutable.HashSet[Symbol | Type]
    def traverse(tp: Type): Unit = if (!seen.contains(tp)) {
      seen += tp
      tp match {
        case tp: NamedType =>
          val sym = tp.symbol
          if !seen.contains(sym) && !sym.is(Package) then
            seen += sym
            addDependency(sym)
            if !sym.isClass then traverse(tp.info)
            traverse(tp.prefix)
        case tp: ThisType =>
          traverse(tp.underlying)
        case tp: ConstantType =>
          traverse(tp.underlying)
        case tp: ParamRef =>
          traverse(tp.underlying)
        case _ =>
          traverseChildren(tp)
      }
    }
  }

  def addTypeDependency(tpe: Type)(using Context): Unit = {
    val traverser = new TypeDependencyTraverser {
      def addDependency(symbol: Symbol) = addMemberRefDependency(symbol)
    }
    traverser.traverse(tpe)
  }

  def addPatMatDependency(tpe: Type)(using Context): Unit = {
    val traverser = new TypeDependencyTraverser {
      def addDependency(symbol: Symbol) =
        if (!ignoreDependency(symbol) && symbol.is(Sealed)) {
          val usedName = symbol.zincMangledName
          addUsedName(usedName, UseScope.PatMatTarget)
        }
    }
    traverser.traverse(tpe)
  }
}
