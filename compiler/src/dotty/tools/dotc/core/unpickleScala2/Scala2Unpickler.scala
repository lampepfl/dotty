package dotty.tools
package dotc
package core
package unpickleScala2

import scala.language.unsafeNulls

import java.io.IOException
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

import Contexts._, Symbols._, Types._, Scopes._, SymDenotations._, Names._, NameOps._
import StdNames._, Denotations._, NameOps._, Flags._, Constants._, Annotations._, Phases._
import NameKinds.{Scala2MethodNameKinds, SuperAccessorName, ExpandedName}
import util.Spans._
import dotty.tools.dotc.ast.{tpd, untpd}, ast.tpd._
import ast.untpd.Modifiers
import backend.sjs.JSDefinitions
import printing.Texts._
import printing.Printer
import io.AbstractFile
import util.common._
import util.NoSourcePosition
import typer.Checking.checkNonCyclic
import typer.Nullables._
import transform.SymUtils._
import PickleBuffer._
import PickleFormat._
import Decorators._
import TypeApplications._
import classfile.ClassfileParser
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.annotation.switch
import reporting._
import cc.{adaptFunctionTypeUnderPureFuns, adaptByNameArgUnderPureFuns}

object Scala2Unpickler {

  /** Exception thrown if classfile is corrupted */
  class BadSignature(msg: String) extends RuntimeException(msg)

  case class TempPolyType(tparams: List[TypeSymbol], tpe: Type) extends UncachedGroundType {
    override def fallbackToText(printer: Printer): Text =
      "[" ~ printer.dclsText(tparams, ", ") ~ "]" ~ printer.toText(tpe)
  }

  /** Temporary type for classinfos, will be decomposed on completion of the class */
  case class TempClassInfoType(parentTypes: List[Type], decls: Scope, clazz: Symbol) extends UncachedGroundType

  /** Convert temp poly type to poly type and leave other types alone. */
  def translateTempPoly(tp: Type)(using Context): Type = tp match {
    case TempPolyType(tparams, restpe) =>
      // This check used to read `owner.isTerm` but that wasn't always correct,
      // I'm not sure `owner.is(Method)` is 100% correct either but it seems to
      // work better. See the commit message where this change was introduced
      // for more information.
      (if (tparams.head.owner.is(Method)) PolyType else HKTypeLambda)
        .fromParams(tparams, restpe)
    case tp => tp
  }

  def addConstructorTypeParams(denot: SymDenotation)(using Context): Unit = {
    assert(denot.isConstructor)
    denot.info = PolyType.fromParams(denot.owner.typeParams, denot.info)
  }

  def ensureConstructor(cls: ClassSymbol, clsDenot: ClassDenotation, scope: Scope)(using Context): Unit = {
    if (scope.lookup(nme.CONSTRUCTOR) == NoSymbol) {
      val constr = newDefaultConstructor(cls)
      // Scala 2 traits have a constructor iff they have initialization code
      // In dotc we represent that as !StableRealizable, which is also owner.is(NoInits)
      if clsDenot.flagsUNSAFE.is(Trait) then
        constr.setFlag(StableRealizable)
        clsDenot.setFlag(NoInits)
      addConstructorTypeParams(constr)
      cls.enter(constr, scope)
    }
  }

  def setClassInfo(denot: ClassDenotation, info: Type, fromScala2: Boolean, selfInfo: Type = NoType)(using Context): Unit = {
    val cls = denot.classSymbol
    val (tparams, TempClassInfoType(parents, decls, clazz)) = info match {
      case TempPolyType(tps, cinfo) => (tps, cinfo)
      case cinfo => (Nil, cinfo)
    }: @unchecked
    val ost =
      if (selfInfo eq NoType) && denot.is(ModuleClass) then
        val sourceModule = denot.sourceModule.orElse {
          // For non-toplevel modules, `sourceModule` won't be set when completing
          // the module class, we need to go find it ourselves.
          val modName = cls.name.sourceModuleName
          val alternate =
            if cls.privateWithin.exists && cls.owner.is(Trait) then modName.expandedName(cls.owner)
            else EmptyTermName
          NamerOps.findModuleBuddy(modName, denot.owner.info.decls, alternate)
        }
        denot.owner.thisType.select(sourceModule)
      else selfInfo
    val tempInfo = new TempClassInfo(denot.owner.thisType, cls, decls, ost)
    denot.info = tempInfo // first rough info to avoid CyclicReferences
    val parents1 = if (parents.isEmpty) defn.ObjectType :: Nil else parents.map(_.dealias)
    // Adjust parents of the tuple classes and BoxedUnit from the standard library
    // If from Scala 2, adjust for tuple classes; if not, it's from Java, and adjust for BoxedUnit
    val normalizedParents =
      if (fromScala2) defn.adjustForTuple(cls, tparams, parents1)
      else defn.adjustForBoxedUnit(cls, parents1)
    for (tparam <- tparams) {
      val tsym = decls.lookup(tparam.name)
      if (tsym.exists) tsym.setFlag(TypeParam)
      else denot.enter(tparam, decls)
    }
    if (!denot.flagsUNSAFE.isAllOf(JavaModule)) ensureConstructor(cls, denot, decls)

    val scalacCompanion = denot.classSymbol.scalacLinkedClass

    def registerCompanionPair(module: Symbol, claz: Symbol) = {
      module.registerCompanion(claz)
      claz.registerCompanion(module)
    }

    if (denot.flagsUNSAFE.is(Module))
      registerCompanionPair(denot.classSymbol, scalacCompanion)
    else
      registerCompanionPair(scalacCompanion, denot.classSymbol)

    denot.info = tempInfo.finalized(normalizedParents)
    denot.ensureTypeParamsInCorrectOrder()
    defn.patchStdLibClass(denot)
  }
}

/** Unpickle symbol table information descending from a class and/or module root
 *  from an array of bytes.
 *  @param bytes      bytearray from which we unpickle
 *  @param classroot  the top-level class which is unpickled, or NoSymbol if inapplicable
 *  @param moduleroot the top-level module class which is unpickled, or NoSymbol if inapplicable
 *  @param filename   filename associated with bytearray, only used for error messages
 */
class Scala2Unpickler(bytes: Array[Byte], classRoot: ClassDenotation, moduleClassRoot: ClassDenotation)(ictx: Context)
  extends PickleBuffer(bytes, 0, -1) with ClassfileParser.Embedded {

  def showPickled(): Unit =
    atReadPos(0, () => {
      println(s"classRoot = ${classRoot.debugString}, moduleClassRoot = ${moduleClassRoot.debugString}")
      util.ShowPickled.printFile(this)
    })

  // print("unpickling "); showPickled() // !!! DEBUG

  import Scala2Unpickler._

  val moduleRoot: SymDenotation = inContext(ictx) { moduleClassRoot.sourceModule.denot }
  assert(moduleRoot.isTerm)

  checkVersion(using ictx)

  private val loadingMirror = defn(using ictx) // was: mirrorThatLoaded(classRoot)

  /** A map from entry numbers to array offsets */
  private val index = createIndex

  /** A map from entry numbers to symbols, types, or annotations */
  private val entries = new Array[AnyRef](index.length)

  /** A map from symbols to their associated `decls` scopes */
  private val symScopes = mutable.AnyRefMap[Symbol, Scope]()

  /** A mapping from method types to the parameters used in constructing them */
  private val paramsOfMethodType = new java.util.IdentityHashMap[MethodType, List[Symbol]]

  protected def errorBadSignature(msg: String, original: Option[RuntimeException] = None)(using Context): Nothing = {
    val ex = new BadSignature(
      i"""error reading Scala signature of $classRoot from $source:
         |error occurred at position $readIndex: $msg""")
    if (ctx.settings.YdebugMissingRefs.value) original.getOrElse(ex).printStackTrace()
    throw ex
  }

  protected def handleRuntimeException(ex: RuntimeException)(using Context): Nothing = ex match {
    case ex: BadSignature => throw ex
    case _ => errorBadSignature(s"a runtime exception occurred: $ex", Some(ex))
  }

  def run()(using Context): Unit =
    try {
      var i = 0
      while (i < index.length) {
        if (entries(i) == null && isSymbolEntry(i)) {
          val savedIndex = readIndex
          readIndex = index(i)
          val sym = readSymbol()
          if sym.exists then
            entries(i) = sym
            sym.infoOrCompleter match {
              case info: ClassUnpickler => info.init()
              case _ =>
            }
          readIndex = savedIndex
        }
        i += 1
      }
      // read children last, fix for #3951
      i = 0
      while (i < index.length) {
        if (entries(i) == null) {
          if (isSymbolAnnotationEntry(i)) {
            val savedIndex = readIndex
            readIndex = index(i)
            readSymbolAnnotation()
            readIndex = savedIndex
          }
          else if (isChildrenEntry(i)) {
            val savedIndex = readIndex
            readIndex = index(i)
            readChildren()
            readIndex = savedIndex
          }
        }
        i += 1
      }
    }
    catch {
      case ex: RuntimeException => handleRuntimeException(ex)
    }

  def source(using Context): AbstractFile = {
    val f = classRoot.symbol.associatedFile
    if (f != null) f else moduleClassRoot.symbol.associatedFile
  }

  private def checkVersion(using Context): Unit = {
    val major = readNat()
    val minor = readNat()
    if (major != MajorVersion || minor > MinorVersion)
      throw new IOException("Scala signature " + classRoot.fullName +
        " has wrong version\n expected: " +
        MajorVersion + "." + MinorVersion +
        "\n found: " + major + "." + minor +
        " in " + source)
  }

  /** The `decls` scope associated with given symbol */
  protected def symScope(sym: Symbol): Scope = symScopes.getOrElseUpdate(sym, newScope(0))

  /** Does entry represent an (internal) symbol */
  protected def isSymbolEntry(i: Int)(using Context): Boolean = {
    val tag = bytes(index(i)).toInt
    (firstSymTag <= tag && tag <= lastSymTag &&
      (tag != CLASSsym || !isRefinementSymbolEntry(i)))
  }

  /** Does entry represent an (internal or external) symbol */
  protected def isSymbolRef(i: Int): Boolean = {
    val tag = bytes(index(i))
    (firstSymTag <= tag && tag <= lastExtSymTag)
  }

  /** Does entry represent a name? */
  protected def isNameEntry(i: Int): Boolean = {
    val tag = bytes(index(i)).toInt
    tag == TERMname || tag == TYPEname
  }

  /** Does entry represent a symbol annotation? */
  protected def isSymbolAnnotationEntry(i: Int): Boolean = {
    val tag = bytes(index(i)).toInt
    tag == SYMANNOT
  }

  /** Does the entry represent children of a symbol? */
  protected def isChildrenEntry(i: Int): Boolean = {
    val tag = bytes(index(i)).toInt
    tag == CHILDREN
  }

  /** Does entry represent a refinement symbol?
   *  pre: Entry is a class symbol
   */
  protected def isRefinementSymbolEntry(i: Int)(using Context): Boolean = {
    val savedIndex = readIndex
    readIndex = index(i)
    val tag = readByte().toInt
    assert(tag == CLASSsym)

    readNat(); // read length
    val result = readNameRef() == tpnme.REFINE_CLASS
    readIndex = savedIndex
    result
  }

  protected def isRefinementClass(sym: Symbol)(using Context): Boolean =
    sym.name == tpnme.REFINE_CLASS

  protected def isLocal(sym: Symbol)(using Context): Boolean = isUnpickleRoot(sym.topLevelClass)

  protected def isUnpickleRoot(sym: Symbol)(using Context): Boolean = {
    val d = sym.denot
    d == moduleRoot || d == moduleClassRoot || d == classRoot
  }

  /** If entry at <code>i</code> is undefined, define it by performing
   *  operation <code>op</code> with <code>readIndex at start of i'th
   *  entry. Restore <code>readIndex</code> afterwards.
   */
  protected def at[T <: AnyRef](i: Int, op: () => T): T = {
    var r = entries(i)
    if (r eq null) {
      r = atReadPos(index(i), op)
      assert(entries(i) eq null, entries(i))
      entries(i) = r
    }
    r.asInstanceOf[T]
  }

  protected def atReadPos[T](start: Int, op: () => T): T = {
    val savedIndex = readIndex
    readIndex = start
    try op()
    finally readIndex = savedIndex
  }

  /** Read a name */
  protected def readName()(using Context): Name = {
    val tag = readByte()
    val len = readNat()
    tag match {
      case TERMname => termName(bytes, readIndex, len)
      case TYPEname => typeName(bytes, readIndex, len)
      case _ => errorBadSignature("bad name tag: " + tag)
    }
  }
  protected def readTermName()(using Context): TermName = readName().toTermName
  protected def readTypeName()(using Context): TypeName = readName().toTypeName

  /** Read a symbol */
  protected def readSymbol()(using Context): Symbol = readDisambiguatedSymbol(alwaysTrue)()

  /** Read a symbol, with possible disambiguation */
  protected def readDisambiguatedSymbol(p: Symbol => Boolean)()(using Context): Symbol = {
    val start = indexCoord(readIndex)
    val tag = readByte()
    val end = readNat() + readIndex
    def atEnd = readIndex == end

    def readExtSymbol(): Symbol = {
      val nameRef = readNameRef()
      var name = nameRef.decode

      // If the symbol tag is EXTMODCLASSref, then we know that the method names
      // mangling do not make sense, but in general we don't know what kind of
      // symbol we're reading at this point, so we don't know which unmanglings
      // are safe to apply. Empirically, we at least need to unmangle default
      // getter names, since they're used to encode the default parameters of
      // annotations, but more might be needed.
      if (tag != EXTMODCLASSref)
        name = name.unmangle(Scala2MethodNameKinds)

      val owner = if (atEnd) loadingMirror.RootClass else readSymbolRef()

      def adjust(denot: Denotation) = {
        val denot1 = denot.disambiguate(p)
        val sym = denot1.symbol
        if (denot.exists && !denot1.exists) { // !!!DEBUG
          val alts = denot.alternatives map (d => s"$d:${d.info}/${d.signature}")
          System.err.println(s"!!! disambiguation failure: $alts")
          val members = denot.alternatives.head.symbol.owner.info.decls.toList map (d => s"$d:${d.info}/${d.signature}")
          System.err.println(s"!!! all members: $members")
        }
        if (tag == EXTref) sym else sym.moduleClass
      }

      def fromName(name: Name): Symbol = name.toTermName match {
        case nme.ROOT => loadingMirror.RootClass
        case nme.ROOTPKG => loadingMirror.RootPackage
        case _ =>
          def declIn(owner: Symbol) = adjust(owner.info.decl(name))
          val sym = declIn(owner)
          if (sym.exists || owner.ne(defn.ObjectClass)) sym else declIn(defn.AnyClass)
      }

      def slowSearch(name: Name): Symbol =
        owner.info.decls.find(_.name == name)

      def nestedObjectSymbol: Symbol =
        // If the owner is overloaded (i.e. a method), it's not possible to select the
        // right member, so return NoSymbol. This can only happen when unpickling a tree.
        // the "case Apply" in readTree() takes care of selecting the correct alternative
        //  after parsing the arguments.
        //if (owner.isOverloaded)
        //  return NoSymbol

        if (tag == EXTMODCLASSref) {
          val module = owner.info.decl(name.toTermName).suchThat(_.is(Module))
          module.info // force it, as completer does not yet point to module class.
          module.symbol.moduleClass
        }

          /* was:
            val moduleVar = owner.info.decl(name.toTermName.moduleVarName).symbol
            if (moduleVar.isLazyAccessor)
              return moduleVar.lazyAccessor.lazyAccessor
           */
        else NoSymbol

      // println(s"read ext symbol $name from ${owner.denot.debugString} in ${classRoot.debugString}")  // !!! DEBUG

      // (1) Try name.
      fromName(name) orElse {
        // (2) Try with expanded name.  Can happen if references to private
        // symbols are read from outside: for instance when checking the children
        // of a class.  See #1722.
        fromName(name.toTermName.expandedName(owner)) orElse {
          // (3) Try as a nested object symbol.
          nestedObjectSymbol orElse {
            // (4) Call the mirror's "missing" hook.
            adjust(missingHook(owner, name)) orElse {
              // println(owner.info.decls.toList.map(_.debugString).mkString("\n  ")) // !!! DEBUG
              //              }
              // (5) Create a stub symbol to defer hard failure a little longer.
              System.err.println(i"***** missing reference, looking for ${name.debugString} in $owner")
              System.err.println(i"decls = ${owner.info.decls}")
              owner.info.decls.checkConsistent()
              if (slowSearch(name).exists)
                System.err.println(i"**** slow search found: ${slowSearch(name)}")
              if (ctx.settings.YdebugMissingRefs.value) Thread.dumpStack()
              newStubSymbol(owner, name, source)
            }
          }
        }
      }
    }

    tag match {
      case NONEsym => return NoSymbol
      case EXTref | EXTMODCLASSref => return readExtSymbol()
      case _ =>
    }

    // symbols that were pickled with Pickler.writeSymInfo
    val nameref = readNat()
    var name = at(nameref, () => readName()(using ctx))
    val owner = readSymbolRef()

    var flags = unpickleScalaFlags(readLongNat(), name.isTypeName)

    if (name eq nme.getClass_) && defn.hasProblematicGetClass(owner.name)
       // Scala 2 sometimes pickle the same type parameter symbol multiple times
       // (see i11173 for an example), but we should only unpickle it once.
       || tag == TYPEsym && flags.is(TypeParam) && symScope(owner).lookup(name.asTypeName).exists
    then
      // skip this member
      return NoSymbol

    name = name.adjustIfModuleClass(flags)
    if (flags.is(Method))
      name =
        if (name == nme.TRAIT_CONSTRUCTOR) nme.CONSTRUCTOR
        else name.asTermName.unmangle(Scala2MethodNameKinds)
    if ((flags.is(Scala2ExpandedName))) {
      name = name.unmangle(ExpandedName)
      flags = flags &~ Scala2ExpandedName
    }
    if (flags.is(Scala2SuperAccessor)) {
      /* Scala 2 super accessors are pickled as private, but are compiled as public expanded.
       * Dotty super accessors, however, are already pickled as public expanded.
       * We bridge the gap right now.
       */
      name = name.asTermName.unmangle(SuperAccessorName).expandedName(owner)
      flags = flags &~ (Scala2SuperAccessor | Private)
    }
    name = name.mapLast(_.decode)

    def nameMatches(rootName: Name) = name == rootName
    def isClassRoot = nameMatches(classRoot.name) && (owner == classRoot.owner) && !flags.is(ModuleClass)
    def isModuleClassRoot = nameMatches(moduleClassRoot.name) && (owner == moduleClassRoot.owner) && flags.is(Module)
    def isModuleRoot = nameMatches(moduleClassRoot.name.sourceModuleName) && (owner == moduleClassRoot.owner) && flags.is(Module)

    //if (isClassRoot) println(s"classRoot of $classRoot found at $readIndex, flags = ${flags.flagsString}") // !!! DEBUG
    //if (isModuleRoot) println(s"moduleRoot of $moduleRoot found at $readIndex, flags = ${flags.flagsString}") // !!! DEBUG
    //if (isModuleClassRoot) println(s"moduleClassRoot of $moduleClassRoot found at $readIndex, flags = ${flags.flagsString}") // !!! DEBUG

    def completeRoot(denot: ClassDenotation, completer: LazyType, privateWithin: Symbol): Symbol = {
      denot.setFlag(flags)
      denot.resetFlag(Touched) // allow one more completion

      // Temporary measure, as long as we do not recompile these traits with Scala 3.
      // Scala 2 is more aggressive when it comes to defining a $init$ method than Scala 3.
      // Any concrete definition, even a `def`, causes a trait to receive a $init$ method
      // to be created, even if it does not do anything, and hence causes not have the NoInits flag.
      // We override this for Product so that cases classes can be pure.
      // A full solution requires that we compile Product with Scala 3 in the future.
      if (owner == defn.ScalaPackageClass && (name eq tpnme.Product))
        denot.setFlag(NoInits)

      denot.setPrivateWithin(privateWithin)
      denot.info = completer
      denot.symbol
    }

    def finishSym(sym: Symbol): Symbol = {
      if (sym.isClass)
        sym.setFlag(Scala2x)
      if (!(isRefinementClass(sym) || isUnpickleRoot(sym) || sym.is(Scala2Existential))) {
        val owner = sym.owner
        val canEnter =
          owner.isClass &&
          (!sym.is(TypeParam) ||
            owner.infoOrCompleter.match
              case completer: ClassUnpickler =>
                // Type parameters seen after class initialization are not
                // actually type parameters of the current class but of some
                // external class because of the bizarre way in which Scala 2
                // pickles them (see
                // https://github.com/scala/scala/blob/aa31e3e6bb945f5d69740d379ede1cd514904109/src/compiler/scala/tools/nsc/symtab/classfile/Pickler.scala#L181-L197).
                // Make sure we don't enter them in the class otherwise the
                // compiler will get very confused (testcase in sbt-test/scala2-compat/i12641).
                // Note: I don't actually know if these stray type parameters
                // can also show up before initialization, if that's the case
                // we'll need to study more closely how Scala 2 handles type
                // parameter unpickling and try to emulate it.
                !completer.areParamsInitialized
              case _ =>
                true)

        if (canEnter)
          owner.asClass.enter(sym, symScope(owner))
      }
      sym
    }

    val (privateWithin, infoRef) = {
      val ref = readNat()
      if (!isSymbolRef(ref))
        (NoSymbol, ref)
      else {
        val pw = at(ref, () => readSymbol())
        (pw, readNat())
      }
    }

    finishSym(tag match {
      case TYPEsym | ALIASsym =>
        var name1 = name.asTypeName
        var flags1 = flags
        if (flags.is(TypeParam)) flags1 |= owner.typeParamCreationFlags
        newSymbol(owner, name1, flags1, localMemberUnpickler, privateWithin, coord = start)
      case CLASSsym =>
        if (isClassRoot)
          completeRoot(
            classRoot, rootClassUnpickler(start, classRoot.symbol, NoSymbol, infoRef), privateWithin)
        else if (isModuleClassRoot)
          completeRoot(
            moduleClassRoot, rootClassUnpickler(start, moduleClassRoot.symbol, moduleClassRoot.sourceModule, infoRef), privateWithin)
        else {
          def completer(cls: Symbol) = {
            val unpickler = new ClassUnpickler(infoRef) withDecls symScope(cls)
            if (flags.is(ModuleClass))
              unpickler.withSourceModule(
                cls.owner.info.decls.lookup(cls.name.sourceModuleName)
                  .suchThat(_.is(Module)).symbol)
            else unpickler
          }
          newClassSymbol(owner, name.asTypeName, flags, completer, privateWithin, coord = start)
        }
      case VALsym =>
        newSymbol(owner, name.asTermName, flags, localMemberUnpickler, privateWithin, coord = start)
      case MODULEsym =>
        if (isModuleRoot) {
          moduleRoot setFlag flags
          moduleRoot.symbol
        } else newSymbol(owner, name.asTermName, flags,
          new LocalUnpickler().withModuleClass(
            owner.info.decls.lookup(name.moduleClassName)
              .suchThat(_.is(Module)).symbol)
          , privateWithin, coord = start)
      case _ =>
        errorBadSignature("bad symbol tag: " + tag)
    })
  }

  class LocalUnpickler extends LazyType {
    def startCoord(denot: SymDenotation): Coord = denot.symbol.coord

    def paramssOfType(tp: Type): List[List[Symbol]] = tp match
      case TempPolyType(tparams, restpe) => tparams :: paramssOfType(restpe)
      case mt: MethodType =>
        val params = paramsOfMethodType.remove(mt)
        val rest = paramssOfType(mt.resType)
        if params == null then rest else params :: rest
      case _ => Nil

    def complete(denot: SymDenotation)(using Context): Unit = try {
      def parseToCompletion(denot: SymDenotation)(using Context) = {
        val tag = readByte()
        val end = readNat() + readIndex
        def atEnd = readIndex == end
        val unusedNameref = readNat()
        val unusedOwnerref = readNat()
        val unusedFlags = readLongNat()

        var inforef = readNat()
        if (isSymbolRef(inforef)) inforef = readNat()

        // println("reading type for " + denot) // !!! DEBUG
        val tp = at(inforef, () => readType()(using ctx))
        if denot.is(Method) then
          var params = paramssOfType(tp)
          if denot.isConstructor && denot.owner.typeParams.nonEmpty then
            params = denot.owner.typeParams :: params
          denot.rawParamss = params

        denot match {
          case denot: ClassDenotation if !isRefinementClass(denot.symbol) =>
            val selfInfo = if (atEnd) NoType else readTypeRef()
            setClassInfo(denot, tp, fromScala2 = true, selfInfo)
            NamerOps.addConstructorProxies(denot.classSymbol)
          case denot =>
            val tp1 = translateTempPoly(tp)
            denot.info =
              if (tag == ALIASsym) TypeAlias(tp1)
              else if (denot.isType) checkNonCyclic(denot.symbol, tp1, reportErrors = false)
                // we need the checkNonCyclic call to insert LazyRefs for F-bounded cycles
              else if (!denot.is(Param)) tp1.translateFromRepeated(toArray = false)
              else tp1
            if (denot.isConstructor)
              denot.owner.setStableConstructor()
              addConstructorTypeParams(denot)
            if (atEnd)
              assert(!denot.symbol.isSuperAccessor, denot)
            else {
              assert(denot.is(ParamAccessor) || denot.symbol.isSuperAccessor, denot)
              def disambiguate(alt: Symbol) = // !!! DEBUG
                trace.onDebug(s"disambiguating ${denot.info} =:= ${denot.owner.thisType.memberInfo(alt)} ${denot.owner}") {
                  denot.info matches denot.owner.thisType.memberInfo(alt)
                }
              val alias = readDisambiguatedSymbolRef(disambiguate).asTerm
              if alias.name == denot.name then denot.setFlag(SuperParamAlias)
            }
        }
        // println(s"unpickled ${denot.debugString}, info = ${denot.info}") !!! DEBUG
      }
      atReadPos(startCoord(denot).toIndex,
          () => withMode(Mode.Scala2Unpickling) {
            atPhaseBeforeTransforms {
              parseToCompletion(denot)
            }
          })
    }
    catch {
      case ex: RuntimeException => handleRuntimeException(ex)
    }
  }

  object localMemberUnpickler extends LocalUnpickler

  class ClassUnpickler(infoRef: Int) extends LocalUnpickler with TypeParamsCompleter {
    private var myTypeParams: List[TypeSymbol] = null

    private def readTypeParams()(using Context): Unit = {
      val tag = readByte()
      val end = readNat() + readIndex
      myTypeParams =
        if (tag == POLYtpe) {
          val unusedRestpeRef = readNat()
          until(end, () => readSymbolRef()(using ctx)).asInstanceOf[List[TypeSymbol]]
        } else Nil
    }
    private def loadTypeParams()(using Context) =
      atReadPos(index(infoRef), () => readTypeParams()(using ctx))

    /** Have the type params of this class already been unpickled? */
    def areParamsInitialized: Boolean = myTypeParams ne null

    /** Force reading type params early, we need them in setClassInfo of subclasses. */
    def init()(using Context): List[TypeSymbol] =
      if !areParamsInitialized then loadTypeParams()
      myTypeParams

    override def completerTypeParams(sym: Symbol)(using Context): List[TypeSymbol] =
      init()
  }

  def rootClassUnpickler(start: Coord, cls: Symbol, module: Symbol, infoRef: Int): ClassUnpickler =
    (new ClassUnpickler(infoRef) with SymbolLoaders.SecondCompleter {
      override def startCoord(denot: SymDenotation): Coord = start
    }).withDecls(symScope(cls)).withSourceModule(module)

  /** Convert
   *    tp { type name = sym } forSome { sym >: L <: H }
   *  to
   *    tp { name >: L <: H }
   *  and
   *    tp { name: sym } forSome { sym <: T with Singleton }
   *  to
   *    tp { name: T }
   */
  def elimExistentials(boundSyms: List[Symbol], tp: Type)(using Context): Type = {
    // Need to be careful not to run into cyclic references here (observed when
    // compiling t247.scala). That's why we avoid taking `symbol` of a TypeRef
    // unless names match up.
    val isBound = { (tp: Type) =>
      def refersTo(tp: Type, sym: Symbol): Boolean = tp match {
        case tp: TypeRef => sym.name == tp.name && sym == tp.symbol
        case tp: TypeVar => refersTo(tp.underlying, sym)
        case tp : LazyRef => refersTo(tp.ref, sym)
        case _ => false
      }
      boundSyms.exists(refersTo(tp, _))
    }
    // Cannot use standard `existsPart` method because it calls `lookupRefined`
    // which can cause CyclicReference errors.
    val isBoundAccumulator = new ExistsAccumulator(isBound, StopAt.Static, forceLazy = true):
      override def apply(x: Boolean, tp: Type): Boolean = tp match
        case tp: TypeRef => applyToPrefix(x, tp)
        case _ => super.foldOver(x, tp)

    def removeSingleton(tp: Type): Type =
      if (tp isRef defn.SingletonClass) defn.AnyType else tp
    def mapArg(arg: Type) = arg match {
      case arg: TypeRef if isBound(arg) => arg.symbol.info
      case _ => arg
    }
    def elim(tp: Type): Type = tp match {
      case tp @ RefinedType(parent, name, rinfo) =>
        val parent1 = elim(tp.parent)
        rinfo match {
          case TypeAlias(info: TypeRef) if isBound(info) =>
            RefinedType(parent1, name, info.symbol.info)
          case info: TypeRef if isBound(info) =>
            val info1 = info.symbol.info
            assert(info1.derivesFrom(defn.SingletonClass))
            RefinedType(parent1, name, info1.mapReduceAnd(removeSingleton)(_ & _))
          case info =>
            tp.derivedRefinedType(parent1, name, info)
        }
      case tp @ AppliedType(tycon, args) =>
        val tycon1 = tycon.safeDealias
        if (tycon1 ne tycon) elim(tycon1.appliedTo(args))
        else tp.derivedAppliedType(tycon, args.map(mapArg))
      case tp: AndOrType =>
        // scalajs.js.|.UnionOps has a type parameter upper-bounded by `_ | _`
        tp.derivedAndOrType(mapArg(tp.tp1).bounds.hi, mapArg(tp.tp2).bounds.hi)
      case _ =>
        tp
    }
    val tp1 = elim(tp)
    if (isBoundAccumulator(false, tp1)) {
      val anyTypes = boundSyms map (_ => defn.AnyType)
      val boundBounds = boundSyms map (_.info.bounds.hi)
      val tp2 = tp1.subst(boundSyms, boundBounds).subst(boundSyms, anyTypes)
      report.warning(FailureToEliminateExistential(tp, tp1, tp2, boundSyms, classRoot.symbol), NoSourcePosition)
      tp2
    }
    else tp1
  }

  /** Read type ref, mapping a TypeRef to a package to the package's ThisType
   *  Package references should be TermRefs or ThisTypes but it was observed that
   *  nsc sometimes pickles them as TypeRefs instead.
   */
  private def readPrefix()(using Context): Type = readTypeRef() match {
    case pre: TypeRef if pre.symbol.is(Package) => pre.symbol.thisType
    case pre => pre
  }

  /** Read a type
   *
   *  @param forceProperType is used to ease the transition to NullaryMethodTypes (commentmarker: NMT_TRANSITION)
   *        the flag say that a type of kind * is expected, so that PolyType(tps, restpe) can be disambiguated to PolyType(tps, NullaryMethodType(restpe))
   *        (if restpe is not a ClassInfoType, a MethodType or a NullaryMethodType, which leaves TypeRef/SingletonType -- the latter would make the polytype a type constructor)
   */
  protected def readType()(using Context): Type = {
    def select(pre: Type, sym: Symbol): Type =
      // structural members need to be selected by name, their symbols are only
      // valid in the synthetic refinement class that defines them.
      if !pre.isInstanceOf[ThisType] && isRefinementClass(sym.owner) then pre.select(sym.name) else pre.select(sym)

    val tag = readByte()
    val end = readNat() + readIndex
    (tag: @switch) match {
      case NOtpe =>
        NoType
      case NOPREFIXtpe =>
        NoPrefix
      case THIStpe =>
        readSymbolRef().thisType
      case SINGLEtpe =>
        val pre = readPrefix()
        val sym = readDisambiguatedSymbolRef(_.info.isParameterless)
        select(pre, sym)
      case SUPERtpe =>
        val thistpe = readTypeRef()
        val supertpe = readTypeRef()
        SuperType(thistpe, supertpe)
      case CONSTANTtpe =>
        readConstantRef() match
          case c: Constant => ConstantType(c)
          case tp: TermRef => tp
      case TYPEREFtpe =>
        var pre = readPrefix()
        val sym = readSymbolRef()
        pre match {
          case thispre: ThisType =>
            // The problem is that class references super.C get pickled as
            // this.C. Dereferencing the member might then get an overriding class
            // instance. The problem arises for instance for LinkedHashMap#MapValues
            // and also for the inner Transform class in all views. We fix it by
            // replacing the this with the appropriate super.
            if (sym.owner != thispre.cls) {
              val overriding = thispre.cls.info.decls.lookup(sym.name)
              if (overriding.exists && overriding != sym) {
                val base = pre.baseType(sym.owner)
                assert(base.exists)
                pre = SuperType(thispre, base)
              }
            }
          case NoPrefix if sym.is(TypeParam) =>
            pre = sym.owner.thisType
          case _ =>
        }
        val tycon = select(pre, sym)
        val args = until(end, () => readTypeRef())
        if (sym == defn.ByNameParamClass2x) ExprType(args.head.adaptByNameArgUnderPureFuns)
        else if (ctx.settings.scalajs.value && args.length == 2 &&
            sym.owner == JSDefinitions.jsdefn.ScalaJSJSPackageClass && sym == JSDefinitions.jsdefn.PseudoUnionClass) {
          // Treat Scala.js pseudo-unions as real unions, this requires a
          // special-case in erasure, see TypeErasure#eraseInfo.
          OrType(args(0), args(1), soft = false)
        }
        else if args.nonEmpty then
          tycon.safeAppliedTo(EtaExpandIfHK(sym.typeParams, args.map(translateTempPoly)))
            .adaptFunctionTypeUnderPureFuns
        else if (sym.typeParams.nonEmpty) tycon.EtaExpand(sym.typeParams)
        else tycon
      case TYPEBOUNDStpe =>
        val lo = readTypeRef()
        val hi = readTypeRef()
        createNullableTypeBounds(lo, hi)
      case REFINEDtpe =>
        val clazz = readSymbolRef().asClass
        val decls = symScope(clazz)
        symScopes(clazz) = EmptyScope // prevent further additions
        val parents = until(end, () => readTypeRef())
        val parent = parents.reduceLeft(AndType(_, _))
        if (decls.isEmpty) parent
        else {
          def subst(info: Type, rt: RecType) = info.substThis(clazz.asClass, rt.recThis)
          def addRefinement(tp: Type, sym: Symbol) = RefinedType(tp, sym.name, sym.info)
          val refined = decls.toList.foldLeft(parent)(addRefinement)
          RecType.closeOver(rt => refined.substThis(clazz, rt.recThis))
        }
      case CLASSINFOtpe =>
        val clazz = readSymbolRef()
        TempClassInfoType(until(end, () => readTypeRef()), symScope(clazz), clazz)
      case METHODtpe | IMPLICITMETHODtpe =>
        val restpe = readTypeRef()
        val params = until(end, () => readSymbolRef())
        val maker = MethodType.companion(
          isImplicit = tag == IMPLICITMETHODtpe || params.nonEmpty && params.head.is(Implicit))
        val result = maker.fromSymbols(params, restpe)
        result.resType match
          case restpe1: MethodType if restpe1 ne restpe =>
            val prevResParams = paramsOfMethodType.remove(restpe)
            if prevResParams != null then
              paramsOfMethodType.put(restpe1, prevResParams)
          case _ =>
        if params.nonEmpty then paramsOfMethodType.put(result, params)
        result
      case POLYtpe =>
        val restpe = readTypeRef()
        val typeParams = until(end, () => readSymbolRef())
        if typeParams.nonEmpty then
          TempPolyType(typeParams.asInstanceOf[List[TypeSymbol]], restpe.widenExpr)
        else
          ExprType(restpe)
      case EXISTENTIALtpe =>
        val restpe = readTypeRef()
        val boundSyms = until(end, () => readSymbolRef())
        elimExistentials(boundSyms, restpe)
      case ANNOTATEDtpe =>
        AnnotatedType.make(readTypeRef(), until(end, () => readAnnotationRef()))
      case _ =>
        noSuchTypeTag(tag, end)
    }
  }

  def readTypeParams()(using Context): List[Symbol] = {
    val tag = readByte()
    val end = readNat() + readIndex
    if (tag == POLYtpe) {
      val unusedRestperef = readNat()
      until(end, () => readSymbolRef())
    }
    else Nil
  }

  def noSuchTypeTag(tag: Int, end: Int)(using Context): Type =
    errorBadSignature("bad type tag: " + tag)

  /** Read a constant */
  protected def readConstant()(using Context): Constant | TermRef = {
    val tag = readByte().toInt
    val len = readNat()
    (tag: @switch) match {
      case LITERALunit => Constant(())
      case LITERALboolean => Constant(readLong(len) != 0L)
      case LITERALbyte => Constant(readLong(len).toByte)
      case LITERALshort => Constant(readLong(len).toShort)
      case LITERALchar => Constant(readLong(len).toChar)
      case LITERALint => Constant(readLong(len).toInt)
      case LITERALlong => Constant(readLong(len))
      case LITERALfloat => Constant(intBitsToFloat(readLong(len).toInt))
      case LITERALdouble => Constant(longBitsToDouble(readLong(len)))
      case LITERALstring => Constant(readNameRef().toString)
      case LITERALnull => Constant(null)
      case LITERALclass => Constant(readTypeRef())
      case LITERALenum => readSymbolRef().termRef
      case _ => noSuchConstantTag(tag, len)
    }
  }

  def noSuchConstantTag(tag: Int, len: Int)(using Context): Constant =
    errorBadSignature("bad constant tag: " + tag)

  /** Read children and store them into the corresponding symbol.
   */
  protected def readChildren()(using Context): Unit = {
    val tag = readByte()
    assert(tag == CHILDREN)
    val end = readNat() + readIndex
    val target = readSymbolRef()
    while (readIndex != end) {
      val start = readIndex
      readNat() // skip reference for now
      target.addAnnotation(
          Annotation.Child.later(atReadPos(start, () => readSymbolRef()), NoSpan))
    }
  }

  /* Read a reference to a pickled item */
  protected def readSymbolRef()(using Context): Symbol = { //OPT inlined from: at(readNat(), readSymbol) to save on closure creation
    val i = readNat()
    var r = entries(i)
    if (r eq null) {
      val savedIndex = readIndex
      readIndex = index(i)
      r = readSymbol()
      assert(entries(i) eq null, entries(i))
      entries(i) = r
      readIndex = savedIndex
    }
    r.asInstanceOf[Symbol]
  }

  protected def readDisambiguatedSymbolRef(p: Symbol => Boolean)(using Context): Symbol =
    at(readNat(), () => readDisambiguatedSymbol(p)())

  protected def readNameRef()(using Context): Name = at(readNat(), () => readName())
  protected def readTypeRef()(using Context): Type = at(readNat(), () => readType()) // after the NMT_TRANSITION period, we can leave off the () => ... ()
  protected def readConstantRef()(using Context): Constant | TermRef = at(readNat(), () => readConstant())

  protected def readTypeNameRef()(using Context): TypeName = readNameRef().toTypeName
  protected def readTermNameRef()(using Context): TermName = readNameRef().toTermName

  protected def readAnnotationRef()(using Context): Annotation = at(readNat(), () => readAnnotation())

  protected def readModifiersRef(isType: Boolean)(using Context): Modifiers = at(readNat(), () => readModifiers(isType))
  protected def readTreeRef()(using Context): Tree = at(readNat(), () => readTree())

  /** Read an annotation argument, which is pickled either
   *  as a Constant or a Tree.
   */
  protected def readAnnotArg(i: Int)(using Context): untpd.Tree = untpd.TypedSplice(bytes(index(i)) match
    case TREE => at(i, () => readTree())
    case _ => at(i, () =>
      readConstant() match
        case c: Constant => Literal(c)
        case tp: TermRef => ref(tp)
    )
  )

  /** Read a ClassfileAnnotArg (argument to a classfile annotation)
   */
  private def readArrayAnnotArg()(using Context): untpd.Tree = {
    readByte() // skip the `annotargarray` tag
    val end = readNat() + readIndex
    // array elements are trees representing instances of scala.annotation.Annotation
    untpd.JavaSeqLiteral(
      until(end, () => readClassfileAnnotArg(readNat())),
      untpd.TypeTree())
  }

  private def readAnnotInfoArg()(using Context): untpd.Tree = untpd.TypedSplice {
    readByte() // skip the `annotinfo` tag
    val end = readNat() + readIndex
    readAnnotationContents(end)
  }

  protected def readClassfileAnnotArg(i: Int)(using Context): untpd.Tree = bytes(index(i)) match {
    case ANNOTINFO => at(i, () => readAnnotInfoArg())
    case ANNOTARGARRAY => at(i, () => readArrayAnnotArg())
    case _ => readAnnotArg(i)
  }

  /** Read an annotation's contents. Not to be called directly, use
   *  readAnnotation, readSymbolAnnotation, or readAnnotInfoArg
   */
  protected def readAnnotationContents(end: Int)(using Context): Tree = {
    val atp = readTypeRef()
    val args = {
      val t = new ListBuffer[untpd.Tree]

      while (readIndex != end) {
        val argref = readNat()
        t += {
          if (isNameEntry(argref)) {
            val name = at(argref, () => readName())
            val arg = readClassfileAnnotArg(readNat())
            untpd.NamedArg(name.asTermName, arg)
          }
          else readAnnotArg(argref)
        }
      }
      t.toList
    }
    untpd.resolveConstructor(atp, args)
  }

  /** Read an annotation and as a side effect store it into
   *  the symbol it requests. Called at top-level, for all
   *  (symbol, annotInfo) entries.
   */
  protected def readSymbolAnnotation()(using Context): Unit = {
    val tag = readByte()
    if (tag != SYMANNOT)
      errorBadSignature("symbol annotation expected (" + tag + ")")
    val end = readNat() + readIndex
    val target = readSymbolRef()
    target.addAnnotation(deferredAnnot(end))
  }

  /** Read an annotation and return it. Used when unpickling
   *  an ANNOTATED(WSELF)tpe or a NestedAnnotArg
   */
  protected def readAnnotation()(using Context): Annotation = {
    val tag = readByte()
    if (tag != ANNOTINFO)
      errorBadSignature("annotation expected (" + tag + ")")
    val end = readNat() + readIndex
    deferredAnnot(end)
  }

  /** A deferred annotation that can be completed by reading
   *  the bytes between `readIndex` and `end`.
   */
  protected def deferredAnnot(end: Int)(using Context): Annotation = {
    val start = readIndex
    val phase = ctx.phase
    Annotation.deferredSymAndTree(
        atReadPos(start, () => atPhase(phase)(readTypeRef().typeSymbol)))(
        atReadPos(start, () => atPhase(phase)(readAnnotationContents(end))))
  }

  /* Read an abstract syntax tree */
  protected def readTree()(using Context): Tree = {
    val outerTag = readByte()
    if (outerTag != TREE)
      errorBadSignature("tree expected (" + outerTag + ")")
    val end = readNat() + readIndex
    val tag = readByte()
    val tpe = if (tag == EMPTYtree) NoType else readTypeRef()

    // Set by the three functions to follow.  If symbol is non-null
    // after the new tree 't' has been created, t has its Symbol
    // set to symbol; and it always has its Type set to tpe.
    var symbol: Symbol = null
    var mods: Modifiers = null
    var name: Name = null

    /** Read a Symbol, Modifiers, and a Name */
    def setSymModsName(): Unit = {
      symbol = readSymbolRef()
      mods = readModifiersRef(symbol.isType)
      name = readNameRef()
    }
    /** Read a Symbol and a Name */
    def setSymName(): Unit = {
      symbol = readSymbolRef()
      name = readNameRef()
    }
    /** Read a Symbol */
    def setSym(): Unit =
      symbol = readSymbolRef()

    implicit val span: Span = NoSpan

    tag match {
      case EMPTYtree =>
        EmptyTree

      case PACKAGEtree =>
        setSym()
        val pid = readTreeRef().asInstanceOf[RefTree]
        val stats = until(end, () => readTreeRef())
        PackageDef(pid, stats)

      case CLASStree =>
        setSymModsName()
        val impl = readTemplateRef()
        val tparams = until(end, () => readTypeDefRef())
        val cls = symbol.asClass
        val ((constr: DefDef) :: Nil, stats) =
          impl.body.partition(_.symbol == cls.primaryConstructor): @unchecked
        ClassDef(cls, constr, tparams ++ stats)

      case MODULEtree =>
        setSymModsName()
        ModuleDef(symbol.asTerm, readTemplateRef().body)

      case VALDEFtree =>
        setSymModsName()
        val tpt = readTreeRef()
        val rhs = readTreeRef()
        ValDef(symbol.asTerm, rhs)

      case DEFDEFtree =>
        setSymModsName()
        val tparams = times(readNat(), () => readTypeDefRef())
        val vparamss = times(readNat(), () => times(readNat(), () => readValDefRef()))
        val tpt = readTreeRef()
        val rhs = readTreeRef()
        DefDef(symbol.asTerm, rhs)

      case TYPEDEFtree =>
        setSymModsName()
        val rhs = readTreeRef()
        val tparams = until(end, () => readTypeDefRef())
        TypeDef(symbol.asType)

      case LABELtree =>
        ???
        setSymName()
        val rhs = readTreeRef()
        val params = until(end, () => readIdentRef())
        val ldef = DefDef(symbol.asTerm, rhs)
        def isCaseLabel(sym: Symbol) = sym.name.startsWith(nme.CASEkw.toString)
        if (isCaseLabel(symbol)) ldef
        else Block(ldef :: Nil, Apply(Ident(symbol.termRef), Nil))

      case IMPORTtree =>
        setSym()
        val expr = readTreeRef()
        val selectors = until(end, () => {
          val fromName = readNameRef()
          val toName = readNameRef()
          untpd.ImportSelector(
            untpd.Ident(fromName),
            if toName.isEmpty then EmptyTree else untpd.Ident(toName))
        })
        Import(expr, selectors)

      case TEMPLATEtree =>
        setSym()
        val parents = times(readNat(), () => readTreeRef())
        val self = readValDefRef()
        val body = until(end, () => readTreeRef())
        untpd.Template(???, parents, Nil, self, body) // !!! TODO: pull out primary constructor
          .withType(symbol.namedType)

      case BLOCKtree =>
        val expr = readTreeRef()
        val stats = until(end, () => readTreeRef())
        Block(stats, expr)

      case CASEtree =>
        val pat = readTreeRef()
        val guard = readTreeRef()
        val body = readTreeRef()
        CaseDef(pat, guard, body)

      case ALTERNATIVEtree =>
        Alternative(until(end, () => readTreeRef()))

      case STARtree =>
        readTreeRef()
        unimplementedTree("STAR")

      case BINDtree =>
        setSymName()
        Bind(symbol.asTerm, readTreeRef())

      case UNAPPLYtree =>
        val fun = readTreeRef()
        val args = until(end, () => readTreeRef())
        UnApply(fun, Nil, args, defn.AnyType) // !!! this is wrong in general

      case ARRAYVALUEtree =>
        val elemtpt = readTreeRef()
        val trees = until(end, () => readTreeRef())
        SeqLiteral(trees, elemtpt)
          // note can't deal with trees passed to Java methods as arrays here

      case FUNCTIONtree =>
        setSym()
        val body = readTreeRef()
        val vparams = until(end, () => readValDefRef())
        val applyType = MethodType(vparams map (_.name), vparams map (_.tpt.tpe), body.tpe)
        val applyMeth = newSymbol(symbol.owner, nme.apply, Method, applyType)
        Closure(applyMeth, Function.const(body.changeOwner(symbol, applyMeth)) _)

      case ASSIGNtree =>
        val lhs = readTreeRef()
        val rhs = readTreeRef()
        Assign(lhs, rhs)

      case IFtree =>
        val cond = readTreeRef()
        val thenp = readTreeRef()
        val elsep = readTreeRef()
        If(cond, thenp, elsep)

      case MATCHtree =>
        val selector = readTreeRef()
        val cases = until(end, () => readCaseDefRef())
        Match(selector, cases)

      case RETURNtree =>
        setSym()
        Return(readTreeRef(), Ident(symbol.termRef))

      case TREtree =>
        val block = readTreeRef()
        val finalizer = readTreeRef()
        val catches = until(end, () => readCaseDefRef())
        Try(block, catches, finalizer)

      case THROWtree =>
        Throw(readTreeRef())

      case NEWtree =>
        New(readTreeRef().tpe)

      case TYPEDtree =>
        val expr = readTreeRef()
        val tpt = readTreeRef()
        Typed(expr, tpt)

      case TYPEAPPLYtree =>
        val fun = readTreeRef()
        val args = until(end, () => readTreeRef())
        TypeApply(fun, args)

      case APPLYtree =>
        val fun = readTreeRef()
        val args = until(end, () => readTreeRef())
        /*
          if (fun.symbol.isOverloaded) {
            fun.setType(fun.symbol.info)
            inferMethodAlternative(fun, args map (_.tpe), tpe)
          }
*/
        Apply(fun, args) // note: can't deal with overloaded syms yet

      case APPLYDYNAMICtree =>
        setSym()
        val qual = readTreeRef()
        val args = until(end, () => readTreeRef())
        unimplementedTree("APPLYDYNAMIC")

      case SUPERtree =>
        setSym()
        val qual = readTreeRef()
        val mix = readTypeNameRef()
        Super(qual, mix)

      case THIStree =>
        setSym()
        val name = readTypeNameRef()
        This(symbol.asClass)

      case SELECTtree =>
        setSym()
        val qualifier = readTreeRef()
        val selector = readNameRef()
        qualifier.select(symbol.namedType)
      case IDENTtree =>
        setSymName()
        Ident(symbol.namedType)

      case LITERALtree =>
        readConstantRef() match
          case c: Constant => Literal(c)
          case tp: TermRef => ref(tp)

      case TYPEtree =>
        TypeTree(tpe)

      case ANNOTATEDtree =>
        val annot = readTreeRef()
        val arg = readTreeRef()
        Annotated(arg, annot)

      case SINGLETONTYPEtree =>
        SingletonTypeTree(readTreeRef())

      case SELECTFROMTYPEtree =>
        val qualifier = readTreeRef()
        val selector = readTypeNameRef()
        Select(qualifier, symbol.namedType)

      case COMPOUNDTYPEtree =>
        readTemplateRef()
        TypeTree(tpe)

      case APPLIEDTYPEtree =>
        val tpt = readTreeRef()
        val args = until(end, () => readTreeRef())
        AppliedTypeTree(tpt, args)

      case TYPEBOUNDStree =>
        val lo = readTreeRef()
        val hi = readTreeRef()
        createNullableTypeBoundsTree(lo, hi)

      case EXISTENTIALTYPEtree =>
        val tpt = readTreeRef()
        val whereClauses = until(end, () => readTreeRef())
        TypeTree(tpe)

      case _ =>
        noSuchTreeTag(tag, end)
    }
  }

  def noSuchTreeTag(tag: Int, end: Int)(using Context): Nothing =
    errorBadSignature("unknown tree type (" + tag + ")")

  def unimplementedTree(what: String)(using Context): Nothing =
    errorBadSignature(s"cannot read $what trees from Scala 2.x signatures")

  def readModifiers(isType: Boolean)(using Context): Modifiers = {
    val tag = readNat()
    if (tag != MODIFIERS)
      errorBadSignature("expected a modifiers tag (" + tag + ")")
    val end = readNat() + readIndex
    val pflagsHi = readNat()
    val pflagsLo = readNat()
    val pflags = (pflagsHi.toLong << 32) + pflagsLo
    val flags = unpickleScalaFlags(pflags, isType)
    val privateWithin = readNameRef().asTypeName
    Modifiers(flags, privateWithin, Nil)
  }

  protected def readTemplateRef()(using Context): Template =
    readTreeRef() match {
      case templ: Template => templ
      case other =>
        errorBadSignature("expected a template (" + other + ")")
    }
  protected def readCaseDefRef()(using Context): CaseDef =
    readTreeRef() match {
      case tree: CaseDef => tree
      case other =>
        errorBadSignature("expected a case def (" + other + ")")
    }
  protected def readValDefRef()(using Context): ValDef =
    readTreeRef() match {
      case tree: ValDef => tree
      case other =>
        errorBadSignature("expected a ValDef (" + other + ")")
    }
  protected def readIdentRef()(using Context): Ident =
    readTreeRef() match {
      case tree: Ident => tree
      case other =>
        errorBadSignature("expected an Ident (" + other + ")")
    }
  protected def readTypeDefRef()(using Context): TypeDef =
    readTreeRef() match {
      case tree: TypeDef => tree
      case other =>
        errorBadSignature("expected an TypeDef (" + other + ")")
    }
}
