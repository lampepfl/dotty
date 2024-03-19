package dotty.tools.backend.sjs

import scala.language.unsafeNulls

import scala.annotation.switch
import scala.collection.mutable

import dotty.tools.FatalError
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core._
import Contexts._
import Decorators._
import Flags._
import Names._
import NameKinds.DefaultGetterName
import Types._
import Symbols._
import Phases._
import StdNames._
import TypeErasure.ErasedValueType

import dotty.tools.dotc.transform.{Erasure, ValueClasses}
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.report

import org.scalajs.ir
import org.scalajs.ir.{ClassKind, Position, Names => jsNames, Trees => js, Types => jstpe}
import org.scalajs.ir.Names.{ClassName, MethodName, SimpleMethodName}
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.OptimizerHints

import dotty.tools.dotc.transform.sjs.JSSymUtils._

import JSEncoding._
import ScopedVar.withScopedVars

/** Main codegen for Scala.js IR.
 *
 *  [[GenSJSIR]] creates one instance of `JSCodeGen` per compilation unit.
 *  The `run()` method processes the whole compilation unit and generates
 *  `.sjsir` files for it.
 *
 *  There are 4 main levels of translation:
 *
 *  - `genCompilationUnit()` iterates through all the type definitions in the
 *    compilation unit. Each generated `js.ClassDef` is serialized to an
 *    `.sjsir` file.
 *  - `genScalaClass()` and other similar methods generate the skeleton of
 *    classes.
 *  - `genMethod()` and similar methods generate the declarations of methods.
 *  - `genStatOrExpr()` and everything else generate the bodies of methods.
 */
class JSCodeGen()(using genCtx: Context) {
  import JSCodeGen._
  import tpd._

  val sjsPlatform = dotty.tools.dotc.config.SJSPlatform.sjsPlatform
  val jsdefn = JSDefinitions.jsdefn
  private val primitives = new JSPrimitives(genCtx)

  val positionConversions = new JSPositions()(using genCtx)
  import positionConversions._

  private val jsExportsGen = new JSExportsGen(this)

  // Some state --------------------------------------------------------------

  private val lazilyGeneratedAnonClasses = new MutableSymbolMap[TypeDef]
  private val generatedClasses = mutable.ListBuffer.empty[js.ClassDef]
  private val generatedStaticForwarderClasses = mutable.ListBuffer.empty[(Symbol, js.ClassDef)]

  val currentClassSym = new ScopedVar[Symbol]
  private val currentMethodSym = new ScopedVar[Symbol]
  private val localNames = new ScopedVar[LocalNameGenerator]
  private val thisLocalVarIdent = new ScopedVar[Option[js.LocalIdent]]
  private val isModuleInitialized = new ScopedVar[ScopedVar.VarBox[Boolean]]
  private val undefinedDefaultParams = new ScopedVar[mutable.Set[Symbol]]

  /* Contextual JS class value for some operations of nested JS classes that need one. */
  private val contextualJSClassValue = new ScopedVar[Option[js.Tree]](None)

  /** Resets all of the scoped state in the context of `body`. */
  private def resetAllScopedVars[T](body: => T): T = {
    withScopedVars(
        currentClassSym := null,
        currentMethodSym := null,
        localNames := null,
        thisLocalVarIdent := null,
        isModuleInitialized := null,
        undefinedDefaultParams := null
    ) {
      body
    }
  }

  private def withPerMethodBodyState[A](methodSym: Symbol)(body: => A): A = {
    withScopedVars(
        currentMethodSym := methodSym,
        thisLocalVarIdent := None,
        isModuleInitialized := new ScopedVar.VarBox(false),
        undefinedDefaultParams := mutable.Set.empty,
    ) {
      body
    }
  }

  private def acquireContextualJSClassValue[A](f: Option[js.Tree] => A): A = {
    val jsClassValue = contextualJSClassValue.get
    withScopedVars(
        contextualJSClassValue := None
    ) {
      f(jsClassValue)
    }
  }

  def withNewLocalNameScope[A](body: => A): A = {
    withScopedVars(localNames := new LocalNameGenerator) {
      body
    }
  }

  /** Implicitly materializes the current local name generator. */
  implicit def implicitLocalNames: LocalNameGenerator = localNames.get

  def currentThisType: jstpe.Type = {
    encodeClassType(currentClassSym) match {
      case tpe @ jstpe.ClassType(cls) =>
        jstpe.BoxedClassToPrimType.getOrElse(cls, tpe)
      case tpe =>
        tpe
    }
  }

  /** Returns a new fresh local identifier. */
  private def freshLocalIdent()(implicit pos: Position): js.LocalIdent =
    localNames.get.freshLocalIdent()

  /** Returns a new fresh local identifier. */
  def freshLocalIdent(base: String)(implicit pos: Position): js.LocalIdent =
    localNames.get.freshLocalIdent(base)

  /** Returns a new fresh local identifier. */
  private def freshLocalIdent(base: TermName)(implicit pos: Position): js.LocalIdent =
    localNames.get.freshLocalIdent(base)

  private def consumeLazilyGeneratedAnonClass(sym: Symbol): TypeDef = {
    val typeDef = lazilyGeneratedAnonClasses.remove(sym)
    if (typeDef == null) {
      throw new FatalError(
          i"Could not find tree for lazily generated anonymous class ${sym.fullName} at ${sym.sourcePos}")
    } else {
      typeDef
    }
  }

  // Compilation unit --------------------------------------------------------

  def run(): Unit = {
    try {
      genCompilationUnit(ctx.compilationUnit)
    } finally {
      generatedClasses.clear()
      generatedStaticForwarderClasses.clear()
    }
  }

  /** Generates the Scala.js IR for a compilation unit
   *  This method iterates over all the class and interface definitions
   *  found in the compilation unit and emits their IR (.sjsir).
   *
   *  Some classes are never actually emitted:
   *  - Classes representing primitive types
   *  - The scala.Array class
   *
   *  TODO Some classes representing anonymous functions are not actually emitted.
   *  Instead, a temporary representation of their `apply` method is built
   *  and recorded, so that it can be inlined as a JavaScript anonymous
   *  function in the method that instantiates it.
   *
   *  Other ClassDefs are emitted according to their nature:
   *  * Non-native JS class       -> `genNonNativeJSClass()`
   *  * Other JS type (<: js.Any) -> `genRawJSClassData()`
   *  * Interface                 -> `genInterface()`
   *  * Normal class              -> `genClass()`
   */
  private def genCompilationUnit(cunit: CompilationUnit): Unit = {
    def collectTypeDefs(tree: Tree): List[TypeDef] = {
      tree match {
        case EmptyTree            => Nil
        case PackageDef(_, stats) => stats.flatMap(collectTypeDefs)
        case cd: TypeDef          => cd :: Nil
        case _: ValDef            => Nil // module instance
      }
    }
    val allTypeDefs = collectTypeDefs(cunit.tpdTree)

    /* #13221 Set JavaStatic on all the Module fields of static module classes.
     * This is necessary for `desugarIdent` not to crash in some obscure
     * scenarios.
     *
     * !!! Part of this logic is duplicated in BCodeSkelBuilder.genPlainClass
     *
     * However, here we only do this for Module fields, not all fields.
     */
    for (typeDef <- allTypeDefs) {
      if (typeDef.symbol.is(ModuleClass)) {
        typeDef.symbol.info.decls.foreach { f =>
          if (f.isField && f.is(Module))
            f.setFlag(JavaStatic)
        }
      }
    }

    val (anonJSClassTypeDefs, otherTypeDefs) =
      allTypeDefs.partition(td => td.symbol.isAnonymousClass && td.symbol.isJSType)

    // Record the TypeDefs of anonymous JS classes to be lazily generated
    for (td <- anonJSClassTypeDefs)
      lazilyGeneratedAnonClasses(td.symbol) = td

    /* Finally, we emit true code for the remaining class defs. */
    for (td <- otherTypeDefs) {
      val sym = td.symbol
      implicit val pos: Position = sym.span

      /* Do not actually emit code for primitive types nor scala.Array. */
      val isPrimitive =
        sym.isPrimitiveValueClass || sym == defn.ArrayClass

      if (!isPrimitive) {
        withScopedVars(
            currentClassSym := sym
        ) {
          val tree = if (sym.isJSType) {
            if (!sym.is(Trait) && sym.isNonNativeJSClass)
              genNonNativeJSClass(td)
            else
              genRawJSClassData(td)
          } else if (sym.is(Trait)) {
            genInterface(td)
          } else {
            genScalaClass(td)
          }

          generatedClasses += tree
        }
      }
    }

    for (tree <- generatedClasses)
      genIRFile(cunit, tree)

    if (generatedStaticForwarderClasses.nonEmpty) {
      /* #4148 Add generated static forwarder classes, except those that
       * would collide with regular classes on case insensitive file systems.
       */

      /* I could not find any reference anywhere about what locale is used
       * by case insensitive file systems to compare case-insensitively.
       * In doubt, force the English locale, which is probably going to do
       * the right thing in virtually all cases (especially if users stick
       * to ASCII class names), and it has the merit of being deterministic,
       * as opposed to using the OS' default locale.
       * The JVM backend performs a similar test to emit a warning for
       * conflicting top-level classes. However, it uses `toLowerCase()`
       * without argument, which is not deterministic.
       */
      def caseInsensitiveNameOf(classDef: js.ClassDef): String =
        classDef.name.name.nameString.toLowerCase(java.util.Locale.ENGLISH)

      val generatedCaseInsensitiveNames =
        generatedClasses.map(caseInsensitiveNameOf).toSet

      for ((site, classDef) <- generatedStaticForwarderClasses) {
        if (!generatedCaseInsensitiveNames.contains(caseInsensitiveNameOf(classDef))) {
          genIRFile(cunit, classDef)
        } else {
          report.warning(
              s"Not generating the static forwarders of ${classDef.name.name.nameString} " +
              "because its name differs only in case from the name of another class or trait in this compilation unit.",
              site.srcPos)
        }
      }
    }
  }

  private def genIRFile(cunit: CompilationUnit, tree: ir.Trees.ClassDef): Unit = {
    val outfile = getFileFor(cunit, tree.name.name, ".sjsir")
    val output = outfile.bufferedOutput
    try {
      ir.Serializers.serialize(output, tree)
    } finally {
      output.close()
    }
  }

  private def getFileFor(cunit: CompilationUnit, className: ClassName,
      suffix: String): dotty.tools.io.AbstractFile = {
    val outputDirectory = ctx.settings.outputDir.value
    val pathParts = className.nameString.split('.')
    val dir = pathParts.init.foldLeft(outputDirectory)(_.subdirectoryNamed(_))
    val filename = pathParts.last
    dir.fileNamed(filename + suffix)
  }

  // Generate a class --------------------------------------------------------

  /** Gen the IR ClassDef for a Scala class definition (maybe a module class).
   */
  private def genScalaClass(td: TypeDef): js.ClassDef = {
    val sym = td.symbol.asClass
    implicit val pos: SourcePosition = sym.sourcePos

    assert(!sym.is(Trait),
        "genScalaClass() must be called only for normal classes: "+sym)
    assert(sym.superClass != NoSymbol, sym)

    if (hasDefaultCtorArgsAndJSModule(sym)) {
      report.error(
          "Implementation restriction: " +
          "constructors of Scala classes cannot have default parameters if their companion module is JS native.",
          td)
    }

    val classIdent = encodeClassNameIdent(sym)
    val originalName = originalNameOfClass(sym)
    val isHijacked = false //isHijackedBoxedClass(sym)

    // Optimizer hints

    val isDynamicImportThunk = sym.isSubClass(jsdefn.DynamicImportThunkClass)

    def isStdLibClassWithAdHocInlineAnnot(sym: Symbol): Boolean = {
      val fullName = sym.fullName.toString
      (fullName.startsWith("scala.Tuple") && !fullName.endsWith("$")) ||
      (fullName.startsWith("scala.collection.mutable.ArrayOps$of"))
    }

    val shouldMarkInline = (
        isDynamicImportThunk ||
        sym.hasAnnotation(jsdefn.InlineAnnot) ||
        (sym.isAnonymousFunction && !sym.isSubClass(defn.PartialFunctionClass)) ||
        isStdLibClassWithAdHocInlineAnnot(sym))

    val optimizerHints = {
      OptimizerHints.empty
        .withInline(shouldMarkInline)
        .withNoinline(sym.hasAnnotation(jsdefn.NoinlineAnnot))
    }

    // Generate members (constructor + methods)

    val generatedNonFieldMembers = new mutable.ListBuffer[js.MemberDef]

    val tpl = td.rhs.asInstanceOf[Template]
    for (tree <- tpl.constr :: tpl.body) {
      tree match {
        case EmptyTree => ()

        case vd: ValDef =>
          // fields are added via genClassFields(), but we need to generate the JS native members
          val sym = vd.symbol
          if (!sym.is(Module) && sym.hasAnnotation(jsdefn.JSNativeAnnot))
            generatedNonFieldMembers += genJSNativeMemberDef(vd)

        case dd: DefDef =>
          val sym = dd.symbol
          if sym.hasAnnotation(jsdefn.JSNativeAnnot) then
            if !sym.is(Accessor) then
              generatedNonFieldMembers += genJSNativeMemberDef(dd)
          else
            generatedNonFieldMembers ++= genMethod(dd)

        case _ =>
          throw new FatalError("Illegal tree in body of genScalaClass(): " + tree)
      }
    }

    // Generate fields and add to methods + ctors
    val generatedMembers = genClassFields(td) ++ generatedNonFieldMembers.toList

    // Generate member exports
    val memberExports = jsExportsGen.genMemberExports(sym)

    // Generate top-level export definitions
    val topLevelExportDefs = jsExportsGen.genTopLevelExports(sym)

    // Static initializer
    val optStaticInitializer = {
      // Initialization of reflection data, if required
      val reflectInit = {
        val enableReflectiveInstantiation = {
          sym.baseClasses.exists { ancestor =>
            ancestor.hasAnnotation(jsdefn.EnableReflectiveInstantiationAnnot)
          }
        }
        if (enableReflectiveInstantiation)
          genRegisterReflectiveInstantiation(sym).toList
        else
          Nil
      }

      // Initialization of the module because of field exports
      val needsStaticModuleInit =
        topLevelExportDefs.exists(_.isInstanceOf[js.TopLevelFieldExportDef])
      val staticModuleInit =
        if (!needsStaticModuleInit) Nil
        else List(genLoadModule(sym))

      val staticInitializerStats = reflectInit ::: staticModuleInit
      if (staticInitializerStats.nonEmpty)
        List(genStaticConstructorWithStats(ir.Names.StaticInitializerName, js.Block(staticInitializerStats)))
      else
        Nil
    }

    val optDynamicImportForwarder =
      if (isDynamicImportThunk) List(genDynamicImportForwarder(sym))
      else Nil

    val allMemberDefsExceptStaticForwarders =
      generatedMembers ::: memberExports ::: optStaticInitializer ::: optDynamicImportForwarder

    // Add static forwarders
    val allMemberDefs = if (!isCandidateForForwarders(sym)) {
      allMemberDefsExceptStaticForwarders
    } else {
      if (isStaticModule(sym)) {
        /* If the module class has no linked class, we must create one to
         * hold the static forwarders. Otherwise, this is going to be handled
         * when generating the companion class.
         */
        if (!sym.linkedClass.exists) {
          val forwarders = genStaticForwardersFromModuleClass(Nil, sym)
          if (forwarders.nonEmpty) {
            val forwardersClassDef = js.ClassDef(
                js.ClassIdent(ClassName(classIdent.name.nameString.stripSuffix("$"))),
                originalName,
                ClassKind.Class,
                None,
                Some(js.ClassIdent(ir.Names.ObjectClass)),
                Nil,
                None,
                None,
                forwarders,
                Nil
            )(js.OptimizerHints.empty)
            generatedStaticForwarderClasses += sym -> forwardersClassDef
          }
        }
        allMemberDefsExceptStaticForwarders
      } else {
        val forwarders = genStaticForwardersForClassOrInterface(
            allMemberDefsExceptStaticForwarders, sym)
        allMemberDefsExceptStaticForwarders ::: forwarders
      }
    }

    // Hashed definitions of the class
    val hashedDefs = ir.Hashers.hashMemberDefs(allMemberDefs)

    // The complete class definition
    val kind =
      if (isStaticModule(sym)) ClassKind.ModuleClass
      else if (isHijacked) ClassKind.HijackedClass
      else ClassKind.Class

    val classDefinition = js.ClassDef(
        classIdent,
        originalName,
        kind,
        None,
        Some(encodeClassNameIdent(sym.superClass)),
        genClassInterfaces(sym, forJSClass = false),
        None,
        None,
        hashedDefs,
        topLevelExportDefs)(
        optimizerHints)

    classDefinition
  }

  /** Gen the IR ClassDef for a Scala.js-defined JS class. */
  private def genNonNativeJSClass(td: TypeDef): js.ClassDef = {
    val sym = td.symbol.asClass
    implicit val pos: SourcePosition = sym.sourcePos

    assert(sym.isNonNativeJSClass,
        i"genNonNativeJSClass() must be called only for non-native JS classes: $sym")
    assert(sym.superClass != NoSymbol, sym)

    if (hasDefaultCtorArgsAndJSModule(sym)) {
      report.error(
          "Implementation restriction: " +
          "constructors of non-native JS classes cannot have default parameters if their companion module is JS native.",
          td)
    }

    val classIdent = encodeClassNameIdent(sym)
    val originalName = originalNameOfClass(sym)

    // Generate members (constructor + methods)

    val constructorTrees = new mutable.ListBuffer[DefDef]
    val generatedMethods = new mutable.ListBuffer[js.MethodDef]
    val dispatchMethodNames = new mutable.ListBuffer[JSName]

    val tpl = td.rhs.asInstanceOf[Template]
    for (tree <- tpl.constr :: tpl.body) {
      tree match {
        case EmptyTree => ()

        case _: ValDef =>
          () // fields are added via genClassFields()

        case dd: DefDef =>
          val sym = dd.symbol
          val exposed = sym.isJSExposed

          if (sym.isClassConstructor) {
            constructorTrees += dd
          } else if (exposed && sym.is(Accessor, butNot = Lazy)) {
            // Exposed accessors must not be emitted, since the field they access is enough.
          } else if (sym.hasAnnotation(jsdefn.JSOptionalAnnot)) {
            // Optional methods must not be emitted
          } else {
            generatedMethods ++= genMethod(dd)

            // Collect the names of the dispatchers we have to create
            if (exposed && !sym.is(Deferred)) {
              /* We add symbols that we have to expose here. This way we also
               * get inherited stuff that is implemented in this class.
               */
              dispatchMethodNames += sym.jsName
            }
          }

        case _ =>
          throw new FatalError("Illegal tree in gen of genNonNativeJSClass(): " + tree)
      }
    }

    // Static members (exported from the companion object)
    val staticMembers = {
      val module = sym.companionModule
      if (!module.exists) {
        Nil
      } else {
        val companionModuleClass = module.moduleClass
        val exports = withScopedVars(currentClassSym := companionModuleClass) {
          jsExportsGen.genStaticExports(companionModuleClass)
        }
        if (exports.exists(_.isInstanceOf[js.JSFieldDef])) {
          val classInitializer =
            genStaticConstructorWithStats(ir.Names.ClassInitializerName, genLoadModule(companionModuleClass))
          exports :+ classInitializer
        } else {
          exports
        }
      }
    }

    val topLevelExports = jsExportsGen.genTopLevelExports(sym)

    val (generatedConstructor, jsClassCaptures) = withNewLocalNameScope {
      val isNested = sym.isNestedJSClass

      if (isNested)
        localNames.reserveLocalName(JSSuperClassParamName)

      val (captures, ctor) = genJSClassCapturesAndConstructor(constructorTrees.toList)

      val jsClassCaptures = if (isNested) {
        val superParam = js.ParamDef(js.LocalIdent(JSSuperClassParamName),
            NoOriginalName, jstpe.AnyType, mutable = false)
        Some(superParam :: captures)
      } else {
        assert(captures.isEmpty, s"found non nested JS class with captures $captures at $pos")
        None
      }

      (ctor, jsClassCaptures)
    }

    // Generate fields (and add to methods + ctors)
    val generatedMembers = {
      genClassFields(td) :::
      generatedConstructor ::
      jsExportsGen.genJSClassDispatchers(sym, dispatchMethodNames.result().distinct) :::
      generatedMethods.toList :::
      staticMembers
    }

    // Hashed definitions of the class
    val hashedMemberDefs = ir.Hashers.hashMemberDefs(generatedMembers)

    // The complete class definition
    val kind =
      if (isStaticModule(sym)) ClassKind.JSModuleClass
      else ClassKind.JSClass

    val classDefinition = js.ClassDef(
        classIdent,
        originalNameOfClass(sym),
        kind,
        jsClassCaptures,
        Some(encodeClassNameIdent(sym.superClass)),
        genClassInterfaces(sym, forJSClass = true),
        jsSuperClass = jsClassCaptures.map(_.head.ref),
        None,
        hashedMemberDefs,
        topLevelExports)(
        OptimizerHints.empty)

    classDefinition
  }

  /** Gen the IR ClassDef for a raw JS class or trait.
   */
  private def genRawJSClassData(td: TypeDef): js.ClassDef = {
    val sym = td.symbol.asClass
    implicit val pos: Position = sym.span

    val classIdent = encodeClassNameIdent(sym)
    val kind = {
      if (sym.is(Trait)) ClassKind.AbstractJSType
      else if (sym.is(ModuleClass)) ClassKind.NativeJSModuleClass
      else ClassKind.NativeJSClass
    }
    val superClass =
      if (sym.is(Trait)) None
      else Some(encodeClassNameIdent(sym.superClass))
    val jsNativeLoadSpec = computeJSNativeLoadSpecOfClass(sym)

    js.ClassDef(
        classIdent,
        originalNameOfClass(sym),
        kind,
        None,
        superClass,
        genClassInterfaces(sym, forJSClass = true),
        None,
        jsNativeLoadSpec,
        Nil,
        Nil)(
        OptimizerHints.empty)
  }

  /** Gen the IR ClassDef for an interface definition.
   */
  private def genInterface(td: TypeDef): js.ClassDef = {
    val sym = td.symbol.asClass
    implicit val pos: SourcePosition = sym.sourcePos

    val classIdent = encodeClassNameIdent(sym)

    val generatedMethods = new mutable.ListBuffer[js.MethodDef]

    val tpl = td.rhs.asInstanceOf[Template]
    for (tree <- tpl.constr :: tpl.body) {
      tree match {
        case EmptyTree  => ()
        case dd: DefDef => generatedMethods ++= genMethod(dd)
        case _ =>
          throw new FatalError(
            i"""Illegal tree in gen of genInterface(): $tree
               |class = $td
               |in ${ctx.compilationUnit}""")
      }
    }

    val superInterfaces = genClassInterfaces(sym, forJSClass = false)

    val genMethodsList = generatedMethods.toList
    val allMemberDefs =
      if (!isCandidateForForwarders(sym)) genMethodsList
      else genMethodsList ::: genStaticForwardersForClassOrInterface(genMethodsList, sym)

    // Hashed definitions of the interface
    val hashedDefs = ir.Hashers.hashMemberDefs(allMemberDefs)

    js.ClassDef(
        classIdent,
        originalNameOfClass(sym),
        ClassKind.Interface,
        None,
        None,
        superInterfaces,
        None,
        None,
        hashedDefs,
        Nil)(
        OptimizerHints.empty)
  }

  private def genClassInterfaces(sym: ClassSymbol, forJSClass: Boolean)(
      implicit pos: Position): List[js.ClassIdent] = {
    for {
      intf <- sym.directlyInheritedTraits
      if !(forJSClass && intf == defn.DynamicClass)
    } yield {
      encodeClassNameIdent(intf)
    }
  }

  // Static forwarders -------------------------------------------------------

  /* This mimics the logic in BCodeHelpers.addForwarders and the code that
   * calls it, except that we never have collisions with existing methods in
   * the companion class. This is because in the IR, only methods with the
   * same `MethodName` (including signature) and that are also
   * `PublicStatic` would collide. There should never be an actual collision
   * because the only `PublicStatic` methods that are otherwise generated are
   * the bodies of SAMs, which have mangled names. If that assumption is
   * broken, an error message is emitted asking the user to report a bug.
   *
   * It is important that we always emit forwarders, because some Java APIs
   * actually have a public static method and a public instance method with
   * the same name. For example the class `Integer` has a
   * `def hashCode(): Int` and a `static def hashCode(Int): Int`. The JVM
   * back-end considers them as colliding because they have the same name,
   * but we must not.
   *
   * By default, we only emit forwarders for top-level objects, like the JVM
   * back-end. However, if requested via a compiler option, we enable them
   * for all static objects. This is important so we can implement static
   * methods of nested static classes of JDK APIs (see scala-js/#3950).
   */

  /** Is the given Scala class, interface or module class a candidate for
   *  static forwarders?
   *
   *  - the flag `-XnoForwarders` is not set to true, and
   *  - the symbol is static, and
   *  - either of both of the following is true:
   *    - the flag `-scalajsGenStaticForwardersForNonTopLevelObjects` is set to true, or
   *    - the symbol was originally at the package level
   *
   *  Other than the Scala.js-specific flag, and the fact that we also consider
   *  interfaces, this performs the same tests as the JVM back-end.
   */
  def isCandidateForForwarders(sym: Symbol): Boolean = {
    !ctx.settings.XnoForwarders.value && sym.isStatic && {
      ctx.settings.scalajsGenStaticForwardersForNonTopLevelObjects.value || {
        atPhase(flattenPhase) {
          toDenot(sym).owner.is(PackageClass)
        }
      }
    }
  }

  /** Gen the static forwarders to the members of a class or interface for
   *  methods of its companion object.
   *
   *  This is only done if there exists a companion object and it is not a JS
   *  type.
   *
   *  Precondition: `isCandidateForForwarders(sym)` is true
   */
  def genStaticForwardersForClassOrInterface(
      existingMembers: List[js.MemberDef], sym: Symbol)(
      implicit pos: SourcePosition): List[js.MemberDef] = {
    val module = sym.companionModule
    if (!module.exists) {
      Nil
    } else {
      val moduleClass = module.moduleClass
      if (!moduleClass.isJSType)
        genStaticForwardersFromModuleClass(existingMembers, moduleClass)
      else
        Nil
    }
  }

  /** Gen the static forwarders for the methods of a module class.
   *
   *  Precondition: `isCandidateForForwarders(moduleClass)` is true
   */
  def genStaticForwardersFromModuleClass(existingMembers: List[js.MemberDef],
      moduleClass: Symbol)(
      implicit pos: SourcePosition): List[js.MemberDef] = {

    assert(moduleClass.is(ModuleClass), moduleClass)

    val existingPublicStaticMethodNames = existingMembers.collect {
      case js.MethodDef(flags, name, _, _, _, _)
          if flags.namespace == js.MemberNamespace.PublicStatic =>
        name.name
    }.toSet

    val members = {
      moduleClass.info.membersBasedOnFlags(required = Flags.Method,
          excluded = Flags.ExcludedForwarder).map(_.symbol)
    }

    def isExcluded(m: Symbol): Boolean = {
      def hasAccessBoundary = m.accessBoundary(defn.RootClass) ne defn.RootClass

      def isOfJLObject: Boolean = m.owner eq defn.ObjectClass

      def isDefaultParamOfJSNativeDef: Boolean = {
        m.name.is(DefaultGetterName) && {
          val info = new DefaultParamInfo(m)
          !info.isForConstructor && info.attachedMethod.hasAnnotation(jsdefn.JSNativeAnnot)
        }
      }

      m.is(Deferred)
        || m.isConstructor
        || hasAccessBoundary
        || isOfJLObject
        || m.hasAnnotation(jsdefn.JSNativeAnnot) || isDefaultParamOfJSNativeDef // #4557
    }

    val forwarders = for {
      m <- members
      if !isExcluded(m)
    } yield {
      withNewLocalNameScope {
        val flags = js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic)
        val methodIdent = encodeMethodSym(m)
        val originalName = originalNameOfMethod(m)
        val jsParams = for {
          (paramName, paramInfo) <- m.info.paramNamess.flatten.zip(m.info.paramInfoss.flatten)
        } yield {
          js.ParamDef(freshLocalIdent(paramName), NoOriginalName,
              toIRType(paramInfo), mutable = false)
        }
        val resultType = toIRType(m.info.resultType)

        if (existingPublicStaticMethodNames.contains(methodIdent.name)) {
          report.error(
              "Unexpected situation: found existing public static method " +
              s"${methodIdent.name.nameString} in the companion class of " +
              s"${moduleClass.fullName}; cannot generate a static forwarder " +
              "the method of the same name in the object." +
              "Please report this as a bug in the Scala.js support in dotty.",
              pos)
        }

        js.MethodDef(flags, methodIdent, originalName, jsParams, resultType, Some {
          genApplyMethod(genLoadModule(moduleClass), m, jsParams.map(_.ref))
        })(OptimizerHints.empty, None)
      }
    }

    forwarders.toList
  }

  // Generate the fields of a class ------------------------------------------

  /** Gen definitions for the fields of a class. */
  private def genClassFields(td: TypeDef): List[js.MemberDef] = {
    val classSym = td.symbol.asClass
    assert(currentClassSym.get == classSym,
        "genClassFields called with a ClassDef other than the current one")

    val isJSClass = classSym.isNonNativeJSClass

    // Term members that are neither methods nor modules are fields
    classSym.info.decls.filter { f =>
      !f.isOneOf(MethodOrModule) && f.isTerm
        && !f.hasAnnotation(jsdefn.JSNativeAnnot)
        && !f.hasAnnotation(jsdefn.JSOptionalAnnot)
        && !f.hasAnnotation(jsdefn.JSExportStaticAnnot)
    }.flatMap({ f =>
      implicit val pos = f.span

      val isTopLevelExport = f.hasAnnotation(jsdefn.JSExportTopLevelAnnot)
      val isJavaStatic = f.is(JavaStatic)
      assert(!(isTopLevelExport && isJavaStatic),
          em"found ${f.fullName} which is both a top-level export and a Java static")
      val isStaticField = isTopLevelExport || isJavaStatic

      val namespace = if isStaticField then js.MemberNamespace.PublicStatic else js.MemberNamespace.Public
      val mutable = isStaticField || f.is(Mutable)

      val flags = js.MemberFlags.empty.withMutable(mutable).withNamespace(namespace)

      val irTpe0 =
        if (isJSClass) genExposedFieldIRType(f)
        else if (isTopLevelExport) jstpe.AnyType
        else toIRType(f.info)

      // scala-js/#4370 Fields cannot have type NothingType
      val irTpe =
        if (irTpe0 == jstpe.NothingType) encodeClassType(defn.NothingClass)
        else irTpe0

      if (isJSClass && f.isJSExposed)
        js.JSFieldDef(flags, genExpr(f.jsName)(f.sourcePos), irTpe) :: Nil
      else
        val fieldIdent = encodeFieldSym(f)
        val originalName = originalNameOfField(f)
        val fieldDef = js.FieldDef(flags, fieldIdent, originalName, irTpe)
        val optionalStaticFieldGetter =
          if isJavaStatic then
            // Here we are generating a public static getter for the static field,
            // this is its API for other units. This is necessary for singleton
            // enum values, which are backed by static fields.
            val className = encodeClassName(classSym)
            val body = js.Block(
                js.LoadModule(className),
                js.SelectStatic(className, fieldIdent)(irTpe))
            js.MethodDef(js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic),
                encodeStaticMemberSym(f), originalName, Nil, irTpe,
                Some(body))(
                OptimizerHints.empty, None) :: Nil
          else
            Nil
        fieldDef :: optionalStaticFieldGetter
    }).toList
  }

  def genExposedFieldIRType(f: Symbol): jstpe.Type = {
    val tpeEnteringPosterasure = atPhase(elimErasedValueTypePhase)(f.info)
    tpeEnteringPosterasure match {
      case tpe: ErasedValueType =>
        /* Here, we must store the field as the boxed representation of
         * the value class. The default value of that field, as
         * initialized at the time the instance is created, will
         * therefore be null. This will not match the behavior we would
         * get in a Scala class. To match the behavior, we would need to
         * initialized to an instance of the boxed representation, with
         * an underlying value set to the zero of its type. However we
         * cannot implement that, so we live with the discrepancy.
         *
         * In dotc this is usually not an issue, because it unboxes `null` to
         * the zero of the underlying type, unlike scalac which throws an NPE.
         */
        jstpe.ClassType(encodeClassName(tpe.tycon.typeSymbol))

      case _ =>
        // Other types are not boxed, so we can initialized them to their true zero.
        toIRType(f.info)
    }
  }

  // Static initializers -----------------------------------------------------

  private def genStaticConstructorWithStats(name: MethodName, stats: js.Tree)(
      implicit pos: Position): js.MethodDef = {
    js.MethodDef(
        js.MemberFlags.empty.withNamespace(js.MemberNamespace.StaticConstructor),
        js.MethodIdent(name),
        NoOriginalName,
        Nil,
        jstpe.NoType,
        Some(stats))(
        OptimizerHints.empty, None)
  }

  private def genRegisterReflectiveInstantiation(sym: Symbol)(
      implicit pos: SourcePosition): Option[js.Tree] = {
    if (isStaticModule(sym))
      genRegisterReflectiveInstantiationForModuleClass(sym)
    else if (sym.is(ModuleClass))
      None // scala-js#3228
    else if (sym.is(Lifted) && !sym.originalOwner.isClass)
      None // scala-js#3227
    else
      genRegisterReflectiveInstantiationForNormalClass(sym)
  }

  private def genRegisterReflectiveInstantiationForModuleClass(sym: Symbol)(
      implicit pos: SourcePosition): Option[js.Tree] = {
    val fqcnArg = js.StringLiteral(sym.fullName.toString)
    val runtimeClassArg = js.ClassOf(toTypeRef(sym.info))
    val loadModuleFunArg =
      js.Closure(arrow = true, Nil, Nil, None, genLoadModule(sym), Nil)

    val stat = genApplyMethod(
        genLoadModule(jsdefn.ReflectModule),
        jsdefn.Reflect_registerLoadableModuleClass,
        List(fqcnArg, runtimeClassArg, loadModuleFunArg))

    Some(stat)
  }

  private def genRegisterReflectiveInstantiationForNormalClass(sym: Symbol)(
      implicit pos: SourcePosition): Option[js.Tree] = {
    val ctors =
      if (sym.is(Abstract)) Nil
      else sym.info.member(nme.CONSTRUCTOR).alternatives.map(_.symbol).filter(m => !m.isOneOf(Private | Protected))

    if (ctors.isEmpty) {
      None
    } else {
      val constructorsInfos = for {
        ctor <- ctors
      } yield {
        withNewLocalNameScope {
          val (parameterTypes, formalParams, actualParams) = (for {
            (paramName, paramInfo) <- ctor.info.paramNamess.flatten.zip(ctor.info.paramInfoss.flatten)
          } yield {
            val paramType = js.ClassOf(toTypeRef(paramInfo))
            val paramDef = js.ParamDef(freshLocalIdent(paramName),
                NoOriginalName, jstpe.AnyType, mutable = false)
            val actualParam = unbox(paramDef.ref, paramInfo)
            (paramType, paramDef, actualParam)
          }).unzip3

          val paramTypesArray = js.JSArrayConstr(parameterTypes)

          val newInstanceFun = js.Closure(arrow = true, Nil, formalParams, None, {
            js.New(encodeClassName(sym), encodeMethodSym(ctor), actualParams)
          }, Nil)

          js.JSArrayConstr(List(paramTypesArray, newInstanceFun))
        }
      }

      val fqcnArg = js.StringLiteral(sym.fullName.toString)
      val runtimeClassArg = js.ClassOf(toTypeRef(sym.info))
      val ctorsInfosArg = js.JSArrayConstr(constructorsInfos)

      val stat = genApplyMethod(
          genLoadModule(jsdefn.ReflectModule),
          jsdefn.Reflect_registerInstantiatableClass,
          List(fqcnArg, runtimeClassArg, ctorsInfosArg))

      Some(stat)
    }
  }

  // Constructor of a non-native JS class ------------------------------------

  def genJSClassCapturesAndConstructor(constructorTrees: List[DefDef])(
      implicit pos: SourcePosition): (List[js.ParamDef], js.JSConstructorDef) = {
    /* We need to merge all Scala constructors into a single one because the
     * IR, like JavaScript, only allows a single one.
     *
     * We do this by applying:
     * 1. Applying runtime type based dispatch, just like exports.
     * 2. Splitting secondary ctors into parts before and after the `this` call.
     * 3. Topo-sorting all constructor statements and including/excluding
     *    them based on the overload that was chosen.
     */

    val (primaryTree :: Nil, secondaryTrees) =
      constructorTrees.partition(_.symbol.isPrimaryConstructor): @unchecked

    val primaryCtor = genPrimaryJSClassCtor(primaryTree)
    val secondaryCtors = secondaryTrees.map(genSecondaryJSClassCtor(_))

    // VarDefs for the parameters of all constructors.
    val paramVarDefs = for {
      vparam <- constructorTrees.flatMap(_.paramss.flatten)
    } yield {
      val sym = vparam.symbol
      val tpe = toIRType(sym.info)
      js.VarDef(encodeLocalSym(sym), originalNameOfLocal(sym), tpe, mutable = true, jstpe.zeroOf(tpe))(vparam.span)
    }

    /* organize constructors in a called-by tree
     * (the implicit root is the primary constructor)
     */
    val ctorTree = {
      val ctorToChildren = secondaryCtors
        .groupBy(_.targetCtor)
        .withDefaultValue(Nil)

      /* when constructing the call-by tree, we use pre-order traversal to
       * assign overload numbers.
       * this puts all descendants of a ctor in a range of overloads numbers.
       *
       * this property is useful, later, when we need to make statements
       * conditional based on the chosen overload.
       */
      var nextOverloadNum = 0
      def subTree[T <: JSCtor](ctor: T): ConstructorTree[T] = {
        val overloadNum = nextOverloadNum
        nextOverloadNum += 1
        val subtrees = ctorToChildren(ctor.sym).map(subTree(_))
        new ConstructorTree(overloadNum, ctor, subtrees)
      }

      subTree(primaryCtor)
    }

    /* prepare overload dispatch for all constructors.
     * as a side-product, we retrieve the capture parameters.
     */
    val (exports, jsClassCaptures) = {
      val exports = List.newBuilder[jsExportsGen.Exported]
      val jsClassCaptures = List.newBuilder[js.ParamDef]

      def add(tree: ConstructorTree[_ <: JSCtor]): Unit = {
        val (e, c) = genJSClassCtorDispatch(tree.ctor.sym,
            tree.ctor.paramsAndInfo, tree.overloadNum)
        exports += e
        jsClassCaptures ++= c
        tree.subCtors.foreach(add(_))
      }

      add(ctorTree)

      (exports.result(), jsClassCaptures.result())
    }

    // The name 'constructor' is used for error reporting here
    val (formalArgs, restParam, overloadDispatchBody) =
      jsExportsGen.genOverloadDispatch(JSName.Literal("constructor"), exports, jstpe.IntType)

    val overloadVar = js.VarDef(freshLocalIdent("overload"), NoOriginalName,
        jstpe.IntType, mutable = false, overloadDispatchBody)

    val constructorBody = wrapJSCtorBody(
      paramVarDefs :+ overloadVar,
      genJSClassCtorBody(overloadVar.ref, ctorTree),
      js.Undefined() :: Nil
    )

    val constructorDef = js.JSConstructorDef(
        js.MemberFlags.empty.withNamespace(js.MemberNamespace.Constructor),
        formalArgs, restParam, constructorBody)(OptimizerHints.empty, None)

    (jsClassCaptures, constructorDef)
  }

  private def genPrimaryJSClassCtor(dd: DefDef): PrimaryJSCtor = {
    val sym = dd.symbol
    val Block(stats, _) = dd.rhs: @unchecked
    assert(sym.isPrimaryConstructor, s"called with non-primary ctor: $sym")

    var jsSuperCall: Option[js.JSSuperConstructorCall] = None
    val jsStats = List.newBuilder[js.Tree]

    /* Move all statements after the super constructor call since JS
     * cannot access `this` before the super constructor call.
     *
     * dotc inserts statements before the super constructor call for param
     * accessor initializers (including val's and var's declared in the
     * params). We move those after the super constructor call, and are
     * therefore executed later than for a Scala class.
     */
    withPerMethodBodyState(sym) {
      stats.foreach {
        case tree @ Apply(fun @ Select(Super(This(_), _), _), args)
            if fun.symbol.isClassConstructor =>
          assert(jsSuperCall.isEmpty, s"Found 2 JS Super calls at ${dd.sourcePos}")
          implicit val pos: Position = tree.span
          jsSuperCall = Some(js.JSSuperConstructorCall(genActualJSArgs(fun.symbol, args)))

        case stat =>
          val jsStat = genStat(stat)
          assert(jsSuperCall.isDefined || !jsStat.isInstanceOf[js.VarDef],
              "Trying to move a local VarDef after the super constructor call of a non-native JS class at " +
              dd.sourcePos)
          jsStats += jsStat
      }
    }

    assert(jsSuperCall.isDefined,
        s"Did not find Super call in primary JS construtor at ${dd.sourcePos}")

    new PrimaryJSCtor(sym, genParamsAndInfo(sym, dd.paramss),
        js.JSConstructorBody(Nil, jsSuperCall.get, jsStats.result())(dd.span))
  }

  private def genSecondaryJSClassCtor(dd: DefDef): SplitSecondaryJSCtor = {
    val sym = dd.symbol
    assert(!sym.isPrimaryConstructor, s"called with primary ctor $sym")

    def flattenBlocks(t: Tree): List[Tree] = t match {
      case Block(stats, expr) => (stats :+ expr).flatMap(flattenBlocks)
      case _                  => t :: Nil
    }
    val stats = flattenBlocks(dd.rhs)

    val beforeThisCall = List.newBuilder[js.Tree]
    var thisCall: Option[(Symbol, List[js.Tree])] = None
    val afterThisCall = List.newBuilder[js.Tree]

    withPerMethodBodyState(sym) {
      stats.foreach {
        case tree @ Apply(fun @ Select(This(_), _), args)
            if fun.symbol.isClassConstructor =>
          assert(thisCall.isEmpty,
              s"duplicate this() call in secondary JS constructor at ${dd.sourcePos}")

          implicit val pos: Position = tree.span
          val sym = fun.symbol
          thisCall = Some((sym, genActualArgs(sym, args)))

        case stat =>
          val jsStat = genStat(stat)
          if (thisCall.isEmpty)
            beforeThisCall += jsStat
          else
            afterThisCall += jsStat
      }
    }

    assert(thisCall.isDefined,
        i"could not find the this() call in secondary JS constructor at ${dd.sourcePos}:\n${stats.map(_.show).mkString("\n")}")
    val Some((targetCtor, ctorArgs)) = thisCall: @unchecked

    new SplitSecondaryJSCtor(sym, genParamsAndInfo(sym, dd.paramss),
        beforeThisCall.result(), targetCtor, ctorArgs, afterThisCall.result())
  }

  private def genParamsAndInfo(ctorSym: Symbol,
      vparamss: List[ParamClause]): List[(Symbol, JSParamInfo)] = {
    implicit val pos: SourcePosition = ctorSym.sourcePos

    val paramSyms = if (vparamss.isEmpty) Nil else vparamss.head.map(_.symbol)
    paramSyms.zip(ctorSym.jsParamInfos)
  }

  private def genJSClassCtorDispatch(ctorSym: Symbol,
      allParamsAndInfos: List[(Symbol, JSParamInfo)],
      overloadNum: Int): (jsExportsGen.Exported, List[js.ParamDef]) = {

    implicit val pos: SourcePosition = ctorSym.sourcePos

    /* `allParams` are the parameters as seen from inside the constructor body,
     * i.e., the ones generated by the trees in the constructor body.
     */
    val (captureParamsAndInfos, normalParamsAndInfos) =
      allParamsAndInfos.partition(_._2.capture)

    /* For class captures, we need to generate different names than the ones
     * used by the constructor body. This is necessary so that we can forward
     * captures properly between constructor delegation calls.
     */
    val (jsClassCaptures, captureAssigns) = (for {
      (param, info) <- captureParamsAndInfos
    } yield {
      val ident = freshLocalIdent(param.name.toTermName)
      val jsClassCapture =
        js.ParamDef(ident, originalNameOfLocal(param), toIRType(info.info), mutable = false)
      val captureAssign =
        js.Assign(genVarRef(param), jsClassCapture.ref)
      (jsClassCapture, captureAssign)
    }).unzip

    val normalInfos = normalParamsAndInfos.map(_._2).toIndexedSeq

    val jsExport = new jsExportsGen.Exported(ctorSym, normalInfos) {
      def genBody(formalArgsRegistry: jsExportsGen.FormalArgsRegistry): js.Tree = {
        val paramAssigns = for {
          ((param, info), i) <- normalParamsAndInfos.zipWithIndex
        } yield {
          val rhs = jsExportsGen.genScalaArg(this, i, formalArgsRegistry, info, static = true,
              captures = captureParamsAndInfos.map(pi => genVarRef(pi._1)))(
              prevArgsCount => normalParamsAndInfos.take(prevArgsCount).map(pi => genVarRef(pi._1)))

          js.Assign(genVarRef(param), rhs)
        }

        js.Block(captureAssigns ::: paramAssigns, js.IntLiteral(overloadNum))
      }
    }

    (jsExport, jsClassCaptures)
  }

  /** Generates a JS constructor body based on a constructor tree. */
  private def genJSClassCtorBody(overloadVar: js.VarRef,
      ctorTree: ConstructorTree[PrimaryJSCtor])(implicit pos: Position): js.JSConstructorBody = {

    /* generates a statement that conditionally executes body iff the chosen
     * overload is any of the descendants of `tree` (including itself).
     *
     * here we use the property from building the trees, that a set of
     * descendants always has a range of overload numbers.
     */
    def ifOverload(tree: ConstructorTree[_], body: js.Tree): js.Tree = body match {
      case js.Skip() => js.Skip()

      case body =>
        val x = overloadVar
        val cond = {
          import tree.{lo, hi}

          if (lo == hi) {
            js.BinaryOp(js.BinaryOp.Int_==, js.IntLiteral(lo), x)
          } else {
            val lhs = js.BinaryOp(js.BinaryOp.Int_<=, js.IntLiteral(lo), x)
            val rhs = js.BinaryOp(js.BinaryOp.Int_<=, x, js.IntLiteral(hi))
            js.If(lhs, rhs, js.BooleanLiteral(false))(jstpe.BooleanType)
          }
        }

        js.If(cond, body, js.Skip())(jstpe.NoType)
    }

    /* preStats / postStats use pre/post order traversal respectively to
     * generate a topo-sorted sequence of statements.
     */

    def preStats(tree: ConstructorTree[SplitSecondaryJSCtor],
        nextParamsAndInfo: List[(Symbol, JSParamInfo)]): js.Tree = {
      val inner = tree.subCtors.map(preStats(_, tree.ctor.paramsAndInfo))

      assert(tree.ctor.ctorArgs.size == nextParamsAndInfo.size, "param count mismatch")
      val paramsInfosAndArgs = nextParamsAndInfo.zip(tree.ctor.ctorArgs)

      val (captureParamsInfosAndArgs, normalParamsInfosAndArgs) =
        paramsInfosAndArgs.partition(_._1._2.capture)

      val captureAssigns = for {
        ((param, _), arg) <- captureParamsInfosAndArgs
      } yield {
        js.Assign(genVarRef(param), arg)
      }

      val normalAssigns = for {
        (((param, info), arg), i) <- normalParamsInfosAndArgs.zipWithIndex
      } yield {
        val newArg = arg match {
          case js.Transient(UndefinedParam) =>
            /* Go full circle: We have ignored the default param getter for
             * this, we'll create it again.
             *
             * This seems not optimal: We could simply not ignore the calls to
             * default param getters in the first place.
             *
             * However, this proves to be difficult: Because of translations in
             * earlier phases, calls to default param getters may be assigned
             * to temporary variables first (see the undefinedDefaultParams
             * ScopedVar). If this happens, it becomes increasingly difficult
             * to distinguish a default param getter call for a constructor
             * call of *this* instance (in which case we would want to keep
             * the default param getter call) from one for a *different*
             * instance (in which case we would want to discard the default
             * param getter call)
             *
             * Because of this, it ends up being easier to just re-create the
             * default param getter call if necessary.
             */
            implicit val pos: SourcePosition = tree.ctor.sym.sourcePos
            jsExportsGen.genCallDefaultGetter(tree.ctor.sym, i, static = false,
                captures = captureParamsInfosAndArgs.map(p => genVarRef(p._1._1)))(
                prevArgsCount => normalParamsInfosAndArgs.take(prevArgsCount).map(p => genVarRef(p._1._1)))

          case arg => arg
        }

        js.Assign(genVarRef(param), newArg)
      }

      ifOverload(tree, js.Block(
          inner ++ tree.ctor.beforeCall ++ captureAssigns ++ normalAssigns))
    }

    def postStats(tree: ConstructorTree[SplitSecondaryJSCtor]): js.Tree = {
      val inner = tree.subCtors.map(postStats(_))
      ifOverload(tree, js.Block(tree.ctor.afterCall ++ inner))
    }

    val primaryCtor = ctorTree.ctor
    val secondaryCtorTrees = ctorTree.subCtors

    wrapJSCtorBody(
        secondaryCtorTrees.map(preStats(_, primaryCtor.paramsAndInfo)),
        primaryCtor.body,
        secondaryCtorTrees.map(postStats(_))
    )
  }

  private def wrapJSCtorBody(before: List[js.Tree], body: js.JSConstructorBody,
      after: List[js.Tree]): js.JSConstructorBody = {
    js.JSConstructorBody(before ::: body.beforeSuper, body.superCall,
        body.afterSuper ::: after)(body.pos)
  }

  private sealed trait JSCtor {
    val sym: Symbol
    val paramsAndInfo: List[(Symbol, JSParamInfo)]
  }

  private class PrimaryJSCtor(val sym: Symbol,
      val paramsAndInfo: List[(Symbol, JSParamInfo)],
      val body: js.JSConstructorBody) extends JSCtor

  private class SplitSecondaryJSCtor(val sym: Symbol,
      val paramsAndInfo: List[(Symbol, JSParamInfo)],
      val beforeCall: List[js.Tree],
      val targetCtor: Symbol, val ctorArgs: List[js.Tree],
      val afterCall: List[js.Tree]) extends JSCtor

  private class ConstructorTree[Ctor <: JSCtor](
      val overloadNum: Int, val ctor: Ctor,
      val subCtors: List[ConstructorTree[SplitSecondaryJSCtor]]) {
    val lo: Int = overloadNum
    val hi: Int = subCtors.lastOption.fold(lo)(_.hi)

    assert(lo <= hi, "bad overload range")
  }

  // Generate a method -------------------------------------------------------

  /** Generates the JSNativeMemberDef. */
  def genJSNativeMemberDef(tree: ValOrDefDef): js.JSNativeMemberDef = {
    implicit val pos = tree.span

    val sym = tree.symbol
    val flags = js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic)
    val methodName = encodeJSNativeMemberSym(sym)
    val jsNativeLoadSpec = computeJSNativeLoadSpecOfValDef(sym)
    js.JSNativeMemberDef(flags, methodName, jsNativeLoadSpec)
  }

  private def genMethod(dd: DefDef): Option[js.MethodDef] = {
    withScopedVars(
        localNames := new LocalNameGenerator
    ) {
      genMethodWithCurrentLocalNameScope(dd)
    }
  }

  /** Gen JS code for a method definition in a class or in an impl class.
   *  On the JS side, method names are mangled to encode the full signature
   *  of the Scala method, as described in `JSEncoding`, to support
   *  overloading.
   *
   *  Some methods are not emitted at all:
   *  - Primitives, since they are never actually called
   *  - Constructors of hijacked classes
   *
   *  Constructors are emitted by generating their body as a statement.
   *
   *  Other (normal) methods are emitted with `genMethodBody()`.
   */
  private def genMethodWithCurrentLocalNameScope(dd: DefDef): Option[js.MethodDef] = {
    implicit val pos = dd.span
    val sym = dd.symbol
    val vparamss = dd.termParamss
    val rhs = dd.rhs

    /* Is this method a default accessor that should be ignored?
     *
     * This is the case iff one of the following applies:
     * - It is a constructor default accessor and the linked class is a
     *   native JS class.
     * - It is a default accessor for a native JS def, but with the caveat
     *   that its rhs must be `js.native` because of #4553.
     *
     * Both of those conditions can only happen if the default accessor is in
     * a module class, so we use that as a fast way out. (But omitting that
     * condition would not change the result.)
     *
     * This is different than `isJSDefaultParam` in `genApply`: we do not
     * ignore default accessors of *non-native* JS types. Neither for
     * constructor default accessor nor regular default accessors. We also
     * do not need to worry about non-constructor members of native JS types,
     * since for those, the entire member list is ignored in `genJSClassData`.
     */
    def isIgnorableDefaultParam: Boolean = {
      sym.name.is(DefaultGetterName) && sym.owner.is(ModuleClass) && {
        val info = new DefaultParamInfo(sym)
        if (info.isForConstructor) {
          /* This is a default accessor for a constructor parameter. Check
           * whether the attached constructor is a native JS constructor,
           * which is the case iff the linked class is a native JS type.
           */
          info.constructorOwner.hasAnnotation(jsdefn.JSNativeAnnot)
        } else {
          /* #4553 We need to ignore default accessors for JS native defs.
           * However, because Scala.js <= 1.7.0 actually emitted code calling
           * those accessors, we must keep default accessors that would
           * compile. The only accessors we can actually get rid of are those
           * that are `= js.native`.
           */
          !sym.owner.isJSType &&
          info.attachedMethod.hasAnnotation(jsdefn.JSNativeAnnot) && {
            dd.rhs match {
              case MaybeAsInstanceOf(Apply(fun, _)) =>
                fun.symbol == jsdefn.JSPackage_native
              case _ =>
                false
            }
          }
        }
      }
    }

    withPerMethodBodyState(sym) {
      assert(vparamss.isEmpty || vparamss.tail.isEmpty,
          "Malformed parameter list: " + vparamss)
      val params = if (vparamss.isEmpty) Nil else vparamss.head.map(_.symbol)

      val methodName = encodeMethodSym(sym)
      val originalName = originalNameOfMethod(sym)

      def jsParams = params.map(genParamDef(_))

      if (primitives.isPrimitive(sym)) {
        None
      } else if (sym.is(Deferred) && currentClassSym.isNonNativeJSClass) {
        // scala-js/#4409: Do not emit abstract methods in non-native JS classes
        None
      } else if (sym.is(Deferred)) {
        Some(js.MethodDef(js.MemberFlags.empty, methodName, originalName,
            jsParams, toIRType(patchedResultType(sym)), None)(
            OptimizerHints.empty, None))
      } else if (isIgnorableDefaultParam) {
        // #11592
        None
      } else if (sym.is(Bridge) && sym.name.is(DefaultGetterName) && currentClassSym.isNonNativeJSClass) {
        /* #12572 Bridges for default accessors in non-native JS classes must not be emitted,
         * because they call another default accessor, making their entire body an
         * <undefined-param> that cannot be eliminated.
         * Such methods are never called anyway, because they are filtered out in
         * JSExportsGen.defaultGetterDenot().
         */
        None
      } else /*if (sym.isClassConstructor && isHijackedBoxedClass(sym.owner)) {
        None
      } else*/ {
        /*def isTraitImplForwarder = dd.rhs match {
          case app: Apply => foreignIsImplClass(app.symbol.owner)
          case _          => false
        }*/

        val shouldMarkInline = {
          sym.hasAnnotation(jsdefn.InlineAnnot) ||
          sym.isAnonymousFunction
        }

        val shouldMarkNoinline = {
          sym.hasAnnotation(jsdefn.NoinlineAnnot) /*&&
          !isTraitImplForwarder*/
        }

        val optimizerHints = {
          OptimizerHints.empty
            .withInline(shouldMarkInline)
            .withNoinline(shouldMarkNoinline)
        }

        val methodDef = {
          if (sym.isClassConstructor) {
            val namespace = js.MemberNamespace.Constructor
            js.MethodDef(js.MemberFlags.empty.withNamespace(namespace),
                methodName, originalName, jsParams, jstpe.NoType, Some(genStat(rhs)))(
                optimizerHints, None)
          } else {
            val namespace = if (isMethodStaticInIR(sym)) {
              if (sym.isPrivate) js.MemberNamespace.PrivateStatic
              else js.MemberNamespace.PublicStatic
            } else {
              if (sym.isPrivate) js.MemberNamespace.Private
              else js.MemberNamespace.Public
            }
            val resultIRType = toIRType(patchedResultType(sym))
            genMethodDef(namespace, methodName, originalName,
                params, resultIRType, rhs, optimizerHints)
          }
        }

        Some(methodDef)
      }
    }
  }

  /** Generates the MethodDef of a (non-constructor) method
   *
   *  Most normal methods are emitted straightforwardly. If the result
   *  type is Unit, then the body is emitted as a statement. Otherwise, it is
   *  emitted as an expression.
   *
   *  Instance methods in non-native JS classes are compiled as static methods
   *  taking an explicit parameter for their `this` value. Static methods in
   *  non-native JS classes are compiled as is, like methods in Scala classes.
   */
  private def genMethodDef(namespace: js.MemberNamespace, methodName: js.MethodIdent,
      originalName: OriginalName, paramsSyms: List[Symbol], resultIRType: jstpe.Type,
      tree: Tree, optimizerHints: OptimizerHints): js.MethodDef = {
    implicit val pos = tree.span

    val jsParams = paramsSyms.map(genParamDef(_))

    def genBody() = localNames.makeLabeledIfRequiresEnclosingReturn(resultIRType) {
      if (resultIRType == jstpe.NoType) genStat(tree)
      else genExpr(tree)
    }

    if (namespace.isStatic || !currentClassSym.isNonNativeJSClass) {
      val flags = js.MemberFlags.empty.withNamespace(namespace)
      js.MethodDef(flags, methodName, originalName, jsParams, resultIRType, Some(genBody()))(
            optimizerHints, None)
    } else {
      val thisLocalIdent = freshLocalIdent("this")
      withScopedVars(
        thisLocalVarIdent := Some(thisLocalIdent)
      ) {
        val staticNamespace =
          if (namespace.isPrivate) js.MemberNamespace.PrivateStatic
          else js.MemberNamespace.PublicStatic
        val flags =
          js.MemberFlags.empty.withNamespace(staticNamespace)
        val thisParamDef = js.ParamDef(thisLocalIdent, thisOriginalName,
            jstpe.AnyType, mutable = false)

        js.MethodDef(flags, methodName, originalName,
            thisParamDef :: jsParams, resultIRType, Some(genBody()))(
            optimizerHints, None)
      }
    }
  }

  // ParamDefs ---------------------------------------------------------------

  def genParamDef(sym: Symbol): js.ParamDef =
    genParamDef(sym, toIRType(sym.info))

  private def genParamDef(sym: Symbol, ptpe: jstpe.Type): js.ParamDef =
    genParamDef(sym, ptpe, sym.span)

  private def genParamDef(sym: Symbol, pos: Position): js.ParamDef =
    genParamDef(sym, toIRType(sym.info), pos)

  private def genParamDef(sym: Symbol, ptpe: jstpe.Type, pos: Position): js.ParamDef = {
    js.ParamDef(encodeLocalSym(sym)(implicitly, pos, implicitly),
        originalNameOfLocal(sym), ptpe, mutable = false)(pos)
  }

  // Generate statements and expressions -------------------------------------

  /** Gen JS code for a tree in statement position (in the IR).
   */
  private def genStat(tree: Tree): js.Tree = {
    exprToStat(genStatOrExpr(tree, isStat = true))
  }

  /** Turn a JavaScript expression of type Unit into a statement */
  private def exprToStat(tree: js.Tree): js.Tree = {
    /* Any JavaScript expression is also a statement, but at least we get rid
     * of some pure expressions that come from our own codegen.
     */
    implicit val pos = tree.pos
    tree match {
      case js.Block(stats :+ expr) =>
        js.Block(stats :+ exprToStat(expr))
      case _:js.Literal | _:js.This | _:js.VarRef =>
        js.Skip()
      case _ =>
        tree
    }
  }

  /** Gen JS code for a tree in expression position (in the IR).
   */
  private def genExpr(tree: Tree): js.Tree = {
    val result = genStatOrExpr(tree, isStat = false)
    assert(result.tpe != jstpe.NoType,
        s"genExpr($tree) returned a tree with type NoType at pos ${tree.span}")
    result
  }

  def genExpr(name: JSName)(implicit pos: SourcePosition): js.Tree = name match {
    case JSName.Literal(name) => js.StringLiteral(name)
    case JSName.Computed(sym) => genComputedJSName(sym)
  }

  private def genComputedJSName(sym: Symbol)(implicit pos: SourcePosition): js.Tree = {
    /* By construction (i.e. restriction in PrepJSInterop), we know that sym
     * must be a static method.
     * Therefore, at this point, we can invoke it by loading its owner and
     * calling it.
     */
    def moduleOrGlobalScope = genLoadModuleOrGlobalScope(sym.owner)
    def module = genLoadModule(sym.owner)

    if (sym.owner.isJSType) {
      if (!sym.owner.isNonNativeJSClass || sym.isJSExposed)
        genApplyJSMethodGeneric(sym, moduleOrGlobalScope, args = Nil, isStat = false)
      else
        genApplyJSClassMethod(module, sym, arguments = Nil)
    } else {
      genApplyMethod(module, sym, arguments = Nil)
    }
  }

  /** Gen JS code for a tree in expression position (in the IR) or the
   *  global scope.
   */
  def genExprOrGlobalScope(tree: Tree): MaybeGlobalScope = {
    implicit def pos: SourcePosition = tree.sourcePos

    tree match {
      case _: This =>
        val sym = tree.symbol
        if (sym != currentClassSym.get && sym.is(Module))
          genLoadModuleOrGlobalScope(sym)
        else
          MaybeGlobalScope.NotGlobalScope(genExpr(tree))

      case _:Ident | _:Select =>
        val sym = tree.symbol
        if (sym.is(Module)) {
          assert(!sym.is(PackageClass), "Cannot use package as value: " + tree)
          genLoadModuleOrGlobalScope(sym)
        } else {
          MaybeGlobalScope.NotGlobalScope(genExpr(tree))
        }

      case Apply(fun, _) =>
        if (fun.symbol == jsdefn.JSDynamic_global)
          MaybeGlobalScope.GlobalScope(pos)
        else
          MaybeGlobalScope.NotGlobalScope(genExpr(tree))

      case _ =>
        MaybeGlobalScope.NotGlobalScope(genExpr(tree))
    }
  }

  /** Gen JS code for a tree in statement or expression position (in the IR).
   *
   *  This is the main transformation method. Each node of the Scala AST
   *  is transformed into an equivalent portion of the JS AST.
   */
  private def genStatOrExpr(tree: Tree, isStat: Boolean): js.Tree = {
    implicit val pos: SourcePosition = tree.sourcePos

    report.debuglog("  " + tree)
    report.debuglog("")

    tree match {
      /** Local val or var declaration */
      case tree @ ValDef(name, _, _) =>
        val sym = tree.symbol
        val rhs = tree.rhs
        val rhsTree = genExpr(rhs)

        rhsTree match {
          case js.Transient(UndefinedParam) =>
            /* This is an intermediate assignment for default params on a
             * js.Any. Add the symbol to the corresponding set to inform
             * the Ident resolver how to replace it and don't emit the symbol.
             */
            undefinedDefaultParams += sym
            js.Skip()
          case _ =>
            js.VarDef(encodeLocalSym(sym), originalNameOfLocal(sym),
                toIRType(sym.info), sym.is(Mutable), rhsTree)
        }

      case If(cond, thenp, elsep) =>
        val tpe =
          if (isStat) jstpe.NoType
          else toIRType(tree.tpe)

        js.If(genExpr(cond), genStatOrExpr(thenp, isStat),
            genStatOrExpr(elsep, isStat))(tpe)

      case Labeled(bind, expr) =>
        js.Labeled(encodeLabelSym(bind.symbol), toIRType(tree.tpe), genStatOrExpr(expr, isStat))

      case Return(expr, from) =>
        val fromSym = from.symbol
        val label =
          if (fromSym.is(Label)) encodeLabelSym(fromSym)
          else localNames.get.getEnclosingReturnLabel()
        js.Return(toIRType(expr.tpe) match {
          case jstpe.NoType => js.Block(genStat(expr), js.Undefined())
          case _            => genExpr(expr)
        }, label)

      case WhileDo(cond, body) =>
        val genCond =
          if (cond == EmptyTree) js.BooleanLiteral(true)
          else genExpr(cond)
        js.While(genCond, genStat(body))

      case t: Try =>
        genTry(t, isStat)

      case app: Apply =>
        genApply(app, isStat)

      case app: TypeApply =>
        genTypeApply(app)

      /*case app: ApplyDynamic =>
        genApplyDynamic(app)*/

      case tree: This =>
        val currentClass = currentClassSym.get
        val symIsModuleClass = tree.symbol.is(ModuleClass)
        assert(tree.symbol == currentClass || symIsModuleClass,
            s"Trying to access the this of another class: tree.symbol = ${tree.symbol}, class symbol = $currentClass")
        if (symIsModuleClass && tree.symbol != currentClass)
          genLoadModule(tree.symbol)
        else
          genThis()

      case Select(qualifier, _) =>
        val sym = tree.symbol
        if (sym.is(Module)) {
          assert(!sym.is(Package), "Cannot use package as value: " + tree)
          genLoadModule(sym)
        } else if (sym.is(JavaStatic)) {
          genLoadStaticField(sym)
        } else if (sym.hasAnnotation(jsdefn.JSNativeAnnot)) {
          genJSNativeMemberSelect(tree)
        } else {
          val (field, boxed) = genAssignableField(sym, qualifier)
          if (boxed) unbox(field, atPhase(elimErasedValueTypePhase)(sym.info))
          else field
        }

      case tree: Ident =>
        desugarIdent(tree).fold[js.Tree] {
          val sym = tree.symbol
          assert(!sym.is(Package), "Cannot use package as value: " + tree)
          if (sym.is(Module)) {
            genLoadModule(sym)
          } else if (undefinedDefaultParams.contains(sym)) {
            /* This is a default parameter whose assignment was moved to
             * a local variable. Put an undefined param instead.
             */
            js.Transient(UndefinedParam)
          } else {
            genVarRef(sym)
          }
        } { select =>
          genStatOrExpr(select, isStat)
        }

      case Literal(value) =>
        import Constants._
        value.tag match {
          case UnitTag =>
            js.Skip()
          case BooleanTag =>
            js.BooleanLiteral(value.booleanValue)
          case ByteTag =>
            js.ByteLiteral(value.byteValue)
          case ShortTag =>
            js.ShortLiteral(value.shortValue)
          case CharTag =>
            js.CharLiteral(value.charValue)
          case IntTag =>
            js.IntLiteral(value.intValue)
          case LongTag =>
            js.LongLiteral(value.longValue)
          case FloatTag =>
            js.FloatLiteral(value.floatValue)
          case DoubleTag =>
            js.DoubleLiteral(value.doubleValue)
          case StringTag =>
            js.StringLiteral(value.stringValue)
          case NullTag =>
            js.Null()
          case ClazzTag =>
            genClassConstant(value.typeValue)
        }

      case Block(stats, expr) =>
        // #15419 Collapse { <undefined-param>; BoxedUnit } to <undefined-param>
        val genStatsAndExpr0 = stats.map(genStat(_)) :+ genStatOrExpr(expr, isStat)
        val genStatsAndExpr = genStatsAndExpr0 match {
          case (undefParam @ js.Transient(UndefinedParam)) :: js.Undefined() :: Nil =>
            undefParam :: Nil
          case _ =>
            genStatsAndExpr0
        }
        js.Block(genStatsAndExpr)

      case Typed(expr, _) =>
        expr match {
          case _: Super => genThis()
          case _        => genExpr(expr)
        }

      case Assign(lhs0, rhs) =>
        val sym = lhs0.symbol
        if (sym.is(JavaStaticTerm) && sym.source != ctx.compilationUnit.source)
          throw new FatalError(s"Assignment to static member ${sym.fullName} not supported")
        def genRhs = genExpr(rhs)
        val lhs = lhs0 match {
          case lhs: Ident => desugarIdent(lhs).getOrElse(lhs)
          case lhs => lhs
        }
        lhs match {
          case lhs: Select =>
            val qualifier = lhs.qualifier

            def ctorAssignment = (
                currentMethodSym.get.name == nme.CONSTRUCTOR &&
                currentMethodSym.get.owner == qualifier.symbol &&
                qualifier.isInstanceOf[This]
            )
            // TODO This fails for OFFSET$x fields. Re-enable when we can.
            /*if (!sym.is(Mutable) && !ctorAssignment)
              throw new FatalError(s"Assigning to immutable field ${sym.fullName} at $pos")*/

            if (sym.hasAnnotation(jsdefn.JSNativeAnnot)) {
              /* This is an assignment to a @js.native field. Since we reject
               * `@js.native var`s as compile errors, this can only happen in
               * the constructor of the enclosing object.
               * We simply ignore the assignment, since the field will not be
               * emitted at all.
               */
              js.Skip()
            } else {
              val (field, boxed) = genAssignableField(sym, qualifier)
              if (boxed) {
                val genBoxedRhs = box(genRhs, atPhase(elimErasedValueTypePhase)(sym.info))
                js.Assign(field, genBoxedRhs)
              } else {
                js.Assign(field, genRhs)
              }
            }

          case _ =>
            js.Assign(genVarRef(sym), genRhs)
        }

      /** Array constructor */
      case javaSeqLiteral: JavaSeqLiteral =>
        genJavaSeqLiteral(javaSeqLiteral)

      /** A Match reaching the backend is supposed to be optimized as a switch */
      case mtch: Match =>
        genMatch(mtch, isStat)

      case tree: Closure =>
        genClosure(tree)

      case EmptyTree =>
        js.Skip()

      case _ =>
        throw new FatalError("Unexpected tree in genExpr: " +
            tree + "/" + tree.getClass + " at: " + (tree.span: Position))
    }
  } // end of genStatOrExpr()

  private def qualifierOf(fun: Tree): Tree = fun match {
    case fun: Ident =>
      fun.tpe match {
        case TermRef(prefix: TermRef, _) => tpd.ref(prefix)
        case TermRef(prefix: ThisType, _) => tpd.This(prefix.cls)
      }
    case Select(qualifier, _) =>
      qualifier
    case TypeApply(fun, _) =>
      qualifierOf(fun)
  }

  /** Gen JS this of the current class.
   *  Normally encoded straightforwardly as a JS this.
   *  But must be replaced by the `thisLocalVarIdent` local variable if there
   *  is one.
   */
  private def genThis()(implicit pos: Position): js.Tree = {
    /*if (tryingToGenMethodAsJSFunction) {
      throw new CancelGenMethodAsJSFunction(
          "Trying to generate `this` inside the body")
    }*/

    thisLocalVarIdent.fold[js.Tree] {
      js.This()(currentThisType)
    } { thisLocalIdent =>
      js.VarRef(thisLocalIdent)(currentThisType)
    }
  }

  /** Gen IR code for a `try..catch` or `try..finally` block.
   *
   *  `try..finally` blocks are compiled straightforwardly to `try..finally`
   *  blocks of the IR.
   *
   *  `try..catch` blocks are a bit more subtle, as the IR does not have
   *  type-based selection of exceptions to catch. We thus encode explicitly
   *  the type tests, like in:
   *
   *  ```
   *  try { ... }
   *  catch (e) {
   *    if (e.isInstanceOf[IOException]) { ... }
   *    else if (e.isInstanceOf[Exception]) { ... }
   *    else {
   *      throw e; // default, re-throw
   *    }
   *  }
   *  ```
   *
   *  In addition, there are provisions to handle catching JavaScript
   *  exceptions (which do not extend `Throwable`) as wrapped in a
   *  `js.JavaScriptException`.
   */
  private def genTry(tree: Try, isStat: Boolean): js.Tree = {
    implicit val pos: SourcePosition = tree.sourcePos
    val Try(block, catches, finalizer) = tree

    val blockAST = genStatOrExpr(block, isStat)

    val resultType =
      if (isStat) jstpe.NoType
      else toIRType(tree.tpe)

    val handled =
      if (catches.isEmpty) blockAST
      else genTryCatch(blockAST, catches, resultType, isStat)

    genStat(finalizer) match {
      case js.Skip() => handled
      case ast       => js.TryFinally(handled, ast)
    }
  }

  private def genTryCatch(body: js.Tree, catches: List[CaseDef],
      resultType: jstpe.Type,
      isStat: Boolean)(implicit pos: SourcePosition): js.Tree = {
    val exceptIdent = freshLocalIdent("e")
    val origExceptVar = js.VarRef(exceptIdent)(jstpe.AnyType)

    val mightCatchJavaScriptException = catches.exists { caseDef =>
      caseDef.pat match {
        case Typed(Ident(nme.WILDCARD), tpt) =>
          isMaybeJavaScriptException(tpt.tpe)
        case Ident(nme.WILDCARD) =>
          true
        case pat @ Bind(_, _) =>
          isMaybeJavaScriptException(pat.symbol.info)
      }
    }

    val (exceptValDef, exceptVar) = if (mightCatchJavaScriptException) {
      val valDef = js.VarDef(freshLocalIdent("e"), NoOriginalName,
          encodeClassType(defn.ThrowableClass), mutable = false, js.WrapAsThrowable(origExceptVar))
      (valDef, valDef.ref)
    } else {
      (js.Skip(), origExceptVar)
    }

    val elseHandler: js.Tree = js.Throw(origExceptVar)

    val handler = catches.foldRight(elseHandler) { (caseDef, elsep) =>
      implicit val pos: SourcePosition = caseDef.sourcePos
      val CaseDef(pat, _, body) = caseDef

      // Extract exception type and variable
      val (tpe, boundVar) = (pat match {
        case Typed(Ident(nme.WILDCARD), tpt) =>
          (tpt.tpe, None)
        case Ident(nme.WILDCARD) =>
          (defn.ThrowableType, None)
        case Bind(_, _) =>
          val ident = encodeLocalSym(pat.symbol)
          val origName = originalNameOfLocal(pat.symbol)
          (pat.symbol.info, Some(ident, origName))
      })

      // Generate the body that must be executed if the exception matches
      val bodyWithBoundVar = (boundVar match {
        case None =>
          genStatOrExpr(body, isStat)
        case Some((boundVarIdent, boundVarOriginalName)) =>
          val castException = genAsInstanceOf(exceptVar, tpe)
          js.Block(
              js.VarDef(boundVarIdent, boundVarOriginalName, toIRType(tpe),
                  mutable = false, castException),
              genStatOrExpr(body, isStat))
      })

      // Generate the test
      if (tpe =:= defn.ThrowableType) {
        bodyWithBoundVar
      } else {
        val cond = genIsInstanceOf(exceptVar, tpe)
        js.If(cond, bodyWithBoundVar, elsep)(resultType)
      }
    }

    js.TryCatch(body, exceptIdent, NoOriginalName,
        js.Block(exceptValDef, handler))(resultType)
  }

  /** Gen JS code for an Apply node (method call)
   *
   *  There's a whole bunch of varieties of Apply nodes: regular method
   *  calls, super calls, constructor calls, isInstanceOf/asInstanceOf,
   *  primitives, JS calls, etc. They are further dispatched in here.
   */
  private def genApply(tree: Apply, isStat: Boolean): js.Tree = {
    implicit val pos = tree.span
    val args = tree.args
    val sym = tree.fun.symbol

    /* Is the method a JS default accessor, which should become an
     * `UndefinedParam` rather than being compiled normally.
     *
     * This is true iff one of the following conditions apply:
     * - It is a constructor default param for the constructor of a JS class.
     * - It is a default param of an instance method of a native JS type.
     * - It is a default param of an instance method of a non-native JS type
     *   and the attached method is exposed.
     * - It is a default param for a native JS def.
     *
     * This is different than `isIgnorableDefaultParam` in
     * `genMethodWithCurrentLocalNameScope`: we include here the default
     * accessors of *non-native* JS types (unless the corresponding methods are
     * not exposed). We also need to handle non-constructor members of native
     * JS types.
     */
    def isJSDefaultParam: Boolean = {
      sym.name.is(DefaultGetterName) && {
        val info = new DefaultParamInfo(sym)
        if (info.isForConstructor) {
          /* This is a default accessor for a constructor parameter. Check
           * whether the attached constructor is a JS constructor, which is
           * the case iff the linked class is a JS type.
           */
          info.constructorOwner.isJSType
        } else {
          if (sym.owner.isJSType) {
            /* The default accessor is in a JS type. It is a JS default
             * param iff the enclosing class is native or the attached method
             * is exposed.
             */
            !sym.owner.isNonNativeJSClass || info.attachedMethod.isJSExposed
          } else {
            /* The default accessor is in a Scala type. It is a JS default
             * param iff the attached method is a native JS def. This can
             * only happen if the owner is a module class, which we test
             * first as a fast way out.
             */
            sym.owner.is(ModuleClass) && info.attachedMethod.hasAnnotation(jsdefn.JSNativeAnnot)
          }
        }
      }
    }

    tree.fun match {
      case _ if isJSDefaultParam =>
        js.Transient(UndefinedParam)

      case Select(Super(_, _), _) =>
        genSuperCall(tree, isStat)

      case Select(New(_), nme.CONSTRUCTOR) =>
        genApplyNew(tree)

      case _ =>
        if (primitives.isPrimitive(tree)) {
          genPrimitiveOp(tree, isStat)
        } else if (Erasure.Boxing.isBox(sym)) {
          // Box a primitive value (cannot be Unit)
          val arg = args.head
          makePrimitiveBox(genExpr(arg), arg.tpe)
        } else if (Erasure.Boxing.isUnbox(sym)) {
          // Unbox a primitive value (cannot be Unit)
          val arg = args.head
          makePrimitiveUnbox(genExpr(arg), tree.tpe)
        } else {
          genNormalApply(tree, isStat)
        }
    }
  }

  /** Gen JS code for a super call, of the form Class.super[mix].fun(args).
   *
   *  This does not include calls defined in mixin traits, as these are
   *  already desugared by the 'mixin' phase. Only calls to super classes
   *  remain.
   *
   *  Since a class has exactly one direct superclass, and calling a method
   *  two classes above the current one is invalid in Scala, the `mix` item is
   *  irrelevant.
   */
  private def genSuperCall(tree: Apply, isStat: Boolean): js.Tree = {
    implicit val pos = tree.span
    val Apply(fun @ Select(sup @ Super(qual, _), _), args) = tree: @unchecked
    val sym = fun.symbol

    if (sym == defn.Any_getClass) {
      // The only primitive that is also callable as super call
      js.GetClass(genThis())
    } else if (currentClassSym.isNonNativeJSClass) {
      genJSSuperCall(tree, isStat)
    } else {
      /* #3013 `qual` can be `this.$outer()` in some cases since Scala 2.12,
       * so we call `genExpr(qual)`, not just `genThis()`.
       */
      val superCall = genApplyMethodStatically(
          genExpr(qual), sym, genActualArgs(sym, args))

      // Initialize the module instance just after the super constructor call.
      if (isStaticModule(currentClassSym) && !isModuleInitialized.get.value &&
          currentMethodSym.get.isClassConstructor) {
        isModuleInitialized.get.value = true
        val className = encodeClassName(currentClassSym)
        val thisType = jstpe.ClassType(className)
        val initModule = js.StoreModule(className, js.This()(thisType))
        js.Block(superCall, initModule)
      } else {
        superCall
      }
    }
  }

  /** Gen JS code for a constructor call (new).
   *  Further refined into:
   *  * new String(...)
   *  * new of a hijacked boxed class
   *  * new of an anonymous function class that was recorded as JS function
   *  * new of a raw JS class
   *  * new Array
   *  * regular new
   */
  private def genApplyNew(tree: Apply): js.Tree = {
    implicit val pos: SourcePosition = tree.sourcePos

    val Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) = tree: @unchecked
    val ctor = fun.symbol
    val tpe = tpt.tpe

    assert(ctor.isClassConstructor,
        "'new' call to non-constructor: " + ctor.name)

    val clsSym = tpe.typeSymbol

    if (isHijackedClass(clsSym)) {
      genNewHijackedClass(clsSym, ctor, args.map(genExpr))
    } else /*if (translatedAnonFunctions contains tpe.typeSymbol) {
      val functionMaker = translatedAnonFunctions(tpe.typeSymbol)
      functionMaker(args map genExpr)
    } else*/ if (clsSym.isJSType) {
      genNewJSClass(tree)
    } else {
      toTypeRef(tpe) match {
        case jstpe.ClassRef(className) =>
          js.New(className, encodeMethodSym(ctor), genActualArgs(ctor, args))

        case other =>
          throw new FatalError(s"Non ClassRef cannot be instantiated: $other")
      }
    }
  }

  /** Gen JS code for a call to a constructor of a hijacked class.
   *  Reroute them to the `new` method with the same signature in the
   *  companion object.
   */
  private def genNewHijackedClass(clazz: Symbol, ctor: Symbol,
      args: List[js.Tree])(implicit pos: SourcePosition): js.Tree = {

    val className = encodeClassName(clazz)
    val initName = encodeMethodSym(ctor).name
    val newName = MethodName(newSimpleMethodName, initName.paramTypeRefs,
        jstpe.ClassRef(className))
    val newMethodIdent = js.MethodIdent(newName)

    js.ApplyStatic(js.ApplyFlags.empty, className, newMethodIdent, args)(
        jstpe.ClassType(className))
  }

  /** Gen JS code for a new of a JS class (subclass of `js.Any`). */
  private def genNewJSClass(tree: Apply): js.Tree = {
    acquireContextualJSClassValue { jsClassValue =>
      implicit val pos: Position = tree.span

      val Apply(fun @ Select(New(tpt), _), args) = tree: @unchecked
      val cls = tpt.tpe.typeSymbol
      val ctor = fun.symbol

      val nestedJSClass = cls.isNestedJSClass
      assert(jsClassValue.isDefined == nestedJSClass,
          s"$cls at $pos: jsClassValue.isDefined = ${jsClassValue.isDefined} " +
          s"but isInnerNonNativeJSClass = $nestedJSClass")

      def genArgs: List[js.TreeOrJSSpread] = genActualJSArgs(ctor, args)
      def genArgsAsClassCaptures: List[js.Tree] = args.map(genExpr)

      jsClassValue.fold {
        // Static JS class (by construction, it cannot be a module class, as their News do not reach the back-end)
        if (cls == jsdefn.JSObjectClass && args.isEmpty)
          js.JSObjectConstr(Nil)
        else if (cls == jsdefn.JSArrayClass && args.isEmpty)
          js.JSArrayConstr(Nil)
        else
          js.JSNew(genLoadJSConstructor(cls), genArgs)
      } { jsClassVal =>
        // Nested JS class
        if (cls.isAnonymousClass)
          genNewAnonJSClass(cls, jsClassVal, genArgsAsClassCaptures)(fun.span)
        else if (atPhase(erasurePhase)(cls.is(ModuleClass))) // LambdaLift removes the ModuleClass flag of lifted classes
          js.JSNew(js.CreateJSClass(encodeClassName(cls), jsClassVal :: genArgsAsClassCaptures), Nil)
        else
          js.JSNew(jsClassVal, genArgs)
      }
    }
  }

  /** Generate an instance of an anonymous (non-lambda) JS class inline
   *
   *  @param sym Class to generate the instance of
   *  @param jsSuperClassValue JS class value of the super class
   *  @param args Arguments to the Scala constructor, which map to JS class captures
   *  @param pos Position of the original New tree
   */
  private def genNewAnonJSClass(sym: Symbol, jsSuperClassValue: js.Tree, args: List[js.Tree])(
      implicit pos: Position): js.Tree = {
    assert(sym.isAnonymousClass,
        s"Generating AnonJSClassNew of non anonymous JS class ${sym.fullName}")

    // Find the TypeDef for this anonymous class and generate it
    val typeDef = consumeLazilyGeneratedAnonClass(sym)
    val originalClassDef = resetAllScopedVars {
      withScopedVars(
          currentClassSym := sym
      ) {
        genNonNativeJSClass(typeDef)
      }
    }

    // Partition class members.
    val privateFieldDefs = mutable.ListBuffer.empty[js.FieldDef]
    val classDefMembers = mutable.ListBuffer.empty[js.MemberDef]
    val instanceMembers = mutable.ListBuffer.empty[js.MemberDef]
    var constructor: Option[js.JSConstructorDef] = None

    originalClassDef.memberDefs.foreach {
      case fdef: js.FieldDef =>
        privateFieldDefs += fdef

      case fdef: js.JSFieldDef =>
        instanceMembers += fdef

      case mdef: js.MethodDef =>
        assert(mdef.flags.namespace.isStatic,
            "Non-static, unexported method in non-native JS class")
        classDefMembers += mdef

      case cdef: js.JSConstructorDef =>
        assert(constructor.isEmpty, "two ctors in class")
        constructor = Some(cdef)

      case mdef: js.JSMethodDef =>
        assert(!mdef.flags.namespace.isStatic, "Exported static method")
        instanceMembers += mdef

      case property: js.JSPropertyDef =>
        instanceMembers += property

      case nativeMemberDef: js.JSNativeMemberDef =>
        throw new FatalError("illegal native JS member in JS class at " + nativeMemberDef.pos)
    }

    assert(originalClassDef.topLevelExportDefs.isEmpty,
        "Found top-level exports in anonymous JS class at " + pos)

    // Make new class def with static members
    val newClassDef = {
      implicit val pos = originalClassDef.pos
      val parent = js.ClassIdent(jsNames.ObjectClass)
      js.ClassDef(originalClassDef.name, originalClassDef.originalName,
          ClassKind.AbstractJSType, None, Some(parent), interfaces = Nil,
          jsSuperClass = None, jsNativeLoadSpec = None,
          classDefMembers.toList, Nil)(
          originalClassDef.optimizerHints)
    }

    generatedClasses += newClassDef

    // Construct inline class definition

    val jsClassCaptures = originalClassDef.jsClassCaptures.getOrElse {
      throw new AssertionError(s"no class captures for anonymous JS class at $pos")
    }
    val js.JSConstructorDef(_, ctorParams, ctorRestParam, ctorBody) = constructor.getOrElse {
      throw new AssertionError("No ctor found")
    }
    assert(ctorParams.isEmpty && ctorRestParam.isEmpty,
        s"non-empty constructor params for anonymous JS class at $pos")

    /* The first class capture is always a reference to the super class.
     * This is enforced by genJSClassCapturesAndConstructor.
     */
    def jsSuperClassRef(implicit pos: ir.Position): js.VarRef =
      jsClassCaptures.head.ref

    /* The `this` reference.
     * FIXME This could clash with a local variable of the constructor or a JS
     * class capture. It seems Scala 2 has the same vulnerability. How do we
     * avoid this?
     */
    val selfName = freshLocalIdent("this")(pos)
    def selfRef(implicit pos: ir.Position) =
      js.VarRef(selfName)(jstpe.AnyType)

    def memberLambda(params: List[js.ParamDef], restParam: Option[js.ParamDef], body: js.Tree)(implicit pos: ir.Position): js.Closure =
      js.Closure(arrow = false, captureParams = Nil, params, restParam, body, captureValues = Nil)

    val memberDefinitions0 = instanceMembers.toList.map {
      case fdef: js.FieldDef =>
        throw new AssertionError("unexpected FieldDef")

      case fdef: js.JSFieldDef =>
        implicit val pos = fdef.pos
        js.Assign(js.JSSelect(selfRef, fdef.name), jstpe.zeroOf(fdef.ftpe))

      case mdef: js.MethodDef =>
        throw new AssertionError("unexpected MethodDef")

      case cdef: js.JSConstructorDef =>
        throw new AssertionError("unexpected JSConstructorDef")

      case mdef: js.JSMethodDef =>
        implicit val pos = mdef.pos
        val impl = memberLambda(mdef.args, mdef.restParam, mdef.body)
        js.Assign(js.JSSelect(selfRef, mdef.name), impl)

      case pdef: js.JSPropertyDef =>
        implicit val pos = pdef.pos
        val optGetter = pdef.getterBody.map { body =>
          js.StringLiteral("get") -> memberLambda(params = Nil, restParam = None, body)
        }
        val optSetter = pdef.setterArgAndBody.map { case (arg, body) =>
          js.StringLiteral("set") -> memberLambda(params = arg :: Nil, restParam = None, body)
        }
        val descriptor = js.JSObjectConstr(
            optGetter.toList :::
            optSetter.toList :::
            List(js.StringLiteral("configurable") -> js.BooleanLiteral(true))
        )
        js.JSMethodApply(js.JSGlobalRef("Object"),
            js.StringLiteral("defineProperty"),
            List(selfRef, pdef.name, descriptor))

      case nativeMemberDef: js.JSNativeMemberDef =>
        throw new FatalError("illegal native JS member in JS class at " + nativeMemberDef.pos)
    }

    val memberDefinitions = if (privateFieldDefs.isEmpty) {
      memberDefinitions0
    } else {
      /* Private fields, declared in FieldDefs, are stored in a separate
       * object, itself stored as a non-enumerable field of the `selfRef`.
       * The name of that field is retrieved at
       * `scala.scalajs.runtime.privateFieldsSymbol()`, and is a Symbol if
       * supported, or a randomly generated string that has the same enthropy
       * as a UUID (i.e., 128 random bits).
       *
       * This encoding solves two issues:
       *
       * - Hide private fields in anonymous JS classes from `JSON.stringify`
       *   and other cursory inspections in JS (#2748).
       * - Get around the fact that abstract JS types cannot declare
       *   FieldDefs (#3777).
       */
      val fieldsObjValue = {
        js.JSObjectConstr(privateFieldDefs.toList.map { fdef =>
          implicit val pos = fdef.pos
          js.StringLiteral(fdef.name.name.nameString) -> jstpe.zeroOf(fdef.ftpe)
        })
      }
      val definePrivateFieldsObj = {
        /* Object.defineProperty(selfRef, privateFieldsSymbol, {
         *   value: fieldsObjValue
         * });
         *
         * `writable`, `configurable` and `enumerable` are false by default.
         */
        js.JSMethodApply(
            js.JSGlobalRef("Object"),
            js.StringLiteral("defineProperty"),
            List(
                selfRef,
                genPrivateFieldsSymbol()(using sym.sourcePos),
                js.JSObjectConstr(List(
                    js.StringLiteral("value") -> fieldsObjValue
                ))
            )
        )
      }
      definePrivateFieldsObj :: memberDefinitions0
    }

    // Transform the constructor body.
    val inlinedCtorStats: List[js.Tree] = {
      val beforeSuper = ctorBody.beforeSuper

      val superCall = {
        implicit val pos = ctorBody.superCall.pos
        val js.JSSuperConstructorCall(args) = ctorBody.superCall

        val newTree = {
          val ident = originalClassDef.superClass.getOrElse(throw new FatalError("No superclass"))
          if (args.isEmpty && ident.name == JSObjectClassName)
            js.JSObjectConstr(Nil)
          else
            js.JSNew(jsSuperClassRef, args)
        }

        val selfVarDef = js.VarDef(selfName, thisOriginalName, jstpe.AnyType, mutable = false, newTree)
        selfVarDef :: memberDefinitions
      }

      // After the super call, substitute `selfRef` for `This()`
      val afterSuper = new ir.Transformers.Transformer {
        override def transform(tree: js.Tree, isStat: Boolean): js.Tree = tree match {
          case js.This() =>
            selfRef(tree.pos)

          // Don't traverse closure boundaries
          case closure: js.Closure =>
            val newCaptureValues = closure.captureValues.map(transformExpr)
            closure.copy(captureValues = newCaptureValues)(closure.pos)

          case tree =>
            super.transform(tree, isStat)
        }
      }.transformStats(ctorBody.afterSuper)

      beforeSuper ::: superCall ::: afterSuper
    }

    val closure = js.Closure(arrow = true, jsClassCaptures, Nil, None,
        js.Block(inlinedCtorStats, selfRef), jsSuperClassValue :: args)
    js.JSFunctionApply(closure, Nil)
  }

  /** Gen JS code for a primitive method call. */
  private def genPrimitiveOp(tree: Apply, isStat: Boolean): js.Tree = {
    import dotty.tools.backend.ScalaPrimitivesOps._

    implicit val pos = tree.span

    val Apply(fun, args) = tree
    val receiver = qualifierOf(fun)

    val code = primitives.getPrimitive(tree, receiver.tpe)

    if (isArithmeticOp(code) || isLogicalOp(code) || isComparisonOp(code))
      genSimpleOp(tree, receiver :: args, code)
    else if (code == CONCAT)
      genStringConcat(tree, receiver, args)
    else if (code == HASH)
      genScalaHash(tree, receiver)
    else if (isArrayOp(code))
      genArrayOp(tree, code)
    else if (code == SYNCHRONIZED)
      genSynchronized(tree, isStat)
    else if (isCoercion(code))
      genCoercion(tree, receiver, code)
    else if (code == JSPrimitives.THROW)
      genThrow(tree, args)
    else if (JSPrimitives.isJSPrimitive(code))
      genJSPrimitive(tree, args, code, isStat)
    else
      throw new FatalError(s"Unknown primitive: ${tree.symbol.fullName} at: $pos")
  }

  /** Gen JS code for a simple operation (arithmetic, logical, or comparison) */
  private def genSimpleOp(tree: Apply, args: List[Tree], code: Int): js.Tree = {
    args match {
      case List(arg)      => genSimpleUnaryOp(tree, arg, code)
      case List(lhs, rhs) => genSimpleBinaryOp(tree, lhs, rhs, code)
      case _              => throw new FatalError("Incorrect arity for primitive")
    }
  }

  /** Gen JS code for a simple unary operation. */
  private def genSimpleUnaryOp(tree: Apply, arg: Tree, code: Int): js.Tree = {
    import dotty.tools.backend.ScalaPrimitivesOps._

    implicit val pos = tree.span

    val resultIRType = toIRType(tree.tpe)
    val genArg = adaptPrimitive(genExpr(arg), resultIRType)

    (code: @switch) match {
      case POS =>
        genArg

      case NEG =>
        (resultIRType: @unchecked) match {
          case jstpe.IntType =>
            js.BinaryOp(js.BinaryOp.Int_-, js.IntLiteral(0), genArg)
          case jstpe.LongType =>
            js.BinaryOp(js.BinaryOp.Long_-, js.LongLiteral(0), genArg)
          case jstpe.FloatType =>
            js.BinaryOp(js.BinaryOp.Float_*, js.FloatLiteral(-1.0f), genArg)
          case jstpe.DoubleType =>
            js.BinaryOp(js.BinaryOp.Double_*, js.DoubleLiteral(-1.0), genArg)
        }

      case NOT =>
        (resultIRType: @unchecked) match {
          case jstpe.IntType =>
            js.BinaryOp(js.BinaryOp.Int_^, js.IntLiteral(-1), genArg)
          case jstpe.LongType =>
            js.BinaryOp(js.BinaryOp.Long_^, js.LongLiteral(-1), genArg)
        }

      case ZNOT =>
        js.UnaryOp(js.UnaryOp.Boolean_!, genArg)

      case _ =>
        throw new FatalError("Unknown unary operation code: " + code)
    }
  }

  /** Gen JS code for a simple binary operation. */
  private def genSimpleBinaryOp(tree: Apply, lhs: Tree, rhs: Tree, code: Int): js.Tree = {
    import dotty.tools.backend.ScalaPrimitivesOps._

    implicit val pos: SourcePosition = tree.sourcePos

    val lhsIRType = toIRType(lhs.tpe)
    val rhsIRType = toIRType(rhs.tpe)

    val isShift = isShiftOp(code)

    val opType = {
      if (isShift) {
        if (lhsIRType == jstpe.LongType) jstpe.LongType
        else jstpe.IntType
      } else {
        (lhsIRType, rhsIRType) match {
          case (jstpe.DoubleType, _) | (_, jstpe.DoubleType)                          => jstpe.DoubleType
          case (jstpe.FloatType, _) | (_, jstpe.FloatType)                            => jstpe.FloatType
          case (jstpe.LongType, _) | (_, jstpe.LongType)                              => jstpe.LongType
          case (jstpe.IntType | jstpe.ByteType | jstpe.ShortType | jstpe.CharType, _) => jstpe.IntType
          case (_, jstpe.IntType | jstpe.ByteType | jstpe.ShortType | jstpe.CharType) => jstpe.IntType
          case (jstpe.BooleanType, _) | (_, jstpe.BooleanType)                        => jstpe.BooleanType
          case _                                                                      => jstpe.AnyType
        }
      }
    }

    val lsrc =
      if (opType == jstpe.AnyType) genExpr(lhs)
      else adaptPrimitive(genExpr(lhs), opType)
    val rsrc =
      if (opType == jstpe.AnyType) genExpr(rhs)
      else adaptPrimitive(genExpr(rhs), if (isShift) jstpe.IntType else opType)

    if (opType == jstpe.AnyType && isUniversalEqualityOp(code)) {
      genUniversalEqualityOp(lhs.tpe, rhs.tpe, lsrc, rsrc, code)
    } else if (code == ZOR) {
      js.If(lsrc, js.BooleanLiteral(true), rsrc)(jstpe.BooleanType)
    } else if (code == ZAND) {
      js.If(lsrc, rsrc, js.BooleanLiteral(false))(jstpe.BooleanType)
    } else {
      import js.BinaryOp._

      (opType: @unchecked) match {
        case jstpe.IntType =>
          val op = (code: @switch) match {
            case ADD => Int_+
            case SUB => Int_-
            case MUL => Int_*
            case DIV => Int_/
            case MOD => Int_%
            case OR  => Int_|
            case AND => Int_&
            case XOR => Int_^
            case LSL => Int_<<
            case LSR => Int_>>>
            case ASR => Int_>>

            case EQ => Int_==
            case NE => Int_!=
            case LT => Int_<
            case LE => Int_<=
            case GT => Int_>
            case GE => Int_>=
          }
          js.BinaryOp(op, lsrc, rsrc)

        case jstpe.FloatType =>
          def withFloats(op: Int): js.Tree =
            js.BinaryOp(op, lsrc, rsrc)

          def toDouble(value: js.Tree): js.Tree =
            js.UnaryOp(js.UnaryOp.FloatToDouble, value)

          def withDoubles(op: Int): js.Tree =
            js.BinaryOp(op, toDouble(lsrc), toDouble(rsrc))

          (code: @switch) match {
            case ADD => withFloats(Float_+)
            case SUB => withFloats(Float_-)
            case MUL => withFloats(Float_*)
            case DIV => withFloats(Float_/)
            case MOD => withFloats(Float_%)

            case EQ => withDoubles(Double_==)
            case NE => withDoubles(Double_!=)
            case LT => withDoubles(Double_<)
            case LE => withDoubles(Double_<=)
            case GT => withDoubles(Double_>)
            case GE => withDoubles(Double_>=)
          }

        case jstpe.DoubleType =>
          val op = (code: @switch) match {
            case ADD => Double_+
            case SUB => Double_-
            case MUL => Double_*
            case DIV => Double_/
            case MOD => Double_%

            case EQ => Double_==
            case NE => Double_!=
            case LT => Double_<
            case LE => Double_<=
            case GT => Double_>
            case GE => Double_>=
          }
          js.BinaryOp(op, lsrc, rsrc)

        case jstpe.LongType =>
          val op = (code: @switch) match {
            case ADD => Long_+
            case SUB => Long_-
            case MUL => Long_*
            case DIV => Long_/
            case MOD => Long_%
            case OR  => Long_|
            case XOR => Long_^
            case AND => Long_&
            case LSL => Long_<<
            case LSR => Long_>>>
            case ASR => Long_>>

            case EQ => Long_==
            case NE => Long_!=
            case LT => Long_<
            case LE => Long_<=
            case GT => Long_>
            case GE => Long_>=
          }
          js.BinaryOp(op, lsrc, rsrc)

        case jstpe.BooleanType =>
          val op = (code: @switch) match {
            case EQ  => Boolean_==
            case NE  => Boolean_!=
            case OR  => Boolean_|
            case AND => Boolean_&
            case XOR => Boolean_!=
          }
          js.BinaryOp(op, lsrc, rsrc)

        case jstpe.AnyType =>
          val op = code match {
            case ID => ===
            case NI => !==
          }
          js.BinaryOp(op, lsrc, rsrc)
      }
    }
  }

  private def adaptPrimitive(value: js.Tree, to: jstpe.Type)(
      implicit pos: Position): js.Tree = {
    genConversion(value.tpe, to, value)
  }

  /* This method corresponds to the method of the same name in
   * BCodeBodyBuilder of the JVM back-end. It ends up calling the method
   * BCodeIdiomatic.emitT2T, whose logic we replicate here.
    */
  private def genConversion(from: jstpe.Type, to: jstpe.Type, value: js.Tree)(
      implicit pos: Position): js.Tree = {
    import js.UnaryOp._

    if (from == to || from == jstpe.NothingType) {
      value
    } else if (from == jstpe.BooleanType || to == jstpe.BooleanType) {
      throw new AssertionError(s"Invalid genConversion from $from to $to")
    } else {
      def intValue = (from: @unchecked) match {
        case jstpe.IntType    => value
        case jstpe.CharType   => js.UnaryOp(CharToInt, value)
        case jstpe.ByteType   => js.UnaryOp(ByteToInt, value)
        case jstpe.ShortType  => js.UnaryOp(ShortToInt, value)
        case jstpe.LongType   => js.UnaryOp(LongToInt, value)
        case jstpe.FloatType  => js.UnaryOp(DoubleToInt, js.UnaryOp(FloatToDouble, value))
        case jstpe.DoubleType => js.UnaryOp(DoubleToInt, value)
      }

      def doubleValue = from match {
        case jstpe.DoubleType => value
        case jstpe.FloatType  => js.UnaryOp(FloatToDouble, value)
        case jstpe.LongType   => js.UnaryOp(LongToDouble, value)
        case _                => js.UnaryOp(IntToDouble, intValue)
      }

      (to: @unchecked) match {
        case jstpe.CharType =>
          js.UnaryOp(IntToChar, intValue)
        case jstpe.ByteType =>
          js.UnaryOp(IntToByte, intValue)
        case jstpe.ShortType =>
          js.UnaryOp(IntToShort, intValue)
        case jstpe.IntType =>
          intValue
        case jstpe.LongType =>
          from match {
            case jstpe.FloatType | jstpe.DoubleType =>
              js.UnaryOp(DoubleToLong, doubleValue)
            case _ =>
              js.UnaryOp(IntToLong, intValue)
          }
        case jstpe.FloatType =>
          if (from == jstpe.LongType)
            js.UnaryOp(js.UnaryOp.LongToFloat, value)
          else
            js.UnaryOp(js.UnaryOp.DoubleToFloat, doubleValue)
        case jstpe.DoubleType =>
          doubleValue
      }
    }
  }

  /** Gen JS code for a universal equality test. */
  private def genUniversalEqualityOp(ltpe: Type, rtpe: Type, lhs: js.Tree, rhs: js.Tree, code: Int)(
      implicit pos: SourcePosition): js.Tree = {

    import dotty.tools.backend.ScalaPrimitivesOps._

    val bypassEqEq = {
      // Do not call equals if we have a literal null at either side.
      lhs.isInstanceOf[js.Null] ||
      rhs.isInstanceOf[js.Null]
    }

    if (bypassEqEq) {
      js.BinaryOp(
          if (code == EQ) js.BinaryOp.=== else js.BinaryOp.!==,
          lhs, rhs)
    } else {
      val body = genEqEqPrimitive(ltpe, rtpe, lhs, rhs)
      if (code == EQ) body
      else js.UnaryOp(js.UnaryOp.Boolean_!, body)
    }
  }

  private lazy val externalEqualsNumNum: Symbol =
    defn.BoxesRunTimeModule.requiredMethod(nme.equalsNumNum)
  private lazy val externalEqualsNumChar: Symbol =
    NoSymbol // requiredMethod(BoxesRunTimeTypeRef, nme.equalsNumChar) // this method is private
  private lazy val externalEqualsNumObject: Symbol =
    defn.BoxesRunTimeModule.requiredMethod(nme.equalsNumObject)
  private lazy val externalEquals: Symbol =
    defn.BoxesRunTimeModule.info.decl(nme.equals_).suchThat(toDenot(_).info.firstParamTypes.size == 2).symbol

  /** Gen JS code for a call to Any.== */
  private def genEqEqPrimitive(ltpe: Type, rtpe: Type, lsrc: js.Tree, rsrc: js.Tree)(
      implicit pos: SourcePosition): js.Tree = {
    report.debuglog(s"$ltpe == $rtpe")
    val lsym = ltpe.typeSymbol.asClass
    val rsym = rtpe.typeSymbol.asClass

    /* True if the equality comparison is between values that require the
     * use of the rich equality comparator
     * (scala.runtime.BoxesRunTime.equals).
     * This is the case when either side of the comparison might have a
     * run-time type subtype of java.lang.Number or java.lang.Character,
     * **which includes when either is a JS type**.
     * When it is statically known that both sides are equal and subtypes of
     * Number or Character, not using the rich equality is possible (their
     * own equals method will do ok), except for java.lang.Float and
     * java.lang.Double: their `equals` have different behavior around `NaN`
     * and `-0.0`, see Javadoc (scala-dev#329, scala-js#2799).
     */
    val mustUseAnyComparator: Boolean = {
      lsym.isJSType || rsym.isJSType || {
        val p = ctx.platform
        p.isMaybeBoxed(lsym) && p.isMaybeBoxed(rsym) && {
          val areSameFinals = lsym.is(Final) && rsym.is(Final) && (ltpe =:= rtpe)
          !areSameFinals || lsym == defn.BoxedFloatClass || lsym == defn.BoxedDoubleClass
        }
      }
    }

    if (mustUseAnyComparator) {
      val equalsMethod: Symbol = {
        val ptfm = ctx.platform
        if (lsym.derivesFrom(defn.BoxedNumberClass)) {
          if (rsym.derivesFrom(defn.BoxedNumberClass)) externalEqualsNumNum
          else if (rsym.derivesFrom(defn.BoxedCharClass)) externalEqualsNumObject // will be externalEqualsNumChar in 2.12, SI-9030
          else externalEqualsNumObject
        } else externalEquals
      }
      genApplyStatic(equalsMethod, List(lsrc, rsrc))
    } else {
      // if (lsrc eq null) rsrc eq null else lsrc.equals(rsrc)
      if (lsym == defn.StringClass) {
        // String.equals(that) === (this eq that)
        js.BinaryOp(js.BinaryOp.===, lsrc, rsrc)
      } else {
        /* This requires to evaluate both operands in local values first.
         * The optimizer will eliminate them if possible.
         */
        val ltemp = js.VarDef(freshLocalIdent(), NoOriginalName, lsrc.tpe, mutable = false, lsrc)
        val rtemp = js.VarDef(freshLocalIdent(), NoOriginalName, rsrc.tpe, mutable = false, rsrc)
        js.Block(
            ltemp,
            rtemp,
            js.If(js.BinaryOp(js.BinaryOp.===, ltemp.ref, js.Null()),
                js.BinaryOp(js.BinaryOp.===, rtemp.ref, js.Null()),
                genApplyMethod(ltemp.ref, defn.Any_equals, List(rtemp.ref)))(
                jstpe.BooleanType))
      }
    }
  }

  /** Gen JS code for string concatenation.
   */
  private def genStringConcat(tree: Apply, receiver: Tree,
      args: List[Tree]): js.Tree = {
    implicit val pos = tree.span

    js.BinaryOp(js.BinaryOp.String_+, genExpr(receiver), genExpr(args.head))
  }

  /** Gen JS code for a call to Any.## */
  private def genScalaHash(tree: Apply, receiver: Tree): js.Tree = {
    implicit val pos: SourcePosition = tree.sourcePos

    genModuleApplyMethod(defn.ScalaRuntimeModule.requiredMethod(nme.hash_),
        List(genExpr(receiver)))
  }

  /** Gen JS code for an array operation (get, set or length) */
  private def genArrayOp(tree: Tree, code: Int): js.Tree = {
    import dotty.tools.backend.ScalaPrimitivesOps._

    implicit val pos = tree.span

    val Apply(fun, args) = tree: @unchecked
    val arrayObj = qualifierOf(fun)

    val genArray = genExpr(arrayObj)
    val genArgs = args.map(genExpr)

    def elementType: Type = arrayObj.tpe.widenDealias match {
      case defn.ArrayOf(el)  => el
      case JavaArrayType(el) => el
      case tpe =>
        val msg = em"expected Array $tpe"
        report.error(msg)
        ErrorType(msg)
    }

    def genSelect(): js.AssignLhs =
      js.ArraySelect(genArray, genArgs(0))(toIRType(elementType))

    if (isArrayGet(code)) {
      // get an item of the array
      assert(args.length == 1,
          s"Array get requires 1 argument, found ${args.length} in $tree")
      genSelect()
    } else if (isArraySet(code)) {
      // set an item of the array
      assert(args.length == 2,
          s"Array set requires 2 arguments, found ${args.length} in $tree")
      js.Assign(genSelect(), genArgs(1))
    } else {
      // length of the array
      js.ArrayLength(genArray)
    }
  }

  /** Gen JS code for a call to AnyRef.synchronized */
  private def genSynchronized(tree: Apply, isStat: Boolean): js.Tree = {
    /* JavaScript is single-threaded, so we can drop the
     * synchronization altogether.
     */
    val Apply(fun, List(arg)) = tree
    val receiver = qualifierOf(fun)

    val genReceiver = genExpr(receiver)
    val genArg = genStatOrExpr(arg, isStat)

    genReceiver match {
      case js.This() =>
        // common case for which there is no side-effect nor NPE
        genArg
      case _ =>
        implicit val pos = tree.span
        js.Block(
            js.If(js.BinaryOp(js.BinaryOp.===, genReceiver, js.Null()),
                js.Throw(js.New(NullPointerExceptionClass, js.MethodIdent(jsNames.NoArgConstructorName), Nil)),
                js.Skip())(jstpe.NoType),
            genArg)
    }
  }

  /** Gen JS code for a coercion */
  private def genCoercion(tree: Apply, receiver: Tree, code: Int): js.Tree = {
    implicit val pos = tree.span

    val source = genExpr(receiver)
    val resultType = toIRType(tree.tpe)
    adaptPrimitive(source, resultType)
  }

  /** Gen a call to the special `throw` method. */
  private def genThrow(tree: Apply, args: List[Tree]): js.Tree = {
    implicit val pos: SourcePosition = tree.sourcePos
    val exception = args.head
    val genException = genExpr(exception)
    genException match {
      case js.New(cls, _, _) if cls != JavaScriptExceptionClassName =>
        // Common case where ex is neither null nor a js.JavaScriptException
        js.Throw(genException)
      case _ =>
        js.Throw(js.UnwrapFromThrowable(genException))
    }
  }

  /** Gen a "normal" apply (to a true method).
   *
   *  But even these are further refined into:
   *  * Methods of java.lang.String, which are redirected to the
   *    RuntimeString trait implementation.
   *  * Calls to methods of raw JS types (Scala.js -> JS interop)
   *  * Calls to methods in impl classes of Scala2 traits.
   *  * Regular method call
   */
  private def genNormalApply(tree: Apply, isStat: Boolean): js.Tree = {
    implicit val pos = tree.span

    val fun = tree.fun match {
      case fun: Ident => desugarIdent(fun).get
      case fun: Select => fun
    }
    val receiver = fun.qualifier
    val args = tree.args
    val sym = fun.symbol

    def isStringMethodFromObject: Boolean = sym.name match {
      case nme.toString_ | nme.equals_ | nme.hashCode_ => true
      case _                                           => false
    }

    if (isMethodStaticInIR(sym)) {
      genApplyStatic(sym, genActualArgs(sym, args))
    } else if (sym.owner.isJSType) {
      if (!sym.owner.isNonNativeJSClass || sym.isJSExposed)
        genApplyJSMethodGeneric(sym, genExprOrGlobalScope(receiver), genActualJSArgs(sym, args), isStat)(tree.sourcePos)
      else
        genApplyJSClassMethod(genExpr(receiver), sym, genActualArgs(sym, args))
    } else if (sym.hasAnnotation(jsdefn.JSNativeAnnot)) {
      genJSNativeMemberCall(tree)
    } else {
      genApplyMethodMaybeStatically(genExpr(receiver), sym, genActualArgs(sym, args))
    }
  }

  /** Gen JS code for a call to a JS method (of a subclass of `js.Any`).
   *
   *  Basically it boils down to calling the method as a `JSBracketSelect`,
   *  without name mangling. But other aspects come into play:
   *
   *  - Operator methods are translated to JS operators (not method calls)
   *  - `apply` is translated as a function call, i.e., `o()` instead of `o.apply()`
   *  - Scala varargs are turned into JS varargs (see `genPrimitiveJSArgs()`)
   *  - Getters and parameterless methods are translated as `JSBracketSelect`
   *  - Setters are translated to `Assign` to `JSBracketSelect`
   */
  private def genApplyJSMethodGeneric(sym: Symbol,
      receiver: MaybeGlobalScope, args: List[js.TreeOrJSSpread], isStat: Boolean,
      jsSuperClassValue: Option[js.Tree] = None)(
      implicit pos: SourcePosition): js.Tree = {

    def argsNoSpread: List[js.Tree] = {
      assert(!args.exists(_.isInstanceOf[js.JSSpread]), s"Unexpected spread at $pos")
      args.asInstanceOf[List[js.Tree]]
    }

    val argc = args.size // meaningful only for methods that don't have varargs

    def requireNotSuper(): Unit = {
      if (jsSuperClassValue.isDefined)
        report.error("Illegal super call in Scala.js-defined JS class", pos)
    }

    def requireNotSpread(arg: js.TreeOrJSSpread): js.Tree =
      arg.asInstanceOf[js.Tree]

    def genSuperReference(propName: js.Tree): js.AssignLhs = {
      jsSuperClassValue.fold[js.AssignLhs] {
        genJSSelectOrGlobalRef(receiver, propName)
      } { superClassValue =>
        js.JSSuperSelect(superClassValue, ruleOutGlobalScope(receiver), propName)
      }
    }

    def genSelectGet(propName: js.Tree): js.Tree =
      genSuperReference(propName)

    def genSelectSet(propName: js.Tree, value: js.Tree): js.Tree =
      js.Assign(genSuperReference(propName), value)

    def genCall(methodName: js.Tree, args: List[js.TreeOrJSSpread]): js.Tree = {
      jsSuperClassValue.fold[js.Tree] {
        genJSMethodApplyOrGlobalRefApply(receiver, methodName, args)
      } { superClassValue =>
        js.JSSuperMethodCall(superClassValue, ruleOutGlobalScope(receiver), methodName, args)
      }
    }

    val boxedResult = sym.jsCallingConvention match {
      case JSCallingConvention.UnaryOp(code) =>
        requireNotSuper()
        assert(argc == 0, s"bad argument count ($argc) for unary op at $pos")
        js.JSUnaryOp(code, ruleOutGlobalScope(receiver))

      case JSCallingConvention.BinaryOp(code) =>
        requireNotSuper()
        assert(argc == 1, s"bad argument count ($argc) for binary op at $pos")
        js.JSBinaryOp(code, ruleOutGlobalScope(receiver), requireNotSpread(args.head))

      case JSCallingConvention.Call =>
        requireNotSuper()
        if (sym.owner.isSubClass(jsdefn.JSThisFunctionClass))
          js.JSMethodApply(ruleOutGlobalScope(receiver), js.StringLiteral("call"), args)
        else
          js.JSFunctionApply(ruleOutGlobalScope(receiver), args)

      case JSCallingConvention.Property(jsName) =>
        argsNoSpread match {
          case Nil =>
            genSelectGet(genExpr(jsName))
          case value :: Nil =>
            genSelectSet(genExpr(jsName), value)
          case _ =>
            throw new AssertionError(s"property methods should have 0 or 1 non-varargs arguments at $pos")
        }

      case JSCallingConvention.BracketAccess =>
        argsNoSpread match {
          case keyArg :: Nil =>
            genSelectGet(keyArg)
          case keyArg :: valueArg :: Nil =>
            genSelectSet(keyArg, valueArg)
          case _ =>
            throw new AssertionError(s"@JSBracketAccess methods should have 1 or 2 non-varargs arguments at $pos")
        }

      case JSCallingConvention.BracketCall =>
        val (methodName, actualArgs) = extractFirstArg(args)
        genCall(methodName, actualArgs)

      case JSCallingConvention.Method(jsName) =>
        genCall(genExpr(jsName), args)
    }

    if (isStat) {
      boxedResult
    } else {
      val tpe = atPhase(elimErasedValueTypePhase) {
        sym.info.finalResultType
      }
      if (tpe.isRef(defn.BoxedUnitClass) && sym.isGetter) {
        /* Work around to reclaim Scala 2 erasure behavior, assumed by the test
         * NonNativeJSTypeTest.defaultValuesForFields.
         * Scala 2 erases getters of `Unit`-typed fields as returning `Unit`
         * (not `BoxedUnit`). Therefore, when called in expression position,
         * the call site introduces an explicit `BoxedUnit.UNIT`. Even if the
         * field has not been initialized at all (with `= _`), this results in
         * an actual `()` value.
         * In Scala 3, the same pattern returns `null`, as a `BoxedUnit`, so we
         * introduce here an explicit `()` value.
         * TODO We should remove this branch if the upstream test is updated
         * not to assume such a strict interpretation of erasure.
         */
        js.Block(boxedResult, js.Undefined())
      } else {
        unbox(boxedResult, tpe)
      }
    }
  }

  /** Extract the first argument in a list of actual arguments.
   *
   *  This is nothing else than decomposing into head and tail, except that
   *  we assert that the first element is not a JSSpread.
   */
  private def extractFirstArg(args: List[js.TreeOrJSSpread]): (js.Tree, List[js.TreeOrJSSpread]) = {
    assert(args.nonEmpty,
        "Trying to extract the first argument of an empty argument list")
    val firstArg = args.head
    assert(!firstArg.isInstanceOf[js.JSSpread],
        "Trying to extract the first argument of an argument list starting " +
        "with a Spread argument: " + firstArg)
    (firstArg.asInstanceOf[js.Tree], args.tail)
  }

  /** Gen JS code for a call to a native JS def or val. */
  private def genJSNativeMemberSelect(tree: Tree): js.Tree =
    genJSNativeMemberSelectOrCall(tree, Nil)

  /** Gen JS code for a call to a native JS def or val. */
  private def genJSNativeMemberCall(tree: Apply): js.Tree =
    genJSNativeMemberSelectOrCall(tree, tree.args)

  /** Gen JS code for a call to a native JS def or val. */
  private def genJSNativeMemberSelectOrCall(tree: Tree, args: List[Tree]): js.Tree = {
    val sym = tree.symbol

    implicit val pos = tree.span

    val jsNativeMemberValue =
      js.SelectJSNativeMember(encodeClassName(sym.owner), encodeJSNativeMemberSym(sym))

    val boxedResult =
      if (sym.isJSGetter) jsNativeMemberValue
      else js.JSFunctionApply(jsNativeMemberValue, genActualJSArgs(sym, args))

    unbox(boxedResult, atPhase(elimErasedValueTypePhase) {
      sym.info.resultType
    })
  }

  private def genJSSuperCall(tree: Apply, isStat: Boolean): js.Tree = {
    acquireContextualJSClassValue { explicitJSSuperClassValue =>
      implicit val pos = tree.span
      val Apply(fun @ Select(sup @ Super(qual, _), _), args) = tree: @unchecked
      val sym = fun.symbol

      val genReceiver = genExpr(qual)
      def genScalaArgs = genActualArgs(sym, args)
      def genJSArgs = genActualJSArgs(sym, args)

      if (sym.owner == defn.ObjectClass) {
        // Normal call anyway
        assert(!sym.isClassConstructor,
            s"Trying to call the super constructor of Object in a non-native JS class at $pos")
        genApplyMethod(genReceiver, sym, genScalaArgs)
      } else if (sym.isClassConstructor) {
        throw new AssertionError(
            s"calling a JS super constructor should have happened in genPrimaryJSClassCtor at $pos")
      } else if (sym.owner.isNonNativeJSClass && !sym.isJSExposed) {
        // Reroute to the static method
        genApplyJSClassMethod(genReceiver, sym, genScalaArgs)
      } else {
        val jsSuperClassValue = explicitJSSuperClassValue.orElse {
          Some(genLoadJSConstructor(currentClassSym.get.asClass.superClass))
        }
        genApplyJSMethodGeneric(sym, MaybeGlobalScope.NotGlobalScope(genReceiver),
            genJSArgs, isStat, jsSuperClassValue)(tree.sourcePos)
      }
    }
  }

  /** Gen JS code for a call to a polymorphic method.
   *
   *  The only methods that reach the back-end as polymorphic are
   *  `isInstanceOf` and `asInstanceOf`.
   *
   *  (Well, in fact `DottyRunTime.newRefArray` too, but it is handled as a
   *  primitive instead.)
   */
  private def genTypeApply(tree: TypeApply): js.Tree = {
    implicit val pos: SourcePosition = tree.sourcePos

    val TypeApply(fun, targs) = tree

    val sym = fun.symbol
    val receiver = qualifierOf(fun)

    val to = targs.head.tpe

    assert(!isPrimitiveValueType(receiver.tpe),
        s"Found receiver of type test with primitive type ${receiver.tpe} at $pos")
    assert(!isPrimitiveValueType(to),
        s"Found target type of type test with primitive type ${receiver.tpe} at $pos")

    val genReceiver = genExpr(receiver)

    if (sym == defn.Any_asInstanceOf) {
      genAsInstanceOf(genReceiver, to)
    } else if (sym == defn.Any_isInstanceOf) {
      genIsInstanceOf(genReceiver, to)
    } else {
      throw new FatalError(
          s"Unexpected type application $fun with symbol ${sym.fullName}")
    }
  }

  /** Gen JS code for a Java Seq literal. */
  private def genJavaSeqLiteral(tree: JavaSeqLiteral): js.Tree = {
    implicit val pos = tree.span

    val genElems = tree.elems.map(genExpr)
    val arrayTypeRef = toTypeRef(tree.tpe).asInstanceOf[jstpe.ArrayTypeRef]
    js.ArrayValue(arrayTypeRef, genElems)
  }

  /** Gen JS code for a switch-`Match`, which is translated into an IR `js.Match`. */
  def genMatch(tree: Tree, isStat: Boolean): js.Tree = {
    implicit val pos = tree.span
    val Match(selector, cases) = tree: @unchecked

    def abortMatch(msg: String): Nothing =
      throw new FatalError(s"$msg in switch-like pattern match at ${tree.span}: $tree")

    val genSelector = genExpr(selector)

    // Sanity check: we can handle Ints and Strings (including `null`s), but nothing else
    genSelector.tpe match {
      case jstpe.IntType | jstpe.ClassType(jsNames.BoxedStringClass) | jstpe.NullType | jstpe.NothingType =>
        // ok
      case _ =>
        abortMatch(s"Invalid selector type ${genSelector.tpe}")
    }

    val resultType = toIRType(tree.tpe) match {
      case jstpe.NothingType => jstpe.NothingType // must take priority over NoType below
      case _ if isStat       => jstpe.NoType
      case resType           => resType
    }

    var clauses: List[(List[js.MatchableLiteral], js.Tree)] = Nil
    var optDefaultClause: Option[js.Tree] = None

    for (caze @ CaseDef(pat, guard, body) <- cases) {
      if (guard != EmptyTree)
        abortMatch("Found a case guard")

      val genBody = genStatOrExpr(body, isStat)

      def invalidCase(): Nothing =
        abortMatch("Invalid case")

      def genMatchableLiteral(tree: Literal): js.MatchableLiteral = {
        genExpr(tree) match {
          case matchableLiteral: js.MatchableLiteral => matchableLiteral
          case otherExpr                             => invalidCase()
        }
      }

      pat match {
        case lit: Literal =>
          clauses = (List(genMatchableLiteral(lit)), genBody) :: clauses
        case Ident(nme.WILDCARD) =>
          optDefaultClause = Some(genBody)
        case Alternative(alts) =>
          val genAlts = alts.map {
            case lit: Literal => genMatchableLiteral(lit)
            case _            => invalidCase()
          }
          clauses = (genAlts, genBody) :: clauses
        case _ =>
          invalidCase()
      }
    }

    clauses = clauses.reverse
    val defaultClause = optDefaultClause.getOrElse {
      throw new AssertionError("No elseClause in pattern match")
    }

    /* Builds a `js.Match`, but simplifies it to a `js.If` if there is only
     * one case with one alternative, and to a `js.Block` if there is no case
     * at all. This happens in practice in the standard library. Having no
     * case is a typical product of `match`es that are full of
     * `case n if ... =>`, which are used instead of `if` chains for
     * convenience and/or readability.
     */
    def isInt(tree: js.Tree): Boolean = tree.tpe == jstpe.IntType

    clauses match {
      case Nil =>
        // Completely remove the Match. Preserve the side-effects of `genSelector`.
        js.Block(exprToStat(genSelector), defaultClause)

      case (uniqueAlt :: Nil, caseRhs) :: Nil =>
        /* Simplify the `match` as an `if`, so that the optimizer has less
         * work to do, and we emit less code at the end of the day.
         * Use `Int_==` instead of `===` if possible, since it is a common case.
         */
        val op =
          if (isInt(genSelector) && isInt(uniqueAlt)) js.BinaryOp.Int_==
          else js.BinaryOp.===
        js.If(js.BinaryOp(op, genSelector, uniqueAlt), caseRhs, defaultClause)(resultType)

      case _ =>
        // We have more than one case: use a js.Match
        js.Match(genSelector, clauses, defaultClause)(resultType)
    }
  }

  /** Gen JS code for a closure.
   *
   *  Input: a `Closure` tree of the form
   *  {{{
   *  Closure(env, call, functionalInterface)
   *  }}}
   *  representing the pseudo-syntax
   *  {{{
   *  { (p1, ..., pm) => call(env1, ..., envn, p1, ..., pm) }: functionInterface
   *  }}}
   *  where `envi` are identifiers in the local scope. The qualifier of `call`
   *  is also implicitly captured.
   *
   *  Output: a `js.Closure` tree of the form
   *  {{{
   *  js.Closure(formalCaptures, formalParams, body, actualCaptures)
   *  }}}
   *  representing the pseudo-syntax
   *  {{{
   *  lambda<formalCapture1 = actualCapture1, ..., formalCaptureN = actualCaptureN>(
   *      formalParam1, ..., formalParamM) = body
   *  }}}
   *  where the `actualCaptures` and `body` are, in general, arbitrary
   *  expressions. But in this case, `actualCaptures` will be identifiers from
   *  `env`, and the `body` will be of the form
   *  {{{
   *  call(formalCapture1.ref, ..., formalCaptureN.ref,
   *      formalParam1.ref, ...formalParamM.ref)
   *  }}}
   *
   *  When the `js.Closure` node is evaluated, i.e., when the closure value is
   *  created, the expressions of the `actualCaptures` are evaluated, and the
   *  results of those evaluations is "stored" in the environment of the
   *  closure as the corresponding `formalCapture`.
   *
   *  When we later *call* the closure, the `formalCaptures` already have their
   *  values from the environment, and they are available in the `body`. The
   *  `formalParams` of the created closure receive their values from the
   *  actual arguments at the call-site of the closure, and they are also
   *  available in the `body`.
   */
  private def genClosure(tree: Closure): js.Tree = {
    implicit val pos = tree.span
    val Closure(env, call, functionalInterface) = tree

    val envSize = env.size

    val (fun, args) = call match {
      // case Apply(fun, args) => (fun, args) // Conjectured not to happen
      case t @ Select(_, _) => (t, Nil)
      case t @ Ident(_) => (t, Nil)
    }
    val sym = fun.symbol
    val isStaticCall = isMethodStaticInIR(sym)

    val qualifier = qualifierOf(fun)
    val allCaptureValues =
      if (isStaticCall) env
      else qualifier :: env

    val formalAndActualCaptures = allCaptureValues.map { value =>
      implicit val pos = value.span
      val (formalIdent, originalName) = value match {
        case Ident(name) => (freshLocalIdent(name.toTermName), OriginalName(name.toString))
        case This(_)     => (freshLocalIdent("this"), thisOriginalName)
        case _           => (freshLocalIdent(), NoOriginalName)
      }
      val formalCapture = js.ParamDef(formalIdent, originalName,
          toIRType(value.tpe), mutable = false)
      val actualCapture = genExpr(value)
      (formalCapture, actualCapture)
    }
    val (formalCaptures, actualCaptures) = formalAndActualCaptures.unzip

    val funInterfaceSym = functionalInterface.tpe.typeSymbol
    val hasRepeatedParam = {
      funInterfaceSym.exists && {
        val Seq(samMethodDenot) = funInterfaceSym.info.possibleSamMethods
        val samMethod = samMethodDenot.symbol
        atPhase(elimRepeatedPhase)(samMethod.info.paramInfoss.flatten.exists(_.isRepeatedParam))
      }
    }

    val formalParamNames = sym.info.paramNamess.flatten.drop(envSize)
    val formalParamTypes = sym.info.paramInfoss.flatten.drop(envSize)
    val formalParamRepeateds =
      if (hasRepeatedParam) (0 until (formalParamTypes.size - 1)).map(_ => false) :+ true
      else (0 until formalParamTypes.size).map(_ => false)

    val formalAndActualParams = formalParamNames.lazyZip(formalParamTypes).lazyZip(formalParamRepeateds).map {
      (name, tpe, repeated) =>
        val formalParam = js.ParamDef(freshLocalIdent(name),
            OriginalName(name.toString), jstpe.AnyType, mutable = false)
        val actualParam =
          if (repeated) genJSArrayToVarArgs(formalParam.ref)(tree.sourcePos)
          else unbox(formalParam.ref, tpe)
        (formalParam, actualParam)
    }
    val (formalAndRestParams, actualParams) = formalAndActualParams.unzip

    val (formalParams, restParam) =
      if (hasRepeatedParam) (formalAndRestParams.init, Some(formalAndRestParams.last))
      else (formalAndRestParams, None)

    val genBody = {
      val call = if (isStaticCall) {
        genApplyStatic(sym, formalCaptures.map(_.ref) ::: actualParams)
      } else {
        val thisCaptureRef :: argCaptureRefs = formalCaptures.map(_.ref): @unchecked
        if (!sym.owner.isNonNativeJSClass || sym.isJSExposed)
          genApplyMethodMaybeStatically(thisCaptureRef, sym, argCaptureRefs ::: actualParams)
        else
          genApplyJSClassMethod(thisCaptureRef, sym, argCaptureRefs ::: actualParams)
      }
      box(call, sym.info.finalResultType)
    }

    val isThisFunction = funInterfaceSym.isSubClass(jsdefn.JSThisFunctionClass) && {
      val ok = formalParams.nonEmpty
      if (!ok)
        report.error("The SAM or apply method for a js.ThisFunction must have a leading non-varargs parameter", tree)
      ok
    }

    if (isThisFunction) {
      val thisParam :: otherParams = formalParams: @unchecked
      js.Closure(
          arrow = false,
          formalCaptures,
          otherParams,
          restParam,
          js.Block(
              js.VarDef(thisParam.name, thisParam.originalName,
                  thisParam.ptpe, mutable = false,
                  js.This()(thisParam.ptpe)(thisParam.pos))(thisParam.pos),
              genBody),
          actualCaptures)
    } else {
      val closure = js.Closure(arrow = true, formalCaptures, formalParams, restParam, genBody, actualCaptures)

      if (!funInterfaceSym.exists || defn.isFunctionClass(funInterfaceSym)) {
        val formalCount = formalParams.size
        val cls = ClassName("scala.scalajs.runtime.AnonFunction" + formalCount)
        val ctorName = MethodName.constructor(
            jstpe.ClassRef(ClassName("scala.scalajs.js.Function" + formalCount)) :: Nil)
        js.New(cls, js.MethodIdent(ctorName), List(closure))
      } else if (funInterfaceSym.name == tpnme.FunctionXXL && funInterfaceSym.owner == defn.ScalaRuntimePackageClass) {
        val cls = ClassName("scala.scalajs.runtime.AnonFunctionXXL")
        val ctorName = MethodName.constructor(
            jstpe.ClassRef(ClassName("scala.scalajs.js.Function1")) :: Nil)
        js.New(cls, js.MethodIdent(ctorName), List(closure))
      } else {
        assert(funInterfaceSym.isJSType,
            s"Invalid functional interface $funInterfaceSym reached the back-end")
        closure
      }
    }
  }

  /** Generates a static method instantiating and calling this
   *  DynamicImportThunk's `apply`:
   *
   *  {{{
   *  static def dynamicImport$;<params>;Ljava.lang.Object(<params>): any = {
   *    new <clsSym>.<init>;<params>:V(<params>).apply;Ljava.lang.Object()
   *  }
   *  }}}
   */
  private def genDynamicImportForwarder(clsSym: Symbol)(using Position): js.MethodDef = {
    withNewLocalNameScope {
      val ctor = clsSym.primaryConstructor
      val paramSyms = ctor.paramSymss.flatten
      val paramDefs = paramSyms.map(genParamDef(_))

      val body = {
        val inst = js.New(encodeClassName(clsSym), encodeMethodSym(ctor), paramDefs.map(_.ref))
        genApplyMethod(inst, jsdefn.DynamicImportThunkClass_apply, Nil)
      }

      js.MethodDef(
          js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic),
          encodeDynamicImportForwarderIdent(paramSyms),
          NoOriginalName,
          paramDefs,
          jstpe.AnyType,
          Some(body))(OptimizerHints.empty, None)
    }
  }

  /** Boxes a value of the given type before `elimErasedValueType`.
   *
   *  This should be used when sending values to a JavaScript context, which
   *  is erased/boxed at the IR level, although it is not erased at the
   *  dotty/JVM level.
   *
   *  @param expr Tree to be boxed if needed.
   *  @param tpeEnteringElimErasedValueType The type of `expr` as it was
   *    entering the `elimErasedValueType` phase.
   */
  def box(expr: js.Tree, tpeEnteringElimErasedValueType: Type)(implicit pos: Position): js.Tree = {
    tpeEnteringElimErasedValueType match {
      case tpe if isPrimitiveValueType(tpe) =>
        makePrimitiveBox(expr, tpe)

      case tpe: ErasedValueType =>
        val boxedClass = tpe.tycon.typeSymbol
        val ctor = boxedClass.primaryConstructor
        js.New(encodeClassName(boxedClass), encodeMethodSym(ctor), List(expr))

      case _ =>
        expr
    }
  }

  /** Unboxes a value typed as Any to the given type before `elimErasedValueType`.
   *
   *  This should be used when receiving values from a JavaScript context,
   *  which is erased/boxed at the IR level, although it is not erased at the
   *  dotty/JVM level.
   *
   *  @param expr Tree to be extracted.
   *  @param tpeEnteringElimErasedValueType The type of `expr` as it was
   *    entering the `elimErasedValueType` phase.
   */
  def unbox(expr: js.Tree, tpeEnteringElimErasedValueType: Type)(implicit pos: Position): js.Tree = {
    tpeEnteringElimErasedValueType match {
      case tpe if isPrimitiveValueType(tpe) =>
        makePrimitiveUnbox(expr, tpe)

      case tpe: ErasedValueType =>
        val boxedClass = tpe.tycon.typeSymbol.asClass
        val unboxMethod = ValueClasses.valueClassUnbox(boxedClass)
        val content = genApplyMethod(
            js.AsInstanceOf(expr, encodeClassType(boxedClass)), unboxMethod, Nil)
        if (unboxMethod.info.resultType <:< tpe.erasedUnderlying)
          content
        else
          unbox(content, tpe.erasedUnderlying)

      case tpe =>
        genAsInstanceOf(expr, tpe)
    }
  }

  /** Gen JS code for an asInstanceOf cast (for reference types only) */
  private def genAsInstanceOf(value: js.Tree, to: Type)(implicit pos: Position): js.Tree =
    genAsInstanceOf(value, toIRType(to))

  /** Gen JS code for an asInstanceOf cast (for reference types only) */
  private def genAsInstanceOf(value: js.Tree, to: jstpe.Type)(implicit pos: Position): js.Tree = {
    to match {
      case jstpe.AnyType =>
        value
      case jstpe.NullType =>
        js.If(
            js.BinaryOp(js.BinaryOp.===, value, js.Null()),
            js.Null(),
            genThrowClassCastException())(
            jstpe.NullType)
      case jstpe.NothingType =>
        js.Block(value, genThrowClassCastException())
      case _ =>
        js.AsInstanceOf(value, to)
    }
  }

  private def genThrowClassCastException()(implicit pos: Position): js.Tree = {
    js.Throw(js.New(jsNames.ClassCastExceptionClass,
        js.MethodIdent(jsNames.NoArgConstructorName), Nil))
  }

  /** Gen JS code for an isInstanceOf test (for reference types only) */
  def genIsInstanceOf(value: js.Tree, to: Type)(
      implicit pos: SourcePosition): js.Tree = {
    val sym = to.typeSymbol

    if (sym == defn.ObjectClass) {
      js.BinaryOp(js.BinaryOp.!==, value, js.Null())
    } else if (sym.isJSType) {
      if (sym.is(Trait)) {
        report.error(
            em"isInstanceOf[${sym.fullName}] not supported because it is a JS trait",
            pos)
        js.BooleanLiteral(true)
      } else {
        js.AsInstanceOf(js.JSBinaryOp(
            js.JSBinaryOp.instanceof, value, genLoadJSConstructor(sym)),
            jstpe.BooleanType)
      }
    } else {
      // The Scala type system prevents x.isInstanceOf[Null] and ...[Nothing]
      assert(sym != defn.NullClass && sym != defn.NothingClass,
          s"Found a .isInstanceOf[$sym] at $pos")
      js.IsInstanceOf(value, toIRType(to))
    }
  }

  /** Gen a statically linked call to an instance method. */
  def genApplyMethodMaybeStatically(receiver: js.Tree, method: Symbol,
      arguments: List[js.Tree])(implicit pos: Position): js.Tree = {
    if (method.isPrivate || method.isClassConstructor)
      genApplyMethodStatically(receiver, method, arguments)
    else
      genApplyMethod(receiver, method, arguments)
  }

  /** Gen a dynamically linked call to a Scala method. */
  def genApplyMethod(receiver: js.Tree, method: Symbol, arguments: List[js.Tree])(
      implicit pos: Position): js.Tree = {
    assert(!method.isPrivate,
        s"Cannot generate a dynamic call to private method $method at $pos")
    js.Apply(js.ApplyFlags.empty, receiver, encodeMethodSym(method), arguments)(
        toIRType(patchedResultType(method)))
  }

  /** Gen a statically linked call to an instance method. */
  def genApplyMethodStatically(receiver: js.Tree, method: Symbol, arguments: List[js.Tree])(
      implicit pos: Position): js.Tree = {
    val flags = js.ApplyFlags.empty
      .withPrivate(method.isPrivate && !method.isClassConstructor)
      .withConstructor(method.isClassConstructor)
    js.ApplyStatically(flags, receiver, encodeClassName(method.owner),
        encodeMethodSym(method), arguments)(
        toIRType(patchedResultType(method)))
  }

  /** Gen a call to a static method. */
  private def genApplyStatic(method: Symbol, arguments: List[js.Tree])(
      implicit pos: Position): js.Tree = {
    js.ApplyStatic(js.ApplyFlags.empty.withPrivate(method.isPrivate),
        encodeClassName(method.owner), encodeMethodSym(method), arguments)(
        toIRType(patchedResultType(method)))
  }

  /** Gen a call to a non-exposed method of a non-native JS class. */
  def genApplyJSClassMethod(receiver: js.Tree, method: Symbol, arguments: List[js.Tree])(
      implicit pos: Position): js.Tree = {
    genApplyStatic(method, receiver :: arguments)
  }

  /** Gen a call to a method of a Scala top-level module. */
  private def genModuleApplyMethod(methodSym: Symbol, arguments: List[js.Tree])(
      implicit pos: SourcePosition): js.Tree = {
    genApplyMethod(genLoadModule(methodSym.owner), methodSym, arguments)
  }

  /** Gen a boxing operation (tpe is the primitive type) */
  private def makePrimitiveBox(expr: js.Tree, tpe: Type)(
      implicit pos: Position): js.Tree = {
    toIRType(tpe) match {
      case jstpe.NoType => // for JS interop cases
        js.Block(expr, js.Undefined())
      case jstpe.BooleanType | jstpe.CharType | jstpe.ByteType |
          jstpe.ShortType | jstpe.IntType | jstpe.LongType | jstpe.FloatType |
          jstpe.DoubleType =>
        expr // box is identity for all those primitive types
      case typeRef =>
        throw new FatalError(
            s"makePrimitiveBox requires a primitive type, found $typeRef for $tpe at $pos")
    }
  }

  /** Gen an unboxing operation (tpe is the primitive type) */
  private def makePrimitiveUnbox(expr: js.Tree, tpe: Type)(
      implicit pos: Position): js.Tree = {
    toIRType(tpe) match {
      case jstpe.NoType => expr // for JS interop cases
      case irTpe        => js.AsInstanceOf(expr, irTpe)
    }
  }

  /** Gen JS code for a Scala.js-specific primitive method */
  private def genJSPrimitive(tree: Apply, args: List[Tree], code: Int,
      isStat: Boolean): js.Tree = {

    import JSPrimitives._

    implicit val pos = tree.span

    def genArgs1: js.Tree = {
      assert(args.size == 1,
          s"Expected exactly 1 argument for JS primitive $code but got " +
          s"${args.size} at $pos")
      genExpr(args.head)
    }

    def genArgs2: (js.Tree, js.Tree) = {
      assert(args.size == 2,
          s"Expected exactly 2 arguments for JS primitive $code but got " +
          s"${args.size} at $pos")
      (genExpr(args.head), genExpr(args.tail.head))
    }

    def genArgsVarLength: List[js.TreeOrJSSpread] =
      genActualJSArgs(tree.symbol, args)

    def resolveReifiedJSClassSym(arg: Tree): Symbol = {
      def fail(): Symbol = {
        report.error(
            tree.symbol.name.toString + " must be called with a constant " +
            "classOf[T] representing a class extending js.Any " +
            "(not a trait nor an object)",
            tree.sourcePos)
        NoSymbol
      }
      arg match {
        case Literal(value) if value.tag == Constants.ClazzTag =>
          val classSym = value.typeValue.typeSymbol
          if (classSym.isJSType && !classSym.is(Trait) && !classSym.is(ModuleClass))
            classSym
          else
            fail()
        case _ =>
          fail()
      }
    }

    (code: @switch) match {
      case DYNNEW =>
        // js.Dynamic.newInstance(clazz)(actualArgs: _*)
        val (jsClass, actualArgs) = extractFirstArg(genArgsVarLength)
        js.JSNew(jsClass, actualArgs)

      case ARR_CREATE =>
        // js.Array(elements: _*)
        js.JSArrayConstr(genArgsVarLength)

      case CONSTRUCTOROF =>
        // runtime.constructorOf(clazz)
        val classSym = resolveReifiedJSClassSym(args.head)
        if (classSym == NoSymbol)
          js.Undefined() // compile error emitted by resolveReifiedJSClassSym
        else
          genLoadJSConstructor(classSym)

      case CREATE_INNER_JS_CLASS | CREATE_LOCAL_JS_CLASS =>
        // runtime.createInnerJSClass(clazz, superClass)
        // runtime.createLocalJSClass(clazz, superClass, fakeNewInstances)
        val classSym = resolveReifiedJSClassSym(args(0))
        val superClassValue = genExpr(args(1))
        if (classSym == NoSymbol) {
          js.Undefined() // compile error emitted by resolveReifiedJSClassSym
        } else {
          val captureValues = {
            if (code == CREATE_INNER_JS_CLASS) {
              /* Private inner classes that do not actually access their outer
               * pointer do not receive an outer argument. We therefore count
               * the number of constructors that have non-empty param list to
               * know how many times we need to pass `this`.
               */
              val requiredThisParams =
                classSym.info.decls.lookupAll(nme.CONSTRUCTOR).count(_.info.paramInfoss.head.nonEmpty)
              val outer = genThis()
              List.fill(requiredThisParams)(outer)
            } else {
              val fakeNewInstances = args(2).asInstanceOf[JavaSeqLiteral].elems
              fakeNewInstances.flatMap(genCaptureValuesFromFakeNewInstance(_))
            }
          }
          js.CreateJSClass(encodeClassName(classSym), superClassValue :: captureValues)
        }

      case WITH_CONTEXTUAL_JS_CLASS_VALUE =>
        // withContextualJSClassValue(jsclass, inner)
        val jsClassValue = genExpr(args(0))
        withScopedVars(
            contextualJSClassValue := Some(jsClassValue)
        ) {
          genStatOrExpr(args(1), isStat)
        }

      case LINKING_INFO =>
        // runtime.linkingInfo
        js.JSLinkingInfo()

      case DEBUGGER =>
        // js.special.debugger()
        js.Debugger()

      case UNITVAL =>
        // BoxedUnit.UNIT, which is the boxed version of ()
        js.Undefined()

      case JS_NEW_TARGET =>
        // js.new.target
        val valid = currentMethodSym.get.isClassConstructor && currentClassSym.isNonNativeJSClass
        if (!valid) {
          report.error(
              "Illegal use of js.`new`.target.\n" +
              "It can only be used in the constructor of a JS class, " +
              "as a statement or in the rhs of a val or var.\n" +
              "It cannot be used inside a lambda or by-name parameter, nor in any other location.",
              tree.sourcePos)
        }
        js.JSNewTarget()

      case JS_IMPORT =>
        // js.import(arg)
        val arg = genArgs1
        js.JSImportCall(arg)

      case JS_IMPORT_META =>
        // js.import.meta
        js.JSImportMeta()

      case DYNAMIC_IMPORT =>
        // runtime.dynamicImport
        assert(args.size == 1,
            s"Expected exactly 1 argument for JS primitive $code but got " +
            s"${args.size} at $pos")

        args.head match {
          case Block(stats, expr @ Typed(Apply(fun @ Select(New(tpt), _), args), _)) =>
            /* stats is always empty if no other compiler plugin is present.
             * However, code instrumentation (notably scoverage) might add
             * statements here. If this is the case, the thunk anonymous class
             * has already been created when the other plugin runs (i.e. the
             * plugin ran after jsinterop).
             *
             * Therefore, it is OK to leave the statements on our side of the
             * dynamic loading boundary.
             */

            val clsSym = tpt.symbol
            val ctor = fun.symbol

            assert(clsSym.isSubClass(jsdefn.DynamicImportThunkClass),
                s"expected subclass of DynamicImportThunk, got: $clsSym at: ${expr.sourcePos}")
            assert(ctor.isPrimaryConstructor,
                s"expected primary constructor, got: $ctor at: ${expr.sourcePos}")

            js.Block(
                stats.map(genStat(_)),
                js.ApplyDynamicImport(
                    js.ApplyFlags.empty,
                    encodeClassName(clsSym),
                    encodeDynamicImportForwarderIdent(ctor.paramSymss.flatten),
                    genActualArgs(ctor, args))
            )

          case tree =>
            throw new FatalError(
                s"Unexpected argument tree in dynamicImport: $tree/${tree.getClass} at: $pos")
        }

      case JS_NATIVE =>
        // js.native
        report.error(
            "js.native may only be used as stub implementation in facade types",
            tree.sourcePos)
        js.Undefined()

      case TYPEOF =>
        // js.typeOf(arg)
        val arg = genArgs1
        val typeofExpr = arg match {
          case arg: js.JSGlobalRef => js.JSTypeOfGlobalRef(arg)
          case _                   => js.JSUnaryOp(js.JSUnaryOp.typeof, arg)
        }
        js.AsInstanceOf(typeofExpr, jstpe.ClassType(jsNames.BoxedStringClass))

      case STRICT_EQ =>
        // js.special.strictEquals(arg1, arg2)
        val (arg1, arg2) = genArgs2
        js.JSBinaryOp(js.JSBinaryOp.===, arg1, arg2)

      case IN =>
        // js.special.in(arg1, arg2)
        val (arg1, arg2) = genArgs2
        js.AsInstanceOf(js.JSBinaryOp(js.JSBinaryOp.in, arg1, arg2),
            jstpe.BooleanType)

      case INSTANCEOF =>
        // js.special.instanceof(arg1, arg2)
        val (arg1, arg2) = genArgs2
        js.AsInstanceOf(js.JSBinaryOp(js.JSBinaryOp.instanceof, arg1, arg2),
            jstpe.BooleanType)

      case DELETE =>
        // js.special.delete(arg1, arg2)
        val (arg1, arg2) = genArgs2
        js.JSDelete(arg1, arg2)

      case FORIN =>
        /* js.special.forin(arg1, arg2)
         *
         * We must generate:
         *
         * val obj = arg1
         * val f = arg2
         * for (val key in obj) {
         *   f(key)
         * }
         *
         * with temporary vals, because `arg2` must be evaluated only
         * once, and after `arg1`.
         */
        val (arg1, arg2) = genArgs2
        val objVarDef = js.VarDef(freshLocalIdent("obj"), NoOriginalName,
            jstpe.AnyType, mutable = false, arg1)
        val fVarDef = js.VarDef(freshLocalIdent("f"), NoOriginalName,
            jstpe.AnyType, mutable = false, arg2)
        val keyVarIdent = freshLocalIdent("key")
        val keyVarRef = js.VarRef(keyVarIdent)(jstpe.AnyType)
        js.Block(
            objVarDef,
            fVarDef,
            js.ForIn(objVarDef.ref, keyVarIdent, NoOriginalName, {
              js.JSFunctionApply(fVarDef.ref, List(keyVarRef))
            }))

      case JS_THROW =>
        // js.special.throw(arg)
        js.Throw(genArgs1)

      case JS_TRY_CATCH =>
        /* js.special.tryCatch(arg1, arg2)
         *
         * We must generate:
         *
         * val body = arg1
         * val handler = arg2
         * try {
         *   body()
         * } catch (e) {
         *   handler(e)
         * }
         *
         * with temporary vals, because `arg2` must be evaluated before
         * `body` executes. Moreover, exceptions thrown while evaluating
         * the function values `arg1` and `arg2` must not be caught.
         */
        val (arg1, arg2) = genArgs2
        val bodyVarDef = js.VarDef(freshLocalIdent("body"), NoOriginalName,
            jstpe.AnyType, mutable = false, arg1)
        val handlerVarDef = js.VarDef(freshLocalIdent("handler"), NoOriginalName,
            jstpe.AnyType, mutable = false, arg2)
        val exceptionVarIdent = freshLocalIdent("e")
        val exceptionVarRef = js.VarRef(exceptionVarIdent)(jstpe.AnyType)
        js.Block(
          bodyVarDef,
          handlerVarDef,
          js.TryCatch(
            js.JSFunctionApply(bodyVarDef.ref, Nil),
            exceptionVarIdent,
            NoOriginalName,
            js.JSFunctionApply(handlerVarDef.ref, List(exceptionVarRef))
          )(jstpe.AnyType)
        )

      case WRAP_AS_THROWABLE =>
        // js.special.wrapAsThrowable(arg)
        js.WrapAsThrowable(genArgs1)

      case UNWRAP_FROM_THROWABLE =>
        // js.special.unwrapFromThrowable(arg)
        js.UnwrapFromThrowable(genArgs1)

      case UNION_FROM | UNION_FROM_TYPE_CONSTRUCTOR =>
        /* js.|.from and js.|.fromTypeConstructor
         * We should not have to deal with those. They have a perfectly valid
         * user-space implementation. However, the Dotty type checker inserts
         * way too many of those, even when they are completely unnecessary.
         * That still wouldn't be an issue ... if only it did not insert them
         * around the default getters to their parameters! But even there it
         * does it (although the types are, by construction, *equivalent*!),
         * and that kills our `UndefinedParam` treatment. So we have to handle
         * those two methods as primitives to completely eliminate them.
         *
         * Hopefully this will become unnecessary when/if we manage to
         * reinterpret js.| as a true Dotty union type.
         */
        genArgs2._1

      case REFLECT_SELECTABLE_SELECTDYN =>
        // scala.reflect.Selectable.selectDynamic
        genReflectiveCall(tree, isSelectDynamic = true)
      case REFLECT_SELECTABLE_APPLYDYN =>
        // scala.reflect.Selectable.applyDynamic
        genReflectiveCall(tree, isSelectDynamic = false)
    }
  }

  /** Gen the SJSIR for a reflective call.
   *
   *  Reflective calls are calls to a structural type field or method that
   *  involve a reflective Selectable. They look like the following in source
   *  code:
   *  {{{
   *  import scala.reflect.Selectable.reflectiveSelectable
   *
   *  type Structural = {
   *    val foo: Int
   *    def bar(x: Int, y: String): String
   *  }
   *
   *  val structural: Structural = new {
   *    val foo: Int = 5
   *    def bar(x: Int, y: String): String = x.toString + y
   *  }
   *
   *  structural.foo
   *  structural.bar(6, "hello")
   *  }}}
   *
   *  After expansion by the Scala 3 rules for structural member selections and
   *  calls, they look like
   *
   *  {{{
   *  reflectiveSelectable(structural).selectDynamic("foo")
   *  reflectiveSelectable(structural).applyDynamic("bar",
   *    classOf[Int], classOf[String]
   *  )(
   *    6, "hello"
   *  )
   *  }}}
   *
   *  When the original `structural` value is already of a subtype of
   *  `scala.reflect.Selectable`, there is no conversion involved. There could
   *  also be any other arbitrary conversion, such as the deprecated bridge for
   *  Scala 2's `import scala.language.reflectiveCalls`. In general, the shape
   *  is therefore the following, for some `selectable: reflect.Selectable`:
   *
   *  {{{
   *  selectable.selectDynamic("foo")
   *  selectable.applyDynamic("bar",
   *    classOf[Int], classOf[String]
   *  )(
   *    6, "hello"
   *  )
   *  }}}
   *
   *  and eventually reaches the back-end as
   *
   *  {{{
   *  selectable.selectDynamic("foo") // same as above
   *  selectable.applyDynamic("bar",
   *    wrapRefArray([ classOf[Int], classOf[String] : jl.Class ]
   *  )(
   *    genericWrapArray([ Int.box(6), "hello" : Object ])
   *  )
   *  }}}
   *
   *  In SJSIR, they must be encoded as follows:
   *
   *  {{{
   *  selectable.selectedValue;O().foo;R()
   *  selectable.selectedValue;O().bar;I;Ljava.lang.String;R(
   *    Int.box(6).asInstanceOf[int],
   *    "hello".asInstanceOf[java.lang.String]
   *  )
   *  }}}
   *
   *  where `selectedValue;O()` is declared in `scala.reflect.Selectable` and
   *  holds the actual instance on which to perform the reflective operations.
   *  For the typical use case from the first snippet, it returns `structural`.
   *
   *  This means that we must deconstruct the elaborated calls to recover:
   *
   *  - the method name as a compile-time string `foo` or `bar`
   *  - the `tp: Type`s that have been wrapped in `classOf[tp]`, as a
   *    compile-time List[Type], from which we'll derive `jstpe.Type`s for the
   *    `asInstanceOf`s and `jstpe.TypeRef`s for the `MethodName.reflectiveProxy`
   *  - the actual arguments as a compile-time `List[Tree]`
   *
   *  Virtually all of the code in `genReflectiveCall` deals with recovering
   *  those elements. Constructing the IR Tree is the easy part after that.
   */
  private def genReflectiveCall(tree: Apply, isSelectDynamic: Boolean): js.Tree = {
    implicit val pos = tree.span
    val Apply(fun @ Select(receiver, _), args) = tree: @unchecked

    val selectedValueTree = js.Apply(js.ApplyFlags.empty, genExpr(receiver),
        js.MethodIdent(selectedValueMethodName), Nil)(jstpe.AnyType)

    // Extract the method name as a String
    val methodNameStr = args.head match {
      case Literal(Constants.Constant(name: String)) =>
        name
      case _ =>
        report.error(
            "The method name given to Selectable.selectDynamic or Selectable.applyDynamic " +
            "must be a literal string. " +
            "Other uses are not supported in Scala.js.",
            args.head.sourcePos)
        "erroneous"
    }

    val (formalParamTypeRefs, actualArgs) = if (isSelectDynamic) {
      (Nil, Nil)
    } else {
      // Extract the param type refs and actual args from the 2nd and 3rd argument to applyDynamic
      args.tail match {
        case WrapArray(classOfsArray: JavaSeqLiteral) :: WrapArray(actualArgsAnyArray: JavaSeqLiteral) :: Nil =>
          // Extract jstpe.Type's and jstpe.TypeRef's from the classOf[_] trees
          val formalParamTypesAndTypeRefs = classOfsArray.elems.map {
            // classOf[tp] -> tp
            case Literal(const) if const.tag == Constants.ClazzTag =>
              toIRTypeAndTypeRef(const.typeValue)
            // Anything else is invalid
            case otherTree =>
              report.error(
                  "The java.lang.Class[_] arguments passed to Selectable.applyDynamic must be " +
                  "literal classOf[T] expressions (typically compiler-generated). " +
                  "Other uses are not supported in Scala.js.",
                  otherTree.sourcePos)
              (jstpe.AnyType, jstpe.ClassRef(jsNames.ObjectClass))
          }

          // Gen the actual args, downcasting them to the formal param types
          val actualArgs = actualArgsAnyArray.elems.zip(formalParamTypesAndTypeRefs).map {
            (actualArgAny, formalParamTypeAndTypeRef) =>
              val genActualArgAny = genExpr(actualArgAny)
              genAsInstanceOf(genActualArgAny, formalParamTypeAndTypeRef._1)(genActualArgAny.pos)
          }

          (formalParamTypesAndTypeRefs.map(pair => toParamOrResultTypeRef(pair._2)), actualArgs)

        case _ =>
          report.error(
              "Passing the varargs of Selectable.applyDynamic with `: _*` " +
              "is not supported in Scala.js.",
              tree.sourcePos)
          (Nil, Nil)
      }
    }

    val methodName = MethodName.reflectiveProxy(methodNameStr, formalParamTypeRefs)

    js.Apply(js.ApplyFlags.empty, selectedValueTree, js.MethodIdent(methodName), actualArgs)(jstpe.AnyType)
  }

  /** Gen actual actual arguments to Scala method call.
   *  Returns a list of the transformed arguments.
   *
   *  This tries to optimize repeated arguments (varargs) by turning them
   *  into js.WrappedArray instead of Scala wrapped arrays.
   */
  private def genActualArgs(sym: Symbol, args: List[Tree])(
      implicit pos: Position): List[js.Tree] = {
    args.map(genExpr)
    /*val wereRepeated = exitingPhase(currentRun.typerPhase) {
      sym.tpe.params.map(p => isScalaRepeatedParamType(p.tpe))
    }

    if (wereRepeated.size > args.size) {
      // Should not happen, but let's not crash
      args.map(genExpr)
    } else {
      /* Arguments that are in excess compared to the type signature after
       * erasure are lambda-lifted arguments. They cannot be repeated, hence
       * the extension to `false`.
       */
      for ((arg, wasRepeated) <- args.zipAll(wereRepeated, EmptyTree, false)) yield {
        if (wasRepeated) {
          tryGenRepeatedParamAsJSArray(arg, handleNil = false).fold {
            genExpr(arg)
          } { genArgs =>
            genNew(WrappedArrayClass, WrappedArray_ctor,
                List(js.JSArrayConstr(genArgs)))
          }
        } else {
          genExpr(arg)
        }
      }
    }*/
  }

  /** Gen actual actual arguments to a JS method call.
   *  Returns a list of the transformed arguments.
   *
   *  - TODO Repeated arguments (varargs) are expanded
   *  - Default arguments are omitted or replaced by undefined
   *  - All arguments are boxed
   *
   *  Repeated arguments that cannot be expanded at compile time (i.e., if a
   *  Seq is passed to a varargs parameter with the syntax `seq: _*`) will be
   *  wrapped in a [[js.JSSpread]] node to be expanded at runtime.
   */
  private def genActualJSArgs(sym: Symbol, args: List[Tree])(
      implicit pos: Position): List[js.TreeOrJSSpread] = {

    var reversedArgs: List[js.TreeOrJSSpread] = Nil

    for ((arg, info) <- args.zip(sym.jsParamInfos)) {
      if (info.repeated) {
        reversedArgs = genJSRepeatedParam(arg) reverse_::: reversedArgs
      } else if (info.capture) {
        // Ignore captures
        assert(sym.isClassConstructor,
            i"Found a capture param in method ${sym.fullName}, which is not a class constructor, at $pos")
      } else {
        val unboxedArg = genExpr(arg)
        val boxedArg = unboxedArg match {
          case js.Transient(UndefinedParam) =>
            unboxedArg
          case _ =>
            box(unboxedArg, info.info)
        }
        reversedArgs ::= boxedArg
      }
    }

    /* Remove all consecutive UndefinedParam's at the end of the argument
     * list. No check is performed whether they may be there, since they will
     * only be placed where default arguments can be anyway.
     */
    reversedArgs = reversedArgs.dropWhile(_.isInstanceOf[js.Transient])

    /* Find remaining UndefinedParam and replace by js.Undefined. This can
     * happen with named arguments or with multiple argument lists.
     */
    reversedArgs = reversedArgs map {
      case js.Transient(UndefinedParam) => js.Undefined()
      case arg => arg
    }

    reversedArgs.reverse
  }

  /** Gen JS code for a repeated param of a JS method.
   *
   *  In this case `arg` has type `Seq[T]` for some `T`, but the result should
   *  be an expanded list of the elements in the sequence. So this method
   *  takes care of the conversion.
   *
   *  It is specialized for the shapes of tree generated by the desugaring
   *  of repeated params in Scala, so that these are actually expanded at
   *  compile-time.
   *
   *  Otherwise, it returns a `JSSpread` with the `Seq` converted to a
   *  `js.Array`.
   */
  private def genJSRepeatedParam(arg: Tree): List[js.TreeOrJSSpread] = {
    tryGenRepeatedParamAsJSArray(arg, handleNil = true).getOrElse {
      /* Fall back to calling runtime.genTraversableOnce2jsArray
       * to perform the conversion to js.Array, then wrap in a Spread
       * operator.
       */
      implicit val pos: SourcePosition = arg.sourcePos
      val jsArrayArg = genModuleApplyMethod(
          jsdefn.Runtime_toJSVarArgs,
          List(genExpr(arg)))
      List(js.JSSpread(jsArrayArg))
    }
  }

  /** Try and expand an actual argument to a repeated param `(xs: T*)`.
   *
   *  This method recognizes the shapes of tree generated by the desugaring
   *  of repeated params in Scala, and expands them.
   *  If `arg` does not have the shape of a generated repeated param, this
   *  method returns `None`.
   */
  private def tryGenRepeatedParamAsJSArray(arg: Tree,
      handleNil: Boolean): Option[List[js.Tree]] = {
    implicit val pos = arg.span

    // Given a method `def foo(args: T*)`
    arg match {
      // foo(arg1, arg2, ..., argN) where N > 0
      case MaybeAsInstanceOf(WrapArray(MaybeAsInstanceOf(array: JavaSeqLiteral))) =>
        /* Value classes in arrays are already boxed, so no need to use
         * the type before erasure.
         * TODO Is this true in dotty?
         */
        Some(array.elems.map(e => box(genExpr(e), e.tpe)))

      // foo()
      case Ident(_) if handleNil && arg.symbol == defn.NilModule =>
        Some(Nil)

      // foo(argSeq: _*) - cannot be optimized
      case _ =>
        None
    }
  }

  private object MaybeAsInstanceOf {
    def unapply(tree: Tree): Some[Tree] = tree match {
      case TypeApply(asInstanceOf_? @ Select(base, _), _)
          if asInstanceOf_?.symbol == defn.Any_asInstanceOf =>
        Some(base)
      case _ =>
        Some(tree)
    }
  }

  private object WrapArray {
    lazy val isWrapArray: Set[Symbol] = {
      val names0 = defn.ScalaValueClasses().map(sym => nme.wrapXArray(sym.name))
      val names1 = names0 ++ Set(nme.wrapRefArray, nme.genericWrapArray)
      val symsInPredef = names1.map(defn.ScalaPredefModule.requiredMethod(_))
      val symsInScalaRunTime = names1.map(defn.ScalaRuntimeModule.requiredMethod(_))
      (symsInPredef ++ symsInScalaRunTime).toSet
    }

    def unapply(tree: Apply): Option[Tree] = tree match {
      case Apply(wrapArray_?, List(wrapped)) if isWrapArray(wrapArray_?.symbol) =>
        Some(wrapped)
      case _ =>
        None
    }
  }

  /** Wraps a `js.Array` to use as varargs. */
  def genJSArrayToVarArgs(arrayRef: js.Tree)(implicit pos: SourcePosition): js.Tree =
    genModuleApplyMethod(jsdefn.Runtime_toScalaVarArgs, List(arrayRef))

  /** Gen the actual capture values for a JS constructor based on its fake `new` invocation. */
  private def genCaptureValuesFromFakeNewInstance(tree: Tree): List[js.Tree] = {
    implicit val pos: Position = tree.span

    val Apply(fun @ Select(New(_), _), args) = tree: @unchecked
    val sym = fun.symbol

    /* We use the same strategy as genActualJSArgs to detect which parameters were
     * introduced by explicitouter or lambdalift (but reversed, of course).
     */

    val existedBeforeUncurry = atPhase(elimRepeatedPhase) {
      sym.info.paramNamess.flatten.toSet
    }

    for {
      (arg, paramName) <- args.zip(sym.info.paramNamess.flatten)
      if !existedBeforeUncurry(paramName)
    } yield {
      genExpr(arg)
    }
  }

  private def genVarRef(sym: Symbol)(implicit pos: Position): js.VarRef =
    js.VarRef(encodeLocalSym(sym))(toIRType(sym.info))

  private def genAssignableField(sym: Symbol, qualifier: Tree)(implicit pos: SourcePosition): (js.AssignLhs, Boolean) = {
    def qual = genExpr(qualifier)

    if (sym.owner.isNonNativeJSClass) {
      val f = if (sym.isJSExposed) {
        js.JSSelect(qual, genExpr(sym.jsName))
      } else if (sym.owner.isAnonymousClass) {
        js.JSSelect(
            js.JSSelect(qual, genPrivateFieldsSymbol()),
            encodeFieldSymAsStringLiteral(sym))
      } else {
        js.JSPrivateSelect(qual, encodeClassName(sym.owner),
            encodeFieldSym(sym))
      }

      (f, true)
    } else if (sym.hasAnnotation(jsdefn.JSExportTopLevelAnnot)) {
      val f = js.SelectStatic(encodeClassName(sym.owner), encodeFieldSym(sym))(jstpe.AnyType)
      (f, true)
    } else if (sym.hasAnnotation(jsdefn.JSExportStaticAnnot)) {
      val jsName = sym.getAnnotation(jsdefn.JSExportStaticAnnot).get.argumentConstantString(0).getOrElse {
        sym.defaultJSName
      }
      val companionClass = sym.owner.linkedClass
      val f = js.JSSelect(genLoadJSConstructor(companionClass), js.StringLiteral(jsName))
      (f, true)
    } else {
      val className = encodeClassName(sym.owner)
      val fieldIdent = encodeFieldSym(sym)

      /* #4370 Fields cannot have type NothingType, so we box them as
       * scala.runtime.Nothing$ instead. They will be initialized with
       * `null`, and any attempt to access them will throw a
       * `ClassCastException` (generated in the unboxing code).
       */
      val (irType, boxed) = toIRType(sym.info) match
        case jstpe.NothingType =>
          (encodeClassType(defn.NothingClass), true)
        case ftpe =>
          (ftpe, false)

      val f =
        if sym.is(JavaStatic) then
          js.SelectStatic(className, fieldIdent)(irType)
        else
          js.Select(qual, className, fieldIdent)(irType)

      (f, boxed)
    }
  }

  /** Gen JS code for loading a Java static field.
   */
  private def genLoadStaticField(sym: Symbol)(implicit pos: SourcePosition): js.Tree = {
    /* Actually, there is no static member in Scala.js. If we come here, that
     * is because we found the symbol in a Java-emitted .class in the
     * classpath. But the corresponding implementation in Scala.js will
     * actually be a val in the companion module.
     */

    if (sym == defn.BoxedUnit_UNIT) {
      js.Undefined()
    } else if (sym == defn.BoxedUnit_TYPE) {
      js.ClassOf(jstpe.VoidRef)
    } else {
      val className = encodeClassName(sym.owner)
      val method = encodeStaticMemberSym(sym)
      js.ApplyStatic(js.ApplyFlags.empty, className, method, Nil)(toIRType(sym.info))
    }
  }

  /** Generates a call to `runtime.privateFieldsSymbol()` */
  private def genPrivateFieldsSymbol()(implicit pos: SourcePosition): js.Tree =
    genModuleApplyMethod(jsdefn.Runtime_privateFieldsSymbol, Nil)

  /** Generate loading of a module value.
   *
   *  Can be given either the module symbol or its module class symbol.
   *
   *  If the module we load refers to the global scope (i.e., it is
   *  annotated with `@JSGlobalScope`), report a compile error specifying
   *  that a global scope object should only be used as the qualifier of a
   *  `.`-selection.
   */
  def genLoadModule(sym: Symbol)(implicit pos: SourcePosition): js.Tree =
    ruleOutGlobalScope(genLoadModuleOrGlobalScope(sym))

  /** Generate loading of a module value or the global scope.
   *
   *  Can be given either the module symbol of its module class symbol.
   *
   *  Unlike `genLoadModule`, this method does not fail if the module we load
   *  refers to the global scope.
   */
  def genLoadModuleOrGlobalScope(sym0: Symbol)(
      implicit pos: SourcePosition): MaybeGlobalScope = {

    require(sym0.is(Module),
        "genLoadModule called with non-module symbol: " + sym0)
    val sym = if (sym0.isTerm) sym0.moduleClass else sym0

    // Does that module refer to the global scope?
    if (sym.hasAnnotation(jsdefn.JSGlobalScopeAnnot)) {
      MaybeGlobalScope.GlobalScope(pos)
    } else {
      val cls = encodeClassName(sym)
      val tree =
        if (sym.isJSType) js.LoadJSModule(cls)
        else js.LoadModule(cls)
      MaybeGlobalScope.NotGlobalScope(tree)
    }
  }

  /** Gen JS code representing the constructor of a JS class. */
  private def genLoadJSConstructor(sym: Symbol)(
      implicit pos: Position): js.Tree = {
    assert(!isStaticModule(sym) && !sym.is(Trait),
        s"genLoadJSConstructor called with non-class $sym")
    js.LoadJSConstructor(encodeClassName(sym))
  }

  private inline val GenericGlobalObjectInformationMsg = {
    "\n  " +
    "See https://www.scala-js.org/doc/interoperability/global-scope.html " +
    "for further information."
  }

  /** Rule out the `GlobalScope` case of a `MaybeGlobalScope` and extract the
   *  value tree.
   *
   *  If `tree` represents the global scope, report a compile error.
   */
  private def ruleOutGlobalScope(tree: MaybeGlobalScope): js.Tree = {
    tree match {
      case MaybeGlobalScope.NotGlobalScope(t) =>
        t
      case MaybeGlobalScope.GlobalScope(pos) =>
        reportErrorLoadGlobalScope()(pos)
    }
  }

  /** Report a compile error specifying that the global scope cannot be
   *  loaded as a value.
   */
  private def reportErrorLoadGlobalScope()(implicit pos: SourcePosition): js.Tree = {
    report.error(
        "Loading the global scope as a value (anywhere but as the " +
        "left-hand-side of a `.`-selection) is not allowed." +
        GenericGlobalObjectInformationMsg,
        pos)
    js.Undefined()
  }

  /** Gen a JS bracket select or a `JSGlobalRef`.
   *
   *  If the receiver is a normal value, i.e., not the global scope, then
   *  emit a `JSSelect`.
   *
   *  Otherwise, if the `item` is a constant string that is a valid
   *  JavaScript identifier, emit a `JSGlobalRef`.
   *
   *  Otherwise, report a compile error.
   */
  private def genJSSelectOrGlobalRef(qual: MaybeGlobalScope, item: js.Tree)(
      implicit pos: SourcePosition): js.AssignLhs = {
    qual match {
      case MaybeGlobalScope.NotGlobalScope(qualTree) =>
        js.JSSelect(qualTree, item)

      case MaybeGlobalScope.GlobalScope(_) =>
        item match {
          case js.StringLiteral(value) =>
            if (js.JSGlobalRef.isValidJSGlobalRefName(value)) {
              js.JSGlobalRef(value)
            } else if (js.JSGlobalRef.ReservedJSIdentifierNames.contains(value)) {
              report.error(
                  "Invalid selection in the global scope of the reserved " +
                  s"identifier name `$value`." +
                  GenericGlobalObjectInformationMsg,
                  pos)
              js.JSGlobalRef("erroneous")
            } else {
              report.error(
                  "Selecting a field of the global scope whose name is " +
                  "not a valid JavaScript identifier is not allowed." +
                  GenericGlobalObjectInformationMsg,
                  pos)
              js.JSGlobalRef("erroneous")
            }

          case _ =>
            report.error(
                "Selecting a field of the global scope with a dynamic " +
                "name is not allowed." +
                GenericGlobalObjectInformationMsg,
                pos)
            js.JSGlobalRef("erroneous")
        }
    }
  }

  /** Gen a JS bracket method apply or an apply of a `GlobalRef`.
   *
   *  If the receiver is a normal value, i.e., not the global scope, then
   *  emit a `JSMethodApply`.
   *
   *  Otherwise, if the `method` is a constant string that is a valid
   *  JavaScript identifier, emit a `JSFunctionApply(JSGlobalRef(...), ...)`.
   *
   *  Otherwise, report a compile error.
   */
  private def genJSMethodApplyOrGlobalRefApply(
      receiver: MaybeGlobalScope, method: js.Tree, args: List[js.TreeOrJSSpread])(
      implicit pos: SourcePosition): js.Tree = {
    receiver match {
      case MaybeGlobalScope.NotGlobalScope(receiverTree) =>
        js.JSMethodApply(receiverTree, method, args)

      case MaybeGlobalScope.GlobalScope(_) =>
        method match {
          case js.StringLiteral(value) =>
            if (js.JSGlobalRef.isValidJSGlobalRefName(value)) {
              js.JSFunctionApply(js.JSGlobalRef(value), args)
            } else if (js.JSGlobalRef.ReservedJSIdentifierNames.contains(value)) {
              report.error(
                  "Invalid call in the global scope of the reserved " +
                  s"identifier name `$value`." +
                  GenericGlobalObjectInformationMsg,
                  pos)
              js.Undefined()
            } else {
              report.error(
                  "Calling a method of the global scope whose name is not " +
                  "a valid JavaScript identifier is not allowed." +
                  GenericGlobalObjectInformationMsg,
                  pos)
              js.Undefined()
            }

          case _ =>
            report.error(
                "Calling a method of the global scope with a dynamic " +
                "name is not allowed." +
                GenericGlobalObjectInformationMsg,
                pos)
            js.Undefined()
        }
    }
  }

  private def computeJSNativeLoadSpecOfValDef(sym: Symbol): js.JSNativeLoadSpec = {
    atPhaseBeforeTransforms {
      computeJSNativeLoadSpecOfInPhase(sym)
    }
  }

  private def computeJSNativeLoadSpecOfClass(sym: Symbol): Option[js.JSNativeLoadSpec] = {
    if (sym.is(Trait) || sym.hasAnnotation(jsdefn.JSGlobalScopeAnnot)) {
      None
    } else {
      atPhaseBeforeTransforms {
        if (sym.owner.isStaticOwner)
          Some(computeJSNativeLoadSpecOfInPhase(sym))
        else
          None
      }
    }
  }

  private def computeJSNativeLoadSpecOfInPhase(sym: Symbol)(using Context): js.JSNativeLoadSpec = {
    import js.JSNativeLoadSpec._

    val symOwner = sym.owner

    // Marks a code path as unexpected because it should have been reported as an error in `PrepJSInterop`.
    def unexpected(msg: String): Nothing =
      throw new FatalError(i"$msg for ${sym.fullName} at ${sym.srcPos}")

    if (symOwner.hasAnnotation(jsdefn.JSNativeAnnot)) {
      val jsName = sym.jsName match {
        case JSName.Literal(jsName) => jsName
        case JSName.Computed(_)     => unexpected("could not read the simple JS name as a string literal")
      }

      if (symOwner.hasAnnotation(jsdefn.JSGlobalScopeAnnot)) {
        Global(jsName, Nil)
      } else {
        val ownerLoadSpec = computeJSNativeLoadSpecOfInPhase(symOwner)
        ownerLoadSpec match {
          case Global(globalRef, path) =>
            Global(globalRef, path :+ jsName)
          case Import(module, path) =>
            Import(module, path :+ jsName)
          case ImportWithGlobalFallback(Import(module, modulePath), Global(globalRef, globalPath)) =>
            ImportWithGlobalFallback(
                Import(module, modulePath :+ jsName),
                Global(globalRef, globalPath :+ jsName))
        }
      }
    } else {
      def parsePath(pathName: String): List[String] =
        pathName.split('.').toList

      def parseGlobalPath(pathName: String): Global = {
        val globalRef :: path = parsePath(pathName): @unchecked
        Global(globalRef, path)
      }

      val annot = sym.annotations.find { annot =>
        annot.symbol == jsdefn.JSGlobalAnnot || annot.symbol == jsdefn.JSImportAnnot
      }.getOrElse {
        unexpected("could not find the JS native load spec annotation")
      }

      if (annot.symbol == jsdefn.JSGlobalAnnot) {
        val pathName = annot.argumentConstantString(0).getOrElse {
          sym.defaultJSName
        }
        parseGlobalPath(pathName)
      } else { // annot.symbol == jsdefn.JSImportAnnot
        val module = annot.argumentConstantString(0).getOrElse {
          unexpected("could not read the module argument as a string literal")
        }
        val path = annot.argumentConstantString(1).fold {
          if (annot.arguments.sizeIs < 2)
            parsePath(sym.defaultJSName)
          else
            Nil
        } { pathName =>
          parsePath(pathName)
        }
        val importSpec = Import(module, path)
        annot.argumentConstantString(2).fold[js.JSNativeLoadSpec] {
          importSpec
        } { globalPathName =>
          ImportWithGlobalFallback(importSpec, parseGlobalPath(globalPathName))
        }
      }
    }
  }

  private def isMethodStaticInIR(sym: Symbol): Boolean =
    sym.is(JavaStatic)

  /** Generate a Class[_] value (e.g. coming from classOf[T]) */
  private def genClassConstant(tpe: Type)(implicit pos: Position): js.Tree =
    js.ClassOf(toTypeRef(tpe))

  private def isStaticModule(sym: Symbol): Boolean =
    sym.is(Module) && sym.isStatic

  private def isPrimitiveValueType(tpe: Type): Boolean = {
    tpe.widenDealias match {
      case JavaArrayType(_)   => false
      case _: ErasedValueType => false
      case t                  => t.typeSymbol.asClass.isPrimitiveValueClass
    }
  }

  protected lazy val isHijackedClass: Set[Symbol] = {
    /* This list is a duplicate of ir.Definitions.HijackedClasses, but
     * with global.Symbol's instead of IR encoded names as Strings.
     * We also add java.lang.Void, which BoxedUnit "erases" to.
     */
    Set[Symbol](
        defn.BoxedUnitClass, defn.BoxedBooleanClass, defn.BoxedCharClass, defn.BoxedByteClass,
        defn.BoxedShortClass, defn.BoxedIntClass, defn.BoxedLongClass, defn.BoxedFloatClass,
        defn.BoxedDoubleClass, defn.StringClass, jsdefn.JavaLangVoidClass
    )
  }

  private def isMaybeJavaScriptException(tpe: Type): Boolean =
    jsdefn.JavaScriptExceptionClass.isSubClass(tpe.typeSymbol)

  private def hasDefaultCtorArgsAndJSModule(classSym: Symbol): Boolean = {
    def hasNativeCompanion =
      classSym.companionModule.moduleClass.hasAnnotation(jsdefn.JSNativeAnnot)
    def hasDefaultParameters =
      classSym.info.decls.exists(sym => sym.isClassConstructor && sym.hasDefaultParams)

    hasNativeCompanion && hasDefaultParameters
  }

  // Copied from DottyBackendInterface

  private val desugared = new java.util.IdentityHashMap[Type, tpd.Select]

  def desugarIdent(i: Ident): Option[tpd.Select] = {
    var found = desugared.get(i.tpe)
    if (found == null) {
      tpd.desugarIdent(i) match {
        case sel: tpd.Select =>
          desugared.put(i.tpe, sel)
          found = sel
        case _ =>
      }
    }
    if (found == null) None else Some(found)
  }
}

object JSCodeGen {

  private val NullPointerExceptionClass = ClassName("java.lang.NullPointerException")
  private val JSObjectClassName = ClassName("scala.scalajs.js.Object")
  private val JavaScriptExceptionClassName = ClassName("scala.scalajs.js.JavaScriptException")

  private val ObjectClassRef = jstpe.ClassRef(ir.Names.ObjectClass)

  private val newSimpleMethodName = SimpleMethodName("new")

  private val selectedValueMethodName = MethodName("selectedValue", Nil, ObjectClassRef)

  private val ObjectArgConstructorName = MethodName.constructor(List(ObjectClassRef))

  private val thisOriginalName = OriginalName("this")

  sealed abstract class MaybeGlobalScope

  object MaybeGlobalScope {
    final case class NotGlobalScope(tree: js.Tree) extends MaybeGlobalScope

    final case class GlobalScope(pos: SourcePosition) extends MaybeGlobalScope
  }

  /** Marker object for undefined parameters in JavaScript semantic calls.
   *
   *  To be used inside a `js.Transient` node.
   */
  case object UndefinedParam extends js.Transient.Value {
    val tpe: jstpe.Type = jstpe.UndefType

    def traverse(traverser: ir.Traversers.Traverser): Unit = ()

    def transform(transformer: ir.Transformers.Transformer, isStat: Boolean)(
        implicit pos: ir.Position): js.Tree = {
      js.Transient(this)
    }

    def printIR(out: ir.Printers.IRTreePrinter): Unit =
      out.print("<undefined-param>")
  }

  /** Info about a default param accessor.
   *
   *  The method must have a default getter name for this class to make sense.
   */
  private class DefaultParamInfo(sym: Symbol)(using Context) {
    private val methodName = sym.name.exclude(DefaultGetterName)

    def isForConstructor: Boolean = methodName == nme.CONSTRUCTOR

    /** When `isForConstructor` is true, returns the owner of the attached
     *  constructor.
     */
    def constructorOwner: Symbol = sym.owner.linkedClass

    /** When `isForConstructor` is false, returns the method attached to the
     *  specified default accessor.
     */
    def attachedMethod: Symbol = {
      // If there are overloads, we need to find the one that has default params.
      val overloads = sym.owner.info.decl(methodName)
      if (!overloads.isOverloaded)
        overloads.symbol
      else
        overloads.suchThat(_.is(HasDefaultParams, butNot = Bridge)).symbol
    }
  }

}
