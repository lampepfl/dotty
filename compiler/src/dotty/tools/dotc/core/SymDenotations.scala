package dotty.tools
package dotc
package core

import Periods.*, Contexts.*, Symbols.*, Denotations.*, Names.*, NameOps.*, Annotations.*
import Types.*, Flags.*, Decorators.*, DenotTransformers.*, StdNames.*, Scopes.*
import NameOps.*, NameKinds.*
import Phases.{Phase, typerPhase, unfusedPhases}
import Constants.Constant
import TypeApplications.TypeParamInfo
import Scopes.Scope
import dotty.tools.io.AbstractFile
import Decorators.*
import ast.*
import ast.Trees.{LambdaTypeTree, TypeBoundsTree}
import Trees.Literal
import Variances.Variance
import annotation.tailrec
import util.SimpleIdentityMap
import util.Stats
import java.util.WeakHashMap
import scala.util.control.NonFatal
import config.Config
import reporting.*
import collection.mutable
import cc.{CapturingType, derivedCapturingType}

import scala.annotation.internal.sharable
import scala.compiletime.uninitialized

object SymDenotations {

  /** A sym-denotation represents the contents of a definition
   *  during a period.
   */
  class SymDenotation private[SymDenotations] (
    symbol: Symbol,
    final val maybeOwner: Symbol,
    final val name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol = NoSymbol) extends SingleDenotation(symbol, initInfo, name.isTypeName) {

    //assert(symbol.id != 4940, name)

    override def hasUniqueSym: Boolean = exists

    /** Debug only
    override def validFor_=(p: Period) = {
      super.validFor_=(p)
    }
    */
    if (Config.checkNoSkolemsInInfo) assertNoSkolems(initInfo)

    // ------ Getting and setting fields -----------------------------

    private var myFlags: FlagSet = adaptFlags(initFlags)
    private var myPrivateWithin: Symbol = initPrivateWithin
    private var myAnnotations: List[Annotation] = Nil
    private var myParamss: List[List[Symbol]] = Nil

    /** The owner of the symbol; overridden in NoDenotation */
    def owner: Symbol = maybeOwner

    /** The flag set */
    final def flags(using Context): FlagSet = { ensureCompleted(); myFlags }

    /** The flag set without forcing symbol completion.
     *  Should be used only for printing.
     */
    private[dotc] final def flagsUNSAFE: FlagSet = myFlags

    final def flagsString(using Context): String = flags.flagsString

    /** Adapt flag set to this denotation's term or type nature */
    private def adaptFlags(flags: FlagSet) = if (isType) flags.toTypeFlags else flags.toTermFlags

    /** Update the flag set */
    final def flags_=(flags: FlagSet): Unit =
      myFlags = adaptFlags(flags)

    /** Set given flags(s) of this denotation */
    final def setFlag(flags: FlagSet): Unit = { myFlags |= flags }

    /** Unset given flags(s) of this denotation */
    final def resetFlag(flags: FlagSet): Unit = { myFlags &~= flags }

    /** Set applicable flags in {NoInits, PureInterface}
     *  @param  parentFlags  The flags that match the class or trait's parents
     *  @param  bodyFlags    The flags that match the class or trait's body
     */
    final def setNoInitsFlags(parentFlags: FlagSet, bodyFlags: FlagSet): Unit =
      setFlag(
        if (myFlags.is(Trait)) NoInitsInterface & bodyFlags // no parents are initialized from a trait
        else NoInits & bodyFlags & parentFlags)

    final def setStableConstructor()(using Context): Unit =
      val ctorStable = if myFlags.is(Trait) then myFlags.is(NoInits) else isNoInitsRealClass
      if ctorStable then primaryConstructor.setFlag(StableRealizable)

    def isCurrent(fs: FlagSet)(using Context): Boolean =
      def knownFlags(info: Type): FlagSet = info match
        case _: SymbolLoader | _: ModuleCompleter => FromStartFlags
        case _ => AfterLoadFlags
      !myInfo.isInstanceOf[LazyType] || fs <= knownFlags(myInfo)

    final def relevantFlagsFor(fs: FlagSet)(using Context) =
      if (isCurrent(fs)) myFlags else flags

    /** Has this denotation one of given flag set? */
    final def is(flag: Flag)(using Context): Boolean =
      (if (isCurrent(flag)) myFlags else flags).is(flag)

    /** Has this denotation one of the flags in `fs` set? */
    final def isOneOf(fs: FlagSet)(using Context): Boolean =
      (if (isCurrent(fs)) myFlags else flags).isOneOf(fs)

    /** Has this denotation the given flag set, whereas none of the flags
     *  in `butNot` are set?
     */
    final def is(flag: Flag, butNot: FlagSet)(using Context): Boolean =
      (if (isCurrent(flag) && isCurrent(butNot)) myFlags else flags).is(flag, butNot)

    /** Has this denotation one of the flags in `fs` set, whereas none of the flags
     *  in `butNot` are set?
     */
    final def isOneOf(fs: FlagSet, butNot: FlagSet)(using Context): Boolean =
      (if (isCurrent(fs) && isCurrent(butNot)) myFlags else flags).isOneOf(fs, butNot)

    /** Has this denotation all of the flags in `fs` set? */
    final def isAllOf(fs: FlagSet)(using Context): Boolean =
      (if (isCurrent(fs)) myFlags else flags).isAllOf(fs)

    /** Has this denotation all of the flags in `fs` set, whereas none of the flags
     *  in `butNot` are set?
     */
    final def isAllOf(fs: FlagSet, butNot: FlagSet)(using Context): Boolean =
      (if (isCurrent(fs) && isCurrent(butNot)) myFlags else flags).isAllOf(fs, butNot)

    /** The type info, or, if symbol is not yet completed, the completer */
    final def infoOrCompleter: Type = myInfo

    /** Optionally, the info if it is completed */
    final def unforcedInfo: Option[Type] = myInfo match {
      case myInfo: LazyType => None
      case _ => Some(myInfo)
    }

    final def completeFrom(completer: LazyType)(using Context): Unit =
      if completer.needsCompletion(this) then
        if (Config.showCompletions) {
          println(i"${"  " * indent}completing ${if (isType) "type" else "val"} $name")
          indent += 1

          if (myFlags.is(Touched)) throw CyclicReference(this)
          myFlags |= Touched

          // completions.println(s"completing ${this.debugString}")
          try atPhase(validFor.firstPhaseId)(completer.complete(this))
          catch {
            case ex: CyclicReference =>
              println(s"error while completing ${this.debugString}")
              throw ex
          }
          finally {
            indent -= 1
            println(i"${"  " * indent}completed $name in $owner")
          }
        }
        else
          val traceCycles = CyclicReference.isTraced
          try
            if traceCycles then
              CyclicReference.pushTrace("compute the signature of ", symbol, "")
            if myFlags.is(Touched) then
              throw CyclicReference(this)(using ctx.withOwner(symbol))
            myFlags |= Touched
            atPhase(validFor.firstPhaseId)(completer.complete(this))
          finally
            if traceCycles then CyclicReference.popTrace()

    protected[dotc] def info_=(tp: Type): Unit = {
      /* // DEBUG
       def illegal: String = s"illegal type for $this: $tp"
      if (this is Module) // make sure module invariants that allow moduleClass and sourceModule to work are kept.
        tp match {
          case tp: ClassInfo => assert(tp.selfInfo.isInstanceOf[TermRefBySym], illegal)
          case tp: NamedType => assert(tp.isInstanceOf[TypeRefBySym], illegal)
          case tp: ExprType => assert(tp.resultType.isInstanceOf[TypeRefBySym], illegal)
          case _ =>
        }
        */
      if (Config.checkNoSkolemsInInfo) assertNoSkolems(tp)
      myInfo = tp
    }

    /** The name, except
     *   - if this is a module class, strip the module class suffix
     *   - if this is a companion object with a clash-avoiding name, strip the
     *     "avoid clash" suffix
     */
    def effectiveName(using Context): Name =
      if (this.is(ModuleClass)) name.stripModuleClassSuffix
      else name

    /** The privateWithin boundary, NoSymbol if no boundary is given.
     */
    @tailrec
    final def privateWithin(using Context): Symbol = myInfo match {
      case myInfo: ModuleCompleter =>
        // Instead of completing the ModuleCompleter, we can get `privateWithin`
        // directly from the module class, which might require less completions.
        myInfo.moduleClass.privateWithin
      case _: SymbolLoader =>
         // Completing a SymbolLoader might call `setPrivateWithin()`
        completeOnce()
        privateWithin
      case _ =>
        // Otherwise, no completion is necessary, see the preconditions of `markAbsent()`.
        myPrivateWithin
    }

    /** Set privateWithin, prefer setting it at symbol-creation time instead if
     *  possible.
     *  @pre `isCompleting` is false, or this is a ModuleCompleter or SymbolLoader
     */
    protected[dotc] final def setPrivateWithin(pw: Symbol)(using Context): Unit = {
      if (isCompleting)
        assert(myInfo.isInstanceOf[ModuleCompleter | SymbolLoader],
          s"Illegal call to `setPrivateWithin($pw)` while completing $this using completer $myInfo")
      myPrivateWithin = pw
    }

    /** The annotations of this denotation */
    final def annotations(using Context): List[Annotation] = {
      ensureCompleted(); myAnnotations
    }

    /** The annotations without ensuring that the symbol is completed.
     *  Used for diagnostics where we don't want to force symbols.
     */
    final def annotationsUNSAFE(using Context): List[Annotation] = myAnnotations

    /** Update the annotations of this denotation */
    final def annotations_=(annots: List[Annotation]): Unit =
      myAnnotations = annots

    /** Does this denotation have an annotation matching the given class symbol? */
    final def hasAnnotation(cls: Symbol)(using Context): Boolean =
      dropOtherAnnotations(annotations, cls).nonEmpty

    /** Apply transform `f` to all annotations of this denotation */
    final def transformAnnotations(f: Annotation => Annotation)(using Context): Unit =
      annotations = annotations.mapConserve(f)

    /** Keep only those annotations that satisfy `p` */
    final def filterAnnotations(p: Annotation => Boolean)(using Context): Unit =
      annotations = annotations.filterConserve(p)

    def annotationsCarrying(meta: Set[Symbol], orNoneOf: Set[Symbol] = Set.empty)(using Context): List[Annotation] =
      annotations.filterConserve(_.hasOneOfMetaAnnotation(meta, orNoneOf = orNoneOf))

    def keepAnnotationsCarrying(phase: DenotTransformer, meta: Set[Symbol], orNoneOf: Set[Symbol] = Set.empty)(using Context): Unit =
      updateAnnotationsAfter(phase, annotationsCarrying(meta, orNoneOf = orNoneOf))

    def updateAnnotationsAfter(phase: DenotTransformer, annots: List[Annotation])(using Context): Unit =
      if annots ne annotations then
        val cpy = copySymDenotation()
        cpy.annotations = annots
        cpy.installAfter(phase)

    /** Optionally, the annotation matching the given class symbol */
    final def getAnnotation(cls: Symbol)(using Context): Option[Annotation] =
      dropOtherAnnotations(annotations, cls) match {
        case annot :: _ => Some(annot)
        case nil => None
      }

    /** The same as getAnnotation, but without ensuring
     *  that the symbol carrying the annotation is completed
     */
    final def unforcedAnnotation(cls: Symbol)(using Context): Option[Annotation] =
      dropOtherAnnotations(myAnnotations, cls) match {
        case annot :: _ => Some(annot)
        case nil => None
      }

    /** Add given annotation to the annotations of this denotation */
    final def addAnnotation(annot: Annotation): Unit =
      annotations = annot :: myAnnotations

    /** Add the given annotation without parameters to the annotations of this denotation */
    final def addAnnotation(cls: ClassSymbol)(using Context): Unit =
      addAnnotation(Annotation(cls, symbol.span))

    /** Remove annotation with given class from this denotation */
    final def removeAnnotation(cls: Symbol)(using Context): Unit =
      annotations = myAnnotations.filterNot(_ matches cls)

    /** Remove any annotations with same class as `annot`, and add `annot` */
    final def updateAnnotation(annot: Annotation)(using Context): Unit = {
      removeAnnotation(annot.symbol)
      addAnnotation(annot)
    }

    /** Add all given annotations to this symbol */
    final def addAnnotations(annots: IterableOnce[Annotation])(using Context): Unit =
      annots.iterator.foreach(addAnnotation)

    @tailrec
    private def dropOtherAnnotations(anns: List[Annotation], cls: Symbol)(using Context): List[Annotation] = anns match {
      case ann :: rest => if (ann matches cls) anns else dropOtherAnnotations(rest, cls)
      case Nil => Nil
    }

    /** If this is a method, the parameter symbols, by section.
     *  Both type and value parameters are included. Empty sections are skipped.
     */
    final def rawParamss: List[List[Symbol]] = myParamss
    final def rawParamss_=(pss: List[List[Symbol]]): Unit =
      myParamss = pss

    final def setParamss(paramss: List[List[Symbol]])(using Context): Unit =
      rawParamss = paramss.filterConserve(!_.isEmpty)

    final def setParamssFromDefs(paramss: List[tpd.ParamClause])(using Context): Unit =
      setParamss(paramss.map(_.map(_.symbol)))

    /** The symbols of each type parameter list and value parameter list of this
     *  method, or Nil if this isn't a method.
     *
     *  Makes use of `rawParamss` when present, or constructs fresh parameter symbols otherwise.
     *  This method can be allocation-heavy.
     */
    final def paramSymss(using Context): List[List[Symbol]] =

      def recurWithParamss(info: Type, paramss: List[List[Symbol]]): List[List[Symbol]] =
        info match
          case info: LambdaType =>
            if info.paramNames.isEmpty then Nil :: recurWithParamss(info.resType, paramss)
            else paramss.head :: recurWithParamss(info.resType, paramss.tail)
          case _ =>
            Nil

      def recurWithoutParamss(info: Type): List[List[Symbol]] = info match
        case info: LambdaType =>
          val params = info.paramNames.lazyZip(info.paramInfos).map((pname, ptype) =>
            newSymbol(symbol, pname, SyntheticParam, ptype))
          val prefs = params.map(_.namedType)
          for param <- params do
            param.info = param.info.substParams(info, prefs)
          params :: recurWithoutParamss(info.instantiate(prefs))
        case _ =>
          Nil

      ensureCompleted()
      if rawParamss.isEmpty then recurWithoutParamss(info)
      else recurWithParamss(info, rawParamss)
    end paramSymss

    /** The extension parameter of this extension method
     *  @pre this symbol is an extension method
     */
    final def extensionParam(using Context): Symbol =
      def leadParam(paramss: List[List[Symbol]]): Symbol = paramss match
        case (param :: _) :: paramss1 if param.isType => leadParam(paramss1)
        case _ :: (snd :: Nil) :: _ if name.isRightAssocOperatorName => snd
        case (fst :: Nil) :: _ => fst
        case _ => NoSymbol
      assert(isAllOf(ExtensionMethod))
      ensureCompleted()
      leadParam(rawParamss)

    /** The denotation is completed: info is not a lazy type and attributes have defined values */
    final def isCompleted: Boolean = !myInfo.isInstanceOf[LazyType]

    /** The denotation is in train of being completed */
    final def isCompleting: Boolean = myFlags.is(Touched) && !isCompleted

    /** The completer of this denotation. @pre: Denotation is not yet completed */
    final def completer: LazyType = myInfo.asInstanceOf[LazyType]

    /** If this denotation is not completed, run the completer.
     *  The resulting info might be another completer.
     *
     *  @see ensureCompleted
     */
    final def completeOnce()(using Context): Unit = myInfo match {
      case myInfo: LazyType =>
        completeFrom(myInfo)
      case _ =>
    }

    /** Make sure this denotation is fully completed.
     *
     *  @see completeOnce
     */
    final def ensureCompleted()(using Context): Unit = info

    /** The symbols defined in this class or object.
     *  Careful! This does not force the type, so is compilation order dependent.
     *  This method should be used only in the following circumstances:
     *
     *  1. When accessing type parameters or type parameter accessors (both are entered before
     *     completion).
     *  2. When obtaining the current scope in order to enter, rename or delete something there.
     *  3. When playing it safe in order not to raise CylicReferences, e.g. for printing things
     *     or taking more efficient shortcuts (e.g. the stillValid test).
     */
    final def unforcedDecls(using Context): Scope = myInfo match {
      case cinfo: LazyType =>
        val knownDecls = cinfo.decls
        if (knownDecls ne EmptyScope) knownDecls
        else { completeOnce(); unforcedDecls }
      case _ => info.decls
    }

    /** If this is a package class, the symbols entered in it
     *  before it is completed. (this is needed to eagerly enter synthetic
     *  aliases such as AnyRef into a package class without forcing it.
     *  Right now, the only usage is for the AnyRef alias in Definitions.
     */
    final private[core] def currentPackageDecls(using Context): MutableScope = myInfo match {
      case pinfo: SymbolLoaders.PackageLoader => pinfo.currentDecls
      case _ => unforcedDecls.openForMutations
    }

    /** If this is an opaque alias, replace the right hand side `info`
     *  by appropriate bounds and store `info` in the refinement of the
     *  self type of the enclosing class.
     *  Otherwise return `info`
     *
     *  @param info    Is assumed to be a (lambda-abstracted) right hand side TypeAlias
     *                 of the opaque type definition.
     *  @param rhs     The right hand side tree of the type definition
     *  @param tparams The type parameters with which the right-hand side bounds should be abstracted
     *
     */
    def opaqueToBounds(info: Type, rhs: tpd.Tree, tparams: List[TypeSymbol])(using Context): Type =

      def setAlias(tp: Type) =
        def recur(self: Type): Unit = self match
          case RefinedType(parent, name, rinfo) => rinfo match
            case TypeAlias(lzy: LazyRef) if name == this.name =>
              if !lzy.completed then
                lzy.update(tp)
              else
                throw CyclicReference(this)
            case _ =>
              recur(parent)
        recur(owner.asClass.givenSelfType)
      end setAlias

      def bounds(t: tpd.Tree): TypeBounds = t match
        case LambdaTypeTree(tparams, body) =>
          bounds(body)
        case TypeBoundsTree(lo, hi, alias) =>
          assert(!alias.isEmpty)
          TypeBounds(lo.tpe, hi.tpe)
        case _ =>
          TypeBounds.empty

      info match
        case info: AliasingBounds if isOpaqueAlias && owner.isClass =>
          setAlias(info.alias)
          HKTypeLambda.boundsFromParams(tparams, bounds(rhs))
        case _ =>
          info
    end opaqueToBounds

    // ------ Names ----------------------------------------------

    /** The expanded name of this denotation. */
    final def expandedName(using Context): Name =
      if (name.is(ExpandedName) || isConstructor) name
      else name.expandedName(initial.owner)
        // need to use initial owner to disambiguate, as multiple private symbols with the same name
        // might have been moved from different origins into the same class

    /** The effective name with which the denoting symbol was created */
    final def originalName(using Context): Name = initial.effectiveName

    /** The owner with which the denoting symbol was created. */
    final def originalOwner(using Context): Symbol = initial.maybeOwner

    /** The encoded full path name of this denotation, where outer names and inner names
     *  are separated by `separator` strings as indicated by the given name kind.
     *  Drops package objects. Represents each term in the owner chain by a simple `_$`.
     */
    def fullNameSeparated(kind: QualifiedNameKind)(using Context): Name =
      maybeOwner.fullNameSeparated(kind, kind, name)

    /** The encoded full path name of this denotation (separated by `prefixKind`),
     *  followed by the separator implied by `kind` and the given `name`.
     *  Drops package objects. Represents each term in the owner chain by a simple `_$`.
     */
    def fullNameSeparated(prefixKind: QualifiedNameKind, kind: QualifiedNameKind, name: Name)(using Context): Name =
      if (symbol == NoSymbol || isEffectiveRoot || kind == FlatName && is(PackageClass))
        name
      else {
        var filler = ""
        var encl = symbol
        while (!encl.isClass && !encl.isPackageObject) {
          encl = encl.owner
          filler += "_$"
        }
        var prefix = encl.fullNameSeparated(prefixKind)
        if (kind.separator == "$")
          // duplicate scalac's behavior: don't write a double '$$' for module class members.
          prefix = prefix.exclude(ModuleClassName)
        def qualify(n: SimpleName) =
          val qn = kind(prefix.toTermName, if (filler.isEmpty) n else termName(filler + n))
          if kind == FlatName && !encl.is(JavaDefined) then qn.compactified else qn
        val fn = name.replaceDeep {
          case n: SimpleName => qualify(n)
        }
        if name.isTypeName then fn.toTypeName else fn.toTermName
      }

    /** The encoded flat name of this denotation, where joined names are separated by `separator` characters. */
    def flatName(using Context): Name = fullNameSeparated(FlatName)

    /** `fullName` where `.' is the separator character */
    def fullName(using Context): Name = fullNameSeparated(QualifiedName)

    /** The fully qualified name on the JVM of the class corresponding to this symbol. */
    def binaryClassName(using Context): String =
      val builder = new StringBuilder
      val pkg = enclosingPackageClass
      if !pkg.isEffectiveRoot then
        builder.append(pkg.fullName.mangledString)
        builder.append(".")
      val flatName = this.flatName
      // Some companion objects are fake (that is, they're a compiler fiction
      // that doesn't correspond to a class that exists at runtime), this
      // can happen in two cases:
      // - If a Java class has static members.
      // - If we create constructor proxies for a class (see NamerOps#addConstructorProxies).
      //
      // In both cases it's may be vital that we don't return the object name.
      // For instance, sending it to zinc: when sbt is restarted, zinc will inspect the binary
      // dependencies to see if they're still on the classpath, if it
      // doesn't find them it will invalidate whatever referenced them, so
      // any reference to a fake companion will lead to extra recompilations.
      // Instead, use the class name since it's guaranteed to exist at runtime.
      val clsFlatName = if isOneOf(JavaDefined | ConstructorProxy) then flatName.stripModuleClassSuffix else flatName
      builder.append(clsFlatName.mangledString)
      builder.toString

    private var myTargetName: Name | Null = null

    private def computeTargetName(targetNameAnnot: Option[Annotation])(using Context): Name =
      targetNameAnnot match
        case Some(ann) =>
          ann.arguments match
            case Literal(Constant(str: String)) :: Nil =>
              if isType then
                if is(ModuleClass) then str.toTypeName.moduleClassName
                else str.toTypeName
              else str.toTermName
            case _ => name
        case _ => name

    def setTargetName(name: Name): Unit =
      myTargetName = name

    def hasTargetName(name: Name)(using Context): Boolean =
      targetName.matchesTargetName(name)

    /** The name given in a `@targetName` annotation if one is present, `name` otherwise */
    def targetName(using Context): Name =
      if myTargetName == null then
        val carrier: SymDenotation =
          if isAllOf(ModuleClass | Synthetic) then companionClass else this
        val targetNameAnnot =
          if carrier.isCompleting // annotations have been set already in this case
          then carrier.unforcedAnnotation(defn.TargetNameAnnot)
          else carrier.getAnnotation(defn.TargetNameAnnot)
        myTargetName = computeTargetName(targetNameAnnot)
        if name.is(SuperAccessorName) then
          myTargetName = myTargetName.nn.unmangle(List(ExpandedName, SuperAccessorName, ExpandPrefixName))

      myTargetName.nn

    // ----- Tests -------------------------------------------------

    /** Is this denotation a class? */
    final def isClass: Boolean = isInstanceOf[ClassDenotation]

    /** Is this denotation a non-trait class? */
    final def isRealClass(using Context): Boolean = isClass && !is(Trait)

    /** Cast to class denotation */
    final def asClass: ClassDenotation = asInstanceOf[ClassDenotation]

    /** is this symbol the result of an erroneous definition? */
    def isError: Boolean = false

    /** Make denotation not exist.
     *  @pre `isCompleting` is false, or this is a ModuleCompleter or SymbolLoader
     */
    final def markAbsent()(using Context): Unit = {
      if (isCompleting)
        assert(myInfo.isInstanceOf[ModuleCompleter | SymbolLoader],
          s"Illegal call to `markAbsent()` while completing $this using completer $myInfo")
      myInfo = NoType
    }

    /** Is symbol known to not exist?
     *  @param canForce  If this is true, the info may be forced to avoid a false-negative result
     */
    @tailrec
    final def isAbsent(canForce: Boolean = true)(using Context): Boolean = myInfo match {
      case myInfo: ModuleCompleter =>
        // Instead of completing the ModuleCompleter, we can check whether
        // the module class is absent, which might require less completions.
        myInfo.moduleClass.isAbsent(canForce)
      case _: SymbolLoader if canForce =>
         // Completing a SymbolLoader might call `markAbsent()`
        completeOnce()
        isAbsent(canForce)
      case _ =>
        // Otherwise, no completion is necessary, see the preconditions of `markAbsent()`.
        (myInfo `eq` NoType)
        || is(Invisible) && ctx.isTyper
        || is(ModuleVal, butNot = Package) && moduleClass.isAbsent(canForce)
    }

    /** Is this symbol the root class or its companion object? */
    final def isRoot: Boolean =
      (maybeOwner eq NoSymbol) && (name.toTermName == nme.ROOT || name == nme.ROOTPKG)

    /** Is this symbol the empty package class or its companion object? */
    final def isEmptyPackage(using Context): Boolean =
      name.toTermName == nme.EMPTY_PACKAGE && owner.isRoot

    /** Is this symbol the empty package class or its companion object? */
    final def isEffectiveRoot(using Context): Boolean = isRoot || isEmptyPackage

    /** Is this symbol an anonymous class? */
    final def isAnonymousClass(using Context): Boolean =
      isClass && initial.name.isAnonymousClassName

    final def isAnonymousFunction(using Context): Boolean =
      this.symbol.is(Method) && initial.name.isAnonymousFunctionName

    final def isAnonymousModuleVal(using Context): Boolean =
      this.symbol.is(ModuleVal) && initial.name.isAnonymousClassName

    /** Is this a synthetic method that represents conversions between representations of a value class
      *  These methods are generated in ExtensionMethods
      *  and used in ElimErasedValueType.
      */
    final def isValueClassConvertMethod(using Context): Boolean =
      name.toTermName == nme.U2EVT ||
      name.toTermName == nme.EVT2U

    /** Is symbol a primitive value class? */
    def isPrimitiveValueClass(using Context): Boolean =
      maybeOwner == defn.ScalaPackageClass && defn.ScalaValueClasses().contains(symbol)

    /** Is symbol a primitive numeric value class? */
    def isNumericValueClass(using Context): Boolean =
      maybeOwner == defn.ScalaPackageClass && defn.ScalaNumericValueClasses().contains(symbol)

    /** Is symbol a class for which no runtime representation exists? */
    def isNotRuntimeClass(using Context): Boolean = defn.NotRuntimeClasses contains symbol

    /** Is this symbol a class representing a refinement? These classes
     *  are used only temporarily in Typer and Unpickler as an intermediate
     *  step for creating Refinement types.
     */
    final def isRefinementClass(using Context): Boolean =
      name == tpnme.REFINE_CLASS

    /** Is this symbol a package object or its module class? */
    def isPackageObject(using Context): Boolean =
      name.isPackageObjectName && owner.is(Package) && this.is(Module)

    /** Is this symbol a package object containing top-level definitions? */
    def isTopLevelDefinitionsObject(using Context): Boolean =
      name.isTopLevelPackageObjectName && owner.is(Package) && this.is(Module)

    /** Is this symbol a toplevel definition in a package object? */
    def isWrappedToplevelDef(using Context): Boolean =
      !isConstructor && owner.isPackageObject

    /** Is this symbol an abstract type? */
    final def isAbstractType(using Context): Boolean = this.is(DeferredType)

    /** Is this symbol an alias type? */
    final def isAliasType(using Context): Boolean = isAbstractOrAliasType && !this.is(Deferred)

    /** Is this symbol an abstract or alias type? */
    final def isAbstractOrAliasType: Boolean = isType & !isClass

    /** Is this symbol an abstract type or type parameter? */
    final def isAbstractOrParamType(using Context): Boolean = this.isOneOf(DeferredOrTypeParam)

    /** Is this symbol a user-defined opaque alias type? */
    def isOpaqueAlias(using Context): Boolean = is(Opaque) && !isClass

    /** Is this symbol a module that contains opaque aliases? */
    def containsOpaques(using Context): Boolean = is(Opaque) && isClass

    def seesOpaques(using Context): Boolean =
      containsOpaques ||
      is(Module, butNot = Package) && owner.seesOpaques

    def isProvisional(using Context): Boolean =
      flagsUNSAFE.is(Provisional) // do not force the info to check the flag

    /** Is this the denotation of a self symbol of some class?
     *  This is the case if one of two conditions holds:
     *  1. It is the symbol referred to in the selfInfo part of the ClassInfo
     *     which is the type of this symbol's owner.
     *  2. This symbol is owned by a class, it's selfInfo field refers to a type
     *     (indicating the self definition does not introduce a name), and the
     *     symbol's name is "_".
     *  TODO: Find a more robust way to characterize self symbols, maybe by
     *       spending a Flag on them?
     */
    final def isSelfSym(using Context): Boolean = owner.infoOrCompleter match {
      case ClassInfo(_, _, _, _, selfInfo) =>
        selfInfo == symbol ||
          selfInfo.isInstanceOf[Type] && name == nme.WILDCARD
      case _ => false
    }

    /** Is this definition contained in `boundary`?
     *  Same as `ownersIterator contains boundary` but more efficient.
     */
    final def isContainedIn(boundary: Symbol)(using Context): Boolean = {
      def recur(sym: Symbol): Boolean =
        if (sym eq boundary) true
        else if (sym eq NoSymbol) false
        else if (sym.is(PackageClass) && !boundary.is(PackageClass)) false
        else recur(sym.owner)
      recur(symbol)
    }

    final def isProperlyContainedIn(boundary: Symbol)(using Context): Boolean =
      symbol != boundary && isContainedIn(boundary)

    /** Is this denotation static (i.e. with no outer instance)? */
    final def isStatic(using Context): Boolean =
      (if (maybeOwner eq NoSymbol) isRoot else maybeOwner.originDenotation.isStaticOwner) ||
        myFlags.is(JavaStatic)

    /** Is this a package class or module class that defines static symbols? */
    final def isStaticOwner(using Context): Boolean =
      myFlags.is(ModuleClass) && (myFlags.is(PackageClass) || isStatic)

    /** Is this denotation defined in the same scope and compilation unit as that symbol? */
    final def isCoDefinedWith(other: Symbol)(using Context): Boolean =
      (this.effectiveOwner == other.effectiveOwner) &&
      (  !this.effectiveOwner.is(PackageClass)
        || this.isAbsent(canForce = false) || other.isAbsent(canForce = false)
        || { // check if they are defined in the same file(or a jar)
           val thisFile = this.symbol.associatedFile
           val thatFile = other.associatedFile
           (  thisFile == null
           || thatFile == null
           || thisFile.path == thatFile.path // Cheap possibly wrong check, then expensive normalization
           || thisFile.canonicalPath == thatFile.canonicalPath
           )
         }
      )

    /** Do this symbol and `cls` represent a pair of a given or implicit method and
     *  its associated class that were defined by a single definition?
     *  This can mean one of two things:
     *   - the method and class are defined in a structural given instance, or
     *   - the class is an implicit class and the method is its implicit conversion.
	 */
    final def isCoDefinedGiven(cls: Symbol)(using Context): Boolean =
      is(Method) && isOneOf(GivenOrImplicit)
      && ( is(Synthetic)                 // previous scheme used in 3.0
         || cls.isOneOf(GivenOrImplicit) // new scheme from 3.1
         )
      && name == cls.name.toTermName && owner == cls.owner

    /** Is this a denotation of a stable term (or an arbitrary type)?
      * Terms are stable if they are idempotent (as in TreeInfo.Idempotent): that is, they always return the same value,
      * if any.
      *
      * A *member* is stable, basically, if it behaves like a field projection: that is, it projects a constant result
      * out of its owner.
      *
      * However, a stable member might not yet be initialized (if it is an object or anyhow lazy).
      * So the first call to a stable member might fail and/or produce side effects.
      */
    final def isStableMember(using Context): Boolean = {
      def isUnstableValue = isOneOf(UnstableValueFlags) || info.isInstanceOf[ExprType] || isAllOf(InlineParam)
      isType || is(StableRealizable) || exists && !isUnstableValue
    }

    /** Is this a denotation of a real class that does not have - either direct or inherited -
     *  initialization code?
     */
    def isNoInitsRealClass(using Context): Boolean =
      isRealClass &&
      (asClass.baseClasses.forall(_.is(NoInits)) || defn.isAssuredNoInits(symbol))

    /** Is this a "real" method? A real method is a method which is:
     *  - not an accessor
     *  - not an anonymous function
     */
    final def isRealMethod(using Context): Boolean =
      this.is(Method, butNot = Accessor) && !isAnonymousFunction

    /** Is this a getter? */
    final def isGetter(using Context): Boolean =
      this.is(Accessor) && !originalName.isSetterName && !(originalName.isScala2LocalSuffix && symbol.owner.is(Scala2x))

    /** Is this a setter? */
    final def isSetter(using Context): Boolean =
      this.is(Accessor) &&
      originalName.isSetterName &&
      (!isCompleted || info.firstParamTypes.nonEmpty) // to avoid being fooled by   var x_= : Unit = ...

    /** Is this a symbol representing an import? */
    final def isImport: Boolean = name == nme.IMPORT

    /** Is this the constructor of a class? */
    final def isClassConstructor: Boolean = name == nme.CONSTRUCTOR

    /** Is this the constructor of a trait or a class */
    final def isConstructor: Boolean = name.isConstructorName

    /** Is this a local template dummmy? */
    final def isLocalDummy: Boolean = name.isLocalDummyName

    /** Does this symbol denote the primary constructor of its enclosing class? */
    final def isPrimaryConstructor(using Context): Boolean =
      isConstructor && owner.primaryConstructor == symbol

    /** Does this symbol denote the static constructor of its enclosing class? */
    final def isStaticConstructor(using Context): Boolean =
      name.isStaticConstructorName

    /** Is this a subclass of the given class `base`? */
    def isSubClass(base: Symbol)(using Context): Boolean = false

    /** Is this a subclass of `base`,
     *  and is the denoting symbol also different from `Null` or `Nothing`?
     *  @note  erroneous classes are assumed to derive from all other classes
     *         and all classes derive from them.
     */
    def derivesFrom(base: Symbol)(using Context): Boolean = false

    /** Is this a Scala or Java annotation ? */
    def isAnnotation(using Context): Boolean =
      isClass && (derivesFrom(defn.AnnotationClass) || is(JavaAnnotation))

    /** Is this symbol a class that extends `java.io.Serializable` ? */
    def isSerializable(using Context): Boolean =
      isClass && derivesFrom(defn.JavaSerializableClass)

    /** Is this symbol a class that extends `AnyVal`? Overridden in ClassDenotation */
    def isValueClass(using Context): Boolean = false

    /** Is this symbol a class of which `null` is a value? */
    final def isNullableClass(using Context): Boolean =
      if ctx.mode.is(Mode.SafeNulls) && !ctx.phase.erasedTypes
      then symbol == defn.NullClass || symbol == defn.AnyClass || symbol == defn.MatchableClass
      else isNullableClassAfterErasure

    /** Is this symbol a class of which `null` is a value after erasure?
     *  For example, if `-Yexplicit-nulls` is set, `String` is not nullable before erasure,
     *  but it becomes nullable after erasure.
     */
    final def isNullableClassAfterErasure(using Context): Boolean =
      isClass && !isValueClass && !is(ModuleClass) && symbol != defn.NothingClass

    /** Is `pre` the same as C.this, where C is exactly the owner of this symbol,
     *  or, if this symbol is protected, a subclass of the owner?
     */
    def isAccessPrivilegedThisType(pre: Type)(using Context): Boolean = pre match
      case pre: ThisType =>
        (pre.cls eq owner) || this.is(Protected) && pre.cls.derivesFrom(owner)
      case pre: TermRef =>
        pre.symbol.moduleClass == owner
      case _ =>
        false

    /** Is this definition accessible as a member of tree with type `pre`?
     *  @param pre          The type of the tree from which the selection is made
     *  @param superAccess  Access is via super
     *  Everything is accessible if `pre` is `NoPrefix`.
     *  A symbol with type `NoType` is not accessible for any other prefix.
     *
     *  As a side effect, drop Local flags of members that are not accessed via the ThisType
     *  of their owner.
     */
    final def isAccessibleFrom(pre: Type, superAccess: Boolean = false)(using Context): Boolean = {

      /** Are we inside definition of `boundary`?
       *  If this symbol is Java defined, package structure is interpreted to be flat.
       */
      def accessWithin(boundary: Symbol) =
        ctx.owner.isContainedIn(boundary)
        && !(is(JavaDefined) && boundary.is(PackageClass) && ctx.owner.enclosingPackageClass != boundary)

      /** Are we within definition of linked class of `boundary`? */
      def accessWithinLinked(boundary: Symbol) = {
        val linked = boundary.linkedClass
        (linked ne NoSymbol) && accessWithin(linked)
      }

      /** Is protected access to target symbol permitted? */
      def isProtectedAccessOK: Boolean =
        val cls = owner.enclosingSubClass
        if !cls.exists then
          pre.termSymbol.isPackageObject && accessWithin(pre.termSymbol.owner)
        else
          // allow accesses to types from arbitrary subclasses fixes #4737
          // don't perform this check for static members
          isType || pre.derivesFrom(cls) || isConstructor || owner.is(ModuleClass)
      end isProtectedAccessOK

      if pre eq NoPrefix then true
      else if isAbsent() then false
      else {
        val boundary = accessBoundary(owner)

        (  boundary.isTerm
        || boundary.isRoot
        || (accessWithin(boundary) || accessWithinLinked(boundary)) &&
             (  !this.is(Local)
             || isAccessPrivilegedThisType(pre)
             || canBeLocal(name, flags)
                && {
                  resetFlag(Local)
                  true
                }
             )
        || this.is(Protected) &&
             (  superAccess
             || pre.isInstanceOf[ThisType]
             || ctx.phase.erasedTypes
             || isProtectedAccessOK
             )
        )
      }
    }

    /** Do members of this symbol need translation via asSeenFrom when
     *  accessed via prefix `pre`?
     */
    def membersNeedAsSeenFrom(pre: Type)(using Context): Boolean =
      def preIsThis = pre match
        case pre: ThisType => pre.sameThis(thisType)
        case _ => false
      !(  this.isTerm
       || this.isStaticOwner && !this.seesOpaques
       || ctx.erasedTypes
       || (pre eq NoPrefix)
       || preIsThis
       )

    /** Is this symbol concrete, or that symbol deferred? */
    def isAsConcrete(that: Symbol)(using Context): Boolean =
      !this.is(Deferred) || that.is(Deferred)

    /** Does this symbol have defined or inherited default parameters?
     *  Default parameters are recognized until erasure.
     */
    def hasDefaultParams(using Context): Boolean =
      if ctx.erasedTypes then false
      else if is(HasDefaultParams) then true
      else if is(NoDefaultParams) || !is(Method) then false
      else
        val result =
          rawParamss.nestedExists(_.is(HasDefault))
          || allOverriddenSymbols.exists(_.hasDefaultParams)
        setFlag(if result then HasDefaultParams else NoDefaultParams)
        result

    /** Symbol is an owner that would be skipped by effectiveOwner. Skipped are
     *   - package objects
     *   - non-lazy valdefs
     */
    def isWeakOwner(using Context): Boolean =
      isPackageObject ||
      isTerm && !isOneOf(MethodOrLazy) && !isLocalDummy

    def isSkolem: Boolean = name == nme.SKOLEM

    // Java language spec: https://docs.oracle.com/javase/specs/jls/se11/html/jls-15.html#jls-15.12.3
    // Scala 2 spec: https://scala-lang.org/files/archive/spec/2.13/06-expressions.html#signature-polymorphic-methods
    def isSignaturePolymorphic(using Context): Boolean =
      containsSignaturePolymorphic
      && is(JavaDefined)
      && hasAnnotation(defn.NativeAnnot)
      && atPhase(typerPhase)(symbol.denot).paramSymss.match
        case List(List(p)) => p.info.isRepeatedParam
        case _             => false

    def containsSignaturePolymorphic(using Context): Boolean =
      maybeOwner == defn.MethodHandleClass
      || maybeOwner == defn.VarHandleClass

    def originalSignaturePolymorphic(using Context): Denotation =
      if containsSignaturePolymorphic && !isSignaturePolymorphic then
        val d = owner.info.member(name)
        if d.symbol.isSignaturePolymorphic then d else NoDenotation
      else NoDenotation

    def isInlineMethod(using Context): Boolean =
      isAllOf(InlineMethod, butNot = Accessor)

    /** Does this method or field need to be retained at runtime */
    def isRetainedInline(using Context): Boolean =
      is(Inline, butNot = Deferred)
      && allOverriddenSymbols.exists(!_.is(Inline))

    /** Does this method need to be retained at runtime */
    def isRetainedInlineMethod(using Context): Boolean =
      is(Method, butNot = Accessor) && isRetainedInline

    /** Is this a Scala 2 macro */
    final def isScala2Macro(using Context): Boolean =
      isScala2MacroInScala3 || (is(Macro) && symbol.owner.is(Scala2x))

    /** Is this a Scala 2 macro defined */
    final def isScala2MacroInScala3(using Context): Boolean =
      is(Macro, butNot = Inline) && flagsUNSAFE.is(Erased) // flag is set initially for macros - we check if it's a scala 2 macro before completing the type constructor so do not force the info to check the flag
      // Consider the macros of StringContext as plain Scala 2 macros when
      // compiling the standard library with Dotty.
      // This should be removed on Scala 3.x
      && owner.ne(defn.StringContextClass)

    /** An erased value or an erased inline method or field */
    def isEffectivelyErased(using Context): Boolean =
      isOneOf(EffectivelyErased)
      || is(Inline) && !isRetainedInline && !hasAnnotation(defn.ScalaStaticAnnot)

    /** Is this a member that will become public in the generated binary */
    def hasPublicInBinary(using Context): Boolean =
      isTerm && hasAnnotation(defn.PublicInBinaryAnnot)

    /** ()T and => T types should be treated as equivalent for this symbol.
     *  Note: For the moment, we treat Scala-2 compiled symbols as loose matching,
     *  because the Scala library does not always follow the right conventions.
     *  Examples are: isWhole(), toInt(), toDouble() in BigDecimal, Numeric, RichInt, ScalaNumberProxy.
     */
    def matchNullaryLoosely(using Context): Boolean = {
      def test(sym: Symbol) =
        sym.is(JavaDefined) ||
        sym.owner == defn.AnyClass ||
        sym == defn.Object_clone ||
        sym.owner.is(Scala2x)
      this.exists && (test(symbol) || allOverriddenSymbols.exists(test))
    }

    // ------ access to related symbols ---------------------------------

    /* Modules and module classes are represented as follows:
     *
     * object X extends Y { def f() }
     *
     * <module> lazy val X: X$ = new X$
     * <module> class X$ extends Y { this: X.type => def f() }
     *
     * During completion, references to moduleClass and sourceModules are stored in
     * the completers.
     */
    /** If this a module, return the corresponding class, if this is a module, return itself,
     *  otherwise NoSymbol
     */
    final def moduleClass(using Context): Symbol = {
      def notFound = {
      	if (Config.showCompletions) println(s"missing module class for $name: $myInfo")
      	NoSymbol
      }
      if (this.is(ModuleVal))
        myInfo match {
          case info: TypeRef           => info.symbol
          case ExprType(info: TypeRef) => info.symbol // needed after uncurry, when module terms might be accessor defs
          case info: LazyType          => info.moduleClass
          case t: MethodType           =>
            t.resultType match {
              case info: TypeRef => info.symbol
              case _ => notFound
            }
          case _ => notFound
        }
      else if (this.is(ModuleClass))
        symbol
      else
        NoSymbol
    }

    /** If this a module class, return the corresponding module, if this is a module, return itself,
     *  otherwise NoSymbol
     */
    final def sourceModule(using Context): Symbol =
      if (this.is(ModuleClass))
        myInfo match {
          case ClassInfo(_, _, _, _, selfType) =>
            def sourceOfSelf(tp: TypeOrSymbol): Symbol = (tp: @unchecked) match {
              case tp: TermRef => tp.symbol
              case tp: Symbol => sourceOfSelf(tp.info)
              case tp: RefinedType => sourceOfSelf(tp.parent)
              case tp: AnnotatedType => sourceOfSelf(tp.parent)
              case tp: ThisType => tp.cls
            }
            sourceOfSelf(selfType)
          case info: LazyType =>
            info.sourceModule
          case _ =>
            NoSymbol
        }
      else if (this.is(ModuleVal))
        symbol
      else
        NoSymbol

    /** The field accessed by this getter or setter, or if it does not exist, the getter */
    def accessedFieldOrGetter(using Context): Symbol = {
      val fieldName = if (isSetter) name.asTermName.getterName else name
      val d = owner.info.decl(fieldName)
      val field = d.suchThat(!_.is(Method)).symbol
      def getter = d.suchThat(_.info.isParameterless).symbol
      field orElse getter
    }

    /** The field accessed by a getter or setter, or
     *  if it does not exists, the getter of a setter, or
     *  if that does not exist the symbol itself.
     */
    def underlyingSymbol(using Context): Symbol =
      if (is(Accessor)) accessedFieldOrGetter orElse symbol else symbol

    /** The chain of owners of this denotation, starting with the denoting symbol itself */
    final def ownersIterator(using Context): Iterator[Symbol] = new Iterator[Symbol] {
      private var current = symbol
      def hasNext = current.exists
      def next: Symbol = {
        val result = current
        current = current.owner
        result
      }
    }

    /** If this is a weak owner, its owner, otherwise the denoting symbol. */
    final def skipWeakOwner(using Context): Symbol =
      if (isWeakOwner) owner.skipWeakOwner else symbol

    /** The owner, skipping package objects and non-lazy valdefs. */
    final def effectiveOwner(using Context): Symbol = owner.skipWeakOwner

    /** The class containing this denotation.
     *  If this denotation is already a class, return itself
     *  Definitions flagged with JavaStatic are treated specially.
     *  Their enclosing class is not the lexically enclosing class,
     *  but in turn the enclosing class of the latter. This reflects
     *  the context created by `Context#superCallContext`, `Context#thisCallArgContext`
     *  for these definitions.
     *
     *  Note, that as packages have ClassSymbols, top level classes will have an `enclosingClass`
     *  with Package flag set.
     */
    final def enclosingClass(using Context): Symbol = {
      def enclClass(sym: Symbol, skip: Boolean): Symbol = {
        def newSkip = sym.is(JavaStaticTerm)
        if (!sym.exists)
          NoSymbol
        else if (sym.isClass)
          if (skip) enclClass(sym.owner, newSkip) else sym
        else
          enclClass(sym.owner, skip || newSkip)
      }
      enclClass(symbol, false)
    }

    /** Skips symbol that are not owned by a class */
    def skipLocalOwners(using Context): Symbol =
      if symbol.owner.isClass then symbol
      else symbol.owner.skipLocalOwners

    /** A class that in source code would be lexically enclosing */
    final def lexicallyEnclosingClass(using Context): Symbol =
      if (!exists || isClass) symbol else owner.lexicallyEnclosingClass

    /** A class is extensible if it is not final, nor a module class,
     *  nor an anonymous class.
     */
    final def isExtensibleClass(using Context): Boolean =
      isClass && !isOneOf(FinalOrModuleClass) && !isAnonymousClass

    /** A symbol is effectively final if it cannot be overridden in a subclass */
    final def isEffectivelyFinal(using Context): Boolean =
      isOneOf(EffectivelyFinalFlags)
      || is(Inline, butNot = Deferred)
      || is(JavaDefinedVal, butNot = Method)
      || isConstructor
      || !owner.isExtensibleClass

    /** A class is effectively sealed if has the `final` or `sealed` modifier, or it
     *  is defined in Scala 3 and is neither abstract nor open.
     */
    final def isEffectivelySealed(using Context): Boolean =
      isOneOf(FinalOrSealed)
      || isClass && (!isOneOf(EffectivelyOpenFlags)
      || isLocalToCompilationUnit)

    final def isLocalToCompilationUnit(using Context): Boolean =
      is(Private)
      || owner.ownersIterator.takeWhile(!_.isStaticOwner).exists(_.isTerm)
      || accessBoundary(defn.RootClass).isProperlyContainedIn(symbol.topLevelClass)

    final def isTransparentClass(using Context): Boolean =
      is(TransparentType)
      || defn.isAssumedTransparent(symbol)
      || isClass && hasAnnotation(defn.TransparentTraitAnnot)

    /** The class containing this denotation which has the given effective name. */
    final def enclosingClassNamed(name: Name)(using Context): Symbol = {
      val cls = enclosingClass
      if (cls.effectiveName == name || !cls.exists) cls else cls.owner.enclosingClassNamed(name)
    }

    /** The closest enclosing method containing this definition.
     *  A local dummy owner is mapped to the primary constructor of the class.
     */
    final def enclosingMethod(using Context): Symbol =
      if (this.is(Method)) symbol
      else if (this.isClass) primaryConstructor
      else if (this.exists) owner.enclosingMethod
      else NoSymbol

    /** The closest enclosing extension method containing this definition,
     *  including methods outside the current class.
     */
    final def enclosingExtensionMethod(using Context): Symbol =
      if this.is(ExtensionMethod) then symbol
      else if this.exists then owner.enclosingExtensionMethod
      else NoSymbol

    /** The top-level class containing this denotation,
     *  except for a toplevel module, where its module class is returned.
     */
    final def topLevelClass(using Context): Symbol = {
      @tailrec def topLevel(d: SymDenotation): Symbol =
        if (d.isTopLevelClass) d.symbol
        else topLevel(d.owner)

      val sym = topLevel(this)
      if (sym.isClass) sym else sym.moduleClass
    }

    final def isTopLevelClass(using Context): Boolean =
      !this.exists || this.isEffectiveRoot || this.is(PackageClass) || this.owner.is(PackageClass)

    /** The package class containing this denotation */
    final def enclosingPackageClass(using Context): Symbol =
      if (this.is(PackageClass)) symbol else owner.enclosingPackageClass

    /** Register target as a companion; overridden in ClassDenotation */
    def registerCompanion(target: Symbol)(using Context) = ()

    /** The registered companion; overridden in ClassDenotation */
    def registeredCompanion(using Context): Symbol = NoSymbol
    def registeredCompanion_=(c: Symbol): Unit = ()

    /** The module object with the same (term-) name as this class or module class,
     *  and which is also defined in the same scope and compilation unit.
     *  NoSymbol if this module does not exist.
     */
    final def companionModule(using Context): Symbol =
      if (is(Module)) sourceModule
      else if registeredCompanion.isAbsent() then NoSymbol
      else registeredCompanion.sourceModule

    private def companionType(using Context): Symbol =
      if (is(Package)) NoSymbol
      else if (is(ModuleVal)) moduleClass.denot.companionType
      else if registeredCompanion.isAbsent() then NoSymbol
      else registeredCompanion

    /** The class with the same (type-) name as this module or module class,
     *  and which is also defined in the same scope and compilation unit.
     *  NoSymbol if this class does not exist.
     */
    final def companionClass(using Context): Symbol =
      companionType.suchThat(_.isClass).symbol

    final def scalacLinkedClass(using Context): Symbol =
      if (this.is(ModuleClass)) companionNamed(effectiveName.toTypeName)
      else if (this.isClass) companionNamed(effectiveName.moduleClassName)
      else NoSymbol

    /** Find companion class symbol with given name, or NoSymbol if none exists.
     *  Three alternative strategies:
     *  1. If owner is a class, look in its members, otherwise
     *  2. If current compilation unit has a typed tree,
     *     determine the defining statement sequence and search its trees, otherwise
     *  3. If context has an enclosing scope which defines this symbol,
     *     lookup its companion in the same scope.
     */
    private def companionNamed(name: TypeName)(using Context): Symbol =
      val unit = ctx.compilationUnit
      if (owner.isClass)
        owner.unforcedDecls.lookup(name).suchThat(_.isCoDefinedWith(symbol)).symbol
      else if (!owner.exists || (unit eq NoCompilationUnit))
        NoSymbol
      else if (!unit.tpdTree.isEmpty)
        tpd.definingStats(symbol).iterator
          .map(tpd.definedSym)
          .find(_.name == name)
          .getOrElse(NoSymbol)
      else if (ctx.scope eq EmptyScope)
        NoSymbol
      else if (ctx.scope.lookup(this.name) == symbol)
        ctx.scope.lookup(name)
      else
        companionNamed(name)(using ctx.outersIterator.dropWhile(_.scope eq ctx.scope).next())

    /** Is this symbol the same or a linked class of `sym`? */
    final def isLinkedWith(sym: Symbol)(using Context): Boolean =
      (symbol eq sym) || (linkedClass eq sym)

    /** If this is a class, the module class of its companion object.
     *  If this is a module class, its companion class.
     *  NoSymbol otherwise.
     */
    final def linkedClass(using Context): Symbol =
      if (this.is(ModuleClass)) companionClass
      else if (this.isClass) companionModule.moduleClass
      else NoSymbol

    /** The class that encloses the owner of the current context
     *  and that is a subclass of this class. NoSymbol if no such class exists.
     */
    final def enclosingSubClass(using Context): Symbol =
      ctx.owner.ownersIterator.findSymbol(_.isSubClass(symbol))

    /** The alias of an opaque type alias that's stored in the self type of the
     *  containing object.
     */
    def opaqueAlias(using Context): Type = {
      def recur(tp: Type): Type = tp match {
        case RefinedType(parent, rname, TypeAlias(alias)) =>
          if rname == name then alias.stripLazyRef else recur(parent)
        case _ =>
          NoType
      }
      recur(owner.asClass.givenSelfType)
    }

    /** The non-private symbol whose name and type matches the type of this symbol
     *  in the given class.
     *  @param inClass   The class containing the result symbol's definition
     *  @param site      The base type from which member types are computed
     *
     *  inClass <-- find denot.symbol      class C { <-- symbol is here
     *
     *                   site: Subtype of both inClass and C
     */
    final def matchingDecl(inClass: Symbol, site: Type, name: Name = this.name)(using Context): Symbol = {
      var denot = inClass.info.nonPrivateDecl(name)
      if (denot.isTerm) // types of the same name always match
        denot = denot.matchingDenotation(site, site.memberInfo(symbol), symbol.targetName)
      denot.symbol
    }

    /** The non-private member of `site` whose name and type matches the type of this symbol
     */
    final def matchingMember(site: Type)(using Context): Symbol = {
      var denot = site.nonPrivateMember(name)
      if (denot.isTerm) // types of the same name always match
        denot = denot.matchingDenotation(site, site.memberInfo(symbol), symbol.targetName)
      denot.symbol
    }

    /** If false, this symbol cannot possibly participate in an override,
     *  either as overrider or overridee.
     */
    final def canMatchInheritedSymbols(using Context): Boolean =
      maybeOwner.isClass && memberCanMatchInheritedSymbols

    /** If false, this class member cannot possibly participate in an override,
     *  either as overrider or overridee.
     */
    final def memberCanMatchInheritedSymbols(using Context): Boolean =
      !isConstructor && !is(Private)

    /** The symbol, in class `inClass`, that is overridden by this denotation in class `siteClass`.*/
    final def overriddenSymbol(inClass: ClassSymbol, siteClass: ClassSymbol = owner.asClass)(using Context): Symbol =
      if (!canMatchInheritedSymbols && (owner ne inClass)) NoSymbol
      else matchingDecl(inClass, siteClass.thisType)

    /** All symbols overridden by this denotation. */
    final def allOverriddenSymbols(using Context): Iterator[Symbol] =
      if (!canMatchInheritedSymbols) Iterator.empty
      else overriddenFromType(owner.info)

    /** Equivalent to `allOverriddenSymbols.headOption.getOrElse(NoSymbol)` but more efficient. */
    final def nextOverriddenSymbol(using Context): Symbol = {
      val overridden = allOverriddenSymbols
      if (overridden.hasNext)
        overridden.next
      else
        NoSymbol
    }

    /** Returns all matching symbols defined in parents of the selftype. */
    final def extendedOverriddenSymbols(using Context): Iterator[Symbol] =
      if (!canMatchInheritedSymbols) Iterator.empty
      else overriddenFromType(owner.asClass.classInfo.selfType)

    private def overriddenFromType(tp: Type)(using Context): Iterator[Symbol] =
      tp.baseClasses match {
        case _ :: inherited => inherited.iterator.map(overriddenSymbol(_)).filter(_.exists)
        case Nil => Iterator.empty
      }

    /** The symbol overriding this symbol in given subclass `inClass`.
     *
     *  @pre `inClass` is a subclass of this symbol's owner
     */
    final def overridingSymbol(inClass: ClassSymbol)(using Context): Symbol =
      if (canMatchInheritedSymbols) matchingDecl(inClass, inClass.thisType)
      else NoSymbol

    /** The symbol accessed by a super in the definition of this symbol when
     *  seen from class `base`. This symbol is always concrete.
     *  pre: `this.owner` is in the base class sequence of `base`.
     */
    final def superSymbolIn(base: Symbol)(using Context): Symbol = {
      @tailrec def loop(bcs: List[ClassSymbol]): Symbol = bcs match {
        case bc :: bcs1 =>
          val sym = matchingDecl(bcs.head, base.thisType)
            .suchThat(alt => !alt.is(Deferred)).symbol
          if (sym.exists) sym else loop(bcs.tail)
        case _ =>
          NoSymbol
      }
      loop(base.info.baseClasses.dropWhile(owner != _).tail)
    }

    /** A member of class `base` is incomplete if
     *  (1) it is declared deferred or
     *  (2) it is abstract override and its super symbol in `base` is
     *      nonexistent or incomplete.
     */
    @tailrec final def isIncompleteIn(base: Symbol)(using Context): Boolean =
      this.is(Deferred) ||
      this.is(AbsOverride) && {
        val supersym = superSymbolIn(base)
        supersym == NoSymbol || supersym.isIncompleteIn(base)
      }

    /** The class or term symbol up to which this symbol is accessible,
     *  or RootClass if it is public.  As java protected statics are
     *  otherwise completely inaccessible in scala, they are treated
     *  as public.
     *  @param base  The access boundary to assume if this symbol is protected
     */
    final def accessBoundary(base: Symbol)(using Context): Symbol =
      if (this.is(Private)) owner
      else if (this.isAllOf(StaticProtected)) defn.RootClass
      else if (privateWithin.exists && (!ctx.phase.erasedTypes || this.is(JavaDefined))) privateWithin
      else if (this.is(Protected)) base
      else defn.RootClass

    final def isPublic(using Context): Boolean =
      accessBoundary(owner) == defn.RootClass

    /** The primary constructor of a class or trait, NoSymbol if not applicable. */
    def primaryConstructor(using Context): Symbol = NoSymbol

    /** The current declaration in this symbol's class owner that has the same name
     *  as this one, and, if there are several, also has the same signature.
     */
    def currentSymbol(using Context): Symbol = {
      val candidates = owner.info.decls.lookupAll(name)
      def test(sym: Symbol): Symbol =
        if (sym == symbol || sym.signature == signature) sym
        else if (candidates.hasNext) test(candidates.next)
        else NoSymbol
      if (candidates.hasNext) {
        val sym = candidates.next
        if (candidates.hasNext) test(sym) else sym
      }
      else NoSymbol
    }

    // ----- type-related ------------------------------------------------

    /** The type parameters of a class symbol, Nil for all other symbols */
    def typeParams(using Context): List[TypeSymbol] = Nil

    /** The type This(cls), where cls is this class, NoPrefix for all other symbols */
    def thisType(using Context): Type = NoPrefix

    def typeRef(using Context): TypeRef =
      TypeRef(maybeOwner.thisType, symbol)

    def termRef(using Context): TermRef =
      TermRef(maybeOwner.thisType, symbol)

    /** The typeRef applied to its own type parameters */
    def appliedRef(using Context): Type =
      typeRef.appliedTo(symbol.typeParams.map(_.typeRef))

    /** The NamedType representing this denotation at its original location.
     *  Same as either `typeRef` or `termRef` depending whether this denotes a type or not.
     */
    def namedType(using Context): NamedType =
      if (isType) typeRef else termRef

    /** Like typeRef, but the prefix is widened.
     *
     *  See tests/neg/i19619/Test.scala
     */
    def javaTypeRef(using Context) =
      TypeRef(maybeOwner.reachablePrefix.widen, symbol)

    /** Like typeRef, but objects in the prefix are represented by their singleton type,
     *  this means we output `pre.O.member` rather than `pre.O$.this.member`.
     *
     *  This is required to avoid owner crash in ExplicitOuter.
     *  See tests/pos/i10769.scala
     */
     def reachableTypeRef(using Context) =
       TypeRef(owner.reachablePrefix, symbol)

    /** The reachable typeRef with wildcard arguments for each type parameter */
    def reachableRawTypeRef(using Context) =
      reachableTypeRef.appliedTo(typeParams.map(_ => TypeBounds.emptyPolyKind))

    /** Like termRef, if it is addressable from the current context,
     *  but objects in the prefix are represented by their singleton type,
     *  this means we output `pre.O.member` rather than `pre.O$.this.member`.
     *
     *  This is required to avoid owner crash in ExplicitOuter.
     *  See tests/pos/i10769.scala
     *
     *  If the reference is to an object that is not accessible from the
     *  current context since the object is nested in a class that is not an outer
     *  class of the current context, fall back to a TypeRef to the module class.
     *  Test case is tests/pos/i17556.scala.
     *  If the reference is to some other inaccessible object, throw an AssertionError.
     */
    def reachableTermRef(using Context): Type = owner.reachablePrefix match
      case pre: SingletonType => TermRef(pre, symbol)
      case pre if symbol.is(ModuleVal) => TypeRef(pre, symbol.moduleClass)
      case _ => throw AssertionError(i"cannot compute path to TermRef $this from ${ctx.owner}")

    /** Like thisType, but objects in the type are represented by their singleton type,
     *  this means we output `pre.O.member` rather than `pre.O$.this.member`.
     */
    def reachableThisType(using Context): Type =
      if this.is(Package) then
        symbol.thisType
      else if this.isTerm then
        NoPrefix
      else if this.is(Module) then
        TermRef(owner.reachableThisType, this.sourceModule)
      else
        ThisType.raw(TypeRef(owner.reachableThisType, symbol.asType))

    /** Like `reachableThisType`, except if that would refer to a class where
     *  the `this` cannot be accessed. In that case, fall back to the
     *  rawTypeRef of the class. E.g. instead of `A.this.X` where `A.this`
     *  is inaccessible, use `A#X`.
     */
    def reachablePrefix(using Context): Type = reachableThisType match
      case pre: ThisType
      if !pre.cls.isStaticOwner && !ctx.owner.isContainedIn(pre.cls) =>
        pre.cls.reachableRawTypeRef
      case pre =>
        pre

    /** The variance of this type parameter or type member as a subset of
     *  {Covariant, Contravariant}
     */
    final def variance(using Context): Variance =
      if is(Covariant) then Covariant
      else if is(Contravariant) then Contravariant
      else EmptyFlags

    /** The flags to be used for a type parameter owned by this symbol.
     *  Overridden by ClassDenotation.
     */
    def typeParamCreationFlags: FlagSet = TypeParam

    def kindString: String =
      if myFlags.is(ModuleClass) then "module class"
      else if myFlags.is(Trait)  then "trait"
      else if isClass            then "class"
      else if isType             then "type"
      else if myFlags.is(Module) then "object"
      else if myFlags.is(Method) then "method"
      else                            "val"

    override def toString: String = s"$kindString $name"

    // ----- Sanity checks and debugging */

    def debugString: String = toString + "#" + symbol.id // !!! DEBUG

    def hasSkolems(tp: Type): Boolean = tp match {
      case tp: SkolemType => true
      case tp: NamedType => hasSkolems(tp.prefix)
      case tp: RefinedType => hasSkolems(tp.parent) || hasSkolems(tp.refinedInfo)
      case tp: RecType => hasSkolems(tp.parent)
      case tp: TypeBounds => hasSkolems(tp.lo) || hasSkolems(tp.hi)
      case tp: TypeVar => hasSkolems(tp.inst)
      case tp: ExprType => hasSkolems(tp.resType)
      case tp: AppliedType => hasSkolems(tp.tycon) || tp.args.exists(hasSkolems)
      case tp: LambdaType => tp.paramInfos.exists(hasSkolems) || hasSkolems(tp.resType)
      case tp: AndOrType => hasSkolems(tp.tp1) || hasSkolems(tp.tp2)
      case tp: AnnotatedType => hasSkolems(tp.parent)
      case _ => false
    }

    def assertNoSkolems(tp: Type): Unit =
      if (!this.isSkolem)
        assert(!hasSkolems(tp), s"assigning type $tp containing skolems to $this")

    // ----- copies and transforms  ----------------------------------------

    protected def newLikeThis(s: Symbol, i: Type, pre: Type, isRefinedMethod: Boolean)(using Context): SingleDenotation =
      if isRefinedMethod then
        new JointRefDenotation(s, i, currentStablePeriod, pre, isRefinedMethod)
      else
        new UniqueRefDenotation(s, i, currentStablePeriod, pre)

    /** Copy this denotation, overriding selective fields */
    final def copySymDenotation(
      symbol: Symbol = this.symbol,
      owner: Symbol = this.owner,
      name: Name = this.name,
      initFlags: FlagSet = UndefinedFlags,
      info: Type | Null = null,
      privateWithin: Symbol | Null = null,
      annotations: List[Annotation] | Null = null,
      rawParamss: List[List[Symbol]] | Null = null)(
        using Context): SymDenotation = {
      // simulate default parameters, while also passing implicit context ctx to the default values
      val initFlags1 = (if (initFlags != UndefinedFlags) initFlags else this.flags)
      val info1 = if (info != null) info else this.info
      if (ctx.isAfterTyper && changedClassParents(info, info1, completersMatter = false))
        assert(ctx.phase.changesParents, i"undeclared parent change at ${ctx.phase} for $this, was: $info, now: $info1")
      val privateWithin1 = if (privateWithin != null) privateWithin else this.privateWithin
      val annotations1 = if (annotations != null) annotations else this.annotations
      val rawParamss1 = if rawParamss != null then rawParamss else this.rawParamss
      val d = SymDenotation(symbol, owner, name, initFlags1, info1, privateWithin1)
      d.annotations = annotations1
      d.rawParamss = rawParamss1
      d.registeredCompanion = registeredCompanion
      d
    }

    /** Copy mamberNames and baseData caches from given denotation, provided
     *  they are valid at given `phase`.
     */
    def copyCaches(from: SymDenotation, phase: Phase)(using Context): this.type = this

    /** Are `info1` and `info2` ClassInfo types with different parents?
     *  @param completersMatter  if `true`, consider parents changed if `info1` or `info2 `is a type completer
     */
    protected def changedClassParents(info1: Type | Null, info2: Type | Null, completersMatter: Boolean): Boolean =
      info2 match {
        case info2: ClassInfo =>
          info1 match {
            case info1: ClassInfo => info1.declaredParents ne info2.declaredParents
            case _ => completersMatter
          }
        case _ => completersMatter
      }

    override def initial: SymDenotation = super.initial.asSymDenotation

    /** Install this denotation as the result of the given denotation transformer. */
    override def installAfter(phase: DenotTransformer)(using Context): Unit =
      super.installAfter(phase)

    /** Apply a transformation `f` to all denotations in this group that start at or after
     *  given phase. Denotations are replaced while keeping the same validity periods.
     */
    override def transformAfter(phase: DenotTransformer, f: SymDenotation => SymDenotation)(using Context): Unit =
      super.transformAfter(phase, f)

    /** Set flag `flags` in current phase and in all phases that follow */
    def setFlagFrom(phase: DenotTransformer, flags: FlagSet)(using Context): Unit =
      setFlag(flags)
      transformAfter(phase, sd => { sd.setFlag(flags); sd })

    /** If denotation is private, remove the Private flag and expand the name if necessary */
    def ensureNotPrivate(using Context): SymDenotation =
      if (is(Private))
        copySymDenotation(name = expandedName, initFlags = this.flags &~ Private)
      else this

    /** If this is a sealed class, its known children in the order of textual occurrence
     */
    def children(using Context): List[Symbol] =

      def completeChildrenIn(owner: Symbol)(using Context) =
        // Possible children are: classes extending Scala classes and
        // Scala or Java enum values that are defined in owner.
        // If owner is a package, we complete only
        // children that are defined in the same file as their parents.
        def maybeChild(sym: Symbol) =
          (sym.isClass && !this.is(JavaDefined) || sym.originDenotation.is(EnumVal))
          && (!owner.is(Package)
             || sym.originDenotation.infoOrCompleter.match
                  case _: SymbolLoaders.SecondCompleter => sym.associatedFile == this.symbol.associatedFile
                  case _ => false)

        if owner.isClass then
          for c <- owner.info.decls.toList if maybeChild(c) do
            c.ensureCompleted()
      end completeChildrenIn

      if is(Sealed) || isAllOf(JavaEnum) && isClass then
        if !is(ChildrenQueried) then
          // Make sure all visible children are completed, so that
          // they show up in Child annotations. A possible child is visible if it
          // is defined in the same scope as `cls` or in the companion object of `cls`.
          completeChildrenIn(owner)
          completeChildrenIn(companionClass)
          setFlag(ChildrenQueried)

      annotations.collect { case Annotation.Child(child) => child }.reverse
    end children

    /** Recursively assemble all children of this symbol, Preserves order of insertion.
     */
    final def sealedStrictDescendants(using Context): List[Symbol] =

      @tailrec
      def findLvlN(
        explore: mutable.ArrayDeque[Symbol],
        seen: util.HashSet[Symbol],
        acc: mutable.ListBuffer[Symbol]
      ): List[Symbol] =
        if explore.isEmpty then
          acc.toList
        else
          val sym      = explore.head
          val explore1 = explore.dropInPlace(1)
          val lvlN     = sym.children
          val notSeen  = lvlN.filterConserve(!seen.contains(_))
          if notSeen.isEmpty then
            findLvlN(explore1, seen, acc)
          else
            findLvlN(explore1 ++= notSeen, {seen ++= notSeen; seen}, acc ++= notSeen)
      end findLvlN

      /** Scans through `explore` to see if there are recursive children.
       *  If a symbol in `explore` has children that are not contained in
       *  `lvl1`, fallback to `findLvlN`, or else return `lvl1`.
       */
      @tailrec
      def findLvl2(
        lvl1: List[Symbol], explore: List[Symbol], seenOrNull: util.HashSet[Symbol] | Null
      ): List[Symbol] = explore match
        case sym :: explore1 =>
          val lvl2 = sym.children
          if lvl2.isEmpty then // no children, scan rest of explore1
            findLvl2(lvl1, explore1, seenOrNull)
          else // check if we have seen the children before
            val seen = // initialise the seen set if not already
              if seenOrNull != null then seenOrNull
              else util.HashSet.from(lvl1)
            val notSeen = lvl2.filterConserve(!seen.contains(_))
            if notSeen.isEmpty then // we found children, but we had already seen them, scan the rest of explore1
              findLvl2(lvl1, explore1, seen)
            else // found unseen recursive children, we should fallback to the loop
              findLvlN(
                explore = mutable.ArrayDeque.from(explore1).appendAll(notSeen),
                seen = {seen ++= notSeen; seen},
                acc = mutable.ListBuffer.from(lvl1).appendAll(notSeen)
              )
        case nil =>
          lvl1
      end findLvl2

      val lvl1 = children
      findLvl2(lvl1, lvl1, seenOrNull = null)
    end sealedStrictDescendants

    /** Same as `sealedStrictDescendants` but prepends this symbol as well.
     */
    final def sealedDescendants(using Context): List[Symbol] = this.symbol :: sealedStrictDescendants
  }

  /** The contents of a class definition during a period
   */
  class ClassDenotation private[SymDenotations] (
    symbol: Symbol,
    maybeOwner: Symbol,
    name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol)
    extends SymDenotation(symbol, maybeOwner, name, initFlags, initInfo, initPrivateWithin) {

    import util.EqHashMap

    // ----- caches -------------------------------------------------------

    private var myTypeParams: List[TypeSymbol] | Null = null
    private var fullNameCache: SimpleIdentityMap[QualifiedNameKind, Name] = SimpleIdentityMap.empty

    private var myMemberCache: EqHashMap[Name, PreDenotation] | Null = null
    private var myMemberCachePeriod: Period = Nowhere

    /** A cache from types T to baseType(T, C) */
    type BaseTypeMap = EqHashMap[CachedType, Type]
    private var myBaseTypeCache: BaseTypeMap | Null = null
    private var myBaseTypeCachePeriod: Period = Nowhere

    private var baseDataCache: BaseData = BaseData.None
    private var memberNamesCache: MemberNames = MemberNames.None

    private def memberCache(using Context): EqHashMap[Name, PreDenotation] = {
      if (myMemberCachePeriod != ctx.period) {
        myMemberCache = EqHashMap()
        myMemberCachePeriod = ctx.period
      }
      myMemberCache.nn
    }

    private def baseTypeCache(using Context): BaseTypeMap = {
      if !currentHasSameBaseTypesAs(myBaseTypeCachePeriod) then
        myBaseTypeCache = new BaseTypeMap()
        myBaseTypeCachePeriod = ctx.period
      myBaseTypeCache.nn
    }

    private def invalidateBaseDataCache() = {
      baseDataCache.invalidate()
      baseDataCache = BaseData.None
    }

    private def invalidateMemberNamesCache() = {
      memberNamesCache.invalidate()
      memberNamesCache = MemberNames.None
    }

    def invalidateBaseTypeCache(): Unit = {
      myBaseTypeCache = null
      myBaseTypeCachePeriod = Nowhere
    }

    def invalidateMemberCaches()(using Context): Unit =
      myMemberCachePeriod = Nowhere
      invalidateMemberNamesCache()

    def invalidateMemberCachesFor(sym: Symbol)(using Context): Unit =
      if myMemberCache != null then myMemberCache.uncheckedNN.remove(sym.name)
      if !sym.flagsUNSAFE.is(Private) then
        invalidateMemberNamesCache()
        if sym.isWrappedToplevelDef then
          val outerCache = sym.owner.owner.asClass.classDenot.myMemberCache
          if outerCache != null then outerCache.remove(sym.name)

    override def copyCaches(from: SymDenotation, phase: Phase)(using Context): this.type = {
      from match {
        case from: ClassDenotation =>
          if (from.memberNamesCache.isValidAt(phase)) memberNamesCache = from.memberNamesCache
          if (from.baseDataCache.isValidAt(phase)) {
            baseDataCache = from.baseDataCache
            myBaseTypeCache = from.baseTypeCache
          }
        case _ =>
      }
      this
    }

    // ----- denotation fields and accessors ------------------------------

    if (initFlags.is(Module, butNot = Package))
      assert(name.is(ModuleClassName), s"module naming inconsistency: ${name.debugString}")

    /** The symbol asserted to have type ClassSymbol */
    def classSymbol: ClassSymbol = symbol.asInstanceOf[ClassSymbol]

    /** The info asserted to have type ClassInfo */
    def classInfo(using Context): ClassInfo = info.asInstanceOf[ClassInfo]

    /** The type parameters in this class, in the order they appear in the current
     *  scope `decls`. This might be temporarily the incorrect order when
     *  reading Scala2 pickled info. The problem is fixed by `ensureTypeParamsInCorrectOrder`,
     *  which is called once an unpickled symbol has been completed.
     */
    private def typeParamsFromDecls(using Context) =
      unforcedDecls.filter(sym =>
        sym.is(TypeParam) && sym.owner == symbol).asInstanceOf[List[TypeSymbol]]

    /** The type parameters of this class */
    override final def typeParams(using Context): List[TypeSymbol] = {
      if (myTypeParams == null)
        myTypeParams =
          if (ctx.erasedTypes || is(Module)) Nil // fast return for modules to avoid scanning package decls
          else {
            val di = initial
            if (this ne di) di.typeParams
            else infoOrCompleter match {
              case info: TypeParamsCompleter => info.completerTypeParams(symbol)
              case _ => typeParamsFromDecls
            }
          }
      myTypeParams.nn
    }

    override protected[dotc] final def info_=(tp: Type): Unit = {
      if (changedClassParents(infoOrCompleter, tp, completersMatter = true))
        invalidateBaseDataCache()
      invalidateMemberNamesCache()
      myTypeParams = null // changing the info might change decls, and with it typeParams
      super.info_=(tp)
    }

    /** The types of the parent classes. */
    def parentTypes(using Context): List[Type] = info match
      case classInfo: ClassInfo => classInfo.declaredParents
      case _ => Nil

    /** The symbols of the parent classes. */
    def parentSyms(using Context): List[Symbol] =
      parentTypes.map(_.classSymbol)

    /** The symbol of the superclass, NoSymbol if no superclass exists */
    def superClass(using Context): Symbol = parentTypes match
      case parentType :: _ =>
        val parentCls = parentType.classSymbol
        if parentCls.is(Trait) then NoSymbol else parentCls
      case _ => NoSymbol

    /** The explicitly given self type (self types of modules are assumed to be
     *  explcitly given here).
     */
    def givenSelfType(using Context): Type = classInfo.selfInfo match {
      case tp: Type => tp
      case self: Symbol => self.info
    }

   // ------ class-specific operations -----------------------------------

    private var myThisType: Type | Null = null

    /** The this-type depends on the kind of class:
     *  - for a package class `p`:  ThisType(TypeRef(Noprefix, p))
     *  - for a module class `m`: A term ref to m's source module.
     *  - for all other classes `c` with owner `o`: ThisType(TypeRef(o.thisType, c))
     */
    override def thisType(using Context): Type = {
      if (myThisType == null) myThisType = computeThisType
      myThisType.nn
    }

    private def computeThisType(using Context): Type = {
      val cls = symbol.asType
      val pre = if (this.is(Package)) NoPrefix else owner.thisType
      ThisType.raw(TypeRef(pre, cls))
    }

    private var myTypeRef: TypeRef | Null = null

    override def typeRef(using Context): TypeRef = {
      if (myTypeRef == null) myTypeRef = super.typeRef
      myTypeRef.nn
    }

    override def appliedRef(using Context): Type = classInfo.appliedRef

    private def baseData(implicit onBehalf: BaseData, ctx: Context): (List[ClassSymbol], BaseClassSet) = {
      if (!baseDataCache.isValid) baseDataCache = BaseData.newCache()
      baseDataCache(this)
    }

    /** The base classes of this class in linearization order,
     *  with the class itself as first element.
     */
    def baseClasses(implicit onBehalf: BaseData, ctx: Context): List[ClassSymbol] =
      baseData._1

    /** Like `baseClasses.length` but more efficient. */
    def baseClassesLength(using BaseData, Context): Int =
      // `+ 1` because the baseClassSet does not include the current class unlike baseClasses
      baseClassSet.classIds.length + 1

    /** A bitset that contains the superId's of all base classes */
    private def baseClassSet(implicit onBehalf: BaseData, ctx: Context): BaseClassSet =
      baseData._2

    def computeBaseData(implicit onBehalf: BaseData, ctx: Context): (List[ClassSymbol], BaseClassSet) = {
      def emptyParentsExpected =
        is(Package) || (symbol == defn.AnyClass) || ctx.erasedTypes && (symbol == defn.ObjectClass)
      val parents = parentTypes
      if (parents.isEmpty && !emptyParentsExpected)
        onBehalf.signalProvisional()
      val builder = new BaseDataBuilder
      def traverse(parents: List[Type]): Unit = parents match {
        case p :: parents1 =>
          p.classSymbol match {
            case pcls: ClassSymbol => builder.addAll(pcls.baseClasses)
            case _ => assert(isRefinementClass || p.isError || ctx.mode.is(Mode.Interactive), s"$this has non-class parent: $p")
          }
          traverse(parents1)
        case nil =>
      }
      traverse(parents)
      (classSymbol :: builder.baseClasses, builder.baseClassSet)
    }

    final override def derivesFrom(base: Symbol)(using Context): Boolean =
      !isAbsent() &&
      base.isClass &&
      (  (symbol eq base)
      || (baseClassSet contains base)
      )

    final override def isSubClass(base: Symbol)(using Context): Boolean =
      derivesFrom(base)
      || base.isClass
         && (
          (symbol eq defn.NothingClass)
          || (symbol eq defn.NullClass)
              && (!ctx.mode.is(Mode.SafeNulls) || ctx.phase.erasedTypes)
              && (base ne defn.NothingClass)
        )

    /** Is it possible that a class inherits both `this` and `that`?
     *
     *  @note The test is based on single-class inheritance and the closed
     *        hierarchy of final classes.
     *
     *  @return The result may contain false positives, but never false negatives.
     */
    final def mayHaveCommonChild(that: ClassSymbol)(using Context): Boolean =
         this.is(Trait) && !that.isEffectivelyFinal
      || that.is(Trait) && !this.isEffectivelyFinal
      || this.derivesFrom(that)
      || that.derivesFrom(this.symbol)

    final override def typeParamCreationFlags: FlagSet = ClassTypeParamCreationFlags

    /** Hook to do a pre-enter test. Overridden in PackageDenotation */
    protected def proceedWithEnter(sym: Symbol, mscope: MutableScope)(using Context): Boolean = true

    final override def isValueClass(using Context): Boolean =
      val di = initial.asClass
      val anyVal = defn.AnyValClass
      if di.baseDataCache.isValid && !ctx.erasedTypes then
        // fast path that does not demand time travel
        (symbol eq anyVal) || di.baseClassSet.contains(anyVal)
      else
        // We call derivesFrom at the initial phase both because AnyVal does not exist
        // after Erasure and to avoid cyclic references caused by forcing denotations
        atPhase(di.validFor.firstPhaseId)(di.derivesFrom(anyVal))

    /** Enter a symbol in current scope, and future scopes of same denotation.
     *  Note: We require that this does not happen after the first time
     *  someone does a findMember on a subclass.
     *  @param scope   The scope in which symbol should be entered.
     *                 If this is EmptyScope, the scope is `decls`.
     */
    def enter(sym: Symbol, scope: Scope = EmptyScope)(using Context): Unit = {
      val mscope = scope match {
        case scope: MutableScope => scope
        case _ => unforcedDecls.openForMutations
      }
      if (proceedWithEnter(sym, mscope)) {
        enterNoReplace(sym, mscope)
        val nxt = this.nextInRun
        if (nxt.validFor.code > this.validFor.code)
          this.nextInRun.asSymDenotation.asClass.enter(sym)
      }
    }

    /** Enter a symbol in given `scope` without potentially replacing the old copy. */
    def enterNoReplace(sym: Symbol, scope: MutableScope)(using Context): Unit =
      scope.enter(sym)
      invalidateMemberCachesFor(sym)

    /** Replace symbol `prev` (if defined in current class) by symbol `replacement`.
     *  If `prev` is not defined in current class, do nothing.
     *  @pre `prev` and `replacement` have the same name.
     */
    def replace(prev: Symbol, replacement: Symbol)(using Context): Unit = {
      unforcedDecls.openForMutations.replace(prev, replacement)
      if (myMemberCache != null) myMemberCache.uncheckedNN.remove(replacement.name)
    }

    /** Delete symbol from current scope.
     *  Note: We require that this does not happen after the first time
     *  someone does a findMember on a subclass.
     */
    def delete(sym: Symbol)(using Context): Unit = {
      val scope = info.decls.openForMutations
      scope.unlink(sym, sym.name)
      if sym.name != sym.originalName then scope.unlink(sym, sym.originalName)
      if (myMemberCache != null) myMemberCache.uncheckedNN.remove(sym.name)
      if (!sym.flagsUNSAFE.is(Private)) invalidateMemberNamesCache()
    }

    /** Make sure the type parameters of this class appear in the order given
     *  by `typeParams` in the scope of the class. Reorder definitions in scope if necessary.
     */
    def ensureTypeParamsInCorrectOrder()(using Context): Unit = {
      val tparams = typeParams
      if (!ctx.erasedTypes && !typeParamsFromDecls.corresponds(tparams)(_.name == _.name)) {
        val decls = info.decls
        val decls1 = newScope
        for (tparam <- typeParams) decls1.enter(decls.lookup(tparam.name))
        for (sym <- decls) if (!tparams.contains(sym)) decls1.enter(sym)
        info = classInfo.derivedClassInfo(decls = decls1)
        myTypeParams = null
      }
    }

    /** All members of this class that have the given name.
     *  The elements of the returned pre-denotation all
     *  have existing symbols.
     */
    final def membersNamed(name: Name)(using Context): PreDenotation =
      Stats.record("membersNamed")
      if Config.cacheMembersNamed then
        var denots: PreDenotation | Null = memberCache.lookup(name)
        if denots == null then
          denots = computeMembersNamed(name)
          memberCache(name) = denots
        else if Config.checkCacheMembersNamed then
          val denots1 = computeMembersNamed(name)
          assert(denots.exists == denots1.exists, s"cache inconsistency: cached: $denots, computed $denots1, name = $name, owner = $this")
        denots
      else computeMembersNamed(name)


    /** All non-private members of this class that have the given name.
     *  The elements of the returned pre-denotation all have existing symbols.
     */
    final def nonPrivateMembersNamed(name: Name)(using Context): PreDenotation =
      membersNamedNoShadowingBasedOnFlags(name, excluded = Private)

    /** All members of this class that have the given name and match the
     *  `required` and `excluded` flag sets; members excluded based on the
     *  flag sets do not shadow inherited members that would not be excluded.
     *
     *  The elements of the returned pre-denotation all have existing symbols.
     */
    final def membersNamedNoShadowingBasedOnFlags(name: Name,
        required: FlagSet = EmptyFlags, excluded: FlagSet = EmptyFlags)(using Context): PreDenotation =
      val mbr = membersNamed(name)
      val filtered = mbr.filterWithFlags(required, excluded)
      if filtered eq mbr then mbr
      else addInherited(name, filtered, required, excluded)

    private[core] def computeMembersNamed(name: Name)(using Context): PreDenotation =
      Stats.record("computeMembersNamed")
      val ownDenots = info.decls.denotsNamed(name)
      if debugTrace then
        println(s"$this.member($name), ownDenots = $ownDenots")
      addInherited(name, ownDenots)

    private def addInherited(name: Name, ownDenots: PreDenotation,
        required: FlagSet = EmptyFlags, excluded: FlagSet = EmptyFlags)(using Context): PreDenotation =
      def collect(denots: PreDenotation, parents: List[Type]): PreDenotation = parents match
        case p :: ps =>
          val denots1 = collect(denots, ps)
          p.classSymbol.denot match
            case parentd: ClassDenotation =>
              val inherited = parentd.membersNamedNoShadowingBasedOnFlags(name, required, excluded | Private)
              denots1.union(inherited.mapInherited(ownDenots, denots1, thisType))
            case _ =>
              denots1
        case nil => denots
      if name.isConstructorName then ownDenots
      else collect(ownDenots, info.parents)

    override final def findMember(name: Name, pre: Type, required: FlagSet, excluded: FlagSet)(using Context): Denotation =
      val raw = if excluded.is(Private) then nonPrivateMembersNamed(name) else membersNamed(name)
      val pre1 = pre match
        case pre: OrType => pre.widenUnion
        case _ => pre
      raw.filterWithFlags(required, excluded).asSeenFrom(pre1).toDenot(pre1)

    final def findMemberNoShadowingBasedOnFlags(name: Name, pre: Type,
        required: FlagSet = EmptyFlags, excluded: FlagSet = EmptyFlags)(using Context): Denotation =
      membersNamedNoShadowingBasedOnFlags(name, required, excluded).asSeenFrom(pre).toDenot(pre)

    /** Compute tp.baseType(this) */
    final def baseTypeOf(tp: Type)(using Context): Type = {
      val btrCache = baseTypeCache
      def inCache(tp: Type) = tp match
        case tp: CachedType => btrCache.contains(tp)
        case _ => false
      def record(tp: CachedType, baseTp: Type) = {
        if (Stats.monitored) {
          Stats.record("basetype cache entries")
          if (!baseTp.exists) Stats.record("basetype cache NoTypes")
        }
        if !(tp.isProvisional || CapturingType.isUncachable(tp) || ctx.gadt.isNarrowing) then
          btrCache(tp) = baseTp
        else
          btrCache.remove(tp) // Remove any potential sentinel value
      }

      def ensureAcyclic(baseTp: Type) = {
        if (baseTp `eq` NoPrefix) throw CyclicReference(this)
        baseTp
      }

      def recur(tp: Type): Type = try {
        tp match {
          case tp: CachedType =>
            val baseTp: Type | Null = btrCache.lookup(tp)
            if (baseTp != null)
              return ensureAcyclic(baseTp)
          case _ =>
        }
        if (Stats.monitored) {
          Stats.record("computeBaseType, total")
          Stats.record(s"computeBaseType, ${tp.getClass}")
        }
        val normed = tp.tryNormalize
        if (normed.exists) return recur(normed)

        tp match {
          case tp @ TypeRef(prefix, _) =>
            def foldGlb(bt: Type, ps: List[Type]): Type = ps match {
              case p :: ps1 => foldGlb(bt & recur(p), ps1)
              case _ => bt
            }

            def computeTypeRef = {
              btrCache(tp) = NoPrefix
              val tpSym = tp.symbol
              tpSym.denot match {
                case clsd: ClassDenotation =>
                  def isOwnThis = prefix match {
                    case prefix: ThisType => prefix.cls `eq` clsd.owner
                    case NoPrefix => true
                    case _ => false
                  }
                  val baseTp =
                    if (tpSym eq symbol)
                      tp
                    else if (isOwnThis)
                      if (clsd.baseClassSet.contains(symbol))
                        if (symbol.isStatic && symbol.typeParams.isEmpty) symbol.typeRef
                        else foldGlb(NoType, clsd.info.parents)
                      else NoType
                    else
                      recur(clsd.typeRef).asSeenFrom(prefix, clsd.owner)
                  record(tp, baseTp)
                  baseTp
                case _ =>
                  val superTp = tp.superType
                  val baseTp = recur(superTp)
                  if (inCache(superTp))
                    record(tp, baseTp)
                  else
                    btrCache.remove(tp)
                  baseTp
              }
            }
            computeTypeRef

          case tp @ AppliedType(tycon, args) =>
            def computeApplied = {
              btrCache(tp) = NoPrefix
              val baseTp =
                if (tycon.typeSymbol eq symbol) && !tycon.isLambdaSub then tp
                else (tycon.typeParams: @unchecked) match
                  case LambdaParam(_, _) :: _ =>
                    recur(tp.superType)
                  case tparams: List[Symbol @unchecked] =>
                    recur(tycon).substApprox(tparams, args)
              record(tp, baseTp)
              baseTp
            }
            computeApplied

          case tp: TypeParamRef =>  // uncachable, since baseType depends on context bounds
            recur(TypeComparer.bounds(tp).hi)

          case CapturingType(parent, refs) =>
            tp.derivedCapturingType(recur(parent), refs)

          case tp: TypeProxy =>
            def computeTypeProxy = {
              val superTp = tp.superType
              val baseTp = recur(superTp)
              tp match {
                case tp: CachedType if baseTp.exists && inCache(superTp) =>
                  record(tp, baseTp)
                case _ =>
              }
              baseTp
            }
            computeTypeProxy

          case tp: AndOrType =>
            def computeAndOrType: Type =
              val tp1 = tp.tp1
              val tp2 = tp.tp2
              if !tp.isAnd then
                if tp1.isBottomType && (tp1 frozen_<:< tp2) then return recur(tp2)
                if tp2.isBottomType && (tp2 frozen_<:< tp1) then return recur(tp1)
              val baseTp =
                if symbol.isStatic && tp.derivesFrom(symbol) && symbol.typeParams.isEmpty then
                  symbol.typeRef
                else
                  val baseTp1 = recur(tp1)
                  val baseTp2 = recur(tp2)
                  val combined = if (tp.isAnd) baseTp1 & baseTp2 else baseTp1 | baseTp2
                  combined match
                    case combined: AndOrType
                    if (combined.tp1 eq tp1) && (combined.tp2 eq tp2) && (combined.isAnd == tp.isAnd) => tp
                    case _ => combined

              if (baseTp.exists && inCache(tp1) && inCache(tp2)) record(tp, baseTp)
              baseTp

            computeAndOrType

          case JavaArrayType(_) if symbol == defn.ObjectClass =>
            this.typeRef

          case _ =>
            NoType
        }
      }
      catch {
        case ex: Throwable =>
          tp match
            case tp: CachedType => btrCache.remove(tp)
            case _ =>
          throw ex
      }


      trace.onDebug(s"$tp.baseType($this)") {
        Stats.record("baseTypeOf")
        recur(tp)
      }
    }

    def memberNames(keepOnly: NameFilter)(implicit onBehalf: MemberNames, ctx: Context): Set[Name] =
      if (this.is(PackageClass) || !Config.cacheMemberNames)
        computeMemberNames(keepOnly) // don't cache package member names; they might change
      else {
        if (!memberNamesCache.isValid) memberNamesCache = MemberNames.newCache()
        memberNamesCache(keepOnly, this)
      }

    def computeMemberNames(keepOnly: NameFilter)(implicit onBehalf: MemberNames, ctx: Context): Set[Name] = {
      var names = Set[Name]()
      def maybeAdd(name: Name) = if (keepOnly(thisType, name)) names += name
      try {
        for ptype <- parentTypes do
          ptype.classSymbol match
            case pcls: ClassSymbol =>
              for name <- pcls.memberNames(keepOnly) do
                maybeAdd(name)
        val ownSyms =
          if (keepOnly eq implicitFilter)
            if (this.is(Package)) Iterator.empty
              // implicits in package objects are added by the overriding `memberNames` in `PackageClassDenotation`
            else info.decls.iterator.filter(_.isOneOf(GivenOrImplicitVal))
          else info.decls.iterator
        for (sym <- ownSyms) maybeAdd(sym.name)
        names
      }
      catch {
        case ex: Throwable =>
          handleRecursive("member names", i"of $this", ex)
      }
    }

    override final def fullNameSeparated(kind: QualifiedNameKind)(using Context): Name = {
      val cached = fullNameCache(kind)
      if (cached != null) cached
      else {
        val fn = super.fullNameSeparated(kind)
        fullNameCache = fullNameCache.updated(kind, fn)
        fn
      }
    }

    // to avoid overloading ambiguities
    override def fullName(using Context): Name = super.fullName

    override def primaryConstructor(using Context): Symbol = {
      def constrNamed(cname: TermName) = info.decls.denotsNamed(cname).last.symbol
        // denotsNamed returns Symbols in reverse order of occurrence
      if (this.is(Package)) NoSymbol
      else constrNamed(nme.CONSTRUCTOR).orElse(constrNamed(nme.TRAIT_CONSTRUCTOR))
    }

    /** The term parameter accessors of this class.
     *  Both getters and setters are returned in this list.
     */
    def paramAccessors(using Context): List[Symbol] =
      unforcedDecls.filter(_.is(ParamAccessor))

    /** The term parameter getters of this class. */
    def paramGetters(using Context): List[Symbol] =
      paramAccessors.filterNot(_.isSetter)

    /** If this class has the same `decls` scope reference in `phase` and
     *  `phase.next`, install a new denotation with a cloned scope in `phase.next`.
     */
    def ensureFreshScopeAfter(phase: DenotTransformer)(using Context): Unit =
      if (ctx.phaseId != phase.next.id) atPhase(phase.next)(ensureFreshScopeAfter(phase))
      else {
        val prevClassInfo = atPhase(phase) {
          current.asInstanceOf[ClassDenotation].classInfo
        }
        val ClassInfo(pre, _, ps, decls, selfInfo) = classInfo
        if (prevClassInfo.decls eq decls)
          copySymDenotation(info = ClassInfo(pre, classSymbol, ps, decls.cloneScope, selfInfo))
            .copyCaches(this, phase.next)
            .installAfter(phase)
      }

    private var myCompanion: Symbol = NoSymbol

    /** Register companion class */
    override def registerCompanion(companion: Symbol)(using Context) =
      if (companion.isClass && !isAbsent(canForce = false) && !companion.isAbsent(canForce = false))
        myCompanion = companion

    private[core] def unforcedRegisteredCompanion: Symbol = myCompanion

    override def registeredCompanion(using Context) =
      if !myCompanion.exists then
        ensureCompleted()
      myCompanion

    override def registeredCompanion_=(c: Symbol) =
      myCompanion = c
  }

  /** The denotation of a package class.
   *  It overrides ClassDenotation to take account of package objects when looking for members
   */
  final class PackageClassDenotation private[SymDenotations] (
    symbol: Symbol,
    ownerIfExists: Symbol,
    name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol)
    extends ClassDenotation(symbol, ownerIfExists, name, initFlags, initInfo, initPrivateWithin) {

    private var packageObjsCache: List[ClassDenotation] = uninitialized
    private var packageObjsRunId: RunId = NoRunId
    private var ambiguityWarningIssued: Boolean = false

    /** The package objects in this class */
    def packageObjs(using Context): List[ClassDenotation] = {
      if (packageObjsRunId != ctx.runId) {
        packageObjsRunId = ctx.runId
        packageObjsCache = Nil // break cycle in case we are looking for package object itself
        packageObjsCache = {
          val pkgObjBuf = new mutable.ListBuffer[ClassDenotation]
          for (sym <- info.decls) { // don't use filter, since that loads classes with `$`s in their name
            val denot = sym.lastKnownDenotation  // don't use `sym.denot`, as this brings forward classes too early
            if (denot.isType && denot.name.isPackageObjectName)
              pkgObjBuf += sym.asClass.classDenot
          }
          pkgObjBuf.toList
        }
      }
      packageObjsCache
    }

    /** The package object (as a term symbol) in this package that might contain
     *  `sym` as a member.
     */
    def packageObjFor(sym: Symbol)(using Context): Symbol = {
      val owner = sym.maybeOwner
      if (owner.is(Package)) NoSymbol
      else if (owner.isPackageObject) owner.sourceModule
      else // owner could be class inherited by package object (until package object inheritance is removed)
        packageObjs.find(_.name == packageTypeName) match {
          case Some(pobj) => pobj.sourceModule
          case _ => NoSymbol
        }
    }

    /** Looks in both the package object and the package for members. The precise algorithm
     *  is as follows:
     *
     *  If this is the scala package look in the package first, and if nothing is found
     *  there, look in the package object second. Otherwise, look in the both the package object
     *  and the package and form a union of the results.
     *
     *  The reason for the special treatment of the scala package is that if we
     *  complete it too early, we freeze its superclass Any, so that no members can
     *  be entered in it. As a consequence, there should be no entry in the scala package
     *  object that hides a class or object in the scala package of the same name, because
     *  the behavior would then be unintuitive for such members.
     */
    override def computeMembersNamed(name: Name)(using Context): PreDenotation =

      def recur(pobjs: List[ClassDenotation], acc: PreDenotation): PreDenotation = pobjs match
        case pcls :: pobjs1 =>
          if (pcls.isCompleting) recur(pobjs1, acc)
          else
            val pobjMembers = pcls.nonPrivateMembersNamed(name).filterWithPredicate { d =>
              !defn.topClasses.contains(d.symbol.maybeOwner) // Drop members of top classes
            }
            recur(pobjs1, acc.union(pobjMembers))
        case nil =>
          val directMembers = super.computeMembersNamed(name)
          if !acc.exists then directMembers
          else acc.union(directMembers.filterWithPredicate(!_.symbol.isAbsent())) match
            case d: DenotUnion => dropStale(d)
            case d => d

      /** Filter symbols making up a DenotUnion to remove alternatives from stale classfiles.
       *  This proceeds as follow:
       *
       *   - prefer alternatives that are currently compiled over ones that have been compiled before.
       *   - if no alternative is compiled now, and they all come from the same file, keep all of them
       *   - if no alternative is compiled now, and they come from different files, keep the
       *     ones from the youngest file, but issue a warning that one of the class files
       *     should be removed from the classpath.
       */
      def dropStale(multi: DenotUnion): PreDenotation =
        val compiledNow = multi.filterWithPredicate(d =>
          d.symbol.isDefinedInCurrentRun || d.symbol.associatedFile == null
            // if a symbol does not have an associated file, assume it is defined
            // in the current run anyway. This is true for packages, and also can happen for pickling and
            // from-tasty tests that generate a fresh symbol and then re-use it in the next run.
          )
        if compiledNow.exists then compiledNow
        else
          val assocFiles = multi.aggregate(d => Set(d.symbol.associatedFile.nn), _ union _)
          if assocFiles.size == 1 then
            multi // they are all overloaded variants from the same file
          else
            // pick the variant(s) from the youngest class file
            val lastModDate = assocFiles.map(_.lastModified).max
            val youngest = assocFiles.filter(_.lastModified == lastModDate)
            val chosen = youngest.head
            def ambiguousFilesMsg(f: AbstractFile) =
              i"""Toplevel definition $name is defined in
                 |  $chosen
                 |and also in
                 |  $f"""
            if youngest.size > 1 then
              throw TypeError(em"""${ambiguousFilesMsg(youngest.tail.head)}
                                  |One of these files should be removed from the classpath.""")

            // Warn if one of the older files comes from a different container.
            // In that case picking the youngest file is not necessarily what we want,
            // since the older file might have been loaded from a jar earlier in the
            // classpath.
            def sameContainer(f: AbstractFile): Boolean =
              try f.container == chosen.container catch case NonFatal(ex) => true
            if !ambiguityWarningIssued then
              for conflicting <- assocFiles.find(!sameContainer(_)) do
                report.warning(em"""${ambiguousFilesMsg(conflicting.nn)}
                                   |Keeping only the definition in $chosen""")
                ambiguityWarningIssued = true
            multi.filterWithPredicate(_.symbol.associatedFile == chosen)
      end dropStale

      if name == nme.CONSTRUCTOR then
        NoDenotation // packages don't have constructors, even if package objects do.
      else if symbol eq defn.ScalaPackageClass then
        // revert order: search package first, then nested package objects
        val denots = super.computeMembersNamed(name)
        if denots.exists then denots
        else recur(packageObjs, NoDenotation)
      else recur(packageObjs, NoDenotation)
    end computeMembersNamed

    /** The union of the member names of the package and the package object */
    override def memberNames(keepOnly: NameFilter)(implicit onBehalf: MemberNames, ctx: Context): Set[Name] = {
      def recur(pobjs: List[ClassDenotation], acc: Set[Name]): Set[Name] = pobjs match {
        case pcls :: pobjs1 =>
          recur(pobjs1, acc.union(pcls.memberNames(keepOnly)))
        case nil =>
          acc
      }
      recur(packageObjs, super.memberNames(keepOnly))
    }

    /** If another symbol with the same name is entered, unlink it.
     *  If symbol is a package object, invalidate the packageObj cache.
     *  @return  `sym` is not already entered
     */
    override def proceedWithEnter(sym: Symbol, mscope: MutableScope)(using Context): Boolean = {
      val entry = mscope.lookupEntry(sym.name)
      if (entry != null) {
        if (entry.sym == sym) return false
        mscope.unlink(entry)
      }
      if (sym.name.isPackageObjectName) packageObjsRunId = NoRunId
      true
    }

    /** Unlink all package members defined in `file` in a previous run. */
    def unlinkFromFile(file: AbstractFile)(using Context): Unit = {
      val scope = unforcedDecls.openForMutations
      for (sym <- scope.toList.iterator)
        // We need to be careful to not force the denotation of `sym` here,
        // otherwise it will be brought forward to the current run.
        if (sym.defRunId != ctx.runId && sym.isClass && sym.asClass.compUnitInfo != null && sym.asClass.compUnitInfo.nn.associatedFile == file)
          scope.unlink(sym, sym.lastKnownDenotation.name)
    }
  }

  @sharable object NoDenotation
  extends SymDenotation(NoSymbol, NoSymbol, "<none>".toTermName, Permanent, NoType) {
    override def isTerm: Boolean = false
    override def exists: Boolean = false
    override def owner: Symbol = throw new AssertionError("NoDenotation.owner")
    override def computeAsSeenFrom(pre: Type)(using Context): SingleDenotation = this
    override def mapInfo(f: Type => Type)(using Context): SingleDenotation = this
    override def asSeenFrom(pre: Type)(using Context): AsSeenFromResult = this

    override def matches(other: SingleDenotation)(using Context): Boolean = false
    override def targetName(using Context): Name = EmptyTermName
    override def mapInherited(ownDenots: PreDenotation, prevDenots: PreDenotation, pre: Type)(using Context): SingleDenotation = this
    override def filterWithPredicate(p: SingleDenotation => Boolean): SingleDenotation = this
    override def filterDisjoint(denots: PreDenotation)(using Context): SingleDenotation = this
    override def filterWithFlags(required: FlagSet, excluded: FlagSet)(using Context): SingleDenotation = this

    NoSymbol.denot = this
    validFor = Period.allInRun(NoRunId)
  }

  /** Can a private symbol with given name and flags be inferred to be local,
   *  if all references to such symbols are via `this`?
   *  This holds for all symbols except
   *   - constructors, since they can never be referred to as members of their
   *     own, fully elaborated `this`.
   *   - parameters and parameter accessors, since their Local status is already
   *     determined by whether they have a `val` or `var` or not.
   */
  def canBeLocal(name: Name, flags: FlagSet)(using Context) =
    !name.isConstructorName && !flags.is(Param) && !flags.is(ParamAccessor)

  /** Factory method for SymDenotion creation. All creations
   *  should be done via this method.
   */
  def SymDenotation(
    symbol: Symbol,
    owner: Symbol,
    name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol = NoSymbol)(using Context): SymDenotation = {
    val result =
      if (symbol.isClass)
        if (initFlags.is(Package)) new PackageClassDenotation(symbol, owner, name, initFlags, initInfo, initPrivateWithin)
        else new ClassDenotation(symbol, owner, name, initFlags, initInfo, initPrivateWithin)
      else new SymDenotation(symbol, owner, name, initFlags, initInfo, initPrivateWithin)
    result.validFor = currentStablePeriod
    result
  }

  def stillValid(denot: SymDenotation)(using Context): Boolean =
    if (denot.isOneOf(ValidForeverFlags) || denot.isRefinementClass || denot.isImport) true
    else {
      val initial = denot.initial
      val firstPhaseId =
        initial.validFor.firstPhaseId.max(typerPhase.id)
      if firstPhaseId > ctx.lastPhaseId then
        false
      else if (initial ne denot) || ctx.phaseId != firstPhaseId then
        atPhase(firstPhaseId)(stillValidInOwner(initial))
      else
        stillValidInOwner(denot)
    }

  private[SymDenotations] def stillValidInOwner(denot: SymDenotation)(using Context): Boolean = try
    val owner = denot.maybeOwner.denot
    stillValid(owner)
    && (
      !owner.isClass
      || owner.isRefinementClass
      || owner.is(Scala2x)
      || owner.unforcedDecls.contains(denot.name, denot.symbol)
      || (denot.is(Synthetic) && denot.is(ModuleClass) && stillValidInOwner(denot.companionClass))
      || denot.isSelfSym
      || denot.isLocalDummy)
  catch case ex: StaleSymbol => false

  /** Explain why symbol is invalid; used for debugging only */
  def traceInvalid(denot: Denotation)(using Context): Boolean = {
    def show(d: Denotation) = s"$d#${d.symbol.id}"
    def explain(msg: String) = {
      println(s"${show(denot)} is invalid at ${ctx.period} because $msg")
      false
    }
    denot match {
      case denot: SymDenotation =>
        def explainSym(msg: String) = explain(s"$msg\ndefined = ${denot.definedPeriodsString}")
        if (denot.isOneOf(ValidForeverFlags) || denot.isRefinementClass) true
        else
          val initial = denot.initial
          if ((initial ne denot) || ctx.phaseId != initial.validFor.firstPhaseId)
            atPhase(initial.validFor.firstPhaseId)(traceInvalid(initial))
          else try {
            val owner = denot.owner.denot
            if (!traceInvalid(owner)) explainSym("owner is invalid")
            else if (!owner.isClass || owner.isRefinementClass || denot.isSelfSym) true
            else if (owner.unforcedDecls.lookupAll(denot.name) contains denot.symbol) true
            else explainSym(s"decls of ${show(owner)} are ${owner.unforcedDecls.lookupAll(denot.name).toList}, do not contain ${denot.symbol}")
          }
          catch {
            case ex: StaleSymbol => explainSym(s"$ex was thrown")
          }
      case _ =>
        explain("denotation is not a SymDenotation")
    }
  }

  /** Configurable: Accept stale symbol with warning if in IDE
   *  Always accept stale symbols when testing pickling.
   */
  def staleOK(using Context): Boolean =
    Config.ignoreStaleInIDE && ctx.mode.is(Mode.Interactive)
    || ctx.settings.YtestPickler.value

  /** Possibly accept stale symbol with warning if in IDE */
  def acceptStale(denot: SingleDenotation)(using Context): Boolean =
    staleOK && {
      report.debugwarn(denot.staleSymbolMsg)
      true
    }

// ---- Completion --------------------------------------------------------

  /** Instances of LazyType are carried by uncompleted symbols.
   *  Note: LazyTypes double up as (constant) functions from Symbol and
   *  from (TermSymbol, ClassSymbol) to LazyType. That way lazy types can be
   *  directly passed to symbol creation methods in Symbols that demand instances
   *  of these function types.
   */
  abstract class LazyType extends UncachedGroundType
    with (Symbol => LazyType)
    with ((TermSymbol, ClassSymbol) => LazyType) { self =>

    /** Sets all missing fields of given denotation */
    def complete(denot: SymDenotation)(using Context): Unit

    def apply(sym: Symbol): LazyType = this
    def apply(module: TermSymbol, modcls: ClassSymbol): LazyType = this

    private var myDecls: Scope = EmptyScope
    private var mySourceModule: Symbol | Null = null
    private var myModuleClass: Symbol | Null = null
    private var mySourceModuleFn: Context ?=> Symbol = LazyType.NoSymbolFn
    private var myModuleClassFn: Context ?=> Symbol = LazyType.NoSymbolFn

    /** The type parameters computed by the completer before completion has finished */
    def completerTypeParams(sym: Symbol)(using Context): List[TypeParamInfo] =
      if (sym.is(Touched)) Nil // return `Nil` instead of throwing a cyclic reference
      else sym.info.typeParams

    def decls: Scope = myDecls
    def sourceModule(using Context): Symbol =
      if mySourceModule == null then mySourceModule = mySourceModuleFn
      mySourceModule.nn
    def moduleClass(using Context): Symbol =
      if myModuleClass == null then myModuleClass = myModuleClassFn
      myModuleClass.nn

    def withDecls(decls: Scope): this.type = { myDecls = decls; this }
    def withSourceModule(sourceModuleFn: Context ?=> Symbol): this.type = { mySourceModuleFn = sourceModuleFn; this }
    def withModuleClass(moduleClassFn: Context ?=> Symbol): this.type = { myModuleClassFn = moduleClassFn; this }

    override def toString: String = getClass.toString

    /** A hook that is called before trying to complete a symbol with its
     *  associated cycle detection via the Touched flag. This is overridden
     *  for Type definitions in Namer, where we make sure that owners are
     *  completed before nested types.
     */
    def needsCompletion(symd: SymDenotation)(using Context): Boolean = true
  }

  object LazyType:
    private val NoSymbolFn = (_: Context) ?=> NoSymbol

  /** A subtrait of LazyTypes where completerTypeParams yields a List[TypeSymbol], which
   *  should be completed independently of the info.
   */
  trait TypeParamsCompleter extends LazyType {
    override def completerTypeParams(sym: Symbol)(using Context): List[TypeSymbol] =
      unsupported("completerTypeParams") // should be abstract, but Scala-2 will then compute the wrong type for it
  }

  /** A missing completer */
  trait NoCompleter extends LazyType {
    def complete(denot: SymDenotation)(using Context): Unit = unsupported("complete")
  }

  @sharable object NoCompleter extends NoCompleter

  /** A lazy type for modules that points to the module class.
   *  Needed so that `moduleClass` works before completion.
   *  Completion of modules is always completion of the underlying
   *  module class, followed by copying the relevant fields to the module.
   */
  class ModuleCompleter(_moduleClass: ClassSymbol) extends LazyType {
    override def moduleClass(using Context): ClassSymbol = _moduleClass
    def complete(denot: SymDenotation)(using Context): Unit = {
      val from = moduleClass.denot.asClass
      denot.setFlag(from.flags.toTermFlags & RetainedModuleValFlags)
      denot.annotations = from.annotations filter (_.appliesToModule)
        // !!! ^^^ needs to be revised later. The problem is that annotations might
        // only apply to the module but not to the module class. The right solution
        // is to have the module class completer set the annotations of both the
        // class and the module.
      denot.info = moduleClass.typeRef
      denot.setPrivateWithin(from.privateWithin)
    }
  }

  /** A completer for missing references */
  class StubInfo() extends LazyType {

    def initializeToDefaults(denot: SymDenotation, errMsg: Message)(using Context): Unit = {
      denot.info = denot match {
        case denot: ClassDenotation =>
          ClassInfo(denot.owner.thisType, denot.classSymbol, Nil, EmptyScope)
        case _ =>
          ErrorType(errMsg)
      }
      denot.setPrivateWithin(NoSymbol)
    }

    def complete(denot: SymDenotation)(using Context): Unit = {
      val sym = denot.symbol
      val errMsg = BadSymbolicReference(denot)
      report.error(errMsg, sym.srcPos)
      if (ctx.debug) throw new scala.Error()
      initializeToDefaults(denot, errMsg)
    }
  }

  // ---- Caches for inherited info -----------------------------------------

  /** Base trait for caches that keep info dependent on inherited classes */
  trait InheritedCache {

    /** Is the cache valid in current period? */
    def isValid(using Context): Boolean

    /** is the cache valid in current run at given phase? */
    def isValidAt(phase: Phase)(using Context): Boolean

    /** Render invalid this cache and all caches that depend on it */
    def invalidate(): Unit
  }

  /** A cache for sets of member names, indexed by a NameFilter */
  trait MemberNames extends InheritedCache {
    def apply(keepOnly: NameFilter, clsd: ClassDenotation)
             (implicit onBehalf: MemberNames, ctx: Context): Set[Name]
  }

  object MemberNames {
    implicit val None: MemberNames = new InvalidCache with MemberNames {
      def apply(keepOnly: NameFilter, clsd: ClassDenotation)(implicit onBehalf: MemberNames, ctx: Context) = ???
    }
    def newCache()(using Context): MemberNames = new MemberNamesImpl(ctx.period)
  }

  /** A cache for baseclasses, as a sequence in linearization order and as a set that
   *  can be queried efficiently for containment.
   */
  trait BaseData extends InheritedCache {
    def apply(clsd: ClassDenotation)
             (implicit onBehalf: BaseData, ctx: Context): (List[ClassSymbol], BaseClassSet)
    def signalProvisional(): Unit
  }

  object BaseData {
    implicit val None: BaseData = new InvalidCache with BaseData {
      def apply(clsd: ClassDenotation)(implicit onBehalf: BaseData, ctx: Context) = ???
      def signalProvisional() = ()
    }
    def newCache()(using Context): BaseData = new BaseDataImpl(ctx.period)
  }

  private abstract class InheritedCacheImpl(val createdAt: Period) extends InheritedCache {
    protected def sameGroup(p1: Phase, p2: Phase): Boolean

    private var dependent: WeakHashMap[InheritedCache, Unit] | Null = null
    private var checkedPeriod: Period = Nowhere

    protected def invalidateDependents() = {
      import scala.language.unsafeNulls
      if (dependent != null) {
        val it = dependent.keySet.iterator()
        while (it.hasNext()) it.next().invalidate()
      }
      dependent = null
    }

    protected def addDependent(dep: InheritedCache) = {
      if (dependent == null) dependent = new WeakHashMap
      dependent.nn.put(dep, ())
    }

    def isValidAt(phase: Phase)(using Context) =
      checkedPeriod.code == ctx.period.code ||
        createdAt.runId == ctx.runId &&
        createdAt.phaseId < unfusedPhases.length &&
        sameGroup(unfusedPhases(createdAt.phaseId), phase) &&
        { checkedPeriod = ctx.period; true }
  }

  private class InvalidCache extends InheritedCache {
    def isValid(using Context) = false
    def isValidAt(phase: Phase)(using Context) = false
    def invalidate(): Unit = ()
  }

  private class MemberNamesImpl(createdAt: Period) extends InheritedCacheImpl(createdAt) with MemberNames {
    private var cache: SimpleIdentityMap[NameFilter, Set[Name]] | Null = SimpleIdentityMap.empty

    final def isValid(using Context): Boolean =
      cache != null && isValidAt(ctx.phase)

    private var locked = false

    /** Computing parent member names might force parents, which could invalidate
     *  the cache itself. In that case we should cancel invalidation and
     *  proceed as usual. However, all cache entries should be cleared.
     */
    def invalidate(): Unit =
      if (cache != null)
        if (locked) cache = SimpleIdentityMap.empty
        else {
          cache = null
          invalidateDependents()
        }

    def apply(keepOnly: NameFilter, clsd: ClassDenotation)(implicit onBehalf: MemberNames, ctx: Context) = {
      assert(isValid)
      val cached = cache.nn(keepOnly)
      try
        if (cached != null) cached
        else {
          locked = true
          val computed =
            try clsd.computeMemberNames(keepOnly)(this, ctx)
            finally locked = false
          cache = cache.nn.updated(keepOnly, computed)
          computed
        }
      finally addDependent(onBehalf)
    }

    def sameGroup(p1: Phase, p2: Phase) = p1.sameMembersStartId == p2.sameMembersStartId
  }

  private class BaseDataImpl(createdAt: Period) extends InheritedCacheImpl(createdAt) with BaseData {
    private var cache: (List[ClassSymbol], BaseClassSet) | Null = null

    private var valid = true
    private var locked = false
    private var provisional = false

    final def isValid(using Context): Boolean =
      valid && createdAt.runId == ctx.runId
        // Note: We rely on the fact that whenever base types of classes change,
        // the affected classes will get new denotations with new basedata caches.
        // So basedata caches can become invalid only if the run changes.

    def invalidate(): Unit =
      if (valid && !locked) {
        cache = null
        valid = false
        invalidateDependents()
      }

    def signalProvisional() = provisional = true

    def apply(clsd: ClassDenotation)(implicit onBehalf: BaseData, ctx: Context)
        : (List[ClassSymbol], BaseClassSet) = {
      assert(isValid)
      val traceCycles = CyclicReference.isTraced
      try
        if traceCycles then
          CyclicReference.pushTrace("compute the base classes of ", clsd.symbol, "")
        if (cache != null) cache.uncheckedNN
        else {
          if (locked) throw CyclicReference(clsd)
          locked = true
          provisional = false
          val computed =
            try clsd.computeBaseData(this, ctx)
            finally locked = false
          if (!provisional) cache = computed
          else onBehalf.signalProvisional()
          computed
        }
      finally
        if traceCycles then CyclicReference.popTrace()
        addDependent(onBehalf)
    }

    def sameGroup(p1: Phase, p2: Phase) = p1.sameParentsStartId == p2.sameParentsStartId
  }

  class BaseClassSet(val classIds: Array[Int]) extends AnyVal {
    def contains(sym: Symbol, limit: Int): Boolean = {
      val id = sym.id
      var i = 0
      while (i < limit && classIds(i) != id) i += 1
      i < limit && {
        if (i > 0) {
          val t = classIds(i)
          classIds(i) = classIds(i - 1)
          classIds(i - 1) = t
        }
        true
      }
    }
    def contains(sym: Symbol): Boolean = contains(sym, classIds.length)
  }

  object BaseClassSet {
    def apply(bcs: List[ClassSymbol]): BaseClassSet =
      new BaseClassSet(bcs.toArray.map(_.id))
  }

  /** A class to combine base data from parent types */
  class BaseDataBuilder {
    private var classes: List[ClassSymbol] = Nil
    private var classIds = new Array[Int](32)
    private var length = 0

    private def resize(size: Int) = {
      val classIds1 = new Array[Int](size)
      System.arraycopy(classIds, 0, classIds1, 0, classIds.length min size)
      classIds = classIds1
    }

    private def add(sym: Symbol): Unit = {
      if (length == classIds.length) resize(length * 2)
      classIds(length) = sym.id
      length += 1
    }

    def addAll(bcs: List[ClassSymbol]): this.type = {
      val len = length
      bcs match {
        case bc :: bcs1 =>
          addAll(bcs1)
          if (!new BaseClassSet(classIds).contains(bc, len)) {
            add(bc)
            classes = bc :: classes
          }
        case nil =>
      }
      this
    }

    def baseClassSet: BaseClassSet = {
      if (length != classIds.length) resize(length)
      new BaseClassSet(classIds)
    }

    def baseClasses: List[ClassSymbol] = classes
  }

  private val packageTypeName = ModuleClassName(nme.PACKAGE).toTypeName

  @sharable private var indent = 0 // for completions printing
}
