package dotty.tools
package dotc
package typer

import core.*
import ast.*
import Trees.*, StdNames.*, Scopes.*, Denotations.*, NamerOps.*, ContextOps.*
import Contexts.*, Symbols.*, Types.*, SymDenotations.*, Names.*, NameOps.*, Flags.*
import Decorators.*, Comments.{_, given}
import NameKinds.DefaultGetterName
import ast.desugar, ast.desugar.*
import ProtoTypes.*
import util.Spans.*
import util.Property
import collection.mutable
import tpd.tpes
import Variances.alwaysInvariant
import config.{Config, Feature}
import config.Printers.typr
import inlines.{Inlines, PrepareInlineable}
import parsing.JavaParsers.JavaParser
import parsing.Parsers.Parser
import Annotations.*
import Inferencing.*
import Nullables.*
import transform.ValueClasses.*
import TypeErasure.erasure
import reporting.*
import config.Feature.sourceVersion
import config.SourceVersion.*

import scala.compiletime.uninitialized

/** This class creates symbols from definitions and imports and gives them
 *  lazy types.
 *
 *  Timeline:
 *
 *  During enter, trees are expanded as necessary, populating the expandedTree map.
 *  Symbols are created, and the symOfTree map is set up.
 *
 *  Symbol completion causes some trees to be already typechecked and typedTree
 *  entries are created to associate the typed trees with the untyped expanded originals.
 *
 *  During typer, original trees are first expanded using expandedTree. For each
 *  expanded member definition or import we extract and remove the corresponding symbol
 *  from the symOfTree map and complete it. We then consult the typedTree map to see
 *  whether a typed tree exists already. If yes, the typed tree is returned as result.
 *  Otherwise, we proceed with regular type checking.
 *
 *  The scheme is designed to allow sharing of nodes, as long as each duplicate appears
 *  in a different method.
 */
class Namer { typer: Typer =>

  import untpd.*

  val TypedAhead      : Property.Key[tpd.Tree]            = new Property.Key
  val ExpandedTree    : Property.Key[untpd.Tree]          = new Property.Key
  val ExportForwarders: Property.Key[List[tpd.MemberDef]] = new Property.Key
  val SymOfTree       : Property.Key[Symbol]              = new Property.Key
  val AttachedDeriver : Property.Key[Deriver]             = new Property.Key
    // was `val Deriver`, but that gave shadowing problems with constructor proxies

  /** A partial map from unexpanded member and pattern defs and to their expansions.
   *  Populated during enterSyms, emptied during typer.
   */
  //lazy val expandedTree = new mutable.AnyRefMap[DefTree, Tree]
  /*{
    override def default(tree: DefTree) = tree // can't have defaults on AnyRefMaps :-(
  }*/

  /** A map from expanded MemberDef, PatDef or Import trees to their symbols.
   *  Populated during enterSyms, emptied at the point a typed tree
   *  with the same symbol is created (this can be when the symbol is completed
   *  or at the latest when the tree is typechecked.
   */
  //lazy val symOfTree = new mutable.AnyRefMap[Tree, Symbol]

  /** A map from expanded trees to their typed versions.
   *  Populated when trees are typechecked during completion (using method typedAhead).
   */
  // lazy val typedTree = new mutable.AnyRefMap[Tree, tpd.Tree]

  /** A map from method symbols to nested typers.
   *  Populated when methods are completed. Emptied when they are typechecked.
   *  The nested typer contains new versions of the four maps above including this
   *  one, so that trees that are shared between different DefDefs can be independently
   *  used as indices. It also contains a scope that contains nested parameters.
   */
  lazy val nestedTyper: mutable.AnyRefMap[Symbol, Typer] = new mutable.AnyRefMap

  /** We are entering symbols coming from a SourceLoader */
  private var lateCompile = false

  /** The symbol of the given expanded tree. */
  def symbolOfTree(tree: Tree)(using Context): Symbol = {
    val xtree = expanded(tree)
    xtree.getAttachment(TypedAhead) match {
      case Some(ttree) => ttree.symbol
      case none =>
        xtree.getAttachment(SymOfTree) match
          case Some(sym) => sym
          case _ => throw IllegalArgumentException(i"$xtree does not have a symbol")
    }
  }

  def hasDefinedSymbol(tree: Tree)(using Context): Boolean =
    val xtree = expanded(tree)
    xtree.hasAttachment(TypedAhead) || xtree.hasAttachment(SymOfTree)

  /** The enclosing class with given name; error if none exists */
  def enclosingClassNamed(name: TypeName, span: Span)(using Context): Symbol =
    if (name.isEmpty) NoSymbol
    else {
      val cls = ctx.owner.enclosingClassNamed(name)
      if (!cls.exists)
        report.error(UnknownNamedEnclosingClassOrObject(name), ctx.source.atSpan(span))
      cls
    }

  /** Record `sym` as the symbol defined by `tree` */
  def recordSym(sym: Symbol, tree: Tree)(using Context): Symbol = {
    for (refs <- tree.removeAttachment(References); ref <- refs) ref.watching(sym)
    tree.pushAttachment(SymOfTree, sym)
    sym
  }

  /** Check that a new definition with given name and privacy status
   *  in current context would not conflict with existing currently
   *  compiled definitions.
   *  The logic here is very subtle and fragile due to the fact that
   *  we are not allowed to force anything.
   */
  def checkNoConflict(name: Name, isPrivate: Boolean, span: Span)(using Context): Name =
    val owner = ctx.owner
    var conflictsDetected = false

    def conflict(conflicting: Symbol) =
      val other =
        if conflicting.is(ConstructorProxy) then conflicting.companionClass
        else conflicting
      report.error(AlreadyDefined(name, owner, other), ctx.source.atSpan(span))
      conflictsDetected = true

    def checkNoConflictIn(owner: Symbol) =
      val preExisting = owner.unforcedDecls.lookup(name)
      if (preExisting.isDefinedInCurrentRun || preExisting.lastKnownDenotation.is(Package))
          && (!preExisting.lastKnownDenotation.is(Private) || preExisting.owner.is(Package))
          && (!preExisting.lastKnownDenotation.isPackageObject
              || preExisting.associatedFile != ctx.source.file)
              // isDefinedInCurrentRun does not work correctly for package objects, because
              // package objects are updated to the new run earlier than normal classes, everytime
              // some member of the enclosing package is accessed. Therefore, we use another
              // test: conflict if package objects have the same name but come from different
              // sources. See i9252.
      then conflict(preExisting)

    def pkgObjs(pkg: Symbol) =
      pkg.denot.asInstanceOf[PackageClassDenotation].packageObjs.map(_.symbol)

    if owner.is(PackageClass) then
      checkNoConflictIn(owner)
      for pkgObj <- pkgObjs(owner) do
        checkNoConflictIn(pkgObj)
    else
      def preExisting = ctx.effectiveScope.lookup(name)
      if (!owner.isClass || name.isTypeName) && preExisting.exists then
        conflict(preExisting)
      else if owner.isPackageObject && !isPrivate && name != nme.CONSTRUCTOR then
        checkNoConflictIn(owner.owner)
        for pkgObj <- pkgObjs(owner.owner) if pkgObj != owner do
          checkNoConflictIn(pkgObj)

    if conflictsDetected then name.freshened else name
  end checkNoConflict

  /** If this tree is a member def or an import, create a symbol of it
   *  and store in symOfTree map.
   */
  def createSymbol(tree: Tree)(using Context): Symbol = {

    def privateWithinClass(mods: Modifiers) =
      enclosingClassNamed(mods.privateWithin, tree.span)

    /** Check that flags are OK for symbol. This is done early to avoid
     *  catastrophic failure when we create a TermSymbol with TypeFlags, or vice versa.
     *  A more complete check is done in checkWellFormed.
     *  Also, speculatively add a Local flag to private members that can be Local if
     *  referred to exclusively from their owner's this-type. The Local flag is retracted in
     *  `isAccessibleFrom` if the access not from such a this-type.
     */
    def checkFlags(flags: FlagSet) =
      if (flags.isEmpty) flags
      else {
        val (ok, adapted, kind) = tree match {
          case tree: TypeDef => (flags.isTypeFlags, flags.toTypeFlags, "type")
          case _ => (flags.isTermFlags, flags.toTermFlags, "value")
        }
        def canBeLocal = tree match
          case tree: MemberDef => SymDenotations.canBeLocal(tree.name, flags)
          case _ => false
        if !ok then
          report.error(em"modifier(s) `${flags.flagsString}` incompatible with $kind definition", tree.srcPos)
        if adapted.is(Private) && canBeLocal then adapted | Local else adapted
      }

    /** Add moduleClass/sourceModule to completer if it is for a module val or class */
    def adjustIfModule(completer: LazyType, tree: MemberDef) =
      if (tree.mods.is(Module)) adjustModuleCompleter(completer, tree.name)
      else completer

    typr.println(i"creating symbol for $tree in ${ctx.mode}")

    /** Create new symbol or redefine existing symbol under lateCompile. */
    def createOrRefine[S <: Symbol](
        tree: MemberDef, name: Name, flags: FlagSet, owner: Symbol, infoFn: S => Type,
        symFn: (FlagSet, S => Type, Symbol) => S): Symbol = {
      val prev =
        if (lateCompile && ctx.owner.is(Package)) ctx.effectiveScope.lookup(name)
        else NoSymbol

      var flags1 = flags
      if name.isTypeName && Feature.ccEnabled then
        flags1 |= CaptureChecked
      var privateWithin = privateWithinClass(tree.mods)
      val effectiveOwner = owner.skipWeakOwner
      if (flags.is(Private) && effectiveOwner.is(Package)) {
        // If effective owner is a package p, widen private to private[p]
        flags1 = flags1 &~ PrivateLocal
        privateWithin = effectiveOwner
      }

      val sym =
        if (prev.exists) {
          prev.flags = flags1
          prev.info = infoFn(prev.asInstanceOf[S])
          prev.setPrivateWithin(privateWithin)
          prev
        }
        else symFn(flags1, infoFn, privateWithin)
      recordSym(sym, tree)
    }

    tree match {
      case tree: TypeDef if tree.isClassDef =>
        val flags = checkFlags(tree.mods.flags)
        val name = checkNoConflict(tree.name, flags.is(Private), tree.span).asTypeName
        val cls =
          createOrRefine[ClassSymbol](tree, name, flags, ctx.owner,
            cls => adjustIfModule(new ClassCompleter(cls, tree)(ctx), tree),
            newClassSymbol(ctx.owner, name, _, _, _, tree.nameSpan, ctx.compilationUnit.info))
        cls.completer.asInstanceOf[ClassCompleter].init()
        cls
      case tree: MemberDef =>
        var flags = checkFlags(tree.mods.flags)
        val name = checkNoConflict(tree.name, flags.is(Private), tree.span)
        tree match
          case tree: ValOrDefDef =>
            if tree.isInstanceOf[ValDef] && !flags.is(Param) && name.endsWith("_=") then
              report.error("Names of vals or vars may not end in `_=`", tree.namePos)
            if tree.unforcedRhs == EmptyTree
               && !flags.isOneOf(TermParamOrAccessor)
               && !tree.name.isConstructorName
            then
              flags |= Deferred
            if (tree.isInstanceOf[DefDef]) flags |= Method
            else if flags.isAllOf(EnumValue) && ctx.owner.isStaticOwner then flags |= JavaStatic
          case tree: TypeDef =>
            def analyzeRHS(rhs: Tree): Unit = rhs match
              case _: TypeBoundsTree | _: MatchTypeTree =>
                flags |= Deferred // Typedefs with Match rhs classify as abstract
              case LambdaTypeTree(_, body) =>
                analyzeRHS(body)
              case _ =>
                if rhs.isEmpty || flags.is(Opaque) then flags |= Deferred
            analyzeRHS(tree.rhs)

        // to complete a constructor, move one context further out -- this
        // is the context enclosing the class. Note that the context in which a
        // constructor is recorded and the context in which it is completed are
        // different: The former must have the class as owner (because the
        // constructor is owned by the class), the latter must not (because
        // constructor parameters are interpreted as if they are outside the class).
        // Don't do this for Java constructors because they need to see the import
        // of the companion object, and it is not necessary for them because they
        // have no implementation.
        val cctx = if (tree.name == nme.CONSTRUCTOR && !flags.is(JavaDefined)) ctx.outer else ctx

        val completer = tree match
          case tree: TypeDef => TypeDefCompleter(tree)(cctx)
          case _ => Completer(tree)(cctx)
        val info = adjustIfModule(completer, tree)
        createOrRefine[Symbol](tree, name, flags, ctx.owner, _ => info,
          (fs, _, pwithin) => newSymbol(ctx.owner, name, fs, info, pwithin, tree.nameSpan))
      case tree: Import =>
        recordSym(newImportSymbol(ctx.owner, Completer(tree)(ctx), tree.span), tree)
      case _ =>
        NoSymbol
    }
  }

   /** If `sym` exists, enter it in effective scope. Check that
    *  package members are not entered twice in the same run.
    */
  def enterSymbol(sym: Symbol)(using Context): Unit =
    // We do not enter Scala 2 macros defined in Scala 3 as they have an equivalent Scala 3 inline method.
    if sym.exists && !sym.isScala2MacroInScala3 then
      typr.println(s"entered: $sym in ${ctx.owner}")
      ctx.enter(sym)

  /** Create package if it does not yet exist. */
  private def createPackageSymbol(pid: RefTree)(using Context): Symbol = {
    val pkgOwner = pid match {
      case Ident(_) => if (ctx.owner eq defn.EmptyPackageClass) defn.RootClass else ctx.owner
      case Select(qual: RefTree, _) => createPackageSymbol(qual).moduleClass
    }
    val existing = pkgOwner.info.decls.lookup(pid.name)

    if (existing.is(Package) && (pkgOwner eq existing.owner)) {
      existing.moduleClass.denot match {
        case d: PackageClassDenotation =>
          // Remove existing members coming from a previous compilation of this file,
          // they are obsolete.
          d.unlinkFromFile(ctx.source.file)
        case _ =>
      }
      existing
    }
    else {
      /** If there's already an existing type, then the package is a dup of this type */
      val existingType = pkgOwner.info.decls.lookup(pid.name.toTypeName)
      if (existingType.exists) {
        report.error(PkgDuplicateSymbol(existingType), pid.srcPos)
        newCompletePackageSymbol(pkgOwner, (pid.name ++ "$_error_").toTermName).entered
      }
      else newCompletePackageSymbol(pkgOwner, pid.name.asTermName).entered
    }
  }

  /** Expand tree and store in `expandedTree` */
  def expand(tree: Tree)(using Context): Unit = {
    def record(expanded: Tree) =
      if (expanded `ne` tree) {
        typr.println(i"Expansion: $tree expands to $expanded")
        tree.pushAttachment(ExpandedTree, expanded)
      }
    tree match {
      case tree: DefTree    => record(desugar.defTree(tree))
      case tree: PackageDef => record(desugar.packageDef(tree))
      case tree: ExtMethods => record(desugar.extMethods(tree))
      case _                =>
    }
  }

  /** The expanded version of this tree, or tree itself if not expanded */
  def expanded(tree: Tree)(using Context): Tree = tree match {
    case _: DefTree | _: PackageDef | _: ExtMethods => tree.attachmentOrElse(ExpandedTree, tree)
    case _ => tree
  }

  /** For all class definitions `stat` in `xstats`: If the companion class is
    * not also defined in `xstats`, invalidate it by setting its info to
    * NoType.
    */
  def invalidateCompanions(pkg: Symbol, xstats: List[untpd.Tree])(using Context): Unit = {
    val definedNames = xstats collect { case stat: NameTree => stat.name }
    def invalidate(name: TypeName) =
      if (!(definedNames contains name)) {
        val member = pkg.info.decl(name).asSymDenotation
        if (member.isClass && !(member.is(Package))) member.markAbsent()
      }
    xstats foreach {
      case stat: TypeDef if stat.isClassDef =>
        invalidate(stat.name.moduleClassName)
      case _ =>
    }
  }

  /** Expand tree and create top-level symbols for statement and enter them into symbol table */
  def index(stat: Tree)(using Context): Context = {
    expand(stat)
    indexExpanded(stat)
  }

  /** Create top-level symbols for all statements in the expansion of this statement and
   *  enter them into symbol table
   */
  def indexExpanded(origStat: Tree)(using Context): Context = {
    def recur(stat: Tree): Context = stat match {
      case pcl: PackageDef =>
        val pkg = createPackageSymbol(pcl.pid)
        index(pcl.stats)(using ctx.fresh.setOwner(pkg.moduleClass))
        invalidateCompanions(pkg, Trees.flatten(pcl.stats map expanded))
        setDocstring(pkg, stat)
        ctx
      case imp: Import =>
        ctx.importContext(imp, createSymbol(imp))
      case mdef: DefTree =>
        val sym = createSymbol(mdef)
        enterSymbol(sym)
        setDocstring(sym, origStat)
        addEnumConstants(mdef, sym)
        ctx
      case stats: Thicket =>
        stats.toList.foreach(recur)
        ctx
      case _ =>
        ctx
    }
    recur(expanded(origStat))
  }

  /** Determines whether this field holds an enum constant. */
  def isEnumConstant(vd: ValDef)(using Context): Boolean =
    vd.mods.isAllOf(JavaEnumValue)

  /** Ensure that the first type in a list of parent types Ps points to a non-trait class.
   *  If that's not already the case, add one. The added class type CT is determined as follows.
   *  First, let C be the unique class such that
   *  - there is a parent P_i such that P_i derives from C, and
   *  - for every class D: If some parent P_j, j <= i derives from D, then C derives from D.
   *  Then, let CT be the smallest type which
   *  - has C as its class symbol, and
   *  - for all parents P_i: If P_i derives from C then P_i <:< CT.
   *
   * Tweak: It could be that at the point where the method is called, some superclass
   * is still missing its parents. Parents are set to Nil when completion starts and are
   * set to the actual parents later. If a superclass completes a subclass in one
   * of its parents, the parents of the superclass or some intervening class might
   * not yet be set. This situation can be detected by asking for the baseType of Any -
   * if that type does not exist, one of the base classes of this class misses its parents.
   * If this situation arises, the computation of the superclass might be imprecise.
   * For instance, in i12722.scala, the superclass of `IPersonalCoinOps` is computed
   * as `Object`, where `JsObject` would be correct. The problem cannot be solved locally,
   * but we detect the situaton and mark the superclass with a `@ProvisionalSuperClass`
   * annotation in this case. When typechecking the class, we then run ensureFirstIsClass
   * again and possibly improve the computed super class.
   * An alternatiev fix would compute superclasses at typer instead at completion. But
   * that breaks too many invariants. For instance, we rely on correct @Child annotations
   * after completion, and these in turn need the superclass.
   */
  def ensureFirstIsClass(cls: ClassSymbol, parents: List[Type])(using Context): List[Type] =

    def realClassParent(sym: Symbol): ClassSymbol =
      if !sym.isClass then defn.ObjectClass
      else if !sym.is(Trait) then sym.asClass
      else sym.info.parents match
        case parentRef :: _ => realClassParent(parentRef.typeSymbol)
        case nil => defn.ObjectClass

    def improve(candidate: ClassSymbol, parent: Type): ClassSymbol =
      val pcls = realClassParent(parent.classSymbol)
      if (pcls derivesFrom candidate) pcls else candidate

    parents match
      case p :: _ if p.classSymbol.isRealClass => parents
      case _ =>
        val pcls = parents.foldLeft(defn.ObjectClass)(improve)
        typr.println(i"ensure first is class $parents%, % --> ${parents map (_ baseType pcls)}%, %")
        val bases = parents.map(_.baseType(pcls))
        var first = TypeComparer.glb(defn.ObjectType :: bases)
        val isProvisional = parents.exists(!_.baseType(defn.AnyClass).exists)
        if isProvisional then
          typr.println(i"provisional superclass $first for $cls")
          first = AnnotatedType(first, Annotation(defn.ProvisionalSuperClassAnnot, cls.span))
        checkFeasibleParent(first, cls.srcPos, i" in inferred superclass $first") :: parents
  end ensureFirstIsClass

  /** Add child annotation for `child` to annotations of `cls`. The annotation
   *  is added at the correct insertion point, so that Child annotations appear
   *  in reverse order of their start positions.
   *  @pre `child` must have a position.
   */
  final def addChild(cls: Symbol, child: Symbol)(using Context): Unit = {
    val childStart = if (child.span.exists) child.span.start else -1
    def insertInto(annots: List[Annotation]): List[Annotation] =
      annots.find(_.symbol == defn.ChildAnnot) match {
        case Some(Annotation.Child(other)) if other.span.exists && childStart <= other.span.start =>
          if (child == other)
            annots // can happen if a class has several inaccessible children
          else {
            assert(childStart != other.span.start || child.source != other.source, i"duplicate child annotation $child / $other")
            val (prefix, otherAnnot :: rest) = annots.span(_.symbol != defn.ChildAnnot): @unchecked
            prefix ::: otherAnnot :: insertInto(rest)
          }
        case _ =>
          Annotation.Child(child, cls.span.startPos) :: annots
      }
    cls.annotations = insertInto(cls.annotations)
  }

  /** Add java enum constants */
  def addEnumConstants(mdef: DefTree, sym: Symbol)(using Context): Unit = mdef match {
    case vdef: ValDef if (isEnumConstant(vdef)) =>
      val enumClass = sym.owner.linkedClass
      if (!enumClass.is(Sealed)) enumClass.setFlag(Flags.AbstractSealed)
      addChild(enumClass, sym)
    case _ =>
  }

  def setDocstring(sym: Symbol, tree: Tree)(using Context): Unit = tree match {
    case t: MemberDef if t.rawComment.isDefined =>
      ctx.docCtx.foreach(_.addDocstring(sym, t.rawComment))
    case t: ExtMethods =>
      for meth <- t.methods.find(_.span.point == sym.span.point) do
        setDocstring(sym, meth)
    case _ => ()
  }

  /** Create top-level symbols for statements and enter them into symbol table
   *  @return A context that reflects all imports in `stats`.
   */
  def index(stats: List[Tree])(using Context): Context = {

    // module name -> (stat, moduleCls | moduleVal)
    val moduleClsDef = mutable.Map[TypeName, (Tree, TypeDef)]()
    val moduleValDef = mutable.Map[TermName, (Tree, ValDef)]()

    /** Remove the subtree `tree` from the expanded tree of `mdef` */
    def removeInExpanded(mdef: Tree, tree: Tree): Unit = {
      val Thicket(trees) = expanded(mdef): @unchecked
      mdef.putAttachment(ExpandedTree, Thicket(trees.filter(_ != tree)))
    }

    /** Transfer all references to `from` to `to` */
    def transferReferences(from: ValDef, to: ValDef): Unit = {
      val fromRefs = from.removeAttachment(References).getOrElse(Nil)
      val toRefs = to.removeAttachment(References).getOrElse(Nil)
      to.putAttachment(References, fromRefs ++ toRefs)
    }

    /** Merge the module class `modCls` in the expanded tree of `mdef` with the
     *  body and derived clause of the synthetic module class `fromCls`.
     */
    def mergeModuleClass(mdef: Tree, modCls: TypeDef, fromCls: TypeDef): TypeDef = {
      var res: TypeDef | Null = null
      val Thicket(trees) = expanded(mdef): @unchecked
      val merged = trees.map { tree =>
        if (tree == modCls) {
          val fromTempl = fromCls.rhs.asInstanceOf[Template]
          val modTempl = modCls.rhs.asInstanceOf[Template]
          res = cpy.TypeDef(modCls)(
            rhs = cpy.Template(modTempl)(
              derived = if (fromTempl.derived.nonEmpty) fromTempl.derived else modTempl.derived,
              body = fromTempl.body.filter {
                  case stat: DefDef => stat.name != nme.toString_
                    // toString should only be generated if explicit companion is missing
                  case _ => true
                } ++ modTempl.body))
          if (fromTempl.derived.nonEmpty) {
            if (modTempl.derived.nonEmpty)
              report.error(em"a class and its companion cannot both have `derives` clauses", mdef.srcPos)
            // `res` is inside a closure, so the flow-typing doesn't work here.
            res.uncheckedNN.putAttachment(desugar.DerivingCompanion, fromTempl.srcPos.startPos)
          }
          res.uncheckedNN
        }
        else tree
      }

      mdef.putAttachment(ExpandedTree, Thicket(merged))

      res.nn
    }

    /** Merge `fromCls` of `fromStat` into `toCls` of `toStat`
     *  if the former is synthetic and the latter not.
     *
     *  Note:
     *    1. `fromStat` and `toStat` could be the same stat
     *    2. `fromCls` and `toCls` are necessarily different
     */
    def mergeIfSynthetic(fromStat: Tree, fromCls: TypeDef, toStat: Tree, toCls: TypeDef): Unit =
      if (fromCls.mods.is(Synthetic) && !toCls.mods.is(Synthetic)) {
        removeInExpanded(fromStat, fromCls)
        val mcls = mergeModuleClass(toStat, toCls, fromCls)
        mcls.setMods(toCls.mods)
        moduleClsDef(fromCls.name) = (toStat, mcls)
      }

    /** Merge the definitions of a synthetic companion generated by a case class
     *  and the real companion, if both exist.
     */
    def mergeCompanionDefs() = {
      def valid(mdef: MemberDef): Boolean = mdef.mods.is(Module, butNot = Package)

      for (stat <- stats)
        expanded(stat) match {
          case Thicket(trees) => // companion object always expands to thickets
            trees.map {
              case mcls @ TypeDef(name, impl: Template) if valid(mcls) =>
                (moduleClsDef.get(name): @unchecked) match {
                  case Some((stat1, mcls1@TypeDef(_, impl1: Template))) =>
                    mergeIfSynthetic(stat, mcls, stat1, mcls1)
                    mergeIfSynthetic(stat1, mcls1, stat, mcls)
                  case None =>
                    moduleClsDef(name) = (stat, mcls)
                }

              case vdef @ ValDef(name, _, _) if valid(vdef) =>
                moduleValDef.get(name) match {
                  case Some((stat1, vdef1)) =>
                    if (vdef.mods.is(Synthetic) && !vdef1.mods.is(Synthetic)) {
                      transferReferences(vdef, vdef1)
                      removeInExpanded(stat, vdef)
                    }
                    else if (!vdef.mods.is(Synthetic) && vdef1.mods.is(Synthetic)) {
                      transferReferences(vdef1, vdef)
                      removeInExpanded(stat1, vdef1)
                      moduleValDef(name) = (stat, vdef)
                    }
                    else {
                      // double definition of objects or case classes, handled elsewhere
                    }
                  case None =>
                    moduleValDef(name) = (stat, vdef)
                }

              case _ =>
            }
          case _ =>
        }
    }

    val classDef  = mutable.Map[TypeName, TypeDef]()
    val moduleDef = mutable.Map[TypeName, TypeDef]()

    /** Create links between companion object and companion class.
     *  Populate `moduleDef` and `classDef` as a side effect.
     */
    def createCompanionLinks()(using Context): Unit = {

      def updateCache(cdef: TypeDef): Unit =
        if (cdef.isClassDef && !cdef.mods.is(Package))
          if (cdef.mods.is(ModuleClass)) moduleDef(cdef.name) = cdef
          else classDef(cdef.name) = cdef

      def createLinks(classTree: TypeDef, moduleTree: TypeDef)(using Context) = {
        val claz = ctx.effectiveScope.lookup(classTree.name)
        val modl = ctx.effectiveScope.lookup(moduleTree.name)
        modl.registerCompanion(claz)
        claz.registerCompanion(modl)
      }

      for (stat <- stats)
        expanded(stat) match {
          case cdef : TypeDef => updateCache(cdef)
          case Thicket(trees) =>
            trees.map {
              case cdef: TypeDef => updateCache(cdef)
              case _ =>
            }
          case _ =>
        }

      for (cdef @ TypeDef(name, _) <- classDef.values)
        moduleDef.getOrElse(name.moduleClassName, EmptyTree) match {
          case t: TypeDef =>
            createLinks(cdef, t)
          case EmptyTree =>
        }
    }

    /** If a top-level object or class has no companion in the current run, we
     *  enter a dummy companion (`denot.isAbsent` returns true) or constructor
     *  proxy in scope. This ensures that we never use a companion from a previous
     *  run or from thenclass path. See tests/pos/false-companion for an example
     *  where this matters.
     *  Also: We add constructor proxies for classes in some local scope, i.e.
     *  that are not members of other classes. Constructor proxies for member
     *  classes are added in addConstructorProxies.
     */
    def addAbsentCompanions()(using Context): Unit =
      if ctx.owner.isTerm then
        for case cdef @ TypeDef(className, _) <- classDef.values do
          val classSym = ctx.effectiveScope.lookup(className)
          val moduleName = className.toTermName
          if needsConstructorProxies(classSym) && ctx.effectiveScope.lookupEntry(moduleName) == null then
            enterSymbol(classConstructorCompanion(classSym.asClass))
      else if ctx.owner.is(PackageClass) then
        for case cdef @ TypeDef(moduleName, _) <- moduleDef.values do
          val moduleSym = ctx.effectiveScope.lookup(moduleName)
          if moduleSym.isDefinedInCurrentRun then
            val className = moduleName.stripModuleClassSuffix.toTypeName
            val classSym = ctx.effectiveScope.lookup(className)
            if !classSym.isDefinedInCurrentRun then
              val absentClassSymbol = newClassSymbol(ctx.owner, className, EmptyFlags, _ => NoType)
              enterSymbol(absentClassSymbol)

        for case cdef @ TypeDef(className, _) <- classDef.values do
          val classSym = ctx.effectiveScope.lookup(className.encode)
          if classSym.isDefinedInCurrentRun then
            val moduleName = className.toTermName
            val companionVals = ctx.effectiveScope.lookupAll(moduleName.encode)
            if companionVals.isEmpty && needsConstructorProxies(classSym) then
              enterSymbol(classConstructorCompanion(classSym.asClass))
            else
              for moduleSym <- companionVals do
                if moduleSym.is(Module) && !moduleSym.isDefinedInCurrentRun then
                  val companion =
                    if needsConstructorProxies(classSym) then
                      classConstructorCompanion(classSym.asClass)
                    else newModuleSymbol(
                      ctx.owner, moduleName, EmptyFlags, EmptyFlags, (_, _) => NoType)
                  enterSymbol(companion)
    end addAbsentCompanions

    stats.foreach(expand)
    mergeCompanionDefs()
    val ctxWithStats = stats.foldLeft(ctx)((ctx, stat) => indexExpanded(stat)(using ctx))
    inContext(ctxWithStats) {
      createCompanionLinks()
      addAbsentCompanions()
    }
    ctxWithStats
  }

  /** Parse the source and index symbols in the compilation unit's untpdTree
   *  while asserting the `lateCompile` flag. This will cause any old
   *  top-level symbol with the same fully qualified name as a newly created
   *  symbol to be replaced.
   *
   *  Will call the callback with an implementation of type checking
   *  That will set the tpdTree and root tree for the compilation unit.
   */
  def lateEnterUnit(typeCheck: Boolean)(typeCheckCB: (() => Unit) => Unit)(using Context) =
    val unit = ctx.compilationUnit

    /** Index symbols in unit.untpdTree with lateCompile flag = true */
    def lateEnter()(using Context): Context =
      val saved = lateCompile
      lateCompile = true
      try
        index(unit.untpdTree :: Nil)
      finally
        lateCompile = saved
        if !typeCheck then ctx.run.advanceLate()

    /** Set the tpdTree and root tree of the compilation unit */
    def lateTypeCheck()(using Context) =
      try
        unit.tpdTree = typer.typedExpr(unit.untpdTree)
        val phase = new transform.SetRootTree()
        phase.run
      finally
        if typeCheck then ctx.run.advanceLate()

    unit.untpdTree =
      if (unit.isJava) new JavaParser(unit.source).parse()
      else new Parser(unit.source).parse()

    atPhase(Phases.typerPhase) {
      inContext(PrepareInlineable.initContext(ctx)) {
        // inline body annotations are set in namer, capturing the current context
        // we need to prepare the context for inlining.
        lateEnter()
        if typeCheck then
          typeCheckCB { () =>
            lateTypeCheck()
          }
      }
    }
  end lateEnterUnit

  /** The type bound on wildcard imports of an import list, with special values
   *    Nothing  if no wildcard imports of this kind exist
   *    Any      if there are unbounded wildcard imports of this kind
   */
  def importBound(sels: List[untpd.ImportSelector], isGiven: Boolean)(using Context): Type =
    sels.foldLeft(defn.NothingType: Type) { (bound, sel) =>
      if sel.isWildcard && sel.isGiven == isGiven then
        if sel.bound.isEmpty then defn.AnyType
        else bound | typedAheadType(sel.bound).tpe
      else bound
    }

  def missingType(sym: Symbol, modifier: String)(using Context): Unit = {
    report.error(em"${modifier}type of implicit definition needs to be given explicitly", sym.srcPos)
    sym.resetFlag(GivenOrImplicit)
  }

  /** The completer of a symbol defined by a member def or import (except ClassSymbols) */
  class Completer(val original: Tree)(ictx: Context) extends LazyType with SymbolLoaders.SecondCompleter {

    protected def localContext(owner: Symbol): FreshContext = ctx.fresh.setOwner(owner).setTree(original)

    /** Stores the latest NotNullInfos (updated by `setNotNullInfos`) */
    private var myNotNullInfos: List[NotNullInfo] | Null = null

    /** The context with which this completer was created */
    given creationContext[Dummy_so_its_a_def]: Context =
      if myNotNullInfos == null then ictx else ictx.withNotNullInfos(myNotNullInfos.nn)

    // make sure testing contexts are not captured by completers
    assert(!ictx.reporter.isInstanceOf[ExploringReporter])

    def setNotNullInfos(infos: List[NotNullInfo]): Unit =
      myNotNullInfos = infos

    protected def typeSig(sym: Symbol): Type = original match
      case original: ValDef =>
        if (sym.is(Module)) moduleValSig(sym)
        else valOrDefDefSig(original, sym, Nil, identity)(using localContext(sym).setNewScope)
      case original: DefDef =>
        val typer1 = ctx.typer.newLikeThis(ctx.nestingLevel + 1)
        nestedTyper(sym) = typer1
        typer1.defDefSig(original, sym, this)(using localContext(sym).setTyper(typer1))
      case imp: Import =>
        try
          val expr1 = typedImportQualifier(imp, typedAheadExpr(_, _)(using ctx.withOwner(sym)))
          ImportType(expr1)
        catch case ex: CyclicReference =>
          typr.println(s"error while completing ${imp.expr}")
          throw ex

    final override def complete(denot: SymDenotation)(using Context): Unit = {
      if (Config.showCompletions && ctx.typerState != creationContext.typerState) {
        def levels(c: Context): Int =
          if (c.typerState eq creationContext.typerState) 0
          else if (c.outer.typerState == c.typerState) levels(c.outer)
          else levels(c.outer) + 1
        println(s"!!!completing ${denot.symbol.showLocated} in buried typerState, gap = ${levels(ctx)}")
      }
      val creationRunId = creationContext.runId
      if ctx.runId > creationRunId then
        assert(ctx.mode.is(Mode.Interactive), s"completing $denot in wrong run ${ctx.runId}, was created in $creationRunId")
        denot.info = UnspecifiedErrorType
      else
        try
          completeInCreationContext(denot)
          if (denot.isCompleted) registerIfChild(denot)
        catch
          case ex: CompilationUnit.SuspendException =>
            val completer = SuspendCompleter()
            denot.info = completer
            completer.complete(denot)
    }

    private var completedTypeParamSyms: List[TypeSymbol] | Null = null

    def setCompletedTypeParams(tparams: List[TypeSymbol]) =
      completedTypeParamSyms = tparams

    override def completerTypeParams(sym: Symbol)(using Context): List[TypeSymbol] =
      if completedTypeParamSyms != null then completedTypeParamSyms.uncheckedNN
      else Nil

    protected def addAnnotations(sym: Symbol): Unit = original match {
      case original: untpd.MemberDef =>
        lazy val annotCtx = annotContext(original, sym)
        for (annotTree <- original.mods.annotations) {
          val cls = typedAheadAnnotationClass(annotTree)(using annotCtx)
          if (cls eq sym)
            report.error(em"An annotation class cannot be annotated with iself", annotTree.srcPos)
          else {
            val ann = Annotation.deferred(cls)(typedAheadExpr(annotTree)(using annotCtx))
            sym.addAnnotation(ann)
          }
        }
      case _ =>
    }

    private def addInlineInfo(sym: Symbol) = original match {
      case original: untpd.DefDef if sym.isInlineMethod =>
        def rhsToInline(using Context): tpd.Tree =
          if !original.symbol.exists && !hasDefinedSymbol(original) then
            throw
              if sym.isCompleted then Inlines.MissingInlineInfo()
              else CyclicReference(sym)
          val mdef = typedAheadExpr(original).asInstanceOf[tpd.DefDef]
          PrepareInlineable.wrapRHS(original, mdef.tpt, mdef.rhs)
        PrepareInlineable.registerInlineInfo(sym, rhsToInline)(using localContext(sym))
      case _ =>
    }

    /** Invalidate `denot` by overwriting its info with `NoType` if
     *  `denot` is a compiler generated case class method that clashes
     *  with a user-defined method in the same scope with a matching type.
     */
    private def invalidateIfClashingSynthetic(denot: SymDenotation): Unit =

      def isJavaRecord(owner: Symbol) =
        owner.is(JavaDefined) && owner.derivesFrom(defn.JavaRecordClass)

      def isCaseClassOrCompanion(owner: Symbol) =
        owner.isClass && {
          if (owner.is(Module)) owner.linkedClass.is(CaseClass)
          else owner.is(CaseClass)
        }

      def definesMember =
        denot.owner.info.decls.lookupAll(denot.name).exists(alt =>
          alt != denot.symbol && alt.info.matchesLoosely(denot.info))

      def inheritsConcreteMember =
        denot.owner.asClass.info.parents.exists(parent =>
          parent.member(denot.name).hasAltWith(sd =>
            !sd.symbol.is(Deferred) && sd.matches(denot)))

      val isClashingSynthetic =
        denot.is(Synthetic, butNot = ConstructorProxy) &&
        (
          (desugar.isRetractableCaseClassMethodName(denot.name)
            && isCaseClassOrCompanion(denot.owner)
            && (definesMember || inheritsConcreteMember)
          )
          ||
          // remove synthetic constructor or method of a java Record if it clashes with a non-synthetic constructor
          (isJavaRecord(denot.owner)
            && denot.is(Method)
            && denot.owner.unforcedDecls.lookupAll(denot.name).exists(c => c != denot.symbol && c.info.matches(denot.info))
          )
        )

      if isClashingSynthetic then
        typr.println(i"invalidating clashing $denot in ${denot.owner}")
        denot.markAbsent()
    end invalidateIfClashingSynthetic

    /** If completed symbol is an enum value or a named class, register it as a child
     *  in all direct parent classes which are sealed.
     */
    def registerIfChild(denot: SymDenotation)(using Context): Unit = {
      val sym = denot.symbol

      def register(child: Symbol, parentCls: ClassSymbol) = {
        if (parentCls.is(Sealed))
          if ((child.isInaccessibleChildOf(parentCls) || child.isAnonymousClass) && !sym.hasAnonymousChild)
            addChild(parentCls, parentCls)
          else if (!parentCls.is(ChildrenQueried))
            addChild(parentCls, child)
          else
            report.error(em"""children of $parentCls were already queried before $sym was discovered.
                          |As a remedy, you could move $sym on the same nesting level as $parentCls.""",
                      child.srcPos)
      }

      if denot.isClass && !sym.isEnumAnonymClass && !sym.isRefinementClass then
        val child = if (denot.is(Module)) denot.sourceModule else denot.symbol
        denot.info.parents.foreach { parent => register(child, parent.classSymbol.asClass) }
      else if denot.is(CaseVal, butNot = MethodOrModule) then
        assert(denot.is(Enum), denot)
        denot.info.classSymbols.foreach { parent => register(denot.symbol, parent) }
      end if
    }

    /** Intentionally left without `implicit ctx` parameter. We need
     *  to pick up the context at the point where the completer was created.
     */
    def completeInCreationContext(denot: SymDenotation): Unit = {
      val sym = denot.symbol
      addAnnotations(sym)
      addInlineInfo(sym)
      denot.info = typeSig(sym)
      invalidateIfClashingSynthetic(denot)
      Checking.checkWellFormed(sym)
      denot.info = avoidPrivateLeaks(sym)
    }
  }

  class TypeDefCompleter(original: TypeDef)(ictx: Context)
  extends Completer(original)(ictx) with TypeParamsCompleter {
    private var myTypeParams: List[TypeSymbol] | Null = null
    private var nestedCtx: Context | Null = null
    assert(!original.isClassDef)

    /** If completion of the owner of the to be completed symbol has not yet started,
     *  complete the owner first and check again. This prevents cyclic references
     *  where we need to copmplete a type parameter that has an owner that is not
     *  yet completed. Test case is pos/i10967.scala.
     */
    override def needsCompletion(symd: SymDenotation)(using Context): Boolean =
      val owner = symd.owner
      !owner.exists
      || owner.is(Touched)
      || {
        owner.ensureCompleted()
        !symd.isCompleted
      }

    override def completerTypeParams(sym: Symbol)(using Context): List[TypeSymbol] =
      if myTypeParams == null then
        //println(i"completing type params of $sym in ${sym.owner}")
        nestedCtx = localContext(sym).setNewScope
        given Context = nestedCtx.uncheckedNN

        def typeParamTrees(tdef: Tree): List[TypeDef] = tdef match
          case TypeDef(_, original) =>
            original match
              case LambdaTypeTree(tparams, _) => tparams
              case original: DerivedFromParamTree => typeParamTrees(original.watched)
              case _ => Nil
          case _ => Nil

        val tparams = typeParamTrees(original)
        index(tparams)
        myTypeParams = tparams.map(symbolOfTree(_).asType)
        for param <- tparams do typedAheadExpr(param)
      end if
      myTypeParams.uncheckedNN
    end completerTypeParams

    override final def typeSig(sym: Symbol): Type =
      val tparamSyms = completerTypeParams(sym)(using ictx)
      given ctx: Context = nestedCtx.nn

      def abstracted(tp: TypeBounds): TypeBounds =
        HKTypeLambda.boundsFromParams(tparamSyms, tp)

      val dummyInfo1 = abstracted(TypeBounds.empty)
      sym.info = dummyInfo1
      sym.setFlag(Provisional)
        // Temporarily set info of defined type T to ` >: Nothing <: Any.
        // This is done to avoid cyclic reference errors for F-bounds.
        // This is subtle: `sym` has now an empty TypeBounds, but is not automatically
        // made an abstract type. If it had been made an abstract type, it would count as an
        // abstract type of its enclosing class, which might make that class an invalid
        // prefix. I verified this would lead to an error when compiling io.ClassPath.
        // A distilled version is in pos/prefix.scala.
        //
        // The scheme critically relies on an implementation detail of isRef, which
        // inspects a TypeRef's info, instead of simply dealiasing alias types.

      val isDerived = original.rhs.isInstanceOf[untpd.DerivedTypeTree]
      val rhs = original.rhs match {
        case LambdaTypeTree(_, body) => body
        case rhs => rhs
      }

      // For match types: approximate with upper bound while evaluating the rhs.
      val dummyInfo2 = rhs match {
        case MatchTypeTree(bound, _, _) if !bound.isEmpty =>
          abstracted(TypeBounds.upper(typedAheadType(bound).tpe))
        case _ =>
          dummyInfo1
      }
      sym.info = dummyInfo2

      // Treat the parameters of an upper type lambda bound on the RHS as non-variant.
      // E.g.   type F <: [X] =>> G   and   type F[X] <: G
      // are treated alike.
      def addVariances(tp: Type): Type = tp match
        case tp: TypeBounds =>
          def recur(tp: Type): Type = tp match
            case tp: HKTypeLambda if !tp.isDeclaredVarianceLambda =>
              val tp1 = tp.withVariances(tp.paramNames.map(alwaysInvariant))
              tp1.derivedLambdaType(resType = recur(tp1.resType))
            case tp => tp
          tp.derivedTypeBounds(tp.lo, recur(tp.hi))
        case _ =>
          tp

      val rhs1 = typedAheadType(rhs)
      val rhsBodyType: TypeBounds = addVariances(rhs1.tpe).toBounds
      val unsafeInfo = if (isDerived) rhsBodyType else abstracted(rhsBodyType)

      def opaqueToBounds(info: Type): Type =
        if sym.isOpaqueAlias then
          if info.typeParams.nonEmpty && info.hkResult.typeParams.nonEmpty then
            report.error(em"opaque type alias cannot have multiple type parameter lists", rhs.srcPos)
          sym.opaqueToBounds(info, rhs1, tparamSyms)
        else
          info

      if (isDerived) sym.info = unsafeInfo
      else {
        sym.info = NoCompleter
        sym.info = opaqueToBounds(checkNonCyclic(sym, unsafeInfo, reportErrors = true))
      }
      if sym.isOpaqueAlias then sym.typeRef.recomputeDenot() // make sure we see the new bounds from now on
      sym.resetFlag(Provisional)

      // Here we pay the price for the cavalier setting info to TypeBounds.empty above.
      // We need to compensate by reloading the denotation of references that might
      // still contain the TypeBounds.empty. If we do not do this, stdlib factories
      // fail with a bounds error in PostTyper.
      def ensureUpToDate(tref: TypeRef, outdated: Type) =
        if (tref.info == outdated && sym.info != outdated) tref.recomputeDenot()
      ensureUpToDate(sym.typeRef, dummyInfo1)
      if (dummyInfo2 `ne` dummyInfo1) ensureUpToDate(sym.typeRef, dummyInfo2)

      sym.info
    end typeSig
  }

  class ClassCompleter(cls: ClassSymbol, original: TypeDef)(ictx: Context) extends Completer(original)(ictx) {
    withDecls(newScope(using ictx))

    protected implicit val completerCtx: Context = localContext(cls)

    private var localCtx: Context = uninitialized

    /** info to be used temporarily while completing the class, to avoid cyclic references. */
    private var tempInfo: TempClassInfo | Null = null

    val TypeDef(name, impl @ Template(constr, _, self, _)) = original: @unchecked

    private val (params, rest): (List[Tree], List[Tree]) = impl.body.span {
      case td: TypeDef => td.mods.is(Param)
      case vd: ValDef => vd.mods.is(ParamAccessor)
      case _ => false
    }

    def init(): Context = index(params)

    /** The forwarders defined by export `exp`
     *  @param  pathMethod  If it exists, the symbol referenced by the path of an export
     *                      in an extension clause. That symbol is always a companion
     *                      extension method.
     */
    private def exportForwarders(exp: Export, pathMethod: Symbol)(using Context): List[tpd.MemberDef] =
      val buf = new mutable.ListBuffer[tpd.MemberDef]
      val Export(expr, selectors) = exp
      if expr.isEmpty then
        report.error(em"Export selector must have prefix and `.`", exp.srcPos)
        return Nil

      val (path, pathType) =
        if pathMethod.exists then
          val path = typedAhead(expr, _.withType(pathMethod.termRef))
          (path, pathMethod.info.finalResultType)
        else
          val path = typedAheadExpr(expr, AnySelectionProto)
          checkLegalExportPath(path, selectors)
          (path, path.tpe)
      lazy val wildcardBound = importBound(selectors, isGiven = false)
      lazy val givenBound = importBound(selectors, isGiven = true)

      val targets = mutable.Set[Name]()
      def canForward(mbr: SingleDenotation, alias: TermName): CanForward = {
        import CanForward.*
        val sym = mbr.symbol
        if !sym.isAccessibleFrom(pathType) then
          No("is not accessible")
        else if sym.isConstructor || sym.is(ModuleClass) || sym.is(Bridge) || sym.is(ConstructorProxy) || sym.isAllOf(JavaModule) then
          Skip
        else if cls.derivesFrom(sym.owner) && (sym.owner == cls || !sym.is(Deferred)) then
          No(i"is already a member of $cls")
        else if pathMethod.exists && mbr.isType then
          No("is a type, so it cannot be exported as extension method")
        else if pathMethod.exists && sym.is(ExtensionMethod) then
          No("is already an extension method, cannot be exported into another one")
        else if targets.contains(alias) then
          No(i"clashes with another export in the same export clause")
        else if sym.is(Override) || sym.is(JavaDefined) then
          // The tests above are used to avoid futile searches of `allOverriddenSymbols`.
          // Scala defined symbols can override concrete symbols only if declared override.
          // For Java defined symbols, this does not hold, so we have to search anyway.
          sym.allOverriddenSymbols.find(
            other => cls.derivesFrom(other.owner) && !other.is(Deferred)
          ) match
              case Some(other) => No(i"overrides ${other.showLocated}, which is already a member of $cls")
              case None => Yes
        else
          Yes
      }

      def foreachDefaultGetterOf(sym: TermSymbol, op: TermSymbol => Unit): Unit =
        var n = 0
        val methodName =
          if sym.name == nme.apply && sym.is(Synthetic) && sym.owner.companionClass.is(Case) then
            // The synthesized `apply` methods of case classes use the constructor's default getters
            nme.CONSTRUCTOR
          else sym.name
        for params <- sym.paramSymss; param <- params do
          if param.isTerm then
            if param.is(HasDefault) then
              val getterName = DefaultGetterName(methodName, n)
              val getter = pathType.member(getterName).symbol
              assert(getter.exists, i"$path does not have a default getter named $getterName")
              op(getter.asTerm)
            n += 1

      /** Add a forwarder with name `alias` or its type name equivalent to `mbr`,
        *  provided `mbr` is accessible and of the right implicit/non-implicit kind.
        */
      def addForwarder(alias: TermName, mbr: SingleDenotation, span: Span): Unit =

        def adaptForwarderParams(acc: List[List[tpd.Tree]], tp: Type, prefss: List[List[tpd.Tree]])
          : List[List[tpd.Tree]] = tp match
            case mt: MethodType
            if mt.paramInfos.nonEmpty && mt.paramInfos.last.isRepeatedParam =>
              // Note: in this branch we use the assumptions
              // that `prefss.head` corresponds to `mt.paramInfos` and
              // that `prefss.tail` corresponds to `mt.resType`
              val init :+ vararg = prefss.head: @unchecked
              val prefs = init :+ ctx.typeAssigner.seqToRepeated(vararg)
              adaptForwarderParams(prefs :: acc, mt.resType, prefss.tail)
            case mt: MethodOrPoly =>
              adaptForwarderParams(prefss.head :: acc, mt.resultType, prefss.tail)
            case _ =>
              acc.reverse ::: prefss

        if canForward(mbr, alias) == CanForward.Yes then
          val sym = mbr.symbol
          val hasDefaults = sym.hasDefaultParams // compute here to ensure HasDefaultParams and NoDefaultParams flags are set
          val forwarder =
            if mbr.isType then
              val forwarderName = checkNoConflict(alias.toTypeName, isPrivate = false, span)
              var target = pathType.select(sym)
              if target.typeParams.nonEmpty then
                target = target.etaExpand
              newSymbol(
                cls, forwarderName,
                MandatoryExportTypeFlags | (sym.flags & RetainedExportTypeFlags),
                TypeAlias(target),
                coord = span)
              // Note: This will always create unparameterzied aliases. So even if the original type is
              // a parameterized class, say `C[X]` the alias will read `type C = d.C`. We currently do
              // allow such type aliases. If we forbid them at some point (requiring the referred type to be
              // fully applied), we'd have to change the scheme here as well.
            else
              def refersToPrivate(tp: Type): Boolean = tp match
                case tp: TermRef => tp.termSymbol.is(Private) || refersToPrivate(tp.prefix)
                case _ => false
              val (maybeStable, mbrInfo) =
                if sym.isStableMember
                    && sym.isPublic
                    && pathType.isStable
                    && !refersToPrivate(pathType)
                then
                  (StableRealizable, ExprType(pathType.select(sym)))
                else
                  def addPathMethodParams(pathType: Type, info: Type): Type =
                    def defines(pt: Type, pname: Name): Boolean = pt match
                      case pt: MethodOrPoly =>
                        pt.paramNames.contains(pname) || defines(pt.resType, pname)
                      case _ =>
                        false
                    def avoidNameClashes(info: Type): Type = info match
                      case info: MethodOrPoly =>
                        info.derivedLambdaType(
                          paramNames = info.paramNames.mapConserve {
                            pname => if defines(pathType, pname) then pname.freshened else pname
                          },
                          resType = avoidNameClashes(info.resType))
                      case info =>
                        info
                    def wrap(pt: Type, info: Type): Type = pt match
                      case pt: MethodOrPoly =>
                        pt.derivedLambdaType(resType = wrap(pt.resType, info))
                      case _ =>
                        info
                    wrap(pathType, avoidNameClashes(info))

                  val mbrInfo =
                    if pathMethod.exists
                    then addPathMethodParams(pathMethod.info, mbr.info.widenExpr)
                    else mbr.info.ensureMethodic
                  (EmptyFlags, mbrInfo)
              var mbrFlags = MandatoryExportTermFlags | maybeStable | (sym.flags & RetainedExportTermFlags)
              if pathMethod.exists then mbrFlags |= ExtensionMethod
              val forwarderName = checkNoConflict(alias, isPrivate = false, span)
              newSymbol(cls, forwarderName, mbrFlags, mbrInfo, coord = span)

          forwarder.info = avoidPrivateLeaks(forwarder)
          forwarder.addAnnotations(sym.annotations.filterConserve { annot =>
            annot.symbol != defn.BodyAnnot
            && annot.symbol != defn.TailrecAnnot
            && annot.symbol != defn.MainAnnot
            && !annot.symbol.derivesFrom(defn.MacroAnnotationClass)
          })

          if forwarder.isType then
            buf += tpd.TypeDef(forwarder.asType).withSpan(span)
          else
            import tpd.*
            def extensionParamsCount(pt: Type): Int = pt match
              case pt: MethodOrPoly => 1 + extensionParamsCount(pt.resType)
              case _ => 0
            val ddef = tpd.DefDef(forwarder.asTerm, prefss => {
              val forwarderCtx = ctx.withOwner(forwarder)
              val (pathRefss, methRefss) = prefss.splitAt(extensionParamsCount(path.tpe.widen))
              val ref = path.appliedToArgss(pathRefss).select(sym.asTerm)
              val rhs = ref.appliedToArgss(adaptForwarderParams(Nil, sym.info, methRefss))
                .etaExpandCFT(using forwarderCtx)
              if forwarder.isInlineMethod then
                // Eagerly make the body inlineable. `registerInlineInfo` does this lazily
                // but it does not get evaluated during typer as the forwarder we are creating
                // is already typed.
                val inlinableRhs = PrepareInlineable.makeInlineable(rhs)(using forwarderCtx)
                PrepareInlineable.registerInlineInfo(forwarder, inlinableRhs)(using forwarderCtx)
                inlinableRhs
              else
                rhs
            })
            buf += ddef.withSpan(span)
            if hasDefaults then
              foreachDefaultGetterOf(sym.asTerm,
                getter => addForwarder(
                  getter.name.asTermName, getter.asSeenFrom(path.tpe), span))
      end addForwarder

      def addForwardersNamed(name: TermName, alias: TermName, span: Span): Unit =
        val size = buf.size
        val mbrs = List(name, name.toTypeName).flatMap(pathType.member(_).alternatives)
        mbrs.foreach(addForwarder(alias, _, span))
        if buf.size == size then
          val reason = mbrs.map(canForward(_, alias)).collect {
            case CanForward.No(whyNot) => i"\n$path.$name cannot be exported because it $whyNot"
          }.headOption.getOrElse("")
          report.error(em"""no eligible member $name at $path$reason""", ctx.source.atSpan(span))
        else
          targets += alias

      def addWildcardForwardersNamed(name: TermName, span: Span): Unit =
        List(name, name.toTypeName)
          .flatMap(pathType.memberBasedOnFlags(_, excluded = Private|Given|ConstructorProxy).alternatives)
          .foreach(addForwarder(name, _, span)) // ignore if any are not added

      def addWildcardForwarders(seen: List[TermName], span: Span): Unit =
        val nonContextual = mutable.HashSet(seen*)
        val fromCaseClass = pathType.widen.classSymbols.exists(_.is(Case))
        def isCaseClassSynthesized(mbr: Symbol) =
          fromCaseClass && defn.caseClassSynthesized.contains(mbr)
        for mbr <- pathType.membersBasedOnFlags(required = EmptyFlags, excluded = PrivateOrSynthetic) do
          if !mbr.symbol.isSuperAccessor
              // Scala 2 superaccessors have neither Synthetic nor Artfact set, so we
              // need to filter them out here (by contrast, Scala 3 superaccessors are Artifacts)
              // Symbols from base traits of case classes that will get synthesized implementations
              // at PostTyper are also excluded.
            && !isCaseClassSynthesized(mbr.symbol)
            && !mbr.symbol.name.is(DefaultGetterName)
              // default getters are exported with the members they belong to
          then
            val alias = mbr.name.toTermName
            if mbr.symbol.is(Given) then
              if !seen.contains(alias) && mbr.matchesImportBound(givenBound) then
                addForwarder(alias, mbr, span)
            else if !nonContextual.contains(alias) && mbr.matchesImportBound(wildcardBound) then
              nonContextual += alias
              addWildcardForwardersNamed(alias, span)

      def addForwarders(sels: List[untpd.ImportSelector], seen: List[TermName]): Unit = sels match
        case sel :: sels1 =>
          if sel.isWildcard then
            addWildcardForwarders(seen, sel.span)
          else
            if !sel.isUnimport then
              addForwardersNamed(sel.name, sel.rename, sel.span)
            addForwarders(sels1, sel.name :: seen)
        case _ =>

      /** Avoid a clash of export forwarder `forwarder` with other forwarders in `forwarders`.
       *  @return If `forwarder` clashes, a new leading forwarder and trailing forwarders list
       *          that avoids the clash according to the scheme described in `avoidClashes`.
       *          If there's no clash, the inputs as they are in a pair.
       */
      def avoidClashWith(forwarder: tpd.DefDef, forwarders: List[tpd.MemberDef]): (tpd.DefDef, List[tpd.MemberDef]) =
        def clashes(fwd1: Symbol, fwd2: Symbol) =
          fwd1.targetName == fwd2.targetName
          && erasure(fwd1.info).signature == erasure(fwd2.info).signature

        forwarders match
          case forwarders @ ((forwarder1: tpd.DefDef) :: forwarders1)
          if forwarder.name == forwarder1.name =>
            if clashes(forwarder.symbol, forwarder1.symbol) then
              val alt1 = tpd.methPart(forwarder.rhs).tpe
              val alt2 = tpd.methPart(forwarder1.rhs).tpe
              val cmp = alt1 match
                case alt1: TermRef => alt2 match
                  case alt2: TermRef => compare(alt1, alt2)
                  case _ => 0
                case _ => 0
              if cmp == 0 then
                report.error(
                  em"""Clashing exports: The exported
                      |     ${forwarder.rhs.symbol}: ${alt1.widen}
                      |and  ${forwarder1.rhs.symbol}: ${alt2.widen}
                      |have the same signature after erasure and overloading resolution could not disambiguate.""",
                  exp.srcPos)
              avoidClashWith(if cmp < 0 then forwarder1 else forwarder, forwarders1)
            else
              val (forwarder2, forwarders2) = avoidClashWith(forwarder, forwarders1)
              (forwarder2, forwarders.derivedCons(forwarder1, forwarders2))
          case _ =>
            (forwarder, forwarders)
      end avoidClashWith

      /** Avoid clashes of any two export forwarders in `forwarders`.
       *  A clash is if two forwarders f1 and f2 have the same name and signatures after erasure.
       *  We try to avoid a clash by dropping one of f1 and f2, keeping the one whose right hand
       *  side reference would be preferred by overloading resolution.
       *  If neither of f1 or f2 is preferred over the other, report an error.
       *
       *  The idea is that this simulates the hypothetical case where export forwarders
       *  are not generated and we treat an export instead more like an import where we
       *  expand the use site reference. Test cases in {neg,pos}/i14966.scala.
       *
       *  @pre Forwarders with the same name are consecutive in `forwarders`.
       */
      def avoidClashes(forwarders: List[tpd.MemberDef]): List[tpd.MemberDef] = forwarders match
        case forwarders @ (forwarder :: forwarders1) =>
          val (forwarder2, forwarders2) = forwarder match
            case forwarder: tpd.DefDef => avoidClashWith(forwarder, forwarders1)
            case _ => (forwarder, forwarders1)
          forwarders.derivedCons(forwarder2, avoidClashes(forwarders2))
        case Nil => forwarders

      addForwarders(selectors, Nil)
      val forwarders = avoidClashes(buf.toList)
      exp.pushAttachment(ExportForwarders, forwarders)
      forwarders
    end exportForwarders

    /** Add forwarders as required by the export statements in this class */
    private def processExports(using Context): Unit =

      def processExport(exp: Export, pathSym: Symbol)(using Context): Unit =
        for forwarder <- exportForwarders(exp, pathSym) do
          forwarder.symbol.entered

      def exportPathSym(path: Tree, ext: ExtMethods)(using Context): Symbol =
        def fail(msg: String): Symbol =
          report.error(em"export qualifier $msg", path.srcPos)
          NoSymbol
        path match
          case Ident(name) =>
            def matches(cand: Tree) = cand match
              case meth: DefDef => meth.name == name && meth.paramss.hasSameLengthAs(ext.paramss)
              case _ => false
            expanded(ext).toList.filter(matches) match
              case cand :: Nil => symbolOfTree(cand)
              case Nil => fail(i"$name is not a parameterless companion extension method")
              case _ => fail(i"$name cannot be overloaded")
          case _ =>
            fail("must be a simple reference to a companion extension method")

      def process(stats: List[Tree])(using Context): Unit = stats match
        case (stat: Export) :: stats1 =>
          processExport(stat, NoSymbol)
          process(stats1)
        case (stat: Import) :: stats1 =>
          process(stats1)(using ctx.importContext(stat, symbolOfTree(stat)))
        case (stat: ExtMethods) :: stats1 =>
          for case exp: Export <- stat.methods do
            val pathSym = exportPathSym(exp.expr, stat)
            if pathSym.exists then processExport(exp, pathSym)
          process(stats1)
        case stat :: stats1 =>
          process(stats1)
        case Nil =>

      def hasExport(stats: List[Tree]): Boolean = stats.exists {
        case _: Export => true
        case ExtMethods(_, stats1) => hasExport(stats1)
        case _ => false
      }
      // Do a quick scan whether we need to process at all. This avoids creating
      // import contexts for nothing.
      if hasExport(rest) then
        process(rest)
    end processExports

    /** Ensure constructor is completed so that any parameter accessors
     *  which have type trees deriving from its parameters can be
     *  completed in turn. Note that parent types access such parameter
     *  accessors, that's why the constructor needs to be completed before
     *  the parent types are elaborated.
     */
    def completeConstructor(denot: SymDenotation): Unit = {
      if (tempInfo != null) // Constructor has been completed already
        return

      addAnnotations(denot.symbol)

      val selfInfo: TypeOrSymbol =
        if (self.isEmpty) NoType
        else if (cls.is(Module)) {
          val moduleType = cls.owner.thisType select sourceModule
          if (self.name == nme.WILDCARD) moduleType
          else recordSym(
            newSymbol(cls, self.name, self.mods.flags, moduleType, coord = self.span),
            self)
        }
        else createSymbol(self)

      val savedInfo = denot.infoOrCompleter
      denot.info = new TempClassInfo(cls.owner.thisType, cls, decls, selfInfo)

      localCtx = completerCtx.inClassContext(selfInfo)

      index(constr)
      index(rest)(using localCtx)

      symbolOfTree(constr).info.stripPoly match // Completes constr symbol as a side effect
        case mt: MethodType if cls.is(Case) && mt.isParamDependent =>
          // See issue #8073 for background
          report.error(
              em"""Implementation restriction: case classes cannot have dependencies between parameters""",
              cls.srcPos)
        case _ =>

      tempInfo = denot.asClass.classInfo.integrateOpaqueMembers.asInstanceOf[TempClassInfo]
      denot.info = savedInfo
    }

    /** The type signature of a ClassDef with given symbol */
    override def completeInCreationContext(denot: SymDenotation): Unit = {
      val parents = impl.parents

      /* The type of a parent constructor. Types constructor arguments
       * only if parent type contains uninstantiated type parameters.
       */
      def parentType(parent: untpd.Tree)(using Context): Type =

        def typedParentApplication(parent: untpd.Tree): Type =
          val (core, targs) = stripApply(parent) match
            case TypeApply(core, targs) => (core, targs)
            case core => (core, Nil)
          core match
            case Select(New(tpt), nme.CONSTRUCTOR) =>
              val targs1 = targs map (typedAheadType(_))
              val ptype = typedAheadType(tpt).tpe appliedTo targs1.tpes
              if (ptype.typeParams.isEmpty) ptype
              else
                if (denot.is(ModuleClass) && denot.sourceModule.isOneOf(GivenOrImplicit))
                  missingType(denot.symbol, "parent ")(using creationContext)
                fullyDefinedType(typedAheadExpr(parent).tpe, "class parent", parent.srcPos)
            case _ =>
              UnspecifiedErrorType.assertingErrorsReported

        def typedParentType(tree: untpd.Tree): tpd.Tree =
          val parentTpt = typer.typedType(parent, AnyTypeConstructorProto)
          val ptpe = parentTpt.tpe.dealias.etaCollapse
          if ptpe.typeParams.nonEmpty
              && ptpe.underlyingClassRef(refinementOK = false).exists
          then
            // Try to infer type parameters from a synthetic application.
            // This might yield new info if implicit parameters are resolved.
            // A test case is i16778.scala.
            val app = untpd.Apply(untpd.Select(untpd.New(parentTpt), nme.CONSTRUCTOR), Nil)
            typedParentApplication(app)
            app.getAttachment(TypedAhead).getOrElse(parentTpt)
          else
            parentTpt

        if parent.isType then typedAhead(parent, typedParentType).tpe
        else typedParentApplication(parent)
      end parentType

      /** Check parent type tree `parent` for the following well-formedness conditions:
       *  (1) It must be a class type with a stable prefix (unless `isJava`) (@see checkClassTypeWithStablePrefix)
       *  (2) If may not derive from itself
       *  (3) The class is not final
       *  (4) If the class is sealed, it is defined in the same compilation unit as the current class
       *      (unless defined in Java. See JEP-409)
       *
       * @param isJava  If true, the parent type is in Java mode, and we do not require a stable prefix
       */
      def checkedParentType(parent: untpd.Tree, isJava: Boolean): Type = {
        val ptype = parentType(parent)(using completerCtx.superCallContext).dealiasKeepAnnots
        if (cls.isRefinementClass) ptype
        else {
          val pt = checkClassType(ptype, parent.srcPos,
              traitReq = parent ne parents.head, stablePrefixReq = !isJava)
          if (pt.derivesFrom(cls)) {
            val addendum = parent match {
              case Select(qual: Super, _) if Feature.migrateTo3 =>
                "\n(Note that inheriting a class of the same name is no longer allowed)"
              case _ => ""
            }
            report.error(CyclicInheritance(cls, addendum), parent.srcPos)
            defn.ObjectType
          }
          else {
            val pclazz = pt.typeSymbol
            if pclazz.is(Final) then
              report.error(ExtendFinalClass(cls, pclazz), cls.srcPos)
            else if pclazz.isEffectivelySealed && pclazz.associatedFile != cls.associatedFile then
              if pclazz.is(Sealed) && !pclazz.is(JavaDefined) then
                report.error(UnableToExtendSealedClass(pclazz), cls.srcPos)
              else if sourceVersion.isAtLeast(future) then
                checkFeature(nme.adhocExtensions,
                  i"Unless $pclazz is declared 'open', its extension in a separate file",
                  cls.topLevelClass,
                  parent.srcPos)
            pt
          }
        }
      }

      /** If `parents` contains references to traits that have supertraits with implicit parameters
       *  add those supertraits in linearization order unless they are already covered by other
       *  parent types. For instance, in
       *
       *    class A
       *    trait B(using I) extends A
       *    trait C extends B
       *    class D extends A, C
       *
       *  the class declaration of `D` is augmented to
       *
       *    class D extends A, B, C
       *
       *  so that an implicit `I` can be passed to `B`. See i7613.scala for more examples.
       */
      def addUsingTraits(parents: List[Type]): List[Type] =
        lazy val existing = parents.map(_.classSymbol).toSet
        def recur(parents: List[Type]): List[Type] = parents match
          case parent :: parents1 =>
            val psym = parent.classSymbol
            val addedTraits =
              if psym.is(Trait) then
                psym.asClass.baseClasses.tail.iterator
                  .takeWhile(_.is(Trait))
                  .filter(p =>
                    p.primaryConstructor.info.takesImplicitParams
                    && !cls.superClass.isSubClass(p)
                    && !existing.contains(p))
                  .toList.reverse
              else Nil
            addedTraits.map(parent.baseType) ::: parent :: recur(parents1)
          case nil =>
            Nil
        if cls.isRealClass then recur(parents) else parents
      end addUsingTraits

      completeConstructor(denot)
      denot.info = tempInfo.nn

      val parentTypes = defn.adjustForTuple(cls, cls.typeParams,
        defn.adjustForBoxedUnit(cls,
          addUsingTraits(
            locally:
              val isJava = ctx.isJava
              ensureFirstIsClass(cls, parents.map(checkedParentType(_, isJava)))
          )
        )
      )
      typr.println(i"completing $denot, parents = $parents%, %, parentTypes = $parentTypes%, %")

      if (impl.derived.nonEmpty) {
        val (derivingClass, derivePos) = original.removeAttachment(desugar.DerivingCompanion) match {
          case Some(pos) => (cls.companionClass.orElse(cls).asClass, pos)
          case None => (cls, impl.srcPos.startPos)
        }
        val deriver = new Deriver(derivingClass, derivePos)(using localCtx)
        deriver.enterDerived(impl.derived)
        original.putAttachment(AttachedDeriver, deriver)
      }

      denot.info = tempInfo.nn.finalized(parentTypes)
      tempInfo = null // The temporary info can now be garbage-collected

      Checking.checkWellFormed(cls)
      if (isDerivedValueClass(cls)) cls.setFlag(Final)
      cls.info = avoidPrivateLeaks(cls)
      cls.baseClasses.foreach(_.invalidateBaseTypeCache()) // we might have looked before and found nothing
      cls.invalidateMemberCaches() // we might have checked for a member when parents were not known yet.
      cls.setNoInitsFlags(parentsKind(parents), untpd.bodyKind(rest))
      cls.setStableConstructor()
      processExports(using localCtx)
      defn.patchStdLibClass(cls)
      addConstructorProxies(cls)
    }
  }

  /** Possible actions to perform when deciding on a forwarder for a member */
  private enum CanForward:
    case Yes
    case No(whyNot: String)
    case Skip  // for members that have never forwarders

  class SuspendCompleter extends LazyType, SymbolLoaders.SecondCompleter {

    final override def complete(denot: SymDenotation)(using Context): Unit =
      denot.resetFlag(Touched) // allow one more completion
      ctx.compilationUnit.suspend()
  }

  /** Typecheck `tree` during completion using `typed`, and remember result in TypedAhead map */
  def typedAhead(tree: Tree, typed: untpd.Tree => tpd.Tree)(using Context): tpd.Tree = {
    val xtree = expanded(tree)
    xtree.getAttachment(TypedAhead) match {
      case Some(ttree) => ttree
      case none =>
        val ttree = typed(tree)
        if !ttree.isEmpty then xtree.putAttachment(TypedAhead, ttree)
        ttree
    }
  }

  def typedAheadType(tree: Tree, pt: Type = WildcardType)(using Context): tpd.Tree =
    typedAhead(tree, typer.typedType(_, pt))

  def typedAheadExpr(tree: Tree, pt: Type = WildcardType)(using Context): tpd.Tree =
    typedAhead(tree, typer.typedExpr(_, pt))

  def typedAheadAnnotationClass(tree: Tree)(using Context): Symbol = tree match
    case Apply(fn, _) => typedAheadAnnotationClass(fn)
    case TypeApply(fn, _) => typedAheadAnnotationClass(fn)
    case Select(qual, nme.CONSTRUCTOR) => typedAheadAnnotationClass(qual)
    case New(tpt) => typedAheadType(tpt).tpe.classSymbol
    case TypedSplice(_) =>
      val sym = tree.symbol
      if sym.isConstructor then sym.owner else sym

  /** Enter and typecheck parameter list */
  def completeParams(params: List[MemberDef])(using Context): Unit = {
    index(params)
    for (param <- params) typedAheadExpr(param)
  }

  /** The signature of a module valdef.
   *  This will compute the corresponding module class TypeRef immediately
   *  without going through the defined type of the ValDef. This is necessary
   *  to avoid cyclic references involving imports and module val defs.
   */
  def moduleValSig(sym: Symbol)(using Context): Type = {
    val clsName = sym.name.moduleClassName
    val cls = ctx.effectiveScope.lookupAll(clsName)
      .find(_.is(ModuleClass))
      .getOrElse(newStubSymbol(ctx.owner, clsName).assertingErrorsReported)
    ctx.owner.thisType.select(clsName, cls)
  }

  /** The type signature of a ValDef or DefDef
   *  @param mdef     The definition
   *  @param sym      Its symbol
   *  @param paramFn  A wrapping function that produces the type of the
   *                  defined symbol, given its final return type
   */
  def valOrDefDefSig(mdef: ValOrDefDef, sym: Symbol, paramss: List[List[Symbol]], paramFn: Type => Type)(using Context): Type = {

    def inferredType = inferredResultType(mdef, sym, paramss, paramFn, WildcardType)

    val tptProto = mdef.tpt match {
      case _: untpd.DerivedTypeTree =>
        WildcardType
      case TypeTree() =>
        checkMembersOK(inferredType, mdef.srcPos)

      // We cannot rely on `typedInLambdaTypeTree` since the computed type might not be fully-defined.
      case InLambdaTypeTree(/*isResult =*/ true, tpFun) =>
        // A lambda has at most one type parameter list followed by exactly one term parameter list.
        val tpe = (paramss: @unchecked) match
          case TypeSymbols(tparams) :: TermSymbols(vparams) :: Nil => tpFun(tparams, vparams)
          case TermSymbols(vparams) :: Nil => tpFun(Nil, vparams)
        val rhsCtx = prepareRhsCtx(ctx.fresh, paramss)
        if (isFullyDefined(tpe, ForceDegree.none)) tpe
        else typedAheadExpr(mdef.rhs, tpe)(using rhsCtx).tpe

      case TypedSplice(tpt: TypeTree) if !isFullyDefined(tpt.tpe, ForceDegree.none) =>
        mdef match {
          case mdef: DefDef if mdef.name == nme.ANON_FUN =>
            // This case applies if the closure result type contains uninstantiated
            // type variables. In this case, constrain the closure result from below
            // by the parameter-capture-avoiding type of the body.
            val rhsType = typedAheadExpr(mdef.rhs, tpt.tpe).tpe

            // The following part is important since otherwise we might instantiate
            // the closure result type with a plain functon type that refers
            // to local parameters. An example where this happens in `dependent-closures.scala`
            // If the code after `val rhsType` is commented out, this file fails pickling tests.
            // AVOIDANCE TODO: Follow up why this happens, and whether there
            // are better ways to achieve this. It would be good if we could get rid of this code.
            // It seems at least partially redundant with the nesting level checking on TypeVar
            // instantiation.
            // It turns out if we fix levels on instantiation we still need this code.
            // Examples that fail otherwise are pos/scalaz-redux.scala and pos/java-futures.scala.
            // So fixing levels at instantiation avoids the soundness problem but apparently leads
            // to type inference problems since it comes too late.
            if !Config.checkLevelsOnConstraints then
              val termParams = paramss.collect { case TermSymbols(vparams) => vparams }.flatten
              val hygienicType = TypeOps.avoid(rhsType, termParams)
              if (!hygienicType.isValueType || !(hygienicType <:< tpt.tpe))
                report.error(
                  em"""return type ${tpt.tpe} of lambda cannot be made hygienic
                      |it is not a supertype of the hygienic type $hygienicType""",
                  mdef.srcPos)
            //println(i"lifting $rhsType over $termParamss -> $hygienicType = ${tpt.tpe}")
            //println(TypeComparer.explained { implicit ctx => hygienicType <:< tpt.tpe })
          case _ =>
        }
        WildcardType
      case _ =>
        WildcardType
    }
    val mbrTpe = paramFn(checkSimpleKinded(typedAheadType(mdef.tpt, tptProto)).tpe)
    if (ctx.explicitNulls && mdef.mods.is(JavaDefined))
      JavaNullInterop.nullifyMember(sym, mbrTpe, mdef.mods.isAllOf(JavaEnumValue))
    else mbrTpe
  }

  /** The type signature of a DefDef with given symbol */
  def defDefSig(ddef: DefDef, sym: Symbol, completer: Namer#Completer)(using Context): Type = {
    // Beware: ddef.name need not match sym.name if sym was freshened!
    val isConstructor = sym.name == nme.CONSTRUCTOR

    // The following 3 lines replace what was previously just completeParams(tparams).
    // But that can cause bad bounds being computed, as witnessed by
    // tests/pos/paramcycle.scala. The problematic sequence is this:
    //   0. Class constructor gets completed.
    //   1. Type parameter CP of constructor gets completed
    //   2. As a first step CP's bounds are set to Nothing..Any.
    //   3. CP's real type bound demands the completion of corresponding type parameter DP
    //      of enclosing class.
    //   4. Type parameter DP has a rhs a DerivedFromParam tree, as installed by
    //      desugar.classDef
    //   5. The completion of DP then copies the current bounds of CP, which are still Nothing..Any.
    //   6. The completion of CP finishes installing the real type bounds.
    // Consequence: CP ends up with the wrong bounds!
    // To avoid this we always complete type parameters of a class before the type parameters
    // of the class constructor, but after having indexed the constructor parameters (because
    // indexing is needed to provide a symbol to copy for DP's completion.
    // With the patch, we get instead the following sequence:
    //   0. Class constructor gets completed.
    //   1. Class constructor parameter CP is indexed.
    //   2. Class parameter DP starts completion.
    //   3. Info of CP is computed (to be copied to DP).
    //   4. CP is completed.
    //   5. Info of CP is copied to DP and DP is completed.
    index(ddef.leadingTypeParams)
    if (isConstructor) sym.owner.typeParams.foreach(_.ensureCompleted())
    val completedTypeParams =
      for tparam <- ddef.leadingTypeParams yield typedAheadExpr(tparam).symbol
    if completedTypeParams.forall(_.isType) then
      completer.setCompletedTypeParams(completedTypeParams.asInstanceOf[List[TypeSymbol]])
    ddef.trailingParamss.foreach(completeParams)
    val paramSymss = normalizeIfConstructor(ddef.paramss.nestedMap(symbolOfTree), isConstructor)
    sym.setParamss(paramSymss)
    def wrapMethType(restpe: Type): Type =
      instantiateDependent(restpe, paramSymss)
      methodType(paramSymss, restpe, ddef.mods.is(JavaDefined))
    if isConstructor then
      // set result type tree to unit, but take the current class as result type of the symbol
      typedAheadType(ddef.tpt, defn.UnitType)
      wrapMethType(effectiveResultType(sym, paramSymss))
    else
      valOrDefDefSig(ddef, sym, paramSymss, wrapMethType)
  }

  def inferredResultType(
      mdef: ValOrDefDef,
      sym: Symbol,
      paramss: List[List[Symbol]],
      paramFn: Type => Type,
      fallbackProto: Type
    )(using Context): Type =

    /** A type for this definition that might be inherited from elsewhere:
     *  If this is a setter parameter, the corresponding getter type.
     *  If this is a class member, the conjunction of all result types
     *  of overridden methods.
     *  NoType if neither case holds.
     */
    val inherited =
      if (sym.owner.isTerm) NoType
      else
        // TODO: Look only at member of supertype instead?
        lazy val schema = paramFn(WildcardType)
        val site = sym.owner.thisType
        val bcs = sym.owner.info.baseClasses
        if bcs.isEmpty then
          assert(ctx.reporter.errorsReported)
          NoType
        else bcs.tail.foldLeft(NoType: Type) { (tp, cls) =>
          def instantiatedResType(info: Type, paramss: List[List[Symbol]]): Type = info match
            case info: PolyType =>
              paramss match
                case TypeSymbols(tparams) :: paramss1 if info.paramNames.length == tparams.length =>
                  instantiatedResType(info.instantiate(tparams.map(_.typeRef)), paramss1)
                case _ =>
                  NoType
            case info: MethodType =>
              paramss match
                case TermSymbols(vparams) :: paramss1 if info.paramNames.length == vparams.length =>
                  instantiatedResType(info.instantiate(vparams.map(_.termRef)), paramss1)
                case _ =>
                  NoType
            case _ =>
              if paramss.isEmpty then info.widenExpr
              else NoType

          val iRawInfo =
            cls.info.nonPrivateDecl(sym.name).matchingDenotation(site, schema, sym.targetName).info
          val iResType = instantiatedResType(iRawInfo, paramss).asSeenFrom(site, cls)
          if (iResType.exists)
            typr.println(i"using inherited type for ${mdef.name}; raw: $iRawInfo, inherited: $iResType")
          tp & iResType
        }
    end inherited

    /** If this is a default getter, the type of the corresponding method parameter,
     *  otherwise NoType.
     */
    def defaultParamType = sym.name match
      case DefaultGetterName(original, idx) =>
        val meth: Denotation =
          if (original.isConstructorName && (sym.owner.is(ModuleClass)))
            sym.owner.companionClass.info.decl(nme.CONSTRUCTOR)
          else
            ctx.defContext(sym).denotNamed(original)
        def paramProto(paramss: List[List[Type]], idx: Int): Type = paramss match {
          case params :: paramss1 =>
            if (idx < params.length) params(idx)
            else paramProto(paramss1, idx - params.length)
          case nil =>
            NoType
        }
        val defaultAlts = meth.altsWith(_.hasDefaultParams)
        if (defaultAlts.length == 1)
          paramProto(defaultAlts.head.info.widen.paramInfoss, idx)
        else
          NoType
      case _ =>
        NoType

    /** The expected type for a default argument. This is normally the `defaultParamType`
     *  with references to internal parameters replaced by wildcards. This replacement
     *  makes it possible that the default argument can have a more specific type than the
     *  parameter. For instance, we allow
     *
     *      class C[A](a: A) { def copy[B](x: B = a): C[B] = C(x) }
     *
     *  However, if the default parameter type is a context function type, we
     *  have to make sure that wildcard types do not leak into the implicitly
     *  generated closure's result type. Test case is pos/i12019.scala. If there
     *  would be a leakage with the wildcard approximation, we pick the original
     *  default parameter type as expected type.
     */
    def expectedDefaultArgType =
      val originalTp = defaultParamType
      val approxTp = withMode(Mode.TypevarsMissContext):
        // assert TypevarsMissContext so that TyperState does not leak into approximation
        // We approximate precisely because we want to unlink the type variable. Test case is i18795.scala.
        wildApprox(originalTp)
      approxTp.stripPoly match
        case atp @ defn.ContextFunctionType(_, resType)
        if !defn.isNonRefinedFunction(atp) // in this case `resType` is lying, gives us only the non-dependent upper bound
            || resType.existsPart(_.isInstanceOf[WildcardType], StopAt.Static, forceLazy = false) =>
          originalTp
        case _ =>
          approxTp

    var rhsCtx = ctx.fresh.addMode(Mode.InferringReturnType)
    if sym.isInlineMethod then rhsCtx = rhsCtx.addMode(Mode.InlineableBody)
    if sym.is(ExtensionMethod) then rhsCtx = rhsCtx.addMode(Mode.InExtensionMethod)
    rhsCtx = prepareRhsCtx(rhsCtx, paramss)

    def typedAheadRhs(pt: Type) =
      PrepareInlineable.dropInlineIfError(sym,
        typedAheadExpr(mdef.rhs, pt)(using rhsCtx))

    def rhsType =
      // For default getters, we use the corresponding parameter type as an
      // expected type but we run it through `wildApprox` to allow default
      // parameters like in `def mkList[T](value: T = 1): List[T]`.
      val defaultTp = defaultParamType
      val pt = inherited.orElse(expectedDefaultArgType).orElse(fallbackProto).widenExpr
      val tp = typedAheadRhs(pt).tpe
      if (defaultTp eq pt) && (tp frozen_<:< defaultTp) then
        // When possible, widen to the default getter parameter type to permit a
        // larger choice of overrides (see `default-getter.scala`).
        // For justification on the use of `@uncheckedVariance`, see
        // `default-getter-variance.scala`.
        AnnotatedType(defaultTp, Annotation(defn.UncheckedVarianceAnnot, sym.span))
      else
        // don't strip @uncheckedVariance annot for default getters
        TypeOps.simplify(tp.widenTermRefExpr,
            if defaultTp.exists then TypeOps.SimplifyKeepUnchecked() else null) match
          case ctp: ConstantType if sym.isInlineVal => ctp
          case tp => TypeComparer.widenInferred(tp, pt, widenUnions = true)

    // Replace aliases to Unit by Unit itself. If we leave the alias in
    // it would be erased to BoxedUnit.
    def dealiasIfUnit(tp: Type) = if (tp.isRef(defn.UnitClass)) defn.UnitType else tp

    def cookedRhsType = dealiasIfUnit(rhsType).deskolemized
    def lhsType = fullyDefinedType(cookedRhsType, "right-hand side", mdef.srcPos)
    //if (sym.name.toString == "y") println(i"rhs = $rhsType, cooked = $cookedRhsType")
    if (inherited.exists)
      if sym.isInlineVal then lhsType else inherited
    else {
      if (sym.is(Implicit))
        mdef match {
          case _: DefDef => missingType(sym, "result ")
          case _: ValDef if sym.owner.isType => missingType(sym, "")
          case _ =>
        }
      lhsType orElse WildcardType
    }
  end inferredResultType

  /** Prepare a GADT-aware context used to type the RHS of a ValOrDefDef. */
  def prepareRhsCtx(rhsCtx: FreshContext, paramss: List[List[Symbol]])(using Context): FreshContext =
    val typeParams = paramss.collect { case TypeSymbols(tparams) => tparams }.flatten
    if typeParams.nonEmpty then
      // we'll be typing an expression from a polymorphic definition's body,
      // so we must allow constraining its type parameters
      // compare with typedDefDef, see tests/pos/gadt-inference.scala
      rhsCtx.setFreshGADTBounds
      rhsCtx.gadtState.addToConstraint(typeParams)
    rhsCtx
}
