package dotty.tools.dotc.interactive

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.config.Printers.interactiv
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.{Name, TermName}
import dotty.tools.dotc.core.NameKinds.SimpleNameKind
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.core.Scopes.*
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol, defn, newSymbol}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.TypeError
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.core.Types.{AppliedType, ExprType, MethodOrPoly, NameFilter, NoType, RefinedType, TermRef, Type, TypeProxy}
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.typer.Implicits.SearchSuccess
import dotty.tools.dotc.typer.Inferencing
import dotty.tools.dotc.util.Chars
import dotty.tools.dotc.util.SourcePosition

import scala.collection.mutable
import scala.util.control.NonFatal
import dotty.tools.dotc.core.ContextOps.localContext
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Constants

/**
 * One of the results of a completion query.
 *
 * @param label         The label of this completion result, or the text that this completion result
 *                      should insert in the scope where the completion request happened.
 * @param description   The description of this completion result: the fully qualified name for
 *                      types, or the type for terms.
 * @param symbols       The symbols that are matched by this completion result.
 */
case class Completion(label: String, description: String, symbols: List[Symbol])

object Completion:

  /** Get possible completions from tree at `pos`
   *
   *  @return offset and list of symbols for possible completions
   */
  def completions(pos: SourcePosition)(using Context): (Int, List[Completion]) =
    val tpdPath = Interactive.pathTo(ctx.compilationUnit.tpdTree, pos.span)
    val completionContext = Interactive.contextOfPath(tpdPath).withPhase(Phases.typerPhase)
    inContext(completionContext):
      val untpdPath = Interactive.resolveTypedOrUntypedPath(tpdPath, pos)
      val mode = completionMode(untpdPath, pos)
      val rawPrefix = completionPrefix(untpdPath, pos)
      val completions = rawCompletions(pos, mode, rawPrefix, tpdPath, untpdPath)

      postProcessCompletions(untpdPath, completions, rawPrefix)

  /** Get possible completions from tree at `pos`
   *  This method requires manually computing the mode, prefix and paths.
   *
   *  @return completion map of name to list of denotations
   */
  def rawCompletions(
    pos: SourcePosition,
    mode: Mode,
    rawPrefix: String,
    tpdPath: List[tpd.Tree],
    untpdPath: List[untpd.Tree]
  )(using Context): CompletionMap =
    val adjustedPath = typeCheckExtensionConstructPath(untpdPath, tpdPath, pos)
    computeCompletions(pos, mode, rawPrefix, adjustedPath)

  /**
   * Inspect `path` to determine what kinds of symbols should be considered.
   *
   * If the path starts with:
   *  - a `RefTree`, then accept symbols of the same kind as its name;
   *  - a renaming import, and the cursor is on the renamee, accept both terms and types;
   *  - an import, accept both terms and types;
   *
   * Otherwise, provide no completion suggestion.
   */
  def completionMode(path: List[untpd.Tree], pos: SourcePosition): Mode = path match
    case GenericImportSelector(sel) =>
      if sel.imported.span.contains(pos.span) then Mode.ImportOrExport // import scala.@@
      else if sel.isGiven && sel.bound.span.contains(pos.span) then Mode.ImportOrExport
      else Mode.None // import scala.{util => u@@}
    case GenericImportOrExport(_) => Mode.ImportOrExport | Mode.Scope // import TrieMa@@
    case untpd.Literal(Constants.Constant(_: String)) :: _ => Mode.Term | Mode.Scope // literal completions
    case (ref: untpd.RefTree) :: _ =>
      val maybeSelectMembers = if ref.isInstanceOf[untpd.Select] then Mode.Member else Mode.Scope

      if (ref.name.isTermName) Mode.Term | maybeSelectMembers
      else if (ref.name.isTypeName) Mode.Type | maybeSelectMembers
      else Mode.None

    case _ => Mode.None

  /** When dealing with <errors> in varios palces we check to see if they are
   *  due to incomplete backticks. If so, we ensure we get the full prefix
   *  including the backtick.
   *
   * @param content The source content that we'll check the positions for the prefix
   * @param start The start position we'll start to look for the prefix at
   * @param end The end position we'll look for the prefix at
   * @return Either the full prefix including the ` or an empty string
   */
  private def checkBacktickPrefix(content: Array[Char], start: Int, end: Int): String =
    content.lift(start) match
      case Some(char) if char == '`' =>
        content.slice(start, end).mkString
      case _ =>
        ""

  /**
   * Inspect `path` to determine the completion prefix. Only symbols whose name start with the
   * returned prefix should be considered.
   */
  def completionPrefix(path: List[untpd.Tree], pos: SourcePosition)(using Context): String =
    def fallback: Int =
      var i = pos.point - 1
      while i >= 0 && Character.isUnicodeIdentifierPart(pos.source.content()(i)) do i -= 1
      i + 1

    path match
      case GenericImportSelector(sel) =>
        if sel.isGiven then completionPrefix(sel.bound :: Nil, pos)
        else if sel.isWildcard then pos.source.content()(pos.point - 1).toString
        else completionPrefix(sel.imported :: Nil, pos)

      // Foo.`se<TAB> will result in Select(Ident(Foo), <error>)
      case (select: untpd.Select) :: _ if select.name == nme.ERROR =>
        checkBacktickPrefix(select.source.content(), select.nameSpan.start, select.span.end)

      // import scala.util.chaining.`s<TAB> will result in a Ident(<error>)
      case (ident: untpd.Ident) :: _ if ident.name == nme.ERROR =>
        checkBacktickPrefix(ident.source.content(), ident.span.start, ident.span.end)

      case (tree: untpd.RefTree) :: _ if tree.name != nme.ERROR =>
        tree.name.toString.take(pos.span.point - tree.span.point)

      case _ => pos.source.content.slice(fallback, pos.point).mkString


  end completionPrefix

  private object GenericImportSelector:
    def unapply(path: List[untpd.Tree]): Option[untpd.ImportSelector] =
      path match
        case untpd.Ident(_) :: (sel: untpd.ImportSelector) :: _ => Some(sel)
        case (sel: untpd.ImportSelector) :: _ => Some(sel)
        case _ => None

  private object GenericImportOrExport:
    def unapply(path: List[untpd.Tree]): Option[untpd.ImportOrExport] =
      path match
        case untpd.Ident(_) :: (importOrExport: untpd.ImportOrExport) :: _ => Some(importOrExport)
        case (importOrExport: untpd.ImportOrExport) :: _ => Some(importOrExport)
        case _ => None

  /** Inspect `path` to determine the offset where the completion result should be inserted. */
  def completionOffset(untpdPath: List[untpd.Tree]): Int =
    untpdPath match
      case (ref: untpd.RefTree) :: _ => ref.span.point
      case _ => 0

  /** Handle case when cursor position is inside extension method construct.
   *  The extension method construct is then desugared into methods, and consturct parameters
   *  are no longer a part of a typed tree, but instead are prepended to method parameters.
   *
   *  @param untpdPath The typed or untyped path to the tree that is being completed
   *  @param tpdPath The typed path that will be returned if no extension method construct is found
   *  @param pos The cursor position
   *
   *  @return Typed path to the parameter of the extension construct if found or tpdPath
   */
  private def typeCheckExtensionConstructPath(
    untpdPath: List[untpd.Tree], tpdPath: List[tpd.Tree], pos: SourcePosition
  )(using Context): List[tpd.Tree] =
    untpdPath.collectFirst:
      case untpd.ExtMethods(paramss, _) =>
        val enclosingParam = paramss.flatten.find(_.span.contains(pos.span))
        enclosingParam.map: param =>
          ctx.typer.index(paramss.flatten)
          val typedEnclosingParam = ctx.typer.typed(param)
          Interactive.pathTo(typedEnclosingParam, pos.span)
    .flatten.getOrElse(tpdPath)

  private def computeCompletions(
    pos: SourcePosition, mode: Mode, rawPrefix: String, adjustedPath: List[tpd.Tree]
  )(using Context): CompletionMap =
    val hasBackTick = rawPrefix.headOption.contains('`')
    val prefix = if hasBackTick then rawPrefix.drop(1) else rawPrefix
    val completer = new Completer(mode, prefix, pos)

    val result = adjustedPath match
      // Ignore synthetic select from `This` because in code it was `Ident`
      // See example in dotty.tools.languageserver.CompletionTest.syntheticThis
      case tpd.Select(qual @ tpd.This(_), _) :: _ if qual.span.isSynthetic      => completer.scopeCompletions
      case tpd.Select(qual, _) :: _               if qual.typeOpt.hasSimpleKind => completer.selectionCompletions(qual)
      case tpd.Select(qual, _) :: _                                             => Map.empty
      case (tree: tpd.ImportOrExport) :: _                                      => completer.directMemberCompletions(tree.expr)
      case _                                                                    => completer.scopeCompletions

    interactiv.println(i"""completion info with pos    = $pos,
                          |                     prefix = ${completer.prefix},
                          |                     term   = ${completer.mode.is(Mode.Term)},
                          |                     type   = ${completer.mode.is(Mode.Type)},
                          |                     scope  = ${completer.mode.is(Mode.Scope)},
                          |                     member = ${completer.mode.is(Mode.Member)}""")

    result

  def postProcessCompletions(path: List[untpd.Tree], completions: CompletionMap, rawPrefix: String)(using Context): (Int, List[Completion]) =
    val describedCompletions = describeCompletions(completions)
    val hasBackTick = rawPrefix.headOption.contains('`')
    val backtickedCompletions =
      describedCompletions.map(completion => backtickCompletions(completion, hasBackTick))

    interactiv.println(i"""completion resutls = $backtickedCompletions%, %""")

    val offset = completionOffset(path)
    (offset, backtickedCompletions)

  def backtickCompletions(completion: Completion, hasBackTick: Boolean) =
    if hasBackTick || needsBacktick(completion.label) then
      completion.copy(label = s"`${completion.label}`")
    else
      completion

  // This borrows from Metals, which itself borrows from Ammonite. This uses
  // the same approach, but some of the utils that already exist in Dotty.
  // https://github.com/scalameta/metals/blob/main/mtags/src/main/scala/scala/meta/internal/mtags/KeywordWrapper.scala
  // https://github.com/com-lihaoyi/Ammonite/blob/73a874173cd337f953a3edc9fb8cb96556638fdd/amm/util/src/main/scala/ammonite/util/Model.scala
  private def needsBacktick(s: String) =
    val chunks = s.split("_", -1).nn

    val validChunks = chunks.zipWithIndex.forall { case (chunk, index) =>
      chunk.nn.forall(Chars.isIdentifierPart) ||
      (chunk.nn.forall(Chars.isOperatorPart) &&
        index == chunks.length - 1 &&
        !(chunks.lift(index - 1).contains("") && index - 1 == 0))
    }

    val validStart =
      Chars.isIdentifierStart(s(0)) || chunks(0).nn.forall(Chars.isOperatorPart)

    val valid = validChunks && validStart && !keywords.contains(s)

    !valid
  end needsBacktick

  private lazy val keywords = Tokens.keywords.map(Tokens.tokenString)

  /**
   * Return the list of code completions with descriptions based on a mapping from names to the denotations they refer to.
   * If several denotations share the same name, each denotation will be transformed into a separate completion item.
   */
  def describeCompletions(completions: CompletionMap)(using Context): List[Completion] =
    for
      (name, denots) <- completions.toList
      denot <- denots
    yield
      Completion(name.show, description(denot), List(denot.symbol))

  def description(denot: SingleDenotation)(using Context): String =
    if denot.isType then denot.symbol.showFullName
    else denot.info.widenTermRefExpr.show

  /** Include in completion sets only symbols that
   *   1. is not absent (info is not NoType)
   *   2. are not a primary constructor,
   *   3. have an existing source symbol,
   *   4. are the module class in case of packages,
   *   5. are mutable accessors, to exclude setters for `var`,
   *   6. symbol is not a package object
   *   7. symbol is not an artifact of the compiler
   *   8. symbol is not a constructor proxy module when in type completion mode
   *   9. have same term/type kind as name prefix given so far
   */
  def isValidCompletionSymbol(sym: Symbol, completionMode: Mode)(using Context): Boolean =
    sym.exists &&
    !sym.isAbsent() &&
    !sym.isPrimaryConstructor &&
    sym.sourceSymbol.exists &&
    (!sym.is(Package) || sym.is(ModuleClass)) &&
    !sym.isAllOf(Mutable | Accessor) &&
    !sym.isPackageObject &&
    !sym.is(Artifact) &&
    !(completionMode.is(Mode.Type) && sym.isAllOf(ConstructorProxyModule)) &&
    (
         (completionMode.is(Mode.Term) && (sym.isTerm || sym.is(ModuleClass))
      || (completionMode.is(Mode.Type) && (sym.isType || sym.isStableMember)))
    )

  given ScopeOrdering(using Context): Ordering[Seq[SingleDenotation]] with
    val order =
      List(defn.ScalaPredefModuleClass, defn.ScalaPackageClass, defn.JavaLangPackageClass)

    override def compare(x: Seq[SingleDenotation], y: Seq[SingleDenotation]): Int =
      val owner0 = x.headOption.map(_.symbol.effectiveOwner).getOrElse(NoSymbol)
      val owner1 = y.headOption.map(_.symbol.effectiveOwner).getOrElse(NoSymbol)

      order.indexOf(owner0) - order.indexOf(owner1)

  /** Computes code completions depending on the context in which completion is requested
   *  @param mode    Should complete names of terms, types or both
   *  @param prefix  The prefix that all suggested completions should start with
   *  @param pos     Cursor position where completion was requested
   *
   *  For the results of all `xyzCompletions` methods term names and type names are always treated as different keys in the same map
   *  and they never conflict with each other.
   */
  class Completer(val mode: Mode, val prefix: String, pos: SourcePosition):
    /** Completions for terms and types that are currently in scope:
     *  the members of the current class, local definitions and the symbols that have been imported,
     *  recursively adding completions from outer scopes.
     *  In case a name is ambiguous, no completions are returned for it.
     *  This mimics the logic for deciding what is ambiguous used by the compiler.
     *  In general in case of a name clash symbols introduced in more deeply nested scopes
     *  have higher priority and shadow previous definitions with the same name although:
     *  - imports with the same level of nesting cause an ambiguity if they are in the same name space
     *  - members and local definitions with the same level of nesting are allowed for overloading
     *  - an import is ignored if there is a local definition or a member introduced in the same scope
     *    (even if the import follows it syntactically)
     *  - a more deeply nested import shadowing a member or a local definition causes an ambiguity
     */
    def scopeCompletions(using context: Context): CompletionMap =

      /** Temporary data structure representing denotations with the same name introduced in a given scope
       *  as a member of a type, by a local definition or by an import clause
       */
      case class ScopedDenotations private (denots: Seq[SingleDenotation], ctx: Context)
      object ScopedDenotations:
        def apply(denots: Seq[SingleDenotation], ctx: Context, includeFn: SingleDenotation => Boolean): ScopedDenotations =
          ScopedDenotations(denots.filter(includeFn), ctx)

      val mappings = collection.mutable.Map.empty[Name, List[ScopedDenotations]].withDefaultValue(List.empty)
      def addMapping(name: Name, denots: ScopedDenotations) =
        mappings(name) = mappings(name) :+ denots

      ctx.outersIterator.foreach { case ctx @ given Context =>
        if ctx.isImportContext then
          importedCompletions.foreach { (name, denots) =>
            addMapping(name, ScopedDenotations(denots, ctx, include(_, name)))
          }
        else if ctx.owner.isClass then
          accessibleMembers(ctx.owner.thisType)
            .groupByName.foreach { (name, denots) =>
              addMapping(name, ScopedDenotations(denots, ctx, include(_, name)))
            }
        else if ctx.scope ne EmptyScope then
          ctx.scope.toList.filter(symbol => include(symbol, symbol.name))
            .flatMap(_.alternatives)
            .groupByName.foreach { (name, denots) =>
              addMapping(name, ScopedDenotations(denots, ctx, include(_, name)))
            }
      }

      var resultMappings = Map.empty[Name, Seq[SingleDenotation]]

      mappings.foreach { (name, denotss) =>
        val first = denotss.head

        // import a.c
        def isSingleImport =  denotss.length < 2
        // import a.C
        // locally {  import b.C }
        def isImportedInDifferentScope = first.ctx.scope ne denotss(1).ctx.scope
        // import a.C
        // import a.C
        def isSameSymbolImportedDouble = denotss.forall(_.denots == first.denots)

        // https://scala-lang.org/files/archive/spec/3.4/02-identifiers-names-and-scopes.html
        // import java.lang.*
        // {
        //   import scala.*
        //   {
        //     import Predef.*
        //     { /* source */ }
        //   }
        // }
        def notConflictingWithDefaults = // is imported symbol
          denotss.filterNot(_.denots.exists(denot => Interactive.isImportedByDefault(denot.symbol))).size <= 1

        denotss.find(!_.ctx.isImportContext) match {
          // most deeply nested member or local definition if not shadowed by an import
          case Some(local) if local.ctx.scope == first.ctx.scope =>
            resultMappings += name -> local.denots

          case None if isSingleImport || isImportedInDifferentScope || isSameSymbolImportedDouble =>
            resultMappings += name -> first.denots
          case None if notConflictingWithDefaults =>
            val ordered = denotss.map(_.denots).sorted
            resultMappings += name -> ordered.head
          case _ =>
        }
      }

      resultMappings
    end scopeCompletions

    /** Widen only those types which are applied or are exactly nothing
     */
    def widenQualifier(qual: tpd.Tree)(using Context): tpd.Tree =
      qual.typeOpt.widenDealias match
        case widenedType if widenedType.isExactlyNothing => qual.withType(widenedType)
        case appliedType: AppliedType => qual.withType(appliedType)
        case _ => qual

    /** Completions for selections from a term.
     *  Direct members take priority over members from extensions
     *  and so do members from extensions over members from implicit conversions
     */
    def selectionCompletions(qual: tpd.Tree)(using Context): CompletionMap =
      val adjustedQual = widenQualifier(qual)

      implicitConversionMemberCompletions(adjustedQual) ++
        extensionCompletions(adjustedQual) ++
        directMemberCompletions(adjustedQual)

    /** Completions for members of `qual`'s type.
     *  These include inherited definitions but not members added by extensions or implicit conversions
     */
    def directMemberCompletions(qual: tpd.Tree)(using Context): CompletionMap =
      if qual.typeOpt.isExactlyNothing then
        Map.empty
      else
        accessibleMembers(qual.typeOpt).groupByName

    /** Completions introduced by imports directly in this context.
     *  Completions from outer contexts are not included.
     */
    private def importedCompletions(using Context): CompletionMap =
      val imp = ctx.importInfo

      if imp == null then
        Map.empty
      else
        def fromImport(name: Name, nameInScope: Name): Seq[(Name, SingleDenotation)] =
          imp.site.member(name).alternatives
            .collect { case denot if include(denot, nameInScope) => nameInScope -> denot }

        val givenImports = imp.importedImplicits
          .map { ref => (ref.implicitName: Name, ref.underlyingRef.denot.asSingleDenotation) }
          .filter((name, denot) => include(denot, name))
          .groupByName

        val wildcardMembers =
          if imp.selectors.exists(_.imported.name == nme.WILDCARD) then
            val denots = accessibleMembers(imp.site)
              .filter(mbr => !mbr.symbol.is(Given) && !imp.excluded.contains(mbr.name.toTermName))
            denots.groupByName
          else
            Map.empty

        val explicitMembers =
          val importNamesInScope = imp.forwardMapping.toList.map(_._2)
          val duplicatedNames = importNamesInScope.diff(importNamesInScope.distinct)
          val discardedNames = duplicatedNames ++ imp.excluded
          imp.reverseMapping.toList
            .filter { (nameInScope, _) => !discardedNames.contains(nameInScope) }
            .flatMap { (nameInScope, original) =>
              fromImport(original, nameInScope) ++
              fromImport(original.toTypeName, nameInScope.toTypeName)
            }.toSeq.groupByName

        givenImports ++ wildcardMembers ++ explicitMembers
    end importedCompletions

    /** Completions from implicit conversions including old style extensions using implicit classes */
    private def implicitConversionMemberCompletions(qual: tpd.Tree)(using Context): CompletionMap =

      def tryToInstantiateTypeVars(conversionTarget: SearchSuccess): Type =
        try
          val typingCtx = ctx.fresh
          inContext(typingCtx):
            val methodRefTree = tpd.ref(conversionTarget.ref, needLoad = false)
            val convertedTree = ctx.typer.typedAheadExpr(untpd.Apply(untpd.TypedSplice(methodRefTree), untpd.TypedSplice(qual) :: Nil))
            Inferencing.fullyDefinedType(convertedTree.tpe, "", pos)
        catch
          case error => conversionTarget.tree.tpe // fallback to not fully defined type

      if qual.typeOpt.isExactlyNothing || qual.typeOpt.isNullType then
        Map.empty
      else
        implicitConversionTargets(qual)(using ctx.fresh.setExploreTyperState())
          .flatMap { conversionTarget => accessibleMembers(tryToInstantiateTypeVars(conversionTarget)) }
          .toSeq
          .groupByName

    /** Completions from extension methods */
    private def extensionCompletions(qual: tpd.Tree)(using Context): CompletionMap =
      def asDefLikeType(tpe: Type): Type = tpe match
        case _: MethodOrPoly => tpe
        case _ => ExprType(tpe)

      def tryApplyingReceiverToExtension(termRef: TermRef): Option[SingleDenotation] =
        ctx.typer.tryApplyingExtensionMethod(termRef, qual)
          .map { tree =>
            val tpe = asDefLikeType(tree.typeOpt.dealias)
            termRef.denot.asSingleDenotation.mapInfo(_ => tpe)
          }

      def extractMemberExtensionMethods(types: Seq[Type]): Seq[(TermRef, TermName)] =
        object DenotWithMatchingName:
          def unapply(denot: SingleDenotation): Option[(SingleDenotation, TermName)] =
            denot.name match
              case name: TermName if include(denot, name) => Some((denot, name))
              case _ => None

        types.flatMap { tp =>
          val tpe = tp.widenExpr
          tpe.membersBasedOnFlags(required = ExtensionMethod, excluded = EmptyFlags)
            .collect { case DenotWithMatchingName(denot, name) => TermRef(tpe, denot.symbol) -> name }
        }

      // There are four possible ways for an extension method to be applicable

      // 1. The extension method is visible under a simple name, by being defined or inherited or imported in a scope enclosing the reference.
      val termCompleter = new Completer(Mode.Term, prefix, pos)
      val extMethodsInScope = termCompleter.scopeCompletions.toList.flatMap:
        case (name, denots) => denots.collect:
          case d: SymDenotation if d.isTerm && d.termRef.symbol.is(Extension) => (d.termRef, name.asTermName)

      // 2. The extension method is a member of some given instance that is visible at the point of the reference.
      val givensInScope = ctx.implicits.eligible(defn.AnyType).map(_.implicitRef.underlyingRef)
      val extMethodsFromGivensInScope = extractMemberExtensionMethods(givensInScope)

      // 3. The reference is of the form r.m and the extension method is defined in the implicit scope of the type of r.
      val implicitScopeCompanions = ctx.run.nn.implicitScope(qual.typeOpt).companionRefs.showAsList
      val extMethodsFromImplicitScope = extractMemberExtensionMethods(implicitScopeCompanions)

      // 4. The reference is of the form r.m and the extension method is defined in some given instance in the implicit scope of the type of r.
      val givensInImplicitScope = implicitScopeCompanions.flatMap(_.membersBasedOnFlags(required = GivenVal, excluded = EmptyFlags)).map(_.info)
      val extMethodsFromGivensInImplicitScope = extractMemberExtensionMethods(givensInImplicitScope)

      val availableExtMethods = extMethodsFromGivensInImplicitScope ++ extMethodsFromImplicitScope ++ extMethodsFromGivensInScope ++ extMethodsInScope
      val extMethodsWithAppliedReceiver = availableExtMethods.flatMap {
        case (termRef, termName) =>
          if termRef.symbol.is(ExtensionMethod) && !qual.typeOpt.isBottomType then
            tryApplyingReceiverToExtension(termRef)
              .map(denot => termName -> denot)
          else None
      }
      extMethodsWithAppliedReceiver.groupByName

    /** Include in completion sets only symbols that
     *   1. match the filter method,
     *   2. satisfy [[Completion.isValidCompletionSymbol]]
     */
    private def include(denot: SingleDenotation, nameInScope: Name)(using Context): Boolean =
      nameInScope.startsWith(prefix) &&
      completionsFilter(NoType, nameInScope) &&
      isValidCompletionSymbol(denot.symbol, mode)

    private def extractRefinements(site: Type)(using Context): Seq[SingleDenotation] =
      site match
        case RefinedType(parent, name, info) =>
          val flags = info match
            case _: (ExprType | MethodOrPoly) => Method
            case _ => EmptyFlags
          val symbol = newSymbol(owner = NoSymbol, name, flags, info)
          val denot = SymDenotation(symbol, NoSymbol, name, flags, info)
          denot +: extractRefinements(parent)
        case tp: TypeProxy => extractRefinements(tp.superType)
        case _ => List.empty

    /** @param site The type to inspect.
     *  @return The members of `site` that are accessible and pass the include filter.
     */
    private def accessibleMembers(site: Type)(using Context): Seq[SingleDenotation] = {
      def appendMemberSyms(name: Name, buf: mutable.Buffer[SingleDenotation]): Unit =
        try
          val member = site.member(name)
          if member.symbol.is(ParamAccessor) && !member.symbol.isAccessibleFrom(site) then
            buf ++= site.nonPrivateMember(name).alternatives
          else
            buf ++= member.alternatives
        catch
          case ex: TypeError =>

      val members = site.memberDenots(completionsFilter, appendMemberSyms).collect {
        case mbr if include(mbr, mbr.name)
                    && mbr.symbol.isAccessibleFrom(site) => mbr
      }
      val refinements = extractRefinements(site).filter(mbr => include(mbr, mbr.name))

      members ++ refinements
    }

    /**
     * Given `qual` of type T, finds all the types S such that there exists an implicit conversion
     * from T to S. It then applies conversion method for proper type parameter resolution.
     *
     * @param qual The argument to which the implicit conversion should be applied.
     * @return The set of types after `qual` implicit conversion.
     */
    private def implicitConversionTargets(qual: tpd.Tree)(using Context): Set[SearchSuccess] = {
      val typer = ctx.typer
      val conversions = new typer.ImplicitSearch(defn.AnyType, qual, pos.span).allImplicits
      conversions.map(_.tree.typeOpt)

      interactiv.println(i"implicit conversion targets considered: ${conversions.toList}%, %")
      conversions
    }

    /** Filter for names that should appear when looking for completions. */
    private object completionsFilter extends NameFilter:
      def apply(pre: Type, name: Name)(using Context): Boolean =
        !name.isConstructorName && name.toTermName.info.kind == SimpleNameKind
      def isStable = true

    extension (denotations: Seq[SingleDenotation])
      def groupByName(using Context): CompletionMap = denotations.groupBy(_.name)

    extension [N <: Name](namedDenotations: Seq[(N, SingleDenotation)])
      @annotation.targetName("groupByNameTupled")
      def groupByName: CompletionMap = namedDenotations.groupMap((name, denot) => name)((name, denot) => denot)

  private type CompletionMap = Map[Name, Seq[SingleDenotation]]

  /**
   * The completion mode: defines what kinds of symbols should be included in the completion
   * results.
   */
  class Mode(val bits: Int) extends AnyVal:
    def is(other: Mode): Boolean = (bits & other.bits) == other.bits
    def |(other: Mode): Mode = new Mode(bits | other.bits)

  object Mode:
    /** No symbol should be included */
    val None: Mode = new Mode(0)

    /** Term symbols are allowed */
    val Term: Mode = new Mode(1)

    /** Type and stable term symbols are allowed */
    val Type: Mode = new Mode(2)

    /** Both term and type symbols are allowed */
    val ImportOrExport: Mode = new Mode(4) | Term | Type

    val Scope: Mode = new Mode(8)

    val Member: Mode = new Mode(16)

