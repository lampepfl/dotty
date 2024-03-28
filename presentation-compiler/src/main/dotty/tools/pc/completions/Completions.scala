package dotty.tools.pc
package completions

import java.nio.file.Path
import java.nio.file.Paths

import scala.collection.mutable
import scala.meta.internal.metals.Fuzzy
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.mtags.CoursierComplete
import scala.meta.internal.pc.{IdentifierComparator, MemberOrdering}
import scala.meta.pc.*

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.core.Comments.Comment
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.interactive.Completion.Mode
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.SrcPos
import dotty.tools.pc.AutoImports.AutoImportsGenerator
import dotty.tools.pc.completions.OverrideCompletions.OverrideExtractor
import dotty.tools.pc.buildinfo.BuildInfo
import dotty.tools.pc.utils.MtagsEnrichments.*
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.interactive.Interactive

class Completions(
    text: String,
    ctx: Context,
    search: SymbolSearch,
    buildTargetIdentifier: String,
    completionPos: CompletionPos,
    indexedContext: IndexedContext,
    path: List[Tree],
    adjustedPath: List[untpd.Tree],
    config: PresentationCompilerConfig,
    workspace: Option[Path],
    autoImports: AutoImportsGenerator,
    comments: List[Comment],
    options: List[String]
)(using ReportContext):

  given context: Context = ctx

  private lazy val coursierComplete = new CoursierComplete(BuildInfo.scalaVersion)
  private lazy val completionMode = Completion.completionMode(adjustedPath, completionPos.originalCursorPosition)

  private lazy val shouldAddSnippet =
    path match
      /* In case of `method@@()` we should not add snippets and the path
       * will contain apply as the parent of the current tree.
       */
      case (fun) :: (appl: GenericApply) :: _ if appl.fun == fun =>
        false
      case _ :: (withcursor @ Select(fun, name)) :: (appl: GenericApply) :: _
          if appl.fun == withcursor && name.decoded == Cursor.value =>
        false
      case (_: (Import | Export)) :: _ => false
      case _ :: (_: (Import | Export)) :: _ => false
      case (_: Ident) :: (_: SeqLiteral) :: _ => false
      case _ => true

  private lazy val allowTemplateSuffix: Boolean =
    path match
      case _ :: New(selectOrIdent: (Select | Ident)) :: _ => true
      case _ => false

  def includeSymbol(sym: Symbol)(using Context): Boolean =
    def hasSyntheticCursorSuffix: Boolean =
      if !sym.name.endsWith(Cursor.value) then false
      else
        val realNameLength = sym.decodedName.length() - Cursor.value.length()
        sym.source == completionPos.originalCursorPosition.source &&
        sym.span.start + realNameLength == completionPos.queryEnd

    val generalExclude =
      isUninterestingSymbol(sym) ||
        !isNotLocalForwardReference(sym) ||
        sym.isPackageObject ||
        hasSyntheticCursorSuffix

    def isWildcardParam(sym: Symbol) =
      if sym.isTerm && sym.owner.isAnonymousFunction then
        sym.name match
          case DerivedName(under, _) =>
            under.isEmpty
          case _ => false
      else false

    if generalExclude then false
    else if completionMode.is(Mode.Type) then true
    else !isWildcardParam(sym) && (sym.isTerm || sym.is(Package))
    end if
  end includeSymbol

  def completions(): (List[CompletionValue], SymbolSearch.Result) =
    val (advanced, exclusive) = advancedCompletions(path, completionPos)
    val (all, result) =
      if exclusive then (advanced, SymbolSearch.Result.COMPLETE)
      else
        val keywords =
          KeywordsCompletions.contribute(path, completionPos, comments)
        val allAdvanced = advanced ++ keywords
        path match
          // should not show completions for toplevel
          case Nil | (_: PackageDef) :: _ if !completionPos.originalCursorPosition.source.file.ext.isScalaScript =>
            (allAdvanced, SymbolSearch.Result.COMPLETE)
          case Select(qual, _) :: _ if qual.typeOpt.isErroneous =>
            (allAdvanced, SymbolSearch.Result.COMPLETE)
          case Select(qual, _) :: _ =>
            val compilerCompletions = Completion.rawCompletions(completionPos.originalCursorPosition, completionMode, completionPos.query, path, adjustedPath)
            val (compiler, result) = compilerCompletions
              .toList
              .flatMap(toCompletionValues)
              .filterInteresting(qual.typeOpt.widenDealias)
            (allAdvanced ++ compiler, result)
          case _ =>
            val compilerCompletions = Completion.rawCompletions(completionPos.originalCursorPosition, completionMode, completionPos.query, path, adjustedPath)
            val (compiler, result) = compilerCompletions
              .toList
              .flatMap(toCompletionValues)
              .filterInteresting()
            (allAdvanced ++ compiler, result)
        end match

    val application = CompletionApplication.fromPath(path)
    val ordering = completionOrdering(application)
    val sorted = all.sorted(ordering)
    val values = application.postProcess(sorted)
    (values, result)
  end completions

  private def toCompletionValues(
      completion: Name,
      denots: Seq[SingleDenotation]
  ): List[CompletionValue] =
    denots.toList.flatMap: denot =>
      completionsWithSuffix(
        denot,
        completion.show,
        (label, denot, suffix) => CompletionValue.Compiler(label, denot, suffix)
      )
  end toCompletionValues

  inline private def undoBacktick(label: String): String =
    label.stripPrefix("`").stripSuffix("`")

  private def getParams(symbol: Symbol) =
    lazy val extensionParam = symbol.extensionParam
    if symbol.is(Flags.Extension) then
      symbol.paramSymss.filterNot(
        _.contains(extensionParam)
      )
    else symbol.paramSymss

  private def isAbstractType(symbol: Symbol) =
    (symbol.info.typeSymbol.is(Trait) // trait A{ def doSomething: Int}
    // object B{ new A@@}
    // Note: I realised that the value of Flag.Trait is flaky and
    // leads to the failure of one of the DocSuite tests
      || symbol.info.typeSymbol.isAllOf(
        Flags.JavaInterface // in Java:  interface A {}
          // in Scala 3: object B { new A@@}
      ) || symbol.info.typeSymbol.isAllOf(
        Flags.PureInterface // in Java: abstract class Shape { abstract void draw();}
          // Shape has only abstract members, so can be represented by a Java interface
          // in Scala 3: object B{ new Shap@@ }
      ) || (symbol.info.typeSymbol.is(Flags.Abstract) &&
        symbol.isClass) // so as to exclude abstract methods
      // abstract class A(i: Int){ def doSomething: Int}
      // object B{ new A@@}
    )
  end isAbstractType

  private def findSuffix(symbol: Symbol): CompletionSuffix =
    CompletionSuffix.empty
      .chain { suffix => // for [] suffix
        if shouldAddSnippet && symbol.info.typeParams.nonEmpty
        then suffix.withNewSuffixSnippet(SuffixKind.Bracket)
        else suffix
      }
      .chain { suffix => // for () suffix
        if shouldAddSnippet && symbol.is(Flags.Method)
        then
          val paramss = getParams(symbol)
          paramss match
            case Nil => suffix
            case List(Nil) => suffix.withNewSuffix(SuffixKind.Brace)
            case _ if config.isCompletionSnippetsEnabled() =>
              val onlyParameterless = paramss.forall(_.isEmpty)
              lazy val onlyImplicitOrTypeParams = paramss.forall(
                _.exists { sym =>
                  sym.isType || sym.is(Implicit) || sym.is(Given)
                }
              )
              if onlyParameterless then suffix.withNewSuffix(SuffixKind.Brace)
              else if onlyImplicitOrTypeParams then suffix
              else if suffix.hasSnippet then
                suffix.withNewSuffix(SuffixKind.Brace)
              else suffix.withNewSuffixSnippet(SuffixKind.Brace)
            case _ => suffix
          end match
        else suffix
      }
      .chain { suffix => // for {} suffix
        if shouldAddSnippet && allowTemplateSuffix
          && isAbstractType(symbol)
        then
          if suffix.hasSnippet then suffix.withNewSuffix(SuffixKind.Template)
          else suffix.withNewSuffixSnippet(SuffixKind.Template)
        else suffix
      }

  end findSuffix

  def completionsWithSuffix(
      denot: SingleDenotation,
      label: String,
      toCompletionValue: (String, SingleDenotation, CompletionSuffix) => CompletionValue
  ): List[CompletionValue] =
    val sym = denot.symbol
    // find the apply completion that would need a snippet
    val methodDenots: List[SingleDenotation] =
      if shouldAddSnippet && completionMode.is(Mode.Term) &&
        (sym.is(Flags.Module) || sym.isField || sym.isClass && !sym.is(Flags.Trait)) && !sym.is(Flags.JavaDefined)
      then
        val info =
          /* Companion will be added even for normal classes now,
           * but it will not show up from classpath. We can suggest
           * constructors based on those synthetic applies.
           */
          if sym.isClass && sym.companionModule.exists then sym.companionModule.info
          else denot.info
        val applyDenots = info.member(nme.apply).allSymbols.map(_.asSeenFrom(info).asSingleDenotation)
        denot :: applyDenots
      else denot :: Nil

    methodDenots.map { methodDenot =>
      val suffix = findSuffix(methodDenot.symbol)
      val name = undoBacktick(label)
      toCompletionValue(
        name,
        methodDenot,
        suffix
      )
    }
  end completionsWithSuffix

  /**
   * @return Tuple of completionValues and flag. If the latter boolean value is true
   *         Metals should provide advanced completions only.
   */
  private def advancedCompletions(
      path: List[Tree],
      completionPos: CompletionPos
  ): (List[CompletionValue], Boolean) =
    val pos = completionPos.originalCursorPosition
    lazy val rawPath = Paths
      .get(pos.source.path).nn
    lazy val rawFileName = rawPath
      .getFileName()
      .toString()
    lazy val filename = rawFileName
      .stripSuffix(".scala")
    val MatchCaseExtractor = new MatchCaseExtractor(pos, text, completionPos)
    val ScalaCliCompletions =
      new ScalaCliCompletions(coursierComplete, pos, text)

    path match
      case ScalaCliCompletions(dependency) =>
        (ScalaCliCompletions.contribute(dependency), true)

      case _
          if MultilineCommentCompletion.isMultilineCommentCompletion(
            pos,
            text,
          ) =>
        val values = MultilineCommentCompletion.contribute(config)
        (values, true)

      case _ if ScaladocCompletions.isScaladocCompletion(pos, text) =>
        val values = ScaladocCompletions.contribute(pos, text, config)
        (values, true)

      case MatchCaseExtractor.MatchExtractor(selector) =>
        (
          CaseKeywordCompletion.matchContribute(
            selector,
            completionPos,
            indexedContext,
            config,
            search,
            autoImports,
            options.contains("-no-indent")
          ),
          false,
        )

      case MatchCaseExtractor.TypedCasePatternExtractor(
            selector,
            parent,
            identName
          ) =>
        (
          CaseKeywordCompletion.contribute(
            selector,
            completionPos,
            indexedContext,
            config,
            search,
            parent,
            autoImports,
            patternOnly = Some(identName),
            hasBind = true
          ),
          false,
        )

      case MatchCaseExtractor.CasePatternExtractor(
            selector,
            parent,
            identName
          ) =>
        (
          CaseKeywordCompletion.contribute(
            selector,
            completionPos,
            indexedContext,
            config,
            search,
            parent,
            autoImports,
            patternOnly = Some(identName)
          ),
          false,
        )

      case MatchCaseExtractor.CaseExtractor(
            selector,
            parent,
            includeExhaustive
          ) =>
        (
          CaseKeywordCompletion.contribute(
            selector,
            completionPos,
            indexedContext,
            config,
            search,
            parent,
            autoImports,
            includeExhaustive = includeExhaustive
          ),
          true,
        )

      // class FooImpl extends Foo:
      //   def x|
      case OverrideExtractor(td, completing, start, exhaustive, fallbackName) =>
        (
          OverrideCompletions.contribute(
            td,
            completing,
            start,
            indexedContext,
            search,
            config,
            autoImports,
            fallbackName
          ),
          exhaustive,
        )

      // class Fo@@
      case (td: TypeDef) :: _
          if Fuzzy.matches(
            td.symbol.name.decoded.replace(Cursor.value, "").nn,
            filename
          ) =>
        val values = FilenameCompletions.contribute(filename, td)
        (values, true)
      case (lit @ Literal(Constant(_: String))) :: _ =>
        val completions = InterpolatorCompletions
          .contribute(
            text,
            completionPos,
            indexedContext,
            lit,
            path,
            this,
            config.isCompletionSnippetsEnabled(),
            search,
            config,
            buildTargetIdentifier
          )
          .filterInteresting(enrich = false)
          ._1
        (completions, true)

      case (imp @ Import(expr, selectors)) :: _
          if isAmmoniteCompletionPosition(imp, rawFileName, "$file") =>
        (
          AmmoniteFileCompletions.contribute(
            expr,
            selectors,
            pos.endPos.toLsp,
            rawPath.toString(),
            workspace,
            rawFileName
          ),
          true,
        )

      case (imp @ Import(_, selectors)) :: _
          if isAmmoniteCompletionPosition(imp, rawFileName, "$ivy") ||
            isWorksheetIvyCompletionPosition(imp, imp.sourcePos) =>
        (
          AmmoniteIvyCompletions.contribute(
            coursierComplete,
            selectors,
            completionPos,
            text
          ),
          true,
        )

      case (tree: (Import | Export)) :: _
          if tree.selectors.exists(_.renamed.sourcePos.contains(pos)) =>
        (List.empty, true)

      // From Scala 3.1.3-RC3 (as far as I know), path contains
      // `Literal(Constant(null))` on head for an incomplete program, in this case, just ignore the head.
      case Literal(Constant(null)) :: tl =>
        advancedCompletions(tl, completionPos)

      case _ =>
        val args = NamedArgCompletions.contribute(
          pos,
          path,
          adjustedPath,
          indexedContext,
          config.isCompletionSnippetsEnabled()
        )
        (args, false)
    end match
  end advancedCompletions

  private def isAmmoniteCompletionPosition(
      tree: Tree,
      fileName: String,
      magicImport: String
  ): Boolean =

    def getQualifierStart(identOrSelect: Tree): String =
      identOrSelect match
        case Ident(name) => name.toString
        case Select(newQual, name) => getQualifierStart(newQual)
        case _ => ""

    tree match
      case Import(identOrSelect, _) =>
        fileName.isAmmoniteGeneratedFile && getQualifierStart(identOrSelect)
          .toString()
          .startsWith(magicImport)
      case _ => false
  end isAmmoniteCompletionPosition

  def isWorksheetIvyCompletionPosition(
      tree: Tree,
      pos: SourcePosition
  ): Boolean =
    tree match
      case Import(Ident(ivy), _) =>
        pos.source.file.name.isWorksheet &&
        (ivy.decoded == "$ivy" ||
          ivy.decoded == "$dep")
      case _ => false

  private def enrichWithSymbolSearch(
      visit: CompletionValue => Boolean,
      qualType: Type = ctx.definitions.AnyType
  ): Option[SymbolSearch.Result] =
    val query = completionPos.query
    if completionMode.is(Mode.Scope) && query.nonEmpty then
      val visitor = new CompilerSearchVisitor(sym =>
        if !(sym.is(Flags.ExtensionMethod) ||
          (sym.maybeOwner.is(Flags.Implicit) && sym.maybeOwner.isClass))
        then
          indexedContext.lookupSym(sym) match
            case IndexedContext.Result.InScope => false
            case _ =>
              completionsWithSuffix(
                sym,
                sym.decodedName,
                CompletionValue.Workspace(_, _, _, sym)
              ).map(visit).forall(_ == true)
        else false,
      )
      Some(search.search(query, buildTargetIdentifier, visitor).nn)
    else if completionMode.is(Mode.Member) then
      val visitor = new CompilerSearchVisitor(sym =>
        def isExtensionMethod = sym.is(ExtensionMethod) &&
          qualType.widenDealias <:< sym.extensionParam.info.widenDealias
        def isImplicitClass(owner: Symbol) =
          val constructorParam =
            owner.info
              .membersBasedOnFlags(
                Flags.ParamAccessor,
                Flags.EmptyFlags,
              )
              .headOption
              .map(_.info)
          owner.isClass && owner.is(Flags.Implicit) &&
          constructorParam.exists(p =>
            qualType.widenDealias <:< p.widenDealias
          )
        end isImplicitClass

        def isDefaultVariableSetter = sym.is(Flags.Accessor) && sym.is(Flags.Method)
        def isImplicitClassMember =
          isImplicitClass(sym.maybeOwner) && !sym.is(Flags.Synthetic) && sym.isPublic
          && !sym.isConstructor && !isDefaultVariableSetter

        if isExtensionMethod then
          completionsWithSuffix(
            sym,
            sym.decodedName,
            CompletionValue.Extension(_, _, _)
          ).map(visit).forall(_ == true)
        else if isImplicitClassMember then
          completionsWithSuffix(
            sym,
            sym.decodedName,
            CompletionValue.ImplicitClass(_, _, _, sym.maybeOwner),
          ).map(visit).forall(_ == true)
        else false,
      )
      Some(search.searchMethods(query, buildTargetIdentifier, visitor).nn)
    else Some(SymbolSearch.Result.INCOMPLETE)

  end enrichWithSymbolSearch

  extension (s: SrcPos)
    def isAfter(s1: SrcPos) =
      s.sourcePos.exists && s1.sourcePos.exists && s.sourcePos.point > s1.sourcePos.point

  extension (sym: Symbol)
    def detailString: String =
      if sym.is(Method) then
        val sig = sym.signature
        val sigString =
          if sig.paramsSig.isEmpty then "()"
          else
            sig.paramsSig
              .map(p => p.toString)
              .mkString("(", ",", ")")
        sym.showFullName + sigString
      else sym.fullName.stripModuleClassSuffix.show

  extension (l: List[CompletionValue])
    def filterInteresting(
        qualType: Type = ctx.definitions.AnyType,
        enrich: Boolean = true
    ): (List[CompletionValue], SymbolSearch.Result) =

      val isSeen = mutable.Set.empty[String]
      val buf = List.newBuilder[CompletionValue]
      def visit(head: CompletionValue): Boolean =
        val (id, include) =
          head match
            case doc: CompletionValue.Document => (doc.label, true)
            case over: CompletionValue.Override => (over.label, true)
            case ck: CompletionValue.CaseKeyword => (ck.label, true)
            case symOnly: CompletionValue.Symbolic =>
              val sym = symOnly.symbol
              val name = SemanticdbSymbols.symbolName(sym)
              val nameId =
                if sym.isClass || sym.is(Module) then
                  // drop #|. at the end to avoid duplication
                  name.substring(0, name.length() - 1).nn
                else name
              val suffix =
                if symOnly.snippetSuffix.addLabelSnippet then "[]" else ""
              val id = nameId + suffix
              val include = includeSymbol(sym)
              (id, include)
            case kw: CompletionValue.Keyword => (kw.label, true)
            case mc: CompletionValue.MatchCompletion => (mc.label, true)
            case autofill: CompletionValue.Autofill =>
              (autofill.label, true)
            case fileSysMember: CompletionValue.FileSystemMember =>
              (fileSysMember.label, true)
            case ii: CompletionValue.IvyImport => (ii.label, true)

        if !isSeen(id) && include then
          isSeen += id
          buf += head
          true
        else false
      end visit

      l.foreach(visit)

      if enrich then
        val searchResult =
          enrichWithSymbolSearch(visit, qualType).getOrElse(
            SymbolSearch.Result.COMPLETE
          )
        (buf.result, searchResult)
      else (buf.result, SymbolSearch.Result.COMPLETE)

    end filterInteresting
  end extension

  private lazy val isUninterestingSymbol: Set[Symbol] = Set[Symbol](
    defn.Any_==,
    defn.Any_!=,
    defn.Any_##,
    defn.Object_eq,
    defn.Object_ne,
    defn.RepeatedParamClass,
    defn.ByNameParamClass2x,
    defn.Object_notify,
    defn.Object_notifyAll,
    defn.Object_notify,
    defn.Predef_undefined,
    defn.ObjectClass.info.member(nme.wait_).symbol,
    // NOTE(olafur) IntelliJ does not complete the root package and without this filter
    // then `_root_` would appear as a completion result in the code `foobar(_<COMPLETE>)`
    defn.RootPackage,
    // NOTE(gabro) valueOf was added as a Predef member in 2.13. We filter it out since is a niche
    // use case and it would appear upon typing 'val'
    defn.ValueOfClass.info.member(nme.valueOf).symbol,
    defn.ScalaPredefModule.requiredMethod(nme.valueOf)
  ).flatMap(_.alternatives.map(_.symbol)).toSet

  private def isNotLocalForwardReference(sym: Symbol)(using Context): Boolean =
    !sym.isLocalToBlock ||
      !sym.srcPos.isAfter(completionPos.originalCursorPosition) ||
      sym.is(Param)

  private def computeRelevancePenalty(
      completion: CompletionValue,
      application: CompletionApplication,
  ): Int =
    import scala.meta.internal.pc.MemberOrdering.*

    def hasGetter(sym: Symbol) = try
      def isModuleOrClass = sym.is(Module) || sym.isClass
      // isField returns true for some classes
      def isJavaClass = sym.is(JavaDefined) && isModuleOrClass
      (sym.isField && !isJavaClass && !isModuleOrClass) || sym.getter != NoSymbol
    catch case _ => false

    def symbolRelevance(sym: Symbol): Int =
      var relevance = 0
      // symbols defined in this file are more relevant
      if completionPos.originalCursorPosition.source != sym.source || sym.is(Package) then
        relevance |= IsNotDefinedInFile

      // fields are more relevant than non fields (such as method)
      completion match
        // For override-completion, we don't care fields or methods because
        // we can override both fields and non-fields
        case _: CompletionValue.Override =>
          relevance |= IsNotGetter
        case _ if !hasGetter(sym) =>
          relevance |= IsNotGetter
        case _ =>

      // symbols whose owner is a base class are less relevant
      if sym.owner == defn.AnyClass || sym.owner == defn.ObjectClass
      then relevance |= IsInheritedBaseMethod
      // symbols not provided via an implicit are more relevant
      if sym.is(Implicit) ||
        sym.is(ExtensionMethod) ||
        application.isImplicitConversion(sym)
      then relevance |= IsImplicitConversion
      if application.isInherited(sym) then relevance |= IsInherited
      if sym.is(Package) then relevance |= IsPackage
      // accessors of case class members are more relevant
      if !sym.is(CaseAccessor) then relevance |= IsNotCaseAccessor
      // public symbols are more relevant
      if !sym.isPublic then relevance |= IsNotCaseAccessor
      // synthetic symbols are less relevant (e.g. `copy` on case classes)
      if sym.is(Synthetic) && !sym.isAllOf(EnumCase) then
        relevance |= IsSynthetic
      if sym.isDeprecated then relevance |= IsDeprecated
      if isEvilMethod(sym.name) then relevance |= IsEvilMethod
      if !completionMode.is(Mode.ImportOrExport) &&
        completionMode.is(Mode.Type) && !sym.isType then relevance |= IsNotTypeInTypePos
      relevance
    end symbolRelevance

    completion match
      case ov: CompletionValue.Override =>
        var penalty = symbolRelevance(ov.symbol)
        // show the abstract members first
        if !ov.symbol.is(Deferred) then penalty |= MemberOrdering.IsNotAbstract
        penalty
      case CompletionValue.Workspace(_, denot, _, _) =>
        symbolRelevance(denot.symbol) | (IsWorkspaceSymbol + denot.name.show.length())
      case sym: CompletionValue.Symbolic =>
        symbolRelevance(sym.symbol)
      case _ =>
        Int.MaxValue
  end computeRelevancePenalty

  private lazy val isEvilMethod: Set[Name] = Set[Name](
    nme.notifyAll_,
    nme.notify_,
    nme.wait_,
    nme.clone_,
    nme.finalize_
  )

  trait CompletionApplication:
    def isImplicitConversion(symbol: Symbol): Boolean
    def isMember(symbol: Symbol): Boolean
    def isInherited(symbol: Symbol): Boolean
    def postProcess(items: List[CompletionValue]): List[CompletionValue]

  object CompletionApplication:
    val empty = new CompletionApplication:
      def isImplicitConversion(symbol: Symbol): Boolean = false
      def isMember(symbol: Symbol): Boolean = false
      def isInherited(symbol: Symbol): Boolean = false
      def postProcess(items: List[CompletionValue]): List[CompletionValue] =
        items

    def forSelect(sel: Select): CompletionApplication =
      val tpe = sel.qualifier.typeOpt
      val members = tpe.allMembers.map(_.symbol).toSet

      new CompletionApplication:
        def isImplicitConversion(symbol: Symbol): Boolean =
          !isMember(symbol)
        def isMember(symbol: Symbol): Boolean = members.contains(symbol)
        def isInherited(symbol: Symbol): Boolean =
          isMember(symbol) && symbol.owner != tpe.typeSymbol
        def postProcess(items: List[CompletionValue]): List[CompletionValue] =
          items.map {
            case completion @ CompletionValue.Compiler(label, denot, suffix)
                if isMember(completion.symbol) =>
              CompletionValue.Compiler(
                label,
                substituteTypeVars(completion.symbol),
                suffix
              )
            case other => other
          }

        private def substituteTypeVars(symbol: Symbol): Symbol =
          val denot = symbol.asSeenFrom(tpe)
          symbol.withUpdatedTpe(denot.info)

      end new
    end forSelect

    def fromPath(path: List[Tree]): CompletionApplication =
      path.headOption match
        case Some(Select(qual @ This(_), _)) if qual.span.isSynthetic => empty
        case Some(select: Select) => forSelect(select)
        case _ => empty

  end CompletionApplication

  private def completionOrdering(
      application: CompletionApplication
  ): Ordering[CompletionValue] =
    new Ordering[CompletionValue]:
      val queryLower = completionPos.query.toLowerCase()
      val fuzzyCache = mutable.Map.empty[CompletionValue, Int]

      def compareLocalSymbols(s1: Symbol, s2: Symbol): Int =
        if s1.isLocal && s2.isLocal && s1.sourcePos.exists && s2.sourcePos.exists
        then
          val firstIsAfter = s1.srcPos.isAfter(s2.srcPos)
          if firstIsAfter then -1 else 1
        else 0
      end compareLocalSymbols

      def compareByRelevance(o1: CompletionValue, o2: CompletionValue): Int =
        Integer.compare(
          computeRelevancePenalty(o1, application),
          computeRelevancePenalty(o2, application),
        )

      def fuzzyScore(o: CompletionValue.Symbolic): Int =
        fuzzyCache.getOrElseUpdate(
          o, {
            val name = o.label.toLowerCase().nn
            if name.startsWith(queryLower) then 0
            else if name.contains(queryLower) then 1
            else 2
          }
        )

      /**
       * This one is used for the following case:
       * ```scala
       * def foo(argument: Int): Int = ???
       * val argument = 42
       * foo(arg@@) // completions should be ordered as :
       *            // - argument       (local val) - actual value comes first
       *            // - argument = ... (named arg) - named arg after
       *            // - ... all other options
       * ```
       */
      def compareInApplyParams(o1: CompletionValue, o2: CompletionValue): Int =
        def priority(v: CompletionValue): Int =
          v match
            case _: CompletionValue.Compiler => 0
            case _ => 1

        priority(o1) - priority(o2)
      end compareInApplyParams

      def prioritizeKeywords(o1: CompletionValue, o2: CompletionValue): Int =
        def priority(v: CompletionValue): Int =
          v match
            case _: CompletionValue.CaseKeyword => 0
            case _: CompletionValue.NamedArg => 1
            case _: CompletionValue.Keyword => 2
            case _ => 3

        priority(o1) - priority(o2)
      end prioritizeKeywords
      /**
       * Some completion values should be shown first such as CaseKeyword and
       * NamedArg
       */
      def compareCompletionValue(
          sym1: CompletionValue.Symbolic,
          sym2: CompletionValue.Symbolic
      ): Boolean =
        val prioritizeCaseKeyword =
          sym1.isInstanceOf[CompletionValue.CaseKeyword] &&
            !sym2.isInstanceOf[CompletionValue.CaseKeyword]

        // if the name is the same as the parameter name then we should show the symbolic first
        val prefixMatches =
          sym1.symbol.name.toString().startsWith(sym2.symbol.name.toString())

        val prioritizeNamed =
          sym1.isInstanceOf[CompletionValue.NamedArg] &&
            !sym2.isInstanceOf[CompletionValue.NamedArg] &&
            !prefixMatches

        prioritizeCaseKeyword || prioritizeNamed
      end compareCompletionValue

      override def compare(o1: CompletionValue, o2: CompletionValue): Int =
        (o1, o2) match
          case (o1: CompletionValue.NamedArg, o2: CompletionValue.NamedArg) =>
            IdentifierComparator.compare(
              o1.label,
              o2.label
            )
          case (
                sym1: CompletionValue.Symbolic,
                sym2: CompletionValue.Symbolic,
              ) =>
            if compareCompletionValue(sym1, sym2) then 0
            else if compareCompletionValue(sym2, sym1) then 1
            else
              val s1 = sym1.symbol
              val s2 = sym2.symbol
              val byLocalSymbol = compareLocalSymbols(s1, s2)
              if byLocalSymbol != 0 then byLocalSymbol
              else
                val byRelevance = compareByRelevance(o1, o2)
                if byRelevance != 0 then byRelevance
                else
                  val byFuzzy = Integer.compare(
                    fuzzyScore(sym1),
                    fuzzyScore(sym2)
                  )
                  if byFuzzy != 0 then byFuzzy
                  else
                    val byIdentifier = IdentifierComparator.compare(
                      s1.name.show,
                      s2.name.show
                    )
                    if byIdentifier != 0 then byIdentifier
                    else
                      val byOwner =
                        s1.owner.fullName.toString
                          .compareTo(s2.owner.fullName.toString)
                      if byOwner != 0 then byOwner
                      else
                        val byParamCount = Integer.compare(
                          s1.paramSymss.flatten.size,
                          s2.paramSymss.flatten.size
                        )
                        if byParamCount != 0 then byParamCount
                        else s1.detailString.compareTo(s2.detailString)
                  end if
                end if
              end if
            end if
          case _ =>
            val byApplyParams = compareInApplyParams(o1, o2)
            if byApplyParams != 0 then byApplyParams
            else
              val keywords = prioritizeKeywords(o1, o2)
              if keywords != 0 then keywords
              else compareByRelevance(o1, o2)
      end compare

end Completions
