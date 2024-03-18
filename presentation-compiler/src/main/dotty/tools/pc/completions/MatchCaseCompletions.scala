package dotty.tools.pc
package completions

import java.net.URI

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.pc.CompletionFuzzy
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolSearch

import dotty.tools.toOption
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Denotations.Denotation
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.AndType
import dotty.tools.dotc.core.Types.ClassInfo
import dotty.tools.dotc.core.Types.OrType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.AutoImports.AutoImportsGenerator
import dotty.tools.pc.AutoImports.SymbolImport
import dotty.tools.pc.MetalsInteractive.*
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.printer.ShortenedTypePrinter.IncludeDefaultParam
import dotty.tools.pc.utils.MtagsEnrichments.*

import org.eclipse.lsp4j as l

object CaseKeywordCompletion:

  /**
   * A `case` completion showing the valid subtypes of the type being deconstructed.
   *
   * @param selector `selector` of `selector match { cases }`  or `EmptyTree` when
   *                 not in a match expression (for example `List(1).foreach { case@@ }`.
   * @param completionPos the position of the completion
   * @param typedtree typed tree of the file, used for generating auto imports
   * @param indexedContext
   * @param config
   * @param parent the parent tree node of the pattern match, for example `Apply(_, _)` when in
   *               `List(1).foreach { cas@@ }`, used as fallback to compute the type of the selector when
   *               it's `EmptyTree`.
   * @param patternOnly `None` for `case@@`, `Some(query)` for `case query@@ =>` or `case ab: query@@ =>`
   * @param hasBind `true` when `case _: @@ =>`, if hasBind we don't need unapply completions
   */
  def contribute(
      selector: Tree,
      completionPos: CompletionPos,
      indexedContext: IndexedContext,
      config: PresentationCompilerConfig,
      search: SymbolSearch,
      parent: Tree,
      autoImportsGen: AutoImportsGenerator,
      patternOnly: Option[String] = None,
      hasBind: Boolean = false,
      includeExhaustive: Option[NewLineOptions] = None
  )(using ReportContext): List[CompletionValue] =
    import indexedContext.ctx
    val definitions = indexedContext.ctx.definitions
    val clientSupportsSnippets = config.isCompletionSnippetsEnabled()
    val completionGenerator = CompletionValueGenerator(
      completionPos,
      clientSupportsSnippets,
      patternOnly,
      hasBind
    )
    val printer = ShortenedTypePrinter(search, IncludeDefaultParam.Never)(using indexedContext)
    val selTpe = selector match
      case EmptyTree =>
        parent match
          case TreeApply(fun, _) if !fun.tpe.isErroneous =>
            fun.tpe.paramInfoss match
              case (head :: Nil) :: _
                  if definitions.isFunctionType(head) || head.isRef(
                    definitions.PartialFunctionClass
                  ) =>
                val args = head.argTypes.init
                if args.length > 1 then
                  Some(definitions.tupleType(args).widen.metalsDealias)
                else args.headOption.map(_.widen.metalsDealias)
              case _ => None
          case _ => None
      case sel =>
          Some(sel.tpe.widen.metalsDealias)

    selTpe
      .map { selTpe =>
        val selectorSym = selTpe.typeSymbol
        // Special handle case when selector is a tuple or `FunctionN`.
        if definitions.isTupleClass(selectorSym) || definitions.isFunctionClass(
            selectorSym
          )
        then
          if patternOnly.isEmpty then
            val selectorTpe = selTpe.show
            val tpeLabel =
              if !selectorTpe.contains("x$1") then selectorTpe
              else selector.symbol.info.show
            val label = s"case ${tpeLabel} =>"
            List(
              CompletionValue.CaseKeyword(
                selectorSym,
                label,
                Some(
                  if config.isCompletionSnippetsEnabled() then "case ($0) =>"
                  else "case () =>"
                ),
                Nil,
                range = Some(completionPos.toEditRange),
                command = config.parameterHintsCommand().toOption.flatMap(_.asScala),
              )
            )
          else Nil
        else
          val result = ListBuffer.empty[SymbolImport]
          val isVisited = mutable.Set.empty[Symbol]

          val isBottom = Set[Symbol](
            definitions.NullClass,
            definitions.NothingClass,
          )
          val tpes = Set(selectorSym, selectorSym.companion)
          def isSubclass(sym: Symbol) = tpes.exists(par => sym.isSubClass(par))

          def visit(symImport: SymbolImport): Unit =

            def recordVisit(s: Symbol): Unit =
              if s != NoSymbol && !isVisited(s) then
                isVisited += s
                recordVisit(s.moduleClass)
                recordVisit(s.sourceModule)

            val sym = symImport.sym
            if !isVisited(sym) then
              recordVisit(sym)
              if completionGenerator.fuzzyMatches(symImport.name) then
                result += symImport
          end visit

          // Step 1: walk through scope members.
          def isValid(sym: Symbol) = !tpes(sym) &&
            !isBottom(sym) &&
            isSubclass(sym) &&
            (sym.is(Case) || sym.is(Flags.Module) || sym.isClass) &&
            (sym.isPublic || sym.isAccessibleFrom(selectorSym.info))

          indexedContext.scopeSymbols
            .foreach(s =>
              val ts = s.info.metalsDealias.typeSymbol
              if isValid(ts) then visit(autoImportsGen.inferSymbolImport(ts))
            )
          // Step 2: walk through known subclasses of sealed types.
          val sealedDescs = subclassesForType(
            selTpe.bounds.hi
          )
          sealedDescs.foreach { sym =>
            val symbolImport = autoImportsGen.inferSymbolImport(sym)
            visit(symbolImport)
          }
          val res = result.result().flatMap {
            case si @ SymbolImport(sym, name, importSel) =>
              completionGenerator.labelForCaseMember(sym, name.value).map {
                label =>
                  (si, label)
              }
          }
          val caseItems =
            if res.isEmpty then completionGenerator.caseKeywordOnly
            else
              res.map((si, label) =>
                completionGenerator.toCompletionValue(
                  si.sym,
                  label,
                  autoImportsGen.renderImports(si.importSel.toList),
                )
              )

          includeExhaustive match
            // In `List(foo).map { cas@@} we want to provide also `case (exhaustive)` completion
            // which works like exhaustive match.
            case Some(NewLineOptions(moveToNewLine, addNewLineAfter)) =>
              val sealedMembers =
                val sealedMembers0 =
                  res.filter((si, _) => sealedDescs.contains(si.sym))
                sortSubclasses(
                  selectorSym.info,
                  sealedMembers0,
                  completionPos.sourceUri,
                  search,
                )
              sealedMembers match
                case (_, label) :: tail if tail.length > 0 =>
                  val (newLine, addIndent) =
                    if moveToNewLine then ("\n\t", "\t") else ("", "")
                  val insertText = Some(
                    tail
                      .map(_._2)
                      .mkString(
                        if clientSupportsSnippets then
                          s"$newLine${label} $$0\n$addIndent"
                        else s"$newLine${label}\n$addIndent",
                        s"\n$addIndent",
                        if addNewLineAfter then "\n" else "",
                      )
                  )
                  val allImports =
                    sealedMembers.flatMap(_._1.importSel).distinct
                  val importEdit = autoImportsGen.renderImports(allImports)
                  val exhaustive = CompletionValue.MatchCompletion(
                    s"case (exhaustive)",
                    insertText,
                    importEdit.toList,
                    s" ${printer.tpe(selTpe)} (${res.length} cases)",
                  )
                  exhaustive :: caseItems
                case _ => caseItems
              end match
            case None => caseItems
          end match
        end if
      }
      .getOrElse(Nil)
  end contribute

  /**
   * A `match` keyword completion to generate an exhaustive pattern match for sealed types.
   *
   * @param selector the match expression being deconstructed or `EmptyTree` when
   *                 not in a match expression (for example `List(1).foreach { case@@ }`.
   * @param completionPos the position of the completion
   * @param typedtree typed tree of the file, used for generating auto imports
   */
  def matchContribute(
      selector: Tree,
      completionPos: CompletionPos,
      indexedContext: IndexedContext,
      config: PresentationCompilerConfig,
      search: SymbolSearch,
      autoImportsGen: AutoImportsGenerator,
      noIndent: Boolean
  )(using ReportContext): List[CompletionValue] =
    import indexedContext.ctx
    val clientSupportsSnippets = config.isCompletionSnippetsEnabled()

    val printer = ShortenedTypePrinter(search, IncludeDefaultParam.Never)(using indexedContext)

    val completionGenerator = CompletionValueGenerator(
      completionPos,
      clientSupportsSnippets
    )

    val tpeStr = printer.tpe(selector.tpe.widen.metalsDealias.bounds.hi)
    val tpe = selector.typeOpt.widen.metalsDealias.bounds.hi match
      case tr @ TypeRef(_, _) => tr.underlying
      case t => t

    val sortedSubclasses =
      val subclasses =
        subclassesForType(tpe.widen.bounds.hi)
          .map(autoImportsGen.inferSymbolImport)
          .flatMap(si =>
            completionGenerator.labelForCaseMember(si.sym, si.name).map((si, _))
          )
      sortSubclasses(tpe, subclasses, completionPos.sourceUri, search)

    val (labels, imports) =
      sortedSubclasses.map((si, label) => (label, si.importSel)).unzip

    val (obracket, cbracket) = if noIndent then (" {", "}") else ("", "")
    val basicMatch = CompletionValue.MatchCompletion(
      "match",
      Some(
        if clientSupportsSnippets then s"match$obracket\n\tcase$$0\n$cbracket"
        else "match"
      ),
      Nil,
      ""
    )

    val completions = labels match
      case Nil => List(basicMatch)
      case head :: tail =>
        val insertText = Some(
          tail
            .mkString(
              if clientSupportsSnippets then
                s"match$obracket\n\t${head} $$0\n\t"
              else s"match$obracket\n\t${head}\n\t",
              "\n\t",
              s"\n$cbracket"
            )
        )
        val importEdit = autoImportsGen.renderImports(imports.flatten.distinct)
        val exhaustive = CompletionValue.MatchCompletion(
          "match (exhaustive)",
          insertText,
          importEdit.toList,
          s" $tpeStr (${labels.length} cases)"
        )
        List(basicMatch, exhaustive)
    completions
  end matchContribute

  private def sortSubclasses[A](
      tpe: Type,
      syms: List[(SymbolImport, String)],
      uri: URI,
      search: SymbolSearch
  )(using Context): List[(SymbolImport, String)] =
    if syms.forall(_._1.sym.sourcePos.exists) then
      syms.sortBy(_._1.sym.sourcePos.point)
    else
      val defnSymbols = search
        .definitionSourceToplevels(SemanticdbSymbols.symbolName(tpe.typeSymbol), uri).nn
        .asScala
        .zipWithIndex
        .toMap
      syms.sortBy { case (SymbolImport(sym, _, _), _) =>
        val semancticName = SemanticdbSymbols.symbolName(sym)
        defnSymbols.getOrElse(semancticName, -1)
      }

  def sealedDescendants(sym: Symbol)(using Context): List[Symbol] =
    sym.sealedDescendants.filter(child =>
      !(child.is(Sealed) && (child.is(Abstract) || child.is(Trait)))
        && child.maybeOwner.exists
        && (child.isPublic || child.isAccessibleFrom(sym.info))
        && child.name != StdNames.tpnme.LOCAL_CHILD
    )

  def subclassesForType(tpe: Type)(using Context): List[Symbol] =
    /**
     * Split type made of & and | types to a list of simple types.
     * For example, `(A | D) & (B & C)` returns `List(A, D, B, C).
     * Later we use them to generate subclasses of each of these types.
     */
    def getParentTypes(tpe: Type, acc: List[Symbol]): List[Symbol] =
      tpe match
        case AndType(tp1, tp2) =>
          getParentTypes(tp2, getParentTypes(tp1, acc))
        case OrType(tp1, tp2) =>
          getParentTypes(tp2, getParentTypes(tp1, acc))
        case t =>
          tpe.typeSymbol :: acc

    /**
     * Check if `sym` is a subclass of type `tpe`.
     * For `class A extends B with C with D` we have to construct B & C & D type,
     * because `A <:< (B & C) == false`.
     */
    def isExhaustiveMember(sym: Symbol): Boolean =
      sym.info match
        case cl: ClassInfo =>
          val parentsMerged = cl.parents
            .reduceLeftOption((tp1, tp2) => tp1.&(tp2))
            .getOrElse(sym.info)

          cl.selfType <:< tpe || parentsMerged <:< tpe
        case simple => simple <:< tpe

    val parents = getParentTypes(tpe, List.empty)
    parents.toList.map(sealedDescendants) match
      case Nil => Nil
      case subcls :: Nil => subcls
      case subcls =>
        val subclasses = subcls.flatten.distinct
        subclasses.filter(isExhaustiveMember)

  end subclassesForType

end CaseKeywordCompletion

class CompletionValueGenerator(
    completionPos: CompletionPos,
    clientSupportsSnippets: Boolean,
    patternOnly: Option[String] = None,
    hasBind: Boolean = false
):
  def fuzzyMatches(name: String) =
    patternOnly match
      case None => true
      case Some("") => true
      case Some(Cursor.value) => true
      case Some(query) => CompletionFuzzy.matches(query.replace(Cursor.value, "").nn, name)

  def labelForCaseMember(sym: Symbol, name: String)(using
      Context
  ): Option[String] =
    val isModuleLike =
      sym.is(Flags.Module) || sym.isOneOf(JavaEnum) || sym.isOneOf(JavaEnumValue) || sym.isAllOf(EnumCase)
    if isModuleLike && hasBind then None
    else
      val pattern =
        if (sym.is(Case) || isModuleLike) && !hasBind then
          if sym.is(Case) &&
            sym.decodedName == name &&
            !Character.isUnicodeIdentifierStart(name.head)
          then
            // Deconstructing the symbol as an infix operator, for example `case head :: tail =>`
            tryInfixPattern(sym, name).getOrElse(
              unapplyPattern(sym, name, isModuleLike)
            )
          else
            unapplyPattern(
              sym,
              name,
              isModuleLike
            ) // Apply syntax, example `case ::(head, tail) =>`
          end if
        else
          typePattern(
            sym,
            name
          ) // Symbol is not a case class with unapply deconstructor so we use typed pattern, example `_: User`
        end if
      end pattern

      val out =
        if patternOnly.isEmpty then s"case $pattern =>"
        else pattern
      Some(out)
    end if
  end labelForCaseMember

  def caseKeywordOnly: List[CompletionValue.Keyword] =
    if patternOnly.isEmpty then
      val label = "case"
      val suffix =
        if clientSupportsSnippets then " $0 =>"
        else " "
      List(
        CompletionValue.Keyword(
          label,
          Some(label + suffix),
        )
      )
    else Nil

  def toCompletionValue(
      denot: Denotation,
      label: String,
      autoImport: Option[l.TextEdit]
  ): CompletionValue.CaseKeyword =
    val cursorSuffix =
      (if patternOnly.nonEmpty then "" else " ") +
        (if clientSupportsSnippets then "$0" else "")
    CompletionValue.CaseKeyword(
      denot,
      label,
      Some(label + cursorSuffix),
      autoImport.toList,
      range = Some(completionPos.toEditRange)
    )
  end toCompletionValue

  private def tryInfixPattern(sym: Symbol, name: String)(using
      Context
  ): Option[String] =
    sym.primaryConstructor.paramSymss match
      case (a :: b :: Nil) :: Nil =>
        Some(
          s"${a.decodedName} $name ${b.decodedName}"
        )
      case _ :: (a :: b :: Nil) :: _ =>
        Some(
          s"${a.decodedName} $name ${b.decodedName}"
        )
      case _ => None

  private def unapplyPattern(
      sym: Symbol,
      name: String,
      isModuleLike: Boolean
  )(using Context): String =
    val suffix =
      if isModuleLike && !(sym.isClass && sym.is(Enum)) then ""
      else
        sym.primaryConstructor.paramSymss match
          case Nil => "()"
          case tparams :: params :: _ =>
            params
              .map(param => param.showName)
              .mkString("(", ", ", ")")
          case head :: _ =>
            head
              .map(param => param.showName)
              .mkString("(", ", ", ")")
    name + suffix
  end unapplyPattern

  private def typePattern(
      sym: Symbol,
      name: String
  )(using Context): String =
    val suffix = sym.typeParams match
      case Nil => ""
      case tparams => tparams.map(_ => "?").mkString("[", ", ", "]")
    val bind = if hasBind then "" else "_: "
    bind + name + suffix
end CompletionValueGenerator

class MatchCaseExtractor(
    pos: SourcePosition,
    text: String,
    completionPos: CompletionPos
):
  object MatchExtractor:
    def unapply(path: List[Tree]) =
      path match
        // foo mat@@
        case (sel @ Select(qualifier, name)) :: _
            if name.toString() != Cursor.value && "match"
              .startsWith(
                name.toString().replace(Cursor.value, "")
              ) && (text
              .charAt(
                completionPos.queryStart - 1
              ) == ' ' || text.charAt(completionPos.queryStart - 1) == '.') =>
          Some(qualifier)
        case _ => None
  end MatchExtractor
  object CaseExtractor:
    def unapply(path: List[Tree])(using
        Context
    ): Option[(Tree, Tree, Option[NewLineOptions])] =
      path match
        // foo match
        // case None => ()
        // ca@@
        case (id @ Ident(name)) :: Block(stats, expr) :: parent :: _
            if "case"
              .startsWith(
                name.toString().replace(Cursor.value, "")
              ) && stats.lastOption.exists(
              _.isInstanceOf[Match]
            ) && expr == id =>
          val selector = stats.last.asInstanceOf[Match].selector
          Some((selector, parent, None))
        // List(Option(1)).collect {
        //   case Some(value) => ()
        //   ca@@
        // }
        case (ident @ Ident(name)) :: Block(
              _,
              expr
            ) :: (cd: CaseDef) :: (m: Match) :: parent :: _
            if ident == expr && "case"
              .startsWith(
                name.toString().replace(Cursor.value, "")
              ) &&
              cd.sourcePos.startLine != pos.startLine =>
          Some((m.selector, parent, None))
        // foo match
        //  ca@@
        case (_: CaseDef) :: (m: Match) :: parent :: _ =>
          Some((m.selector, parent, None))
        // List(foo).map { ca@@ }
        case (ident @ Ident(name)) :: (block @ Block(stats, expr)) ::
            (apply @ Apply(fun, args)) :: _
            if stats.isEmpty && ident == expr && "case".startsWith(
              name.toString().replace(Cursor.value, "")
            ) =>
          val moveToNewLine = ident.sourcePos.line == apply.sourcePos.line
          val addNewLineAfter = apply.sourcePos.endLine == ident.sourcePos.line
          Some(
            (
              EmptyTree,
              apply,
              Some(NewLineOptions(moveToNewLine, addNewLineAfter)),
            )
          )

        case _ => None
  end CaseExtractor

  object CasePatternExtractor:
    def unapply(path: List[Tree])(using Context) =
      path match
        // case @@
        case (c @ CaseDef(
              Literal((Constant(null))),
              _,
              _
            )) :: (m: Match) :: parent :: _
            if pos.start - c.sourcePos.start > 4 =>
          Some((m.selector, parent, ""))
        // case Som@@
        case Ident(name) :: CaseExtractor(selector, parent, _) =>
          Some((selector, parent, name.decoded))
        // case abc @ Som@@
        case Ident(name) :: Bind(_, _) :: CaseExtractor(selector, parent, _) =>
          Some((selector, parent, name.decoded))
        // case abc@@
        case Bind(name, Ident(_)) :: CaseExtractor(selector, parent, _) =>
          Some((selector, parent, name.decoded))
        // case abc @ @@
        case Bind(name, Literal(_)) :: CaseExtractor(selector, parent, _) =>
          Some((selector, parent, ""))
        case _ => None

  end CasePatternExtractor

  object TypedCasePatternExtractor:
    def unapply(path: List[Tree])(using Context) =
      path match
        // case _: Som@@ =>
        case Ident(name) :: Typed(_, _) :: CaseExtractor(selector, parent, _) =>
          Some((selector, parent, name.decoded))
        // case _: @@ =>
        case Typed(_, _) :: CaseExtractor(selector, parent, _) =>
          Some((selector, parent, ""))
        // case ab: @@ =>
        case Bind(_, Typed(_, _)) :: CaseExtractor(selector, parent, _) =>
          Some((selector, parent, ""))
        // case ab: Som@@ =>
        case Ident(name) :: Typed(_, _) :: Bind(_, _) :: CaseExtractor(
              selector,
              parent,
              _
            ) =>
          Some((selector, parent, name.decoded))
        case _ => None
  end TypedCasePatternExtractor

end MatchCaseExtractor

case class NewLineOptions(moveToNewLine: Boolean, addNewLineAfter: Boolean)
