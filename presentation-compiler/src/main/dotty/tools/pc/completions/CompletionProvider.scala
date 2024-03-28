package dotty.tools.pc
package completions

import java.nio.file.Path

import scala.jdk.CollectionConverters._
import scala.meta.internal.metals.ReportContext
import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolSearch

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.pc.AutoImports.AutoImportEdits
import dotty.tools.pc.AutoImports.AutoImportsGenerator
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.printer.ShortenedTypePrinter.IncludeDefaultParam
import dotty.tools.pc.utils.MtagsEnrichments.*

import org.eclipse.lsp4j.Command
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.CompletionList
import org.eclipse.lsp4j.InsertTextFormat
import org.eclipse.lsp4j.InsertTextMode
import org.eclipse.lsp4j.Range as LspRange
import org.eclipse.lsp4j.TextEdit

class CompletionProvider(
    search: SymbolSearch,
    driver: InteractiveDriver,
    params: OffsetParams,
    config: PresentationCompilerConfig,
    buildTargetIdentifier: String,
    folderPath: Option[Path]
)(using reports: ReportContext):
  def completions(): CompletionList =
    val uri = params.uri().nn
    val text = params.text().nn

    val code = applyCompletionCursor(params)
    val sourceFile = SourceFile.virtual(uri, code)
    driver.run(uri, sourceFile)

    val ctx = driver.currentCtx
    val pos = driver.sourcePosition(params)
    val (items, isIncomplete) = driver.compilationUnits.get(uri) match
      case Some(unit) =>

        val newctx = ctx.fresh.setCompilationUnit(unit).withPhase(Phases.typerPhase(using ctx))
        val tpdPath = Interactive.pathTo(newctx.compilationUnit.tpdTree, pos.span)(using newctx)
        val adjustedPath = Interactive.resolveTypedOrUntypedPath(tpdPath, pos)(using newctx)

        val locatedCtx = Interactive.contextOfPath(tpdPath)(using newctx)
        val indexedCtx = IndexedContext(locatedCtx)

        val completionPos = CompletionPos.infer(pos, params, adjustedPath)(using locatedCtx)

        val autoImportsGen = AutoImports.generator(
          completionPos.toSourcePosition,
          text,
          unit.tpdTree,
          unit.comments,
          indexedCtx,
          config
        )

        val (completions, searchResult) =
          new Completions(
            text,
            locatedCtx,
            search,
            buildTargetIdentifier,
            completionPos,
            indexedCtx,
            tpdPath,
            adjustedPath,
            config,
            folderPath,
            autoImportsGen,
            unit.comments,
            driver.settings
          ).completions()

        val items = completions.zipWithIndex.map { case (item, idx) =>
          completionItems(
            item,
            idx,
            autoImportsGen,
            completionPos,
            tpdPath,
            indexedCtx
          )(using locatedCtx)
        }
        val isIncomplete = searchResult match
          case SymbolSearch.Result.COMPLETE => false
          case SymbolSearch.Result.INCOMPLETE => true
        (items, isIncomplete)
      case None => (Nil, false)

    new CompletionList(
      isIncomplete,
      items.asJava
    )
  end completions

  /**
   * In case if completion comes from empty line like:
   * {{{
   * class Foo:
   *   val a = 1
   *   @@
   * }}}
   * it's required to modify actual code by additional Ident.
   *
   * Otherwise, completion poisition doesn't point at any tree
   * because scala parser trim end position to the last statement pos.
   */
  private def applyCompletionCursor(params: OffsetParams): String =
    val text = params.text().nn
    val offset = params.offset().nn

    val isStartMultilineComment =
      val i = params.offset()
      i >= 3 && (text.charAt(i - 1) match
        case '*' =>
          text.charAt(i - 2) == '*' &&
          text.charAt(i - 3) == '/'
        case _ => false
      )
    if isStartMultilineComment then
      // Insert potentially missing `*/` to avoid comment out all codes after the "/**".
      text.substring(0, offset).nn + Cursor.value + "*/" + text.substring(offset)
    else
      text.substring(0, offset).nn + Cursor.value + text.substring(offset)
  end applyCompletionCursor

  private def completionItems(
      completion: CompletionValue,
      idx: Int,
      autoImports: AutoImportsGenerator,
      completionPos: CompletionPos,
      path: List[Tree],
      indexedContext: IndexedContext
  )(using ctx: Context): CompletionItem =
    val printer =
      ShortenedTypePrinter(search, IncludeDefaultParam.ResolveLater)(using indexedContext)

    // For overloaded signatures we get multiple symbols, so we need
    // to recalculate the description
    // related issue https://github.com/scala/scala3/issues/11941
    lazy val kind: CompletionItemKind = completion.completionItemKind
    val description = completion.description(printer)
    val label = completion.labelWithDescription(printer)
    val ident = completion.insertText.getOrElse(completion.label)

    def mkItem(
        newText: String,
        additionalEdits: List[TextEdit] = Nil,
        range: Option[LspRange] = None
    ): CompletionItem =
      val oldText = params.text().nn.substring(completionPos.queryStart, completionPos.identEnd)
      val editRange = if newText.startsWith(oldText) then completionPos.stripSuffixEditRange
        else completionPos.toEditRange

      val textEdit = new TextEdit(range.getOrElse(editRange), newText)

      val item = new CompletionItem(label)
      item.setSortText(f"${idx}%05d")
      item.setDetail(description)
      item.setFilterText(completion.filterText.getOrElse(completion.label))
      item.setTextEdit(textEdit)
      item.setAdditionalTextEdits((completion.additionalEdits ++ additionalEdits).asJava)
      completion.insertMode.foreach(item.setInsertTextMode)

      val data = completion.completionData(buildTargetIdentifier)
      item.setData(data.toJson)

      item.setTags(completion.lspTags.asJava)

      if config.isCompletionSnippetsEnabled() then
        item.setInsertTextFormat(InsertTextFormat.Snippet)

      completion.command.foreach { command =>
        item.setCommand(new Command("", command))
      }

      item.setKind(kind)
      item
    end mkItem

    val completionTextSuffix = completion.snippetAffix.toSuffix
    val completionTextPrefix = completion.snippetAffix.toPrefix

    lazy val isInStringInterpolation =
      path match
        // s"My name is $name"
        case (_: Ident) :: (_: SeqLiteral) :: (_: Typed) :: Apply(
              Select(Apply(Select(Select(_, name), _), _), _),
              _
            ) :: _ =>
          name == StdNames.nme.StringContext
        // "My name is $name"
        case Literal(Constant(_: String)) :: _ =>
          true
        case _ =>
          false

    lazy val backtickSoftKeyword = path match
      case (_: Select) :: _ => false
      case _ => true

    def mkItemWithImports(
        v: CompletionValue.Workspace | CompletionValue.Extension |
          CompletionValue.Interpolator | CompletionValue.ImplicitClass
    ) =
      val sym = v.symbol
      path match
        case (_: Ident) :: (_: Import) :: _ =>
          mkItem(sym.fullNameBackticked(backtickSoftKeyword = false))
        case _ =>
          autoImports.editsForSymbol(v.importSymbol) match
            case Some(edits) =>
              edits match
                case AutoImportEdits(Some(nameEdit), other) =>
                  mkItem(nameEdit.getNewText().nn, other.toList, range = Some(nameEdit.getRange().nn))
                case _ =>
                  mkItem(
                    v.insertText.getOrElse(completionTextPrefix + ident.backticked(backtickSoftKeyword) + completionTextSuffix),
                    edits.edits,
                    range = v.range
                  )
            case None =>
              val r = indexedContext.lookupSym(sym)
              r match
                case IndexedContext.Result.InScope =>
                  mkItem(
                    v.insertText.getOrElse(
                      completionTextPrefix +
                      ident.backticked(
                        backtickSoftKeyword
                      ) + completionTextSuffix
                    ),
                    range = v.range,
                  )
                case _ if isInStringInterpolation =>
                  mkItem(
                    "{" + completionTextPrefix + sym.fullNameBackticked + completionTextSuffix + "}",
                    range = v.range
                  )
                case _ if v.isExtensionMethod =>
                  mkItem(
                    completionTextPrefix + ident.backticked(backtickSoftKeyword) + completionTextSuffix,
                    range = v.range
                  )
                case _ =>
                  mkItem(
                    completionTextPrefix + sym.fullNameBackticked(
                      backtickSoftKeyword
                    ) + completionTextSuffix,
                    range = v.range
                  )
              end match
          end match
      end match
    end mkItemWithImports

    completion match
      case v: (CompletionValue.Workspace | CompletionValue.Extension | CompletionValue.ImplicitClass) =>
        mkItemWithImports(v)
      case v: CompletionValue.Interpolator if v.isWorkspace || v.isExtension =>
        mkItemWithImports(v)
      case _ =>
        val nameText = completion.insertText.getOrElse(ident.backticked(backtickSoftKeyword))
        val nameWithAffixes = completionTextPrefix + nameText + completionTextSuffix
        val insertText = if completion.snippetAffix.nonEmpty && isInStringInterpolation then
          "{" + nameWithAffixes + "}"
        else nameWithAffixes
        mkItem(insertText, range = completion.range)

    end match
  end completionItems
end CompletionProvider
