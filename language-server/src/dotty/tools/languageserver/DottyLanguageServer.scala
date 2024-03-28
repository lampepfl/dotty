package dotty.tools
package languageserver

import java.net.URI
import java.io._
import java.nio.file._
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap}
import java.util.function.Function

import com.fasterxml.jackson.databind.ObjectMapper

import org.eclipse.lsp4j

import scala.collection._
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal
import scala.io.Codec
import scala.compiletime.uninitialized

import dotc._
import ast.{Trees, tpd, untpd}
import core._, core.Decorators._
import Comments._, Constants._, Contexts._, Flags._, Names._, NameOps._, Symbols._, SymDenotations._, Trees._, Types._
import classpath.ClassPathEntries
import reporting._
import typer.Typer
import util._
import interactive._, interactive.InteractiveDriver._
import decompiler.IDEDecompilerDriver
import Interactive.Include
import config.Printers.interactiv

import languageserver.config.ProjectConfig
import languageserver.worksheet.{Worksheet, WorksheetService}
import languageserver.decompiler.{TastyDecompilerService}

import lsp4j.services._

/** An implementation of the Language Server Protocol for Dotty.
 *
 *  You should not have to directly this class, instead see `dotty.tools.languageserver.Main`.
 *
 *  For more information see:
 *  - The LSP is defined at https://github.com/Microsoft/language-server-protocol
 *  - This implementation is based on the LSP4J library: https://github.com/eclipse/lsp4j
 */
class DottyLanguageServer extends LanguageServer
    with TextDocumentService with WorkspaceService with WorksheetService with TastyDecompilerService { thisServer =>
  import ast.tpd._

  import DottyLanguageServer._
  import InteractiveDriver._

  import lsp4j.jsonrpc.{CancelChecker, CompletableFutures}
  import lsp4j.jsonrpc.messages.{Either => JEither}
  import lsp4j._

  private var rootUri: String = uninitialized

  private var myClient: DottyClient = uninitialized
  def client: DottyClient = myClient

  private var myDrivers: mutable.Map[ProjectConfig, InteractiveDriver] = uninitialized

  private var myDependentProjects: mutable.Map[ProjectConfig, mutable.Set[ProjectConfig]] = uninitialized

  def drivers: Map[ProjectConfig, InteractiveDriver] = thisServer.synchronized {
    if myDrivers == null then
      assert(rootUri != null, "`drivers` cannot be called before `initialize`")
      val configFile = new File(new URI(rootUri + '/' + IDE_CONFIG_FILE))
      val configs: List[ProjectConfig] = (new ObjectMapper).readValue(configFile, classOf[Array[ProjectConfig]]).toList

      val defaultFlags = List("-color:never" /*, "-Yplain-printer","-Yprint-pos"*/)

      myDrivers = new mutable.HashMap
      for (config <- configs) {
        implicit class updateDeco(ss: List[String]) {
          def update(pathKind: String, pathInfo: String) = {
            val idx = ss.indexOf(pathKind)
            val ss1 = if idx >= 0 then ss.take(idx) ++ ss.drop(idx + 2) else ss
            ss1 ++ List(pathKind, pathInfo)
          }
        }
        val settings =
          defaultFlags ++
          config.compilerArguments.toList
            .update("-d", config.classDirectory.getAbsolutePath)
            .update("-classpath", (config.classDirectory +: config.dependencyClasspath).mkString(File.pathSeparator))
            .update("-sourcepath", config.sourceDirectories.mkString(File.pathSeparator))
        myDrivers(config) = new InteractiveDriver(settings)
      }
    myDrivers
  }

  /** Restart all presentation compiler drivers, copying open files over */
  private def restart() = thisServer.synchronized {
    interactiv.println("restarting presentation compiler")
    val driverConfigs = for ((config, driver) <- myDrivers.toList) yield
      (config, new InteractiveDriver(driver.settings), driver.openedFiles)
    for ((config, driver, _) <- driverConfigs)
      myDrivers(config) = driver
    System.gc()
    for ((_, driver, opened) <- driverConfigs; (uri, source) <- opened)
      driver.run(uri, source)
    if Memory.isCritical() then
      println(s"WARNING: Insufficient memory to run Scala language server on these projects.")
  }

  private def checkMemory() =
    if Memory.isCritical() then
      CompletableFutures.computeAsync { _ => restart() }

  /** The configuration of the project that owns `uri`. */
  def configFor(uri: URI): ProjectConfig = thisServer.synchronized {
    val config =
      drivers.keys.find(config => config.sourceDirectories.exists(sourceDir =>
        new File(uri.getPath).getCanonicalPath.startsWith(sourceDir.getCanonicalPath)))

    config.getOrElse {
      val config = drivers.keys.head
      // println(s"No configuration contains $uri as a source file, arbitrarily choosing ${config.id}")
      config
    }
  }

  /** The driver instance responsible for compiling `uri` */
  def driverFor(uri: URI): InteractiveDriver = {
    drivers(configFor(uri))
  }

    /** The driver instance responsible for decompiling `uri` in `classPath` */
  def decompilerDriverFor(uri: URI, classPath: String): IDEDecompilerDriver = thisServer.synchronized {
    val config = configFor(uri)
    val defaultFlags = List("-color:never")

    implicit class updateDeco(ss: List[String]) {
      def update(pathKind: String, pathInfo: String) = {
        val idx = ss.indexOf(pathKind)
        val ss1 = if idx >= 0 then ss.take(idx) ++ ss.drop(idx + 2) else ss
        ss1 ++ List(pathKind, pathInfo)
      }
    }
    val settings =
      defaultFlags ++
      config.compilerArguments.toList
        .update("-classpath", (classPath +: config.dependencyClasspath).mkString(File.pathSeparator))
    new IDEDecompilerDriver(settings)
  }

  /** A mapping from project `p` to the set of projects that transitively depend on `p`. */
  def dependentProjects: Map[ProjectConfig, Set[ProjectConfig]] = thisServer.synchronized {
    if myDependentProjects == null then
      val idToConfig = drivers.keys.map(k => k.id -> k).toMap
      val allProjects = drivers.keySet

      def transitiveDependencies(config: ProjectConfig): Set[ProjectConfig] = {
        val dependencies = config.projectDependencies.flatMap(idToConfig.get).toSet
        dependencies ++ dependencies.flatMap(transitiveDependencies)
      }

      myDependentProjects = new mutable.HashMap().withDefaultValue(mutable.Set.empty)
      for { project <- allProjects
            dependency <- transitiveDependencies(project) } {
        myDependentProjects(dependency) += project
      }
    myDependentProjects
  }

  def connect(client: DottyClient): Unit = {
    myClient = client
  }

  override def exit(): Unit = {
    System.exit(0)
  }

  override def shutdown(): CompletableFuture[Object] = {
    CompletableFuture.completedFuture(new Object)
  }

  def computeAsync[R](fun: CancelChecker => R, synchronize: Boolean = true): CompletableFuture[R] =
    CompletableFutures.computeAsync { cancelToken =>
      // We do not support any concurrent use of the compiler currently.
      def computation(): R = {
        cancelToken.checkCanceled()
        checkMemory()
        try {
          fun(cancelToken)
        } catch {
          case NonFatal(ex) =>
            ex.printStackTrace
            throw ex
        }
      }
      if synchronize then
        thisServer.synchronized { computation() }
      else
        computation()
    }

  override def initialize(params: InitializeParams) = computeAsync { cancelToken =>
    rootUri = params.getRootUri
    assert(rootUri != null)

    class DottyServerCapabilities(val worksheetRunProvider: Boolean = true,
                                  val tastyDecompiler: Boolean = true) extends lsp4j.ServerCapabilities

    val c = new DottyServerCapabilities
    c.setTextDocumentSync(TextDocumentSyncKind.Full)
    c.setDocumentHighlightProvider(true)
    c.setDocumentSymbolProvider(true)
    c.setDefinitionProvider(true)
    c.setRenameProvider(true)
    c.setHoverProvider(true)
    c.setWorkspaceSymbolProvider(true)
    c.setReferencesProvider(true)
    c.setImplementationProvider(true)
    c.setCompletionProvider(new CompletionOptions(
      /* resolveProvider = */ false,
      /* triggerCharacters = */ List(".").asJava))
    c.setSignatureHelpProvider(new SignatureHelpOptions(
      /* triggerCharacters = */ List("(").asJava))

    // Do most of the initialization asynchronously so that we can return early
    // from this method and thus let the client know our capabilities.
    CompletableFuture.supplyAsync(() => drivers)
      .exceptionally { (ex: Throwable) =>
        ex.printStackTrace
        sys.exit(1)
      }

    new InitializeResult(c)
  }

  override def didOpen(params: DidOpenTextDocumentParams): Unit = thisServer.synchronized {
    checkMemory()
    val document = params.getTextDocument
    val uri = new URI(document.getUri)
    val driver = driverFor(uri)
    implicit def ctx: Context = driver.currentCtx
    val worksheetMode = isWorksheet(uri)

    val text =
      if (worksheetMode)
        wrapWorksheet(document.getText)
      else
        document.getText

    val diags = driver.run(uri, text)

    client.publishDiagnostics(new PublishDiagnosticsParams(
      document.getUri,
      diags.flatMap(diagnostic).asJava))
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val document = params.getTextDocument
    val uri = new URI(document.getUri)
    val worksheetMode = isWorksheet(uri)

    thisServer.synchronized  {
      checkMemory()

      val driver = driverFor(uri)
      implicit def ctx: Context = driver.currentCtx

      val change = params.getContentChanges.get(0)
      assert(change.getRange == null, "TextDocumentSyncKind.Incremental support is not implemented")

      val text =
        if (worksheetMode)
          wrapWorksheet(change.getText)
        else
          change.getText

      val diags = driver.run(uri, text)

      client.publishDiagnostics(new PublishDiagnosticsParams(
        document.getUri,
        diags.flatMap(diagnostic).asJava))
    }
  }

  override def didClose(params: DidCloseTextDocumentParams): Unit = thisServer.synchronized {
    val document = params.getTextDocument
    val uri = new URI(document.getUri)

    driverFor(uri).close(uri)
  }

  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit =
    /*thisServer.synchronized*/ {}

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit =
    /*thisServer.synchronized*/ {}

  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    /*thisServer.synchronized*/ {}
  }

  // FIXME: share code with NotAMember
  override def completion(params: CompletionParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit def ctx: Context = driver.currentCtx

    val pos = sourcePosition(driver, uri, params.getPosition)
    val items = driver.compilationUnits.get(uri) match {
      case Some(unit) =>
        val freshCtx = ctx.fresh.setCompilationUnit(unit)
        Completion.completions(pos)(using freshCtx)._2
      case None => Nil
    }

    JEither.forRight(new CompletionList(
      /*isIncomplete = */ false, items.map(completionItem).asJava))
  }

  /** If cursor is on a reference, show its definition and all overriding definitions.
   *  If cursor is on a definition, show this definition together with all overridden
   *  and overriding definitions.
   */
  override def definition(params: TextDocumentPositionParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit def ctx: Context = driver.currentCtx

    val pos = sourcePosition(driver, uri, params.getPosition)
    val path = Interactive.pathTo(driver.openedTrees(uri), pos)

    val definitions = Interactive.findDefinitions(path, pos, driver).toList
    definitions.flatMap(d => location(d.namePos)).asJava
  }

  override def references(params: ReferenceParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)

    val includes = {
      val includeDeclaration = params.getContext.isIncludeDeclaration
      Include.references | Include.overriding | Include.imports | Include.local |
        (if (includeDeclaration) Include.definitions else Include.empty)
    }

    val uriTrees = driver.openedTrees(uri)
    val pos = sourcePosition(driver, uri, params.getPosition)

    val (definitions, originalSymbols) = {
      implicit def ctx: Context = driver.currentCtx
      val path = Interactive.pathTo(driver.openedTrees(uri), pos)
      val definitions = Interactive.findDefinitions(path, pos, driver)
      val originalSymbols = Interactive.enclosingSourceSymbols(path, pos)

      (definitions, originalSymbols)
    }

    val references = {
      // Collect the information necessary to look into each project separately: representation of
      // `originalSymbol` in this project, the context and correct Driver.
      val perProjectInfo = inProjectsSeeing(driver, definitions, originalSymbols)

      perProjectInfo.flatMap { (remoteDriver, ctx, definitions) =>
        definitions.flatMap { definition =>
          val name = definition.name(using ctx).sourceModuleName.toString
          val trees = remoteDriver.sourceTreesContaining(name)(using ctx)
          val matches = Interactive.findTreesMatching(trees, includes, definition)(using ctx)
          matches.map(tree => location(tree.namePos(using ctx)))
        }
      }
    }.toList

    references.flatten.distinct.asJava
  }

  override def rename(params: RenameParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit def ctx: Context = driver.currentCtx

    val uriTrees = driver.openedTrees(uri)
    val pos = sourcePosition(driver, uri, params.getPosition)
    val path = Interactive.pathTo(uriTrees, pos)
    val syms = Interactive.enclosingSourceSymbols(path, pos)
    val newName = params.getNewName

    def findRenamedReferences(trees: List[SourceTree], syms: List[Symbol], withName: Name): List[SourceTree] = {
      val includes = Include.all
      syms.flatMap { sym =>
        Interactive.findTreesMatching(trees, Include.all, sym, t => Interactive.sameName(t.name, withName))
      }
    }

    val refs =
      path match {
        // Selected a renaming in an import node
        case untpd.ImportSelector(_, rename: untpd.Ident, _) :: (_: Import) :: rest if rename.span.contains(pos.span) =>
          findRenamedReferences(uriTrees, syms, rename.name)

        // Selected a reference that has been renamed
        case (nameTree: NameTree) :: rest if Interactive.isRenamed(nameTree) =>
          findRenamedReferences(uriTrees, syms, nameTree.name)

        case _ =>
          val (include, allSymbols) =
            if (syms.exists(_.allOverriddenSymbols.nonEmpty)) {
              showMessageRequest(MessageType.Info,
                RENAME_OVERRIDDEN_QUESTION,
                List(
                  RENAME_OVERRIDDEN    -> (() => (Include.all, syms.flatMap(s => s :: s.allOverriddenSymbols.toList))),
                  RENAME_NO_OVERRIDDEN -> (() => (Include.all.except(Include.overridden), syms)))
              ).get.getOrElse((Include.empty, Nil))
            } else {
              (Include.all, syms)
            }

          val names = allSymbols.map(_.name.sourceModuleName).toSet
          val definitions = Interactive.findDefinitions(allSymbols, driver, include.isOverridden, includeExternal = true)
          val perProjectInfo = inProjectsSeeing(driver, definitions, allSymbols)

          perProjectInfo.flatMap { (remoteDriver, ctx, definitions) =>
            definitions.flatMap { definition =>
              val name = definition.name(using ctx).sourceModuleName.toString
              val trees = remoteDriver.sourceTreesContaining(name)(using ctx)
              Interactive.findTreesMatching(trees,
                                            include,
                                            definition,
                                            t => names.exists(Interactive.sameName(t.name, _)))(using ctx)
            }
          }
      }

    val changes =
      refs.groupBy(ref => toUriOption(ref.source))
        .flatMap { case (uriOpt, refs) => uriOpt.map(uri => (uri.toString, refs)) }
        .transform((_, refs) => refs.flatMap(ref =>
          range(ref.namePos).map(nameRange => new TextEdit(nameRange, newName))).distinct.asJava)

    new WorkspaceEdit(changes.asJava)
  }

  override def documentHighlight(params: TextDocumentPositionParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit def ctx: Context = driver.currentCtx

    val pos = sourcePosition(driver, uri, params.getPosition)
    val uriTrees = driver.openedTrees(uri)
    val path = Interactive.pathTo(uriTrees, pos)
    val syms = Interactive.enclosingSourceSymbols(path, pos)
    val includes = Include.all.except(Include.linkedClass)

    syms.flatMap { sym =>
      val refs = Interactive.findTreesMatching(uriTrees, includes, sym)
      (for {
        ref <- refs
        nameRange <- range(ref.namePos)
      } yield new DocumentHighlight(nameRange, DocumentHighlightKind.Read))
    }.distinct.asJava
  }

  override def hover(params: TextDocumentPositionParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit def ctx: Context = driver.currentCtx

    val pos = sourcePosition(driver, uri, params.getPosition)
    val trees = driver.openedTrees(uri)
    val path = Interactive.pathTo(trees, pos)
    val tp = Interactive.enclosingType(trees, pos)
    val tpw = tp.widenTermRefExpr

    if (tp.isError || tpw == NoType) null // null here indicates that no response should be sent
    else {
      Interactive.enclosingSourceSymbols(path, pos) match {
        case Nil =>
          null
        case symbols =>
          val docComments = symbols.flatMap(ParsedComment.docOf)
          val content = hoverContent(Some(tpw.show), docComments)
          new Hover(content, null)
      }
    }
  }

  override def documentSymbol(params: DocumentSymbolParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit def ctx: Context = driver.currentCtx

    val uriTrees = driver.openedTrees(uri)

    // Excludes type and term params from synthetic symbols
    val excludeParamsFromSyntheticSymbols = (n: NameTree) => {
      val owner = n.symbol.owner
      !n.symbol.is(Param) || {
        !owner.is(Synthetic) &&
        !owner.isPrimaryConstructor
      }
    }

    val defs = Interactive.namedTrees(uriTrees, Include.local, excludeParamsFromSyntheticSymbols)

    (for {
      d <- defs if (!isWorksheetWrapper(d) && !isTopLevelWrapper(d))
      info <- symbolInfo(d.tree.symbol, d.pos)
    } yield JEither.forLeft(info)).asJava
  }

  override def symbol(params: WorkspaceSymbolParams) = computeAsync { cancelToken =>
    val query = params.getQuery

    drivers.values.toList.flatMap { driver =>
      implicit def ctx: Context = driver.currentCtx

      val trees = driver.sourceTreesContaining(query)
      val defs = Interactive.namedTrees(trees, Include.empty, _.name.toString.contains(query))
      defs.flatMap(d => symbolInfo(d.tree.symbol, d.namePos))
    }.asJava
  }

  override def implementation(params: TextDocumentPositionParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)

    val pos = sourcePosition(driver, uri, params.getPosition)

    val (definitions, originalSymbols) = {
      implicit def ctx: Context = driver.currentCtx
      val path = Interactive.pathTo(driver.openedTrees(uri), pos)
      val originalSymbols = Interactive.enclosingSourceSymbols(path, pos)
      val definitions = Interactive.findDefinitions(path, pos, driver)
      (definitions, originalSymbols)
    }

    val implementations = {
      val perProjectInfo = inProjectsSeeing(driver, definitions, originalSymbols)

      perProjectInfo.flatMap { (remoteDriver, ctx, definitions) =>
        val trees = remoteDriver.sourceTrees(using ctx)
        val predicate: NameTree => Boolean = {
          val predicates = definitions.map(Interactive.implementationFilter(_)(using ctx))
          tree => predicates.exists(_(tree))
        }
        val matches = Interactive.namedTrees(trees, Include.local, predicate)(using ctx)
        matches.map(tree => location(tree.namePos(using ctx)))
      }
    }.toList

    implementations.flatten.asJava
  }

  override def signatureHelp(params: TextDocumentPositionParams) = computeAsync { canceltoken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)

    driver.compilationUnits.get(uri) match
      case Some(unit) =>
        given newCtx: Context = driver.currentCtx.fresh.setCompilationUnit(unit)
        val pos = sourcePosition(driver, uri, params.getPosition)
        val adjustedSpan = pos.span.withEnd(pos.span.end + 1)
        val path = Interactive.pathTo(ctx.compilationUnit.tpdTree, adjustedSpan)
        val (paramN, callableN, signatures) = Signatures.signatureHelp(path, adjustedSpan)

        new SignatureHelp(signatures.map(signatureToSignatureInformation).asJava, callableN, paramN)

      case _ => new SignatureHelp()

  }

  override def getTextDocumentService: TextDocumentService = this
  override def getWorkspaceService: WorkspaceService = this

  // Unimplemented features. If you implement one of them, you may need to add a
  // capability in `initialize`
  override def codeAction(params: CodeActionParams) = null
  override def codeLens(params: CodeLensParams) = null
  override def formatting(params: DocumentFormattingParams) = null
  override def rangeFormatting(params: DocumentRangeFormattingParams) = null
  override def onTypeFormatting(params: DocumentOnTypeFormattingParams) = null
  override def resolveCodeLens(params: CodeLens) = null
  override def resolveCompletionItem(params: CompletionItem) = null

  /**
   * Find the set of projects that have any of `definitions` on their classpath.
   *
   * @param definitions The definitions to consider when looking for projects.
   * @return The set of projects that have any of `definitions` on their classpath.
   */
  private def projectsSeeing(definitions: List[SourceTree])(implicit ctx: Context): Set[ProjectConfig] = {
    if (definitions.isEmpty) {
      drivers.keySet
    } else {
      for {
        definition <- definitions.toSet
        uri <- toUriOption(definition.pos.source).toSet
        config = configFor(uri)
        project <- dependentProjects(config) union Set(config)
      } yield project
    }
  }

  /**
   * Finds projects that can see any of `definitions`, translate `symbols` in their universe.
   *
   * @param baseDriver  The driver responsible for the trees in `definitions` and `symbol`.
   * @param definitions The definitions to consider when looking for projects.
   * @param symbol      Symbols to translate in the universes of the remote projects.
   * @return A list consisting of the remote drivers, their context, and the translation of `symbol`
   *         into their universe.
   */
  private def inProjectsSeeing(baseDriver: InteractiveDriver,
                               definitions: List[SourceTree],
                               symbols: List[Symbol]): List[(InteractiveDriver, Context, List[Symbol])] = {
    val projects = projectsSeeing(definitions)(baseDriver.currentCtx)
    projects.toList.map { config =>
      val remoteDriver = drivers(config)
      val ctx = remoteDriver.currentCtx
      val definitions = symbols.map(Interactive.localize(_, baseDriver, remoteDriver))
      (remoteDriver, ctx, definitions)
    }
  }

  /**
   * Send a `window/showMessageRequest` to the client, asking to choose between `choices`, and
   * perform the associated operation.
   *
   * @param tpe     The type of the request
   * @param message The message accompanying the request
   * @param choices The choices and their associated operation
   * @return A future that will complete with the result of executing the action corresponding to
   *         the user's response.
   */
  private def showMessageRequest[T](tpe: MessageType,
                                    message: String,
                                    choices: List[(String, () => T)]): CompletableFuture[Option[T]] = {
    val options = choices.map((title, _) => new MessageActionItem(title))
    val request = new ShowMessageRequestParams(options.asJava)
    request.setMessage(message)
    request.setType(tpe)

    client.showMessageRequest(request).thenApply { (message: MessageActionItem) =>
      for {
        answer <- Option(message)
        (_, action) <- choices.find(_._1 == answer.getTitle)
      } yield action()
    }
  }
}

object DottyLanguageServer {

  final val IDE_CONFIG_FILE = ".dotty-ide.json"

  final val RENAME_OVERRIDDEN_QUESTION = "Do you want to rename the base member, or only this member?"
  final val RENAME_OVERRIDDEN= "Rename the base member"
  final val RENAME_NO_OVERRIDDEN = "Rename only this member"

  /** Convert an lsp4j.Position to a SourcePosition */
  def sourcePosition(driver: InteractiveDriver, uri: URI, pos: lsp4j.Position): SourcePosition = {
    val actualPosition =
      if (isWorksheet(uri)) toWrappedPosition(pos)
      else pos
    val source = driver.openedFiles(uri)
    if (source.exists)
      source.lineToOffsetOpt(actualPosition.getLine).map(_ + actualPosition.getCharacter) match {
        // `<=` to allow an offset to point to the end of the file
        case Some(offset) if offset <= source.content().length =>
          val p = Spans.Span(offset)
          new SourcePosition(source, p)
        case _ =>
          NoSourcePosition
      }
    else NoSourcePosition
  }

  /** Convert a SourcePosition to an lsp4j.Range */
  def range(p: SourcePosition): Option[lsp4j.Range] =
    if (p.exists) {
      val mappedPosition = positionMapperFor(p.source).map(_(p)).getOrElse(p)
      Some(new lsp4j.Range(
        new lsp4j.Position(mappedPosition.startLine, mappedPosition.startColumn),
        new lsp4j.Position(mappedPosition.endLine, mappedPosition.endColumn)
      ))
    } else
      None

  /** Convert a SourcePosition to an lsp4.Location */
  def location(p: SourcePosition): Option[lsp4j.Location] =
    for {
      uri <- toUriOption(p.source)
      r <- range(p)
    } yield new lsp4j.Location(uri.toString, r)

  /**
   * Convert a Diagnostic to an lsp4j.Diagnostic.
   */
  def diagnostic(dia: Diagnostic)(implicit ctx: Context): Option[lsp4j.Diagnostic] =
    if (!dia.pos.exists)
      None // diagnostics without positions are not supported: https://github.com/Microsoft/language-server-protocol/issues/249
    else {
      def severity(level: Int): lsp4j.DiagnosticSeverity = {
        import interfaces.{Diagnostic => D}
        import lsp4j.{DiagnosticSeverity => DS}

        level match {
          case D.INFO =>
            DS.Information
          case D.WARNING =>
            DS.Warning
          case D.ERROR =>
            DS.Error
        }
      }

      val message = dia.msg
      if (displayMessage(message, dia.pos.source)) {
        val code = message.errorId.errorNumber.toString
        range(dia.pos).map(r =>
            new lsp4j.Diagnostic(
              r, dia.message, severity(dia.level), /*source =*/ "", code))
      } else {
        None
      }
    }

  /**
   * Check whether `message` should be displayed in the IDE.
   *
   * Currently we only filter out the warning about pure expressions in statement position when they
   * are immediate children of the worksheet wrapper.
   *
   * @param message    The message to filter.
   * @param sourceFile The sourcefile from which `message` originates.
   * @return true if the message should be displayed in the IDE, false otherwise.
   */
  private def displayMessage(message: Message, sourceFile: SourceFile)(implicit ctx: Context): Boolean = {
    if (isWorksheet(sourceFile)) {
      message match {
        case msg: PureExpressionInStatementPosition =>
          val ownerSym = if (msg.exprOwner.isLocalDummy) msg.exprOwner.owner else msg.exprOwner
          !isWorksheetWrapper(ownerSym)
        case _ =>
          true
      }
    } else {
      true
    }
  }

  /** Does this URI represent a worksheet? */
  private def isWorksheet(uri: URI): Boolean =
    uri.toString.endsWith(".sc")

  /** Does this sourcefile represent a worksheet? */
  private def isWorksheet(sourcefile: SourceFile): Boolean =
    sourcefile.file.ext.isScalaScript

  /** Wrap the source of a worksheet inside an `object`. */
  private def wrapWorksheet(source: String): String =
    s"""object ${StdNames.nme.WorksheetWrapper} {
       |$source
       |}""".stripMargin

  /**
   * Map `position` in a wrapped worksheet to the same position in the unwrapped source.
   *
   * Because worksheet are wrapped in an `object`, the positions in the source are one line
   * above from what the compiler sees.
   *
   * @see wrapWorksheet
   * @param position The position as seen by the compiler (after wrapping)
   * @return The position in the actual source file (before wrapping).
   */
  private def toUnwrappedPosition(position: SourcePosition): SourcePosition = {
    new SourcePosition(position.source, position.span, position.outer) {
      override def startLine: Int = position.startLine - 1
      override def endLine: Int = position.endLine - 1
    }
  }

  /**
   * Map `position` in an unwrapped worksheet to the same position in the wrapped source.
   *
   * Because worksheet are wrapped in an `object`, the positions in the source are one line
   * above from what the compiler sees.
   *
   * @see wrapWorksheet
   * @param position The position as seen by VSCode (before wrapping)
   * @return The position as seen by the compiler (after wrapping)
   */
  private def toWrappedPosition(position: lsp4j.Position): lsp4j.Position = {
    new lsp4j.Position(position.getLine + 1, position.getCharacter)
  }

  /**
   * Returns the position mapper necessary to unwrap positions for `sourcefile`. If `sourcefile` is
   * not a worksheet, no mapper is necessary. Otherwise, return `toUnwrappedPosition`.
   */
  private def positionMapperFor(sourcefile: SourceFile): Option[SourcePosition => SourcePosition] = {
    if (isWorksheet(sourcefile)) Some(toUnwrappedPosition)
    else None
  }

  /**
   * Is `sourceTree` the wrapper object that we put around worksheet sources?
   *
   * @see wrapWorksheet
   */
  def isWorksheetWrapper(sourceTree: SourceTree)(implicit ctx: Context): Boolean = {
    isWorksheet(sourceTree.source) && isWorksheetWrapper(sourceTree.tree.symbol)
  }

  /**
   * Is this symbol the wrapper object that we put around worksheet sources?
   *
   * @see wrapWorksheet
   */
  def isWorksheetWrapper(symbol: Symbol)(implicit ctx: Context): Boolean = {
      symbol.name == StdNames.nme.WorksheetWrapper.moduleClassName &&
      symbol.owner == ctx.definitions.EmptyPackageClass
  }

  /**
   * Is this symbol the wrapper object for top level definitions?
   */
  def isTopLevelWrapper(sourceTree: SourceTree)(implicit ctx: Context): Boolean = {
    val symbol = sourceTree.tree.symbol
    symbol.isPackageObject
  }

  /** Create an lsp4j.CompletionItem from a completion result */
  def completionItem(completion: Completion)(implicit ctx: Context): lsp4j.CompletionItem = {
    def completionItemKind(sym: Symbol)(implicit ctx: Context): lsp4j.CompletionItemKind = {
      import lsp4j.{CompletionItemKind => CIK}

      if sym.is(Package) || sym.is(Module) then
        CIK.Module // No CompletionItemKind.Package (https://github.com/Microsoft/language-server-protocol/issues/155)
      else if sym.isConstructor then
        CIK.Constructor
      else if sym.isClass then
        CIK.Class
      else if sym.is(Mutable) then
        CIK.Variable
      else if sym.is(Method) then
        CIK.Method
      else
        CIK.Field
    }

    val item = new lsp4j.CompletionItem(completion.label)
    item.setDetail(completion.description)

    val documentation = for {
      sym <- completion.symbols
      doc <- ParsedComment.docOf(sym)
    } yield doc

    if (documentation.nonEmpty) {
      item.setDocumentation(hoverContent(None, documentation))
    }

    item.setDeprecated(completion.symbols.forall(_.hasAnnotation(defn.DeprecatedAnnot)))
    completion.symbols.headOption.foreach(s => item.setKind(completionItemKind(s)))
    item
  }

  def markupContent(content: String): lsp4j.MarkupContent = {
    if content.isEmpty then
      null
    else {
      val markup = new lsp4j.MarkupContent
      markup.setKind("markdown")
      markup.setValue(content.trim)
      markup
    }
  }

  private def hoverContent(typeInfo: Option[String],
                           comments: List[ParsedComment]
                          )(implicit ctx: Context): lsp4j.MarkupContent = {
    val buf = new StringBuilder
    typeInfo.foreach { info =>
      buf.append(s"""```scala
                    |$info
                    |```
                    |""".stripMargin)
    }
    comments.foreach { comment =>
      buf.append(comment.renderAsMarkdown)
    }

    markupContent(buf.toString)
  }

  /** Create an lsp4j.SymbolInfo from a Symbol and a SourcePosition */
  def symbolInfo(sym: Symbol, pos: SourcePosition)(implicit ctx: Context): Option[lsp4j.SymbolInformation] = {
    def symbolKind(sym: Symbol)(implicit ctx: Context): lsp4j.SymbolKind = {
      import lsp4j.{SymbolKind => SK}

      if (sym.is(Package))
        SK.Package
      else if (sym.isConstructor)
        SK.Constructor
      else if (sym.is(Module))
        SK.Module
      else if (sym.isAllOf(EnumCase) || sym.isAllOf(EnumValue))
        SK.EnumMember
      else if (sym.is(Enum))
        SK.Enum
      else if (sym.is(Trait))
        SK.Interface
      else if (sym.isClass)
        SK.Class
      else if (sym.is(Mutable))
        SK.Variable
      else if (sym.is(Method))
        SK.Method
      else if (sym.is(TypeParam) || sym.isAbstractOrAliasType)
        SK.TypeParameter
      else
        SK.Field
    }
    def containerName(sym: Symbol): String = {
      val owner = if (sym.owner.exists && sym.owner.isPackageObject) sym.owner.owner else sym.owner
      if (owner.exists && !owner.isEmptyPackage) {
          owner.name.stripModuleClassSuffix.show
      } else
        null
    }

    val name = sym.name.stripModuleClassSuffix.show

    location(pos).map(l => new lsp4j.SymbolInformation(name, symbolKind(sym), l, containerName(sym)))
  }

  /** Convert `signature` to a `SignatureInformation` */
  def signatureToSignatureInformation(signature: Signatures.Signature): lsp4j.SignatureInformation = {
    val paramInfoss =
      (signature.paramss.flatten).map(paramToParameterInformation)
    val paramLists =
      signature.paramss
        .map { paramList =>
          val labels = paramList.map(_.show)
          val isImplicit = paramList.exists:
            case p: Signatures.MethodParam => p.isImplicit
            case _ => false
          val prefix = if isImplicit then "using " else ""
          val isTypeParams = paramList.forall(_.isInstanceOf[Signatures.TypeParam]) && paramList.nonEmpty
          val wrap: String => String = label => if isTypeParams then
            s"[$label]"
          else
            s"($label)"
          wrap(labels.mkString(prefix, ", ", ""))
        }.mkString
    val returnTypeLabel = signature.returnType.map(t => s": $t").getOrElse("")
    val label = s"${signature.name}$paramLists$returnTypeLabel"
    val documentation = signature.doc.map(DottyLanguageServer.markupContent)
    val sig = new lsp4j.SignatureInformation(label)
    sig.setParameters(paramInfoss.asJava)
    documentation.foreach(sig.setDocumentation(_))
    sig
  }

  /** Convert `param` to `ParameterInformation` */
  private def paramToParameterInformation(param: Signatures.Param): lsp4j.ParameterInformation = {
    val documentation = param.doc.map(DottyLanguageServer.markupContent)
    val info = new lsp4j.ParameterInformation(param.show)
    documentation.foreach(info.setDocumentation(_))
    info
  }
}
