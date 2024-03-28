package dotty.tools.repl

import scala.language.unsafeNulls

import java.io.{File => JFile, PrintStream}
import java.nio.charset.StandardCharsets

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.config.CommandLineParser.tokenize
import dotty.tools.dotc.config.Properties.{javaVersion, javaVmName, simpleVersionString}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Phases.{unfusedPhases, typerPhase}
import dotty.tools.dotc.core.Denotations.Denotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.NameKinds.SimpleNameKind
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.{Symbol, defn}
import dotty.tools.dotc.interfaces
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.printing.SyntaxHighlighting
import dotty.tools.dotc.reporting.{ConsoleReporter, StoreReporter}
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.{CompilationUnit, Driver}
import dotty.tools.dotc.config.CompilerCommand
import dotty.tools.io.*
import dotty.tools.repl.Rendering.showUser
import dotty.tools.runner.ScalaClassLoader.*
import org.jline.reader.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal
import scala.util.Using

/** The state of the REPL contains necessary bindings instead of having to have
 *  mutation
 *
 *  The compiler in the REPL needs to do some wrapping in order to compile
 *  valid code. This wrapping occurs when a single `MemberDef` that cannot be
 *  top-level needs to be compiled. In order to do this, we need some unique
 *  identifier for each of these wrappers. That identifier is `objectIndex`.
 *
 *  Free expressions such as `1 + 1` needs to have an assignment in order to be
 *  of use. These expressions are therefore given a identifier on the format
 *  `resX` where `X` starts at 0 and each new expression that needs an
 *  identifier is given the increment of the old identifier. This identifier is
 *  `valIndex`.
 *
 *  @param objectIndex the index of the next wrapper
 *  @param valIndex    the index of next value binding for free expressions
 *  @param imports     a map from object index to the list of user defined imports
 *  @param invalidObjectIndexes the set of object indexes that failed to initialize
 *  @param context     the latest compiler context
 */
case class State(objectIndex: Int,
                 valIndex: Int,
                 imports: Map[Int, List[tpd.Import]],
                 invalidObjectIndexes: Set[Int],
                 context: Context):
  def validObjectIndexes = (1 to objectIndex).filterNot(invalidObjectIndexes.contains(_))

/** Main REPL instance, orchestrating input, compilation and presentation */
class ReplDriver(settings: Array[String],
                 out: PrintStream = Console.out,
                 classLoader: Option[ClassLoader] = None) extends Driver:

  /** Overridden to `false` in order to not have to give sources on the
   *  commandline
   */
  override def sourcesRequired: Boolean = false

  /** Create a fresh and initialized context with IDE mode enabled */
  private def initialCtx(settings: List[String]) = {
    val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions | Mode.Interactive)
    rootCtx.setSetting(rootCtx.settings.YcookComments, true)
    rootCtx.setSetting(rootCtx.settings.YreadComments, true)
    setupRootCtx(this.settings ++ settings, rootCtx)
  }

  private def setupRootCtx(settings: Array[String], rootCtx: Context) = {
    setup(settings, rootCtx) match
      case Some((files, ictx)) => inContext(ictx) {
        shouldStart = true
        if files.nonEmpty then out.println(i"Ignoring spurious arguments: $files%, %")
        ictx.base.initialize()
        ictx
      }
      case None =>
        shouldStart = false
        rootCtx
  }

  /** the initial, empty state of the REPL session */
  final def initialState: State = State(0, 0, Map.empty, Set.empty, rootCtx)

  /** Reset state of repl to the initial state
   *
   *  This method is responsible for performing an all encompassing reset. As
   *  such, when the user enters `:reset` this method should be called to reset
   *  everything properly
   */
  protected def resetToInitial(settings: List[String] = Nil): Unit = {
    rootCtx = initialCtx(settings)
    if (rootCtx.settings.outputDir.isDefault(using rootCtx))
      rootCtx = rootCtx.fresh
        .setSetting(rootCtx.settings.outputDir, new VirtualDirectory("<REPL compilation output>"))
    compiler = new ReplCompiler
    rendering = new Rendering(classLoader)
  }

  private var rootCtx: Context = uninitialized
  private var shouldStart: Boolean = uninitialized
  private var compiler: ReplCompiler = uninitialized
  protected var rendering: Rendering = uninitialized

  // initialize the REPL session as part of the constructor so that once `run`
  // is called, we're in business
  resetToInitial()

  override protected def command: CompilerCommand = ReplCommand

  /** Try to run REPL if there is nothing that prevents us doing so.
   *
   *  Possible reason for unsuccessful run are raised flags in CLI like --help or --version
   */
  final def tryRunning = if shouldStart then runUntilQuit()

  /** Run REPL with `state` until `:quit` command found
   *
   *  This method is the main entry point into the REPL. Its effects are not
   *  observable outside of the CLI, for this reason, most helper methods are
   *  `protected final` to facilitate testing.
   */
  def runUntilQuit(using initialState: State = initialState)(): State = {
    val terminal = new JLineTerminal

    out.println(
      s"""Welcome to Scala $simpleVersionString ($javaVersion, Java $javaVmName).
         |Type in expressions for evaluation. Or try :help.""".stripMargin)

    /** Blockingly read a line, getting back a parse result */
    def readLine()(using state: State): ParseResult = {
      given Context = state.context
      val completer: Completer = { (lineReader, line, candidates) =>
        def makeCandidate(label: String) = {
          new Candidate(
            /* value    = */ label,
            /* displ    = */ stripBackTicks(label), // displayed value
            /* group    = */ null,  // can be used to group completions together
            /* descr    = */ null,  // TODO use for documentation?
            /* suffix   = */ null,
            /* key      = */ null,
            /* complete = */ false  // if true adds space when completing
          )
        }
        val comps = completionsWithSignatures(line.cursor, line.line, state)
        candidates.addAll(comps.map(_.label).distinct.map(makeCandidate).asJava)
        val lineWord = line.word()
        comps.filter(c => c.label == lineWord && c.symbols.nonEmpty) match
          case Nil =>
          case exachMatches =>
            val terminal = lineReader.nn.getTerminal
            lineReader.callWidget(LineReader.CLEAR)
            terminal.writer.println()
            exachMatches.foreach: exact =>
              exact.symbols.foreach: sym =>
                terminal.writer.println(SyntaxHighlighting.highlight(sym.showUser))
            lineReader.callWidget(LineReader.REDRAW_LINE)
            lineReader.callWidget(LineReader.REDISPLAY)
            terminal.flush()
      }

      try {
        val line = terminal.readLine(completer)
        ParseResult(line)
      } catch {
        case _: EndOfFileException |
            _: UserInterruptException => // Ctrl+D or Ctrl+C
          Quit
      }
    }

    @tailrec def loop(using state: State)(): State = {
      val res = readLine()
      if (res == Quit) state
      else loop(using interpret(res))()
    }

    try runBody { loop() }
    finally terminal.close()
  }

  final def run(input: String)(using state: State): State = runBody {
    interpret(ParseResult.complete(input))
  }

  final def runQuietly(input: String)(using State): State = runBody {
    val parsed = ParseResult(input)
    interpret(parsed, quiet = true)
  }

  protected def runBody(body: => State): State = rendering.classLoader()(using rootCtx).asContext(withRedirectedOutput(body))

  // TODO: i5069
  final def bind(name: String, value: Any)(using state: State): State = state

  /**
   * Controls whether the `System.out` and `System.err` streams are set to the provided constructor parameter instance
   * of [[java.io.PrintStream]] during the execution of the repl. On by default.
   *
   * Disabling this can be beneficial when executing a repl instance inside a concurrent environment, for example a
   * thread pool (such as the Scala compile server in the Scala Plugin for IntelliJ IDEA).
   *
   * In such environments, indepently executing `System.setOut` and `System.setErr` without any synchronization can
   * lead to unpredictable results when restoring the original streams (dependent on the order of execution), leaving
   * the Java process in an inconsistent state.
   */
  protected def redirectOutput: Boolean = true

  // redirecting the output allows us to test `println` in scripted tests
  private def withRedirectedOutput(op: => State): State = {
    if redirectOutput then
      val savedOut = System.out
      val savedErr = System.err
      try {
        System.setOut(out)
        System.setErr(out)
        op
      }
      finally {
        System.setOut(savedOut)
        System.setErr(savedErr)
      }
    else op
  }

  private def newRun(state: State, reporter: StoreReporter = newStoreReporter) = {
    val run = compiler.newRun(rootCtx.fresh.setReporter(reporter), state)
    state.copy(context = run.runContext)
  }

  private def stripBackTicks(label: String) =
    if label.startsWith("`") && label.endsWith("`") then
      label.drop(1).dropRight(1)
    else
      label

  @deprecated("Use completionsWithSignatures instead", "3.4.2")
  protected final def completions(cursor: Int, expr: String, state0: State): List[Candidate] =
    completionsWithSignatures(cursor, expr, state0).map: c =>
      new Candidate(
        /* value    = */ c.label,
        /* displ    = */ stripBackTicks(c.label), // displayed value
        /* group    = */ null,  // can be used to group completions together
        /* descr    = */ null,  // TODO use for documentation?
        /* suffix   = */ null,
        /* key      = */ null,
        /* complete = */ false  // if true adds space when completing
      )
  end completions

  /** Extract possible completions at the index of `cursor` in `expr` */
  protected final def completionsWithSignatures(cursor: Int, expr: String, state0: State): List[Completion] =
    if expr.startsWith(":") then
      ParseResult.commands.collect {
        case command if command._1.startsWith(expr) => Completion(command._1, "", List())
      }
    else
      given state: State = newRun(state0)
      compiler
        .typeCheck(expr, errorsAllowed = true)
        .map { (untpdTree, tpdTree) =>
          val file = SourceFile.virtual("<completions>", expr, maybeIncomplete = true)
          val unit = CompilationUnit(file)(using state.context)
          unit.untpdTree = untpdTree
          unit.tpdTree = tpdTree
          given Context = state.context.fresh.setCompilationUnit(unit)
          val srcPos = SourcePosition(file, Span(cursor))
          try Completion.completions(srcPos)._2 catch case NonFatal(_) => Nil
        }
        .getOrElse(Nil)
  end completionsWithSignatures

  protected def interpret(res: ParseResult, quiet: Boolean = false)(using state: State): State = {
    res match {
      case parsed: Parsed if parsed.trees.nonEmpty =>
        compile(parsed, state, quiet)

      case SyntaxErrors(_, errs, _) =>
        displayErrors(errs)
        state

      case cmd: Command =>
        interpretCommand(cmd)

      case SigKill => // TODO
        state

      case _ => // new line, empty tree
        state
    }
  }

  /** Compile `parsed` trees and evolve `state` in accordance */
  private def compile(parsed: Parsed, istate: State, quiet: Boolean = false): State = {
    def extractNewestWrapper(tree: untpd.Tree): Name = tree match {
      case PackageDef(_, (obj: untpd.ModuleDef) :: Nil) => obj.name.moduleClassName
      case _ => nme.NO_NAME
    }

    def extractTopLevelImports(ctx: Context): List[tpd.Import] =
      unfusedPhases(using ctx).collectFirst { case phase: CollectTopLevelImports => phase.imports }.get

    def contextWithNewImports(ctx: Context, imports: List[tpd.Import]): Context =
      if imports.isEmpty then ctx
      else
        imports.foldLeft(ctx.fresh.setNewScope)((ctx, imp) =>
          ctx.importContext(imp, imp.symbol(using ctx)))

    given State = {
      val state0 = newRun(istate, parsed.reporter)
      state0.copy(context = state0.context.withSource(parsed.source))
    }
    compiler
      .compile(parsed)
      .fold(
        displayErrors,
        {
          case (unit: CompilationUnit, newState: State) =>
            val newestWrapper = extractNewestWrapper(unit.untpdTree)
            val newImports = extractTopLevelImports(newState.context)
            var allImports = newState.imports
            if (newImports.nonEmpty)
              allImports += (newState.objectIndex -> newImports)
            val newStateWithImports = newState.copy(
              imports = allImports,
              context = contextWithNewImports(newState.context, newImports)
            )

            val warnings = newState.context.reporter
              .removeBufferedMessages(using newState.context)

            inContext(newState.context) {
              val (updatedState, definitions) =
                if (!ctx.settings.XreplDisableDisplay.value)
                  renderDefinitions(unit.tpdTree, newestWrapper)(using newStateWithImports)
                else
                  (newStateWithImports, Seq.empty)

              // output is printed in the order it was put in. warnings should be
              // shown before infos (eg. typedefs) for the same line. column
              // ordering is mostly to make tests deterministic
              given Ordering[Diagnostic] =
                Ordering[(Int, Int, Int)].on(d => (d.pos.line, -d.level, d.pos.column))

              if (!quiet) {
                (definitions ++ warnings)
                  .sorted
                  .foreach(printDiagnostic)
              }

              updatedState
            }
        }
      )
  }

  private def renderDefinitions(tree: tpd.Tree, newestWrapper: Name)(using state: State): (State, Seq[Diagnostic]) = {
    given Context = state.context

    def resAndUnit(denot: Denotation) = {
      import scala.util.{Success, Try}
      val sym = denot.symbol
      val name = sym.name.show
      val hasValidNumber = Try(name.drop(3).toInt) match {
        case Success(num) => num < state.valIndex
        case _ => false
      }
      name.startsWith(str.REPL_RES_PREFIX) && hasValidNumber && sym.info == defn.UnitType
    }

    def extractAndFormatMembers(symbol: Symbol): (State, Seq[Diagnostic]) = if (tree.symbol.info.exists) {
      val info = symbol.info
      val defs =
        info.bounds.hi.finalResultType
          .membersBasedOnFlags(required = Method, excluded = Accessor | ParamAccessor | Synthetic | Private)
          .filterNot { denot =>
            defn.topClasses.contains(denot.symbol.owner) || denot.symbol.isConstructor
             || denot.symbol.name.is(DefaultGetterName)
          }

      val vals =
        info.fields
          .filterNot(_.symbol.isOneOf(ParamAccessor | Private | Synthetic | Artifact | Module))
          .filter(_.symbol.name.is(SimpleNameKind))

      val typeAliases =
        info.bounds.hi.typeMembers.filter(_.symbol.info.isTypeAlias)

      // The wrapper object may fail to initialize if the rhs of a ValDef throws.
      // In that case, don't attempt to render any subsequent vals, and mark this
      // wrapper object index as invalid.
      var failedInit = false
      val renderedVals =
        val buf = mutable.ListBuffer[Diagnostic]()
        for d <- vals do if !failedInit then rendering.renderVal(d) match
          case Right(Some(v)) =>
            buf += v
          case Left(e) =>
            buf += rendering.renderError(e, d)
            failedInit = true
          case _ =>
        buf.toList

      if failedInit then
        // We limit the returned diagnostics here to `renderedVals`, which will contain the rendered error
        // for the val which failed to initialize. Since any other defs, aliases, imports, etc. from this
        // input line will be inaccessible, we avoid rendering those so as not to confuse the user.
        (state.copy(invalidObjectIndexes = state.invalidObjectIndexes + state.objectIndex), renderedVals)
      else
        val formattedMembers =
          typeAliases.map(rendering.renderTypeAlias)
          ++ defs.map(rendering.renderMethod)
          ++ renderedVals
        val diagnostics = if formattedMembers.isEmpty then rendering.forceModule(symbol) else formattedMembers
        (state.copy(valIndex = state.valIndex - vals.count(resAndUnit)), diagnostics)
    }
    else (state, Seq.empty)

    def isSyntheticCompanion(sym: Symbol) =
      sym.is(Module) && sym.is(Synthetic)

    def typeDefs(sym: Symbol): Seq[Diagnostic] = sym.info.memberClasses
      .collect {
        case x if !isSyntheticCompanion(x.symbol) && !x.symbol.name.isReplWrapperName =>
          rendering.renderTypeDef(x)
      }

    atPhase(typerPhase.next) {
      // Display members of wrapped module:
      tree.symbol.info.memberClasses
        .find(_.symbol.name == newestWrapper.moduleClassName)
        .map { wrapperModule =>
          val (newState, formattedMembers) = extractAndFormatMembers(wrapperModule.symbol)
          val formattedTypeDefs =  // don't render type defs if wrapper initialization failed
            if newState.invalidObjectIndexes.contains(state.objectIndex) then Seq.empty
            else typeDefs(wrapperModule.symbol)
          val highlighted = (formattedTypeDefs ++ formattedMembers)
            .map(d => new Diagnostic(d.msg.mapMsg(SyntaxHighlighting.highlight), d.pos, d.level))
          (newState, highlighted)
        }
        .getOrElse {
          // user defined a trait/class/object, so no module needed
          (state, Seq.empty)
        }
    }
  }

  /** Interpret `cmd` to action and propagate potentially new `state` */
  private def interpretCommand(cmd: Command)(using state: State): State = cmd match {
    case UnknownCommand(cmd) =>
      out.println(s"""Unknown command: "$cmd", run ":help" for a list of commands""")
      state

    case AmbiguousCommand(cmd, matching) =>
      out.println(s""""$cmd" matches ${matching.mkString(", ")}. Try typing a few more characters. Run ":help" for a list of commands""")
      state

    case Help =>
      out.println(Help.text)
      state

    case Reset(arg) =>
      val tokens = tokenize(arg)

      if tokens.nonEmpty then
        out.println(s"""|Resetting REPL state with the following settings:
                        |  ${tokens.mkString("\n  ")}
                        |""".stripMargin)
      else
        out.println("Resetting REPL state.")

      resetToInitial(tokens)
      initialState

    case Imports =>
      for {
        objectIndex <- state.validObjectIndexes
        imp <- state.imports.getOrElse(objectIndex, Nil)
      } out.println(imp.show(using state.context))
      state

    case Load(path) =>
      val file = new JFile(path)
      if (file.exists) {
        val contents = Using(scala.io.Source.fromFile(file, StandardCharsets.UTF_8.name))(_.mkString).get
        run(contents)
      }
      else {
        out.println(s"""Couldn't find file "${file.getCanonicalPath}"""")
        state
      }

    case TypeOf(expr) =>
      expr match {
        case "" => out.println(s":type <expression>")
        case _  =>
          compiler.typeOf(expr)(using newRun(state)).fold(
            displayErrors,
            res => out.println(res)  // result has some highlights
          )
      }
      state

    case DocOf(expr) =>
      expr match {
        case "" => out.println(s":doc <expression>")
        case _  =>
          compiler.docOf(expr)(using newRun(state)).fold(
            displayErrors,
            res => out.println(res)
          )
      }
      state

    case Settings(arg) => arg match
      case "" =>
        given ctx: Context = state.context
        for (s <- ctx.settings.userSetSettings(ctx.settingsState).sortBy(_.name))
          out.println(s"${s.name} = ${if s.value == "" then "\"\"" else s.value}")
        state
      case _  =>
        rootCtx = setupRootCtx(tokenize(arg).toArray, rootCtx)
        state.copy(context = rootCtx)

    case Quit =>
      // end of the world!
      state
  }

  /** shows all errors nicely formatted */
  private def displayErrors(errs: Seq[Diagnostic])(using state: State): State = {
    errs.foreach(printDiagnostic)
    state
  }

  /** Like ConsoleReporter, but without file paths, -Xprompt displaying,
   *  and using a PrintStream rather than a PrintWriter so messages aren't re-encoded. */
  private object ReplConsoleReporter extends ConsoleReporter.AbstractConsoleReporter {
    override def posFileStr(pos: SourcePosition) = "" // omit file paths
    override def printMessage(msg: String): Unit = out.println(msg)
    override def flush()(using Context): Unit    = out.flush()
  }

  /** Print warnings & errors using ReplConsoleReporter, and info straight to out */
  private def printDiagnostic(dia: Diagnostic)(using state: State) = dia.level match
    case interfaces.Diagnostic.INFO => out.println(dia.msg) // print REPL's special info diagnostics directly to out
    case _                          => ReplConsoleReporter.doReport(dia)(using state.context)

end ReplDriver
