package dotty.tools.repl

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.transform.PostTyper
import dotty.tools.dotc.typer.ImportInfo.{withRootImports, RootRef}
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.util.Spans._
import dotty.tools.dotc.util.{ParsedComment, Property, SourceFile}
import dotty.tools.dotc.{CompilationUnit, Compiler, Run}
import dotty.tools.repl.results._
import dotty.tools.repl.transform._

import scala.collection.mutable
import scala.util.chaining.given

/** This subclass of `Compiler` is adapted for use in the REPL.
 *
 *  - compiles parsed expression in the current REPL state:
 *    - adds the appropriate imports in scope
 *    - wraps expressions into a dummy object
 *  - provides utility to query the type of an expression
 *  - provides utility to query the documentation of an expression
 */
class ReplCompiler extends Compiler:

  override protected def frontendPhases: List[List[Phase]] = List(
    List(Parser()),
    List(ReplPhase()),
    List(TyperPhase(addRootImports = false)),
    List(CollectTopLevelImports()),
    List(PostTyper()),
  )

  def newRun(initCtx: Context, state: State): Run =
    val run = new Run(this, initCtx) {
      /** Import previous runs and user defined imports */
      override protected def rootContext(using Context): Context = {
        def importContext(imp: tpd.Import)(using Context) =
          ctx.importContext(imp, imp.symbol)

        def importPreviousRun(id: Int)(using Context) = {
          // we first import the wrapper object id
          val path = nme.EMPTY_PACKAGE ++ "." ++ ReplCompiler.objectNames(id)
          val ctx0 = ctx.fresh
            .setNewScope
            .withRootImports(RootRef(() => requiredModuleRef(path)) :: Nil)

          // then its user defined imports
          val imports = state.imports.getOrElse(id, Nil)
          if imports.isEmpty then ctx0
          else imports.foldLeft(ctx0.fresh.setNewScope)((ctx, imp) =>
            importContext(imp)(using ctx))
        }

        val rootCtx = super.rootContext.fresh
          .withRootImports
          .fresh.setOwner(defn.EmptyPackageClass): Context
        (state.validObjectIndexes).foldLeft(rootCtx)((ctx, id) =>
          importPreviousRun(id)(using ctx))
      }
    }
    run.suppressions.initSuspendedMessages(state.context.run)
    run
  end newRun

  private def packaged(stats: List[untpd.Tree])(using Context): untpd.PackageDef =
    import untpd.*
    PackageDef(Ident(nme.EMPTY_PACKAGE), stats)

  final def compile(parsed: Parsed)(using state: State): Result[(CompilationUnit, State)] =
    assert(!parsed.trees.isEmpty)

    given Context = state.context
    val unit = ReplCompilationUnit(ctx.source).tap { unit =>
      unit.untpdTree = packaged(parsed.trees)
      unit.untpdTree.putAttachment(ReplCompiler.ReplState, state)
    }
    ctx.run.nn.compileUnits(unit :: Nil)
    ctx.run.nn.printSummary() // "2 errors found"

    val newState = unit.tpdTree.getAttachment(ReplCompiler.ReplState).get
    if !ctx.reporter.hasErrors then (unit, newState).result
    else ctx.reporter.removeBufferedMessages.errors
  end compile

  final def typeOf(expr: String)(using state: State): Result[String] =
    typeCheck(expr).map { (_, tpdTree) =>
      given Context = state.context
      tpdTree.rhs match {
        case Block(xs, _) => xs.last.tpe.widen.show
        case _ =>
          """Couldn't compute the type of your expression, so sorry :(
            |
            |Please report this to my masters at github.com/lampepfl/dotty
          """.stripMargin
      }
    }

  def docOf(expr: String)(using state: State): Result[String] = inContext(state.context) {

    /** Extract the "selected" symbol from `tree`.
     *
     *  Because the REPL typechecks an expression, special syntax is needed to get the documentation
     *  of certain symbols:
     *
     *  - To select the documentation of classes, the user needs to pass a call to the class' constructor
     *    (e.g. `new Foo` to select `class Foo`)
     *  - When methods are overloaded, the user needs to enter a lambda to specify which functions he wants
     *    (e.g. `foo(_: Int)` to select `def foo(x: Int)` instead of `def foo(x: String)`
     *
     *  This function returns the right symbol for the received expression, and all the symbols that are
     *  overridden.
     */
    def extractSymbols(tree: tpd.Tree): Iterator[Symbol] = {
      val sym = tree match {
        case tree if tree.isInstantiation => tree.symbol.owner
        case tpd.closureDef(defdef) => defdef.rhs.symbol
        case _ => tree.symbol
      }
      Iterator(sym) ++ sym.allOverriddenSymbols
    }

    typeCheck(expr).map { (_, tpdTree) => tpdTree match
      case ValDef(_, _, Block(stats, _)) if stats.nonEmpty =>
        val stat = stats.last.asInstanceOf[tpd.Tree]
        if (stat.tpe.isError) stat.tpe.show
        else {
          val symbols = extractSymbols(stat)
          val doc = for {
            sym <- symbols
            comment <- ParsedComment.docOf(sym)
          } yield comment.renderAsMarkdown

          if (doc.hasNext) doc.next()
          else s"// No doc for `$expr`"
        }

      case _ =>
        """Couldn't display the documentation for your expression, so sorry :(
          |
          |Please report this to my masters at github.com/lampepfl/dotty
          """.stripMargin
    }
  }

  final def typeCheck(expr: String, errorsAllowed: Boolean = false)(using state: State): Result[(untpd.ValDef, tpd.ValDef)] = {

    def wrapped(expr: String, sourceFile: SourceFile, state: State)(using Context): Result[untpd.PackageDef] = {
      def wrap(trees: List[untpd.Tree]): untpd.PackageDef = {
        import untpd._

        val valdef = ValDef("expr".toTermName, TypeTree(), Block(trees, unitLiteral).withSpan(Span(0, expr.length)))
        val tmpl = Template(emptyConstructor, Nil, Nil, EmptyValDef, List(valdef))
        val wrapper = TypeDef("$wrapper".toTypeName, tmpl)
          .withMods(Modifiers(Final))
          .withSpan(Span(0, expr.length))
        PackageDef(Ident(nme.EMPTY_PACKAGE), List(wrapper))
      }

      ParseResult(sourceFile) match {
        case Parsed(_, trees, _) =>
          wrap(trees).result
        case SyntaxErrors(_, reported, trees) =>
          if (errorsAllowed) wrap(trees).result
          else reported.errors
        case _ => List(
          new Diagnostic.Error(
            s"Couldn't parse '$expr' to valid scala",
            sourceFile.atSpan(Span(0, expr.length))
          )
        ).errors
      }
    }

    def error[Tree <: untpd.Tree](sourceFile: SourceFile): Result[Tree] =
      List(new Diagnostic.Error(s"Invalid scala expression",
        sourceFile.atSpan(Span(0, sourceFile.content.length)))).errors

    def unwrappedTypeTree(tree: tpd.Tree, sourceFile0: SourceFile)(using Context): Result[tpd.ValDef] = {
      import tpd._
      tree match {
        case PackageDef(_, List(TypeDef(_, tmpl: Template))) =>
          tmpl.body
              .collectFirst { case dd: ValDef if dd.name.show == "expr" => dd.result }
              .getOrElse(error[tpd.ValDef](sourceFile0))
        case _ =>
          error[tpd.ValDef](sourceFile0)
      }
    }

    def unwrappedUntypedTree(tree: untpd.Tree, sourceFile0: SourceFile)(using Context): Result[untpd.ValDef] =
      import untpd._
      tree match {
        case PackageDef(_, List(TypeDef(_, tmpl: Template))) =>
          tmpl.body
              .collectFirst { case dd: ValDef if dd.name.show == "expr" => dd.result }
              .getOrElse(error[untpd.ValDef](sourceFile0))
        case _ =>
          error[untpd.ValDef](sourceFile0)
      }

    val src = SourceFile.virtual("<typecheck>", expr)
    inContext(state.context.fresh
      .setReporter(newStoreReporter)
      .setSetting(state.context.settings.YstopAfter, List("typer"))
    ) {
      wrapped(expr, src, state).flatMap { pkg =>
        val unit = CompilationUnit(src)
        unit.untpdTree = pkg
        ctx.run.nn.compileUnits(unit :: Nil, ctx)

        if (errorsAllowed || !ctx.reporter.hasErrors)
          for
            tpdTree <- unwrappedTypeTree(unit.tpdTree, src)
            untpdTree <- unwrappedUntypedTree(unit.untpdTree, src)
          yield untpdTree -> tpdTree
        else
          ctx.reporter.removeBufferedMessages.errors
      }
    }
  }
object ReplCompiler:
  val ReplState: Property.StickyKey[State] = Property.StickyKey()
  val objectNames = mutable.Map.empty[Int, TermName]
end ReplCompiler

class ReplCompilationUnit(source: SourceFile) extends CompilationUnit(source):
  override def isSuspendable: Boolean = false

/** A placeholder phase that receives parse trees..
 *
 *  It is called "parser" for the convenience of collective muscle memory.
 *
 *  This enables -Vprint:parser.
 */
class Parser extends Phase:
  def phaseName: String = "parser"
  def run(using Context): Unit = ()
end Parser

/** A phase that assembles wrapped parse trees from user input.
 *
 *  Ths `ReplState` attachment indicates Repl wrapping is required.
 *
 *  This enables -Vprint:repl so that users can see how their code snippet was wrapped.
 */
class ReplPhase extends Phase:
  def phaseName: String = "repl"

  def run(using Context): Unit =
    ctx.compilationUnit.untpdTree match
    case pkg @ PackageDef(_, stats) =>
      pkg.getAttachment(ReplCompiler.ReplState).foreach {
        case given State =>
          val defs = definitions(stats)
          val res = wrapped(defs, Span(0, stats.last.span.end))
          res.putAttachment(ReplCompiler.ReplState, defs.state)
          ctx.compilationUnit.untpdTree = res
      }
    case _ =>
  end run

  private case class Definitions(stats: List[untpd.Tree], state: State)

  private def definitions(trees: List[untpd.Tree])(using Context, State): Definitions =
    import untpd.*

    // If trees is of the form `{ def1; def2; def3 }` then `List(def1, def2, def3)`
    val flattened = trees match {
      case List(Block(stats, expr)) =>
        if (expr eq EmptyTree) stats // happens when expr is not an expression
        else stats :+ expr
      case _ =>
        trees
    }

    val state = summon[State]
    var valIdx = state.valIndex
    val defs = mutable.ListBuffer.empty[Tree]

    /** If the user inputs a definition whose name is of the form REPL_RES_PREFIX and a number,
     *  such as `val res9 = 1`, we bump `valIdx` to avoid name clashes.  lampepfl/dotty#3536 */
    def maybeBumpValIdx(tree: Tree): Unit = tree match
      case apply: Apply   => for a <- apply.args  do maybeBumpValIdx(a)
      case tuple: Tuple   => for t <- tuple.trees do maybeBumpValIdx(t)
      case patDef: PatDef => for p <- patDef.pats do maybeBumpValIdx(p)
      case tree: NameTree => tree.name.show.stripPrefix(str.REPL_RES_PREFIX).toIntOption match
        case Some(n) if n >= valIdx => valIdx = n + 1
        case _                      =>
      case _              =>

    flattened.foreach {
      case expr @ Assign(id: Ident, _) =>
        // special case simple reassignment (e.g. x = 3)
        // in order to print the new value in the REPL
        val assignName = (id.name ++ str.REPL_ASSIGN_SUFFIX).toTermName
        val assign = ValDef(assignName, TypeTree(), id).withSpan(expr.span)
        defs += expr += assign
      case expr if expr.isTerm =>
        val resName = (str.REPL_RES_PREFIX + valIdx).toTermName
        valIdx += 1
        val vd = ValDef(resName, TypeTree(), expr).withSpan(expr.span)
        defs += vd
      case other =>
        maybeBumpValIdx(other)
        defs += other
    }

    Definitions(defs.toList, state.copy(objectIndex = state.objectIndex + 1, valIndex = valIdx))
  end definitions

  /** Wrap trees in an object and add imports from the previous compilations.
   *
   *  The resulting structure is something like:
   *
   *  ```
   *  package <none> {
   *    object rs$line$nextId {
   *      import rs$line${i <- 0 until nextId}.*
   *
   *      <trees>
   *    }
   *  }
   *  ```
   */
  private def wrapped(defs: Definitions, span: Span)(using Context): untpd.PackageDef =
    import untpd.*

    val objectName = ctx.source.file.toString
    assert(objectName.startsWith(str.REPL_SESSION_LINE))
    assert(objectName.endsWith(defs.state.objectIndex.toString))
    val objectTermName = objectName.toTermName
    ReplCompiler.objectNames.update(defs.state.objectIndex, objectTermName)

    val tmpl = Template(emptyConstructor, Nil, Nil, EmptyValDef, defs.stats)
    val module = ModuleDef(objectTermName, tmpl).withSpan(span)

    PackageDef(Ident(nme.EMPTY_PACKAGE), List(module))
end ReplPhase
