package dotty.tools.dotc
package transform

import java.io.File
import ast.tpd.*

import collection.mutable
import core.Flags.*
import core.Contexts.{Context, ctx, inContext}
import core.DenotTransformers.IdentityDenotTransformer
import core.Symbols.{Symbol, defn}
import core.Constants.Constant
import core.NameOps.isContextFunction
import core.StdNames.nme
import core.Types.*
import core.Decorators.*
import coverage.*
import typer.LiftCoverage
import util.{SourceFile, SourcePosition}
import util.Spans.Span
import localopt.StringInterpolatorOpt
import inlines.Inlines
import scala.util.matching.Regex
import java.util.regex.Pattern

/** Implements code coverage by inserting calls to scala.runtime.coverage.Invoker
  * ("instruments" the source code).
  * The result can then be consumed by the Scoverage tool.
  */
class InstrumentCoverage extends MacroTransform with IdentityDenotTransformer:
  import InstrumentCoverage.{InstrumentedParts, ExcludeMethodFlags}

  override def phaseName = InstrumentCoverage.name

  override def description = InstrumentCoverage.description

  // Enabled by argument "-coverage-out OUTPUT_DIR"
  override def isEnabled(using ctx: Context) =
    ctx.settings.coverageOutputDir.value.nonEmpty

  // counter to assign a unique id to each statement
  private var statementId = 0

  // stores all instrumented statements
  private val coverage = Coverage()

  private var coverageExcludeClasslikePatterns: List[Pattern] = Nil
  private var coverageExcludeFilePatterns: List[Pattern] = Nil

  override def run(using ctx: Context): Unit =
    val outputPath = ctx.settings.coverageOutputDir.value

    // Ensure the dir exists
    val dataDir = File(outputPath)
    val newlyCreated = dataDir.mkdirs()

    if !newlyCreated then
      // If the directory existed before, let's clean it up.
      dataDir.listFiles.nn
        .filter(_.nn.getName.nn.startsWith("scoverage"))
        .foreach(_.nn.delete())
    end if

    coverageExcludeClasslikePatterns = ctx.settings.coverageExcludeClasslikes.value.map(_.r.pattern)
    coverageExcludeFilePatterns = ctx.settings.coverageExcludeFiles.value.map(_.r.pattern)

    super.run

    Serializer.serialize(coverage, outputPath, ctx.settings.sourceroot.value)

  private def isClassIncluded(sym: Symbol)(using Context): Boolean =
    val fqn = sym.fullName.toText(ctx.printerFn(ctx)).show
    coverageExcludeClasslikePatterns.isEmpty || !coverageExcludeClasslikePatterns.exists(
      _.matcher(fqn).nn.matches
    )

  private def isFileIncluded(file: SourceFile)(using Context): Boolean =
    val normalizedPath = file.path.replace(".scala", "")
    coverageExcludeFilePatterns.isEmpty || !coverageExcludeFilePatterns.exists(
      _.matcher(normalizedPath).nn.matches
    )

  override protected def newTransformer(using Context) =
    CoverageTransformer(ctx.settings.coverageOutputDir.value)

  /** Transforms trees to insert calls to Invoker.invoked to compute the coverage when the code is called */
  private class CoverageTransformer(outputPath: String) extends Transformer:
    private val ConstOutputPath = Constant(outputPath)

    /** Generates the tree for:
      * ```
      * Invoker.invoked(id, DIR)
      * ```
      * where DIR is the _outputPath_ defined by the coverage settings.
      */
    private def invokeCall(id: Int, span: Span)(using Context): Apply =
      ref(defn.InvokedMethodRef).withSpan(span)
        .appliedToArgs(
          Literal(Constant(id)) :: Literal(ConstOutputPath) :: Nil
        ).withSpan(span)
        .asInstanceOf[Apply]

    /**
      * Records information about a new coverable statement. Generates a unique id for it.
      *
      * @param tree the tree to add to the coverage report
      * @param pos the position to save in the report
      * @param branch true if it's a branch (branches are considered differently by most coverage analysis tools)
      * @param ctx the current context
      * @return the statement's id
      */
    private def recordStatement(tree: Tree, pos: SourcePosition, branch: Boolean)(using ctx: Context): Int =
      val id = statementId
      statementId += 1

      val sourceFile = pos.source
      val statement = Statement(
        location = Location(tree, sourceFile),
        id = id,
        start = pos.start,
        end = pos.end,
        // +1 to account for the line number starting at 1
        // the internal line number is 0-base https://github.com/scala/scala3/blob/18ada516a85532524a39a962b2ddecb243c65376/compiler/src/dotty/tools/dotc/util/SourceFile.scala#L173-L176
        line = pos.line + 1,
        desc = sourceFile.content.slice(pos.start, pos.end).mkString,
        symbolName = tree.symbol.name.toSimpleName.show,
        treeName = tree.getClass.getSimpleName.nn,
        branch
      )
      coverage.addStatement(statement)
      id

    /**
      * Adds a new statement to the current `Coverage` and creates a corresponding call
      * to `Invoker.invoke` with its id, and the given position.
      *
      * Note that the entire tree won't be saved in the coverage analysis, only some
      * data related to the tree is recorded (e.g. its type, its parent class, ...).
      *
      * @param tree the tree to add to the coverage report
      * @param pos the position to save in the report
      * @param branch true if it's a branch
      * @return the tree corresponding to the call to `Invoker.invoke`
      */
    private def createInvokeCall(tree: Tree, pos: SourcePosition, branch: Boolean = false)(using Context): Apply =
      val statementId = recordStatement(tree, pos, branch)
      println(s"createInvokeCall: $statementId")
      val span = pos.span.toSynthetic
      invokeCall(statementId, span)

    /**
      * Tries to instrument an `Apply`.
      * These "tryInstrument" methods are useful to tweak the generation of coverage instrumentation,
      * in particular in `case TypeApply` in the [[transform]] method.
      *
      * @param tree the tree to instrument
      * @return instrumentation result, with the preparation statement, coverage call and tree separated
      */
    private def tryInstrument(tree: Apply)(using Context): InstrumentedParts =
      println(s"tryInstrument Tree: ${tree}")

      if canInstrumentApply(tree) then
        // Create a call to Invoker.invoked(coverageDirectory, newStatementId)
        val coverageCall = createInvokeCall(tree, tree.sourcePos)

        if needsLift(tree) then
          // Transform args and fun, i.e. instrument them if needed (and if possible)
          val app = cpy.Apply(tree)(transform(tree.fun), tree.args.map(transform))

          // Lifts the arguments. Note that if only one argument needs to be lifted, we lift them all.
          // Also, tree.fun can be lifted too.
          // See LiftCoverage for the internal working of this lifting.
          val liftedDefs = mutable.ListBuffer[Tree]()
          val liftedApp = LiftCoverage.liftForCoverage(liftedDefs, app)

          InstrumentedParts(liftedDefs.toList, coverageCall, liftedApp)
        else
          // Instrument without lifting
          val transformed = cpy.Apply(tree)(transform(tree.fun), transform(tree.args))
          InstrumentedParts.singleExpr(coverageCall, transformed)
      else
        // Transform recursively but don't instrument the tree itself
        val transformed = cpy.Apply(tree)(transform(tree.fun), transform(tree.args))
        InstrumentedParts.notCovered(transformed)

    private def tryInstrument(tree: Ident)(using Context): InstrumentedParts = {
      println(s"tryInstrument Ident: ${tree}")


      val sym = tree.symbol

      if (canInstrumentParameterless(sym)) {
        // call to a local parameterless method f
        val coverageCall = createInvokeCall(tree, tree.sourcePos)
        InstrumentedParts.singleExpr(coverageCall, tree)
      } else {
        println(s"tryInstrument Ident: ${tree} not instrumented")
         InstrumentedParts.notCovered(tree)
      }
    }

    private def tryInstrument(tree: Select)(using Context): InstrumentedParts = {
      val sym = tree.symbol
      val transformed = cpy.Select(tree)(transform(tree.qualifier), tree.name)
      println(s"tryInstrument Select: ${tree}")
      if canInstrumentParameterless(sym) then
        // call to a parameterless method
        val coverageCall = createInvokeCall(tree, tree.sourcePos)
        InstrumentedParts.singleExpr(coverageCall, transformed)
      else
        InstrumentedParts.notCovered(transformed)
    }
    /** Generic tryInstrument */
    private def tryInstrument(tree: Tree)(using Context): InstrumentedParts =
      tree match
        case t: Apply => tryInstrument(t)
        case t: Ident => tryInstrument(t)
        case t: Select => tryInstrument(t)
        case _ => InstrumentedParts.notCovered(transform(tree))

    /**
     * Transforms and instruments a branch if it's non-empty.
     * If the tree is empty, return itself and don't instrument.
     */
    private def transformBranch(tree: Tree)(using Context): Tree =
      if tree.isEmpty then
        // - If t.isEmpty then `transform(t) == t` always hold,
        //   so we can avoid calling transform in that case.
        tree
      else
        val transformed = transform(tree)
        val coverageCall = createInvokeCall(tree, tree.sourcePos, branch = true)
        InstrumentedParts.singleExprTree(coverageCall, transformed)

    override def transform(tree: Tree)(using Context): Tree =
      inContext(transformCtx(tree)) { // necessary to position inlined code properly
        tree match
          // simple cases
          case tree: (Import | Export | Literal | This | Super | New) => tree
          case tree if tree.isEmpty || tree.isType => tree // empty Thicket, Ident (referring to a type), TypeTree, ...
          case tree if !tree.span.exists || tree.span.isZeroExtent => tree // no meaningful position

          // identifier
          case tree: Ident =>
            tryInstrument(tree).toTree

          // branches
          case tree: If =>
            cpy.If(tree)(
              cond = transform(tree.cond),
              thenp = transformBranch(tree.thenp),
              elsep = transformBranch(tree.elsep)
            )
          case tree: Try =>
            cpy.Try(tree)(
              expr = transformBranch(tree.expr),
              cases = tree.cases.map(transformCaseDef),
              finalizer = transformBranch(tree.finalizer)
            )

          // f(args)
          case tree: Apply =>
            tryInstrument(tree).toTree

          // (fun)[args]
          case TypeApply(fun, args) =>
            // Here is where `InstrumentedParts` becomes useful!
            // We extract its components and act carefully.
            println(s"TypeApply: ${fun}")
            val InstrumentedParts(pre, coverageCall, expr) = tryInstrument(fun)

            if coverageCall.isEmpty then
              // `fun` cannot be instrumented and `args` is a type, but `expr` may have been transformed
              cpy.TypeApply(tree)(expr, args)
            else
              // expr[T] shouldn't be transformed to:
              // {invoked(...), expr}[T]
              //
              // but to:
              // {invoked(...), expr[T]}
              //
              // This is especially important for trees like (expr[T])(args),
              // for which the wrong transformation crashes the compiler.
              // See tests/coverage/pos/PolymorphicExtensions.scala
              Block(
                pre :+ coverageCall,
                cpy.TypeApply(tree)(expr, args)
              )

          // a.b
          case tree: Select =>
            tryInstrument(tree).toTree

          case tree: CaseDef =>
            transformCaseDef(tree)

          case tree: ValDef =>
            // only transform the rhs
            val rhs = transform(tree.rhs)
            cpy.ValDef(tree)(rhs = rhs)

          case tree: DefDef =>
            transformDefDef(tree)

          case tree: PackageDef =>
            if isFileIncluded(tree.srcPos.sourcePos.source) && isClassIncluded(tree.symbol) then
              // only transform the statements of the package
              cpy.PackageDef(tree)(tree.pid, transform(tree.stats))
            else
              tree

          case tree: TypeDef =>
            if isFileIncluded(tree.srcPos.sourcePos.source) && isClassIncluded(tree.symbol) then
              super.transform(tree)
            else
              tree

          case tree: Assign =>
            // only transform the rhs
            cpy.Assign(tree)(tree.lhs, transform(tree.rhs))

          case tree: Return =>
            // only transform the expr, because `from` is a "pointer"
            // to the enclosing method, not a tree to instrument.
            cpy.Return(tree)(expr = transform(tree.expr), from = tree.from)

          case tree: Template =>
            // only transform:
            // - the arguments of the `Apply` trees in the parents
            // - the template body
            cpy.Template(tree)(
              transformSub(tree.constr),
              transformTemplateParents(tree.parents)(using ctx.superCallContext),
              tree.derived,
              tree.self,
              transformStats(tree.body, tree.symbol)
            )

          case tree: Inlined =>
            // Ideally, tree.call would provide precise information about the inlined call,
            // and we would use this information for the coverage report.
            // But PostTyper simplifies tree.call, so we can't report the actual method that was inlined.
            // In any case, the subtrees need to be repositioned right now, otherwise the
            // coverage statement will point to a potentially unreachable source file.
            val dropped = Inlines.dropInlined(tree) // drop and reposition
            transform(dropped) // transform the content of the Inlined

          // For everything else just recurse and transform
          case _ =>
            super.transform(tree)
        }

    /** Transforms a `def lhs = rhs` and instruments its body (rhs).
      *
      * The rhs is always transformed recursively.
      *
      * If possible, a coverage call is inserted at the beginning of the body
      * (never outside of the DefDef tree). Therefore, this method always returns a `DefDef`.
      * Thanks to this, it doesn't need to be wrapped in an`InstrumentedParts`.
      */
    private def transformDefDef(tree: DefDef)(using Context): DefDef =
      val sym = tree.symbol
      if sym.isOneOf(Inline | Erased) then
        // Inline and erased definitions will not be in the generated code and therefore do not need to be instrumented.
        // (Note that a retained inline method will have a `$retained` variant that will be instrumented.)
        tree
      else
        // Only transform the params (for the default values) and the rhs, not the name and tpt.
        val transformedParamss = transformParamss(tree.paramss)
        val transformedRhs =
          if tree.rhs.isEmpty then
            tree.rhs
          else if sym.isClassConstructor then
            instrumentSecondaryCtor(tree)
          else if !sym.isOneOf(Accessor | Artifact | Synthetic) then
            // If the body can be instrumented, do it (i.e. insert a "coverage call" at the beginning)
            // This is useful because methods can be stored and called later, or called by reflection,
            // and if the rhs is too simple to be instrumented (like `def f = this`),
            // the method won't show up as covered if we don't insert a call at its beginning.
            instrumentBody(tree, transform(tree.rhs))
          else
            transform(tree.rhs)

        cpy.DefDef(tree)(tree.name, transformedParamss, tree.tpt, transformedRhs)

    /** Transforms a `case ...` and instruments the parts that can be. */
    private def transformCaseDef(tree: CaseDef)(using Context): CaseDef =
      val pat = tree.pat
      val guard = tree.guard

      // compute a span that makes sense for the user that will read the coverage results
      val friendlyEnd = if guard.span.exists then guard.span.end else pat.span.end
      val pos = tree.sourcePos.withSpan(tree.span.withEnd(friendlyEnd)) // user-friendly span

      // recursively transform the guard, but keep the pat
      val transformedGuard = transform(guard)

      // ensure that the body is always instrumented as a branch
      val instrumentedBody = transformBranch(tree.body)

      cpy.CaseDef(tree)(pat, transformedGuard, instrumentedBody)

    /** Transforms the parents of a Template. */
    private def transformTemplateParents(parents: List[Tree])(using Context): List[Tree] =
      def transformParent(parent: Tree): Tree = parent match
        case tree: Apply =>
          // only instrument the args, not the constructor call
          cpy.Apply(tree)(tree.fun, tree.args.mapConserve(transform))
        case tree: TypeApply =>
          // args are types, instrument the fun with transformParent
          cpy.TypeApply(tree)(transformParent(tree.fun), tree.args)
        case other =>
          // should always be a TypeTree, nothing to instrument
          other

      parents.mapConserve(transformParent)

    /** Instruments the body of a DefDef. Handles corner cases.
     * Given a DefDef f like this:
     * ```
     * def f(params) = rhs
     * ```
     *
     * It generally inserts a "coverage call" before rhs:
     * ```
     * def f(params) =
     *   Invoker.invoked(id, DIR)
     *   rhs
     * ```
     *
     * But in some cases (e.g. closures), this would be invalid (see the comment below),
     * and the call is inserted at another place.
     */
    private def instrumentBody(parent: DefDef, body: Tree)(using Context): Tree =
      /* recurse on closures, so that we insert the call at the leaf:

         def g: (a: Ta) ?=> (b: Tb) = {
           // nothing here             <-- not here!
           def $anonfun(using a: Ta) =
             Invoked.invoked(id, DIR)  <-- here
             <userCode>
           closure($anonfun)
         }
      */
      body match
        case b @ Block((meth: DefDef) :: Nil, closure: Closure)
        if meth.symbol == closure.meth.symbol && defn.isContextFunctionType(body.tpe) =>
          val instr = cpy.DefDef(meth)(rhs = instrumentBody(parent, meth.rhs))
          cpy.Block(b)(instr :: Nil, closure)
        case _ =>
          // compute user-friendly position to highlight more text in the coverage UI
          val namePos = parent.namePos
          val pos = namePos.withSpan(namePos.span.withStart(parent.span.start))
          // record info and insert call to Invoker.invoked
          val coverageCall = createInvokeCall(parent, pos)
          InstrumentedParts.singleExprTree(coverageCall, body)

    /** Instruments the body of a secondary constructor DefDef.
     *
     *  We must preserve the delegate constructor call as the first statement of
     *  the rhs Block, otherwise `HoistSuperArgs` will not be happy (see #17042).
     */
    private def instrumentSecondaryCtor(ctorDef: DefDef)(using Context): Tree =
      // compute position like in instrumentBody
      val namePos = ctorDef.namePos
      val pos = namePos.withSpan(namePos.span.withStart(ctorDef.span.start))
      val coverageCall = createInvokeCall(ctorDef, pos)

      ctorDef.rhs match
        case b @ Block(delegateCtorCall :: stats, expr: Literal) =>
          cpy.Block(b)(transform(delegateCtorCall) :: coverageCall :: stats.mapConserve(transform), expr)
        case rhs =>
          cpy.Block(rhs)(transform(rhs) :: coverageCall :: Nil, unitLiteral)
    end instrumentSecondaryCtor

    /**
     * Checks if the apply needs a lift in the coverage phase.
     * In case of a nested application, we have to lift all arguments
     * Example:
     * ```
     * def T(x:Int)(y:Int)
     * T(f())(1)
     * ```
     * should not be changed to {val $x = f(); T($x)}(1) but to {val $x = f(); val $y = 1; T($x)($y)}
     */
    private def needsLift(tree: Apply)(using Context): Boolean =
      println(s"start needsLift: $tree")
      def isShortCircuitedOp(sym: Symbol) =
        sym == defn.Boolean_&& || sym == defn.Boolean_||

      def isUnliftableFun(fun: Tree) =
        /*
         * We don't want to lift a || getB(), to avoid calling getB if a is true.
         * Same idea with a && getB(): if a is false, getB shouldn't be called.
         *
         * On top of that, the `s`, `f` and `raw` string interpolators are special-cased
         * by the compiler and will disappear in phase StringInterpolatorOpt, therefore
         * they shouldn't be lifted.
         */
        val sym = fun.symbol
        sym.exists && (
          isShortCircuitedOp(sym)
          || StringInterpolatorOpt.isCompilerIntrinsic(sym)
          || sym == defn.Object_synchronized
          || isContextFunctionApply(fun)
        )
      end isUnliftableFun

      val fun = tree.fun
      val nestedApplyNeedsLift = fun match
        case a: Apply => needsLift(a)
        case _ => false

      val liftMe = nestedApplyNeedsLift ||
      !isUnliftableFun(fun) && !tree.args.isEmpty && !tree.args.forall(LiftCoverage.noLift)
      println(s"end needsLift: $tree, liftMe: $liftMe, nestedApplyNeedsLift: $nestedApplyNeedsLift, isUnliftableFun: ${isUnliftableFun(fun)}, tree.args.isEmpty: ${tree.args}, tree.args.forall(LiftCoverage.noLift): ${tree.args.forall(LiftCoverage.noLift)}")
      liftMe

    private def isContextFunctionApply(fun: Tree)(using Context): Boolean =
      fun match
        case Select(prefix, nme.apply) =>
          defn.isContextFunctionType(prefix.tpe.widen)
        case _ => false

    /** Check if an Apply can be instrumented. Prevents this phase from generating incorrect code. */
    private def canInstrumentApply(tree: Apply)(using Context): Boolean =
      def isSecondaryCtorDelegateCall: Boolean = tree.fun match
        case Select(This(_), nme.CONSTRUCTOR) => true
        case _                                => false

      val sym = tree.symbol
      val canI = !sym.isOneOf(ExcludeMethodFlags)
      && !isCompilerIntrinsicMethod(sym)
      && !(sym.isClassConstructor && isSecondaryCtorDelegateCall)
      && (tree.typeOpt match {
        case AppliedType(tycon: NamedType, _) =>
          /* If the last expression in a block is a context function, we'll try to
             summon its arguments at the current point, even if the expected type
             is a function application. Therefore, this is not valid:
            ```
            def f = (t: Exception) ?=> (c: String) ?=> result

            ({
              invoked()
              f(using e)
            })(using s)
            ```
          */
          !tycon.name.isContextFunction
        case m: MethodType =>
          /* def f(a: Ta)(b: Tb)
             f(a)(b)

             Here, f(a)(b) cannot be rewritten to {invoked();f(a)}(b)
          */
          false
        case _ =>
          false
      })
      println(s"canInstrumentApply ${tree.symbol}: $canI is ${sym.name} is method ${sym.is(Method)} is compiler intrinsic ${isCompilerIntrinsicMethod(sym)} typeOpt ${tree.typeOpt}")
      canI


    /** Is this the symbol of a parameterless method that we can instrument?
      * Note: it is crucial that `asInstanceOf` and `isInstanceOf`, among others,
      * do NOT get instrumented, because that would generate invalid code and crash
      * in post-erasure checking.
      */
    private def canInstrumentParameterless(sym: Symbol)(using Context): Boolean = {
      val isP =  (sym.is(Method, butNot = ExcludeMethodFlags)
      && sym.info.isParameterless
      && !isCompilerIntrinsicMethod(sym)
      && !sym.info.typeSymbol.name.isContextFunction ) // exclude context functions like in canInstrumentApply)
      println(s"canInstrumentParameterless $sym: $isP is ${sym.name} kind string ${sym.kindString} is method ${sym.is(Method)} is parameterless ${sym.info.isParameterless} is compiler intrinsic ${isCompilerIntrinsicMethod(sym)} typeSymbol ${sym.info.typeSymbol} is context function ${sym.info.typeSymbol.name.isContextFunction}")
      isP
    }

    /** Does sym refer to a "compiler intrinsic" method, which only exist during compilation,
      * like Any.isInstanceOf?
      * If this returns true, the call souldn't be instrumented.
      */
    private def isCompilerIntrinsicMethod(sym: Symbol)(using Context): Boolean =
      val owner = sym.maybeOwner
      owner.exists && (
        owner.eq(defn.AnyClass) ||
        owner.isPrimitiveValueClass ||
        owner.maybeOwner == defn.CompiletimePackageClass
      )

object InstrumentCoverage:
  val name: String = "instrumentCoverage"
  val description: String = "instrument code for coverage checking"
  val ExcludeMethodFlags: FlagSet = Synthetic | Artifact | Erased

  /**
   * An instrumented Tree, in 3 parts.
   * @param pre preparation code, e.g. lifted arguments. May be empty.
   * @param invokeCall call to Invoker.invoked(dir, id), or an empty tree.
   * @param expr the instrumented expression, executed just after the invokeCall
   */
  case class InstrumentedParts(pre: List[Tree], invokeCall: Apply | EmptyTree.type, expr: Tree):
    require(pre.isEmpty || (pre.nonEmpty && !invokeCall.isEmpty), "if pre isn't empty then invokeCall shouldn't be empty")

    /** Turns this into an actual Tree. */
    def toTree(using Context): Tree =
      if invokeCall.isEmpty then expr
      else if pre.isEmpty then Block(invokeCall :: Nil, expr)
      else Block(pre :+ invokeCall, expr)

  object InstrumentedParts:
    def notCovered(expr: Tree) = InstrumentedParts(Nil, EmptyTree, expr)
    def singleExpr(invokeCall: Apply, expr: Tree) = InstrumentedParts(Nil, invokeCall, expr)

    /** Shortcut for `singleExpr(call, expr).toTree` */
    def singleExprTree(invokeCall: Apply, expr: Tree)(using Context): Tree =
      Block(invokeCall :: Nil, expr)
