package dotty.tools
package dotc

import core.*
import Contexts.*
import typer.{TyperPhase, RefChecks}
import parsing.Parser
import Phases.Phase
import transform.*
import dotty.tools.backend
import backend.jvm.{CollectSuperCalls, GenBCode}
import localopt.StringInterpolatorOpt

/** The central class of the dotc compiler. The job of a compiler is to create
 *  runs, which process given `phases` in a given `rootContext`.
 */
class Compiler {

  /** Meta-ordering constraint:
   *
   *  DenotTransformers that change the signature of their denotation's info must go
   *  after erasure. The reason is that denotations are permanently referred to by
   *  TermRefs which contain a signature. If the signature of a symbol would change,
   *  all refs to it would become outdated - they could not be dereferenced in the
   *  new phase.
   *
   *  After erasure, signature changing denot-transformers are OK because signatures
   *  are never recomputed later than erasure.
   */
  def phases: List[List[Phase]] =
    frontendPhases ::: picklerPhases ::: transformPhases ::: backendPhases

  /** Phases dealing with the frontend up to trees ready for TASTY pickling */
  protected def frontendPhases: List[List[Phase]] =
    List(new Parser) ::             // Compiler frontend: scanner, parser
    List(new TyperPhase) ::         // Compiler frontend: namer, typer
    List(new CheckUnused.PostTyper) :: // Check for unused elements
    List(new CheckShadowing) :: // Check shadowing elements
    List(new YCheckPositions) ::    // YCheck positions
    List(new sbt.ExtractDependencies) :: // Sends information on classes' dependencies to sbt via callbacks
    List(new semanticdb.ExtractSemanticDB.ExtractSemanticInfo) :: // Extract info into .semanticdb files
    List(new PostTyper) ::          // Additional checks and cleanups after type checking
    List(new sjs.PrepJSInterop) ::  // Additional checks and transformations for Scala.js (Scala.js only)
    List(new SetRootTree) ::        // Set the `rootTreeOrProvider` on class symbols
    Nil

  /** Phases dealing with TASTY tree pickling and unpickling */
  protected def picklerPhases: List[List[Phase]] =
    List(new Pickler) ::            // Generate TASTY info
    List(new sbt.ExtractAPI) ::     // Sends a representation of the API of classes to sbt via callbacks
    List(new Inlining) ::           // Inline and execute macros
    List(new PostInlining) ::       // Add mirror support for inlined code
    List(new CheckUnused.PostInlining) ::  // Check for unused elements
    List(new Staging) ::            // Check staging levels and heal staged types
    List(new Splicing) ::           // Replace level 1 splices with holes
    List(new PickleQuotes) ::       // Turn quoted trees into explicit run-time data structures
    Nil

  /** Phases dealing with the transformation from pickled trees to backend trees */
  protected def transformPhases: List[List[Phase]] =
    List(new InstrumentCoverage) ::  // Perform instrumentation for code coverage (if -coverage-out is set)
    List(new CrossVersionChecks,     // Check issues related to deprecated and experimental
         new FirstTransform,         // Some transformations to put trees into a canonical form
         new CheckReentrant,         // Internal use only: Check that compiled program has no data races involving global vars
         new ElimPackagePrefixes,    // Eliminate references to package prefixes in Select nodes
         new CookComments,           // Cook the comments: expand variables, doc, etc.
         new CheckLoopingImplicits,  // Check that implicit defs do not call themselves in an infinite loop
         new BetaReduce,             // Reduce closure applications
         new InlineVals,             // Check right hand-sides of an `inline val`s
         new ExpandSAMs,             // Expand single abstract method closures to anonymous classes
         new ElimRepeated,           // Rewrite vararg parameters and arguments
         new RefChecks) ::           // Various checks mostly related to abstract members and overriding
    List(new semanticdb.ExtractSemanticDB.AppendDiagnostics) :: // Attach warnings to extracted SemanticDB and write to .semanticdb file
    List(new init.Checker) ::        // Check initialization of objects
    List(new ProtectedAccessors,     // Add accessors for protected members
         new ExtensionMethods,       // Expand methods of value classes with extension methods
         new UncacheGivenAliases,    // Avoid caching RHS of simple parameterless given aliases
         new CheckStatic,            // Check restrictions that apply to @static members
         new ElimByName,             // Map by-name parameters to functions
         new HoistSuperArgs,         // Hoist complex arguments of supercalls to enclosing scope
         new ForwardDepChecks,       // Check that there are no forward references to local vals
         new SpecializeApplyMethods, // Adds specialized methods to FunctionN
         new TryCatchPatterns,       // Compile cases in try/catch
         new PatternMatcher) ::      // Compile pattern matches
    List(new TestRecheck.Pre) ::     // Test only: run rechecker, enabled under -Yrecheck-test
    List(new TestRecheck) ::         // Test only: run rechecker, enabled under -Yrecheck-test
    List(new cc.Setup) ::            // Preparations for check captures phase, enabled under captureChecking
    List(new cc.CheckCaptures) ::    // Check captures, enabled under captureChecking
    List(new ElimOpaque,             // Turn opaque into normal aliases
         new sjs.ExplicitJSClasses,  // Make all JS classes explicit (Scala.js only)
         new ExplicitOuter,          // Add accessors to outer classes from nested ones.
         new ExplicitSelf,           // Make references to non-trivial self types explicit as casts
         new StringInterpolatorOpt,  // Optimizes raw and s and f string interpolators by rewriting them to string concatenations or formats
         new DropBreaks) ::          // Optimize local Break throws by rewriting them
    List(new PruneErasedDefs,        // Drop erased definitions from scopes and simplify erased expressions
         new UninitializedDefs,      // Replaces `compiletime.uninitialized` by `_`
         new InlinePatterns,         // Remove placeholders of inlined patterns
         new VCInlineMethods,        // Inlines calls to value class methods
         new SeqLiterals,            // Express vararg arguments as arrays
         new InterceptedMethods,     // Special handling of `==`, `|=`, `getClass` methods
         new Getters,                // Replace non-private vals and vars with getter defs (fields are added later)
         new SpecializeFunctions,    // Specialized Function{0,1,2} by replacing super with specialized super
         new SpecializeTuples,       // Specializes Tuples by replacing tuple construction and selection trees
         new CollectNullableFields,  // Collect fields that can be nulled out after use in lazy initialization
         new ElimOuterSelect,        // Expand outer selections
         new ResolveSuper,           // Implement super accessors
         new FunctionXXLForwarders,  // Add forwarders for FunctionXXL apply method
         new ParamForwarding,        // Add forwarders for aliases of superclass parameters
         new TupleOptimizations,     // Optimize generic operations on tuples
         new LetOverApply,           // Lift blocks from receivers of applications
         new ArrayConstructors) ::   // Intercept creation of (non-generic) arrays and intrinsify.
    List(new Erasure) ::             // Rewrite types to JVM model, erasing all type parameters, abstract types and refinements.
    List(new ElimErasedValueType,    // Expand erased value types to their underlying implmementation types
         new PureStats,              // Remove pure stats from blocks
         new VCElideAllocations,     // Peep-hole optimization to eliminate unnecessary value class allocations
         new EtaReduce,              // Reduce eta expansions of pure paths to the underlying function reference
         new ArrayApply,             // Optimize `scala.Array.apply([....])` and `scala.Array.apply(..., [....])` into `[...]`
         new sjs.AddLocalJSFakeNews, // Adds fake new invocations to local JS classes in calls to `createLocalJSClass`
         new ElimPolyFunction,       // Rewrite PolyFunction subclasses to FunctionN subclasses
         new TailRec,                // Rewrite tail recursion to loops
         new CompleteJavaEnums,      // Fill in constructors for Java enums
         new Mixin,                  // Expand trait fields and trait initializers
         new LazyVals,               // Expand lazy vals
         new Memoize,                // Add private fields to getters and setters
         new NonLocalReturns,        // Expand non-local returns
         new CapturedVars) ::        // Represent vars captured by closures as heap objects
    List(new Constructors,           // Collect initialization code in primary constructors
                                        // Note: constructors changes decls in transformTemplate, no InfoTransformers should be added after it
         new Instrumentation) ::     // Count calls and allocations under -Yinstrument
    List(new LambdaLift,             // Lifts out nested functions to class scope, storing free variables in environments
                                     // Note: in this mini-phase block scopes are incorrect. No phases that rely on scopes should be here
         new ElimStaticThis,         // Replace `this` references to static objects by global identifiers
         new CountOuterAccesses) ::  // Identify outer accessors that can be dropped
    List(new DropOuterAccessors,     // Drop unused outer accessors
         new CheckNoSuperThis,       // Check that supercalls don't contain references to `this`
         new Flatten,                // Lift all inner classes to package scope
         new TransformWildcards,     // Replace wildcards with default values
         new MoveStatics,            // Move static methods from companion to the class itself
         new ExpandPrivate,          // Widen private definitions accessed from nested classes
         new RestoreScopes,          // Repair scopes rendered invalid by moving definitions in prior phases of the group
         new SelectStatic,           // get rid of selects that would be compiled into GetStatic
         new sjs.JUnitBootstrappers, // Generate JUnit-specific bootstrapper classes for Scala.js (not enabled by default)
         new CollectEntryPoints,     // Collect all entry points and save them in the context
         new CollectSuperCalls,      // Find classes that are called with super
         new RepeatableAnnotations) :: // Aggregate repeatable annotations
    Nil

  /** Generate the output of the compilation */
  protected def backendPhases: List[List[Phase]] =
    List(new backend.sjs.GenSJSIR) :: // Generate .sjsir files for Scala.js (not enabled by default)
    List(new GenBCode) ::             // Generate JVM bytecode
    Nil

  var runId: Int = 1
  def nextRunId: Int = {
    runId += 1; runId
  }

  def reset()(using Context): Unit = {
    ctx.base.reset()
    val run = ctx.run
    if (run != null) run.reset()
  }

  def newRun(using Context): Run = {
    reset()
    val rctx =
      if ctx.settings.Xsemanticdb.value then
        ctx.addMode(Mode.ReadPositions)
      else
        ctx
    new Run(this, rctx)
  }
}
