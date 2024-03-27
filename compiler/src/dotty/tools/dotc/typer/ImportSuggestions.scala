package dotty.tools
package dotc
package typer

import backend.sjs.JSDefinitions
import core.*
import Contexts.*, Types.*, Symbols.*, Names.*, Decorators.*, ProtoTypes.*
import Flags.*, SymDenotations.*
import NameKinds.FlatName
import StdNames.*
import config.Printers.{implicits, implicitsDetailed}
import ast.{untpd, tpd}
import Implicits.{hasExtMethod, Candidate}
import java.util.{Timer, TimerTask}
import collection.mutable
import scala.util.control.NonFatal
import cc.isCaptureChecking

/** This trait defines the method `importSuggestionAddendum` that adds an addendum
 *  to error messages suggesting additional imports.
 */
trait ImportSuggestions:
  this: Typer =>

  /** The maximal number of suggested imports to make */
  inline val MaxSuggestions = 10

  import tpd.*

  /** Timeout to test a single implicit value as a suggestion, in ms */
  private inline val testOneImplicitTimeOut = 500

  /** A list of TermRefs referring to the roots where suggestions for
   *  imports of givens or extension methods that might fix a type error
   *  are searched.
   *
   *  These roots are the smallest set of objects and packages that includes
   *
   *   - any object that is a defined in an enclosing scope,
   *   - any object that is a member of an enclosing class,
   *   - any enclosing package (including the root package),
   *   - any object that is a member of a searched object or package,
   *   - any object or package from which something is imported in an enclosing scope,
   *   - any package that is nested in a searched package, provided
   *     the package was accessed in some way previously.
   *
   *  Excluded from the root set are:
   *
   *   - Objects that contain `$`s in their name. These have to
   *     be omitted since they might be inner Java class files which
   *     cannot be read by the ClassfileParser without crashing.
   *   - Any members of static parts of Java classes.
   *   - Any members of the empty package. These should be
   *     skipped since the empty package often contains unrelated junk files
   *     that should not be used for suggestions.
   *   - Any members of the java or java.lang packages. These are
   *     skipped as an optimization, since they won't contain implicits anyway.
   */
  private def suggestionRoots(using Context) =
    val seen = mutable.Set[TermRef]()

    def lookInside(root: Symbol)(using Context): Boolean =
      explore {
        if root.is(Package) then root.isTerm && root.isCompleted
        else !root.name.is(FlatName)
          && !root.name.lastPart.contains('$')
          && root.is(ModuleVal, butNot = JavaDefined)
          // The implicits in `scalajs.js.|` are implementation details and shouldn't be suggested
          && !(root.name == nme.raw.BAR && ctx.settings.scalajs.value && root == JSDefinitions.jsdefn.PseudoUnionModule)
      }

    def nestedRoots(site: Type)(using Context): List[Symbol] =
      val seenNames = mutable.Set[Name]()
      site.baseClasses.flatMap { bc =>
        bc.info.decls.filter { dcl =>
          lookInside(dcl)
          && !seenNames.contains(dcl.name)
          && { seenNames += dcl.name; true }
        }
      }

    def rootsStrictlyIn(ref: Type)(using Context): List[TermRef] =
      val site = ref.widen
      val refSym = site.typeSymbol
      val nested =
        if refSym.is(Package) then
          if refSym == defn.EmptyPackageClass       // Don't search the empty package
              || refSym == defn.JavaPackageClass     // As an optimization, don't search java...
              || refSym == defn.JavaLangPackageClass // ... or java.lang.
          then Nil
          else refSym.info.decls.filter(lookInside)
        else if refSym.infoOrCompleter.isInstanceOf[StubInfo] then
          Nil // Don't chase roots that do not exist
        else
          if !refSym.is(Touched) then
            refSym.ensureCompleted() // JavaDefined is reliably known only after completion
          if refSym.is(JavaDefined) then Nil
          else nestedRoots(site)
      nested
        .map(mbr => TermRef(ref, mbr.asTerm))
        .flatMap(rootsIn)
        .toList

    def rootsIn(ref: TermRef)(using Context): List[TermRef] =
      if seen.contains(ref) then Nil
      else
        implicitsDetailed.println(i"search for suggestions in ${ref.symbol.fullName}")
        seen += ref
        ref :: rootsStrictlyIn(ref)

    def rootsOnPath(tp: Type)(using Context): List[TermRef] = tp match
      case ref: TermRef => rootsIn(ref) ::: rootsOnPath(ref.prefix)
      case _ => Nil

    def recur(using Context): List[TermRef] =
      if ctx.owner.exists then
        val defined =
          if ctx.owner.isClass then
            if ctx.owner eq ctx.outer.owner then Nil
            else rootsStrictlyIn(ctx.owner.thisType)
          else
            if ctx.scope eq ctx.outer.scope then Nil
            else ctx.scope
              .filter(lookInside(_))
              .flatMap(sym => rootsIn(sym.termRef))
        val imported =
          if ctx.importInfo eqn ctx.outer.importInfo then Nil
          else ctx.importInfo.nn.importSym.info match
            case ImportType(expr) => rootsOnPath(expr.tpe)
            case _ => Nil
        defined ++ imported ++ recur(using ctx.outer)
      else Nil

    recur
  end suggestionRoots

  /** Given an expected type `pt`, return two lists of TermRefs:
   *
   *   1. The _fully matching_ given instances that can be completed
   *      to a full synthesized given term that matches the expected type `pt`.
   *
   *   2. The _head matching_ given instances, that conform to the
   *      expected type `pt`, ignoring any dependent implicit arguments.
   *
   *   If there are no fully matching given instances under (1), and `pt` is
   *   a view prototype of a selection of the form `T ?=>? { name: ... }`,
   *   return instead a list of all possible references to extension methods named
   *   `name` that are applicable to `T`.
   */
  private def importSuggestions(pt: Type)(using Context): (List[TermRef], List[TermRef]) =
    val timer = new Timer()
    val allotted = ctx.run.nn.importSuggestionBudget
    if allotted <= 1 then return (Nil, Nil)
    implicits.println(i"looking for import suggestions, timeout = ${allotted}ms")
    val start = System.currentTimeMillis()
    val deadLine = start + allotted

    // Candidates that are already available without explicit import because they
    // are already provided by the context (imported or inherited) or because they
    // are in the implicit scope of `pt`.
    lazy val alreadyAvailableCandidates: Set[Symbol] = {
      val wildProto = wildApprox(pt)
      val contextualCandidates = ctx.implicits.eligible(wildProto)
      val implicitScopeCandidates = ctx.run.nn.implicitScope(wildProto).eligible
      val allCandidates = contextualCandidates ++ implicitScopeCandidates
      allCandidates.map(_.implicitRef.underlyingRef.symbol).toSet
    }

    def testContext(): Context =
      ctx.fresh.retractMode(Mode.ImplicitsEnabled).setExploreTyperState()

    /** Test whether the head of a given instance matches the expected type `pt`,
     *  ignoring any dependent implicit arguments.
     */
    def shallowTest(ref: TermRef): Boolean =
      System.currentTimeMillis < deadLine
      && inContext(testContext()) {
        def test(pt: Type): Boolean = pt match
          case ViewProto(argType, OrType(rt1, rt2)) =>
            // Union types do not constrain results, since comparison with a union
            // type on the right might lose information. See ProtoTypes.disregardProto.
            // To regain precision, test both sides separately.
            test(ViewProto(argType, rt1)) || test(ViewProto(argType, rt2))
          case pt: ViewProto =>
            pt.isMatchedBy(ref)
          case _ =>
            normalize(ref, pt) <:< pt
        test(pt)
      }

    /** Test whether a full given term can be synthesized that matches
     *  the expected type `pt`.
     */
    def deepTest(ref: TermRef): Boolean =
      System.currentTimeMillis < deadLine
      && {
        val task = new TimerTask:
          def run() =
            implicits.println(i"Cancelling test of $ref when making suggestions for error in ${ctx.source}")
            ctx.run.nn.isCancelled = true
        val span = ctx.owner.srcPos.span
        val (expectedType, argument, kind) = pt match
          case ViewProto(argType, resType) =>
            (resType,
             untpd.Ident(ref.name).withSpan(span).withType(argType),
             if hasExtMethod(ref, resType) then Candidate.Extension
             else Candidate.Conversion)
          case _ =>
            (pt, EmptyTree, Candidate.Value)
        val candidate = Candidate(ref, kind, 0)
        try
          timer.schedule(task, testOneImplicitTimeOut)
          typedImplicit(candidate, expectedType, argument, span)(
            using testContext()).isSuccess
        finally
          val run = ctx.run.nn
          if task.cancel() then // timer task has not run yet
            assert(!run.isCancelled)
          else
            while !run.isCancelled do () // wait until timer task has run to completion
            run.isCancelled = false
      }
    end deepTest

    /** Optionally, an extension method reference `site.name` that is
     *  applicable to `argType`.
     */
    def extensionMethod(site: TermRef, name: TermName, argType: Type): Option[TermRef] =
      site.member(name)
        .alternatives
        .map(mbr => TermRef(site, mbr.symbol))
        .filter(ref => ctx.typer.isApplicableExtensionMethod(ref, argType))
        .headOption

    try
      val roots = suggestionRoots
        .filterNot(root => defn.rootImportTypes.exists(_.symbol == root.symbol))
          // don't suggest things that are imported by default

      def extensionImports = pt match
        case ViewProto(argType, SelectionProto(name: TermName, _, _, _, _)) =>
          roots.flatMap(extensionMethod(_, name, argType))
        case _ =>
          Nil

      roots
        .flatMap(_.implicitMembers.filter { ref =>
          !alreadyAvailableCandidates(ref.symbol) && shallowTest(ref)
        })
          // filter whether the head of the implicit can match
        .partition(deepTest)
          // partition into full matches and head matches
        match
          case (Nil, partials) => (extensionImports, partials)
          case givenImports => givenImports
    catch case NonFatal(ex) =>
      if ctx.settings.Ydebug.value then
        println("caught exception when searching for suggestions")
        ex.printStackTrace()
      (Nil, Nil)
    finally
      timer.cancel()
      reduceTimeBudget(((System.currentTimeMillis() - start) min Int.MaxValue).toInt)
  end importSuggestions

  /** Reduce next timeout for import suggestions by the amount of time it took
   *  for current search, but but never less than to half of the previous budget.
   */
  private def reduceTimeBudget(used: Int)(using Context) =
    val run = ctx.run.nn
    run.importSuggestionBudget =
      (run.importSuggestionBudget - used) max (run.importSuggestionBudget / 2)

  /** The `ref` parts of this list of pairs, discarding subsequent elements that
   *  have the same String part. Elements are sorted by their String parts.
   */
  extension (refs: List[(TermRef, String)]) def distinctRefs(using Context): List[TermRef] =
    val buf = new mutable.ListBuffer[TermRef]
    var last = ""
    for (ref, str) <- refs do
      if last != str then
        buf += ref
        last = str
    buf.toList

  /** The best `n` references in `refs`, according to `compare`
   *  `compare` is a partial order. If there's a tie, we take elements
   *  in the order thy appear in the list.
   */
  extension (refs: List[TermRef]) def best(n: Int)(using Context): List[TermRef] =
    val top = new Array[TermRef](n)
    var filled = 0
    val rest = new mutable.ListBuffer[TermRef]
    val noImplicitsCtx = ctx.retractMode(Mode.ImplicitsEnabled)
    for ref <- refs do
      var i = 0
      var diff = 0
      while i < filled && diff == 0 do
        diff = compare(ref, top(i), preferGeneral = true)(using noImplicitsCtx)
        if diff > 0 then
          rest += top(i)
          top(i) = ref
        i += 1
      end while
      if diff == 0 && filled < n then
        top(filled) = ref
        filled += 1
      else if diff <= 0 then
        rest += ref
    end for
    val remaining =
      if filled < n && rest.nonEmpty then rest.toList.best(n - filled)
      else Nil
    top.take(filled).toList ++ remaining
  //end best  TODO: re-enable with new syntax

  /** An addendum to an error message where the error might be fixed
   *  by some implicit value of type `pt` that is however not found.
   *  The addendum suggests given imports that might fix the problem.
   *  If there's nothing to suggest, an empty string is returned.
   */
  override def importSuggestionAddendum(pt: Type)(using Context): String =
    if isCaptureChecking then
      return "" // it's too late then to look for implicits
    val (fullMatches, headMatches) =
      importSuggestions(pt)(using ctx.fresh.setExploreTyperState())
    implicits.println(i"suggestions for $pt in ${ctx.owner} = ($fullMatches%, %, $headMatches%, %)")
    val (suggestedRefs, help) =
      if fullMatches.nonEmpty then (fullMatches, "fix")
      else (headMatches, "make progress towards fixing")
    def importString(ref: TermRef): String =
      val imported =
        if ref.symbol.is(ExtensionMethod) then
          s"${ctx.printer.toTextPrefixOf(ref).show}${ref.symbol.name}"
        else
          ctx.printer.toTextRef(ref).show
      s"  import $imported"
    val suggestions = suggestedRefs
      .zip(suggestedRefs.map(importString))
      .filter((ref, str) => str.contains('.')) // must be a real import with `.`
      .sortBy(_._2)         // sort first alphabetically for stability
      .distinctRefs         // TermRefs might be different but generate the same strings
      .best(MaxSuggestions) // take MaxSuggestions best references according to specificity
      .map(importString)
    if suggestions.isEmpty then ""
    else
      val fix =
        if suggestions.tail.isEmpty then "The following import"
        else "One of the following imports"
      i"""
         |
         |$fix might $help the problem:
         |
         |$suggestions%\n%
         |
         |"""
  end importSuggestionAddendum
end ImportSuggestions
