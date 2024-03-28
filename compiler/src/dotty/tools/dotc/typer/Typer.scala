package dotty.tools
package dotc
package typer

import backend.sjs.JSDefinitions
import core.*
import ast.*
import Trees.*
import Constants.*
import StdNames.*
import Scopes.*
import Denotations.*
import ProtoTypes.*
import Contexts.*
import Symbols.*
import Types.*
import SymDenotations.*
import Annotations.*
import Names.*
import NameOps.*
import NameKinds.*
import NamerOps.*
import ContextOps.*
import Flags.*
import Decorators.*
import ErrorReporting.*
import Checking.*
import Inferencing.*
import Dynamic.isDynamicExpansion
import EtaExpansion.etaExpand
import TypeComparer.CompareResult
import inlines.{Inlines, PrepareInlineable}
import util.Spans.*
import util.common.*
import util.{Property, SimpleIdentityMap, SrcPos}
import Applications.{tupleComponentTypes, wrapDefs, defaultArgument}

import collection.mutable
import annotation.tailrec
import Implicits.*
import util.Stats.record
import config.Printers.{gadts, typr}
import config.Feature
import config.Feature.{sourceVersion, migrateTo3}
import config.SourceVersion.*
import rewrites.Rewrites, Rewrites.patch
import staging.StagingLevel
import reporting.*
import Nullables.*
import NullOpsDecorator.*
import cc.{CheckCaptures, isRetainsLike}
import config.Config
import config.MigrationVersion

import scala.annotation.constructorOnly

object Typer {

  /** The precedence of bindings which determines which of several bindings will be
   *  accessed by an Ident.
   */
  enum BindingPrec {
    case NothingBound, PackageClause, WildImport, NamedImport, Inheritance, Definition

    def isImportPrec = this == NamedImport || this == WildImport

    /** special cases: definitions beat imports, and named imports beat
     *  wildcard imports, provided both are in contexts with same scope */
    def beats(prevPrec: BindingPrec): Boolean =
      this == Definition || this == NamedImport && prevPrec == WildImport
  }

  /** Assert tree has a position, unless it is empty or a typed splice */
  def assertPositioned(tree: untpd.Tree)(using Context): Unit =
    if (!tree.isEmpty && !tree.isInstanceOf[untpd.TypedSplice] && ctx.typerState.isGlobalCommittable)
      assert(tree.span.exists, i"position not set for $tree # ${tree.uniqueId} of ${tree.getClass} in ${tree.source}")

  /** An attachment for GADT constraints that were inferred for a pattern. */
  val InferredGadtConstraints = new Property.StickyKey[core.GadtConstraint]

  /** An attachment on a Select node with an `apply` field indicating that the `apply`
   *  was inserted by the Typer.
   */
  private val InsertedApply = new Property.Key[Unit]

  /** An attachment on a result of an implicit conversion or extension method
   *  that was added by tryInsertImplicitOnQualifier. Needed to prevent infinite
   *  expansions in error cases (e.g. in fuzzy/i9293.scala).
   */
  private val InsertedImplicitOnQualifier = new Property.Key[Unit]

  /** An attachment on a tree `t` occurring as part of a `t()` where
   *  the `()` was dropped by the Typer.
   */
  private val DroppedEmptyArgs = new Property.Key[Unit]

  /** An attachment that indicates a failed conversion or extension method
   *  search was tried on a tree. This will in some cases be reported in error messages
   */
  private[typer] val HiddenSearchFailure = new Property.Key[List[SearchFailure]]

  /** Is tree a compiler-generated `.apply` node that refers to the
   *  apply of a function class?
   */
  private[typer] def isSyntheticApply(tree: tpd.Tree): Boolean = tree match {
    case tree: tpd.Select => tree.hasAttachment(InsertedApply)
    case TypeApply(fn, targs) => isSyntheticApply(fn) && targs.forall(_.isInstanceOf[tpd.InferredTypeTree])
    case _ => false
  }

  /** Add `fail` to the list of search failures attached to `tree` */
  def rememberSearchFailure(tree: tpd.Tree, fail: SearchFailure) =
    tree.putAttachment(HiddenSearchFailure,
      fail :: tree.attachmentOrElse(HiddenSearchFailure, Nil))
}
/** Typecheck trees, the main entry point is `typed`.
 *
 *  @param nestingLevel The nesting level of the `scope` of this Typer.
 */
class Typer(@constructorOnly nestingLevel: Int = 0) extends Namer
               with TypeAssigner
               with Applications
               with Implicits
               with ImportSuggestions
               with Inferencing
               with Dynamic
               with Checking
               with QuotesAndSplices
               with Deriving
               with Migrations {

  import Typer.*
  import tpd.{cpy => _, _}
  import untpd.cpy

  /** The scope of the typer.
   *  For nested typers (cf `Namer#nestedTyper`), this is a place parameters are
   *  entered during completion and where they survive until typechecking. A
   *  context with this typer also has this scope.
   */
  val scope: MutableScope = newScope(nestingLevel)

  /** A temporary data item valid for a single typed ident:
   *  The set of all root import symbols that have been
   *  encountered as a qualifier of an import so far.
   *  Note: It would be more proper to move importedFromRoot into typedIdent.
   *  We should check that this has no performance degradation, however.
   */
  private var unimported: Set[Symbol] = Set()

  /** Temporary data item for single call to typed ident:
   *  This symbol would be found under Scala2 mode, but is not
   *  in dotty (because dotty conforms to spec section 2
   *  wrt to package member resolution but scalac doe not).
   */
  private var foundUnderScala2: Type = NoType

  // Overridden in derived typers
  def newLikeThis(nestingLevel: Int): Typer = new Typer(nestingLevel)

  /** Find the type of an identifier with given `name` in given context `ctx`.
   *   @param name       the name of the identifier
   *   @param pt         the expected type
   *   @param required   flags the result's symbol must have
   *   @param excluded   flags the result's symbol must not have
   *   @param pos        indicates position to use for error reporting
   *   @param altImports a ListBuffer in which alternative imported references are
   *                     collected in case `findRef` is called from an expansion of
   *                     an extension method, i.e. when `e.m` is expanded to `m(e)` and
   *                     a reference for `m` is searched. `null` in all other situations.
   */
  def findRef(name: Name, pt: Type, required: FlagSet, excluded: FlagSet, pos: SrcPos,
      altImports: mutable.ListBuffer[TermRef] | Null = null)(using Context): Type = {
    val refctx = ctx
    val noImports = ctx.mode.is(Mode.InPackageClauseName)
    def suppressErrors = excluded.is(ConstructorProxy)
      // when searching for references shadowed by a constructor proxy, do not report errors
    def fail(msg: Message) =
      if !suppressErrors then report.error(msg, pos)

    /** A symbol qualifies if it really exists and is not a package class.
     *  Package classes are part of their parent's scope, because otherwise
     *  we could not reload them via `_.member`. On the other hand, accessing a
     *  package as a type from source is always an error.

     *  In addition:
     *    - if we are in a constructor of a pattern, we ignore all definitions
     *      which are parameterized (including nullary) methods and not accessors
     *      (note: if we don't do that case x :: xs in class List would return the :: method).
     *    - Members of the empty package can be accessed only from within the empty package.
     *      Note: it would be cleaner to never nest package definitions in empty package definitions,
     *      but then we'd have to give up the fiction that a compilation unit consists of
     *      a single tree (because a source file may have both toplevel classes which go
     *      into the empty package and package definitions, which would have to stay outside).
     *      Since the principle of a single tree per compilation unit is assumed by many
     *      tools, we did not want to take that step.
     */
    def qualifies(denot: Denotation): Boolean =
      def isRealMethod(sd: SingleDenotation): Boolean =
        sd.symbol.is(Method, butNot = Accessor) && !sd.info.isParameterless
      reallyExists(denot)
      && (!pt.isInstanceOf[UnapplySelectionProto] || denot.hasAltWith(!isRealMethod(_)))
      && !denot.symbol.is(PackageClass)
      && {
        var owner = denot.symbol.maybeOwner
        if owner.isPackageObject then owner = owner.owner
        !owner.isEmptyPackage || ctx.owner.enclosingPackageClass.isEmptyPackage
      }

    /** Find the denotation of enclosing `name` in given context `ctx`.
     *  @param previous    A denotation that was found in a more deeply nested scope,
     *                     or else `NoDenotation` if nothing was found yet.
     *  @param prevPrec    The binding precedence of the previous denotation,
     *                     or else `nothingBound` if nothing was found yet.
     *  @param prevCtx     The context of the previous denotation,
     *                     or else `NoContext` if nothing was found yet.
     */
    def findRefRecur(previous: Type, prevPrec: BindingPrec, prevCtx: Context)(using Context): Type = {
      import BindingPrec.*

      /** Check that any previously found result from an inner context
       *  does properly shadow the new one from an outer context.
       *  @param found     The newly found result
       *  @param newPrec   Its precedence
       *  @param scala2pkg Special mode where we check members of the same package, but defined
       *                   in different compilation units under Scala2. If set, and the
       *                   previous and new contexts do not have the same scope, we select
       *                   the previous (inner) definition. This models what scalac does.
       */
      def checkNewOrShadowed(found: Type, newPrec: BindingPrec, scala2pkg: Boolean = false)(using Context): Type =
        if !previous.exists || TypeComparer.isSameRef(previous, found) then
           found
        else if (prevCtx.scope eq ctx.scope) && newPrec.beats(prevPrec) then
          // special cases: definitions beat imports, and named imports beat
          // wildcard imports, provided both are in contexts with same scope
          found
        else
          if !scala2pkg && !previous.isError && !found.isError then
            fail(AmbiguousReference(name, newPrec, prevPrec, prevCtx,
              isExtension = previous.termSymbol.is(ExtensionMethod) && found.termSymbol.is(ExtensionMethod)))
          previous

      /** Assemble and check alternatives to an imported reference. This implies:
       *   - If we expand an extension method (i.e. altImports != null),
       *     search imports on the same level for other possible resolutions of `name`.
       *     The result and altImports together then contain all possible imported
       *     references of the highest possible precedence, where `NamedImport` beats
       *     `WildImport`.
       *   - Find a posssibly shadowing reference in an outer context.
       *     If the result is the same as `previous`, check that it is new or
       *     shadowed. This order of checking is necessary since an outer package-level
       *     definition might trump two conflicting inner imports, so no error should be
       *     issued in that case. See i7876.scala.
       *  @param previous   the previously found reference (which is an import)
       *  @param prevPrec   the precedence of the reference (either NamedImport or WildImport)
       *  @param prevCtx    the context in which the reference was found
       *  @param using_Context the outer context of `precCtx`
       */
      def checkImportAlternatives(previous: Type, prevPrec: BindingPrec, prevCtx: Context)(using Context): Type =

        def addAltImport(altImp: TermRef) =
          if !TypeComparer.isSameRef(previous, altImp)
              && !altImports.uncheckedNN.exists(TypeComparer.isSameRef(_, altImp))
          then
            altImports.uncheckedNN += altImp

        if altImports != null && ctx.isImportContext then
          val curImport = ctx.importInfo.uncheckedNN
          namedImportRef(curImport) match
            case altImp: TermRef =>
              if prevPrec == WildImport then
                // Discard all previously found references and continue with `altImp`
                altImports.clear()
                checkImportAlternatives(altImp, NamedImport, ctx)(using ctx.outer)
              else
                addAltImport(altImp)
                checkImportAlternatives(previous, prevPrec, prevCtx)(using ctx.outer)
            case _ =>
              if prevPrec == WildImport then
                wildImportRef(curImport) match
                  case altImp: TermRef => addAltImport(altImp)
                  case _ =>
              checkImportAlternatives(previous, prevPrec, prevCtx)(using ctx.outer)
        else
          val found = findRefRecur(previous, prevPrec, prevCtx)
          if found eq previous then checkNewOrShadowed(found, prevPrec)(using prevCtx)
          else found
      end checkImportAlternatives

      def selection(imp: ImportInfo, name: Name, checkBounds: Boolean): Type =
        imp.importSym.info match
          case ImportType(expr) =>
            val pre = expr.tpe
            val denot0 = pre.memberBasedOnFlags(name, required, excluded)
            var accessibleDenot = denot0.accessibleFrom(pre)(using refctx)
            if !accessibleDenot.exists && denot0.hasAltWith(_.symbol.is(Private)) then
              accessibleDenot = pre.memberBasedOnFlags(name, required, excluded | Private)
                .accessibleFrom(pre)(using refctx)
            // Pass refctx so that any errors are reported in the context of the
            // reference instead of the context of the import scope
            if accessibleDenot.exists then
              val denot =
                if checkBounds then
                  accessibleDenot.filterWithPredicate { mbr =>
                    mbr.matchesImportBound(if mbr.symbol.is(Given) then imp.givenBound else imp.wildcardBound)
                  }
                else accessibleDenot
              def isScalaJsPseudoUnion =
                denot.name == tpnme.raw.BAR && ctx.settings.scalajs.value && denot.symbol == JSDefinitions.jsdefn.PseudoUnionClass
              // Just like Scala2Unpickler reinterprets Scala.js pseudo-unions
              // as real union types, we want references to `A | B` in sources
              // to be typed as a real union even if `js.|` has been imported,
              // so we ignore that import.
              if reallyExists(denot) && !isScalaJsPseudoUnion then
                if unimported.isEmpty || !unimported.contains(pre.termSymbol) then
                  return pre.select(name, denot)
          case _ =>
            if imp.importSym.isCompleting then
              report.warning(i"cyclic ${imp.importSym}, ignored", pos)
        NoType

      /** The type representing a named import with enclosing name when imported
       *  from given `site` and `selectors`.
       */
      def namedImportRef(imp: ImportInfo)(using Context): Type = {
        val termName = name.toTermName
        def recur(selectors: List[untpd.ImportSelector]): Type = selectors match
          case selector :: rest =>
            def checkUnambiguous(found: Type) =
              val other = recur(selectors.tail)
              if other.exists && found.exists && found != other then
                fail(em"reference to `$name` is ambiguous; it is imported twice")
              found

            if selector.rename == termName && !selector.isUnimport then
              val memberName =
                if selector.name == termName then name
                else if name.isTypeName then selector.name.toTypeName
                else selector.name
              checkUnambiguous(selection(imp, memberName, checkBounds = false))
            else
              recur(rest)

          case nil =>
            NoType

        recur(imp.selectors)
      }

      /** The type representing a wildcard import with enclosing name when imported
       *  from given import info
       */
      def wildImportRef(imp: ImportInfo)(using Context): Type =
        if (imp.isWildcardImport && !imp.excluded.contains(name.toTermName) && name != nme.CONSTRUCTOR)
          selection(imp, name, checkBounds = true)
        else NoType

      /** Is (some alternative of) the given predenotation `denot`
       *  defined in current compilation unit?
       */
      def isDefinedInCurrentUnit(denot: Denotation)(using Context): Boolean = denot match {
        case MultiDenotation(d1, d2) => isDefinedInCurrentUnit(d1) || isDefinedInCurrentUnit(d2)
        case denot: SingleDenotation => (ctx.compilationUnit ne NoCompilationUnit) && denot.symbol.source == ctx.compilationUnit.source
      }

      /** Is `denot` the denotation of a self symbol? */
      def isSelfDenot(denot: Denotation)(using Context) = denot match {
        case denot: SymDenotation => denot.is(SelfName)
        case _ => false
      }

      /** Would import of kind `prec` be not shadowed by a nested higher-precedence definition? */
      def isPossibleImport(prec: BindingPrec)(using Context) =
        !noImports &&
        (prevPrec.ordinal < prec.ordinal || prevPrec == prec && (prevCtx.scope eq ctx.scope))

      @tailrec def loop(lastCtx: Context)(using Context): Type =
        if (ctx.scope eq EmptyScope) previous
        else {
          var result: Type = NoType
          val curOwner = ctx.owner

          /** Is curOwner a package object that should be skipped?
           *  A package object should always be skipped if we look for a term.
           *  That way we make sure we consider all overloaded alternatives of
           *  a definition, even if they are in different source files.
           *  If we are looking for a type, a package object should be skipped
           *  only if it does not contain opaque definitions. Package objects
           *  with opaque definitions are significant, since opaque aliases
           *  are only seen if the prefix is the this-type of the package object.
           */
          def isTransparentPackageObject =
            curOwner.isPackageObject && (name.isTermName || !curOwner.is(Opaque))

          // Can this scope contain new definitions? This is usually the first
          // context where either the scope or the owner changes wrt the
          // context immediately nested in it. But for package contexts, it's
          // the opposite: the last context before the package changes. This distinction
          // is made so that top-level imports following a package clause are
          // logically nested in that package clause. Finally, for the root package
          // we switch back to the original test. This means that top-level packages in
          // the root package take priority over root imports. For instance,
          // a top-level io package takes priority over scala.io.
          // It would be nice if we could drop all this complication, and
          // always use the second condition. Unfortunately compileStdLib breaks
          // with an error on CI which I cannot replicate locally (not even
          // with the exact list of files given).
          val isNewDefScope =
            if curOwner.is(Package) && !curOwner.isRoot then
              curOwner ne ctx.outer.owner
            else
              ((ctx.scope ne lastCtx.scope) || (curOwner ne lastCtx.owner))
              && !isTransparentPackageObject

          // Does reference `tp` refer only to inherited symbols?
          def isInherited(denot: Denotation) =
            def isCurrent(mbr: SingleDenotation): Boolean =
              !mbr.symbol.exists || mbr.symbol.owner == ctx.owner
            denot match
              case denot: SingleDenotation => !isCurrent(denot)
              case denot => !denot.hasAltWith(isCurrent)

          def checkNoOuterDefs(denot: Denotation, last: Context, prevCtx: Context): Unit =
            def sameTermOrType(d1: SingleDenotation, d2: Denotation) =
              d2.containsSym(d1.symbol) || d2.hasUniqueSym && {
                val sym1 = d1.symbol
                val sym2 = d2.symbol
                if sym1.isTerm then
                  sym1.isStableMember &&
                  sym2.isStableMember &&
                  sym1.termRef =:= sym2.termRef
                else
                  (sym1.isAliasType || sym2.isAliasType) && d1.info =:= d2.info
              }
            val outer = last.outer
            val owner = outer.owner
            if (owner eq last.owner) && (outer.scope eq last.scope) then
              checkNoOuterDefs(denot, outer, prevCtx)
            else if !owner.is(Package) then
              val scope = if owner.isClass then owner.info.decls else outer.scope
              val competing = scope.denotsNamed(name).filterWithFlags(required, excluded)
              if competing.exists then
                val symsMatch = competing
                  .filterWithPredicate(sd => sameTermOrType(sd, denot))
                  .exists
                if !symsMatch && !suppressErrors then
                  report.errorOrMigrationWarning(
                    AmbiguousReference(name, Definition, Inheritance, prevCtx)(using outer),
                    pos, MigrationVersion.Scala2to3)
                  if MigrationVersion.Scala2to3.needsPatch then
                    patch(Span(pos.span.start),
                      if prevCtx.owner == refctx.owner.enclosingClass then "this."
                      else s"${prevCtx.owner.name}.this.")
              else
                checkNoOuterDefs(denot, outer, prevCtx)

          if isNewDefScope then
            val defDenot = ctx.denotNamed(name, required, excluded)
            if (qualifies(defDenot)) {
              val found =
                if (isSelfDenot(defDenot)) curOwner.enclosingClass.thisType
                else if (ctx.isJava && defDenot.symbol.isStatic) {
                  defDenot.symbol.namedType
                }
                else if (ctx.isJava && defDenot.symbol.isClass) {
                  // in a java context a raw identifier to a class should have a widened prefix.
                  defDenot.symbol.javaTypeRef
                } else {
                  val effectiveOwner =
                    if (curOwner.isTerm && defDenot.symbol.maybeOwner.isType)
                      // Don't mix NoPrefix and thisType prefixes, since type comparer
                      // would not detect types to be compatible.
                      defDenot.symbol.owner
                    else
                      curOwner
                  effectiveOwner.thisType.select(name, defDenot)
                }
              if !curOwner.is(Package) || isDefinedInCurrentUnit(defDenot) then
                result = checkNewOrShadowed(found, Definition) // no need to go further out, we found highest prec entry
                found match
                  case found: NamedType
                  if curOwner.isClass && isInherited(found.denot) && !ctx.compilationUnit.isJava =>
                    checkNoOuterDefs(found.denot, ctx, ctx)
                  case _ =>
              else
                if migrateTo3 && !foundUnderScala2.exists then
                  foundUnderScala2 = checkNewOrShadowed(found, Definition, scala2pkg = true)
                if (defDenot.symbol.is(Package))
                  result = checkNewOrShadowed(previous orElse found, PackageClause)
                else if (prevPrec.ordinal < PackageClause.ordinal)
                  result = findRefRecur(found, PackageClause, ctx)(using ctx.outer)
            }

          if result.exists then result
          else {  // find import
            val outer = ctx.outer
            val curImport = ctx.importInfo
            def updateUnimported() =
              if (curImport.nn.unimported ne NoSymbol) unimported += curImport.nn.unimported
            if (curOwner.is(Package) && curImport != null && curImport.isRootImport && previous.exists)
              previous // no more conflicts possible in this case
            else if (isPossibleImport(NamedImport) && (curImport nen outer.importInfo)) {
              val namedImp = namedImportRef(curImport.uncheckedNN)
              if (namedImp.exists)
                checkImportAlternatives(namedImp, NamedImport, ctx)(using outer)
              else if (isPossibleImport(WildImport) && !curImport.nn.importSym.isCompleting) {
                val wildImp = wildImportRef(curImport.uncheckedNN)
                if (wildImp.exists)
                  checkImportAlternatives(wildImp, WildImport, ctx)(using outer)
                else {
                  updateUnimported()
                  loop(ctx)(using outer)
                }
              }
              else {
                updateUnimported()
                loop(ctx)(using outer)
              }
            }
            else loop(ctx)(using outer)
          }
        }

      // begin findRefRecur
      loop(NoContext)
    }

    findRefRecur(NoType, BindingPrec.NothingBound, NoContext)
  }

  /** If `tree`'s type is a `TermRef` identified by flow typing to be non-null, then
   *  cast away `tree`s nullability. Otherwise, `tree` remains unchanged.
   *
   *  Example:
   *  If x is a trackable reference and we know x is not null at this point,
   *  (x: T | Null) => x.$asInstanceOf$[x.type & T]
   */
  def toNotNullTermRef(tree: Tree, pt: Type)(using Context): Tree = tree.tpe match
    case ref: TermRef
    if pt != LhsProto && // Ensure it is not the lhs of Assign
    ctx.notNullInfos.impliesNotNull(ref) &&
    // If a reference is in the context, it is already trackable at the point we add it.
    // Hence, we don't use isTracked in the next line, because checking use out of order is enough.
    !ref.usedOutOfOrder =>
      ref match
        case OrNull(tpnn) => tree.cast(AndType(ref, tpnn))
        case _            => tree
    case _ =>
      tree

  /** Attribute an identifier consisting of a simple name or wildcard
   *
   *  @param tree      The tree representing the identifier.
   *  Transformations: (1) Prefix class members with this.
   *                   (2) Change imported symbols to selections.
   *                   (3) Change pattern Idents id (but not wildcards) to id @ _
   */
  def typedIdent(tree: untpd.Ident, pt: Type)(using Context): Tree =
    record("typedIdent")
    val name = tree.name
    def kind = if (name.isTermName) "" else "type "
    typr.println(s"typed ident $kind$name in ${ctx.owner}")
    if ctx.mode.is(Mode.Pattern) then
      if name == nme.WILDCARD then
        return tree.withType(pt)
      if name == tpnme.WILDCARD then
        return tree.withType(defn.AnyType)
      if untpd.isVarPattern(tree) && name.isTermName then
        return typed(desugar.patternVar(tree), pt)
    else if ctx.mode.isQuotedPattern then
      if untpd.isVarPattern(tree) && name.isTypeName then
        return typedQuotedTypeVar(tree, pt)
    end if

    // Shortcut for the root package, this is not just a performance
    // optimization, it also avoids forcing imports thus potentially avoiding
    // cyclic references.
    if (name == nme.ROOTPKG)
      val tree2 = tree.withType(defn.RootPackage.termRef)
      checkLegalValue(tree2, pt)
      return tree2

    val rawType =
      val saved1 = unimported
      val saved2 = foundUnderScala2
      unimported = Set.empty
      foundUnderScala2 = NoType
      try
        val found = findRef(name, pt, EmptyFlags, EmptyFlags, tree.srcPos)
        if foundUnderScala2.exists && !(foundUnderScala2 =:= found) then
          report.migrationWarning(
            em"""Name resolution will change.
              | currently selected                          : $foundUnderScala2
              | in the future, without -source 3.0-migration: $found""", tree.srcPos)
          foundUnderScala2
        else found
      finally
        unimported = saved1
        foundUnderScala2 = saved2

    /** Normally, returns `ownType` except if `ownType` is a constructor proxy,
     *  and there is another shadowed type accessible with the same name that is not:
     *    - if the prototype is an application:
     *      - if the shadowed type has a method alternative or an apply method,
     *        issue an ambiguity error
     *      - otherwise again return `ownType`
     *    - if the prototype is not an application, return the shadowed type
     */
    def checkNotShadowed(ownType: Type): Type =
      ownType match
        case ownType: TermRef if ownType.symbol.is(ConstructorProxy) =>
          findRef(name, pt, EmptyFlags, ConstructorProxy, tree.srcPos) match
            case shadowed: TermRef if !shadowed.symbol.maybeOwner.isEmptyPackage =>
              pt match
                case pt: FunOrPolyProto =>
                  def err(shadowedIsApply: Boolean) =
                    report.error(ConstrProxyShadows(ownType, shadowed, shadowedIsApply), tree.srcPos)
                  if shadowed.denot.hasAltWith(sd => sd.symbol.is(Method, butNot = Accessor)) then
                    err(shadowedIsApply = false)
                  else if shadowed.member(nme.apply).hasAltWith(_.symbol.is(Method, butNot = Accessor)) then
                    err(shadowedIsApply = true)
                case _ =>
                  return shadowed
            case shadowed =>
        case _ =>
      ownType

    def setType(ownType: Type): Tree =
      val checkedType = checkNotShadowed(ownType)
      val tree1 = checkedType match
        case checkedType: NamedType if !prefixIsElidable(checkedType) =>
          ref(checkedType).withSpan(tree.span)
        case _ =>
          def isScalaModuleRef = checkedType match
            case moduleRef: TypeRef if moduleRef.symbol.is(ModuleClass, butNot = JavaDefined) => true
            case _ => false
          if ctx.isJava && isScalaModuleRef then
            cpy.Ident(tree)(tree.name.unmangleClassName).withType(checkedType)
          else
            tree.withType(checkedType)
      val tree2 = toNotNullTermRef(tree1, pt)
      checkLegalValue(tree2, pt)
      tree2

    def isLocalExtensionMethodRef: Boolean = rawType match
      case rawType: TermRef =>
        rawType.denot.hasAltWith(_.symbol.is(ExtensionMethod))
        && !pt.isExtensionApplyProto
        && {
          val xmethod = ctx.owner.enclosingExtensionMethod
          rawType.denot.hasAltWith { alt =>
            alt.symbol.is(ExtensionMethod)
            && alt.symbol.extensionParam.span == xmethod.extensionParam.span
          }
        }
      case _ =>
        false

    if ctx.mode.is(Mode.InExtensionMethod) && isLocalExtensionMethodRef then
      val xmethod = ctx.owner.enclosingExtensionMethod
      val qualifier = untpd.ref(xmethod.extensionParam.termRef)
      val selection = untpd.cpy.Select(tree)(qualifier, name)
      typed(selection, pt)
    else if rawType.exists then
      setType(ensureAccessible(rawType, superAccess = false, tree.srcPos))
    else if name == nme._scope then
      // gross hack to support current xml literals.
      // awaiting a better implicits based solution for library-supported xml
      ref(defn.XMLTopScopeModule.termRef)
    else if name.toTermName == nme.ERROR then
      setType(UnspecifiedErrorType)
    else if ctx.owner.isConstructor && !ctx.owner.isPrimaryConstructor
        && ctx.owner.owner.unforcedDecls.lookup(tree.name).exists
    then // we are in the arguments of a this(...) constructor call
      errorTree(tree, em"$tree is not accessible from constructor arguments")
    else
      errorTree(tree, MissingIdent(tree, kind, name, pt))
  end typedIdent

  /** (1) If this reference is neither applied nor selected, check that it does
   *      not refer to a package or Java companion object.
   *  (2) Check that a stable identifier pattern is indeed stable (SLS 8.1.5)
   */
  private def checkLegalValue(tree: Tree, pt: Type)(using Context): Unit =
    checkValue(tree, pt)
    if ctx.mode.is(Mode.Pattern)
       && !tree.isType
       && !pt.isInstanceOf[ApplyingProto]
       && !tree.tpe.match
        case tp: NamedType => tp.denot.hasAltWith(_.symbol.isStableMember && tp.prefix.isStable || tp.info.isStable)
        case tp            => tp.isStable
       && !isWildcardArg(tree)
    then
      report.error(StableIdentPattern(tree, pt), tree.srcPos)

  def typedSelect(tree0: untpd.Select, pt: Type, qual: Tree)(using Context): Tree =
    val selName = tree0.name
    val tree = cpy.Select(tree0)(qual, selName)
    val superAccess = qual.isInstanceOf[Super]
    val rawType = selectionType(tree, qual)
    val checkedType = accessibleType(rawType, superAccess)

    def finish(tree: untpd.Select, qual: Tree, checkedType: Type): Tree =
      val select = toNotNullTermRef(assignType(tree, checkedType), pt)
      if selName.isTypeName then checkStable(qual.tpe, qual.srcPos, "type prefix")
      checkLegalValue(select, pt)
      ConstFold(select)

    if checkedType.exists then
      finish(tree, qual, checkedType)
    else if selName == nme.apply && qual.tpe.widen.isInstanceOf[MethodType] then
      // Simplify `m.apply(...)` to `m(...)`
      qual
    else if couldInstantiateTypeVar(qual.tpe.widen) then
       // there's a simply visible type variable in the result; try again with a more defined qualifier type
       // There's a second trial where we try to instantiate all type variables in `qual.tpe.widen`,
       // but that is done only after we search for extension methods or conversions.
      typedSelect(tree, pt, qual)
    else if qual.tpe.isSmallGenericTuple then
      val elems = qual.tpe.widenTermRefExpr.tupleElementTypes.getOrElse(Nil)
      typedSelect(tree, pt, qual.cast(defn.tupleType(elems)))
    else
      val tree1 = {
        if selName.isTypeName then EmptyTree
        else tryExtensionOrConversion(
          tree, pt, IgnoredProto(pt), qual, ctx.typerState.ownedVars, this, inSelect = true)
        }.orElse {
          if ctx.gadt.isNarrowing then
            // try GADT approximation if we're trying to select a member
            // Member lookup cannot take GADTs into account b/c of cache, so we
            // approximate types based on GADT constraints instead. For an example,
            // see MemberHealing in gadt-approximation-interaction.scala.
            val wtp = qual.tpe.widen
            gadts.println(i"Trying to heal member selection by GADT-approximating $wtp")
            val gadtApprox = Inferencing.approximateGADT(wtp)
            gadts.println(i"GADT-approximated $wtp ~~ $gadtApprox")
            val qual1 = qual.cast(gadtApprox)
            val tree1 = cpy.Select(tree0)(qual1, selName)
            val checkedType1 = accessibleType(selectionType(tree1, qual1), superAccess = false)
            if checkedType1.exists then
              gadts.println(i"Member selection healed by GADT approximation")
              finish(tree1, qual1, checkedType1)
            else if qual1.tpe.isSmallGenericTuple then
              gadts.println(i"Tuple member selection healed by GADT approximation")
              typedSelect(tree, pt, qual1)
            else
              tryExtensionOrConversion(tree1, pt, IgnoredProto(pt), qual1, ctx.typerState.ownedVars, this, inSelect = true)
          else EmptyTree
        }
      if !tree1.isEmpty then
        tree1
      else if canDefineFurther(qual.tpe.widen) then
        typedSelect(tree, pt, qual)
      else if qual.tpe.derivesFrom(defn.DynamicClass)
        && selName.isTermName && !isDynamicExpansion(tree)
      then
        val tree2 = cpy.Select(tree0)(untpd.TypedSplice(qual), selName)
        if pt.isInstanceOf[FunOrPolyProto] || pt == LhsProto then
          assignType(tree2, TryDynamicCallType)
        else
          typedDynamicSelect(tree2, Nil, pt)
      else
        assignType(tree,
          rawType match
            case rawType: NamedType =>
              inaccessibleErrorType(rawType, superAccess, tree.srcPos)
            case _ =>
              notAMemberErrorType(tree, qual, pt))
  end typedSelect

  def typedSelect(tree: untpd.Select, pt: Type)(using Context): Tree = {
    record("typedSelect")

    def typeSelectOnTerm(using Context): Tree =
      val qual = typedExpr(tree.qualifier, shallowSelectionProto(tree.name, pt, this, tree.nameSpan))
      if ctx.isJava then
        javaSelection(qual)
      else
        typedSelect(tree, pt, qual).withSpan(tree.span).computeNullable()

    def javaSelection(qual: Tree)(using Context) =
      val tree1 = assignType(cpy.Select(tree)(qual, tree.name), qual)
      tree1.tpe match
        case moduleRef: TypeRef if moduleRef.symbol.is(ModuleClass, butNot = JavaDefined) =>
          // handle unmangling of module names (Foo$ -> Foo[ModuleClass])
          cpy.Select(tree)(qual, tree.name.unmangleClassName).withType(moduleRef)
        case _ =>
          tree1

    def tryJavaSelectOnType(using Context): Tree = tree.qualifier match {
      case sel @ Select(qual, name) =>
        val qual1 = untpd.cpy.Select(sel)(qual, name.toTypeName)
        val qual2 = typedType(qual1, WildcardType)
        javaSelection(qual2)

      case id @ Ident(name) =>
        val qual1 = untpd.cpy.Ident(id)(name.toTypeName)
        val qual2 = typedType(qual1, WildcardType)
        javaSelection(qual2)

      case _ =>
        errorTree(tree, em"cannot convert to type selection") // will never be printed due to fallback
    }

    def selectWithFallback(fallBack: Context ?=> Tree) =
      tryAlternatively(typeSelectOnTerm)(fallBack)

    if (tree.qualifier.isType) {
      val qual1 = typedType(tree.qualifier, shallowSelectionProto(tree.name, pt, this, tree.nameSpan))
      assignType(cpy.Select(tree)(qual1, tree.name), qual1)
    }
    else if (ctx.isJava && tree.name.isTypeName)
      // SI-3120 Java uses the same syntax, A.B, to express selection from the
      // value A and from the type A. We have to try both.
      selectWithFallback(tryJavaSelectOnType) // !!! possibly exponential bcs of qualifier retyping
    else
      typeSelectOnTerm
  }

  def typedThis(tree: untpd.This)(using Context): Tree = {
    record("typedThis")
    assignType(tree)
  }

  def typedSuper(tree: untpd.Super, pt: Type)(using Context): Tree = {
    val qual1 = typed(tree.qual)
    val enclosingInlineable = ctx.owner.ownersIterator.findSymbol(_.isInlineMethod)
    if (enclosingInlineable.exists && !PrepareInlineable.isLocal(qual1.symbol, enclosingInlineable))
      report.error(SuperCallsNotAllowedInlineable(enclosingInlineable), tree.srcPos)
    pt match {
      case pt: SelectionProto if pt.name.isTypeName =>
        qual1 // don't do super references for types; they are meaningless anyway
      case _ =>
        assignType(cpy.Super(tree)(qual1, tree.mix), qual1)
    }
  }

  def typedNumber(tree: untpd.Number, pt: Type)(using Context): Tree = {
    import scala.util.FromDigits.*
    import untpd.NumberKind.*
    record("typedNumber")
    val digits = tree.digits
    val target = pt.dealias
    def lit(value: Any) = Literal(Constant(value)).withSpan(tree.span)
    try {
      // Special case primitive numeric types
      if (target.isRef(defn.IntClass) ||
          target.isRef(defn.CharClass) ||
          target.isRef(defn.ByteClass) ||
          target.isRef(defn.ShortClass))
        tree.kind match {
          case Whole(radix) => return lit(intFromDigits(digits, radix))
          case _ =>
        }
      else if (target.isRef(defn.LongClass))
        tree.kind match {
          case Whole(radix) => return lit(longFromDigits(digits, radix))
          case _ =>
        }
      else if (target.isRef(defn.FloatClass))
        tree.kind match {
          case Whole(16) => // cant parse hex literal as float
          case _         =>
            val float = floatFromDigits(digits)
            if digits.toIntOption.exists(_ != float.toInt) then
              report.warning(LossyWideningConstantConversion(defn.IntType, target), tree.srcPos)
            return lit(float)
        }
      else if (target.isRef(defn.DoubleClass))
        tree.kind match {
          case Whole(16) => // cant parse hex literal as double
          case _         => return lit(doubleFromDigits(digits))
        }
      else if Feature.genericNumberLiteralsEnabled
          && target.isValueType && isFullyDefined(target, ForceDegree.none)
      then
        // If expected type is defined with a FromDigits instance, use that one
        val fromDigitsCls = tree.kind match {
          case Whole(10) => defn.FromDigitsClass
          case Whole(_) => defn.FromDigits_WithRadixClass
          case Decimal => defn.FromDigits_DecimalClass
          case Floating => defn.FromDigits_FloatingClass
        }
        inferImplicit(fromDigitsCls.typeRef.appliedTo(target), EmptyTree, tree.span) match {
          case SearchSuccess(arg, _, _, _) =>
            val fromDigits = untpd.Select(untpd.TypedSplice(arg), nme.fromDigits).withSpan(tree.span)
            val firstArg = Literal(Constant(digits))
            val otherArgs = tree.kind match {
              case Whole(r) if r != 10 => Literal(Constant(r)) :: Nil
              case _ => Nil
            }
            var app: untpd.Tree = untpd.Apply(fromDigits, firstArg :: otherArgs)
            if (ctx.mode.is(Mode.Pattern)) app = untpd.Block(Nil, app)
            return typed(app, pt)
          case _ =>
        }
      // Otherwise convert to Int or Double according to digits format
      tree.kind match {
        case Whole(radix) => lit(intFromDigits(digits, radix))
        case _ => lit(doubleFromDigits(digits))
      }
    }
    catch {
      case ex: FromDigitsException =>
        report.error(ex.getMessage.nn, tree.srcPos)
        tree.kind match {
          case Whole(_) => lit(0)
          case _ => lit(0.0)
        }
    }
  }

  def typedLiteral(tree: untpd.Literal)(using Context): Tree = {
    val tree1 = assignType(tree)
    if (ctx.mode.is(Mode.Type)) tpd.SingletonTypeTree(tree1) // this ensures that tree is classified as a type tree
    else tree1
  }

  def typedNew(tree: untpd.New, pt: Type)(using Context): Tree =
    tree.tpt match {
      case templ: untpd.Template =>
        import untpd.*
        var templ1 = templ
        def isEligible(tp: Type) =
        	tp.exists
        	&& !tp.typeSymbol.is(Final)
        	&& (!tp.isTopType || tp.isAnyRef) // Object is the only toplevel class that can be instantiated
        if (templ1.parents.isEmpty &&
            isFullyDefined(pt, ForceDegree.flipBottom) &&
            isSkolemFree(pt) &&
            isEligible(pt.underlyingClassRef(refinementOK = false)))
          templ1 = cpy.Template(templ)(parents = untpd.TypeTree(pt) :: Nil)
        for case parent: RefTree <- templ1.parents do
          typedAhead(parent, tree => inferTypeParams(typedType(tree), pt))
        val anon = tpnme.ANON_CLASS
        val clsDef = TypeDef(anon, templ1).withFlags(Final | Synthetic)
        typed(cpy.Block(tree)(clsDef :: Nil, New(Ident(anon), Nil)), pt)
      case _ =>
        var tpt1 = typedType(tree.tpt)
        val tsym = tpt1.tpe.underlyingClassRef(refinementOK = false).typeSymbol
        if tsym.is(Package) then
          report error(em"$tsym cannot be instantiated", tpt1.srcPos)
        tpt1 = tpt1.withType(ensureAccessible(tpt1.tpe, superAccess = false, tpt1.srcPos))
        tpt1 match {
          case AppliedTypeTree(_, targs) =>
            for case targ: TypeBoundsTree <- targs do
              report.error(WildcardOnTypeArgumentNotAllowedOnNew(), targ.srcPos)
          case _ =>
        }

        assignType(cpy.New(tree)(tpt1), tpt1)
    }

  def typedTyped(tree: untpd.Typed, pt: Type)(using Context): Tree = {

    /*  Handles three cases:
     *  @param  ifPat    how to handle a pattern (_: T)
     *  @param  ifExpr   how to handle an expression (e: T)
     *  @param  wildName what name `w` to use in the rewriting of
     *                   (x: T) to (x @ (w: T)). This is either `_` or `_*`.
     */
    def cases(ifPat: => Tree, ifExpr: => Tree, wildName: TermName) = tree.expr match {
      case id: untpd.Ident if (ctx.mode is Mode.Pattern) =>
        if (id.name == nme.WILDCARD || id.name == nme.WILDCARD_STAR) ifPat
        else {
          import untpd.*
          typed(Bind(id.name, Typed(Ident(wildName), tree.tpt)).withSpan(tree.span), pt)
        }
      case _ => ifExpr
    }

    def ascription(tpt: Tree, isWildcard: Boolean) = {
      val underlyingTreeTpe =
        if (isRepeatedParamType(tpt)) TypeTree(defn.SeqType.appliedTo(pt :: Nil))
        else tpt
      val expr1 =
        if isWildcard then tree.expr.withType(underlyingTreeTpe.tpe)
        else typed(tree.expr, underlyingTreeTpe.tpe.widenSkolem)
      assignType(cpy.Typed(tree)(expr1, tpt), underlyingTreeTpe)
        .withNotNullInfo(expr1.notNullInfo)
    }

    if (untpd.isWildcardStarArg(tree)) {
      def typedWildcardStarArgExpr = {
        // A sequence argument `xs: _*` can be either a `Seq[T]` or an `Array[_ <: T]`,
        // irrespective of whether the method we're calling is a Java or Scala method,
        // so the expected type is the union `Seq[T] | Array[_ <: T]`.
        val ptArg =
          // FIXME(#8680): Quoted patterns do not support Array repeated arguments
          if ctx.mode.isQuotedPattern then
            pt.translateFromRepeated(toArray = false, translateWildcard = true)
          else
            pt.translateFromRepeated(toArray = false, translateWildcard = true)
            | pt.translateFromRepeated(toArray = true,  translateWildcard = true)
        val expr0 = typedExpr(tree.expr, ptArg)
        val expr1 = if ctx.explicitNulls && (!ctx.mode.is(Mode.Pattern)) then
            if expr0.tpe.isNullType then
              // If the type of the argument is `Null`, we cast it to array directly.
              expr0.cast(pt.translateParameterized(defn.RepeatedParamClass, defn.ArrayClass))
            else
              // We need to make sure its type is no longer nullable
              expr0.castToNonNullable
          else expr0
        val fromCls =
          if expr1.tpe.derivesFrom(defn.ArrayClass)
          then defn.ArrayClass
          else defn.SeqClass
        val tpt1 = TypeTree(expr1.tpe.widen.translateToRepeated(fromCls)).withSpan(tree.tpt.span)
        assignType(cpy.Typed(tree)(expr1, tpt1), tpt1)
      }
      cases(
        ifPat = ascription(TypeTree(defn.RepeatedParamType.appliedTo(pt)), isWildcard = true),
        ifExpr = typedWildcardStarArgExpr,
        wildName = nme.WILDCARD_STAR)
    }
    else {
      def typedTpt = checkSimpleKinded(typedType(tree.tpt, mapPatternBounds = true))
      def handlePattern: Tree = {
        val tpt1 = typedTpt
        if !ctx.isAfterTyper && pt != defn.ImplicitScrutineeTypeRef then
          withMode(Mode.GadtConstraintInference) {
            TypeComparer.constrainPatternType(tpt1.tpe, pt)
          }
        val matched = ascription(tpt1, isWildcard = true)
        // special case for an abstract type that comes with a class tag
        val result = tryWithTypeTest(matched, pt)
        if (result eq matched)
           && pt != defn.ImplicitScrutineeTypeRef
           && !(pt <:< tpt1.tpe)
        then
          // no check for matchability if TestTest was applied
          checkMatchable(pt, tree.srcPos, pattern = true)
        result
      }
      cases(
        ifPat = handlePattern,
        ifExpr = ascription(typedTpt, isWildcard = false),
        wildName = nme.WILDCARD)
    }
  }

  /** For a typed tree `e: T`, if `T` is an abstract type for which an implicit type test or class tag `tt`
   *  exists, rewrite to `tt(e)`.
   *  @pre We are in pattern-matching mode (Mode.Pattern)
   */
  def tryWithTypeTest(tree: Typed, pt: Type)(using Context): Tree =
    def withTag(tpe: Type): Option[Tree] = {
      require(ctx.mode.is(Mode.Pattern))
      withoutMode(Mode.Pattern)(
        inferImplicit(tpe, EmptyTree, tree.tpt.span)
      ) match
        case SearchSuccess(clsTag, _, _, _) =>
          withMode(Mode.InTypeTest) {
            Some(typed(untpd.Apply(untpd.TypedSplice(clsTag), untpd.TypedSplice(tree.expr)), pt))
          }
        case _ =>
          None
    }
    def tagged(tpe: Type) = {
      val tag = withTag(defn.TypeTestClass.typeRef.appliedTo(pt, tpe))
          .orElse(withTag(defn.ClassTagClass.typeRef.appliedTo(tpe)))
          .getOrElse(tree)
      if tag.symbol.maybeOwner == defn.ClassTagClass && config.Feature.sourceVersion.isAtLeast(config.SourceVersion.future) then
        report.warning("Use of `scala.reflect.ClassTag` for type testing may be unsound. Consider using `scala.reflect.TypeTest` instead.", tree.srcPos)
      tag
    }
    tree.tpt.tpe.dealias match {
      case tpe @ AppliedType(tref: TypeRef, _) if !tref.symbol.isClass && !ctx.isAfterTyper && !(tpe =:= pt) => tagged(tpe)
      case tref: TypeRef if !tref.symbol.isClass && !ctx.isAfterTyper && !(tref =:= pt) => tagged(tref)
      case _ => tree
    }

  def typedNamedArg(tree: untpd.NamedArg, pt: Type)(using Context): NamedArg = {
    /* Special case for resolving types for arguments of an annotation defined in Java.
     * It allows that value of any type T can appear in positions where Array[T] is expected.
     * For example, both `@Annot(5)` and `@Annot({5, 6}) are viable calls of the constructor
     * of annotation defined as `@interface Annot { int[] value() }`
     * We assume that calling `typedNamedArg` in context of Java implies that we are dealing
     * with annotation contructor, as named arguments are not allowed anywhere else in Java.
     * Under explicit nulls, the pt could be nullable. We need to strip `Null` type first.
     */
    val arg1 = pt.stripNull match {
      case AppliedType(a, typ :: Nil) if ctx.isJava && a.isRef(defn.ArrayClass) =>
        tryAlternatively { typed(tree.arg, pt) } {
            val elemTp = untpd.TypedSplice(TypeTree(typ))
            typed(untpd.JavaSeqLiteral(tree.arg :: Nil, elemTp), pt)
        }
      case _ => typed(tree.arg, pt)
    }

    assignType(cpy.NamedArg(tree)(tree.name, arg1), arg1)
  }

  def typedAssign(tree: untpd.Assign, pt: Type)(using Context): Tree =
    tree.lhs match {
      case lhs @ Apply(fn, args) =>
        typed(untpd.Apply(untpd.Select(fn, nme.update), args :+ tree.rhs), pt)
      case untpd.TypedSplice(Apply(MaybePoly(Select(fn, app), targs), args)) if app == nme.apply =>
        val rawUpdate: untpd.Tree = untpd.Select(untpd.TypedSplice(fn), nme.update)
        val wrappedUpdate =
          if (targs.isEmpty) rawUpdate
          else untpd.TypeApply(rawUpdate, targs map (untpd.TypedSplice(_)))
        val appliedUpdate =
          untpd.Apply(wrappedUpdate, (args map (untpd.TypedSplice(_))) :+ tree.rhs)
        typed(appliedUpdate, pt)
      case lhs =>
        val locked = ctx.typerState.ownedVars
        val lhsCore = typedUnadapted(lhs, LhsProto, locked)
        def lhs1 = adapt(lhsCore, LhsProto, locked)

        def reassignmentToVal =
          report.error(ReassignmentToVal(lhsCore.symbol.name), tree.srcPos)
          cpy.Assign(tree)(lhsCore, typed(tree.rhs, lhs1.tpe.widen)).withType(defn.UnitType)

        def canAssign(sym: Symbol) =
          sym.is(Mutable, butNot = Accessor) ||
          ctx.owner.isPrimaryConstructor && !sym.is(Method) && sym.maybeOwner == ctx.owner.owner ||
            // allow assignments from the primary constructor to class fields
          ctx.owner.name.is(TraitSetterName) || ctx.owner.isStaticConstructor

        /** Mark private variables that are assigned with a prefix other than
         *  the `this` type of their owner with a `annotation.internal.AssignedNonLocally`
         *  annotation. The annotation influences the variance check for these
         *  variables, which is done at PostTyper. It will be removed after the
         *  variance check.
         */
        def rememberNonLocalAssignToPrivate(sym: Symbol) = lhs1 match
          case Select(qual, _)
          if sym.is(Private, butNot = Local) && !sym.isAccessPrivilegedThisType(qual.tpe) =>
            sym.addAnnotation(Annotation(defn.AssignedNonLocallyAnnot, lhs1.span))
          case _ =>

        lhsCore match
          case Apply(fn, _) if fn.symbol.is(ExtensionMethod) =>
            def toSetter(fn: Tree): untpd.Tree = fn match
              case fn @ Ident(name: TermName) =>
                // We need to make sure that the prefix of this extension getter is
                // retained when we transform it into a setter. Otherwise, we could
                // end up resolving an unrelated setter from another extension. We
                // transform the `Ident` into a `Select` to ensure that the prefix
                // is retained with a `TypedSplice` (see `case Select` bellow).
                // See tests/pos/i18713.scala for an example.
                fn.tpe match
                  case TermRef(qual: TermRef, _) =>
                    toSetter(ref(qual).select(fn.symbol).withSpan(fn.span))
                  case TermRef(qual: ThisType, _) =>
                    toSetter(This(qual.cls).select(fn.symbol).withSpan(fn.span))
                  case TermRef(NoPrefix, _) =>
                    untpd.cpy.Ident(fn)(name.setterName)
              case fn @ Select(qual, name: TermName) =>
                untpd.cpy.Select(fn)(untpd.TypedSplice(qual), name.setterName)
              case fn @ TypeApply(fn1, targs) =>
                untpd.cpy.TypeApply(fn)(toSetter(fn1), targs.map(untpd.TypedSplice(_)))
              case fn @ Apply(fn1, args) =>
                val result = untpd.cpy.Apply(fn)(
                  toSetter(fn1),
                  args.map(untpd.TypedSplice(_, isExtensionReceiver = true)))
                fn1 match
                  case Apply(_, _) => // current apply is to implicit arguments
                    result.setApplyKind(ApplyKind.Using)
                      // Note that we cannot copy the apply kind of `fn` since `fn` is a typed
                      // tree and applyKinds are not preserved for those.
                  case _ => result
              case _ =>
                EmptyTree

            val setter = toSetter(lhsCore)
            if setter.isEmpty then reassignmentToVal
            else tryEither {
              val assign = untpd.Apply(setter, tree.rhs :: Nil)
              typed(assign, IgnoredProto(pt))
            } {
              (_, _) => reassignmentToVal
            }
          case _ => lhsCore.tpe match {
            case ref: TermRef =>
              val lhsVal = lhsCore.denot.suchThat(!_.is(Method))
              val lhsSym = lhsVal.symbol
              if canAssign(lhsSym) then
                rememberNonLocalAssignToPrivate(lhsSym)
                // lhsBounds: (T .. Any) as seen from lhs prefix, where T is the type of lhsSym
                // This ensures we do the as-seen-from on T with variance -1. Test case neg/i2928.scala
                val lhsBounds =
                  TypeBounds.lower(lhsSym.info).asSeenFrom(ref.prefix, lhsSym.owner)
                assignType(cpy.Assign(tree)(lhs1, typed(tree.rhs, lhsBounds.loBound)))
                  .computeAssignNullable()
              else
                val pre = ref.prefix
                val setterName = ref.name.setterName
                val setter = pre.member(setterName)
                lhsCore match {
                  case lhsCore: RefTree if setter.exists =>
                    val setterTypeRaw = pre.select(setterName, setter)
                    val setterType = ensureAccessible(setterTypeRaw, isSuperSelection(lhsCore), tree.srcPos)
                    val lhs2 = untpd.rename(lhsCore, setterName).withType(setterType)
                    typedUnadapted(untpd.Apply(untpd.TypedSplice(lhs2), tree.rhs :: Nil), WildcardType, locked)
                  case _ =>
                    reassignmentToVal
                }
            case TryDynamicCallType =>
              typedDynamicAssign(tree, pt)
            case tpe =>
              reassignmentToVal
        }
    }

  def typedBlockStats(stats: List[untpd.Tree])(using Context): (List[tpd.Tree], Context) =
    index(stats)
    typedStats(stats, ctx.owner)

  def typedBlock(tree: untpd.Block, pt: Type)(using Context): Tree = {
    val (stats1, exprCtx) = withoutMode(Mode.Pattern) {
      typedBlockStats(tree.stats)
    }

    var expr1 = typedExpr(tree.expr, pt.dropIfProto)(using exprCtx)

    // If unsafe nulls is enabled inside a block but not enabled outside
    // and the type does not conform the expected type without unsafe nulls,
    // we will cast the last expression to the expected type.
    // See: tests/explicit-nulls/pos/unsafe-block.scala
    if ctx.mode.is(Mode.SafeNulls)
      && !exprCtx.mode.is(Mode.SafeNulls)
      && pt.isValueType
      && !inContext(exprCtx.addMode(Mode.SafeNulls))(expr1.tpe <:< pt) then
      expr1 = expr1.cast(pt)

    ensureNoLocalRefs(
      cpy.Block(tree)(stats1, expr1)
        .withType(expr1.tpe)
        .withNotNullInfo(stats1.foldRight(expr1.notNullInfo)(_.notNullInfo.seq(_))),
      pt, localSyms(stats1))
  }

  def escapingRefs(block: Tree, localSyms: => List[Symbol])(using Context): List[NamedType] = {
    lazy val locals = localSyms.toSet
    block.tpe.namedPartsWith(tp => locals.contains(tp.symbol) && !tp.isErroneous)
  }

  /** Ensure that an expression's type can be expressed without references to locally defined
   *  symbols. This is done by adding a type ascription of a widened type that does
   *  not refer to the locally defined symbols. The widened type is computed using
   *  `TyperAssigner#avoid`. However, if the expected type is fully defined and not
   *  a supertype of the widened type, we ascribe with the expected type instead.
   *
   *  There's a special case having to do with anonymous classes. Sometimes the
   *  expected type of a block is the anonymous class defined inside it. In that
   *  case there's technically a leak which is not removed by the ascription.
   */
  protected def ensureNoLocalRefs(tree: Tree, pt: Type, localSyms: => List[Symbol])(using Context): Tree = {
    def ascribeType(tree: Tree, pt: Type): Tree = tree match {
      case block @ Block(stats, expr) if !expr.isInstanceOf[Closure] =>
        val expr1 = ascribeType(expr, pt)
        cpy.Block(block)(stats, expr1) withType expr1.tpe // no assignType here because avoid is redundant
      case _ =>
        val target = pt.simplified
        val targetTpt = InferredTypeTree().withType(target)
        if tree.tpe <:< target then Typed(tree, targetTpt)
        else
          // This case should not normally arise. It currently does arise in test cases
          // pos/t4080b.scala and pos/i7067.scala. In that case, a type ascription is wrong
          // and would not pass Ycheck. We have to use a cast instead. TODO: follow-up why
          // the cases arise and eliminate them, if possible.
          tree.cast(targetTpt)
    }
    def noLeaks(t: Tree): Boolean = escapingRefs(t, localSyms).isEmpty
    if (noLeaks(tree)) tree
    else {
      fullyDefinedType(tree.tpe, "block", tree.srcPos)
      var avoidingType = TypeOps.avoid(tree.tpe, localSyms)
      val ptDefined = isFullyDefined(pt, ForceDegree.none)
      if (ptDefined && !(avoidingType.widenExpr <:< pt)) avoidingType = pt
      val tree1 = ascribeType(tree, avoidingType)
      assert(ptDefined || noLeaks(tree1) || tree1.tpe.isErroneous,
          // `ptDefined` needed because of special case of anonymous classes
          i"leak: ${escapingRefs(tree1, localSyms).toList}%, % in $tree1")
      tree1
    }
  }

  def typedIf(tree: untpd.If, pt: Type)(using Context): Tree =
    if tree.isInline then checkInInlineContext("inline if", tree.srcPos)
    val cond1 = typed(tree.cond, defn.BooleanType)

    def isIncomplete(tree: untpd.If): Boolean = tree.elsep match
      case EmptyTree => true
      case elsep: untpd.If => isIncomplete(elsep)
      case _ => false

    // Insert a GADT cast if the type of the branch does not conform
    //   to the type assigned to the whole if tree.
    // This happens when the computation of the type of the if tree
    //   uses GADT constraints. See #15646.
    def gadtAdaptBranch(tree: Tree, branchPt: Type): Tree =
      TypeComparer.testSubType(tree.tpe.widenExpr, branchPt) match {
        case CompareResult.OKwithGADTUsed =>
          insertGadtCast(tree, tree.tpe.widen, branchPt)
        case _ => tree
      }

    val branchPt = if isIncomplete(tree) then defn.UnitType else pt.dropIfProto

    val result =
      if tree.elsep.isEmpty then
        val thenp1 = typed(tree.thenp, branchPt)(using cond1.nullableContextIf(true))
        val elsep1 = tpd.unitLiteral.withSpan(tree.span.endPos)
        cpy.If(tree)(cond1, thenp1, elsep1).withType(defn.UnitType)
      else
        val thenp1 :: elsep1 :: Nil = harmonic(harmonize, pt) {
          val thenp0 = typed(tree.thenp, branchPt)(using cond1.nullableContextIf(true))
          val elsep0 = typed(tree.elsep, branchPt)(using cond1.nullableContextIf(false))
          thenp0 :: elsep0 :: Nil
        }: @unchecked

        val resType = thenp1.tpe | elsep1.tpe
        val thenp2 :: elsep2 :: Nil =
          (thenp1 :: elsep1 :: Nil) map { t =>
            // Adapt each branch to ensure that their types conforms to the
            //   type assigned to the if tree by inserting GADT casts.
            gadtAdaptBranch(t, resType)
          }: @unchecked

        cpy.If(tree)(cond1, thenp2, elsep2).withType(resType)

    def thenPathInfo = cond1.notNullInfoIf(true).seq(result.thenp.notNullInfo)
    def elsePathInfo = cond1.notNullInfoIf(false).seq(result.elsep.notNullInfo)
    result.withNotNullInfo(
      if result.thenp.tpe.isRef(defn.NothingClass) then elsePathInfo
      else if result.elsep.tpe.isRef(defn.NothingClass) then thenPathInfo
      else thenPathInfo.alt(elsePathInfo)
    )
  end typedIf

  /** Decompose function prototype into a list of parameter prototypes and a result
   *  prototype tree, using WildcardTypes where a type is not known.
   *  Note: parameter prototypes may be TypeBounds.
   *  For the result type we do this even if the expected type is not fully
   *  defined, which is a bit of a hack. But it's needed to make the following work
   *  (see typers.scala and printers/PlainPrinter.scala for examples).
   *
   *     def double(x: Char): String = s"$x$x"
   *     "abc" flatMap double
   */
  private def decomposeProtoFunction(pt: Type, defaultArity: Int, pos: SrcPos)(using Context): (List[Type], untpd.Tree) = {
    def typeTree(tp: Type) = tp match {
      case _: WildcardType => new untpd.InferredTypeTree()
      case _ => untpd.InferredTypeTree(tp)
    }
    def interpolateWildcards = new TypeMap {
      def apply(t: Type): Type = t match
        case WildcardType(bounds: TypeBounds) =>
          newTypeVar(apply(bounds.orElse(TypeBounds.empty)).bounds)
        case _ => mapOver(t)
    }

    val pt1 = pt.strippedDealias.normalized
    if (pt1 ne pt1.dropDependentRefinement)
       && defn.isContextFunctionType(pt1.nonPrivateMember(nme.apply).info.finalResultType)
    then
      report.error(
        em"""Implementation restriction: Expected result type $pt1
            |is a curried dependent context function type. Such types are not yet supported.""",
        pos)
    pt1 match {
      case tp: TypeParamRef =>
        decomposeProtoFunction(ctx.typerState.constraint.entry(tp).bounds.hi, defaultArity, pos)
      case _ => pt1.findFunctionType match {
        case pt1 if defn.isNonRefinedFunction(pt1) =>
          // if expected parameter type(s) are wildcards, approximate from below.
          // if expected result type is a wildcard, approximate from above.
          // this can type the greatest set of admissible closures.

          (pt1.argInfos.init, typeTree(interpolateWildcards(pt1.argInfos.last.hiBound)))
        case RefinedType(parent, nme.apply, mt @ MethodTpe(_, formals, restpe))
        if defn.isNonRefinedFunction(parent) && formals.length == defaultArity =>
          (formals, untpd.InLambdaTypeTree(isResult = true, (_, syms) => restpe.substParams(mt, syms.map(_.termRef))))
        case defn.PolyFunctionOf(mt @ MethodTpe(_, formals, restpe)) if formals.length == defaultArity =>
          (formals, untpd.InLambdaTypeTree(isResult = true, (_, syms) => restpe.substParams(mt, syms.map(_.termRef))))
        case SAMType(mt @ MethodTpe(_, formals, _), samParent) =>
          val restpe = mt.resultType match
            case mt: MethodType => mt.toFunctionType(isJava = samParent.classSymbol.is(JavaDefined))
            case tp => tp
          (formals,
           if (mt.isResultDependent)
             untpd.InLambdaTypeTree(isResult = true, (_, syms) => restpe.substParams(mt, syms.map(_.termRef)))
           else
             typeTree(restpe))
        case _ =>
          (List.tabulate(defaultArity)(alwaysWildcardType), untpd.TypeTree())
      }
    }
  }

  /** The parameter type for a parameter in a lambda that does
   *  not have an explicit type given, and where the type is not known from the context.
   *  In this case the parameter type needs to be inferred the "target type" T known
   *  from the callee `f` if the lambda is of a form like `x => f(x)`.
   *  If `T` exists, we know that `S <: I <: T`.
   *
   *  The inference makes two attempts:
   *
   *    1. Compute the target type `T` and make it known that `S <: T`.
   *       If the expected type `S` can be fully defined under ForceDegree.flipBottom,
   *       pick this one (this might use the fact that S <: T for an upper approximation).
   *    2. Otherwise, if the target type `T` can be fully defined under ForceDegree.flipBottom,
   *       pick this one.
   *
   *  If both attempts fail, return `NoType`.
   */
  def inferredFromTarget(
      param: untpd.ValDef, formal: Type, calleeType: Type, isErased: Boolean, paramIndex: Name => Int)(using Context): Type =
    val target = calleeType.widen match
      case mtpe: MethodType =>
        val pos = paramIndex(param.name)
        if pos < mtpe.paramInfos.length then
          val tp = mtpe.paramInfos(pos)
            // This works only if vararg annotations match up.
            // See neg/i14367.scala for an example where the inferred type is mispredicted.
            // Nevertheless, the alternative would be to give up completely, so this is
            // defensible.
          // Strip inferred erased annotation, to avoid accidentally inferring erasedness
          if !isErased then tp.stripAnnots(_.symbol != defn.ErasedParamAnnot) else tp
        else NoType
      case _ => NoType
    if target.exists then formal <:< target
    if !formal.isExactlyNothing && isFullyDefined(formal, ForceDegree.flipBottom) then formal
    else if target.exists && isFullyDefined(target, ForceDegree.flipBottom) then target
    else NoType

  def typedFunction(tree: untpd.Function, pt: Type)(using Context): Tree =
    if (ctx.mode is Mode.Type) typedFunctionType(tree, pt)
    else typedFunctionValue(tree, pt)

  def typedFunctionType(tree: untpd.Function, pt: Type)(using Context): Tree = {
    val untpd.Function(args, body) = tree
    body match
      case untpd.CapturesAndResult(refs, result) =>
        return typedUnadapted(untpd.makeRetaining(
          cpy.Function(tree)(args, result), refs, tpnme.retains), pt)
      case _ =>
    var (funFlags, erasedParams) = tree match {
      case tree: untpd.FunctionWithMods => (tree.mods.flags, tree.erasedParams)
      case _ => (EmptyFlags, args.map(_ => false))
    }

    val numArgs = args.length
    val isContextual = funFlags.is(Given)
    val isImpure = funFlags.is(Impure)

    /** Typechecks dependent function type with given parameters `params` */
    def typedDependent(params: List[untpd.ValDef])(using Context): Tree =
      val fixThis = new untpd.UntypedTreeMap:
        // pretype all references of this in outer context,
        // so that they do not refer to the refined type being constructed
        override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
          case This(id) => untpd.TypedSplice(typedExpr(tree)(using ctx.outer))
          case _ => super.transform(tree)

      val params1 =
        if funFlags.is(Given) then params.map(_.withAddedFlags(Given))
        else params
      val params2 = params1.map(fixThis.transformSub)
      val params3 = params2.zipWithConserve(erasedParams) { (arg, isErased) =>
        if isErased then arg.withAddedFlags(Erased) else arg
      }
      val appDef0 = untpd.DefDef(nme.apply, List(params3), body, EmptyTree).withSpan(tree.span)
      index(appDef0 :: Nil)
      val appDef = typed(appDef0).asInstanceOf[DefDef]
      val mt = appDef.symbol.info.asInstanceOf[MethodType]
      if (mt.isParamDependent)
        report.error(em"$mt is an illegal function type because it has inter-parameter dependencies", tree.srcPos)
      // Restart typechecking if there are erased classes that we want to mark erased
      if mt.erasedParams.zip(mt.paramInfos.map(_.isErasedClass)).exists((paramErased, classErased) => classErased && !paramErased) then
        val newParams = params3.zipWithConserve(mt.paramInfos.map(_.isErasedClass)) { (arg, isErasedClass) =>
          if isErasedClass then arg.withAddedFlags(Erased) else arg
        }
        return typedDependent(newParams)
      val core =
        if mt.hasErasedParams then TypeTree(defn.PolyFunctionClass.typeRef)
        else
          val resTpt = TypeTree(mt.nonDependentResultApprox).withSpan(body.span)
          val paramTpts = appDef.termParamss.head.map(p => TypeTree(p.tpt.tpe).withSpan(p.tpt.span))
          val funSym = defn.FunctionSymbol(numArgs, isContextual, isImpure)
          val tycon = TypeTree(funSym.typeRef)
          AppliedTypeTree(tycon, paramTpts :+ resTpt)
      RefinedTypeTree(core, List(appDef), ctx.owner.asClass)
    end typedDependent

    args match {
      case ValDef(_, _, _) :: _ =>
        typedDependent(args.asInstanceOf[List[untpd.ValDef]])(
          using ctx.fresh.setOwner(newRefinedClassSymbol(tree.span)).setNewScope)
      case _ =>
        if erasedParams.contains(true) then
          typedFunctionType(desugar.makeFunctionWithValDefs(tree, pt), pt)
        else
          val funSym = defn.FunctionSymbol(numArgs, isContextual, isImpure)
          val result = typed(cpy.AppliedTypeTree(tree)(untpd.TypeTree(funSym.typeRef), args :+ body), pt)
          // if there are any erased classes, we need to re-do the typecheck.
          result match
            case r: AppliedTypeTree if r.args.exists(_.tpe.isErasedClass) =>
              typedFunctionType(desugar.makeFunctionWithValDefs(tree, pt), pt)
            case _ => result
    }
  }

  def typedFunctionValue(tree: untpd.Function, pt: Type)(using Context): Tree = {
    val untpd.Function(params: List[untpd.ValDef] @unchecked, _) = tree: @unchecked

    val (isContextual, isDefinedErased) = tree match {
      case tree: untpd.FunctionWithMods => (tree.mods.is(Given), tree.erasedParams)
      case _ => (false, tree.args.map(_ => false))
    }

    /** The function body to be returned in the closure. Can become a TypedSplice
     *  of a typed expression if this is necessary to infer a parameter type.
     */
    var fnBody = tree.body

    def refersTo(arg: untpd.Tree, param: untpd.ValDef): Boolean = arg match
      case Ident(name) => name == param.name
      case Typed(arg1, _) if untpd.isWildcardStarArg(arg) => refersTo(arg1, param)
      case _ => false

    /** If parameter `param` appears exactly once as an argument in `args`,
     *  the singleton list consisting of its position in `args`, otherwise `Nil`.
     */
    def paramIndices(param: untpd.ValDef, args: List[untpd.Tree]): List[Int] = {
      def loop(args: List[untpd.Tree], start: Int): List[Int] = args match {
        case arg :: args1 =>
          val others = loop(args1, start + 1)
          if (refersTo(arg, param)) start :: others else others
        case _ => Nil
      }
      val allIndices = loop(args, 0)
      if (allIndices.length == 1) allIndices else Nil
    }

    /** A map from parameter names to unique positions where the parameter
     *  appears in the argument list of an application.
     */
    var paramIndex = Map[Name, Int]()

    /** Infer parameter type from the body of the function
     *
     *  1. If function is of the form
     *
     *      (x1, ..., xN) => f(... x1, ..., XN, ...)
     *
     *  where each `xi` occurs exactly once in the argument list of `f` (in
     *  any order), the type of `f`, otherwise NoType.
     *
     *  2. If the function is of the form
     *
     *     (using x1, ..., xN) => f
     *
     *  where `f` is a contextual function type of the form `(T1, ..., TN) ?=> T`,
     *  then `xi` takes the type `Ti`.
     *
     *  Updates `fnBody` and `paramIndex` as a side effect.
     *  @post: If result exists, `paramIndex` is defined for the name of
     *         every parameter in `params`.
     */
    lazy val calleeType: Type = untpd.stripAnnotated(untpd.unsplice(fnBody)) match {
      case ident: untpd.Ident if isContextual =>
        val ident1 = typedIdent(ident, WildcardType)
        val tp = ident1.tpe.widen
        if defn.isContextFunctionType(tp) && params.size == defn.functionArity(tp) then
          paramIndex = params.map(_.name).zipWithIndex.toMap
          fnBody = untpd.TypedSplice(ident1)
          tp.select(nme.apply)
        else NoType
      case app @ Apply(expr, args) =>
        paramIndex = {
          for (param <- params; idx <- paramIndices(param, args))
          yield param.name -> idx
        }.toMap
        if (paramIndex.size == params.length)
          expr match
            case untpd.TypedSplice(expr1) =>
              expr1.tpe
            case _ =>
              val outerCtx = ctx
              val nestedCtx = outerCtx.fresh.setNewTyperState()
              inContext(nestedCtx) {
                val protoArgs = args map (_ withType WildcardType)
                val callProto = FunProto(protoArgs, WildcardType)(this, app.applyKind)
                val expr1 = typedExpr(expr, callProto)
                if nestedCtx.reporter.hasErrors then NoType
                else inContext(outerCtx) {
                  nestedCtx.typerState.commit()
                  fnBody = cpy.Apply(fnBody)(untpd.TypedSplice(expr1), args)
                  expr1.tpe
                }
              }
        else NoType
      case _ =>
        NoType
    }

    pt match {
      case pt: TypeVar
      if untpd.isFunctionWithUnknownParamType(tree) && !calleeType.exists =>
        // try to instantiate `pt` if this is possible. If it does not
        // work the error will be reported later in `inferredParam`,
        // when we try to infer the parameter type.
        isFullyDefined(pt, ForceDegree.flipBottom)
      case _ =>
    }

    val (protoFormals, resultTpt) = decomposeProtoFunction(pt, params.length, tree.srcPos)

    /** Returns the type and whether the parameter is erased */
    def protoFormal(i: Int): (Type, Boolean) =
      if (protoFormals.length == params.length) (protoFormals(i), isDefinedErased(i))
      else (errorType(WrongNumberOfParameters(tree, params.length, pt, protoFormals.length), tree.srcPos), false)

    /** Is `formal` a product type which is elementwise compatible with `params`? */
    def ptIsCorrectProduct(formal: Type) =
      isFullyDefined(formal, ForceDegree.flipBottom) &&
      defn.isProductSubType(formal) &&
      tupleComponentTypes(formal).corresponds(params) {
        (argType, param) =>
          param.tpt.isEmpty || argType.widenExpr <:< typedAheadType(param.tpt).tpe
      }

    var desugared: untpd.Tree = EmptyTree
    if protoFormals.length == 1 && params.length != 1 then
      val firstFormal = protoFormals.head.loBound
      if ptIsCorrectProduct(firstFormal) then
        val isGenericTuple =
          firstFormal.derivesFrom(defn.TupleClass)
          && !defn.isTupleClass(firstFormal.typeSymbol)
        desugared = desugar.makeTupledFunction(params, fnBody, isGenericTuple)
    else if protoFormals.length > 1 && params.length == 1 then
      def isParamRef(scrut: untpd.Tree): Boolean = scrut match
        case untpd.Annotated(scrut1, _) => isParamRef(scrut1)
        case untpd.Ident(id) => id == params.head.name
      fnBody match
        case untpd.Match(scrut, cases @ untpd.CaseDef(untpd.Tuple(elems), untpd.EmptyTree, rhs) :: Nil)
        if scrut.span.isSynthetic && isParamRef(scrut) && elems.hasSameLengthAs(protoFormals) =>
          // If `pt` is N-ary function type, convert synthetic lambda
          //   x$1 => x$1 match case (a1, ..., aN) => e
          // to
          //   (a1, ..., aN) => e
          val params1 = desugar.patternsToParams(elems)
          desugared = if params1.hasSameLengthAs(elems)
            then cpy.Function(tree)(params1, rhs)
            else desugar.makeCaseLambda(cases, desugar.MatchCheck.IrrefutablePatDef, protoFormals.length)
        case _ =>

    if desugared.isEmpty then
      val forceDegree =
        if pt.isValueType then
          // Allow variables that appear invariantly in `pt` to be improved by mapping
          // bottom types in their instance types to fresh type variables
          new ForceDegree.Value(IfBottom.fail):
            val tvmap = variances(pt)
            override def canImprove(tvar: TypeVar) =
              tvmap.computedVariance(tvar) == (0: Integer)
        else
          ForceDegree.failBottom

      val inferredParams: List[untpd.ValDef] =
        for ((param, i) <- params.zipWithIndex) yield
          if (!param.tpt.isEmpty) param
          else
            val (formalBounds, isErased) = protoFormal(i)
            val formal = formalBounds.loBound
            val isBottomFromWildcard = (formalBounds ne formal) && formal.isExactlyNothing
            val knownFormal = isFullyDefined(formal, forceDegree)
            // If the expected formal is a TypeBounds wildcard argument with Nothing as lower bound,
            // try to prioritize inferring from target. See issue 16405 (tests/run/16405.scala)
            val paramType =
              // Strip inferred erased annotation, to avoid accidentally inferring erasedness
              val formal0 = if !isErased then formal.stripAnnots(_.symbol != defn.ErasedParamAnnot) else formal
              if knownFormal && !isBottomFromWildcard then
                formal0
              else
                inferredFromTarget(param, formal, calleeType, isErased, paramIndex).orElse(
                  if knownFormal then formal0
                  else errorType(AnonymousFunctionMissingParamType(param, tree, inferredType = formal, expectedType = pt), param.srcPos)
                )
            val paramTpt = untpd.TypedSplice(
                (if knownFormal then InferredTypeTree() else untpd.TypeTree())
                  .withType(paramType.translateFromRepeated(toArray = false))
                  .withSpan(param.span.endPos)
              )
            val param0 = cpy.ValDef(param)(tpt = paramTpt)
            if isErased then param0.withAddedFlags(Flags.Erased) else param0
      desugared = desugar.makeClosure(Nil, inferredParams, fnBody, resultTpt, tree.span)

    typed(desugared, pt)
      .showing(i"desugared fun $tree --> $desugared with pt = $pt", typr)
  }


  def typedPolyFunction(tree: untpd.PolyFunction, pt: Type)(using Context): Tree =
    val tree1 = desugar.normalizePolyFunction(tree)
    if (ctx.mode is Mode.Type) typed(desugar.makePolyFunctionType(tree1), pt)
    else typedPolyFunctionValue(tree1, pt)

  def typedPolyFunctionValue(tree: untpd.PolyFunction, pt: Type)(using Context): Tree =
    val untpd.PolyFunction(tparams: List[untpd.TypeDef] @unchecked, fun) = tree: @unchecked
    val untpd.Function(vparams: List[untpd.ValDef] @unchecked, body) = fun: @unchecked
    val dpt = pt.dealias

    // If the expected type is a polymorphic function with the same number of
    // type and value parameters, then infer the types of value parameters from the expected type.
    val inferredVParams = dpt match
      case defn.PolyFunctionOf(poly @ PolyType(_, mt: MethodType))
      if tparams.lengthCompare(poly.paramNames) == 0 && vparams.lengthCompare(mt.paramNames) == 0 =>
        vparams.zipWithConserve(mt.paramInfos): (vparam, formal) =>
          // Unlike in typedFunctionValue, `formal` cannot be a TypeBounds since
          // it must be a valid method parameter type.
          if vparam.tpt.isEmpty && isFullyDefined(formal, ForceDegree.failBottom) then
            cpy.ValDef(vparam)(tpt = new untpd.InLambdaTypeTree(isResult = false, (tsyms, vsyms) =>
              // We don't need to substitute `mt` by `vsyms` because we currently disallow
              // dependencies between value parameters of a closure.
              formal.substParams(poly, tsyms.map(_.typeRef)))
            )
          else vparam
      case _ =>
        vparams

    val resultTpt = dpt match
      case defn.PolyFunctionOf(poly @ PolyType(_, mt: MethodType)) =>
        untpd.InLambdaTypeTree(isResult = true, (tsyms, vsyms) =>
          mt.resultType.substParams(mt, vsyms.map(_.termRef)).substParams(poly, tsyms.map(_.typeRef)))
      case _ => untpd.TypeTree()

    val desugared = desugar.makeClosure(tparams, inferredVParams, body, resultTpt, tree.span)
    typed(desugared, pt)
  end typedPolyFunctionValue

  def typedClosure(tree: untpd.Closure, pt: Type)(using Context): Tree = {
    val env1 = tree.env mapconserve (typed(_))
    val meth1 = typedUnadapted(tree.meth)
    val target =
      if (tree.tpt.isEmpty)
        meth1.tpe.widen match {
          case mt: MethodType =>
            pt.findFunctionType match {
              case SAMType(samMeth, samParent)
              if !defn.isFunctionNType(samParent) && mt <:< samMeth =>
                if defn.isContextFunctionType(mt.resultType) then
                  report.error(
                    em"""Implementation restriction: cannot convert this expression to `$samParent`
                        |because its result type `${mt.resultType}` is a contextual function type.""",
                    tree.srcPos)
                val targetTpe =
                  if isFullyDefined(samParent, ForceDegree.all) then
                    samParent
                  else if samParent.isRef(defn.PartialFunctionClass) then
                    // Replace the underspecified expected type by one based on the closure method type
                    defn.PartialFunctionOf(mt.firstParamTypes.head, mt.resultType)
                  else
                    errorType(em"result type of lambda is an underspecified SAM type $samParent", tree.srcPos)
                TypeTree(targetTpe)
              case _ =>
                if (mt.isParamDependent)
                  errorTree(tree, ClosureCannotHaveInternalParameterDependencies(mt))
                else if hasCaptureConversionArg(mt.resType) then
                  errorTree(tree,
                    em"""cannot turn method type $mt into closure
                        |because it has capture conversion skolem types""")
                else
                  EmptyTree
            }
          case poly @ PolyType(_, mt: MethodType) =>
            if (mt.isParamDependent)
              errorTree(tree, ClosureCannotHaveInternalParameterDependencies(poly))
            else
              // Polymorphic SAMs are not currently supported (#6904).
              EmptyTree
          case tp =>
            if !tp.isErroneous then
              throw new java.lang.Error(i"internal error: closing over non-method $tp, pos = ${tree.span}")
            TypeTree(defn.AnyType)
        }
      else typed(tree.tpt)
    //println(i"typing closure $tree : ${meth1.tpe.widen}")
    assignType(cpy.Closure(tree)(env1, meth1, target), meth1, target)
  }

  def typedMatch(tree: untpd.Match, pt: Type)(using Context): Tree =
    tree.selector match {
      case EmptyTree =>
        if (tree.isInline) {
          checkInInlineContext("summonFrom", tree.srcPos)
          val cases1 = tree.cases.mapconserve {
            case cdef @ CaseDef(pat @ Typed(Ident(nme.WILDCARD), _), _, _) =>
              // case _ : T  -->  case _$n : T
              cpy.CaseDef(cdef)(pat = untpd.Bind(WildcardParamName.fresh(), pat))
            case cdef => cdef
          }
          typedMatchFinish(tree, tpd.EmptyTree, defn.ImplicitScrutineeTypeRef, cases1, pt)
        }
        else {
          val (protoFormals, _) = decomposeProtoFunction(pt, 1, tree.srcPos)
          val checkMode = desugar.MatchCheck.Exhaustive
          typed(desugar.makeCaseLambda(tree.cases, checkMode, protoFormals.length).withSpan(tree.span), pt)
        }
      case _ =>
        if tree.isInline then checkInInlineContext("inline match", tree.srcPos)
        val sel1 = typedExpr(tree.selector)
        val rawSelectorTpe = fullyDefinedType(sel1.tpe, "pattern selector", tree.srcPos)
        val selType = rawSelectorTpe match
          case c: ConstantType if tree.isInline => c
          case otherTpe => otherTpe.widen

        /** Does `tree` has the same shape as the given match type?
         *  We only support typed patterns with empty guards, but
         *  that could potentially be extended in the future.
         */
        def isMatchTypeShaped(mt: MatchType): Boolean =
          mt.cases.size == tree.cases.size
          && sel1.tpe.frozen_<:<(mt.scrutinee)
          && tree.cases.forall(_.guard.isEmpty)
          && tree.cases
            .map(cas => untpd.unbind(untpd.unsplice(cas.pat)))
            .zip(mt.cases)
            .forall {
              case (pat: untpd.Typed, pt) =>
                // To check that pattern types correspond we need to type
                // check `pat` here and throw away the result.
                val gadtCtx: Context = ctx.fresh.setFreshGADTBounds
                val pat1 = typedPattern(pat, selType)(using gadtCtx)
                val tpt = tpd.unbind(tpd.unsplice(pat1)) match
                  case Typed(_, tpt) => tpt
                  case UnApply(fun, _, p1 :: _) if fun.symbol == defn.TypeTest_unapply => p1
                instantiateMatchTypeProto(pat1, pt) match {
                  case defn.MatchCase(patternTp, _) => tpt.tpe frozen_=:= patternTp
                  case _ => false
                }
              case (id @ Ident(nme.WILDCARD), pt) =>
                pt match {
                  case defn.MatchCase(patternTp, _) => defn.AnyType frozen_=:= patternTp
                  case _ => false
                }
              case _ => false
            }

        val result = pt match {
          case mt: MatchType if isMatchTypeShaped(mt) =>
            typedDependentMatchFinish(tree, sel1, selType, tree.cases, mt)
          case MatchType.InDisguise(mt) if isMatchTypeShaped(mt) =>
            typedDependentMatchFinish(tree, sel1, selType, tree.cases, mt)
          case _ =>
            typedMatchFinish(tree, sel1, selType, tree.cases, pt)
        }

        /** Are some form of brackets necessary to annotate the tree `sel` as `@unchecked`?
         *  If so, return a Some(opening bracket, closing bracket), otherwise None.
         */
        def uncheckedBrackets(sel: untpd.Tree): Option[(String, String)] = sel match
          case _: untpd.If
             | _: untpd.Match
             | _: untpd.ForYield
             | _: untpd.ParsedTry
             | _: untpd.Try
             | _: untpd.Typed => Some("(", ")")
          case _: untpd.Block => Some("{", "}")
          case _ => None

        result match {
          case result @ Match(sel, CaseDef(pat, _, _) :: _) =>
            tree.selector.removeAttachment(desugar.CheckIrrefutable) match {
              case Some(checkMode) if !sel.tpe.hasAnnotation(defn.UncheckedAnnot) =>
                val isPatDef = checkMode == desugar.MatchCheck.IrrefutablePatDef
                if !checkIrrefutable(sel, pat, isPatDef)
                  && sourceVersion.isAtLeast(`3.2`)
                  && sourceVersion.isMigrating
                then
                  if isPatDef then uncheckedBrackets(tree.selector) match
                    case None =>
                      patch(Span(tree.selector.span.end), ": @unchecked")
                    case Some(bl, br) =>
                      patch(Span(tree.selector.span.start), s"$bl")
                      patch(Span(tree.selector.span.end), s"$br: @unchecked")
                  else
                    patch(Span(tree.span.start), "case ")

                // skip exhaustivity check in later phase
                // TODO: move the check above to patternMatcher phase
                val uncheckedTpe = AnnotatedType(sel.tpe.widen, Annotation(defn.UncheckedAnnot, tree.selector.span))
                tpd.cpy.Match(result)(
                  selector = tpd.Typed(sel, new tpd.InferredTypeTree().withType(uncheckedTpe)),
                  cases = result.cases
                )
              case _ =>
                result
            }
          case _ =>
            result
        }
    }

  /** Special typing of Match tree when the expected type is a MatchType,
   *  and the patterns of the Match tree and the MatchType correspond.
   */
  def typedDependentMatchFinish(tree: untpd.Match, sel: Tree, wideSelType0: Type, cases: List[untpd.CaseDef], pt: MatchType)(using Context): Tree = {
    var caseCtx = ctx
    var wideSelType = wideSelType0
    var alreadyStripped = false
    val cases1 = tree.cases.zip(pt.cases)
      .map { case (cas, tpe) =>
        val case1 = typedCase(cas, sel, wideSelType, tpe)(using caseCtx)
        caseCtx = Nullables.afterPatternContext(sel, case1.pat)
        if !alreadyStripped && Nullables.matchesNull(case1) then
          wideSelType = wideSelType.stripNull
          alreadyStripped = true
        case1
      }
      .asInstanceOf[List[CaseDef]]
    assignType(cpy.Match(tree)(sel, cases1), sel, cases1).cast(pt)
  }

  // Overridden in InlineTyper for inline matches
  def typedMatchFinish(tree: untpd.Match, sel: Tree, wideSelType: Type, cases: List[untpd.CaseDef], pt: Type)(using Context): Tree = {
    val cases1 = harmonic(harmonize, pt)(typedCases(cases, sel, wideSelType, pt.dropIfProto))
      .asInstanceOf[List[CaseDef]]
    assignType(cpy.Match(tree)(sel, cases1), sel, cases1)
  }

  def typedCases(cases: List[untpd.CaseDef], sel: Tree, wideSelType0: Type, pt: Type)(using Context): List[CaseDef] =
    var caseCtx = ctx
    var wideSelType = wideSelType0
    var alreadyStripped = false
    cases.mapconserve { cas =>
      val case1 = typedCase(cas, sel, wideSelType, pt)(using caseCtx)
      caseCtx = Nullables.afterPatternContext(sel, case1.pat)
      if !alreadyStripped && Nullables.matchesNull(case1) then
        wideSelType = wideSelType.stripNull
        alreadyStripped = true
      case1
    }

  /** - strip all instantiated TypeVars from pattern types.
    *    run/reducable.scala is a test case that shows stripping typevars is necessary.
    *  - enter all symbols introduced by a Bind in current scope
    */
  private def indexPattern(cdef: untpd.CaseDef)(using Context) = new TreeMap {
    val stripTypeVars = new TypeMap {
      def apply(t: Type) = mapOver(t)
    }
    override def transform(trt: Tree)(using Context) =
      super.transform(trt.withType(stripTypeVars(trt.tpe))) match {
        case b: Bind =>
          val sym = b.symbol
          assert(sym.name != tpnme.WILDCARD)
          if ctx.scope.lookup(b.name) == NoSymbol then ctx.enter(sym)
          else report.error(new DuplicateBind(b, cdef), b.srcPos)
          if (!ctx.isAfterTyper) {
            val bounds = ctx.gadt.fullBounds(sym)
            if (bounds != null) sym.info = bounds
          }
          b
        case t => t
      }
  }

  /** If the prototype `pt` is the type lambda (when doing a dependent
   *  typing of a match), instantiate that type lambda with the pattern
   *  variables found in the pattern `pat`.
   */
  def instantiateMatchTypeProto(pat: Tree, pt: Type)(using Context) = pt match {
    case caseTp: HKTypeLambda =>
      val bindingsSyms = tpd.patVars(pat).reverse
      val bindingsTps = bindingsSyms.collect { case sym if sym.isType => sym.typeRef }
      caseTp.appliedTo(bindingsTps)
    case pt => pt
  }

  /** Type a case. */
  def typedCase(tree: untpd.CaseDef, sel: Tree, wideSelType: Type, pt: Type)(using Context): CaseDef = {
    val originalCtx = ctx
    val gadtCtx: Context = ctx.fresh.setFreshGADTBounds.setNewScope

    def caseRest(pat: Tree)(using Context) = {
      val pt1 = instantiateMatchTypeProto(pat, pt) match {
        case defn.MatchCase(_, bodyPt) => bodyPt
        case pt => pt
      }
      val pat1 = indexPattern(tree).transform(pat)
      val guard1 = typedExpr(tree.guard, defn.BooleanType)
      var body1 = ensureNoLocalRefs(typedExpr(tree.body, pt1), pt1, ctx.scope.toList)
      if ctx.gadt.isNarrowing then
        // Store GADT constraint to later retrieve it (in PostTyper, for now).
        // GADT constraints are necessary to correctly check bounds of type app,
        // see tests/pos/i12226 and issue #12226. It might be possible that this
        // will end up taking too much memory. If it does, we should just limit
        // how much GADT constraints we infer - it's always sound to infer less.
        pat1.putAttachment(InferredGadtConstraints, ctx.gadt)
      if (pt1.isValueType) // insert a cast if body does not conform to expected type if we disregard gadt bounds
        body1 = body1.ensureConforms(pt1)(using originalCtx)
      assignType(cpy.CaseDef(tree)(pat1, guard1, body1), pat1, body1)
    }

    val pat1 = typedPattern(tree.pat, wideSelType)(using gadtCtx)
    caseRest(pat1)(
      using Nullables.caseContext(sel, pat1)(
        using gadtCtx))
  }

  def typedLabeled(tree: untpd.Labeled)(using Context): Labeled = {
    val bind1 = typedBind(tree.bind, WildcardType).asInstanceOf[Bind]
    val expr1 = typed(tree.expr, bind1.symbol.info)
    assignType(cpy.Labeled(tree)(bind1, expr1))
  }

  /** Type a case of a type match */
  def typedTypeCase(cdef: untpd.CaseDef, selType: Type, pt: Type)(using Context): CaseDef = {
    def caseRest(using Context) = {
      val pat1 = withMode(Mode.Pattern) {
        checkSimpleKinded(typedType(cdef.pat, mapPatternBounds = true))
      }
      if !ctx.isAfterTyper && pt != defn.ImplicitScrutineeTypeRef then
        withMode(Mode.GadtConstraintInference) {
          TypeComparer.constrainPatternType(pat1.tpe, selType)
        }
      val pat2 = indexPattern(cdef).transform(pat1)
      var body1 = typedType(cdef.body, pt)
      if !body1.isType then
        assert(ctx.reporter.errorsReported)
        body1 = TypeTree(errorType(em"<error: not a type>", cdef.srcPos))
      assignType(cpy.CaseDef(cdef)(pat2, EmptyTree, body1), pat2, body1)
    }
    caseRest(using ctx.fresh.setFreshGADTBounds.setNewScope)
  }

  def typedReturn(tree: untpd.Return)(using Context): Return =

    def enclMethInfo(cx: Context): (Tree, Type) =
      val owner = cx.owner
      if owner.isType then
        report.error(ReturnOutsideMethodDefinition(owner), tree.srcPos)
        (EmptyTree, WildcardType)
      else if owner != cx.outer.owner && owner.isRealMethod then
        if owner.isInlineMethod then
          (EmptyTree, errorType(NoReturnFromInlineable(owner), tree.srcPos))
        else if !owner.isCompleted then
          (EmptyTree, errorType(MissingReturnTypeWithReturnStatement(owner), tree.srcPos))
        else
          (Ident(TermRef(NoPrefix, owner.asTerm)), owner.returnProto)
      else enclMethInfo(cx.outer)

    val (from, proto) =
      if tree.from.isEmpty then enclMethInfo(ctx)
      else
        val from = tree.from.asInstanceOf[tpd.Tree]
        val proto =
          if (ctx.erasedTypes) from.symbol.info.finalResultType
          else WildcardType // We cannot reliably detect the internal type view of polymorphic or dependent methods
                            // because we do not know the internal type params and method params.
                            // Hence no adaptation is possible, and we assume WildcardType as prototype.
        (from, proto)
    val expr1 = typedExpr(tree.expr orElse untpd.unitLiteral.withSpan(tree.span), proto)
    assignType(cpy.Return(tree)(expr1, from))
  end typedReturn

  def typedWhileDo(tree: untpd.WhileDo)(using Context): Tree =
    inContext(Nullables.whileContext(tree.span)) {
      val cond1 =
        if (tree.cond eq EmptyTree) EmptyTree
        else typed(tree.cond, defn.BooleanType)
      val body1 = typed(tree.body, defn.UnitType)(using cond1.nullableContextIf(true))
      assignType(cpy.WhileDo(tree)(cond1, body1))
        .withNotNullInfo(body1.notNullInfo.retractedInfo.seq(cond1.notNullInfoIf(false)))
    }

  /** Add givens reflecting `CanThrow` capabilities for all checked exceptions matched
   *  by `cases`. The givens appear in nested blocks with earlier cases leading to
   *  more deeply nested givens. This way, given priority will be the same as pattern priority.
   *  The functionality is enabled if the experimental.saferExceptions language feature is enabled.
   */
  def addCanThrowCapabilities(expr: untpd.Tree, cases: List[CaseDef])(using Context): untpd.Tree =
    def makeCanThrow(tp: Type): untpd.Tree =
      untpd.ValDef(
          CanThrowEvidenceName.fresh(),
          untpd.TypeTree(defn.CanThrowClass.typeRef.appliedTo(tp)),
          untpd.ref(defn.Compiletime_erasedValue))
        .withFlags(Given | Final | Erased)
        .withSpan(expr.span)
    val caughtExceptions =
      if Feature.enabled(Feature.saferExceptions) then
        for
          CaseDef(pat, guard, _) <- cases
          if pat.tpe.widen.isCheckedException
        yield
          checkCatch(pat, guard)
          pat.tpe.widen
      else Seq.empty

    if caughtExceptions.isEmpty then expr
    else
      val capabilityProof = caughtExceptions.reduce(OrType(_, _, true))
      untpd.Block(makeCanThrow(capabilityProof), expr)

  def typedTry(tree: untpd.Try, pt: Type)(using Context): Try = {
    val expr2 :: cases2x = harmonic(harmonize, pt) {
      val cases1 = typedCases(tree.cases, EmptyTree, defn.ThrowableType, pt.dropIfProto)
      val expr1 = typed(addCanThrowCapabilities(tree.expr, cases1), pt.dropIfProto)
      expr1 :: cases1
    }: @unchecked
    val finalizer1 = typed(tree.finalizer, defn.UnitType)
    val cases2 = cases2x.asInstanceOf[List[CaseDef]]
    assignType(cpy.Try(tree)(expr2, cases2, finalizer1), expr2, cases2)
  }

  def typedTry(tree: untpd.ParsedTry, pt: Type)(using Context): Try =
    val cases: List[untpd.CaseDef] = tree.handler match
      case Match(EmptyTree, cases) => cases
      case EmptyTree => Nil
      case handler =>
        val handler1 = typed(handler, defn.FunctionType(1).appliedTo(defn.ThrowableType, pt))
        desugar.makeTryCase(handler1) :: Nil
    typedTry(untpd.Try(tree.expr, cases, tree.finalizer).withSpan(tree.span), pt)

  def typedThrow(tree: untpd.Throw)(using Context): Tree =
    val expr1 = typed(tree.expr, defn.ThrowableType)
    val cap = checkCanThrow(expr1.tpe.widen, tree.span)
    val res = Throw(expr1).withSpan(tree.span)
    if Feature.ccEnabled && !cap.isEmpty && !ctx.isAfterTyper then
      // Record access to the CanThrow capabulity recovered in `cap` by wrapping
      // the type of the `throw` (i.e. Nothing) in a `@requiresCapability` annotatoon.
      Typed(res,
        TypeTree(
          AnnotatedType(res.tpe,
            Annotation(defn.RequiresCapabilityAnnot, cap, tree.span))))
    else res

  def typedSeqLiteral(tree: untpd.SeqLiteral, pt: Type)(using Context): SeqLiteral = {
    val elemProto = pt.stripNull.elemType match {
      case NoType => WildcardType
      case bounds: TypeBounds => WildcardType(bounds)
      case elemtp => elemtp
    }

    def assign(elems1: List[Tree], elemtpt1: Tree) =
      assignType(cpy.SeqLiteral(tree)(elems1, elemtpt1), elems1, elemtpt1)

    if (!tree.elemtpt.isEmpty) {
      val elemtpt1 = typed(tree.elemtpt, elemProto)
      val elems1 = tree.elems.mapconserve(typed(_, elemtpt1.tpe))
      assign(elems1, elemtpt1)
    }
    else {
      val elems1 = tree.elems.mapconserve(typed(_, elemProto))
      val elemtptType =
        if (isFullyDefined(elemProto, ForceDegree.none))
          elemProto
        else if (tree.elems.isEmpty && tree.isInstanceOf[Trees.JavaSeqLiteral[?]])
          defn.ObjectType // generic empty Java varargs are of type Object[]
        else
          TypeComparer.lub(elems1.tpes)
      val elemtpt1 = typed(tree.elemtpt, elemtptType)
      assign(elems1, elemtpt1)
    }
  }

  def typedInlined(tree: untpd.Inlined, pt: Type)(using Context): Tree =
    throw new UnsupportedOperationException("cannot type check a Inlined node")

  def completeTypeTree(tree: untpd.TypeTree, pt: Type, original: untpd.Tree)(using Context): TypeTree =
    tree.withSpan(original.span).withAttachmentsFrom(original)
      .withType(
        if isFullyDefined(pt, ForceDegree.flipBottom) then pt
        else if ctx.reporter.errorsReported then UnspecifiedErrorType
        else errorType(em"cannot infer type; expected type $pt is not fully defined", tree.srcPos))

  def typedTypeTree(tree: untpd.TypeTree, pt: Type)(using Context): Tree =
    tree match
      case tree: untpd.DerivedTypeTree =>
        tree.ensureCompletions
        tree.getAttachment(untpd.OriginalSymbol) match {
          case Some(origSym) =>
            tree.derivedTree(origSym).withSpan(tree.span)
            // btw, no need to remove the attachment. The typed
            // tree is different from the untyped one, so the
            // untyped tree is no longer accessed after all
            // accesses with typedTypeTree are done.
          case None =>
            errorTree(tree, em"Something's wrong: missing original symbol for type tree")
        }
      case _ =>
        completeTypeTree(InferredTypeTree(), pt, tree)

  def typedInLambdaTypeTree(tree: untpd.InLambdaTypeTree, pt: Type)(using Context): Tree =
    val tp =
      if tree.isResult then pt // See InLambdaTypeTree logic in Namer#valOrDefDefSig.
      else
        val lambdaCtx = ctx.outersIterator.dropWhile(_.owner.name ne nme.ANON_FUN).next()
        // A lambda has at most one type parameter list followed by exactly one term parameter list.
        // Parameters are entered in order in the scope of the lambda.
        val (tsyms: List[TypeSymbol @unchecked], vsyms: List[TermSymbol @unchecked]) =
          lambdaCtx.scope.toList.partition(_.isType): @unchecked
        tree.tpFun(tsyms, vsyms)
    completeTypeTree(InferredTypeTree(), tp, tree)

  def typedSingletonTypeTree(tree: untpd.SingletonTypeTree)(using Context): SingletonTypeTree = {
    val ref1 = typedExpr(tree.ref, SingletonTypeProto)
    checkStable(ref1.tpe, tree.srcPos, "singleton type")
    assignType(cpy.SingletonTypeTree(tree)(ref1), ref1)
  }

  def typedRefinedTypeTree(tree: untpd.RefinedTypeTree)(using Context): TypTree = {
    val tpt1 = if (tree.tpt.isEmpty) TypeTree(defn.ObjectType) else typedAheadType(tree.tpt)
    val refineClsDef = desugar.refinedTypeToClass(tpt1, tree.refinements).withSpan(tree.span)
    val refineCls = createSymbol(refineClsDef).asClass
    val TypeDef(_, impl: Template) = typed(refineClsDef): @unchecked
    val refinements1 = impl.body
    assert(tree.refinements.hasSameLengthAs(refinements1), i"${tree.refinements}%, % > $refinements1%, %")
    val seen = mutable.Set[Symbol]()
    for (refinement <- refinements1) { // TODO: get clarity whether we want to enforce these conditions
      typr.println(s"adding refinement $refinement")
      checkRefinementNonCyclic(refinement, refineCls, seen)
      val rsym = refinement.symbol
      rsym.setTargetName(EmptyTermName)
        // refinements can refine members with arbitrary target names, so we make their target names
        // polymorphic here in order to avoid to trigger the `member.isOverloaded` test below.
      val polymorphicRefinementAllowed =
        tpt1.tpe.typeSymbol == defn.PolyFunctionClass && rsym.name == nme.apply
      if (!polymorphicRefinementAllowed && rsym.info.isInstanceOf[PolyType] && rsym.allOverriddenSymbols.isEmpty)
        report.error(PolymorphicMethodMissingTypeInParent(rsym, tpt1.symbol), refinement.srcPos)
      val member = refineCls.info.member(rsym.name)
      if (member.isOverloaded)
        report.error(OverloadInRefinement(rsym), refinement.srcPos)
    }
    assignType(cpy.RefinedTypeTree(tree)(tpt1, refinements1), tpt1, refinements1, refineCls)
  }

  def typedAppliedTypeTree(tree: untpd.AppliedTypeTree)(using Context): Tree = {
    tree.args match
      case arg :: _ if arg.isTerm =>
        if Feature.dependentEnabled then
          return errorTree(tree, em"Not yet implemented: T(...)")
        else
          return errorTree(tree, dependentMsg)
      case _ =>

    val tpt1 = withoutMode(Mode.Pattern) {
      typed(tree.tpt, AnyTypeConstructorProto)
    }
    val tparams = tpt1.tpe.typeParams
     if tpt1.tpe.isError then
       val args1 = tree.args.mapconserve(typedType(_))
       assignType(cpy.AppliedTypeTree(tree)(tpt1, args1), tpt1, args1)
     else if (tparams.isEmpty) {
      report.error(TypeDoesNotTakeParameters(tpt1.tpe, tree.args), tree.srcPos)
      tpt1
    }
    else {
      var args = tree.args
      val args1 = {
        if (args.length != tparams.length) {
          wrongNumberOfTypeArgs(tpt1.tpe, tparams, args, tree.srcPos)
          args = args.take(tparams.length)
        }

        // If type constructor is not a class type, we need to eliminate
        // any references to other parameter types of the underlying hk lambda
        // in order not to get orphan parameters. Test case in pos/i15564.scala.
        // Note 1: It would be better to substitute actual arguments for corresponding
        // formal parameters, but it looks very hard to do this at the point where
        // a bound type variable is created.
        // Note 2: If the type constructor is a class type, no sanitization is needed
        // since we can refer to the other paraeters with dependent types C[...]#X.
        def sanitizeBounds(bounds: Type, tycon: Type): Type =
          def underlyingLambda(tp: Type): Type = tp match
            case tp: HKTypeLambda => tp
            case tp: TypeProxy => underlyingLambda(tp.superType)
            case _ => NoType
          val tl = underlyingLambda(tycon)
          val widenMap = new TypeOps.AvoidMap:
            def toAvoid(tp: NamedType) = false
            override def apply(tp: Type): Type = tp match
              case tp: TypeParamRef if tp.binder == tl => emptyRange
              case _ => super.apply(tp)
          widenMap(bounds)

        def typedArg(arg: untpd.Tree, tparam: ParamInfo) = {
          def tparamBounds =
            val bounds =
              tparam.paramInfoAsSeenFrom(tpt1.tpe.appliedTo(tparams.map(_ => TypeBounds.empty)))
            if tparam.isInstanceOf[Symbol] then bounds
            else sanitizeBounds(bounds, tpt1.tpe)
          val (desugaredArg, argPt) =
            if ctx.mode.is(Mode.Pattern) then
              (if (untpd.isVarPattern(arg)) desugar.patternVar(arg) else arg, tparamBounds)
            else if ctx.mode.isQuotedPattern then
              (arg, tparamBounds)
            else
              (arg, WildcardType)
          if (tpt1.symbol.isClass)
            tparam match {
              case tparam: Symbol =>
                tparam.ensureCompleted() // This is needed to get the test `compileParSetSubset` to work
              case _ =>
            }
          if (desugaredArg.isType)
            arg match {
              case untpd.WildcardTypeBoundsTree()
              if tparam.paramInfo.isLambdaSub &&
                 tpt1.tpe.typeParamSymbols.nonEmpty =>
                // An unbounded `_` automatically adapts to type parameter bounds. This means:
                // If we have wildcard application C[?], where `C` is a class replace
                // with C[? >: L <: H] where `L` and `H` are the bounds of the corresponding
                // type parameter in `C`.
                // The transform does not apply for patterns, where empty bounds translate to
                // wildcard identifiers `_` instead.
                TypeTree(tparamBounds).withSpan(arg.span)
              case _ =>
                typedType(desugaredArg, argPt, mapPatternBounds = true)
            }
          else desugaredArg.withType(UnspecifiedErrorType)
        }
        args.zipWithConserve(tparams)(typedArg)
      }
      val paramBounds = tparams.lazyZip(args).map {
        case (tparam, untpd.WildcardTypeBoundsTree()) =>
          // if type argument is a wildcard, suppress kind checking since
          // there is no real argument.
          NoType
        case (tparam, _) =>
          tparam.paramInfo.bounds
      }
      var checkedArgs = preCheckKinds(args1, paramBounds)
        // check that arguments conform to bounds is done in phase PostTyper
      val tycon = tpt1.symbol
      if tycon == defn.andType || tycon == defn.orType then
        checkedArgs = checkedArgs.mapconserve(arg =>
          checkSimpleKinded(checkNoWildcard(arg)))
      else if tycon.isProvisional then
        // A type with Provisional flag is either an alias or abstract type.
        // If it is an alias type, it would mean the type is cyclic
        // If it is an abstract type, it would mean the type is an irreducible
        // application of a higher-kinded type to a wildcard argument.
        // Either way, the wildcard argument is illegal. The early test of
        // `checkNoWildcard` here is needed, so that we do not accidentally reduce
        // an application of a Provisional type away, which would mean that the type constructor
        // is no longer present on the right hand side. See neg/i15507.scala.
        checkedArgs = checkedArgs.mapconserve(checkNoWildcard)
      else if tycon == defn.throwsAlias
          && checkedArgs.length == 2
          && checkedArgs(1).tpe.derivesFrom(defn.RuntimeExceptionClass)
      then
        report.error(em"throws clause cannot be defined for RuntimeException", checkedArgs(1).srcPos)
      else if (ctx.isJava)
        if tycon eq defn.ArrayClass then
          checkedArgs match {
            case List(arg) =>
              val elemtp = arg.tpe.translateJavaArrayElementType
              if (elemtp ne arg.tpe)
                checkedArgs = List(TypeTree(elemtp).withSpan(arg.span))
            case _ =>
          }
      assignType(cpy.AppliedTypeTree(tree)(tpt1, checkedArgs), tpt1, checkedArgs)
    }
  }

  private def typeIndexedLambdaTypeTree(
      tree: untpd.LambdaTypeTree, tparams: List[untpd.TypeDef], body: untpd.Tree)(using Context) =
    val tparams1 = tparams.map(typed(_)).asInstanceOf[List[TypeDef]]
    val body1 = typedType(body)
    assignType(cpy.LambdaTypeTree(tree)(tparams1, body1), tparams1, body1)

  def typedLambdaTypeTree(tree: untpd.LambdaTypeTree)(using Context): Tree =
    val LambdaTypeTree(tparams, body) = tree
    index(tparams)
    typeIndexedLambdaTypeTree(tree, tparams, body)

  def typedTermLambdaTypeTree(tree: untpd.TermLambdaTypeTree)(using Context): Tree =
    if Feature.dependentEnabled then
      errorTree(tree, em"Not yet implemented: (...) =>> ...")
    else
      errorTree(tree, dependentMsg)

  def typedMatchTypeTree(tree: untpd.MatchTypeTree, pt: Type)(using Context): Tree = {
    val bound1 =
      if (tree.bound.isEmpty && isFullyDefined(pt, ForceDegree.none)) TypeTree(pt)
      else typed(tree.bound)
    val sel1 = typed(tree.selector)
    val sel1Tpe = sel1.tpe
    if sel1Tpe.isLambdaSub then
      report.error(MatchTypeScrutineeCannotBeHigherKinded(sel1Tpe), sel1.srcPos)
    val pt1 = if (bound1.isEmpty) pt else bound1.tpe
    val cases1 = tree.cases.mapconserve(typedTypeCase(_, sel1Tpe, pt1))
    val bound2 = if tree.bound.isEmpty then
      val lub = cases1.foldLeft(defn.NothingType: Type): (acc, case1) =>
        if !acc.exists then NoType
        else if case1.body.tpe.isProvisional then NoType
        else acc | case1.body.tpe
      if lub.exists then TypeTree(lub, inferred = true)
      else bound1
    else bound1
    assignType(cpy.MatchTypeTree(tree)(bound2, sel1, cases1), bound2, sel1, cases1)
  }

  def typedByNameTypeTree(tree: untpd.ByNameTypeTree)(using Context): ByNameTypeTree = tree.result match
    case untpd.CapturesAndResult(refs, tpe) =>
      typedByNameTypeTree(
        cpy.ByNameTypeTree(tree)(untpd.makeRetaining(tpe, refs, tpnme.retainsByName)))
    case _ =>
      val result1 = typed(tree.result)
      assignType(cpy.ByNameTypeTree(tree)(result1), result1)

  def typedTypeBoundsTree(tree: untpd.TypeBoundsTree, pt: Type)(using Context): Tree =
    val TypeBoundsTree(lo, hi, alias) = tree
    val lo1 = typed(lo)
    val hi1 = typed(hi)
    val alias1 = typed(alias)
    val lo2 = if (lo1.isEmpty) typed(untpd.TypeTree(defn.NothingType)) else lo1
    val hi2 = if (hi1.isEmpty) typed(untpd.TypeTree(defn.AnyType)) else hi1
    assignType(cpy.TypeBoundsTree(tree)(lo2, hi2, alias1), lo2, hi2, alias1)

  def typedBind(tree: untpd.Bind, pt: Type)(using Context): Tree = {
    if !isFullyDefined(pt, ForceDegree.all) then
      return errorTree(tree, em"expected type of $tree is not fully defined")
    val body1 = typed(tree.body, pt)
    body1 match {
      case UnApply(fn, Nil, arg :: Nil)
      if fn.symbol.exists && (fn.symbol.owner.derivesFrom(defn.TypeTestClass) || fn.symbol.owner == defn.ClassTagClass) && !body1.tpe.isError =>
        // A typed pattern `x @ (e: T)` with an implicit `tt: TypeTest[T]` or `ctag: ClassTag[T]`
        // was rewritten to `x @ tt(e)` `x @ ctag(e)` by `tryWithTypeTest`.
        // Rewrite further to `tt(x @ e)` or `ctag(x @ e)`
        tpd.cpy.UnApply(body1)(fn, Nil,
            typed(untpd.Bind(tree.name, untpd.TypedSplice(arg)).withSpan(tree.span), arg.tpe) :: Nil)
      case _ =>
        var name = tree.name
        if (name == nme.WILDCARD && tree.mods.is(Given)) {
          val Typed(_, tpt) = tree.body: @unchecked
          name = desugar.inventGivenOrExtensionName(tpt)
        }
        if (name == nme.WILDCARD) body1
        else {
          // In `x @ Nil`, `Nil` is a _stable identifier pattern_ and will be compiled
          // to an `==` test, so the type of `x` is unrelated to the type of `Nil`.
          // Similarly, in `x @ 1`, `1` is a _literal pattern_ and will also be compiled
          // to an `==` test.
          // See i3200*.scala and https://github.com/scala/bug/issues/1503.
          val isStableIdentifierOrLiteral =
            body1.isInstanceOf[RefTree] && !isWildcardArg(body1)
            || body1.isInstanceOf[Literal]
          val symTp =
            if isStableIdentifierOrLiteral then pt
            else if isWildcardStarArg(body1)
                    || pt == defn.ImplicitScrutineeTypeRef
                    || body1.tpe <:< pt  // There is some strange interaction with gadt matching.
                                         // and implicit scopes.
                                         // run/t2755.scala fails to compile if this subtype test is omitted
                                         // and the else clause is changed to `body1.tpe & pt`. What
                                         // happens is that we get either an Array[Float] or an Array[T]
                                         // where T is GADT constrained to := Float. But the case body
                                         // compiles only if the bound variable is Array[Float]. If
                                         // it is Array[T] we get an implicit not found. To avoid fragility
                                         // wrt to operand order for `&`, we include the explicit subtype test here.
                                         // See also #5649.
            then body1.tpe
            else body1.tpe match
              case btpe: TypeRef
              if btpe.symbol == defn.TupleXXLClass && pt.tupleElementTypes.isDefined =>
                // leave the original tuple type; don't mix with & TupleXXL which would only obscure things
                pt
              case _ =>
                pt & body1.tpe
          val sym = newPatternBoundSymbol(name, symTp, tree.span)
          if (pt == defn.ImplicitScrutineeTypeRef || tree.mods.is(Given)) sym.setFlag(Given)
          if (ctx.mode.is(Mode.InPatternAlternative))
            report.error(IllegalVariableInPatternAlternative(sym.name), tree.srcPos)
          assignType(cpy.Bind(tree)(name, body1), sym)
        }
    }
  }

  def typedAlternative(tree: untpd.Alternative, pt: Type)(using Context): Alternative = {
    val nestedCtx = ctx.addMode(Mode.InPatternAlternative)
    def ensureValueTypeOrWildcard(tree: Tree) =
      if tree.tpe.isValueTypeOrWildcard then tree
      else
        assert(ctx.reporter.errorsReported)
        tree.withType(defn.AnyType)
    val trees1 = tree.trees.mapconserve(typed(_, pt)(using nestedCtx))
      .mapconserve(ensureValueTypeOrWildcard)
    assignType(cpy.Alternative(tree)(trees1), trees1)
  }

  /** The context to be used for an annotation of `mdef`.
   *  This should be the context enclosing `mdef`, or if `mdef` defines a parameter
   *  the context enclosing the owner of `mdef`.
   *  Furthermore, we need to make sure that annotation trees are evaluated
   *  with an owner that is not the enclosing class since otherwise locally
   *  defined symbols would be entered as class members.
   */
  def annotContext(mdef: untpd.Tree, sym: Symbol)(using Context): Context =
    def isInner(owner: Symbol) = owner == sym || sym.is(Param) && owner == sym.owner
    val outer = ctx.outersIterator.dropWhile(c => isInner(c.owner)).next()
    def local: FreshContext = outer.fresh.setOwner(newLocalDummy(sym.owner))
    sym.owner.infoOrCompleter match
      case completer: Namer#Completer
      if sym.is(Param) && completer.completerTypeParams(sym).nonEmpty =>
        // Create a new local context with a dummy owner and a scope containing the
        // type parameters of the enclosing method or class. Thus annotations can see
        // these type parameters. See i12953.scala for a test case.
        local.setScope(newScopeWith(completer.completerTypeParams(sym)*))
      case _ =>
        if outer.owner.isClass then local else outer

  def completeAnnotations(mdef: untpd.MemberDef, sym: Symbol)(using Context): Unit = {
    // necessary to force annotation trees to be computed.
    sym.annotations.foreach(_.ensureCompleted)
    lazy val annotCtx = annotContext(mdef, sym)
    // necessary in order to mark the typed ahead annotations as definitely typed:
    for (annot <- mdef.mods.annotations)
      val annot1 = typedAnnotation(annot)(using annotCtx)
      checkAnnotApplicable(annot1, sym)
      if Annotations.annotClass(annot1) == defn.NowarnAnnot then
        registerNowarn(annot1, mdef)
  }

  def typedAnnotation(annot: untpd.Tree)(using Context): Tree =
    checkAnnotClass(checkAnnotArgs(typed(annot)))

  def registerNowarn(tree: Tree, mdef: untpd.Tree)(using Context): Unit =
    val annot = Annotations.Annotation(tree)
    def argPos = annot.argument(0).getOrElse(tree).sourcePos
    var verbose = false
    val filters = annot.argumentConstantString(0) match
      case None => annot.argument(0) match
        case Some(t: Select) if t.name.is(DefaultGetterName) =>
          // default argument used for `@nowarn` and `@nowarn()`
          List(MessageFilter.Any)
        case _ =>
          report.warning(s"filter needs to be a compile-time constant string", argPos)
          List(MessageFilter.None)
      case Some("") =>
        List(MessageFilter.Any)
      case Some("verbose") | Some("v") =>
        verbose = true
        List(MessageFilter.Any)
      case Some(s) =>
        WConf.parseFilters(s).left.map(parseErrors =>
          report.warning (s"Invalid message filter\n${parseErrors.mkString ("\n")}", argPos)
            List(MessageFilter.None)
        ).merge
    val range = mdef.sourcePos
    val sup = Suppression(tree.sourcePos, filters, range.start, range.end, verbose)
    // invalid suppressions, don't report as unused
    if filters == List(MessageFilter.None) then sup.markUsed()
    ctx.run.nn.suppressions.addSuppression(sup)

  def typedValDef(vdef: untpd.ValDef, sym: Symbol)(using Context): Tree = {
    val ValDef(name, tpt, _) = vdef
    checkNonRootName(vdef.name, vdef.nameSpan)
    completeAnnotations(vdef, sym)
    if (sym.isOneOf(GivenOrImplicit)) checkImplicitConversionDefOK(sym)
    if sym.is(Module) then checkNoModuleClash(sym)
    val tpt1 = checkSimpleKinded(typedType(tpt))
    val rhs1 = vdef.rhs match {
      case rhs @ Ident(nme.WILDCARD) => rhs withType tpt1.tpe
      case rhs => typedExpr(rhs, tpt1.tpe.widenExpr)
    }
    val vdef1 = assignType(cpy.ValDef(vdef)(name, tpt1, rhs1), sym)
    postProcessInfo(vdef1, sym)
    vdef1.setDefTree
  }

  def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(using Context): Tree = {
    def canBeInvalidated(sym: Symbol): Boolean =
      sym.is(Synthetic)
      && (desugar.isRetractableCaseClassMethodName(sym.name) ||
         (sym.owner.is(JavaDefined) && sym.owner.derivesFrom(defn.JavaRecordClass) && sym.is(Method)))

    if !sym.info.exists then
      // it's a discarded method (synthetic case class method or synthetic java record constructor or overriden member), drop it
      assert(canBeInvalidated(sym))
      sym.owner.info.decls.openForMutations.unlink(sym)
      return EmptyTree

    // TODO: - Remove this when `scala.language.experimental.erasedDefinitions` is no longer experimental.
    //       - Modify signature to `erased def erasedValue[T]: T`
    if sym.eq(defn.Compiletime_erasedValue) then
      // scala.compiletime.erasedValue should be `erased` but we cannot add this in the source.
      // The library cannot use experimental language features,
      // hence we special case it until `erased` is no longer experimental.
      sym.setFlag(Erased)
    val DefDef(name, paramss, tpt, _) = ddef
    checkNonRootName(ddef.name, ddef.nameSpan)
    completeAnnotations(ddef, sym)
    val paramss1 = paramss.nestedMapConserve(typed(_)).asInstanceOf[List[ParamClause]]
    for case ValDefs(vparams) <- paramss1 do
      checkNoForwardDependencies(vparams)
    if (sym.isOneOf(GivenOrImplicit)) checkImplicitConversionDefOK(sym)
    val tpt1 = checkSimpleKinded(typedType(tpt))

    val rhsCtx = ctx.fresh
    val tparamss = paramss1.collect {
      case untpd.TypeDefs(tparams) => tparams
    }

    // Register GADT constraint for class type parameters from outer to inner class definition. (Useful when nested classes exist.) But do not cross a function definition.
    if sym.flags.is(Method) then
      rhsCtx.setFreshGADTBounds
      ctx.outer.outersIterator.takeWhile(!_.owner.is(Method))
        .filter(ctx => ctx.owner.isClass && ctx.owner.typeParams.nonEmpty)
        .toList.reverse
        .foreach(ctx => rhsCtx.gadtState.addToConstraint(ctx.owner.typeParams))

    if tparamss.nonEmpty then
      rhsCtx.setFreshGADTBounds
      val tparamSyms = tparamss.flatten.map(_.symbol)
      if !sym.isConstructor then
        // we're typing a polymorphic definition's body,
        // so we allow constraining all of its type parameters
        // constructors are an exception as we don't allow constraining type params of classes
        rhsCtx.gadtState.addToConstraint(tparamSyms)
      else if !sym.isPrimaryConstructor then
        linkConstructorParams(sym, tparamSyms, rhsCtx)

    if sym.isInlineMethod then rhsCtx.addMode(Mode.InlineableBody)
    if sym.is(ExtensionMethod) then rhsCtx.addMode(Mode.InExtensionMethod)
    val rhs1 = PrepareInlineable.dropInlineIfError(sym,
      if sym.isScala2Macro then typedScala2MacroBody(ddef.rhs)(using rhsCtx)
      else typedExpr(ddef.rhs, tpt1.tpe.widenExpr)(using rhsCtx))

    if sym.isInlineMethod then
      if StagingLevel.level > 0 then
        report.error("inline def cannot be within quotes", sym.sourcePos)
      if sym.is(Given)
          && untpd.stripBlock(untpd.unsplice(ddef.rhs)).isInstanceOf[untpd.Function]
      then
        report.warning(InlineGivenShouldNotBeFunction(), ddef.rhs.srcPos)
      val rhsToInline = PrepareInlineable.wrapRHS(ddef, tpt1, rhs1)
      PrepareInlineable.registerInlineInfo(sym, rhsToInline)

    if sym.isConstructor then
      if sym.is(Inline) then
        report.error("constructors cannot be `inline`", ddef)
      if sym.isPrimaryConstructor then
        if sym.owner.is(Case) then
          for
            params <- paramss1.dropWhile(TypeDefs.unapply(_).isDefined).take(1)
            case param: ValDef <- params
          do
            if defn.isContextFunctionType(param.tpt.tpe) then
              report.error("case class element cannot be a context function", param.srcPos)
      else
        if sym.targetName != sym.name then
          report.error(em"@targetName annotation may not be used on a constructor", ddef.srcPos)

        for params <- paramss1; param <- params do
          checkRefsLegal(param, sym.owner, (name, sym) => sym.is(TypeParam), "secondary constructor")

        def checkThisConstrCall(tree: Tree): Unit = tree match
          case app: Apply if untpd.isSelfConstrCall(app) =>
            if (sym.span.exists && app.symbol.span.exists && sym.span.start <= app.symbol.span.start)
              report.error("secondary constructor must call a preceding constructor", app.srcPos)
          case Block(call :: _, _) => checkThisConstrCall(call)
          case _ =>

        checkThisConstrCall(rhs1)
      end if
    end if

    if sym.is(Method) && sym.owner.denot.isRefinementClass then
      for annot <- sym.paramSymss.flatten.filter(_.isTerm).flatMap(_.getAnnotation(defn.ImplicitNotFoundAnnot)) do
        report.warning(
          i"The annotation ${defn.ImplicitNotFoundAnnot} is not allowed on parameters of methods defined inside a refinement and it will have no effect",
          annot.tree.sourcePos
        )

    val ddef2 = assignType(cpy.DefDef(ddef)(name, paramss1, tpt1, rhs1), sym)

    postProcessInfo(ddef2, sym)
    ddef2.setDefTree
      //todo: make sure dependent method types do not depend on implicits or by-name params
  }

  /** (1) Check that the signature of the class member does not return a repeated parameter type
   *  (2) If info is an erased class, set erased flag of member
   *  (3) Check that erased classes are not parameters of polymorphic functions.
   */
  private def postProcessInfo(mdef: MemberDef, sym: Symbol)(using Context): Unit =
    if (!sym.isOneOf(Synthetic | InlineProxy | Param) && sym.info.finalResultType.isRepeatedParam)
      report.error(em"Cannot return repeated parameter type ${sym.info.finalResultType}", sym.srcPos)
    if !sym.is(Module) && !sym.isConstructor && sym.info.finalResultType.isErasedClass then
      sym.setFlag(Erased)

  def typedTypeDef(tdef: untpd.TypeDef, sym: Symbol)(using Context): Tree = {
    val TypeDef(name, rhs) = tdef
    completeAnnotations(tdef, sym)
    val rhs1 = tdef.rhs match
      case rhs @ LambdaTypeTree(tparams, body) =>
        typeIndexedLambdaTypeTree(rhs, tparams, body)
      case rhs =>
        typedType(rhs)
    checkFullyAppliedType(rhs1)
    if sym.isOpaqueAlias then checkNoContextFunctionType(rhs1)
    assignType(cpy.TypeDef(tdef)(name, rhs1), sym)
  }

  def typedClassDef(cdef: untpd.TypeDef, cls: ClassSymbol)(using Context): Tree = {
    if (!cls.info.isInstanceOf[ClassInfo]) return EmptyTree.assertingErrorsReported

    val TypeDef(name, impl @ Template(constr, _, self, _)) = cdef: @unchecked
    val parents = impl.parents
    val superCtx = ctx.superCallContext
    val seenParents = mutable.Set[Symbol]()

    def typedParent(tree: untpd.Tree): Tree =
      val parent = tree match
        case _: untpd.Apply => typedExpr(tree)(using superCtx)
        case _ => typedType(tree)(using superCtx)
      val psym = parent.tpe.dealias.typeSymbol
      if seenParents.contains(psym) && !cls.isRefinementClass then
        // Desugaring can add parents to classes, but we don't want to emit an
        // error if the same parent was explicitly added in user code.
        if !tree.span.isSourceDerived then
          return EmptyTree
        if !ctx.isAfterTyper then report.error(em"$psym is extended twice", tree.srcPos)
      else
        seenParents += psym
      val result = ensureConstrCall(cls, parent, psym)(using superCtx)
      if parent.isType then
        if !result.symbol.info.takesImplicitParams then
          checkSimpleKinded(parent)
            // allow missing type parameters if there are implicit arguments to pass
            // since we can infer type arguments from them
        val constr = psym.primaryConstructor
        if psym.is(Trait) && constr.exists && !cls.isRefinementClass then
          ensureAccessible(constr.termRef, superAccess = true, tree.srcPos)
      else
        checkParentCall(result, cls)
      if cls is Case then
        checkCaseInheritance(psym, cls, tree.srcPos)
      result

    def ensureCorrectSuperClass(): Unit =
      val parents0 = cls.classInfo.declaredParents
      parents0 match
        case AnnotatedType(sc, ann) :: rest if ann.symbol == defn.ProvisionalSuperClassAnnot =>
          val parents1 = ensureFirstIsClass(cls, rest)
          if parents1.head ne sc then
            typr.println(i"improved provisional superclass $sc to ${parents1.head}")
          cls.info = cls.classInfo.derivedClassInfo(declaredParents = parents1)
        case _ =>

    /** Augment `ptrees` to have the same class symbols as `parents`. Generate TypeTrees
     *  or New trees to fill in any parents for which no tree exists yet.
     */
    def parentTrees(parents: List[Type], ptrees: List[Tree]): List[Tree] =
      if ptrees.exists(_.tpe.isError) then ptrees
      else parents match
        case parent :: parents1 =>
          val psym = parent.classSymbol
          def hasSameParent(ptree: Tree) =
            psym == (
              if ptree.symbol.isConstructor then ptree.symbol.owner
              else ptree.tpe.classSymbol
            )
          ptrees match
            case ptree :: ptrees1 if hasSameParent(ptree) =>
              ptree :: parentTrees(parents1, ptrees1)
            case ptree :: ptrees1 if ptrees1.exists(hasSameParent) =>
              ptree :: parentTrees(parents, ptrees1)
            case _ =>
              val added: Tree = ensureConstrCall(
                cls, TypeTree(parent).withSpan(cdef.nameSpan.focus), psym)(using superCtx)
              added :: parentTrees(parents1, ptrees)
        case _ =>
          ptrees

    /** Checks if one of the decls is a type with the same name as class type member in selfType */
    def classExistsOnSelf(decls: Scope, self: tpd.ValDef): Boolean = {
      val selfType = self.tpt.tpe
      if (!selfType.exists || (selfType.classSymbol eq cls)) false
      else {
        def memberInSelfButNotThis(decl: Symbol) =
          selfType.member(decl.name).symbol.filter(other => other.isClass && other.owner != cls)
        decls.iterator.filter(_.isType).foldLeft(false) { (foundRedef, decl) =>
          val other = memberInSelfButNotThis(decl)
          if (other.exists) {
            val msg = CannotHaveSameNameAs(decl, other, CannotHaveSameNameAs.DefinedInSelf(self))
            report.error(msg, decl.srcPos)
          }
          foundRedef || other.exists
        }
      }
    }

    ensureCorrectSuperClass()
    completeAnnotations(cdef, cls)
    val constr1 = typed(constr).asInstanceOf[DefDef]
    val parents1 = parentTrees(
        cls.classInfo.declaredParents,
        parents.mapconserve(typedParent).filterConserve(!_.isEmpty))
    val firstParentTpe = parents1.head.tpe.dealias
    val firstParent = firstParentTpe.typeSymbol

    checkEnumParent(cls, firstParent)

    if defn.ScalaValueClasses()(cls) && ctx.settings.YcompileScala2Library.value then
      constr1.symbol.resetFlag(Private)

    val self1 = typed(self)(using ctx.outer).asInstanceOf[ValDef] // outer context where class members are not visible
    if (self1.tpt.tpe.isError || classExistsOnSelf(cls.unforcedDecls, self1))
      // fail fast to avoid typing the body with an error type
      cdef.withType(UnspecifiedErrorType)
    else {
      val dummy = localDummy(cls, impl)
      val body1 = addAccessorDefs(cls, typedStats(impl.body, dummy)(using ctx.inClassContext(self1.symbol))._1)

      checkNoDoubleDeclaration(cls)
      val impl1 = cpy.Template(impl)(constr1, parents1, Nil, self1, body1)
        .withType(dummy.termRef)
      if (!cls.isOneOf(AbstractOrTrait) && !ctx.isAfterTyper)
        checkRealizableBounds(cls, cdef.sourcePos.withSpan(cdef.nameSpan))
      if cls.isEnum || firstParentTpe.classSymbol.isEnum then
        checkEnum(cdef, cls, firstParent)
      val cdef1 = assignType(cpy.TypeDef(cdef)(name, impl1), cls)

      val reportDynamicInheritance =
        ctx.phase.isTyper &&
        cdef1.symbol.ne(defn.DynamicClass) &&
        cdef1.tpe.derivesFrom(defn.DynamicClass) &&
        !Feature.dynamicsEnabled
      if (reportDynamicInheritance) {
        val isRequired = parents1.exists(_.tpe.isRef(defn.DynamicClass))
        report.featureWarning(nme.dynamics.toString, "extension of type scala.Dynamic", cls, isRequired, cdef.srcPos)
      }

      checkNonCyclicInherited(cls.thisType, cls.info.parents, cls.info.decls, cdef.srcPos)

      // check value class constraints
      checkDerivedValueClass(cls, body1)

      val effectiveOwner = cls.owner.skipWeakOwner
      if cls.is(ModuleClass)
         && effectiveOwner.is(Trait)
         && !effectiveOwner.derivesFrom(defn.ObjectClass)
      then
        report.error(em"$cls cannot be defined in universal $effectiveOwner", cdef.srcPos)

      // Temporarily set the typed class def as root tree so that we have at least some
      // information in the IDE in case we never reach `SetRootTree`.
      if (ctx.mode.is(Mode.Interactive) && ctx.settings.YretainTrees.value)
        cls.rootTreeOrProvider = cdef1

      for (deriver <- cdef.removeAttachment(AttachedDeriver))
        cdef1.putAttachment(AttachedDeriver, deriver)

      cdef1
    }
  }

      // todo later: check that
      //  1. If class is non-abstract, it is instantiatable:
      //  - self type is s supertype of own type
      //  - all type members have consistent bounds
      // 2. all private type members have consistent bounds
      // 3. Types do not override classes.
      // 4. Polymorphic type defs override nothing.

  protected def addAccessorDefs(cls: Symbol, body: List[Tree])(using Context): List[Tree] =
    PrepareInlineable.addAccessorDefs(cls, body)

  /** Turn a parent type into a constructor call where needed. This is the case where
   *   - we are in a Scala class or module (not a Java class, nor a trait), and
   *     - the parent symbol is a non-trait class, or
   *     - the parent symbol is a trait that takes at least one (explicit or implicit) parameter
   *       and the parent symbol is directly extended by the current class (i.e. not
   *       extended by the superclass).
   */
  def ensureConstrCall(cls: ClassSymbol, parent: Tree, psym: Symbol)(using Context): Tree =
    if parent.isType && !cls.is(Trait) && !cls.is(JavaDefined) && psym.isClass
        // Annotations are represented as traits with constructors, but should
        // never be called as such outside of annotation trees.
        && !psym.is(JavaAnnotation)
        && (!psym.is(Trait)
            || psym.primaryConstructor.info.takesParams && !cls.superClass.isSubClass(psym))
    then typed(untpd.New(untpd.TypedSplice(parent), Nil))
    else parent

  def localDummy(cls: ClassSymbol, impl: untpd.Template)(using Context): Symbol =
    newLocalDummy(cls, impl.span)

  inline private def typedSelectors(selectors: List[untpd.ImportSelector])(using Context): List[untpd.ImportSelector] =
    selectors.mapConserve { sel =>
      if sel.bound.isEmpty then sel
      else cpy.ImportSelector(sel)(
        sel.imported, sel.renamed, untpd.TypedSplice(typedType(sel.bound)))
        .asInstanceOf[untpd.ImportSelector]
    }

  def typedImportQualifier(imp: untpd.Import, typd: (untpd.Tree, Type) => Tree)(using Context): Tree =
    if imp.expr == untpd.EmptyTree then
      assert(imp.selectors.length == 1, imp)
      val from = imp.selectors.head.imported
      val sel = tryAlternatively
          (typedIdent(from, AnySelectionProto))
          (typedIdent(cpy.Ident(from)(from.name.toTypeName), WildcardType))

      sel.tpe match
        case TermRef(prefix: SingletonType, _)  =>
          singleton(prefix).withSpan(from.span)
        case TypeRef(prefix: SingletonType, _)  =>
          singleton(prefix).withSpan(from.span)
        case _ =>
          errorTree(from,
            em"""Illegal import selector: $from
                |The selector is not a member of an object or package.""")
    else typd(imp.expr, AnySelectionProto)

  def typedImport(imp: untpd.Import)(using Context): Tree =
    val sym = retrieveSym(imp)
    val expr1 = typedImportQualifier(imp, typedExpr(_, _)(using ctx.withOwner(sym)))
    checkLegalImportPath(expr1)
    val selectors1 = typedSelectors(imp.selectors)
    checkImportSelectors(expr1.tpe, selectors1)
    assignType(cpy.Import(imp)(expr1, selectors1), sym)

  def typedExport(exp: untpd.Export)(using Context): Tree =
    exp.expr.removeAttachment(TypedAhead) match
      case Some(expr1) =>
        val selectors1 = typedSelectors(exp.selectors)
        assignType(cpy.Export(exp)(expr1, selectors1))
      case _ =>
        errorTree(exp, em"exports are only allowed from objects and classes, they can not belong to local blocks")

  def typedPackageDef(tree: untpd.PackageDef)(using Context): Tree =
    val pid1 = withMode(Mode.InPackageClauseName)(typedExpr(tree.pid, AnySelectionProto))
    val pkg = pid1.symbol
    pid1 match
      case pid1: RefTree if pkg.is(Package) =>
        if ctx.owner != defn.RootClass         // valid top-level "package _root_"
        && ctx.owner != defn.EmptyPackageClass // valid "package _root_" after parser's "package <empty>" wrapper
        then
          checkNonRootName(pid1.name, pid1.span)
        inContext(ctx.packageContext(tree, pkg)) {
          // If it exists, complete the class containing the top-level definitions
          // before typing any statement in the package to avoid cycles as in i13669.scala
          val topLevelClassName = desugar.packageObjectName(ctx.source).moduleClassName
          pkg.moduleClass.info.decls.lookup(topLevelClassName).ensureCompleted()
          var stats1 = typedStats(tree.stats, pkg.moduleClass)._1
          if (!ctx.isAfterTyper)
            stats1 = stats1 ++ typedBlockStats(MainProxies.proxies(stats1))._1
          cpy.PackageDef(tree)(pid1, stats1).withType(pkg.termRef)
        }
      case _ =>
        // Package will not exist if a duplicate type has already been entered, see `tests/neg/1708.scala`
        errorTree(tree,
          if pkg.exists then PackageNameAlreadyDefined(pkg)
          else em"package ${tree.pid.name} does not exist")
  end typedPackageDef

  def typedAnnotated(tree: untpd.Annotated, pt: Type)(using Context): Tree = {
    val annot1 = checkAnnotClass(typedExpr(tree.annot))
    val annotCls = Annotations.annotClass(annot1)
    if annotCls == defn.NowarnAnnot then
      registerNowarn(annot1, tree)
    val arg1 = typed(tree.arg, pt)
    if (ctx.mode is Mode.Type) {
      val cls = annot1.symbol.maybeOwner
      if Feature.ccEnabled && cls.isRetainsLike then
        CheckCaptures.checkWellformed(arg1, annot1)
      if arg1.isType then
        assignType(cpy.Annotated(tree)(arg1, annot1), arg1, annot1)
      else
        assert(ctx.reporter.errorsReported)
        TypeTree(UnspecifiedErrorType)
    }
    else {
      val arg2 = arg1 match {
        case Typed(arg2, tpt: TypeTree) =>
          tpt.tpe match {
            case _: AnnotatedType =>
              // Avoid creating a Typed tree for each type annotation that is added.
              // Drop the outer Typed tree and use its type with the addition all annotation.
              arg2
            case _ => arg1
          }
        case _ => arg1
      }
      val argType =
        if (arg1.isInstanceOf[Bind]) arg1.tpe.widen // bound symbol is not accessible outside of Bind node
        else arg1.tpe.widenIfUnstable
      val annotatedTpt = TypeTree(AnnotatedType(argType, Annotation(annot1)))
      assignType(cpy.Typed(tree)(arg2, annotatedTpt), annotatedTpt)
    }
  }

  def typedTypedSplice(tree: untpd.TypedSplice)(using Context): Tree =
    tree.splice match {
      case tree1: TypeTree => tree1  // no change owner necessary here ...
      case tree1: Ident => tree1     // ... or here, since these trees cannot contain bindings
      case tree1 =>
        if (ctx.owner ne tree.owner) tree1.changeOwner(tree.owner, ctx.owner)
        else tree1
    }

  override def typedAsFunction(tree: untpd.PostfixOp, pt: Type)(using Context): Tree =
    migrate(super.typedAsFunction(tree, pt))

  /** Translate infix operation expression `l op r` to
   *
   *    l.op(r)   			        if `op` is left-associative
   *    { val x = l; r.op(x) }  if `op` is right-associative call-by-value and `l` is impure, and not in a quote pattern
   *    r.op(l)                 if `op` is right-associative call-by-name, or `l` is pure, or in a quote pattern
   *
   *  Translate infix type    `l op r` to `op[l, r]`
   *  Translate infix pattern `l op r` to `op(l, r)`
   */
  def typedInfixOp(tree: untpd.InfixOp, pt: Type)(using Context): Tree = {
    val untpd.InfixOp(l, op, r) = tree
    val result =
      if (ctx.mode.is(Mode.Type))
        typedAppliedTypeTree(
          if op.name == tpnme.throws && Feature.enabled(Feature.saferExceptions)
          then desugar.throws(l, op, r)
          else cpy.AppliedTypeTree(tree)(op, l :: r :: Nil))
      else if (ctx.mode.is(Mode.Pattern))
        typedUnApply(cpy.Apply(tree)(op, l :: r :: Nil), pt)
      else {
        val app = typedApply(desugar.binop(l, op, r), pt)
        if op.name.isRightAssocOperatorName && !ctx.mode.is(Mode.QuotedExprPattern) then
          val defs = new mutable.ListBuffer[Tree]
          def lift(app: Tree): Tree = (app: @unchecked) match
            case Apply(fn, args) =>
              if (app.tpe.isError) app
              else tpd.cpy.Apply(app)(fn, LiftImpure.liftArgs(defs, fn.tpe, args))
            case Assign(lhs, rhs) =>
              tpd.cpy.Assign(app)(lhs, lift(rhs))
            case Block(stats, expr) =>
              tpd.cpy.Block(app)(stats, lift(expr))
          wrapDefs(defs, lift(app))
        else app
      }
    // issue 10383: we stripBlock because e.g. default arguments desugar to blocks during typing,
    // and the block itself doesn't have a symbol (because a Block isn't a ProxyTree),
    // but the last expression in the block does have the right symbol
    checkValidInfix(tree, stripBlock(result).symbol)
    result
  }

  /** Translate tuples of all arities */
  def typedTuple(tree: untpd.Tuple, pt: Type)(using Context): Tree = {
    val arity = tree.trees.length
    if (arity <= Definitions.MaxTupleArity)
      typed(desugar.smallTuple(tree).withSpan(tree.span), pt)
    else {
      val pts =
        pt.tupleElementTypes match
          case Some(types) if types.size == arity => types
          case _ => List.fill(arity)(defn.AnyType)
      val elems = tree.trees.lazyZip(pts).map(
        if ctx.mode.is(Mode.Type) then typedType(_, _, mapPatternBounds = true)
        else typed(_, _))
      if (ctx.mode.is(Mode.Type))
        elems.foldRight(TypeTree(defn.EmptyTupleModule.termRef): Tree)((elemTpt, elemTpts) =>
          AppliedTypeTree(TypeTree(defn.PairClass.typeRef), List(elemTpt, elemTpts)))
          .withSpan(tree.span)
      else {
        val tupleXXLobj = untpd.ref(defn.TupleXXLModule.termRef)
        val app = untpd.cpy.Apply(tree)(tupleXXLobj, elems.map(untpd.TypedSplice(_)))
          .withSpan(tree.span)
        val app1 = typed(app, if ctx.mode.is(Mode.Pattern) then pt else defn.TupleXXLClass.typeRef)
        if (ctx.mode.is(Mode.Pattern)) app1
        else {
          val elemTpes = elems.lazyZip(pts).map((elem, pt) =>
            TypeComparer.widenInferred(elem.tpe, pt, widenUnions = true))
          val resTpe = TypeOps.nestedPairs(elemTpes)
          app1.cast(resTpe)
        }
      }
    }
  }

  /** Retrieve symbol attached to given tree */
  protected def retrieveSym(tree: untpd.Tree)(using Context): Symbol = tree.removeAttachment(SymOfTree) match {
    case Some(sym) =>
      sym.ensureCompleted()
      sym
    case none =>
      NoSymbol
  }

  protected def localTyper(sym: Symbol): Typer = nestedTyper.remove(sym).get

  def typedUnadapted(initTree: untpd.Tree, pt: Type = WildcardType)(using Context): Tree =
    typedUnadapted(initTree, pt, ctx.typerState.ownedVars)

  /** Typecheck tree without adapting it, returning a typed tree.
   *  @param initTree    the untyped tree
   *  @param pt          the expected result type
   *  @param locked      the set of type variables of the current typer state that cannot be interpolated
   *                     at the present time
   */
  def typedUnadapted(initTree: untpd.Tree, pt: Type, locked: TypeVars)(using Context): Tree = {
    record("typedUnadapted")
    val xtree = expanded(initTree)
    xtree.removeAttachment(TypedAhead) match {
      case Some(ttree) => ttree
      case none =>

        def typedNamed(tree: untpd.NameTree, pt: Type)(using Context): Tree = {
          val sym = retrieveSym(xtree)
          tree match {
            case tree: untpd.Ident => typedIdent(tree, pt)
            case tree: untpd.Select => typedSelect(tree, pt)
            case tree: untpd.Bind => typedBind(tree, pt)
            case tree: untpd.ValDef =>
              if (tree.isEmpty) tpd.EmptyValDef
              else typedValDef(tree, sym)(using ctx.localContext(tree, sym))
            case tree: untpd.DefDef =>
              val typer1 = localTyper(sym)
              typer1.typedDefDef(tree, sym)(using ctx.localContext(tree, sym).setTyper(typer1))
            case tree: untpd.TypeDef =>
              // separate method to keep dispatching method `typedNamed` short which might help the JIT
              def typedTypeOrClassDef: Tree =
                migrate(kindProjectorQMark(tree, sym))
                if tree.isClassDef then
                  typedClassDef(tree, sym.asClass)(using ctx.localContext(tree, sym))
                else
                  typedTypeDef(tree, sym)(using ctx.localContext(tree, sym).setNewScope)

              typedTypeOrClassDef
            case tree: untpd.Labeled => typedLabeled(tree)
            case _ => typedUnadapted(desugar(tree, pt), pt, locked)
          }
        }

        def typedUnnamed(tree: untpd.Tree): Tree = tree match {
          case tree: untpd.Apply =>
            if (ctx.mode is Mode.Pattern) typedUnApply(tree, pt) else typedApply(tree, pt)
          case tree: untpd.This => typedThis(tree)
          case tree: untpd.Number => typedNumber(tree, pt)
          case tree: untpd.Literal => typedLiteral(tree)
          case tree: untpd.New => typedNew(tree, pt)
          case tree: untpd.Typed => typedTyped(tree, pt)
          case tree: untpd.NamedArg => typedNamedArg(tree, pt)
          case tree: untpd.Assign => typedAssign(tree, pt)
          case tree: untpd.Block => typedBlock(desugar.block(tree), pt)(using ctx.fresh.setNewScope)
          case tree: untpd.If => typedIf(tree, pt)
          case tree: untpd.Function => typedFunction(tree, pt)
          case tree: untpd.PolyFunction => typedPolyFunction(tree, pt)
          case tree: untpd.Closure => typedClosure(tree, pt)
          case tree: untpd.Import => typedImport(tree)
          case tree: untpd.Export => typedExport(tree)
          case tree: untpd.Match => typedMatch(tree, pt)
          case tree: untpd.Return => typedReturn(tree)
          case tree: untpd.WhileDo => typedWhileDo(tree)
          case tree: untpd.Try => typedTry(tree, pt)
          case tree: untpd.Throw => typedThrow(tree)
          case tree: untpd.TypeApply => typedTypeApply(tree, pt)
          case tree: untpd.Super => typedSuper(tree, pt)
          case tree: untpd.SeqLiteral => typedSeqLiteral(tree, pt)
          case tree: untpd.Inlined => typedInlined(tree, pt)
          case tree: untpd.TypeTree => typedTypeTree(tree, pt)
          case tree: untpd.SingletonTypeTree => typedSingletonTypeTree(tree)
          case tree: untpd.RefinedTypeTree => typedRefinedTypeTree(tree)
          case tree: untpd.AppliedTypeTree => typedAppliedTypeTree(tree)
          case tree: untpd.LambdaTypeTree => typedLambdaTypeTree(tree)(using ctx.localContext(tree, NoSymbol).setNewScope)
          case tree: untpd.TermLambdaTypeTree => typedTermLambdaTypeTree(tree)(using ctx.localContext(tree, NoSymbol).setNewScope)
          case tree: untpd.MatchTypeTree => typedMatchTypeTree(tree, pt)
          case tree: untpd.ByNameTypeTree => typedByNameTypeTree(tree)
          case tree: untpd.TypeBoundsTree => typedTypeBoundsTree(tree, pt)
          case tree: untpd.Alternative => typedAlternative(tree, pt)
          case tree: untpd.PackageDef => typedPackageDef(tree)
          case tree: untpd.Annotated => typedAnnotated(tree, pt)
          case tree: untpd.TypedSplice => typedTypedSplice(tree)
          case tree: untpd.UnApply => typedUnApply(tree, pt)
          case tree: untpd.Tuple => typedTuple(tree, pt)
          case tree: untpd.InLambdaTypeTree => typedInLambdaTypeTree(tree, pt)
          case tree: untpd.InfixOp => typedInfixOp(tree, pt)
          case tree: untpd.ParsedTry => typedTry(tree, pt)
          case tree @ untpd.PostfixOp(qual, Ident(nme.WILDCARD)) => typedAsFunction(tree, pt)
          case untpd.EmptyTree => tpd.EmptyTree
          case tree: untpd.Quote => typedQuote(tree, pt)
          case tree: untpd.Splice => typedSplice(tree, pt)
          case tree: untpd.QuotePattern => typedQuotePattern(tree, pt)
          case tree: untpd.SplicePattern => typedSplicePattern(tree, pt)
          case tree: untpd.MacroTree => report.error("Unexpected macro", tree.srcPos); tpd.nullLiteral  // ill-formed code may reach here
          case tree: untpd.Hole => typedHole(tree, pt)
          case _ => typedUnadapted(desugar(tree, pt), pt, locked)
        }

        def handleTypeError(ex: TypeError): Tree = ex match
          case ex: CyclicReference
          if ctx.reporter.errorsReported
              && xtree.span.isZeroExtent
              && ex.isVal =>
            // Don't report a "recursive val ... needs type" if errors were reported
            // previously and the span of the offending tree is empty. In this case,
            // it's most likely that this is desugared code, and the error message would
            // be redundant and confusing.
            xtree.withType(ErrorType(ex.toMessage))
          case _ =>
            // Use focussed sourcePos since tree might be a large definition
            // and a large error span would hide all errors in interior.
            // TODO: Not clear that hiding is what we want, actually
            errorTree(xtree, ex, xtree.srcPos.focus)

        try
          val ifpt = defn.asContextFunctionType(pt)
          val result =
            if ifpt.exists
              && defn.functionArity(ifpt) > 0 // ContextFunction0 is only used after ElimByName
              && xtree.isTerm
              && !untpd.isContextualClosure(xtree)
              && !ctx.mode.is(Mode.Pattern)
              && !xtree.isInstanceOf[SplicePattern]
              && !ctx.isAfterTyper
              && !ctx.isInlineContext
            then
              makeContextualFunction(xtree, ifpt)
            else xtree match
              case xtree: untpd.NameTree => typedNamed(xtree, pt)
              case xtree => typedUnnamed(xtree)

          val unsimplifiedType = result.tpe
          simplify(result, pt, locked)
          result.tpe.stripTypeVar match
            case e: ErrorType if !unsimplifiedType.isErroneous => errorTree(xtree, e.msg, xtree.srcPos)
            case _ => result
        catch case ex: TypeError =>
          handleTypeError(ex)
     }
  }

  /** Interpolate and simplify the type of the given tree. */
  protected def simplify(tree: Tree, pt: Type, locked: TypeVars)(using Context): tree.type =
    if !tree.denot.isOverloaded then // for overloaded trees: resolve overloading before simplifying
      if !tree.tpe.widen.isInstanceOf[MethodOrPoly] // wait with simplifying until method is fully applied
         || tree.isDef                              // ... unless tree is a definition
      then
        interpolateTypeVars(tree, pt, locked)
        val simplified = tree.tpe.simplified
        if !MatchType.thatReducesUsingGadt(tree.tpe) then // needs a GADT cast. i15743
          tree.overwriteType(simplified)
    tree

  protected def makeContextualFunction(tree: untpd.Tree, pt: Type)(using Context): Tree = {
    val defn.FunctionOf(formals, _, true) = pt.dropDependentRefinement: @unchecked
    val paramNamesOrNil = pt match
      case RefinedType(_, _, rinfo: MethodType) => rinfo.paramNames
      case _ => Nil

    // The getter of default parameters may reach here.
    // Given the code below
    //
    //     class Foo[A](run: A ?=> Int) {
    //        def foo[T](f: T ?=> Int = run) = ()
    //     }
    //
    // it desugars to
    //
    //     class Foo[A](run: A ?=> Int) {
    //        def foo$default$1[T] = run
    //        def foo[T](f: T ?=> Int = run) = ()
    //     }
    //
    // The expected type for checking `run` in `foo$default$1` is
    //
    //      <?> ?=> Int
    //
    // see tests/pos/i7778b.scala

    val paramTypes = {
      val hasWildcard = formals.exists(_.existsPart(_.isInstanceOf[WildcardType], StopAt.Static))
      if hasWildcard then formals.map(_ => untpd.TypeTree())
      else formals.map(formal => untpd.TypeTree(formal.loBound)) // about loBound, see tests/pos/i18649.scala
    }

    val erasedParams = pt match {
      case defn.PolyFunctionOf(mt: MethodType) => mt.erasedParams
      case _ => paramTypes.map(_ => false)
    }

    val ifun = desugar.makeContextualFunction(paramTypes, paramNamesOrNil, tree, erasedParams)
    typr.println(i"make contextual function $tree / $pt ---> $ifun")
    typedFunctionValue(ifun, pt)
  }

  /** Typecheck and adapt tree, returning a typed tree. Parameters as for `typedUnadapted` */
  def typed(tree: untpd.Tree, pt: Type, locked: TypeVars)(using Context): Tree =
    trace(i"typing $tree, pt = $pt", typr, show = true) {
      record(s"typed $getClass")
      record("typed total")
      if ctx.phase.isTyper then
        assertPositioned(tree)
      if tree.source != ctx.source && tree.source.exists then
        typed(tree, pt, locked)(using ctx.withSource(tree.source))
      else if ctx.run.nn.isCancelled then
        tree.withType(WildcardType)
      else adapt(typedUnadapted(tree, pt, locked), pt, locked)
    }

  def typed(tree: untpd.Tree, pt: Type = WildcardType)(using Context): Tree =
    typed(tree, pt, ctx.typerState.ownedVars)

  def typedTrees(trees: List[untpd.Tree])(using Context): List[Tree] =
    trees mapconserve (typed(_))

  def typedStats(stats: List[untpd.Tree], exprOwner: Symbol)(using Context): (List[Tree], Context) = {
    val buf = new mutable.ListBuffer[Tree]
    var enumContexts: SimpleIdentityMap[Symbol, Context] = SimpleIdentityMap.empty
    val initialNotNullInfos = ctx.notNullInfos
      // A map from `enum` symbols to the contexts enclosing their definitions
    @tailrec def traverse(stats: List[untpd.Tree])(using Context): (List[Tree], Context) = stats match {
      case (imp: untpd.Import) :: rest =>
        val imp1 = typed(imp)
        buf += imp1
        traverse(rest)(using ctx.importContext(imp, imp1.symbol))
      case (mdef: untpd.DefTree) :: rest =>
        mdef.removeAttachment(ExpandedTree) match {
          case Some(xtree) =>
            traverse(xtree :: rest)
          case none =>
            val newCtx = if (ctx.owner.isTerm && adaptCreationContext(mdef)) ctx
              else ctx.withNotNullInfos(initialNotNullInfos)
            typed(mdef)(using newCtx) match {
              case mdef1: DefDef
              if mdef1.symbol.is(Inline, butNot = Deferred) && !Inlines.bodyToInline(mdef1.symbol).isEmpty =>
                buf ++= inlineExpansion(mdef1)
                  // replace body with expansion, because it will be used as inlined body
                  // from separately compiled files - the original BodyAnnotation is not kept.
              case mdef1: TypeDef if mdef1.symbol.is(Enum, butNot = Case) =>
                enumContexts = enumContexts.updated(mdef1.symbol, ctx)
                buf += mdef1
              case EmptyTree =>
                // clashing synthetic case methods are converted to empty trees, drop them here
              case mdef1 =>
                buf += mdef1
            }
            traverse(rest)
        }
      case Thicket(stats) :: rest =>
        traverse(stats ::: rest)
      case (stat: untpd.Export) :: rest =>
        buf +=  typed(stat)
        buf ++= stat.attachmentOrElse(ExportForwarders, Nil)
          // no attachment can happen in case of cyclic references
        traverse(rest)
      case (stat: untpd.ExtMethods) :: rest =>
        val xtree = stat.removeAttachment(ExpandedTree).get
        traverse(xtree :: rest)
      case stat :: rest =>
        val stat1 = typed(stat)(using ctx.exprContext(stat, exprOwner))
        if !Linter.warnOnInterestingResultInStatement(stat1) then checkStatementPurity(stat1)(stat, exprOwner)
        buf += stat1
        traverse(rest)(using stat1.nullableContext)
      case nil =>
        (buf.toList, ctx)
    }
    def finalize(stat: Tree)(using Context): Tree = stat match {
      case stat: TypeDef if stat.symbol.is(Module) =>
        val enumContext = enumContexts(stat.symbol.linkedClass)
        if enumContext != null then
          checkEnumCaseRefsLegal(stat, enumContext)
        stat.removeAttachment(AttachedDeriver) match {
          case Some(deriver) => deriver.finalize(stat)
          case None => stat
        }
      case _ =>
        stat
    }
    val (stats0, finalCtx) = traverse(stats)
    val stats1 = stats0.mapConserve(finalize)
    if ctx.owner == exprOwner then checkNoTargetNameConflict(stats1)
    (stats1, finalCtx)
  }

  /** Tries to adapt NotNullInfos from creation context to the DefTree,
   *  returns whether the adaption took place. An adaption only takes place if the
   *  DefTree has a symbol and it has not been completed (is not forward referenced).
   */
  def adaptCreationContext(mdef: untpd.DefTree)(using Context): Boolean =
    // Keep preceding not null facts in the current context only if `mdef`
    // cannot be executed out-of-sequence.
    // We have to check the Completer of symbol befor typedValDef,
    // otherwise the symbol is already completed using creation context.
    mdef.getAttachment(SymOfTree) match {
      case Some(sym) => sym.infoOrCompleter match {
        case completer: Namer#Completer =>
          if completer.creationContext.notNullInfos ne ctx.notNullInfos then
            // The RHS of a val def should know about not null facts established
            // in preceding statements (unless the DefTree is completed ahead of time,
            // then it is impossible).
            completer.setNotNullInfos(ctx.notNullInfos)
          true
        case _ =>
          // If it has been completed, then it must be because there is a forward reference
          // to the definition in the program. Hence, we don't Keep preceding not null facts
          // in the current context.
          false
      }
      case _ => false
    }

  /** Given an inline method `mdef`, the method rewritten so that its body
   *  uses accessors to access non-public members. Also, if the inline method
   *  is retained, add a method to record the retained version of the body.
   *  Overwritten in Retyper to return `mdef` unchanged.
   */
  protected def inlineExpansion(mdef: DefDef)(using Context): List[Tree] =
    tpd.cpy.DefDef(mdef)(rhs = Inlines.bodyToInline(mdef.symbol))
    :: (if mdef.symbol.isRetainedInlineMethod then Inlines.bodyRetainer(mdef) :: Nil else Nil)

  def typedExpr(tree: untpd.Tree, pt: Type = WildcardType)(using Context): Tree =
    withoutMode(Mode.PatternOrTypeBits)(typed(tree, pt))

  def typedType(tree: untpd.Tree, pt: Type = WildcardType, mapPatternBounds: Boolean = false)(using Context): Tree =
    val tree1 = withMode(Mode.Type) { typed(tree, pt) }
    if mapPatternBounds && ctx.mode.is(Mode.Pattern) && !ctx.isAfterTyper then
      tree1 match
        case tree1: TypeBoundsTree =>
          // Associate a pattern-bound type symbol with the wildcard.
          // The bounds of the type symbol can be constrained when comparing a pattern type
          // with an expected type in typedTyped. The type symbol and the defining Bind node
          // are eliminated once the enclosing pattern has been typechecked; see `indexPattern`
          // in `typedCase`.
          val boundName = WildcardParamName.fresh().toTypeName
          val wildcardSym = newPatternBoundSymbol(boundName, tree1.tpe & pt, tree.span)
          untpd.Bind(boundName, tree1).withType(wildcardSym.typeRef)
        case tree1 =>
          tree1
    else tree1

  def typedPattern(tree: untpd.Tree, selType: Type = WildcardType)(using Context): Tree =
    withMode(Mode.Pattern)(typed(tree, selType))

  def tryEither[T](op: Context ?=> T)(fallBack: (T, TyperState) => T)(using Context): T = {
    val nestedCtx = ctx.fresh.setNewTyperState()
    val result = op(using nestedCtx)
    if (nestedCtx.reporter.hasErrors && !nestedCtx.reporter.hasStickyErrors) {
      record("tryEither.fallBack")
      fallBack(result, nestedCtx.typerState)
    }
    else {
      record("tryEither.commit")
      nestedCtx.typerState.commit()
      result
    }
  }

  /** Try `op1`, if there are errors, try `op2`, if `op2` also causes errors, fall back
   *  to errors and result of `op1`.
   */
  def tryAlternatively[T](op1: Context ?=> T)(op2: Context ?=> T)(using Context): T =
    tryEither(op1) { (failedVal, failedState) =>
      tryEither(op2) { (_, _) =>
        failedState.commit()
        failedVal
      }
    }

  /** Is `pt` a prototype of an `apply` selection, or a parameterless function yielding one? */
  def isApplyProto(pt: Type)(using Context): Boolean = pt.revealIgnored match {
    case pt: SelectionProto => pt.name == nme.apply
    case pt: FunProto       => pt.args.isEmpty && isApplyProto(pt.resultType)
    case _                  => false
  }

  /** Potentially add apply node or implicit conversions. Before trying either,
   *  if the function is applied to an empty parameter list (), we try
   *
   *  0th strategy: If `tree` overrides a nullary method, mark the prototype
   *                so that the argument is dropped and return `tree` itself.
   *                (but do this at most once per tree).
   *
   *  After that, two strategies are tried, and the first that is successful is picked.
   *
   *  1st strategy: Try to insert `.apply` so that the result conforms to prototype `pt`.
   *                This strategy is not tried if the prototype represents already
   *                another `.apply` or `.apply()` selection.
   *
   *  2nd strategy: If tree is a select `qual.name`, try to insert an implicit conversion
   *    around the qualifier part `qual` so that the result conforms to the expected type
   *    with wildcard result type.
   *
   *  If neither of the strategies are successful, continues with the `apply` result
   *  if an apply insertion was tried and `tree` has an `apply` method, or continues
   *  with `fallBack` otherwise. `fallBack` is supposed to always give an error.
   */
  def tryInsertApplyOrImplicit(tree: Tree, pt: ProtoType, locked: TypeVars)(fallBack: => Tree)(using Context): Tree = {
    def isMethod(tree: Tree) = tree.tpe match {
      case ref: TermRef => ref.denot.alternatives.forall(_.info.widen.isInstanceOf[MethodicType])
      case _ => false
    }

    var assumeApplyExists = false
      // if true, issue any errors about the apply instead of `fallBack`,
      // since they are more likely to be informative.

    def tryApply(using Context) = {
      val pt1 = pt.withContext(ctx)
      val sel = typedSelect(untpd.Select(untpd.TypedSplice(tree), nme.apply), pt1)
        .withAttachment(InsertedApply, ())
      if sel.tpe.isError then
        // assume the apply exists if qualifier has a hidden search failure of type
        // FailedExtension or NestedFailure
        sel match
          case Select(qual, _) =>
            assumeApplyExists = qual.getAttachment(Typer.HiddenSearchFailure).exists(
              _.exists(_.reason.isInstanceOf[FailedExtension | NestedFailure]))
          case _ =>
        sel
      else
        assumeApplyExists = true
        try adapt(simplify(sel, pt1, locked), pt1, locked) finally sel.removeAttachment(InsertedApply)
    }

    def tryImplicit(fallBack: => Tree) =
      tryInsertImplicitOnQualifier(tree, pt.withContext(ctx), locked).getOrElse(fallBack)

    if (ctx.mode.is(Mode.SynthesizeExtMethodReceiver))
      // Suppress insertion of apply or implicit conversion on extension method receiver
      tree
    else pt match {
      case pt @ FunProto(Nil, _)
      if tree.symbol.allOverriddenSymbols.exists(_.info.isNullaryMethod) &&
         !tree.hasAttachment(DroppedEmptyArgs) =>
        tree.putAttachment(DroppedEmptyArgs, ())
        pt.markAsDropped()
        tree
      case _ =>
        if (isApplyProto(pt) || isMethod(tree) || isSyntheticApply(tree)) tryImplicit(fallBack)
        else tryEither(tryApply) { (app, appState) =>
          tryImplicit {
            if assumeApplyExists then
              appState.commit()
              app
            else fallBack
          }
        }
    }
  }

  /** If this tree is a select node `qual.name` (possibly applied to type variables)
   *  that does not conform to `pt`, try two mitigations:
   *   1. Instantiate any TypeVars in the widened type of `tree` with their lower bounds.
   *   2. Try to insert an implicit conversion `c` around `qual` so that
   *   `c(qual).name` conforms to `pt`.
   */
  def tryInsertImplicitOnQualifier(tree: Tree, pt: Type, locked: TypeVars)(using Context): Option[Tree] = trace(i"try insert impl on qualifier $tree $pt") {
    tree match
      case tree @ Select(qual, name) if name != nme.CONSTRUCTOR =>
        if couldInstantiateTypeVar(qual.tpe.widen, applied = true)
        then
          Some(adapt(tree, pt, locked))
        else
          val selProto = SelectionProto(name, pt, NoViewsAllowed, privateOK = false, tree.nameSpan)
          if selProto.isMatchedBy(qual.tpe) || tree.hasAttachment(InsertedImplicitOnQualifier) then
            None
          else
            tryEither {
              val tree1 = tryExtensionOrConversion(tree, pt, pt, qual, locked, NoViewsAllowed, inSelect = false)
              if tree1.isEmpty then None
              else
                tree1.putAttachment(InsertedImplicitOnQualifier, ())
                Some(adapt(tree1, pt, locked))
            } { (_, _) => None
            }
      case TypeApply(fn, args) if args.forall(_.isInstanceOf[untpd.InferredTypeTree]) =>
        tryInsertImplicitOnQualifier(fn, pt, locked)
      case _ => None
  }

  /** Given a selection `qual.name`, try to convert to an extension method
   *  application `name(qual)` or insert an implicit conversion `c(qual).name`.
   *  @return The converted tree, or `EmptyTree` is not successful.
   */
  def tryExtensionOrConversion
      (tree: untpd.Select, pt: Type, mbrProto: Type, qual: Tree, locked: TypeVars, compat: Compatibility, inSelect: Boolean)
      (using Context): Tree =

    def selectionProto = SelectionProto(tree.name, mbrProto, compat, privateOK = inSelect, tree.nameSpan)

    def tryExtension(using Context): Tree =
      val altImports = new mutable.ListBuffer[TermRef]()
      findRef(tree.name, WildcardType, ExtensionMethod, EmptyFlags, qual.srcPos, altImports) match
        case ref: TermRef =>
          def tryExtMethod(ref: TermRef)(using Context) =
            extMethodApply(untpd.TypedSplice(tpd.ref(ref).withSpan(tree.nameSpan)), qual, pt)
          if altImports.isEmpty then
            tryExtMethod(ref)
          else
            // Try all possible imports and collect successes and failures
            val successes, failures = new mutable.ListBuffer[(Tree, TyperState)]
            for alt <- ref :: altImports.toList do
              val nestedCtx = ctx.fresh.setNewTyperState()
              val app = tryExtMethod(alt)(using nestedCtx)
              (if nestedCtx.reporter.hasErrors then failures else successes)
                += ((app, nestedCtx.typerState))
            typr.println(i"multiple extension methods, success: ${successes.toList}, failure: ${failures.toList}")

            def pick(alt: (Tree, TyperState)): Tree =
              val (app, ts) = alt
              ts.commit()
              app

            successes.toList match
              case Nil => pick(failures.head)
              case success :: Nil => pick(success)
              case (expansion1, _) :: (expansion2, _) :: _ =>
                report.error(AmbiguousExtensionMethod(tree, expansion1, expansion2), tree.srcPos)
                expansion1
        case _ =>
          EmptyTree
    end tryExtension

    def nestedFailure(ex: TypeError) =
      rememberSearchFailure(qual,
        SearchFailure(qual.withType(NestedFailure(ex.toMessage, selectionProto))))

    if qual.symbol.isNoValue then return EmptyTree

    // try an extension method in scope
    try
      val nestedCtx = ctx.fresh.setNewTyperState()
      val app = tryExtension(using nestedCtx)
      if !app.isEmpty && !nestedCtx.reporter.hasErrors then
        nestedCtx.typerState.commit()
        return app
      val errs = nestedCtx.reporter.allErrors
      val remembered = // report AmbiguousReferences as priority, otherwise last error
        (errs.filter(_.msg.isInstanceOf[AmbiguousReference]) ++ errs).take(1)
      for err <- remembered do
        val tree = if app.isEmpty then qual else app
        rememberSearchFailure(qual,
          SearchFailure(tree.withType(FailedExtension(tree, selectionProto, err.msg))))
    catch case ex: TypeError => nestedFailure(ex)

    // try an implicit conversion or given extension
    if ctx.mode.is(Mode.ImplicitsEnabled) && !tree.name.isConstructorName && qual.tpe.isValueType then
      try
        val selProto = selectionProto
        trace(i"try insert impl on qualifier $tree $pt") { inferView(qual, selProto) } match
          case SearchSuccess(found, _, _, isExtension) =>
            if isExtension then return found
            else
              checkImplicitConversionUseOK(found, selProto)
              return withoutMode(Mode.ImplicitsEnabled)(typedSelect(tree, pt, found))
          case failure: SearchFailure =>
            if failure.isAmbiguous then
              return
                if !inSelect // in a selection we will do the canDefineFurther afterwards
                    && canDefineFurther(qual.tpe.widen)
                then
                  tryExtensionOrConversion(tree, pt, mbrProto, qual, locked, compat, inSelect)
                else
                  err.typeMismatch(qual, selProto, failure.reason) // TODO: report NotAMember instead, but need to be aware of failure
            rememberSearchFailure(qual, failure)
      catch case ex: TypeError => nestedFailure(ex)

    EmptyTree
  end tryExtensionOrConversion

  /** Perform the following adaptations of expression, pattern or type `tree` wrt to
   *  given prototype `pt`:
   *  (1) Resolve overloading
   *  (2) Apply parameterless functions
   *  (3) Apply polymorphic types to fresh instances of their type parameters and
   *      store these instances in context.undetparams,
   *      unless followed by explicit type application.
   *  (4) Do the following to unapplied methods used as values:
   *  (4.1) If the method has only implicit parameters pass implicit arguments
   *  (4.2) otherwise, if `pt` is a function type and method is not a constructor,
   *        convert to function by eta-expansion,
   *  (4.3) otherwise, if the method is nullary with a result type compatible to `pt`
   *        and it is not a constructor, apply it to ()
   *  otherwise issue an error
   *  (5) Convert constructors in a pattern as follows:
   *  (5.1) If constructor refers to a case class factory, set tree's type to the unique
   *        instance of its primary constructor that is a subtype of the expected type.
   *  (5.2) If constructor refers to an extractor, convert to application of
   *        unapply or unapplySeq method.
   *
   *  (6) Convert all other types to TypeTree nodes.
   *  (7) When in TYPEmode but not FUNmode or HKmode, check that types are fully parameterized
   *      (7.1) In HKmode, higher-kinded types are allowed, but they must have the expected kind-arity
   *  (8) When in both EXPRmode and FUNmode, add apply method calls to values of object type.
   *  (9) If there are undetermined type variables and not POLYmode, infer expression instance
   *  Then, if tree's type is not a subtype of expected type, try the following adaptations:
   *  (10) If the expected type is Byte, Short or Char, and the expression
   *      is an integer fitting in the range of that type, convert it to that type.
   *  (11) Widen numeric literals to their expected type, if necessary
   *  (12) When in mode EXPRmode, convert E to { E; () } if expected type is scala.Unit.
   *  (13) When in mode EXPRmode, apply AnnotationChecker conversion if expected type is annotated.
   *  (14) When in mode EXPRmode, apply a view
   *  If all this fails, error
   *  Parameters as for `typedUnadapted`.
   */
  def adapt(tree: Tree, pt: Type, locked: TypeVars)(using Context): Tree =
    try
      trace(i"adapting $tree to $pt", typr, show = true) {
        record("adapt")
        adapt1(tree, pt, locked)
      }
    catch case ex: TypeError => errorTree(tree, ex, tree.srcPos.focus)

  final def adapt(tree: Tree, pt: Type)(using Context): Tree =
    adapt(tree, pt, ctx.typerState.ownedVars)

  private def adapt1(tree: Tree, pt: Type, locked: TypeVars)(using Context): Tree = {
    assert(pt.exists && !pt.isInstanceOf[ExprType] || ctx.reporter.errorsReported, i"tree: $tree, pt: $pt")
    def methodStr = err.refStr(methPart(tree).tpe)

    def readapt(tree: Tree)(using Context) = adapt(tree, pt, locked)
    def readaptSimplified(tree: Tree)(using Context) = readapt(simplify(tree, pt, locked))

    def missingArgs(mt: MethodType) =
      ErrorReporting.missingArgs(tree, mt)
      tree.withType(mt.resultType)

    def adaptOverloaded(ref: TermRef) =
      // get all the alternatives
      val altDenots =
        val allDenots = ref.denot.alternatives
        if pt.isExtensionApplyProto then allDenots.filter(_.symbol.is(ExtensionMethod))
        else allDenots

      typr.println(i"adapt overloaded $ref with alternatives ${altDenots map (_.info)}%\n\n %")

      /** Search for an alternative that does not take parameters.
       * If there is one, return it, otherwise emit an error.
       */
      def tryParameterless(alts: List[TermRef])(error: => tpd.Tree): Tree =
        alts.filter(_.info.isParameterless) match
          case alt :: Nil => readaptSimplified(tree.withType(alt))
          case _ =>
            if altDenots.exists(_.info.paramInfoss == ListOfNil) then
              typed(untpd.Apply(untpd.TypedSplice(tree), Nil), pt, locked)
            else
              error

      def altRef(alt: SingleDenotation) = TermRef(ref.prefix, ref.name, alt)
      val alts = altDenots.map(altRef)

      resolveOverloaded(alts, pt) match
        case alt :: Nil =>
          readaptSimplified(tree.withType(alt))
        case Nil =>
          // If no alternative matches, there are still two ways to recover:
          //  1. If context is an application, try to insert an apply or implicit
          //  2. If context is not an application, pick a alternative that does
          //     not take parameters.

          def errorNoMatch = errorTree(tree, NoMatchingOverload(altDenots, pt))

          pt match
            case pt: FunOrPolyProto if pt.applyKind != ApplyKind.Using =>
              // insert apply or convert qualifier, but only for a regular application
              tryInsertApplyOrImplicit(tree, pt, locked)(errorNoMatch)
            case _ =>
              tryParameterless(alts)(errorNoMatch)

        case ambiAlts =>
          // If there are ambiguous alternatives, and:
          // 1. the types aren't erroneous
          // 2. the expected type is not a function type
          // 3. there exist a parameterless alternative
          //
          // Then, pick the parameterless alternative.
          // See tests/pos/i10715-scala and tests/pos/i10715-java.

          /** Constructs an "ambiguous overload" error */
          def errorAmbiguous =
            val remainingDenots = altDenots.filter(denot => ambiAlts.contains(altRef(denot)))
            val addendum =
              if ambiAlts.exists(!_.symbol.exists) then
                i"""|
                    |
                    |Note: Overloaded definitions introduced by refinements cannot be resolved"""
              else ""
            errorTree(tree, AmbiguousOverload(tree, remainingDenots, pt, addendum))
          end errorAmbiguous

          if tree.tpe.isErroneous || pt.isErroneous then
            tree.withType(UnspecifiedErrorType)
          else
            pt match
              case _: FunProto =>
                errorAmbiguous
              case _  =>
                tryParameterless(alts)(errorAmbiguous)

      end match
    end adaptOverloaded

    def adaptToArgs(wtp: Type, pt: FunProto): Tree = wtp match {
      case wtp: MethodOrPoly =>
        def methodStr = methPart(tree).symbol.showLocated
        if matchingApply(wtp, pt) then
          migrate(contextBoundParams(tree, wtp, pt))
          if needsTupledDual(wtp, pt) then adapt(tree, pt.tupledDual, locked)
          else tree
        else if wtp.isContextualMethod then
          adaptNoArgs(wtp)  // insert arguments implicitly
        else if (tree.symbol.isPrimaryConstructor && tree.symbol.info.firstParamTypes.isEmpty)
          readapt(tree.appliedToNone) // insert () to primary constructors
        else
          errorTree(tree, em"Missing arguments for $methodStr")
      case _ => tryInsertApplyOrImplicit(tree, pt, locked) {
        errorTree(tree, MethodDoesNotTakeParameters(tree))
      }
    }

    def adaptNoArgsImplicitMethod(wtp: MethodType): Tree = {
      assert(wtp.isImplicitMethod)
      val tvarsToInstantiate = tvarsInParams(tree, locked).distinct
      def instantiate(tp: Type): Unit = {
        instantiateSelected(tp, tvarsToInstantiate)
        replaceSingletons(tp)
      }
      wtp.paramInfos.foreach(instantiate)
      val saved = ctx.typerState.snapshot()

      def dummyArg(tp: Type) = untpd.Ident(nme.???).withTypeUnchecked(tp)

      def addImplicitArgs(using Context) = {
        def hasDefaultParams = methPart(tree).symbol.hasDefaultParams
        def implicitArgs(formals: List[Type], argIndex: Int, pt: Type): List[Tree] = formals match
          case Nil => Nil
          case formal :: formals1 =>
            // If the implicit parameter list is dependent we must propagate inferred
            // types through the remainder of the parameter list similarly to how it's
            // done for non-implicit parameter lists in Applications#matchArgs#addTyped.
            def inferArgsAfter(leading: Tree) =
              val formals2 =
                if wtp.isParamDependent && leading.tpe.exists then
                  formals1.mapconserve(f1 => safeSubstParam(f1, wtp.paramRefs(argIndex), leading.tpe))
                else formals1
              implicitArgs(formals2, argIndex + 1, pt)

            val arg = inferImplicitArg(formal, tree.span.endPos)
            arg.tpe match
              case failed: AmbiguousImplicits =>
                val pt1 = pt.deepenProtoTrans
                if (pt1 `ne` pt) && (pt1 ne sharpenedPt) && constrainResult(tree.symbol, wtp, pt1)
                then implicitArgs(formals, argIndex, pt1)
                else arg :: implicitArgs(formals1, argIndex + 1, pt1)
              case failed: SearchFailureType =>
                lazy val defaultArg =
                  def appPart(t: Tree): Tree = t match
                    case Block(stats, expr) => appPart(expr)
                    case Inlined(_, _, expr) => appPart(expr)
                    case _ => t
                  defaultArgument(appPart(tree), argIndex, testOnly = false)
                    .showing(i"default argument: for $formal, $tree, $argIndex = $result", typr)
                if !hasDefaultParams || defaultArg.isEmpty then
                  // no need to search further, the adapt fails in any case
                  // the reason why we continue inferring arguments in case of an AmbiguousImplicits
                  // is that we need to know whether there are further errors.
                  // If there are none, we have to propagate the ambiguity to the caller.
                  arg :: formals1.map(dummyArg)
                else
                  // This is tricky. On the one hand, we need the defaultArg to
                  // correctly type subsequent formal parameters in the same using
                  // clause in case there are parameter dependencies. On the other hand,
                  // we cannot simply use `defaultArg` as inferred argument since some parts
                  // of it might need lifting out. What we do instead is to use `defaultArg` for
                  // computing parameter-dependent formals but leave the original erroneous
                  // `arg` in place. We come back to this later in the code conditioned by
                  // `if propFail.exists` where we re-type the whole using clause with named
                  // arguments for all implicits that were found.
                  arg :: inferArgsAfter(defaultArg)
              case _ =>
                arg :: inferArgsAfter(arg)
        end implicitArgs

        val args = implicitArgs(wtp.paramInfos, 0, pt)

        def propagatedFailure(args: List[Tree]): Type = args match {
          case arg :: args1 =>
            arg.tpe match {
              case ambi: AmbiguousImplicits =>
                propagatedFailure(args1) match {
                  case NoType | (_: AmbiguousImplicits) => ambi
                  case failed => failed
                }
              case failed: SearchFailureType => failed
              case _ => propagatedFailure(args1)
            }
          case Nil => NoType
        }

        val propFail = propagatedFailure(args)

        def issueErrors(): Tree = {
          def paramSymWithMethodTree(paramName: TermName) =
            if tree.symbol.exists then
              tree.symbol.paramSymss.flatten
                .map(sym => sym.name -> sym)
                .toMap
                .get(paramName)
                .map((_, tree))
            else
              None

          wtp.paramNames.lazyZip(wtp.paramInfos).lazyZip(args).foreach { (paramName, formal, arg) =>
            arg.tpe match {
              case failure: SearchFailureType =>
                report.error(
                  missingArgMsg(arg, formal, implicitParamString(paramName, methodStr, tree), paramSymWithMethodTree(paramName)),
                  tree.srcPos.endPos
                )
              case _ =>
            }
          }
          untpd.Apply(tree, args).withType(propFail)
        }

        if (propFail.exists) {
          // If there are several arguments, some arguments might already
          // have influenced the context, binding variables, but later ones
          // might fail. In that case the constraint and instantiated variables
          // need to be reset.
          ctx.typerState.resetTo(saved)

          // If method has default params, fall back to regular application
          // where all inferred implicits are passed as named args.
          if hasDefaultParams && !propFail.isInstanceOf[AmbiguousImplicits] then
            val namedArgs = wtp.paramNames.lazyZip(args).flatMap { (pname, arg) =>
              if (arg.tpe.isError) Nil else untpd.NamedArg(pname, untpd.TypedSplice(arg)) :: Nil
            }
            val app = cpy.Apply(tree)(untpd.TypedSplice(tree), namedArgs)
            val needsUsing = wtp.isContextualMethod || wtp.match
              case MethodType(ContextBoundParamName(_) :: _) => sourceVersion.isAtLeast(`3.4`)
              case _ => false
            if needsUsing then app.setApplyKind(ApplyKind.Using)
            typr.println(i"try with default implicit args $app")
            typed(app, pt, locked)
          else issueErrors()
        }
        else tree match {
          case tree: Block =>
            readaptSimplified(tpd.Block(tree.stats, tpd.Apply(tree.expr, args)))
          case tree: NamedArg =>
            readaptSimplified(tpd.NamedArg(tree.name, tpd.Apply(tree.arg, args)))
          case _ =>
            readaptSimplified(tpd.Apply(tree, args))
        }
      }
      pt.revealIgnored match {
        case pt: FunProto if pt.applyKind == ApplyKind.Using =>
          // We can end up here if extension methods are called with explicit given arguments.
          // See for instance #7119.
          tree
        case _ =>
          addImplicitArgs(using argCtx(tree))
      }
    }

    /** A synthetic apply should be eta-expanded if it is the apply of an implicit function
      *  class, and the expected type is a function type. This rule is needed so we can pass
      *  an implicit function to a regular function type. So the following is OK
      *
      *     val f: implicit A => B  =  ???
      *     val g: A => B = f
      *
      *  and the last line expands to
      *
      *     val g: A => B  =  (x$0: A) => f.apply(x$0)
      *
      *  One could be tempted not to eta expand the rhs, but that would violate the invariant
      *  that expressions of implicit function types are always implicit closures, which is
      *  exploited by ShortcutImplicits.
      *
      *  On the other hand, the following would give an error if there is no implicit
      *  instance of A available.
      *
      *     val x: AnyRef = f
      *
      *  That's intentional, we want to fail here, otherwise some unsuccessful implicit searches
      *  would go undetected.
      *
      *  Examples for these cases are found in run/implicitFuns.scala and neg/i2006.scala.
      */
    def adaptNoArgsUnappliedMethod(wtp: MethodType, functionExpected: Boolean, arity: Int): Tree = {
      /** Is reference to this symbol `f` automatically expanded to `f()`? */
      def isAutoApplied(sym: Symbol): Boolean =
        lazy val msg = MissingEmptyArgumentList(sym.show, tree)

        sym.isConstructor
        || sym.matchNullaryLoosely
        || Feature.warnOnMigration(msg, tree.srcPos, version = `3.0`)
          && {
            msg.actions
              .headOption
              .foreach(Rewrites.applyAction)
            true
          }

      /** If this is a selection prototype of the form `.apply(...): R`, return the nested
       *  function prototype `(...)R`. Otherwise `pt`.
       */
      def ptWithoutRedundantApply: Type = pt.revealIgnored match
        case SelectionProto(nme.apply, mpt, _, _, _) =>
          mpt.revealIgnored match
            case fpt: FunProto => fpt
            case _ => pt
        case _ => pt

      // Reasons NOT to eta expand:
      //  - we reference a constructor
      //  - we reference an inline implicit def (see #19862)
      //  - we are in a pattern
      //  - the current tree is a synthetic apply which is not expandable (eta-expasion would simply undo that)
      if arity >= 0
         && !tree.symbol.isConstructor
         && !tree.symbol.isAllOf(InlineImplicitMethod)
         && !ctx.mode.is(Mode.Pattern)
         && !(isSyntheticApply(tree) && !functionExpected)
      then
        val pt1 = ptWithoutRedundantApply
        if pt1 ne pt then
          // Ignore `.apply` in `m.apply(...)`; it will later be simplified in typedSelect to `m(...)`
          adapt1(tree, pt1, locked)
        else
          if (!defn.isFunctionNType(pt))
            pt match {
              case SAMType(_, samParent) if !pt1.classSymbol.hasAnnotation(defn.FunctionalInterfaceAnnot) =>
                report.warning(em"${tree.symbol} is eta-expanded even though $samParent does not have the @FunctionalInterface annotation.", tree.srcPos)
              case _ =>
            }
          simplify(typed(etaExpand(tree, wtp, arity), pt), pt, locked)
      else if (wtp.paramInfos.isEmpty && isAutoApplied(tree.symbol))
        readaptSimplified(tpd.Apply(tree, Nil))
      else if (wtp.isImplicitMethod)
        err.typeMismatch(tree, pt)
      else
        missingArgs(wtp)
    }

    def adaptNoArgsOther(wtp: Type, functionExpected: Boolean): Tree = {
      val implicitFun = defn.isContextFunctionType(wtp) && !untpd.isContextualClosure(tree)
      def caseCompanion =
          functionExpected &&
          tree.symbol.is(Module) &&
          tree.symbol.companionClass.is(Case) &&
          !tree.tpe.baseClasses.exists(defn.isFunctionClass) && {
            report.warning("The method `apply` is inserted. The auto insertion will be deprecated, please write `" + tree.show + ".apply` explicitly.", tree.sourcePos)
            true
          }

      if (implicitFun || caseCompanion)
          && !isApplyProto(pt)
          && pt != SingletonTypeProto
          && pt != LhsProto
          && !ctx.mode.is(Mode.Pattern)
          && !tree.isInstanceOf[SplicePattern]
          && !ctx.isAfterTyper
          && !ctx.isInlineContext
      then
        typr.println(i"insert apply on implicit $tree")
        val sel = untpd.Select(untpd.TypedSplice(tree), nme.apply).withAttachment(InsertedApply, ())
        try typed(sel, pt, locked) finally sel.removeAttachment(InsertedApply)
      else if ctx.mode is Mode.Pattern then
        checkEqualityEvidence(tree, pt)
        tree
      else
        val meth = methPart(tree).symbol
        if meth.isAllOf(DeferredInline) && !Inlines.inInlineMethod then
          errorTree(tree, em"Deferred inline ${meth.showLocated} cannot be invoked")
        else if Inlines.needsInlining(tree) then
          tree.tpe <:< wildApprox(pt)
          val errorCount = ctx.reporter.errorCount
          val inlined = Inlines.inlineCall(tree)
          if ((inlined ne tree) && errorCount == ctx.reporter.errorCount) readaptSimplified(inlined)
          else inlined
        else if (tree.symbol.isScala2Macro &&
                // `raw`, `f` and `s` are eliminated by the StringInterpolatorOpt phase
                tree.symbol != defn.StringContext_raw &&
                tree.symbol != defn.StringContext_f &&
                tree.symbol != defn.StringContext_s)
          if (ctx.settings.XignoreScala2Macros.value) {
            report.warning("Scala 2 macro cannot be used in Dotty, this call will crash at runtime. See https://docs.scala-lang.org/scala3/reference/dropped-features/macros.html", tree.srcPos.startPos)
            Throw(New(defn.MatchErrorClass.typeRef, Literal(Constant(s"Reached unexpanded Scala 2 macro call to ${tree.symbol.showFullName} compiled with -Xignore-scala2-macros.")) :: Nil))
              .withType(tree.tpe)
              .withSpan(tree.span)
          }
          else {
            report.error(
              em"""Scala 2 macro cannot be used in Dotty. See https://docs.scala-lang.org/scala3/reference/dropped-features/macros.html
                  |To turn this error into a warning, pass -Xignore-scala2-macros to the compiler""",
              tree.srcPos.startPos)
            tree
          }
        else TypeComparer.testSubType(tree.tpe.widenExpr, pt) match
          case CompareResult.Fail =>
            wtp match
              case wtp: MethodType => missingArgs(wtp)
              case _ =>
                typr.println(i"adapt to subtype ${tree.tpe} !<:< $pt")
                //typr.println(TypeComparer.explained(tree.tpe <:< pt))
                adaptToSubType(wtp)
          case CompareResult.OKwithGADTUsed
          if pt.isValueType
             && !inContext(ctx.fresh.setGadtState(GadtState(GadtConstraint.empty))) {
               val res = (tree.tpe.widenExpr frozen_<:< pt)
               if res then
                 // we overshot; a cast is not needed, after all.
                 gadts.println(i"unnecessary GADTused for $tree: ${tree.tpe.widenExpr} vs $pt in ${ctx.source}")
               res
              } =>
            insertGadtCast(tree, wtp, pt)
          case CompareResult.OKwithOpaquesUsed if !tree.tpe.frozen_<:<(pt)(using ctx.withOwner(defn.RootClass)) =>
            // guard to avoid extra Typed trees, eg. from testSubType(O.T, O.T) which returns OKwithOpaquesUsed
            Typed(tree, TypeTree(pt))
          case _ =>
            //typr.println(i"OK ${tree.tpe}\n${TypeComparer.explained(_.isSubType(tree.tpe, pt))}") // uncomment for unexpected successes
            tree
    }

    // Follow proxies and approximate type paramrefs by their upper bound
    // in the current constraint in order to figure out robustly
    // whether an expected type is some sort of function type.
    def underlyingApplied(tp: Type): Type = tp.stripTypeVar match {
      case tp: RefinedType => tp
      case tp: AppliedType => tp
      case tp: TypeParamRef => underlyingApplied(TypeComparer.bounds(tp).hi)
      case tp: TypeProxy => underlyingApplied(tp.superType)
      case _ => tp
    }

    // If the expected type is a selection of an extension method, deepen it
    // to also propagate the argument type (which is the receiver we have
    // typechecked already). This is needed for i8311.scala. Doing so
    // for all expected types does not work since it would block the case
    // where we have an argument that must be converted with another
    // implicit conversion to the receiver type.
    def sharpenedPt = pt match
      case pt: SelectionProto
      if pt.memberProto.revealIgnored.isExtensionApplyProto => pt.deepenProto
      case _ => pt

    def adaptNoArgs(wtp: Type): Tree = {
      val ptNorm = underlyingApplied(pt)
      def functionExpected = defn.isFunctionNType(ptNorm)
      def needsEta = pt.revealIgnored match
        case _: SingletonType | _: FunOrPolyProto => false
        case _ => true
      var resMatch: Boolean = false
      wtp match {
        case wtp: ExprType =>
          readaptSimplified(tree.withType(wtp.resultType))
        case wtp: MethodType
          if wtp.isImplicitMethod
          && ({ resMatch = constrainResult(tree.symbol, wtp, sharpenedPt); resMatch} || !functionExpected)
          && !ctx.mode.is(Mode.InQuotePatternHoasArgs) =>
          if (resMatch || ctx.mode.is(Mode.ImplicitsEnabled))
            adaptNoArgsImplicitMethod(wtp)
          else
            // Don't proceed with implicit search if result type cannot match - the search
            // will likely be under-constrained, which means that an unbounded number of alternatives
            // is tried. See strawman-contrib MapDecoratorTest.scala for an example where this happens.
            err.typeMismatch(tree, pt)
        case wtp: MethodType if needsEta =>
          val funExpected = functionExpected
          val arity =
            if funExpected then
              if !isFullyDefined(pt, ForceDegree.none) && isFullyDefined(wtp, ForceDegree.none) then
                // if method type is fully defined, but expected type is not,
                // prioritize method parameter types as parameter types of the eta-expanded closure
                0
              else defn.functionArity(ptNorm)
            else
              val nparams = wtp.paramInfos.length
              if nparams > 1
                  || nparams == 1 && !wtp.isVarArgsMethod
                  || pt.eq(AnyFunctionProto)
              then nparams
              else -1 // no eta expansion in this case
          adaptNoArgsUnappliedMethod(wtp, funExpected, arity)
        case _ =>
          adaptNoArgsOther(wtp, functionExpected)
      }
    }

    /** Adapt an expression of constant type to a different constant type `tpe`. */
    def adaptConstant(tree: Tree, tpe: ConstantType): Tree = {
      def lit = Literal(tpe.value).withSpan(tree.span)
      tree match {
        case Literal(c) => lit
        case tree @ Block(stats, expr) => tpd.cpy.Block(tree)(stats, adaptConstant(expr, tpe))
        case tree =>
          if (isIdempotentExpr(tree)) lit // See discussion in phase Literalize why we demand isIdempotentExpr
          else Block(tree :: Nil, lit)
      }
    }

    def toSAM(tree: Tree, samParent: Type): Tree = tree match {
      case tree: Block => tpd.cpy.Block(tree)(tree.stats, toSAM(tree.expr, samParent))
      case tree: Closure => cpy.Closure(tree)(tpt = TypeTree(samParent)).withType(samParent)
    }

    def adaptToSubType(wtp: Type): Tree =
      // try converting a constant to the target type
      ConstFold(tree).tpe.widenTermRefExpr.normalized match
        case ConstantType(x) =>
          val converted = x.convertTo(pt)
          if converted != null && (converted ne x) then
            val cls = pt.classSymbol
            if x.tag == IntTag && cls == defn.FloatClass && x.intValue.toFloat.toInt != x.intValue
              || x.tag == LongTag && cls == defn.FloatClass  && x.longValue.toFloat.toLong != x.longValue
              || x.tag == LongTag && cls == defn.DoubleClass && x.longValue.toDouble.toLong != x.longValue
            then
              report.warning(LossyWideningConstantConversion(x.tpe, pt), tree.srcPos)
            return readapt(adaptConstant(tree, ConstantType(converted)))
        case _ =>

      val captured = captureWildcardsCompat(wtp, pt)
      if (captured `ne` wtp)
        return readapt(tree.cast(captured))

      // drop type if prototype is Unit
      if (pt isRef defn.UnitClass) {
        // local adaptation makes sure every adapted tree conforms to its pt
        // so will take the code path that decides on inlining
        val tree1 = adapt(tree, WildcardType, locked)
        checkStatementPurity(tree1)(tree, ctx.owner, isUnitExpr = true)
        if (!ctx.isAfterTyper && !tree.isInstanceOf[Inlined] && ctx.settings.WvalueDiscard.value && !isThisTypeResult(tree)) {
          report.warning(ValueDiscarding(tree.tpe), tree.srcPos)
        }
        return tpd.Block(tree1 :: Nil, unitLiteral)
      }

      // convert function literal to SAM closure
      tree match {
        case closure(Nil, id @ Ident(nme.ANON_FUN), _)
        if defn.isFunctionNType(wtp) && !defn.isFunctionNType(pt) =>
          pt match {
            case SAMType(samMeth, samParent)
            if wtp <:< samMeth.toFunctionType(isJava = samParent.classSymbol.is(JavaDefined)) =>
              // was ... && isFullyDefined(pt, ForceDegree.flipBottom)
              // but this prevents case blocks from implementing polymorphic partial functions,
              // since we do not know the result parameter a priori. Have to wait until the
              // body is typechecked.
              return toSAM(tree, samParent)
            case _ =>
          }
        case _ =>
      }

      // try an Any -> Matchable conversion
      if pt.isMatchableBound && !wtp.derivesFrom(defn.MatchableClass) then
        checkMatchable(wtp, tree.srcPos, pattern = false)
        val target = AndType(tree.tpe.widenExpr, defn.MatchableType)
        if target <:< pt then
          return readapt(tree.cast(target))

      // if unsafeNulls is enabled, try to strip nulls from Java function calls
      if Nullables.unsafeNullsEnabled then
        tree match
          case _: Apply | _: Select if tree.symbol.is(JavaDefined) =>
            wtp match
              case OrNull(wtp1) => return readapt(tree.cast(wtp1))
              case _ =>
          case _ =>

      def recover(failure: SearchFailureType) =
        if canDefineFurther(wtp) || canDefineFurther(pt) then readapt(tree)
        else err.typeMismatch(tree, pt, failure)

      pt match
        case _: SelectionProto =>
          tree // adaptations for selections are handled in typedSelect
        case _ if ctx.mode.is(Mode.ImplicitsEnabled) && tree.tpe.isValueType =>
          if pt.isRef(defn.AnyValClass, skipRefined = false)
              || pt.isRef(defn.ObjectClass, skipRefined = false)
          then
            recover(TooUnspecific(pt))
          else inferView(tree, pt) match
            case SearchSuccess(found, _, _, isExtension) =>
              if isExtension then found
              else
                checkImplicitConversionUseOK(found, pt)
                withoutMode(Mode.ImplicitsEnabled)(readapt(found))
            case failure: SearchFailure =>
              if (pt.isInstanceOf[ProtoType] && !failure.isAmbiguous) then
                // don't report the failure but return the tree unchanged. This
                // will cause a failure at the next level out, which usually gives
                // a better error message. To compensate, store the encountered failure
                // as an attachment, so that it can be reported later as an addendum.
                rememberSearchFailure(tree, failure)
                tree
              else recover(failure.reason)
        case _ =>
          recover(NoMatchingImplicits)
    end adaptToSubType

    def adaptType(tp: Type): Tree = {
      val tree1 =
        if (pt eq AnyTypeConstructorProto) || tp.typeParamSymbols.isEmpty then tree
        else {
          if (ctx.isJava)
            // Cook raw type
            val typeArgs = tp.typeParams.map(Function.const(TypeBounds.empty))
            val tree1 = AppliedTypeTree(tree, typeArgs.map(TypeTree(_)))
            val tp1 = AppliedType(tree.tpe, typeArgs)
            tree1.withType(tp1)
          else
            // Eta-expand higher-kinded type
            val tp1 = tree.tpe.etaExpand
            tree.withType(tp1)
        }
      if (ctx.mode.is(Mode.Pattern) || ctx.mode.isQuotedPattern || tree1.tpe <:< pt) tree1
      else err.typeMismatch(tree1, pt)
    }

    /** If tree has an error type but no errors are reported yet, issue
     *  the error message stored in the type.
     *  One way this can happen is if implicit search causes symbols and types
     *  to be completed. The types are stored by `typedAhead` so that they can be
     *  retrieved later and thus avoid duplication of typechecking work.
     *  But if the implicit search causing the `typedAhead` fails locally but
     *  another alternative succeeds we can be left with an ErrorType in the
     *  tree that went unreported. A scenario where this happens is i1802.scala.
     */
    def ensureReported(tp: Type) = tp match {
      case err: ErrorType if !ctx.reporter.errorsReported => report.error(err.msg, tree.srcPos)
      case _ =>
    }

    /** If `tree` is a constructor proxy reference, convert it to a `new` expression,
     *  otherwise return EmptyTree.
     */
    def newExpr(tree: Tree): Tree =
      val ctorResultType = applyProxyResultType(tree)
      if !ctorResultType.exists then return EmptyTree
      val qual = qualifier(tree)
      val tpt = qual match
        case Ident(name) =>
          cpy.Ident(qual)(name.toTypeName)
        case Select(pre, name) =>
          cpy.Select(qual)(pre, name.toTypeName)
        case qual: This if qual.symbol.is(ModuleClass) =>
          cpy.Ident(qual)(qual.symbol.name.sourceModuleName.toTypeName)
        case _ =>
          errorTree(tree, em"cannot convert from $tree to an instance creation expression")
      val tycon = ctorResultType.underlyingClassRef(refinementOK = false)
      typed(
        untpd.Select(
          untpd.New(untpd.TypedSplice(tpt.withType(tycon))),
          nme.CONSTRUCTOR),
        pt)
        .showing(i"convert creator $tree -> $result", typr)

    /** If `tree` is a constructor proxy reference, return the type it constructs,
     *  otherwise return NoType.
     */
    def applyProxyResultType(tree: Tree): Type = tree match
      case Select(_, nme.apply) =>
        // can't use tree.symbol and tree.tpe.widen.finalResultType, because when overloaded
        // tree.symbol is NoSymbol (via MultiDenotation.symbol) and tree.tpe won't widen.
        tree.denot.altsWith(_.isAllOf(ApplyProxyFlags)) match
          case denot :: _ =>
            // any of the constructors will do, in order to get the result type, so using the first one
            denot.info.widen.finalResultType
          case _ => NoType
      case _ => NoType

    tree match {
      case _: MemberDef | _: PackageDef | _: Import | _: WithoutTypeOrPos[?] | _: Closure => tree
      case _ => tree.tpe.widen match {
        case tp: FlexType =>
          ensureReported(tp)
          tree
        case ref: TermRef =>
          pt match {
            case pt: FunProto
            if needsTupledDual(ref, pt) && Feature.autoTuplingEnabled =>
              adapt(tree, pt.tupledDual, locked)
            case _ =>
              newExpr(tree).orElse(adaptOverloaded(ref))
          }
        case poly: PolyType
        if !(ctx.mode is Mode.Type) && dummyTreeOfType.unapply(tree).isEmpty =>
            // If we are in a conversion from a TermRef with polymorphic underlying
        	  // type, give up. In this case the typed `null` literal cannot be instantiated.
            // Test case was but i18695.scala, but it got fixed by a different tweak in #18719.
            // We leave test for this condition in as a defensive measure in case
            // it arises somewhere else.
          newExpr(tree).orElse:
            if pt.isInstanceOf[PolyProto] then tree
            else
              var typeArgs = tree match
                case Select(qual, nme.CONSTRUCTOR) => qual.tpe.widenDealias.argTypesLo.map(TypeTree(_))
                case _ => Nil
              if typeArgs.isEmpty then typeArgs = constrained(poly, tree)._2.map(_.wrapInTypeTree(tree))
              convertNewGenericArray(readapt(tree.appliedToTypeTrees(typeArgs)))
        case wtp =>
          val isStructuralCall = wtp.isValueType && isStructuralTermSelectOrApply(tree)
          if (isStructuralCall)
            readaptSimplified(handleStructural(tree))
          else pt match {
            case pt: FunProto =>
              newExpr(tree).orElse(adaptToArgs(wtp, pt))
            case pt: PolyProto if !wtp.isImplicitMethod =>
              tryInsertApplyOrImplicit(tree, pt, locked)(tree) // error will be reported in typedTypeApply
            case _ =>
              if (ctx.mode is Mode.Type) adaptType(tree.tpe)
              else adaptNoArgs(wtp)
          }
      }
    }
  }

  /** True if this inline typer has already issued errors */
  def hasInliningErrors(using Context): Boolean = false

  /** Does the "contextuality" of the method type `methType` match the one of the prototype `pt`?
   *  This is the case if
   *   - both are contextual, or
   *   - neither is contextual, or
   *   - the prototype is contextual and the method type is implicit.
   *  The last rule is there for a transition period; it allows to mix `with` applications
   *  with old-style context functions.
   *  Overridden in `ReTyper`, where all applications are treated the same
   */
  protected def matchingApply(methType: MethodOrPoly, pt: FunProto)(using Context): Boolean =
    val isUsingApply = pt.applyKind == ApplyKind.Using
    methType.isContextualMethod == isUsingApply
    || methType.isImplicitMethod && isUsingApply // for a transition allow `using` arguments for regular implicit parameters

  /** Check that `tree == x: pt` is typeable. Used when checking a pattern
   *  against a selector of type `pt`. This implementation accounts for
   *  user-defined definitions of `==`.
   *
   *  Overwritten to no-op in ReTyper.
   */
  protected def checkEqualityEvidence(tree: tpd.Tree, pt: Type)(using Context) : Unit =
    tree match
      case _: RefTree | _: Literal
      if !isVarPattern(tree) && !(pt <:< tree.tpe) =>
        withMode(Mode.GadtConstraintInference):
          TypeComparer.constrainPatternType(tree.tpe, pt)

        Linter.warnOnImplausiblePattern(tree, pt)

        val cmp =
          untpd.Apply(
            untpd.Select(untpd.TypedSplice(tree), nme.EQ),
            untpd.TypedSplice(dummyTreeOfType(pt)))
        typedExpr(cmp, defn.BooleanType)
      case _ =>

  private def checkStatementPurity(tree: tpd.Tree)(original: untpd.Tree, exprOwner: Symbol, isUnitExpr: Boolean = false)(using Context): Unit =
    if !tree.tpe.isErroneous
      && !ctx.isAfterTyper
      && !tree.isInstanceOf[Inlined]
      && isPureExpr(tree)
      && !isSelfOrSuperConstrCall(tree)
    then tree match
      case closureDef(meth)
      if meth.span == meth.rhs.span.toSynthetic && !untpd.isFunction(original) =>
        // It's a synthesized lambda, for instance via an eta expansion: report a hard error
        // There are two tests for synthetic lambdas which both have to be true.
        // The first test compares spans of closure definition with the closure's right hand
        // side. This is usually accurate but can fail for compiler-generated test code.
        // See repl.DocTests for two failing tests. The second tests rules out closures
        // if the original tree was a lambda. This does not work always either since
        // sometimes we do not have the original anymore and use the transformed tree instead.
        // But taken together, the two criteria are quite accurate.
        missingArgs(tree, tree.tpe.widen)
      case _ if isUnitExpr =>
        report.warning(PureUnitExpression(original, tree.tpe), original.srcPos)
      case _ =>
        report.warning(PureExpressionInStatementPosition(original, exprOwner), original.srcPos)

  /** Types the body Scala 2 macro declaration `def f = macro <body>` */
  protected def typedScala2MacroBody(call: untpd.Tree)(using Context): Tree =
    // TODO check that call is to a method with valid signature
    def typedPrefix(tree: untpd.RefTree)(splice: Context ?=> Tree => Tree)(using Context): Tree = {
      tryAlternatively {
        splice(typedExpr(tree, defn.AnyType))
      } {
        // Try to type as a macro bundle
        val ref = tree match
          case Ident(name) => untpd.Ident(name.toTypeName).withSpan(tree.span)
          case Select(qual, name) => untpd.Select(qual, name.toTypeName).withSpan(tree.span)
        val bundle = untpd.Apply(untpd.Select(untpd.New(ref), nme.CONSTRUCTOR), untpd.Literal(Constant(null))).withSpan(call.span)
        val bundle1 = typedExpr(bundle, defn.AnyType)
        val bundleVal = SyntheticValDef(NameKinds.UniqueName.fresh(nme.bundle), bundle1).withSpan(call.span)
        tpd.Block(List(bundleVal), splice(tpd.ref(bundleVal.symbol))).withSpan(call.span)
      }
    }
    if ctx.phase.isTyper then
      call match
        case untpd.Ident(nme.???) => // Instinsic macros ignored
        case _ =>
          if !config.Feature.scala2ExperimentalMacroEnabled then
            report.error(
              em"""Scala 2 macro definition needs to be enabled
                  |by making the implicit value scala.language.experimental.macros visible.
                  |This can be achieved by adding the import clause 'import scala.language.experimental.macros'
                  |or by setting the compiler option -language:experimental.macros.
                """,
              call.srcPos)
      call match
        case call: untpd.Ident =>
          typedIdent(call, defn.AnyType)
        case untpd.Select(qual: untpd.RefTree, name) =>
          typedPrefix(qual) { qual =>
            val call2 = untpd.Select(untpd.TypedSplice(qual), name).withSpan(call.span)
            typedSelect(call2, defn.AnyType)
          }
        case untpd.TypeApply(untpd.Select(qual: untpd.RefTree, name), targs) =>
          typedPrefix(qual) { qual =>
            val call2 = untpd.TypeApply(untpd.Select(untpd.TypedSplice(qual), name), targs).withSpan(call.span)
            typedTypeApply(call2, defn.AnyType)
          }
        case _ =>
          report.error(em"Invalid Scala 2 macro $call", call.srcPos)
          EmptyTree
    else typedExpr(call, defn.AnyType)

  /** Insert GADT cast to target type `pt` on the `tree`
    *   so that -Ycheck in later phases succeeds.
    *  The check "safeToInstantiate" in `maximizeType` works to prevent unsound GADT casts.
    */
  private def insertGadtCast(tree: Tree, wtp: Type, pt: Type)(using Context): Tree =
    val target =
      if tree.tpe.isSingleton then
        // In the target type, when the singleton type is intersected, we also intersect
        //   the GADT-approximated type of the singleton to avoid the loss of
        //   information. See #15646.
        val gadtApprox = Inferencing.approximateGADT(wtp)
        gadts.println(i"gadt approx $wtp ~~~ $gadtApprox")
        val conj =
          TypeComparer.testSubType(gadtApprox, pt) match {
            case CompareResult.OK =>
              // GADT approximation of the tree type is a subtype of expected type under empty GADT
              //   constraints, so it is enough to only have the GADT approximation.
              AndType(tree.tpe, gadtApprox)
            case _ =>
              // In other cases, we intersect both the approximated type and the expected type.
              AndType(AndType(tree.tpe, gadtApprox), pt)
          }
        if tree.tpe.isStable && !conj.isStable then
          // this is needed for -Ycheck. Without the annotation Ycheck will
          // skolemize the result type which will lead to different types before
          // and after checking. See i11955.scala.
          AnnotatedType(conj, Annotation(defn.UncheckedStableAnnot, tree.symbol.span))
        else conj
      else pt
    if target.existsPart(_.isInstanceOf[ProtoType]) then
      // we want to avoid embedding a SelectionProto in a Conversion, as the result type
      // as it might end up within a GADT cast type, e.g. tests/pos/i15867.scala
      // so we just bail - in that example, a GADT cast will be insert on application, so it compiles.
      // but tests/pos/i18062.scala is an example with a polymorphic method, which requires type variables to
      // be applied to the tree and then constrained before they match the prototype.
      // so rather than try to handle all that before calling adapt, let's just bail on this side.
      tree
    else
      gadts.println(i"insert GADT cast from $tree to $target")
      tree.cast(target)
  end insertGadtCast
}
