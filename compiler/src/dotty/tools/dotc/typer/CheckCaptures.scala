package dotty.tools
package dotc
package typer

import core._
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types._
import Symbols._
import StdNames._
import Decorators._
import ProtoTypes._
import Inferencing.isFullyDefined
import config.Printers.refinr
import ast.{tpd, untpd, Trees}
import NameKinds.{DocArtifactName, OuterSelectName, DefaultGetterName}
import Trees._
import scala.util.control.NonFatal
import typer.ErrorReporting._
import util.Spans.Span
import util.SimpleIdentitySet
import util.Chars.*
import Nullables._
import transform.*
import scala.collection.mutable
import reporting._
import ProtoTypes._
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions

/** A class that can be used to do type checking again after the first typer phase
 *  is run. This phase will use the output of the previous typer but "forget" certain things
 *  so that they can be reinferred. Things that are forgotten fall into the following
 *  categories:
 *
 *   1. Bindings of inferred type variables in type applications.
 *   2. Inferred types of local or private vals or vars. Exception: Types of
 *      inline vals and Java-defined fields are kept.
 *   3. Inferred result types of local or private methods. Eception: Types
 *      of default getters and Java-defined methods are kept.
 *      (The default getter restriction is there for technical reason, we should be
 *      able to lift it once we change the scheme for default arguments).
 *   4. Types of closure parameters that are inferred from the expected type.
 *      Types of closure parameters that are inferred from the called method
 *      are left alone (also for technical reasons).
 *
 *  The re-typed trees and associated symbol infos are thrown away once the phase
 *  has ended. So the phase can be only used for more refined type checking, but
 *  not for code transformation.
 */
class CheckCaptures extends RefineTypes:
  import ast.tpd.*

  def phaseName: String = "checkCaptures"
  override def isEnabled(using Context) = ctx.settings.YrefineTypes.value

  def newRefiner() = CaptureChecker()

  class CaptureChecker extends TypeRefiner:
    import ast.tpd.*
    override def newLikeThis: Typer = CaptureChecker()

    private var myDeps: Dependencies = null

    def deps(using Context): Dependencies =
      if myDeps == null then
        myDeps = new Dependencies(ctx.compilationUnit.tpdTree, ctx):
          def isExpr(sym: Symbol)(using Context): Boolean =
            sym.isRealClass || sym.isOneOf(MethodOrLazy)
          def enclosure(using Context) =
            def recur(owner: Symbol): Symbol =
              if isExpr(owner) || !owner.exists then owner else recur(owner.owner)
            recur(ctx.owner)
      myDeps

    override def typedClosure(tree: untpd.Closure, pt: Type)(using Context): Tree =
      val tree1 = super.typedClosure(tree, pt).asInstanceOf[Closure]
      val captured = CaptureSet(
        deps.freeVars(tree1.meth.symbol).toList.map(_.termRef).filter(_.isTracked)*)
      tree1.withType(tree1.tpe.capturing(captured))
  end CaptureChecker
end CheckCaptures

/*
  def run(using Context): Unit =
    refinr.println(i"refine types of ${ctx.compilationUnit}")
    val refiner = newRefiner()
    val unit = ctx.compilationUnit
    val refineCtx = ctx
        .fresh
        .setMode(Mode.ImplicitsEnabled)
        .setTyper(refiner)
    val refinedTree = refiner.typedExpr(unit.tpdTree)(using refineCtx)
    if ctx.settings.Xprint.value.containsPhase(this) then
      report.echo(i"discarded result of $unit after refineTypes:\n\n$refinedTree")

  def preRefinePhase = this.prev.asInstanceOf[PreRefine]
  def thisPhase = this

  def newRefiner(): TypeRefiner = TypeRefiner()

  class TypeRefiner extends ReTyper:
    import ast.tpd.*

    override def newLikeThis: Typer = new TypeRefiner

   /* override def typedAhead(tree: untpd.Tree, typed: untpd.Tree => Tree)(using Context): Tree =
      tree.getAttachment(TypedAhead) match
        case Some(ttree) => ttree
        case none =>
          val ttree = typed(tree)
          tree.putAttachment(TypedAhead, ttree)
          ttree*/

    /** Update the symbol's info to `newInfo` for the current phase, and
     *  to the symbol's orginal info for the phase afterwards.
     */
    def updateInfo(sym: Symbol, newInfo: Type)(using Context): Unit =
      sym.copySymDenotation().installAfter(thisPhase) // reset
      sym.copySymDenotation(
          info = newInfo,
          initFlags =
            if newInfo.isInstanceOf[LazyType] then sym.flags &~ Touched
            else sym.flags
        ).installAfter(preRefinePhase)

    /** A completer for local and provate vals, vars, and defs. Re-infers
     *  the type from the type of the right-hand side expression.
     */
    class RefineCompleter(val original: ValOrDefDef)(using Context) extends LazyType:
      def completeInCreationContext(symd: SymDenotation): Unit =
        val (paramss, paramFn) = original match
          case ddef: DefDef =>
            val paramss = ddef.paramss.nestedMap(_.symbol)
            (paramss, wrapMethodType(_: Type, paramss, isJava = false))
          case _: ValDef =>
            (Nil, (x: Type) => x)
        inContext(ctx.fresh.setOwner(symd.symbol).setTree(original)) {
          val rhsType = inferredResultType(original, symd.symbol, paramss, paramFn, WildcardType)
          typedAheadType(original.tpt, rhsType)
          symd.info = paramFn(rhsType)
        }

      def complete(symd: SymDenotation)(using Context): Unit = completeInCreationContext(symd)
    end RefineCompleter

    /** Update the infos of all symbols defined `trees` that have (result) types
     *  that need to be reinferred. This is the case if
     *   1. the type was inferred originally, and
     *   2. the definition is private or local,
     *   3. the definition is not a parameter or Java defined
     *   4. the definition is not an inline value
     *   5. the definition is not a default getter
     */
    override def index(trees: List[untpd.Tree])(using Context): Context =
      for case tree: ValOrDefDef <- trees.asInstanceOf[List[Tree]] do
        val sym = tree.symbol
        if tree.tpt.isInstanceOf[untpd.InferredTypeTree]
          && (sym.is(Private) || sym.owner.isTerm)
          && !sym.isOneOf(Param | JavaDefined)
          && !sym.isOneOf(FinalOrInline, butNot = Method | Mutable)
          && !sym.name.is(DefaultGetterName)
        then
          updateInfo(sym, RefineCompleter(tree))
      ctx

    /** Keep the types of all source-written type trees; re-typecheck the rest */
    override def typedUnadapted(tree: untpd.Tree, pt: Type, locked: TypeVars)(using Context): Tree =
      trace(i"typed $tree, $pt", refinr, show = true) {
        tree.removeAttachment(TypedAhead) match
          case Some(ttree) => ttree
          case none =>
            tree match
              case _: untpd.TypedSplice
                  | _: untpd.Thicket
                  | _: EmptyValDef[?]
                  | _: untpd.TypeTree =>
                super.typedUnadapted(tree, pt, locked)
              case _ if tree.isType =>
                promote(tree)
              case _ =>
                super.typedUnadapted(tree, pt, locked)
      }

    /** Keep the symbol of the `select` but re-infer its type */
    override def typedSelect(tree: untpd.Select, pt: Type)(using Context): Tree =
      val Select(qual, name) = tree
      if name.is(OuterSelectName) then promote(tree)
      else
        val qual1 = withoutMode(Mode.Pattern)(typed(qual, AnySelectionProto))
        val qualType = qual1.tpe.widenIfUnstable
        val pre = maybeSkolemizePrefix(qualType, name)
        val mbr = qualType.findMember(name, pre,
            excluded = if tree.symbol.is(Private) then EmptyFlags else Private)
          .suchThat(tree.symbol ==)
        val ownType = qualType.select(name, mbr)
        untpd.cpy.Select(tree)(qual1, name).withType(ownType)

    /** Set the type of inferred TypeTrees to the expected type. Keep the others unchanged. */
    override def typedTypeTree(tree: untpd.TypeTree, pt: Type)(using Context): TypeTree =
      if tree.isInstanceOf[untpd.InferredTypeTree] && isFullyDefined(pt, ForceDegree.flipBottom) then
        tree.withType(pt)
      else
        promote(tree)

    /** Redo core steps of type checking from Typer (they were overridden in ReTyper).
     *  Compare with `typedTyped` in TreeChecker that does essentially the same thing
     */
    override def typedTyped(tree: untpd.Typed, pt: Type)(using Context): Tree =
      val tpt1 = checkSimpleKinded(typedType(tree.tpt))
      val expr1 = tree.expr match
        case id: untpd.Ident if (ctx.mode is Mode.Pattern) && untpd.isVarPattern(id) && (id.name == nme.WILDCARD || id.name == nme.WILDCARD_STAR) =>
          tree.expr.withType(tpt1.tpe)
        case _ =>
          var pt1 = tpt1.tpe
          if pt1.isRepeatedParam then
            pt1 = pt1.translateFromRepeated(toArray = tree.expr.typeOpt.derivesFrom(defn.ArrayClass))
          typed(tree.expr, pt1)
      untpd.cpy.Typed(tree)(expr1, tpt1).withType(tree.typeOpt)

    /** Replace all type variables in a (possibly embedded) type application
     *  by fresh, uninstantiated type variables that are pairwise linked with
     *  the old ones. The type application can either be the toplevel tree `tree`
     *  or wrapped in one or more closures.
     *  Replacement insde closures is necessary since sometimes type variables
     *  bound in a callee are leaked in the parameter types of an enclosing closure
     *  that infers its parameter types from the callee.
     *  @return  The changed tree with the new type variables,
     *           and a map from old type variables to corresponding freshly created type variables
     */
    private def resetTypeVars[T <: tpd.Tree](tree: T)(using Context): (T, Map[TypeVar, TypeVar]) = tree match
      case tree: TypeApply =>
        val tvars = for arg <- tree.args; case tvar: TypeVar <- arg.tpe :: Nil yield tvar
        if tvars.nonEmpty && tvars.length == tree.args.length then
          val (args1, tvars1) =
            if tvars.head.isInstantiated then
              // we are seeing type variables that are not yet copied by a previous resetTypeVars
              val args1 = constrained(tree.fun.tpe.widen.asInstanceOf[TypeLambda], tree)._2
              val tvars1 = args1.tpes.asInstanceOf[List[TypeVar]]
              for (tvar, tvar1) <- tvars.lazyZip(tvars1) do tvar1.link(tvar)
              (args1, tvars1)
            else
              (tree.args, tvars)
          (cpy.TypeApply(tree)(tree.fun, args1).asInstanceOf[T],
           tvars1.map(tvar => (tvar.linkedOriginal, tvar)).toMap)
        else
          (tree, Map.empty)
      case tree @ Apply(fn, args) =>
        val (fn1, map1) = resetTypeVars(fn)
        (cpy.Apply(tree)(fn, args).asInstanceOf[T], map1)
      case Block(stats, closure: Closure) =>
        var tvmap: Map[TypeVar, TypeVar] = Map.empty
        val stats1 = stats.mapConserve {
          case stat: DefDef if stat.symbol == closure.meth.symbol =>
            val (rhs1, map1) = resetTypeVars(stat.rhs)
            tvmap = map1
            cpy.DefDef(stat)(rhs = rhs1)
          case stat => stat
        }
        (cpy.Block(tree)(stats1, closure).asInstanceOf[T], tvmap)
      case Block(Nil, expr) =>
        val (rhs1, map1) = resetTypeVars(expr)
        (cpy.Block(tree)(Nil, rhs1).asInstanceOf[T], map1)
      case _ =>
        (tree, Map.empty)
    end resetTypeVars

    /** The application with all inferred type arguments reset to fresh type variab;es
     *  classOf[...] applications are left alone.
     */
    override def typedTypeApply(app: untpd.TypeApply, pt: Type)(using Context): Tree =
      val app0 = promote(app)
      if app0.symbol == defn.Predef_classOf then app0
      else super.typedTypeApply(resetTypeVars(app0)._1, pt)

    /** If block is defines closure, replace all parameters that were inferred
     *  from the expected type by corresponding parts of the new expected type.
     *  Update infos of parameter symbols and the anonymous function accordingly.
     */
    override def typedBlock(blk: untpd.Block, pt: Type)(using Context): Tree =
      val blk0 = promote(blk)
      val blk1 = blk0.expr match
        case closure: Closure =>
          val stats1 = blk0.stats.mapConserve {
            case stat: DefDef if stat.symbol == closure.meth.symbol =>
              stat.paramss match
                case ValDefs(params) :: Nil =>
                  val (protoFormals, _) = decomposeProtoFunction(pt, params.length, stat)
                  val params1 = params.zipWithConserve(protoFormals) {
                    case (param @ ValDef(_, tpt: InferredTypeTree, _), formal)
                    if isFullyDefined(formal, ForceDegree.failBottom) =>
                      updateInfo(param.symbol, formal)
                      cpy.ValDef(param)(tpt = param.tpt.withType(formal))
                    case (param, _) =>
                      param
                  }
                  if params eq params1 then stat
                  else
                    val mt = stat.symbol.info.asInstanceOf[MethodType]
                    val formals1 =
                      for i <- mt.paramInfos.indices.toList yield
                        if params(i) eq params1(i) then mt.paramInfos(i) else protoFormals(i)
                    updateInfo(stat.symbol, mt.derivedLambdaType(paramInfos = formals1))
                    cpy.DefDef(stat)(paramss = params1 :: Nil)
                case _ =>
                  stat
          }
          cpy.Block(blk0)(stats1, closure)
        case _ =>
          blk
      super.typedBlock(blk1, pt)

    /** If tree defines an anonymous function, make sure that any type variables
     *  defined in the callee rhs are replaced in the function itself.
     */
    override def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(using Context): Tree =
      sym.ensureCompleted()
      if sym.isAnonymousFunction then
        val ddef0 = promote(ddef)
        val (rhs1, tvmap) = resetTypeVars(ddef0.rhs)
        if tvmap.nonEmpty then
          val tmap = new TypeMap:
            def apply(t: Type) = mapOver {
              t match
                case t: TypeVar => tvmap.getOrElse(t, t)
                case _ => t
            }
          val ValDefs(params) :: Nil = ddef0.paramss
          val params1 = params.mapConserve { param =>
            updateInfo(param.symbol, tmap(param.symbol.info))
            cpy.ValDef(param)(tpt = param.tpt.withType(tmap(param.tpt.tpe)))
          }
          val mt = sym.info.asInstanceOf[MethodType]
          updateInfo(sym, mt.derivedLambdaType(paramInfos = mt.paramInfos.mapConserve(tmap)))
          val ddef1 = cpy.DefDef(ddef0)(paramss = params1 :: Nil, rhs = rhs1)
          val nestedCtx = ctx.fresh.setNewTyperState() // needed so that leaked type variables properly nest in their owning typerstate
          try inContext(nestedCtx) { super.typedDefDef(ddef1, sym) }
          finally nestedCtx.typerState.commit()
        else super.typedDefDef(ddef, sym)
      else super.typedDefDef(ddef, sym)

    override def typedValDef(vdef: untpd.ValDef, sym: Symbol)(using Context): Tree =
      sym.ensureCompleted()
      super.typedValDef(vdef, sym)

    override def typedPackageDef(tree: untpd.PackageDef)(using Context): Tree =
      if tree.symbol == defn.StdLibPatchesPackage then
        promote(tree) // don't check stdlib patches, since their symbols were highjacked by stdlib classes
      else
        super.typedPackageDef(tree)
  end TypeRefiner
*/
