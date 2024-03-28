package dotty.tools
package dotc
package core

import Contexts.*, Types.*, Symbols.*, Names.*, NameKinds.*, Flags.*
import SymDenotations.*
import util.Spans.*
import util.Stats
import Decorators.*
import StdNames.*
import collection.mutable
import ast.tpd.*
import reporting.trace
import config.Printers.typr
import config.Feature
import typer.ProtoTypes.*
import typer.ForceDegree
import typer.Inferencing.*
import typer.IfBottom
import reporting.TestingReporter
import cc.{CapturingType, derivedCapturingType, CaptureSet, isBoxed, isBoxedCapturing}
import CaptureSet.{CompareResult, IdempotentCaptRefMap, IdentityCaptRefMap}

import scala.annotation.internal.sharable
import scala.annotation.threadUnsafe

object TypeOps:

  @sharable var track: Boolean = false // for debugging

  /** The type `tp` as seen from prefix `pre` and owner `cls`. See the spec
   *  for what this means.
   */
  final def asSeenFrom(tp: Type, pre: Type, cls: Symbol)(using Context): Type = {
    pre match {
      case pre: QualSkolemType =>
        // When a selection has an unstable qualifier, the qualifier type gets
        // wrapped in a `QualSkolemType` so that it may appear soundly as the
        // prefix of a path in the selection type.
        // However, we'd like to avoid referring to skolems when possible since
        // they're an extra level of indirection we usually don't need, so we
        // compute the type as seen from the widened prefix, and in the rare
        // cases where this leads to an approximated type we recompute it with
        // the skolemized prefix. See the i6199* tests for usecases.
        val widenedAsf = new AsSeenFromMap(pre.info, cls)
        val ret = widenedAsf.apply(tp)

        if widenedAsf.approxCount == 0 then
          return ret

        Stats.record("asSeenFrom skolem prefix required")
      case _ =>
    }

    new AsSeenFromMap(pre, cls).apply(tp)
  }

  /** The TypeMap handling the asSeenFrom */
  class AsSeenFromMap(pre: Type, cls: Symbol)(using Context) extends ApproximatingTypeMap, IdempotentCaptRefMap {

    /** The number of range approximations in invariant or contravariant positions
     *  performed by this TypeMap.
     *   - Incremented each time we produce a range.
     *   - Decremented each time we drop a prefix range by forwarding to a type alias
     *     or singleton type.
     */
    private[TypeOps] var approxCount: Int = 0

    def apply(tp: Type): Type = {

      /** Map a `C.this` type to the right prefix. If the prefix is unstable, and
       *  the current variance is <= 0, return a range.
       *  @param  pre     The prefix
       *  @param  cls     The class in which the `C.this` type occurs
       *  @param  thiscls The prefix `C` of the `C.this` type.
       */
      def toPrefix(pre: Type, cls: Symbol, thiscls: ClassSymbol): Type = /*>|>*/ trace.conditionally(track, s"toPrefix($pre, $cls, $thiscls)", show = true) /*<|<*/ {
        if ((pre eq NoType) || (pre eq NoPrefix) || (cls is PackageClass))
          tp
        else pre match {
          case pre: SuperType => toPrefix(pre.thistpe, cls, thiscls)
          case _ =>
            if (thiscls.derivesFrom(cls) && pre.baseType(thiscls).exists)
              if (variance <= 0 && !isLegalPrefix(pre))
                approxCount += 1
                range(defn.NothingType, pre)
              else pre
            else if (pre.termSymbol.is(Package) && !thiscls.is(Package))
              toPrefix(pre.select(nme.PACKAGE), cls, thiscls)
            else
              toPrefix(pre.baseType(cls).normalizedPrefix, cls.owner, thiscls)
        }
      }

      trace.conditionally(track, s"asSeen ${tp.show} from (${pre.show}, ${cls.show})", show = true) { // !!! DEBUG
        // All cases except for ThisType are the same as in Map. Inlined for performance
        // TODO: generalize the inlining trick?
        tp match {
          case tp: NamedType =>
            val sym = tp.symbol
            if sym.isStatic && !sym.maybeOwner.seesOpaques || (tp.prefix `eq` NoPrefix)
            then tp
            else derivedSelect(tp, atVariance(variance max 0)(this(tp.prefix)))
          case tp: LambdaType =>
            mapOverLambda(tp) // special cased common case
          case tp: ThisType =>
            toPrefix(pre, cls, tp.cls)
          case _: BoundType =>
            tp
          case _ =>
            mapOver(tp)
        }
      }
    }

    override def reapply(tp: Type): Type =
      // derived infos have already been subjected to asSeenFrom, hence to need to apply the map again.
      tp

    override protected def useAlternate(tp: Type): Type =
      assert(approxCount > 0)
      approxCount -= 1
      tp
  }

  def isLegalPrefix(pre: Type)(using Context): Boolean =
    pre.isStable || !ctx.phase.isTyper

  /** Implementation of Types#simplified */
  def simplify(tp: Type, theMap: SimplifyMap | Null)(using Context): Type = {
    def mapOver = (if (theMap != null) theMap else new SimplifyMap).mapOver(tp)
    tp match {
      case tp: NamedType =>
        if (tp.symbol.isStatic || (tp.prefix `eq` NoPrefix)) tp
        else tp.derivedSelect(simplify(tp.prefix, theMap)) match {
          case tp1: NamedType if tp1.denotationIsCurrent =>
            val tp2 = tp1.reduceProjection
            //if (tp2 ne tp1) println(i"simplified $tp1 -> $tp2")
            tp2
          case tp1 => tp1
        }
      case defn.MatchCase(pat, body) =>
        defn.MatchCase(simplify(pat, theMap), body)
      case tp: AppliedType =>
        tp.tycon match
          case tycon: TypeRef if tycon.info.isInstanceOf[MatchAlias] =>
            isFullyDefined(tp, ForceDegree.all)
          case _ =>
        val normed = tp.tryNormalize
        if normed.exists then simplify(normed, theMap) else tp.map(simplify(_, theMap))
      case tp: TypeParamRef =>
        val tvar = ctx.typerState.constraint.typeVarOfParam(tp)
        if tvar.exists then tvar else tp
      case  _: ThisType | _: BoundType =>
        tp
      case tp: AliasingBounds =>
        tp.derivedAlias(simplify(tp.alias, theMap))
      case AndType(l, r) if !ctx.mode.is(Mode.Type) =>
        simplify(l, theMap) & simplify(r, theMap)
      case tp @ OrType(l, r) if !ctx.mode.is(Mode.Type) =>
        TypeComparer.lub(simplify(l, theMap), simplify(r, theMap), isSoft = tp.isSoft)
      case tp @ CapturingType(parent, refs) =>
        if !ctx.mode.is(Mode.Type)
            && refs.subCaptures(parent.captureSet, frozen = true).isOK
            && (tp.isBoxed || !parent.isBoxedCapturing)
              // fuse types with same boxed status and outer boxed with any type
        then
          simplify(parent, theMap)
        else
          mapOver
      case tp @ AnnotatedType(parent, annot) =>
        val parent1 = simplify(parent, theMap)
        if annot.symbol == defn.UncheckedVarianceAnnot
            && !ctx.mode.is(Mode.Type)
            && !theMap.isInstanceOf[SimplifyKeepUnchecked]
        then parent1
        else tp.derivedAnnotatedType(parent1, annot)
      case _: MatchType =>
        val normed = tp.tryNormalize
        if (normed.exists) simplify(normed, theMap) else mapOver
      case tp: MethodicType =>
        // See documentation of `Types#simplified`
        val addTypeVars = new TypeMap with IdempotentCaptRefMap:
          val constraint = ctx.typerState.constraint
          def apply(t: Type): Type = t match
            case t: TypeParamRef => constraint.typeVarOfParam(t).orElse(t)
            case _ => this.mapOver(t)
        addTypeVars(tp)
      case tp: SkolemType =>
        // Mapping over a skolem creates a new skolem which by definition won't
        // be =:= to the original one.
        tp
      case _ =>
        mapOver
    }
  }

  class SimplifyMap(using Context) extends IdentityCaptRefMap {
    def apply(tp: Type): Type = simplify(tp, this)
  }

  class SimplifyKeepUnchecked(using Context) extends SimplifyMap

  /** Approximate union type by intersection of its dominators.
   *  That is, replace a union type Tn | ... | Tn
   *  by the smallest intersection type of accessible base-class instances of T1,...,Tn.
   *  Example: Given
   *
   *      trait C[+T]
   *      trait D
   *      class A extends C[A] with D
   *      class B extends C[B] with D with E
   *
   *  we approximate `A | B` by `C[A | B] with D`.
   *
   *  Before we do that, we try to find a common non-class supertype of T1 | ... | Tn
   *  in a "best effort", ad-hoc way by selectively widening types in `T1, ..., Tn`
   *  and stopping if the resulting union simplifies to a type that is not a disjunction.
   */
  def orDominator(tp: Type)(using Context): Type = {

    /** a faster version of cs1 intersect cs2 */
    def intersect(cs1: List[ClassSymbol], cs2: List[ClassSymbol]): List[ClassSymbol] =
      val cs2AsSet = BaseClassSet(cs2)
      cs1.filter(cs2AsSet.contains)

    /** a version of Type#baseClasses that treats bottom types correctly */
    def orBaseClasses(tp: Type): List[ClassSymbol] = tp.stripTypeVar match
      case OrType(tp1, tp2) =>
        if tp1.isBottomType && (tp1 frozen_<:< tp2) then orBaseClasses(tp2)
        else if tp2.isBottomType && (tp2 frozen_<:< tp1) then orBaseClasses(tp1)
        else intersect(orBaseClasses(tp1), orBaseClasses(tp2))
      case _ => tp.baseClasses

    /** The minimal set of classes in `cs` which derive all other classes in `cs` */
    def dominators(cs: List[ClassSymbol], accu: List[ClassSymbol]): List[ClassSymbol] = (cs: @unchecked) match {
      case c :: rest =>
        val accu1 = if (accu exists (_ derivesFrom c)) accu else c :: accu
        if (cs == c.baseClasses) accu1 else dominators(rest, accu1)
      case Nil => // this case can happen because after erasure we do not have a top class anymore
        assert(ctx.erasedTypes || ctx.reporter.errorsReported)
        defn.ObjectClass :: Nil
    }

    def mergeRefinedOrApplied(tp1: Type, tp2: Type): Type = {
      def fail = throw new AssertionError(i"Failure to join alternatives $tp1 and $tp2")
      def fallback = tp2 match
        case AndType(tp21, tp22) =>
          mergeRefinedOrApplied(tp1, tp21) & mergeRefinedOrApplied(tp1, tp22)
        case _ =>
          fail
      tp1 match {
        case tp1 @ RefinedType(parent1, name1, rinfo1) =>
          tp2 match {
            case RefinedType(parent2, `name1`, rinfo2) =>
              tp1.derivedRefinedType(
                mergeRefinedOrApplied(parent1, parent2), name1, rinfo1 | rinfo2)
            case _ => fallback
          }
        case tp1 @ AppliedType(tycon1, args1) =>
          tp2 match {
            case AppliedType(tycon2, args2) =>
              tp1.derivedAppliedType(
                mergeRefinedOrApplied(tycon1, tycon2),
                TypeComparer.lubArgs(args1, args2, tycon1.typeParams))
            case _ => fallback
          }
        case tp1 @ TypeRef(pre1, _) =>
          tp2 match {
            case tp2 @ TypeRef(pre2, _) if tp1.name eq tp2.name =>
              tp1.derivedSelect(pre1 | pre2)
            case _ => fallback
          }
        case AndType(tp11, tp12) =>
          mergeRefinedOrApplied(tp11, tp2) & mergeRefinedOrApplied(tp12, tp2)
        case _ => fail
      }
    }

    def approximateOr(tp1: Type, tp2: Type): Type = {
      def isClassRef(tp: Type): Boolean = tp match {
        case tp: TypeRef => tp.symbol.isClass
        case tp: AppliedType => isClassRef(tp.tycon)
        case tp: RefinedType => isClassRef(tp.parent)
        case _ => false
      }

      // Step 1: Get RecTypes and ErrorTypes and CapturingTypes out of the way,
      tp1 match {
        case tp1: RecType =>
          return tp1.rebind(approximateOr(tp1.parent, tp2))
        case CapturingType(parent1, refs1) =>
          return tp1.derivedCapturingType(approximateOr(parent1, tp2), refs1)
        case err: ErrorType =>
          return err
        case _ =>
      }
      tp2 match {
        case tp2: RecType =>
          return tp2.rebind(approximateOr(tp1, tp2.parent))
        case CapturingType(parent2, refs2) =>
          return tp2.derivedCapturingType(approximateOr(tp1, parent2), refs2)
        case err: ErrorType =>
          return err
        case _ =>
      }

      // Step 2: Try to widen either side. This is tricky and incomplete.
      // An illustration is in test pos/padTo.scala: Here we need to compute the join of
      //
      //   `A | C` under the constraints `B >: A` and `C <: B`
      //
      // where `A, B, C` are type parameters.
      // Widening `A` to its upper bound would give `Any | C`, i.e. `Any`.
      // But widening `C` first would give `A | B` and then `B`.
      // So we need to widen `C` first. But how to decide this in general?
      // In the algorithm below, we try to widen both sides (once), and then proceed as follows:
      //
      //  2.0. If no widening succeeds, proceed with step 3.
      //  2.1. If only one widening succeeds, continue with that one.
      //  2.2. If the two widened types are in a subtype relationship, continue with the smaller one.
      //  2.3. If exactly one of the two types is a singleton type, continue with the widened singleton type.
      //  2.4. If the widened tp2 is a supertype of tp1, return widened tp2.
      //  2.5. If the widened tp1 is a supertype of tp2, return widened tp1.
      //  2.6. Otherwise, continue with widened tp1
      //
      // At steps 4-6 we lose possible solutions, since we have to make an
      // arbitrary choice which side to widen. A better solution would look at
      // the constituents of each operand (if the operand is an OrType again) and
      // try to widen them selectively in turn. But this might lead to a combinatorial
      // explosion of possibilities.
      //
      // Another approach could be to store information contained in lower bounds
      // on both sides. So if `B >: A` we'd also record that `A <: B` and therefore
      // widening `A` would yield `B` instead of `Any`, so we'd still be on the right track.
      // This looks feasible if lower bounds are type parameters, but tricky if they
      // are something else. We'd have to extract the strongest possible
      // constraint over all type parameters that is implied by a lower bound.
      // This looks related to an algorithmic problem arising in GADT matching.
      //
      // However, this alone is still not enough. There are other sources of incompleteness,
      // for instance arising from mis-aligned refinements.
      val tp1w = tp1 match {
        case tp1: TypeProxy if !isClassRef(tp1) => tp1.superType.widenExpr
        case _ => tp1
      }
      val tp2w = tp2 match {
        case tp2: TypeProxy if !isClassRef(tp2) => tp2.superType.widenExpr
        case _ => tp2
      }
      if ((tp1w ne tp1) || (tp2w ne tp2)) {
        val isSingle1 = tp1.isInstanceOf[SingletonType]
        val isSingle2 = tp2.isInstanceOf[SingletonType]
        return {
          if (tp2w eq tp2) orDominator(tp1w | tp2)                  // 2.1
          else if (tp1w eq tp1) orDominator(tp1 | tp2w)             // 2.1
          else if (tp1w frozen_<:< tp2w) orDominator(tp1w | tp2)    // 2.2
          else if (tp2w frozen_<:< tp1w) orDominator(tp1 | tp2w)    // 2.2
          else if (isSingle1 && !isSingle2) orDominator(tp1w | tp2) // 2.3
          else if (isSingle2 && !isSingle1) orDominator(tp1 | tp2w) // 2.3
          else if (tp1 frozen_<:< tp2w) tp2w                        // 2.4
          else if (tp2 frozen_<:< tp1w) tp1w                        // 2.5
          else orDominator(tp1w | tp2)                              // 2.6
        }
      }

      def isAccessible(cls: ClassSymbol) =
        if cls.isOneOf(AccessFlags) || cls.privateWithin.exists then
          cls.isAccessibleFrom(tp.baseType(cls).normalizedPrefix)
        else true

      // Step 3: Intersect base classes of both sides
      val commonBaseClasses = orBaseClasses(tp).filterConserve(isAccessible)

      val doms = dominators(commonBaseClasses, Nil)
      def baseTp(cls: ClassSymbol): Type =
        tp.baseType(cls).mapReduceOr(identity)(mergeRefinedOrApplied)
      doms.map(baseTp).reduceLeft(AndType.apply)
    }

    tp match {
      case tp: OrType =>
        (tp.tp1.dealias, tp.tp2.dealias) match
          case (tp1 @ AppliedType(tycon1, args1), tp2 @ AppliedType(tycon2, args2))
          if tycon1.typeSymbol == tycon2.typeSymbol && (tycon1 =:= tycon2) =>
            mergeRefinedOrApplied(tp1, tp2)
          case (tp1, tp2) =>
            approximateOr(tp1, tp2)
      case _ =>
        tp
    }
  }

  /** An abstraction of a class info, consisting of
   *   - the intersection of its parents,
   *   - refined by all non-private fields, methods, and type members,
   *   - abstracted over all type parameters (into a type lambda)
   *   - where all references to `this` of the class are closed over in a RecType.
   */
  def classBound(info: ClassInfo)(using Context): Type = {
    val cls = info.cls
    val parentType = info.parents.reduceLeft(TypeComparer.andType(_, _))
    def isRefinable(sym: Symbol) =
      !sym.is(Private) && !sym.isConstructor && !sym.isClass
    val (refinableDecls, missingDecls) = info.decls.toList.partition(isRefinable)

    def addRefinement(parent: Type, decl: Symbol) = {
      val inherited =
        parentType.findMember(decl.name, cls.thisType,
          required = EmptyFlags, excluded = Private
        ).suchThat(decl.matches(_))
      val inheritedInfo = inherited.info
      val isPolyFunctionApply = decl.name == nme.apply && (parent <:< defn.PolyFunctionType)
      val needsRefinement =
        isPolyFunctionApply
        || {
            if inheritedInfo.exists then
              decl.info.widenExpr <:< inheritedInfo.widenExpr
              && !(inheritedInfo.widenExpr <:< decl.info.widenExpr)
            else
              parent.derivesFrom(defn.SelectableClass)
          }
      if needsRefinement then
        RefinedType(parent, decl.name, avoid(decl.info, missingDecls))
      else parent
    }

    def close(tp: Type) = RecType.closeOver { rt =>
      tp.subst(cls :: Nil, rt.recThis :: Nil).substThis(cls, rt.recThis)
    }

    val raw = refinableDecls.foldLeft(parentType)(addRefinement)
    HKTypeLambda.fromParams(cls.typeParams, raw) match {
      case tl: HKTypeLambda => tl.derivedLambdaType(resType = close(tl.resType))
      case tp => close(tp)
    }
  }

  /** An approximating map that drops NamedTypes matching `toAvoid` and wildcard types. */
  abstract class AvoidMap(using Context) extends AvoidWildcardsMap, IdempotentCaptRefMap:
    @threadUnsafe lazy val localParamRefs = util.HashSet[Type]()

    def toAvoid(tp: NamedType): Boolean

    /** True iff all NamedTypes on this prefix are static */
    override def isStaticPrefix(pre: Type)(using Context): Boolean = pre match
      case pre: NamedType =>
        val sym = pre.currentSymbol
        sym.is(Package) || sym.isStatic && isStaticPrefix(pre.prefix)
      case _ => true

    override def apply(tp: Type): Type =
      try
        tp match
          case tp: TermRef if toAvoid(tp) =>
            tp.info.widenExpr.dealias match {
              case info: SingletonType => apply(info)
              case info => range(defn.NothingType, apply(info))
            }
          case tp: TypeRef if toAvoid(tp) =>
            tp.info match {
              case info: AliasingBounds =>
                apply(info.alias)
              case TypeBounds(lo, hi) =>
                range(atVariance(-variance)(apply(lo)), apply(hi))
              case info: ClassInfo =>
                range(defn.NothingType, apply(classBound(info)))
              case _ =>
                emptyRange // should happen only in error cases
            }
          case tp: ThisType =>
            // ThisType is only used inside a class.
            // Therefore, either they don't appear in the type to be avoided, or
            // it must be a class that encloses the block whose type is to be avoided.
            tp
          case tp: LazyRef =>
            if localParamRefs.contains(tp.ref) then tp
            else if isExpandingBounds then emptyRange
            else mapOver(tp)
          case tl: HKTypeLambda =>
            localParamRefs ++= tl.paramRefs
            mapOver(tl)
          case _ =>
            super.apply(tp)
      catch case ex: Throwable =>
        handleRecursive("traversing for avoiding local references", s"${tp.show}" , ex)
    end apply

    /** Three deviations from standard derivedSelect:
     *   1. We first try a widening conversion to the type's info with
     *      the original prefix. Since the original prefix is known to
     *      be a subtype of the returned prefix, this can improve results.
     *   2. Then, if the approximation result is a singleton reference C#x.type, we
     *      replace by the widened type, which is usually more natural.
     *   3. Finally, we need to handle the case where the prefix type does not have a member
     *      named `tp.name` anymmore. In that case, we need to fall back to Bot..Top.
     */
    override def derivedSelect(tp: NamedType, pre: Type) =
      if (pre eq tp.prefix)
        tp
      else (if pre.isSingleton then NoType else tryWiden(tp, tp.prefix)).orElse {
        if (tp.isTerm && variance > 0 && !pre.isSingleton)
          apply(tp.info.widenExpr)
        else if (upper(pre).member(tp.name).exists)
          super.derivedSelect(tp, pre)
        else
          range(defn.NothingType, defn.AnyType)
      }
  end AvoidMap

  /** An upper approximation of the given type `tp` that does not refer to any symbol in `symsToAvoid`
   *  and does not contain any WildcardType.
   *  We need to approximate with ranges:
   *
   *    term references to symbols in `symsToAvoid`,
   *    term references that have a widened type of which some part refers
   *    to a symbol in `symsToAvoid`,
   *    type references to symbols in `symsToAvoid`,
   *
   *  Type variables that would be interpolated to a type that
   *  needs to be widened are replaced by the widened interpolation instance.
   *
   *  TODO: Could we replace some or all usages of this method by
   *  `LevelAvoidMap` instead? It would be good to investigate this in details
   *  but when I tried it, avoidance for inlined trees broke because `TreeMap`
   *  does not update `ctx.nestingLevel` when entering a block so I'm leaving
   *  this as Future Work™.
   */
  def avoid(tp: Type, symsToAvoid: => List[Symbol])(using Context): Type = {
    val widenMap = new AvoidMap {
      @threadUnsafe lazy val forbidden = symsToAvoid.toSet
      def toAvoid(tp: NamedType) =
        val sym = tp.symbol
        forbidden.contains(sym)

      override def apply(tp: Type): Type = tp match
        case tp: TypeVar if mapCtx.typerState.constraint.contains(tp) =>
          val lo = TypeComparer.instanceType(
            tp.origin,
            fromBelow = variance > 0 || variance == 0 && tp.hasLowerBound,
            widenUnions = tp.widenUnions)(using mapCtx)
          val lo1 = apply(lo)
          if (lo1 ne lo) lo1 else tp
        case _ =>
          super.apply(tp)
      end apply
    }

    widenMap(tp)
  }

  /** If `tpe` is of the form `p.x` where `p` refers to a package
   *  but `x` is not owned by a package, expand it to
   *
   *      p.package.x
   */
  def makePackageObjPrefixExplicit(tpe: NamedType)(using Context): Type = {
    def tryInsert(pkgClass: SymDenotation): Type = pkgClass match {
      case pkg: PackageClassDenotation =>
        var sym = tpe.symbol
        if !sym.exists && tpe.denot.isOverloaded then
          // we know that all alternatives must come from the same package object, since
          // otherwise we would get "is already defined" errors. So we can take the first
          // symbol we see.
          sym = tpe.denot.alternatives.head.symbol
        val pobj = pkg.packageObjFor(sym)
        if (pobj.exists) tpe.derivedSelect(pobj.termRef)
        else tpe
      case _ =>
        tpe
    }
    if (tpe.symbol.isRoot)
      tpe
    else
      tpe.prefix match {
        case pre: ThisType if pre.cls.is(Package) => tryInsert(pre.cls)
        case pre: TermRef if pre.symbol.is(Package) => tryInsert(pre.symbol.moduleClass)
        case _ => tpe
      }
  }

  /** An argument bounds violation is a triple consisting of
   *   - the argument tree
   *   - a string "upper" or "lower" indicating which bound is violated
   *   - the violated bound
   */
  type BoundsViolation = (Tree, String, Type)

  /** The list of violations where arguments are not within bounds.
   *  @param  args          The arguments
   *  @param  boundss       The list of type bounds
   *  @param  instantiate   A function that maps a bound type and the list of argument types to a resulting type.
   *                        Needed to handle bounds that refer to other bounds.
   *  @param  app           The applied type whose arguments are checked, or NoType if
   *                        arguments are for a TypeApply.
   *
   *  This is particularly difficult for F-bounds that also contain wildcard arguments (see below).
   *  In fact the current treatment for this sitiuation can so far only be classified as "not obviously wrong",
   *  (maybe it still needs to be revised).
   */
  def boundsViolations(
      args: List[Tree],
      boundss: List[TypeBounds],
      instantiate: (Type, List[Type]) => Type,
      app: Type)(
      using Context): List[BoundsViolation] = withMode(Mode.CheckBoundsOrSelfType) {
    val argTypes = args.tpes

    /** Replace all wildcards in `tps` with `<app>#<tparam>` where `<tparam>` is the
     *  type parameter corresponding to the wildcard.
     */
    def skolemizeWildcardArgs(tps: List[Type], app: Type) = app match {
      case AppliedType(tycon: TypeRef, args)
      if tycon.typeSymbol.isClass && !Feature.migrateTo3 =>
        tps.zipWithConserve(tycon.typeSymbol.typeParams) {
          (tp, tparam) => tp match {
            case _: TypeBounds => app.select(tparam)
            case _ => tp
          }
        }
      case _ => tps
    }

    // Skolemized argument types are used to substitute in F-bounds.
    val skolemizedArgTypes = skolemizeWildcardArgs(argTypes, app)
    val violations = new mutable.ListBuffer[BoundsViolation]

    def checkOverlapsBounds(lo: Type, hi: Type, arg: Tree, bounds: TypeBounds): Unit = {
      //println(i" = ${instantiate(bounds.hi, argTypes)}")

      var checkCtx = ctx  // the context to be used for bounds checking
      if (argTypes ne skolemizedArgTypes) { // some of the arguments are wildcards

        /** Is there a `LazyRef(TypeRef(_, sym))` reference in `tp`? */
        def isLazyIn(sym: Symbol, tp: Type): Boolean = {
          def isReference(tp: Type) = tp match {
            case tp: LazyRef => tp.ref.isInstanceOf[TypeRef] && tp.ref.typeSymbol == sym
            case _ => false
          }
          tp.existsPart(isReference, forceLazy = false)
        }

        /** The argument types of the form `TypeRef(_, sym)` which appear as a LazyRef in `bounds`.
         *  This indicates that the application is used as an F-bound for the symbol referred to in the LazyRef.
         */
        val lazyRefs = skolemizedArgTypes collect {
          case tp: TypeRef if isLazyIn(tp.symbol, bounds) => tp.symbol
        }

        for (sym <- lazyRefs) {

          // If symbol `S` has an F-bound such as `C[?, S]` that contains wildcards,
          // add a modifieed bound where wildcards are skolemized as a GADT bound for `S`.
          // E.g. for `C[?, S]` we would add `C[C[?, S]#T0, S]` where `T0` is the first
          // type parameter of `C`. The new bound is added as a GADT bound for `S` in
          // `checkCtx`.
          // This mirrors what we do for the bounds that are checked and allows us thus
          // to bounds-check F-bounds with wildcards. A test case is pos/i6146.scala.

          def massage(tp: Type): Type = tp match {
            case tp @ AppliedType(tycon, args) =>
              tp.derivedAppliedType(tycon, skolemizeWildcardArgs(args, tp))
            case tp: AndOrType =>
              tp.derivedAndOrType(massage(tp.tp1), massage(tp.tp2))
            case _ => tp
          }
          def narrowBound(bound: Type, fromBelow: Boolean): Unit = {
            val bound1 = massage(bound)
            if (bound1 ne bound) {
              if (checkCtx eq ctx) checkCtx = ctx.fresh.setFreshGADTBounds
              if (!checkCtx.gadt.contains(sym)) checkCtx.gadtState.addToConstraint(sym)
              checkCtx.gadtState.addBound(sym, bound1, fromBelow)
              typr.println("install GADT bound $bound1 for when checking F-bounded $sym")
            }
          }
          narrowBound(sym.info.loBound, fromBelow = true)
          narrowBound(sym.info.hiBound, fromBelow = false)
        }
      }
      val hiBound = instantiate(bounds.hi, skolemizedArgTypes)
      val loBound = instantiate(bounds.lo, skolemizedArgTypes)

      def check(using Context) = {
        if (!(lo <:< hiBound)) violations += ((arg, "upper", hiBound))
        if (!(loBound <:< hi)) violations += ((arg, "lower", loBound))
      }
      check(using checkCtx)
    }

    def loop(args: List[Tree], boundss: List[TypeBounds]): Unit = args match
      case arg :: args1 => boundss match
        case bounds :: boundss1 =>
          arg.tpe match
            case TypeBounds(lo, hi) => checkOverlapsBounds(lo, hi, arg, bounds)
            case tp => checkOverlapsBounds(tp, tp, arg, bounds)
          loop(args1, boundss1)
        case _ =>
      case _ =>

    loop(args, boundss)
    violations.toList
  }

  /** Refine child based on parent
   *
   *  In child class definition, we have:
   *
   *      class Child[Ts] extends path.Parent[Us] with Es
   *      object Child extends path.Parent[Us] with Es
   *      val child = new path.Parent[Us] with Es           // enum values
   *
   *  Given a parent type `parent` and a child symbol `child`, we infer the prefix
   *  and type parameters for the child:
   *
   *      prefix.child[Vs] <:< parent
   *
   *  where `Vs` are fresh type variables and `prefix` is the symbol prefix with all
   *  non-module and non-package `ThisType` replaced by fresh type variables.
   *
   *  If the subtyping is true, the instantiated type `p.child[Vs]` is
   *  returned. Otherwise, `NoType` is returned.
   */
  def refineUsingParent(parent: Type, child: Symbol, mixins: List[Type] = Nil)(using Context): Type = {
    // <local child> is a place holder from Scalac, it is hopeless to instantiate it.
    //
    // Quote from scalac (from nsc/symtab/classfile/Pickler.scala):
    //
    //     ...When a sealed class/trait has local subclasses, a single
    //     <local child> class symbol is added as pickled child
    //     (instead of a reference to the anonymous class; that was done
    //     initially, but seems not to work, ...).
    //
    if (child.name == tpnme.LOCAL_CHILD) return child.typeRef

    val childTp = if (child.isTerm) child.termRef else child.typeRef

    inContext(ctx.fresh.setExploreTyperState().setFreshGADTBounds.addMode(Mode.GadtConstraintInference)) {
      instantiateToSubType(childTp, parent, mixins).dealias
    }
  }

  /** Instantiate type `tp1` to be a subtype of `tp2`
   *
   *  Return the instantiated type if type parameters in this type
   *  in `tp1` can be instantiated such that `tp1 <:< tp2`.
   *
   *  Otherwise, return NoType.
   */
  private def instantiateToSubType(tp1: NamedType, tp2: Type, mixins: List[Type])(using Context): Type = trace(i"instantiateToSubType($tp1, $tp2, $mixins)", typr) {
    // In order for a child type S to qualify as a valid subtype of the parent
    // T, we need to test whether it is possible S <: T.
    //
    // The check is different from subtype checking due to type parameters and
    // `this`. We perform the following operations to approximate the parameters:
    //
    // 1. Replace type parameters in T with tvars
    // 2. Replace `A.this.C` with `A#C` (see tests/patmat/i12681.scala)
    // 3. Replace non-reducing MatchType with its bound
    //
    val approximateParent = new TypeMap {
      val boundTypeParams = util.HashMap[TypeRef, TypeVar]()

      def apply(tp: Type): Type = tp.dealias match {
        case tp: MatchType =>
          val reduced = tp.reduced
          if reduced.exists then tp // break cycles
          else mapOver(tp.bound) // if the match type doesn't statically reduce
                                 // then to avoid it failing the <:<
                                 // we'll approximate by widening to its bounds

        case ThisType(tref: TypeRef) if !tref.symbol.isStaticOwner =>
          tref

        case tp: TypeRef if !tp.symbol.isClass =>
          val lookup = boundTypeParams.lookup(tp)
          if lookup != null then lookup
          else
            val TypeBounds(lo, hi) = tp.underlying.bounds
            val tv = newTypeVar(TypeBounds(defn.NothingType, hi.topType))
            boundTypeParams(tp) = tv
            assert(tv <:< apply(hi))
            apply(lo) <:< tv //  no assert, since bounds might conflict
            tv

        case tp @ AppliedType(tycon: TypeRef, _) if !tycon.dealias.typeSymbol.isClass && !tp.isMatchAlias =>

          // In tests/patmat/i3645g.scala, we need to tell whether it's possible
          // that K1 <: K[Foo]. If yes, we issue a warning; otherwise, no
          // warnings.
          //
          // - K1 <: K[Foo] is possible <==>
          // - K[Int] <: K[Foo] is possible <==>
          // - Int <: Foo is possible <==>
          // - Int <: Module.Foo.Type is possible
          //
          // If we remove this special case, we will encounter the case Int <:
          // X[Y], where X and Y are tvars. The subtype checking will simply
          // return false. But depending on the bounds of X and Y, the subtyping
          // can be true.
          //
          // As a workaround, we approximate higher-kinded type parameters with
          // the value types that can be instantiated from its bounds.
          //
          // Note that `HKTypeLambda.resType` may contain TypeParamRef that are
          // bound in the HKTypeLambda. This is fine, as the TypeComparer will
          // recurse on the bounds of `TypeParamRef`.
          val bounds: TypeBounds = tycon.underlying match {
            case TypeBounds(tl1: HKTypeLambda, tl2: HKTypeLambda) =>
              TypeBounds(tl1.resType, tl2.resType)
            case TypeBounds(tl1: HKTypeLambda, tp2) =>
              TypeBounds(tl1.resType, tp2)
            case TypeBounds(tp1, tl2: HKTypeLambda) =>
              TypeBounds(tp1, tl2.resType)
          }

          newTypeVar(bounds)

        case tp =>
          mapOver(tp)
      }
    }

    /** Gather GADT symbols and singletons found in `tp2`, ie. the scrutinee. */
    object TraverseTp2 extends TypeTraverser:
      val singletons = util.HashMap[Symbol, SingletonType]()
      val gadtSyms = new mutable.ListBuffer[Symbol]

      def traverse(tp: Type) = try
        val tpd = tp.dealias
        if tpd ne tp then traverse(tpd)
        else tp match
          case tp: ThisType if !singletons.contains(tp.tref.symbol) && !tp.tref.symbol.isStaticOwner =>
            singletons(tp.tref.symbol) = tp
            traverseChildren(tp.tref)
          case tp: TermRef if tp.symbol.is(Param) =>
            singletons(tp.typeSymbol) = tp
            traverseChildren(tp)
          case tp: TypeRef if !gadtSyms.contains(tp.symbol) && tp.symbol.isAbstractOrParamType =>
            gadtSyms += tp.symbol
            traverseChildren(tp)
            // traverse abstract type infos, to add any singletons
            // for example, i16451.CanForward.scala, add `Namer.this`, from the info of the type parameter `A1`
            // also, i19031.ci-reg2.scala, add `out`, from the info of the type parameter `A1` (from synthetic applyOrElse)
            traverseChildren(tp.info)
          case _ =>
            traverseChildren(tp)
      catch case ex: Throwable => handleRecursive("traverseTp2", tp.show, ex)
    TraverseTp2.traverse(tp2)
    val singletons = TraverseTp2.singletons
    val gadtSyms   = TraverseTp2.gadtSyms.toList

    // Prefix inference, given `p.C.this.Child`:
    //   1. return it as is, if `C.this` is found in `tp`, i.e. the scrutinee; or
    //   2. replace it with `X.Child` where `X <: p.C`, stripping ThisType in `p` recursively.
    //
    // See tests/patmat/i3938.scala, tests/pos/i15029.more.scala, tests/pos/i16785.scala
    class InferPrefixMap extends TypeMap {
      var prefixTVar: Type | Null = null
      def apply(tp: Type): Type = tp match {
        case tp: TermRef if singletons.contains(tp.symbol) =>
          prefixTVar = singletons(tp.symbol) // e.g. tests/pos/i19031.ci-reg2.scala, keep out
          prefixTVar.uncheckedNN
        case ThisType(tref) if !tref.symbol.isStaticOwner =>
          val symbol = tref.symbol
          if singletons.contains(symbol) then
            prefixTVar = singletons(symbol) // e.g. tests/pos/i16785.scala, keep Outer.this
            prefixTVar.uncheckedNN
          else if symbol.is(Module) then
            TermRef(this(tref.prefix), symbol.sourceModule)
          else if (prefixTVar != null)
            this(tref.applyIfParameterized(tref.typeParams.map(_ => WildcardType)))
          else {
            prefixTVar = WildcardType  // prevent recursive call from assigning it
            // e.g. tests/pos/i15029.more.scala, create a TypeVar for `Instances`' B, so we can disregard `Ints`
            val tvars = tref.typeParams.map { tparam => newTypeVar(tparam.paramInfo.bounds, DepParamName.fresh(tparam.paramName)) }
            val tref2 = this(tref.applyIfParameterized(tvars))
            prefixTVar = newTypeVar(TypeBounds.upper(tref2), DepParamName.fresh(tref.name))
            prefixTVar.uncheckedNN
          }
        case tp => mapOver(tp)
      }
    }

    val inferThisMap = new InferPrefixMap
    val tvars = tp1.typeParams.map { tparam => newTypeVar(tparam.paramInfo.bounds, DepParamName.fresh(tparam.paramName)) }
    val protoTp1 = inferThisMap.apply(tp1).appliedTo(tvars)

    if gadtSyms.nonEmpty then
      ctx.gadtState.addToConstraint(gadtSyms)

    // If parent contains a reference to an abstract type, then we should
    // refine subtype checking to eliminate abstract types according to
    // variance. As this logic is only needed in exhaustivity check,
    // we manually patch subtyping check instead of changing TypeComparer.
    // See tests/patmat/i3645b.scala
    def parentQualify(tp1: Type, tp2: Type) = tp1.classSymbol.info.parents.exists { parent =>
      parent.argInfos.nonEmpty && approximateParent(parent) <:< tp2
    }

    def instantiate(): Type = {
      for tp <- mixins.reverseIterator do
        protoTp1 <:< tp
      maximizeType(protoTp1, NoSpan)
      wildApprox(protoTp1)
    }

    if (protoTp1 <:< tp2) instantiate()
    else {
      val approxTp2 = approximateParent(tp2)
      if (protoTp1 <:< approxTp2 || parentQualify(protoTp1, approxTp2)) instantiate()
      else NoType
    }
  }

  def nestedPairs(ts: List[Type])(using Context): Type =
    ts.foldRight(defn.EmptyTupleModule.termRef: Type)(defn.PairClass.typeRef.appliedTo(_, _))

  class StripTypeVarsMap(using Context) extends TypeMap:
    def apply(tp: Type) = mapOver(tp).stripTypeVar

  /** Apply [[Type.stripTypeVar]] recursively. */
  def stripTypeVars(tp: Type)(using Context): Type =
    new StripTypeVarsMap().apply(tp)

  /** computes a prefix for `child`, derived from its common prefix with `pre`
   *  - `pre` is assumed to be the prefix of `parent` at a given callsite.
   *  - `child` is assumed to be the sealed child of `parent`, and reachable according to `whyNotGenericSum`.
   */
  def childPrefix(pre: Type, parent: Symbol, child: Symbol)(using Context): Type =
    // Example, given this class hierarchy, we can see how this should work
    // when summoning a mirror for `wrapper.Color`:
    //
    // package example
    // object Outer3:
    //   class Wrapper:
    //     sealed trait Color
    //   val wrapper = new Wrapper
    //   object Inner:
    //     case object Red extends wrapper.Color
    //     case object Green extends wrapper.Color
    //     case object Blue extends wrapper.Color
    //
    //   summon[Mirror.SumOf[wrapper.Color]]
    //                       ^^^^^^^^^^^^^
    //       > pre = example.Outer3.wrapper.type
    //       > parent = sealed trait example.Outer3.Wrapper.Color
    //       > child = module val example.Outer3.Innner.Red
    //       > parentOwners = [example, Outer3, Wrapper] // computed from definition
    //       > childOwners = [example, Outer3, Inner] // computed from definition
    //       > parentRest = [Wrapper] // strip common owners from `childOwners`
    //       > childRest = [Inner] // strip common owners from `parentOwners`
    //       > commonPrefix = example.Outer3.type // i.e. parentRest has only 1 element, use 1st subprefix of `pre`.
    //       > childPrefix = example.Outer3.Inner.type // select all symbols in `childRest` from `commonPrefix`

    /** unwind the prefix into a sequence of sub-prefixes, selecting the one at `limit`
     *  @return `NoType` if there is an unrecognised prefix type.
     */
    def subPrefixAt(pre: Type, limit: Int): Type =
      def go(pre: Type, limit: Int): Type =
        if limit == 0 then pre // EXIT: No More prefix
        else pre match
          case pre: ThisType          => go(pre.tref.prefix, limit - 1)
          case pre: TermRef           => go(pre.prefix, limit - 1)
          case _:SuperType | NoPrefix => pre.ensuring(limit == 1) // EXIT: can't rewind further than this
          case _                      => NoType // EXIT: unrecognized prefix
      go(pre, limit)
    end subPrefixAt

    /** Successively select each symbol in the `suffix` from `pre`, such that they are reachable. */
    def selectAll(pre: Type, suffix: Seq[Symbol]): Type =
      suffix.foldLeft(pre)((pre, sym) =>
        pre.select(
          if sym.isType && sym.is(Module) then sym.sourceModule
          else sym
        )
      )

    def stripCommonPrefix(xs: List[Symbol], ys: List[Symbol]): (List[Symbol], List[Symbol]) = (xs, ys) match
      case (x :: xs1, y :: ys1) if x eq y => stripCommonPrefix(xs1, ys1)
      case _ => (xs, ys)

    val (parentRest, childRest) = stripCommonPrefix(
      parent.owner.ownersIterator.toList.reverse,
      child.owner.ownersIterator.toList.reverse
    )

    val commonPrefix = subPrefixAt(pre, parentRest.size) // unwind parent owners up to common prefix

    if commonPrefix.exists then selectAll(commonPrefix, childRest)
    else NoType

  end childPrefix

end TypeOps
