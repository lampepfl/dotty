package dotty.tools.dotc
package core

import Types._
import Contexts._
import Symbols._
import SymDenotations.{LazyType, TypeParamsCompleter}
import Decorators._
import util.Stats._
import util.common._
import Names._
import NameOps._
import Flags._
import StdNames.tpnme
import util.Positions.Position
import config.Printers._
import collection.mutable
import dotty.tools.dotc.config.Config
import java.util.NoSuchElementException

object TypeApplications {

  /** Assert type is not a TypeBounds instance and return it unchanged */
  val noBounds = (tp: Type) => tp match {
    case tp: TypeBounds => throw new AssertionError("no TypeBounds allowed")
    case _ => tp
  }

  /** If `tp` is a TypeBounds instance return its lower bound else return `tp` */
  val boundsToLo = (tp: Type) => tp match {
    case tp: TypeBounds => tp.lo
    case _ => tp
  }

  /** If `tp` is a TypeBounds instance return its upper bound else return `tp` */
  val boundsToHi = (tp: Type) => tp match {
    case tp: TypeBounds => tp.hi
    case _ => tp
  }

  /** Does variance `v1` conform to variance `v2`?
   *  This is the case if the variances are the same or `sym` is nonvariant.
   */
  def varianceConforms(v1: Int, v2: Int)(implicit ctx: Context): Boolean =
    v1 == v2 || v2 == 0

  /** Does the variance of type parameter `tparam1` conform to the variance of type parameter `tparam2`?
   */
  def varianceConforms(tparam1: TypeParamInfo, tparam2: TypeParamInfo)(implicit ctx: Context): Boolean =
    varianceConforms(tparam1.paramVariance, tparam2.paramVariance)

  /** Doe the variances of type parameters `tparams1` conform to the variances
   *  of corresponding type parameters `tparams2`?
   *  This is only the case of `tparams1` and `tparams2` have the same length.
   */
  def variancesConform(tparams1: List[TypeParamInfo], tparams2: List[TypeParamInfo])(implicit ctx: Context): Boolean =
    tparams1.corresponds(tparams2)(varianceConforms)

  /** Extractor for
   *
   *    [v1 X1: B1, ..., vn Xn: Bn] -> C[X1, ..., Xn]
   *
   *  where v1, ..., vn and B1, ..., Bn are the variances and bounds of the type parameters
   *  of the class C.
   *
   *  @param tycon     C
   */
  object EtaExpansion {
    def apply(tycon: Type)(implicit ctx: Context) = {
      assert(tycon.typeParams.nonEmpty, tycon)
      tycon.EtaExpand(tycon.typeParamSymbols)
    }

    def unapply(tp: Type)(implicit ctx: Context): Option[TypeRef] = tp match {
      case tp @ TypeLambda(tparams, AppliedType(fn: TypeRef, args)) if (args == tparams.map(_.toArg)) => Some(fn)
      case _ => None
    }
  }

  /** Extractor for type application T[U_1, ..., U_n]. This is the refined type
   *
   *     T { type p_1 v_1= U_1; ...; type p_n v_n= U_n }
   *
   *  where v_i, p_i are the variances and names of the type parameters of T.
   */
  object AppliedType {
    def apply(tp: Type, args: List[Type])(implicit ctx: Context): Type = tp.appliedTo(args)

    def unapply(tp: Type)(implicit ctx: Context): Option[(Type, List[Type])] = tp match {
      case tp: RefinedType =>
        var refinements: List[RefinedType] = Nil
        var tycon = tp.stripTypeVar
        while (tycon.isInstanceOf[RefinedType]) {
          val rt = tycon.asInstanceOf[RefinedType]
          refinements = rt :: refinements
          tycon = rt.parent.stripTypeVar
        }
        def collectArgs(tparams: List[TypeParamInfo],
                        refinements: List[RefinedType],
                        argBuf: mutable.ListBuffer[Type]): Option[(Type, List[Type])] = refinements match {
          case Nil if tparams.isEmpty && argBuf.nonEmpty =>
            Some((tycon, argBuf.toList))
          case RefinedType(_, rname, rinfo) :: refinements1
          if tparams.nonEmpty && rname == tparams.head.paramName =>
            collectArgs(tparams.tail, refinements1, argBuf += rinfo.argInfo)
          case _ =>
            None
        }
        collectArgs(tycon.typeParams, refinements, new mutable.ListBuffer[Type])
      case HKApply(tycon, args) =>
        Some((tycon, args))
      case _ =>
        None
    }
  }

   /** Adapt all arguments to possible higher-kinded type parameters using etaExpandIfHK
   */
  def etaExpandIfHK(tparams: List[TypeParamInfo], args: List[Type])(implicit ctx: Context): List[Type] =
    if (tparams.isEmpty) args
    else {
      def bounds(tparam: TypeParamInfo) = tparam match {
        case tparam: Symbol => tparam.infoOrCompleter
        case tparam: LambdaParam => tparam.paramBounds
      }
      args.zipWithConserve(tparams)((arg, tparam) => arg.etaExpandIfHK(bounds(tparam)))
    }

  /** A type map that tries to reduce a (part of) the result type of the type lambda `tycon`
   *  with the given `args`(some of which are wildcard arguments represented by type bounds).
   *  Non-wildcard arguments are substituted everywhere as usual. A wildcard argument
   *  `>: L <: H` is substituted for a type lambda parameter `X` only if `X` appears
   *  in a toplevel refinement of the form
   *
   *        { type A = X }
   *
   *  and there are no other occurrences of `X` in the reduced type. In that case
   *  the refinement above is replaced by
   *
   *        { type A >: L <: U }
   *
   *  The `allReplaced` field indicates whether all occurrences of type lambda parameters
   *  in the reduced type have been replaced with arguments.
   */
  class Reducer(tycon: TypeLambda, args: List[Type])(implicit ctx: Context) extends TypeMap {
    private var available = Set((0 until args.length): _*)
    var allReplaced = true
    def hasWildcardArg(p: PolyParam) =
      p.binder == tycon && args(p.paramNum).isInstanceOf[TypeBounds]
    def apply(t: Type) = t match {
      case t @ TypeAlias(p: PolyParam) if hasWildcardArg(p) && available.contains(p.paramNum) =>
        available -= p.paramNum
        args(p.paramNum)
      case p: PolyParam if p.binder == tycon =>
        if (hasWildcardArg(p)) { allReplaced = false; p }
        else args(p.paramNum)
      case _: TypeBounds | _: HKApply =>
        val saved = available
        available = Set()
        try mapOver(t)
        finally available = saved
      case _ =>
        mapOver(t)
    }
  }
}

import TypeApplications._

/** A decorator that provides methods for modeling type application */
class TypeApplications(val self: Type) extends AnyVal {

  /** The type parameters of this type are:
   *  For a ClassInfo type, the type parameters of its class.
   *  For a typeref referring to a class, the type parameters of the class.
   *  For a typeref referring to a Lambda class, the type parameters of
   *    its right hand side or upper bound.
   *  For a refinement type, the type parameters of its parent, dropping
   *  any type parameter that is-rebound by the refinement. "Re-bind" means:
   *  The refinement contains a TypeAlias for the type parameter, or
   *  it introduces bounds for the type parameter, and we are not in the
   *  special case of a type Lambda, where a LambdaTrait gets refined
   *  with the bounds on its hk args. See `LambdaAbstract`, where these
   *  types get introduced, and see `isBoundedLambda` below for the test.
   */
  final def typeParams(implicit ctx: Context): List[TypeParamInfo] = /*>|>*/ track("typeParams") /*<|<*/ {
    self match {
      case self: ClassInfo =>
        self.cls.typeParams
      case self: TypeLambda =>
        self.typeParams
      case self: TypeRef =>
        val tsym = self.symbol
        if (tsym.isClass) tsym.typeParams
        else if (!tsym.isCompleting) tsym.info.typeParams
        else Nil
      case self: RefinedType =>
        self.parent.typeParams.filterNot(_.paramName == self.refinedName)
      case self: RecType =>
        self.parent.typeParams
      case _: HKApply | _: SingletonType =>
        Nil
      case self: WildcardType =>
        self.optBounds.typeParams
      case self: TypeProxy =>
        self.underlying.typeParams
      case _ =>
        Nil
    }
  }

  /** If `self` is a higher-kinded type, its type parameters $hk_i, otherwise Nil */
  final def hkTypeParams(implicit ctx: Context): List[TypeParamInfo] =
    if (isHK) typeParams else Nil

  /** If `self` is a generic class, its type parameter symbols, otherwise Nil */
  final def typeParamSymbols(implicit ctx: Context): List[TypeSymbol] = typeParams match {
    case (_: Symbol) :: _ =>
      assert(typeParams.forall(_.isInstanceOf[Symbol]))
      typeParams.asInstanceOf[List[TypeSymbol]]
    case _ => Nil
  }

  /** The named type parameters declared or inherited by this type.
   *  These are all uninstantiated named type parameters of this type or one
   *  of its base types.
   */
  final def namedTypeParams(implicit ctx: Context): Set[TypeSymbol] = self match {
    case self: ClassInfo =>
      self.cls.namedTypeParams
    case self: RefinedType =>
      self.parent.namedTypeParams.filterNot(_.name == self.refinedName)
    case self: SingletonType =>
      Set()
    case self: TypeProxy =>
      self.underlying.namedTypeParams
    case _ =>
      Set()
  }

  /** The smallest supertype of this type that instantiated none of the named type parameters
   *  in `params`. That is, for each named type parameter `p` in `params`, either there is
   *  no type field named `p` in this type, or `p` is a named type parameter of this type.
   *  The first case is important for the recursive case of AndTypes, because some of their operands might
   *  be missing the named parameter altogether, but the AndType as a whole can still
   *  contain it.
   */
  final def widenToNamedTypeParams(params: Set[TypeSymbol])(implicit ctx: Context): Type = {

    /** Is widening not needed for `tp`? */
    def isOK(tp: Type) = {
      val ownParams = tp.namedTypeParams
      def isMissingOrOpen(param: TypeSymbol) = {
        val ownParam = tp.nonPrivateMember(param.name).symbol
        !ownParam.exists || ownParams.contains(ownParam.asType)
      }
      params.forall(isMissingOrOpen)
    }

    /** Widen type by forming the intersection of its widened parents */
    def widenToParents(tp: Type) = {
      val parents = tp.parents.map(p =>
        tp.baseTypeWithArgs(p.symbol).widenToNamedTypeParams(params))
      parents.reduceLeft(ctx.typeComparer.andType(_, _))
    }

    if (isOK(self)) self
    else self match {
      case self @ AppliedType(tycon, args) if !isOK(tycon) =>
        widenToParents(self)
      case self: TypeRef if self.symbol.isClass =>
        widenToParents(self)
      case self: RefinedType =>
        val parent1 = self.parent.widenToNamedTypeParams(params)
        if (params.exists(_.name == self.refinedName)) parent1
        else self.derivedRefinedType(parent1, self.refinedName, self.refinedInfo)
      case self: TypeProxy =>
        self.underlying.widenToNamedTypeParams(params)
      case self: AndOrType =>
        self.derivedAndOrType(
          self.tp1.widenToNamedTypeParams(params), self.tp2.widenToNamedTypeParams(params))
    }
  }

  /** Is self type higher-kinded (i.e. of kind != "*")? */
  def isHK(implicit ctx: Context): Boolean = self.dealias match {
    case self: TypeRef => self.info.isHK
    case self: RefinedType => false
    case self: TypeLambda => true
    case self: HKApply => false
    case self: SingletonType => false
    case self: TypeVar => self.origin.isHK // discrepancy with typeParams, why?
    case self: WildcardType => self.optBounds.isHK
    case self: TypeProxy => self.underlying.isHK
    case _ => false
  }

  /** Computes the kind of `self` without forcing anything.
   *  @return   1   if type is known to be higher-kinded
   *           -1   if type is known to be a * type
   *            0   if kind of `self` is unknown (because symbols have not yet completed)
   */
  def knownHK(implicit ctx: Context): Int = self match {
    case self: TypeRef =>
      val tsym = self.symbol
      if (tsym.isClass) -1
      else tsym.infoOrCompleter match {
        case completer: TypeParamsCompleter =>
          if (completer.completerTypeParams(tsym).nonEmpty) 1 else -1
        case _ =>
          if (!tsym.isCompleting || tsym.isAliasType) tsym.info.knownHK
          else 0
      }
    case self: RefinedType => -1
    case self: TypeLambda => 1
    case self: HKApply => -1
    case self: SingletonType => -1
    case self: TypeVar => self.origin.knownHK
    case self: WildcardType => self.optBounds.knownHK
    case self: PolyParam => self.underlying.knownHK
    case self: TypeProxy => self.underlying.knownHK
    case NoType | _: LazyType => 0
    case _ => -1
  }

  /** True if it can be determined without forcing that the class symbol
   *  of this application exists. Equivalent to
   *
   *    self.classSymbol.exists
   *
   *  but without forcing anything.
   */
  def safeIsClassRef(implicit ctx: Context): Boolean = self.stripTypeVar match {
    case self: RefinedOrRecType =>
      self.parent.safeIsClassRef
    case self: TypeRef =>
      self.denot.exists && {
        val sym = self.symbol
        sym.isClass ||
        sym.isCompleted && self.info.isAlias
      }
    case _ =>
      false
  }

  /** Dealias type if it can be done without forcing the TypeRef's info */
  def safeDealias(implicit ctx: Context): Type = self match {
    case self: TypeRef if self.denot.exists && self.symbol.isAliasType =>
      self.info.bounds.hi.stripTypeVar.safeDealias
    case _ =>
      self
  }

  /** Lambda abstract `self` with given type parameters. Examples:
   *
   *      type T[X] = U        becomes    type T = [X] -> U
   *      type T[X] >: L <: U  becomes    type T >: L <: ([X] -> U)
   *
   *  TODO: Handle parameterized lower bounds
   */
  def LambdaAbstract(tparams: List[Symbol])(implicit ctx: Context): Type = {
    def expand(tp: Type) =
      TypeLambda(
        tpnme.syntheticLambdaParamNames(tparams.length), tparams.map(_.variance))(
          tl => tparams.map(tparam => tl.lifted(tparams, tparam.info).bounds),
          tl => tl.lifted(tparams, tp))
    assert(!isHK, self)
    self match {
      case self: TypeAlias =>
        self.derivedTypeAlias(expand(self.alias))
      case self @ TypeBounds(lo, hi) =>
        self.derivedTypeBounds(lo, expand(hi))
      case _ => expand(self)
    }
  }

  /** Convert a type constructor `TC` which has type parameters `T1, ..., Tn`
   *  in a context where type parameters `U1,...,Un` are expected to
   *
   *     LambdaXYZ { Apply = TC[hk$0, ..., hk$n] }
   *
   *  Here, XYZ corresponds to the variances of
   *   - `U1,...,Un` if the variances of `T1,...,Tn` are pairwise compatible with `U1,...,Un`,
   *   - `T1,...,Tn` otherwise.
   *  v1 is compatible with v2, if v1 = v2 or v2 is non-variant.
   */
  def EtaExpand(tparams: List[TypeSymbol])(implicit ctx: Context): Type = {
    val tparamsToUse = if (variancesConform(typeParams, tparams)) tparams else typeParamSymbols
    self.appliedTo(tparams map (_.typeRef)).LambdaAbstract(tparamsToUse)
      //.ensuring(res => res.EtaReduce =:= self, s"res = $res, core = ${res.EtaReduce}, self = $self, hc = ${res.hashCode}")
  }

  /** Eta expand the prefix in front of any refinements. */
  def EtaExpandCore(implicit ctx: Context): Type = self.stripTypeVar match {
    case self: RefinedType =>
      self.derivedRefinedType(self.parent.EtaExpandCore, self.refinedName, self.refinedInfo)
    case _ =>
      self.EtaExpand(self.typeParamSymbols)
  }

  /** If self is not higher-kinded, eta expand it. */
  def ensureHK(implicit ctx: Context): Type =
    if (isHK) self else EtaExpansion(self)

  /** Eta expand if `self` is a (non-lambda) class reference and `bound` is a higher-kinded type */
  def etaExpandIfHK(bound: Type)(implicit ctx: Context): Type = {
    val hkParams = bound.hkTypeParams
    if (hkParams.isEmpty) self
    else self match {
      case self: TypeRef if self.symbol.isClass && self.typeParams.length == hkParams.length =>
        EtaExpansion(self)
      case _ => self
    }
  }

  /** If argument A and type parameter P are higher-kinded, adapt the variances
   *  of A to those of P, ensuring that the variances of the type lambda A
   *  agree with the variances of corresponding higher-kinded type parameters of P. Example:
   *
   *     class GenericCompanion[+CC[X]]
   *     GenericCompanion[List]
   *
   *  with adaptHkVariances, the argument `List` will expand to
   *
   *     [X] => List[X]
   *
   *  instead of
   *
   *     [+X] => List[X]
   *
   *  even though `List` is covariant. This adaptation is necessary to ignore conflicting
   *  variances in overriding members that have types of hk-type parameters such as
   *  `GenericCompanion[GenTraversable]` or `GenericCompanion[ListBuffer]`.
   *  When checking overriding, we need to validate the subtype relationship
   *
   *      GenericCompanion[[X] -> ListBuffer[X]] <: GenericCompanion[[+X] -> GenTraversable[X]]
   *
   *   Without adaptation, this would be false, and hence an overriding error would
   *   result. But with adaptation, the rhs argument will be adapted to
   *
   *     [X] -> GenTraversable[X]
   *
   *   which makes the subtype test succeed. The crucial point here is that, since
   *   GenericCompanion only expects a non-variant CC, the fact that GenTraversable
   *   is covariant is irrelevant, so can be ignored.
   */
  def adaptHkVariances(bound: Type)(implicit ctx: Context): Type = {
    val hkParams = bound.hkTypeParams
    if (hkParams.isEmpty) self
    else {
      def adaptArg(arg: Type): Type = arg match {
        case arg @ TypeLambda(tparams, body) if
             !tparams.corresponds(hkParams)(_.paramVariance == _.paramVariance) &&
             tparams.corresponds(hkParams)(varianceConforms) =>
          TypeLambda(tparams.map(_.paramName), hkParams.map(_.paramVariance))(
            tl => arg.paramBounds.map(_.subst(arg, tl).bounds),
            tl => arg.resultType.subst(arg, tl)
          )
        case arg @ TypeAlias(alias) =>
          arg.derivedTypeAlias(adaptArg(alias))
        case arg @ TypeBounds(lo, hi) =>
          arg.derivedTypeBounds(lo, adaptArg(hi))
        case _ =>
          arg
      }
      adaptArg(self)
    }
  }

  /** Encode
   *
   *     T[U1, ..., Un]
   *
   *  where
   *  @param  self   = `T`
   *  @param  args   = `U1,...,Un`
   *  performing the following simplifications
   *
   *  1. If `T` is an eta expansion `[X1,..,Xn] -> C[X1,...,Xn]` of class `C` compute
   *     `C[U1, ..., Un]` instead.
   *  2. If `T` is some other type lambda `[X1,...,Xn] -> S` none of the arguments
   *     `U1,...,Un` is a wildcard, compute `[X1:=U1, ..., Xn:=Un]S` instead.
   *  3. If `T` is a polytype, instantiate it to `U1,...,Un`.
   */
  final def appliedTo(args: List[Type])(implicit ctx: Context): Type = /*>|>*/ track("appliedTo") /*<|<*/ {
    if (args.isEmpty || ctx.erasedTypes) self
    else self.stripTypeVar match { // TODO investigate why we can't do safeDealias here
      case self: GenericType if !args.exists(_.isInstanceOf[TypeBounds]) =>
        self.instantiate(args)
      case EtaExpansion(self1) =>
        self1.appliedTo(args)
      case self1: WildcardType =>
        self1
      case _ =>
        self.appliedTo(args, typeParams)
    }
  }

  /** Encode application `T[U1, ..., Un]` without simplifications, where
   *  @param self     = `T`
   *  @param args     = `U1, ..., Un`
   *  @param tparams  are assumed to be the type parameters of `T`.
   */
  final def appliedTo(args: List[Type], typParams: List[TypeParamInfo])(implicit ctx: Context): Type = {
    def matchParams(t: Type, tparams: List[TypeParamInfo], args: List[Type])(implicit ctx: Context): Type = args match {
      case arg :: args1 =>
        try {
          val tparam :: tparams1 = tparams
          matchParams(RefinedType(t, tparam.paramName, arg.toBounds(tparam)), tparams1, args1)
        } catch {
          case ex: MatchError =>
            println(s"applied type mismatch: $self with underlying ${self.underlyingIfProxy}, args = $args, typeParams = $typParams") // !!! DEBUG
            //println(s"precomplete decls = ${self.typeSymbol.unforcedDecls.toList.map(_.denot).mkString("\n  ")}")
            throw ex
        }
      case nil => t
    }
    assert(args.nonEmpty)
    self.stripTypeVar.safeDealias match {
      case self: TypeLambda =>
        if (!args.exists(_.isInstanceOf[TypeBounds])) self.instantiate(args)
        else {
          val reducer = new Reducer(self, args)
          val reduced = reducer(self.resType)
          if (reducer.allReplaced) reduced
          else HKApply(self, args)
        }
      case self: AndOrType =>
        self.derivedAndOrType(self.tp1.appliedTo(args), self.tp2.appliedTo(args))
      case self: TypeAlias =>
        self.derivedTypeAlias(self.alias.appliedTo(args))
      case self: TypeBounds =>
        self.derivedTypeBounds(self.lo, self.hi.appliedTo(args))
      case self: LazyRef =>
        LazyRef(() => self.ref.appliedTo(args, typParams))
      case self: TypeRef if self.symbol == defn.NothingClass =>
        self
      case _ if typParams.isEmpty || typParams.head.isInstanceOf[LambdaParam] =>
        HKApply(self, args)
      case dealiased =>
        matchParams(dealiased, typParams, args)
    }
  }

  final def appliedTo(arg: Type)(implicit ctx: Context): Type = appliedTo(arg :: Nil)
  final def appliedTo(arg1: Type, arg2: Type)(implicit ctx: Context): Type = appliedTo(arg1 :: arg2 :: Nil)

  final def applyIfParameterized(args: List[Type])(implicit ctx: Context): Type =
    if (typeParams.nonEmpty) appliedTo(args) else self

  /** A cycle-safe version of `appliedTo` where computing type parameters do not force
   *  the typeconstructor. Instead, if the type constructor is completing, we make
   *  up hk type parameters matching the arguments. This is needed when unpickling
   *  Scala2 files such as `scala.collection.generic.Mapfactory`.
   */
  final def safeAppliedTo(args: List[Type])(implicit ctx: Context) = self match {
    case self: TypeRef if !self.symbol.isClass && self.symbol.isCompleting =>
      HKApply(self, args)
    case _ =>
      appliedTo(args, typeParams)
  }

  /** Turn this type, which is used as an argument for
   *  type parameter `tparam`, into a TypeBounds RHS
   */
  final def toBounds(tparam: TypeParamInfo)(implicit ctx: Context): TypeBounds = self match {
    case self: TypeBounds => // this can happen for wildcard args
      self
    case _ =>
      val v = tparam.paramVariance
      /* Not neeeded.
      if (v > 0 && !(tparam is Local) && !(tparam is ExpandedTypeParam)) TypeBounds.upper(self)
      else if (v < 0 && !(tparam is Local) && !(tparam is ExpandedTypeParam)) TypeBounds.lower(self)
      else
      */
      TypeAlias(self, v)
  }

  /** The type arguments of this type's base type instance wrt. `base`.
   *  Existential types in arguments are returned as TypeBounds instances.
   */
  final def baseArgInfos(base: Symbol)(implicit ctx: Context): List[Type] =
    if (self derivesFrom base)
      self match {
        case self: HKApply => self.upperBound.baseArgInfos(base)
        case _ => base.typeParams.map(param => self.member(param.name).info.argInfo)
      }
    else
      Nil

  /** The type arguments of this type's base type instance wrt.`base`.
   *  Existential types in arguments are disallowed.
   */
  final def baseArgTypes(base: Symbol)(implicit ctx: Context): List[Type] =
    baseArgInfos(base) mapConserve noBounds

  /** The type arguments of this type's base type instance wrt.`base`.
   *  Existential types in arguments are approximated by their lower bound.
   */
  final def baseArgTypesLo(base: Symbol)(implicit ctx: Context): List[Type] =
    baseArgInfos(base) mapConserve boundsToLo

  /** The type arguments of this type's base type instance wrt.`base`.
   *  Existential types in arguments are approximated by their upper bound.
   */
  final def baseArgTypesHi(base: Symbol)(implicit ctx: Context): List[Type] =
    baseArgInfos(base) mapConserve boundsToHi

  /** The first type argument of the base type instance wrt `base` of this type */
  final def firstBaseArgInfo(base: Symbol)(implicit ctx: Context): Type = base.typeParams match {
    case param :: _ if self derivesFrom base =>
      self match {
        case self: HKApply => self.upperBound.firstBaseArgInfo(base)
        case _ => self.member(param.name).info.argInfo
      }
    case _ =>
      NoType
  }

  /** The base type including all type arguments and applicable refinements
   *  of this type. Refinements are applicable if they refine a member of
   *  the parent type which furthermore is not a name-mangled type parameter.
   *  Existential types in arguments are returned as TypeBounds instances.
   */
  final def baseTypeWithArgs(base: Symbol)(implicit ctx: Context): Type = ctx.traceIndented(s"btwa ${self.show} wrt $base", core, show = true) {
    def default = self.baseTypeRef(base).appliedTo(baseArgInfos(base))
    self match {
      case tp: TypeRef =>
        tp.info match {
          case TypeBounds(_, hi) => hi.baseTypeWithArgs(base)
          case _ => default
        }
      case tp @ RefinedType(parent, name, _) if !tp.member(name).symbol.is(ExpandedTypeParam) =>
        tp.wrapIfMember(parent.baseTypeWithArgs(base))
      case tp: TermRef =>
        tp.underlying.baseTypeWithArgs(base)
      case tp: HKApply =>
        tp.upperBound.baseTypeWithArgs(base)
      case AndType(tp1, tp2) =>
        tp1.baseTypeWithArgs(base) & tp2.baseTypeWithArgs(base)
      case OrType(tp1, tp2) =>
        tp1.baseTypeWithArgs(base) | tp2.baseTypeWithArgs(base)
      case _ =>
        default
    }
  }

  /** Translate a type of the form From[T] to To[T], keep other types as they are.
   *  `from` and `to` must be static classes, both with one type parameter, and the same variance.
   *  Do the same for by name types => From[T] and => To[T]
   */
  def translateParameterized(from: ClassSymbol, to: ClassSymbol)(implicit ctx: Context): Type = self match {
    case self @ ExprType(tp) =>
      self.derivedExprType(tp.translateParameterized(from, to))
    case _ =>
      if (self.derivesFrom(from))
        if (ctx.erasedTypes) to.typeRef
        else RefinedType(to.typeRef, to.typeParams.head.name, self.member(from.typeParams.head.name).info)
      else self
  }

  /** If this is repeated parameter type, its underlying Seq type,
   *  or, if isJava is true, Array type, else the type itself.
   */
  def underlyingIfRepeated(isJava: Boolean)(implicit ctx: Context): Type =
    if (self.isRepeatedParam) {
      val seqClass = if (isJava) defn.ArrayClass else defn.SeqClass
      translateParameterized(defn.RepeatedParamClass, seqClass)
    }
    else self

  /** If this is an encoding of a (partially) applied type, return its arguments,
   *  otherwise return Nil.
   *  Existential types in arguments are returned as TypeBounds instances.
   */
  final def argInfos(implicit ctx: Context): List[Type] = self match {
    case AppliedType(tycon, args) => args
    case _ => Nil
  }

  /** Argument types where existential types in arguments are disallowed */
  def argTypes(implicit ctx: Context) = argInfos mapConserve noBounds

  /** Argument types where existential types in arguments are approximated by their lower bound */
  def argTypesLo(implicit ctx: Context) = argInfos mapConserve boundsToLo

  /** Argument types where existential types in arguments are approximated by their upper bound  */
  def argTypesHi(implicit ctx: Context) = argInfos mapConserve boundsToHi

  /** The core type without any type arguments.
   *  @param `typeArgs` must be the type arguments of this type.
   */
  final def withoutArgs(typeArgs: List[Type]): Type = self match {
    case HKApply(tycon, args) => tycon
    case _ =>
      typeArgs match {
        case _ :: typeArgs1 =>
          val RefinedType(tycon, _, _) = self
          tycon.withoutArgs(typeArgs1)
        case nil =>
          self
      }
  }

  final def typeConstructor(implicit ctx: Context): Type = self.stripTypeVar match {
    case AppliedType(tycon, _) => tycon
    case self => self
  }

  /** If this is the image of a type argument; recover the type argument,
   *  otherwise NoType.
   */
  final def argInfo(implicit ctx: Context): Type = self match {
    case self: TypeAlias => self.alias
    case self: TypeBounds => self
    case _ => NoType
  }

  /** If this is a type alias, its underlying type, otherwise the type itself */
  def dropAlias(implicit ctx: Context): Type = self match {
    case TypeAlias(alias) => alias
    case _ => self
  }

  /** The element type of a sequence or array */
  def elemType(implicit ctx: Context): Type = self match {
    case defn.ArrayOf(elemtp) => elemtp
    case JavaArrayType(elemtp) => elemtp
    case _ => firstBaseArgInfo(defn.SeqClass)
  }
}
