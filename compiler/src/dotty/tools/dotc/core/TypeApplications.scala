package dotty.tools
package dotc
package core

import Types._
import Contexts._
import Symbols._
import SymDenotations.LazyType
import Decorators._
import util.Stats._
import Names._
import Flags.Module
import dotty.tools.dotc.config.Config

object TypeApplications {

  type TypeParamInfo = ParamInfo.Of[TypeName]

  /** Assert type is not a TypeBounds instance and return it unchanged */
  def noBounds(tp: Type): Type = tp match {
    case tp: TypeBounds => throw new AssertionError("no TypeBounds allowed")
    case _ => tp
  }

  /** Extractor for
   *
   *    [X1: B1, ..., Xn: Bn] -> C[X1, ..., Xn]
   *
   *  where B1, ..., Bn are bounds of the type parameters of the class C.
   *
   *  @param tycon     C
   */
  object EtaExpansion:

    def apply(tycon: Type)(using Context): Type =
      assert(tycon.typeParams.nonEmpty, tycon)
      tycon.EtaExpand(tycon.typeParamSymbols)

    /** Test that the parameter bounds in a hk type lambda `[X1,...,Xn] => C[X1, ..., Xn]`
     *  contain the bounds of the type parameters of `C`. This is necessary to be able to
     *  contract the hk lambda to `C`.
     */
    private def weakerBounds(tp: HKTypeLambda, tparams: List[ParamInfo])(using Context): Boolean =
      val onlyEmptyBounds = tp.typeParams.forall(_.paramInfo == TypeBounds.empty)
      onlyEmptyBounds
        // Note: this pre-test helps efficiency. It is also necessary to workaround  #9965 since in some cases
        // tparams is empty. This can happen when we change the owners of inlined local
        // classes in mapSymbols. See pos/reference/delegates.scala for an example.
        // In this case, we can still return true if we know that the hk lambda bounds
        // are empty anyway.
      || {
        val paramRefs = tparams.map(_.paramRef)
        tp.typeParams.corresponds(tparams) { (param1, param2) =>
          param2.paramInfo frozen_<:< param1.paramInfo.substParams(tp, paramRefs)
        }
      }

    def unapply(tp: Type)(using Context): Option[Type] = tp match
      case tp @ HKTypeLambda(tparams, AppliedType(fn: Type, args))
      if tparams.hasSameLengthAs(args)
         && args.lazyZip(tparams).forall((arg, tparam) => arg == tparam.paramRef)
         && weakerBounds(tp, fn.typeParams) => Some(fn)
      case _ => None

  end EtaExpansion

   /** Adapt all arguments to possible higher-kinded type parameters using etaExpandIfHK
   */
  def EtaExpandIfHK(tparams: List[TypeParamInfo], args: List[Type])(using Context): List[Type] =
    if (tparams.isEmpty) args
    else args.zipWithConserve(tparams)((arg, tparam) => arg.EtaExpandIfHK(tparam.paramInfoOrCompleter))

  /** A type map that tries to reduce (part of) the result type of the type lambda `tycon`
   *  with the given `args`(some of which are wildcard arguments represented by type bounds).
   *  Non-wildcard arguments are substituted everywhere as usual. A wildcard argument
   *  `>: L <: H` is substituted for a type lambda parameter `X` only under certain conditions.
   *
   *  1. If Mode.AllowLambdaWildcardApply is set:
   *  The wildcard argument is substituted only if `X` appears in a toplevel application of the form
   *
   *        C[..., X, ...]
   *
   *  and there are no other occurrences of `X` in the reduced type. In that case
   *  the refinement above is replaced by
   *
   *        C[..., ? >: L <: H, ...]
   *
   *  The `allReplaced` field indicates whether all occurrences of type lambda parameters
   *  in the reduced type have been replaced with arguments.
   *
   *  2. If Mode.AllowLambdaWildcardApply is not set:
   *  All `X` arguments are replaced by:
   *
   *        ? >: L <: H
   *
   *  Any other occurrence of `X` in `tycon` is replaced by `U`, if the
   *  occurrence of `X` in `tycon` is covariant, or nonvariant, or by `L`,
   *  if the occurrence is contravariant.
   *
   *  The idea is that the `AllowLambdaWildcardApply` mode is used to check whether
   *  a type can be soundly reduced, and to give an error or warning if that
   *  is not the case. By contrast, the default mode, with `AllowLambdaWildcardApply`
   *  not set, reduces all applications even if this yields a different type, so
   *  its postcondition is that no type parameters of `tycon` appear in the
   *  result type. Using this mode, we can guarantee that `appliedTo` will never
   *  produce a higher-kinded application with a type lambda as type constructor.
   */
  class Reducer(tycon: TypeLambda, args: List[Type])(using Context) extends TypeMap {
    private var available = (0 until args.length).toSet
    var allReplaced: Boolean = true
    def hasWildcardArg(p: TypeParamRef): Boolean =
      p.binder == tycon && isBounds(args(p.paramNum))
    def canReduceWildcard(p: TypeParamRef): Boolean =
      !ctx.mode.is(Mode.AllowLambdaWildcardApply) || available.contains(p.paramNum)
    def atNestedLevel(op: => Type): Type = {
      val saved = available
      available = Set()
      try op
      finally available = saved
    }

    // If this is a reference to a reducable type parameter corresponding to a
    // wildcard argument, return the wildcard argument, otherwise apply recursively.
    def applyArg(arg: Type): Type = arg match {
      case p: TypeParamRef if hasWildcardArg(p) && canReduceWildcard(p) =>
        available -= p.paramNum
        args(p.paramNum)
      case _ =>
        atNestedLevel(apply(arg))
    }

    def apply(t: Type): Type = t match {
      case t @ AppliedType(tycon, args1) if tycon.typeSymbol.isClass =>
        t.derivedAppliedType(apply(tycon), args1.mapConserve(applyArg))
      case t @ RefinedType(parent, name, TypeAlias(info)) =>
        t.derivedRefinedType(apply(parent), name, applyArg(info).bounds)
      case p: TypeParamRef if p.binder == tycon =>
        args(p.paramNum) match {
          case TypeBounds(lo, hi) =>
            if (ctx.mode.is(Mode.AllowLambdaWildcardApply)) { allReplaced = false; p }
            else if (variance < 0) lo
            else hi
          case arg =>
            arg
        }
      case _: TypeBounds | _: AppliedType =>
        atNestedLevel(mapOver(t))
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
   *  For a refinement type, the type parameters of its parent, dropping
   *  any type parameter that is-rebound by the refinement.
   */
  final def typeParams(using Context): List[TypeParamInfo] = {
    record("typeParams")
    def isTrivial(prefix: Type, tycon: Symbol) = prefix match {
      case prefix: ThisType =>
        prefix.cls eq tycon.owner
      case prefix: TermRef =>
        val sym = prefix.symbol
        sym.is(Module) && sym.isStatic && (sym.moduleClass eq tycon.owner)
      case NoPrefix => true
      case _ => false
    }
    try self match {
      case self: TypeRef =>
        val tsym = self.symbol
        if (tsym.isClass) tsym.typeParams
        else tsym.infoOrCompleter match {
          case info: LazyType if isTrivial(self.prefix, tsym) => info.completerTypeParams(tsym)
          case _ => self.info.typeParams
        }
      case self: AppliedType =>
        if (self.tycon.typeSymbol.isClass) Nil
        else self.superType.typeParams
      case self: ClassInfo =>
        self.cls.typeParams
      case self: HKTypeLambda =>
        self.typeParams
      case _: SingletonType | _: RefinedType | _: RecType =>
        Nil
      case self: WildcardType =>
        self.optBounds.typeParams
      case self: TypeProxy =>
        self.superType.typeParams
      case _ =>
        Nil
    }
    catch {
      case ex: Throwable => handleRecursive("type parameters of", self.show, ex)
    }
  }

  /** If `self` is a higher-kinded type, its type parameters, otherwise Nil */
  final def hkTypeParams(using Context): List[TypeParamInfo] =
    if (isLambdaSub) typeParams else Nil

  /** If `self` is a generic class, its type parameter symbols, otherwise Nil */
  final def typeParamSymbols(using Context): List[TypeSymbol] = typeParams match {
    case tparams @ (_: Symbol) :: _ =>
      assert(tparams.forall(_.isInstanceOf[Symbol]))
      tparams.asInstanceOf[List[TypeSymbol]]
        // Note: Two successive calls to typeParams can yield different results here because
        // of different completion status. I.e. the first call might produce some symbols,
        // whereas the second call gives some LambdaParams. This was observed
        // for ticket0137.scala
    case _ => Nil
  }

  /** Is self type bounded by a type lambda or AnyKind? */
  def isLambdaSub(using Context): Boolean = hkResult.exists

  /** Is self type of kind "*"? */
  def hasSimpleKind(using Context): Boolean =
    typeParams.isEmpty && !self.hasAnyKind || {
      val alias = self.dealias
      (alias ne self) && alias.hasSimpleKind
    }

  /** The top type with the same kind as `self`. This is largest type capturing
   *  the parameter shape of a type without looking at precise bounds.
   *    - The top-type of simple-kinded types is Any
   *    - A kind like (* -> *) -> * is represented by the top type [X1 <: [X2] =>> Any] =>> Any
   */
  def topType(using Context): Type =
    if self.hasSimpleKind then
      defn.AnyType
    else EtaExpand(self.typeParams) match
      case tp: HKTypeLambda =>
        tp.derivedLambdaType(resType = tp.resultType.topType)
      case _ =>
        defn.AnyKindType

  /** If self type is higher-kinded, its result type, otherwise NoType.
   *  Note: The hkResult of an any-kinded type is again AnyKind.
   */
  def hkResult(using Context): Type = self.dealias match {
    case self: TypeRef =>
      if (self.symbol == defn.AnyKindClass) self else self.info.hkResult
    case self: AppliedType =>
      if (self.tycon.typeSymbol.isClass) NoType else self.superType.hkResult
    case self: HKTypeLambda => self.resultType
    case _: SingletonType | _: RefinedType | _: RecType => NoType
    case self: WildcardType => self.optBounds.hkResult
    case self: TypeVar =>
      // Using `origin` instead of `underlying`, as is done for typeParams,
      // avoids having to set ephemeral in some cases.
      self.origin.hkResult
    case self: TypeProxy => self.superType.hkResult
    case _ => NoType
  }

  /** Do self and other have the same kinds (not counting bounds and variances)?
   *  Note: An any-kinded type "has the same kind" as any other type.
   */
  def hasSameKindAs(other: Type)(using Context): Boolean = {
    def isAnyKind(tp: Type) = tp match {
      case tp: TypeRef => tp.symbol == defn.AnyKindClass
      case _ => false
    }
    val selfResult = self.hkResult
    val otherResult = other.hkResult
    isAnyKind(selfResult) || isAnyKind(otherResult) ||
    { if (selfResult.exists)
        otherResult.exists &&
        selfResult.hasSameKindAs(otherResult) &&
        self.typeParams.corresponds(other.typeParams)((sparam, oparam) =>
          sparam.paramInfo.hasSameKindAs(oparam.paramInfo))
      else !otherResult.exists
    }
  }

  /** Dealias type if it can be done without forcing the TypeRef's info */
  def safeDealias(using Context): Type = self match {
    case self: TypeRef if self.denot.exists && self.symbol.isAliasType =>
      self.superType.stripTypeVar.safeDealias
    case _ =>
      self
  }

  /** Convert a type constructor `TC` which has type parameters `X1, ..., Xn`
   *  to `[X1, ..., Xn] -> TC[X1, ..., Xn]`.
   */
  def EtaExpand(tparams: List[TypeParamInfo])(using Context): Type =
    HKTypeLambda.fromParams(tparams, self.appliedTo(tparams.map(_.paramRef)))
      //.ensuring(res => res.EtaReduce =:= self, s"res = $res, core = ${res.EtaReduce}, self = $self, hc = ${res.hashCode}")

  /** If self is not lambda-bound, eta expand it. */
  def ensureLambdaSub(using Context): Type =
    if (isLambdaSub) self else EtaExpansion(self)

  /** Eta expand if `self` is a (non-lambda) class reference and `bound` is a higher-kinded type */
  def EtaExpandIfHK(bound: Type)(using Context): Type = {
    val hkParams = bound.hkTypeParams
    if (hkParams.isEmpty) self
    else self match {
      case self: TypeRef if self.symbol.isClass && self.typeParams.length == hkParams.length =>
        EtaExpansion(self)
      case _ => self
    }
  }

  /** The type representing
   *
   *     T[U1, ..., Un]
   *
   *  where
   *  @param  self   = `T`
   *  @param  args   = `U1,...,Un`
   */
  final def appliedTo(args: List[Type])(using Context): Type = {
    record("appliedTo")
    val typParams = self.typeParams
    val stripped = self.stripTypeVar
    val dealiased = stripped.safeDealias
    if (args.isEmpty || ctx.erasedTypes) self
    else dealiased match {
      case dealiased: HKTypeLambda =>
        def tryReduce =
          if (!args.exists(isBounds)) {
            val followAlias = (Config.simplifyApplications || self.typeSymbol.isPrivate) && {
              dealiased.resType match {
                case AppliedType(tyconBody, dealiasedArgs) =>
                  // Reduction should not affect type inference when it's
                  // just eta-reduction (ignoring variance annotations).
                  // See i2201*.scala for examples where more aggressive
                  // reduction would break type inference.
                  dealiased.paramRefs == dealiasedArgs ||
                  defn.isCompiletimeAppliedType(tyconBody.typeSymbol)
                case _ => false
              }
            }
            if ((dealiased eq stripped) || followAlias)
              try
                val instantiated = dealiased.instantiate(args)
                if (followAlias) instantiated.normalized else instantiated
              catch
                case ex: IndexOutOfBoundsException =>
                  AppliedType(self, args)
                case ex: Throwable =>
                  handleRecursive("try to instantiate", i"$dealiased[$args%, %]", ex)

            else AppliedType(self, args)
          }
          else dealiased.resType match {
            case AppliedType(tycon, args1) if tycon.safeDealias ne tycon =>
              // In this case we should always dealias since we cannot handle
              // higher-kinded applications to wildcard arguments.
              dealiased
                .derivedLambdaType(resType = tycon.safeDealias.appliedTo(args1))
                .appliedTo(args)
            case _ =>
              val reducer = new Reducer(dealiased, args)
              val reduced = reducer(dealiased.resType)
              if (reducer.allReplaced) reduced
              else AppliedType(dealiased, args)
          }
        tryReduce
      case dealiased: PolyType =>
        dealiased.instantiate(args)
      case dealiased: AndType =>
        dealiased.derivedAndType(dealiased.tp1.appliedTo(args), dealiased.tp2.appliedTo(args))
      case dealiased: OrType =>
        dealiased.derivedOrType(dealiased.tp1.appliedTo(args), dealiased.tp2.appliedTo(args))
      case dealiased: AliasingBounds =>
        dealiased.derivedAlias(dealiased.alias.appliedTo(args))
      case dealiased: TypeBounds =>
        dealiased.derivedTypeBounds(dealiased.lo.appliedTo(args), dealiased.hi.appliedTo(args))
      case dealiased: LazyRef =>
        LazyRef.of(dealiased.ref.appliedTo(args))
      case dealiased: WildcardType =>
        WildcardType(dealiased.optBounds.orElse(TypeBounds.empty).appliedTo(args).bounds)
      case dealiased: TypeRef if dealiased.symbol == defn.NothingClass =>
        dealiased
      case dealiased =>
        AppliedType(self, args)
    }
  }

  final def appliedTo(arg: Type)(using Context): Type = appliedTo(arg :: Nil)
  final def appliedTo(arg1: Type, arg2: Type)(using Context): Type = appliedTo(arg1 :: arg2 :: Nil)

  final def applyIfParameterized(args: List[Type])(using Context): Type =
    if (typeParams.nonEmpty) appliedTo(args) else self

  /** A cycle-safe version of `appliedTo` where computing type parameters do not force
   *  the typeconstructor. Instead, if the type constructor is completing, we make
   *  up hk type parameters matching the arguments. This is needed when unpickling
   *  Scala2 files such as `scala.collection.generic.Mapfactory`.
   */
  final def safeAppliedTo(args: List[Type])(using Context): Type = self match {
    case self: TypeRef if !self.symbol.isClass && self.symbol.isCompleting =>
      AppliedType(self, args)
    case _ =>
      appliedTo(args)
  }

  /** Turns non-bounds types to type bounds.
   *  A (possible lambda abstracted) match type is turned into a match alias.
   *  Every other type is turned into a type alias
   */
  final def toBounds(using Context): TypeBounds = self match {
    case self: TypeBounds => self // this can happen for wildcard args
    case _ => if (self.isMatch) MatchAlias(self) else TypeAlias(self)
  }

  /** Translate a type of the form From[T] to either To[T] or To[? <: T] (if `wildcardArg` is set). Keep other types as they are.
   *  `from` and `to` must be static classes, both with one type parameter, and the same variance.
   *  Do the same for by name types => From[T] and => To[T]
   */
  def translateParameterized(from: ClassSymbol, to: ClassSymbol, wildcardArg: Boolean = false)(using Context): Type = self match {
    case self @ ExprType(tp) =>
      self.derivedExprType(tp.translateParameterized(from, to))
    case _ =>
      if (self.derivesFrom(from)) {
        def elemType(tp: Type): Type = tp.widenDealias match
          case tp: OrType =>
            if tp.tp1.isBottomType then elemType(tp.tp2)
            else if tp.tp2.isBottomType then elemType(tp.tp1)
            else tp.derivedOrType(elemType(tp.tp1), elemType(tp.tp2))
          case tp: AndType => tp.derivedAndType(elemType(tp.tp1), elemType(tp.tp2))
          case _ => tp.baseType(from).argInfos.headOption.getOrElse(defn.NothingType)
        val arg = elemType(self)
        val arg1 = if (wildcardArg) TypeBounds.upper(arg) else arg
        to.typeRef.appliedTo(arg1)
      }
      else self
  }

  /** If this is a repeated parameter `*T`, translate it to either `Seq[T]` or
   *  `Array[? <: T]` depending on the value of `toArray`.
   *  Additionally, if `translateWildcard` is true, a wildcard type
   *  will be translated to `*<?>`.
   *  Other types are kept as-is.
   */
  def translateFromRepeated(toArray: Boolean, translateWildcard: Boolean = false)(using Context): Type =
    val seqClass = if (toArray) defn.ArrayClass else defn.SeqClass
    if translateWildcard && self.isInstanceOf[WildcardType] then
      seqClass.typeRef.appliedTo(WildcardType)
    else if self.isRepeatedParam then
      // We want `Array[? <: T]` because arrays aren't covariant until after
      // erasure. See `tests/pos/i5140`.
      translateParameterized(defn.RepeatedParamClass, seqClass, wildcardArg = toArray)
    else self

  /** Translate a `From[T]` into a `*T`. */
  def translateToRepeated(from: ClassSymbol)(using Context): Type =
    translateParameterized(from, defn.RepeatedParamClass)

  /** Translate `T` to `T & Object` in the situations where an `Array[T]`
   *  coming from Java would need to be interpreted as an `Array[T & Object]`
   *  to be erased correctly.
   *
   *  `Object` is the top-level type in Java, but when it appears in a Java
   *  signature we replace it by a special `FromJavaObject` type for
   *  convenience, this in turns requires us to special-case generic arrays as
   *  described in case 3 in the documentation of `FromJavaObjectSymbol`. This
   *  is necessary because a fully generic Java array erases to an array of
   *  Object, whereas a fully generic Scala array erases to Object to allow
   *  primitive arrays as subtypes.
   *
   *  Note: According to
   *  <http://cr.openjdk.java.net/~briangoetz/valhalla/sov/02-object-model.html>,
   *  it's possible that future JVMs will consider that:
   *
   *      int[] <: Integer[] <: Object[]
   *
   *  So hopefully our grand-children will not have to deal with this non-sense!
   */
  def translateJavaArrayElementType(using Context): Type =
    // A type parameter upper-bounded solely by `FromJavaObject` has `ObjectClass` as its classSymbol
    if self.typeSymbol.isAbstractOrParamType && (self.classSymbol eq defn.ObjectClass) then
      // The order is important here since Java intersections erase to their first operand
      AndType(defn.ObjectType, self)
    else
      self

  /** If this is an encoding of a (partially) applied type, return its arguments,
   *  otherwise return Nil.
   *  Existential types in arguments are returned as TypeBounds instances.
   */
  final def argInfos(using Context): List[Type] = self.stripped match {
    case AppliedType(tycon, args) => args
    case _ => Nil
  }

  /** Argument types where existential types in arguments are disallowed */
  def argTypes(using Context): List[Type] = argInfos mapConserve noBounds

  /** Argument types where existential types in arguments are approximated by their lower bound */
  def argTypesLo(using Context): List[Type] = argInfos.mapConserve(_.loBound)

  /** Argument types where existential types in arguments are approximated by their upper bound  */
  def argTypesHi(using Context): List[Type] = argInfos.mapConserve(_.hiBound)

  /** If this is the image of a type argument; recover the type argument,
   *  otherwise NoType.
   */
  final def argInfo(using Context): Type = self match {
    case self: TypeAlias => self.alias
    case self: TypeBounds => self
    case _ => NoType
  }

  /** If this is a type alias, its underlying type, otherwise the type itself */
  def dropAlias(using Context): Type = self match {
    case TypeAlias(alias) => alias
    case _ => self
  }

  /** The element type of a sequence or array */
  def elemType(using Context): Type = self.widenDealias match {
    case defn.ArrayOf(elemtp) => elemtp
    case JavaArrayType(elemtp) => elemtp
    case tp: OrType if tp.tp1.isBottomType => tp.tp2.elemType
    case tp: OrType if tp.tp2.isBottomType => tp.tp1.elemType
    case _ => self.baseType(defn.SeqClass).argInfos.headOption.getOrElse(NoType)
  }
}
