package dotty.tools
package dotc
package core

import Decorators._
import Symbols._
import Types._
import Flags._
import Contexts.ctx
import dotty.tools.dotc.reporting.trace
import config.Feature.migrateTo3
import config.Printers._

trait PatternTypeConstrainer { self: TypeComparer =>

  /** Derive type and GADT constraints that necessarily follow from a pattern with the given type matching
   *  a scrutinee of the given type.
   *
   *  This function breaks down scrutinee and pattern types into subcomponents between which there must be
   *  a subtyping relationship, and derives constraints from those relationships. We have the following situation
   *  in case of a (dynamic) pattern match:
   *
   *       StaticScrutineeType           PatternType
   *                         \            /
   *                      DynamicScrutineeType
   *
   *  In simple cases, it must hold that `PatternType <: StaticScrutineeType`:
   *
   *            StaticScrutineeType
   *                  |         \
   *                  |          PatternType
   *                  |         /
   *               DynamicScrutineeType
   *
   *  A good example of a situation where the above must hold is when static scrutinee type is the root of an enum,
   *  and the pattern is an unapply of a case class, or a case object literal (of that enum).
   *
   *  In slightly more complex cases, we may need to upcast `StaticScrutineeType`:
   *
   *           SharedPatternScrutineeSuperType
   *                  /         \
   *   StaticScrutineeType       PatternType
   *                  \         /
   *               DynamicScrutineeType
   *
   *  This may be the case if the scrutinee is a singleton type or a path-dependent type. It is also the case
   *  for the following definitions:
   *
   *     trait Expr[T]
   *     trait IntExpr extends Expr[T]
   *     trait Const[T] extends Expr[T]
   *
   *     StaticScrutineeType = Const[T]
   *     PatternType = IntExpr
   *
   *  Union and intersection types are an additional complication - if either scrutinee or pattern are a union type,
   *  then the above relationships only need to hold for the "leaves" of the types.
   *
   *  Finally, if pattern type contains hk-types applied to concrete types (as opposed to type variables),
   *  or either scrutinee or pattern type contain type member refinements, the above relationships do not need
   *  to hold at all. Consider (where `T1`, `T2` are unrelated traits):
   *
   *     StaticScrutineeType = { type T <: T1 }
   *     PatternType = { type T <: T2 }
   *
   *  In the above situation, DynamicScrutineeType can equal { type T = T1 & T2 }, but there is no useful relationship
   *  between StaticScrutineeType and PatternType (nor any of their subcomponents). Similarly:
   *
   *     StaticScrutineeType = Option[T1]
   *     PatternType = Some[T2]
   *
   *  Again, DynamicScrutineeType may equal Some[T1 & T2], and there's no useful relationship between the static
   *  scrutinee and pattern types. This does not apply if the pattern type is only applied to type variables,
   *  in which case the subtyping relationship "heals" the type.
   */
  def constrainPatternType(pat: Type, scrut: Type, forceInvariantRefinement: Boolean = false): Boolean = trace/*force*/(i"constrainPatternType($scrut, $pat)", gadts) {

    def classOrModuleSym(tp: Type) = {
      val immediate = tp match {
        case tp: NamedType =>
          dbg.println(i"immediate: ${tp.symbol}")
          tp.symbol.moduleClass
        case _ => NoSymbol
      }
      immediate.orElse(tp.classSymbol)
    }

    def classesMayBeCompatible: Boolean = {
      import Flags._
      val patCls = pat.classSymbol
      val scrCls = scrut.classSymbol
      !patCls.exists || !scrCls.exists || {
        if (patCls.is(Final)) patCls.derivesFrom(scrCls)
        else if (scrCls.is(Final)) scrCls.derivesFrom(patCls)
        else if (!patCls.is(Flags.Trait) && !scrCls.is(Flags.Trait))
          patCls.derivesFrom(scrCls) || scrCls.derivesFrom(patCls)
        else true
      }
    }

    def stripRefinement(tp: Type): Type = tp match {
      case tp: RefinedOrRecType => stripRefinement(tp.parent)
      case tp => tp
    }

    def tryConstrainSimplePatternType(pat: Type, scrut: Type) = {
      val patCls = classOrModuleSym(pat)
      val scrCls = classOrModuleSym(scrut)
      patCls.exists && scrCls.exists
      && (patCls.derivesFrom(scrCls) || scrCls.derivesFrom(patCls))
      && constrainSimplePatternType(pat, scrut, forceInvariantRefinement)
    }


    def constrainUpcasted(scrut: Type): Boolean = trace(i"constrainUpcasted($scrut)", gadts) {
      // Fold a list of types into an AndType
      def buildAndType(xs: List[Type]): Type = {
        @annotation.tailrec def recur(acc: Type, rem: List[Type]): Type = rem match {
          case Nil => acc
          case x :: rem => recur(AndType(acc, x), rem)
        }
        xs match {
          case Nil => NoType
          case x :: xs => recur(x, xs)
        }
      }

      scrut match {
        case scrut: TypeRef if scrut.symbol.isClass =>
          // consider all parents
          val parents = scrut.parents
          val andType = buildAndType(parents)
          !andType.exists || constrainPatternType(pat, andType)
        case scrut @ AppliedType(tycon: TypeRef, _) if tycon.symbol.isClass =>
          val patCls = pat.classSymbol
          // find all shared parents in the inheritance hierarchy between pat and scrut
          def allParentsSharedWithPat(tp: Type, tpClassSym: ClassSymbol): List[Symbol] = {
            var parents = tpClassSym.info.parents
            if parents.nonEmpty && parents.head.classSymbol == defn.ObjectClass then
              parents = parents.tail
            parents flatMap { tp =>
              val sym = tp.classSymbol.asClass
              if patCls.derivesFrom(sym) then List(sym)
              else allParentsSharedWithPat(tp, sym)
            }
          }
          val allSyms = allParentsSharedWithPat(tycon, tycon.symbol.asClass)
          val baseClasses = allSyms map scrut.baseType
          val andType = buildAndType(baseClasses)
          !andType.exists || constrainPatternType(pat, andType)
        case _ =>
          val upcasted: Type = scrut match {
            case scrut: TypeProxy => scrut.superType
            case _ => NoType
          }
          if (upcasted.exists)
            tryConstrainSimplePatternType(pat, upcasted) || constrainUpcasted(upcasted)
          else true
      }
    }

    def dealiasDropNonmoduleRefs(tp: Type) = tp.dealias match {
      case tp: TermRef =>
        if tp.classSymbol.exists then tp else tp.info
      case tp => tp
    }

    dealiasDropNonmoduleRefs(scrut) match {
      case OrType(scrut1, scrut2) =>
        either(constrainPatternType(pat, scrut1), constrainPatternType(pat, scrut2))
      case AndType(scrut1, scrut2) =>
        constrainPatternType(pat, scrut1) && constrainPatternType(pat, scrut2)
      case scrut: RefinedOrRecType =>
        constrainPatternType(pat, stripRefinement(scrut))
      case scrut => dealiasDropNonmoduleRefs(pat) match {
        case OrType(pat1, pat2) =>
          either(constrainPatternType(pat1, scrut), constrainPatternType(pat2, scrut))
        case AndType(pat1, pat2) =>
          constrainPatternType(pat1, scrut) && constrainPatternType(pat2, scrut)
        case pat: RefinedOrRecType =>
          constrainPatternType(stripRefinement(pat), scrut)
        case pat =>
          tryConstrainSimplePatternType(pat, scrut)
          || classesMayBeCompatible && constrainUpcasted(scrut)
      }
    }
  }

  /** Constrain "simple" patterns (see `constrainPatternType`).
   *
   *  This function attempts to modify pattern and scrutinee type s.t. the pattern must be a subtype of the scrutinee,
   *  or otherwise it cannot possibly match. In order to do that, we:
   *
   *  1. Rely on `constrainPatternType` to break the actual scrutinee/pattern types into subcomponents
   *  2. Widen type parameters of scrutinee type that are not invariantly refined (see below) by the pattern type.
   *  3. Wrap the pattern type in a skolem to avoid overconstraining top-level abstract types in scrutinee type
   *  4. Check that `WidenedScrutineeType <: NarrowedPatternType`
   *
   *  Importantly, note that the pattern type may contain type variables.
   *
   *  ## Invariant refinement
   *  Essentially, we say that `D[B] extends C[B]` s.t. refines parameter `A` of `trait C[A]` invariantly if
   *  when `c: C[T]` and `c` is instance of `D`, then necessarily `c: D[T]`. This is violated if `A` is variant:
   *
   *     trait C[+A]
   *     trait D[+B](val b: B) extends C[B]
   *     trait E extends D[Any](0) with C[String]
   *
   *  `E` is a counter-example to the above - if `e: E`, then `e: C[String]` and `e` is instance of `D`, but
   *  it is false that `e: D[String]`! This is a problem if we're constraining a pattern like the below:
   *
   *     def foo[T](c: C[T]): T = c match {
   *       case d: D[t] => d.b
   *     }
   *
   *  It'd be unsound for us to say that `t <: T`, even though that follows from `D[t] <: C[T]`.
   *  Note, however, that if `D` was a final class, we *could* rely on that relationship.
   *  To support typical case classes, we also assume that this relationship holds for them and their parent traits.
   *  This is enforced by checking that classes inheriting from case classes do not extend the parent traits of those
   *  case classes without also appropriately extending the relevant case class
   *  (see `RefChecks#checkCaseClassInheritanceInvariant`).
   */
  def constrainSimplePatternType(patternTp: Type, scrutineeTp: Type, forceInvariantRefinement: Boolean): Boolean = {
    def refinementIsInvariant(tp: Type): Boolean = tp match {
      case tp: SingletonType => true
      case tp: ClassInfo => tp.cls.is(Final) || tp.cls.is(Case)
      case tp: TypeProxy => refinementIsInvariant(tp.underlying)
      case _ => false
    }

    def widenVariantParams(tp: Type) = tp match {
      case tp @ AppliedType(tycon, args) =>
        val args1 = args.zipWithConserve(tycon.typeParams)((arg, tparam) =>
          if (tparam.paramVarianceSign != 0) TypeBounds.empty else arg
        )
        tp.derivedAppliedType(tycon, args1)
      case tp =>
        tp
    }

    val patternCls = patternTp.classSymbol
    val scrutineeCls = scrutineeTp.classSymbol

    // NOTE: we already know that there is a derives-from relationship in either direction
    val upcastPattern =
      patternCls.derivesFrom(scrutineeCls)

    val pt = if upcastPattern then patternTp.baseType(scrutineeCls) else patternTp
    val tp = if !upcastPattern then scrutineeTp.baseType(patternCls) else scrutineeTp

    val assumeInvariantRefinement =
      migrateTo3 || forceInvariantRefinement || refinementIsInvariant(patternTp)

    trace/*force*/(i"constraining simple pattern type $tp >:< $pt", gadts, res => s"$res\ngadt = ${ctx.gadt.debugBoundsDescription}") {
      (tp, pt) match {
        case (AppliedType(tyconS, argsS), AppliedType(tyconP, argsP)) =>
          tyconS.typeParams.lazyZip(argsS).lazyZip(argsP).forall { (param, argS, argP) =>
            val variance = param.paramVarianceSign
            if variance != 0 && !assumeInvariantRefinement then true
            else if argS.isInstanceOf[TypeBounds] || argP.isInstanceOf[TypeBounds] then true
            else {
              var res = true
              if variance <  1 then res &&= isSubType(argS, argP)
              if variance > -1 then res &&= isSubType(argP, argS)
              res
            }
          }
        case _ =>
          // give up if we don't get AppliedType, for instance if we upcasted to
          // Any. Note that this false doesn't mean a contradictory constraint,
          // but rather that
          false
      }
    }
  }
}
