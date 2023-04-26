package dotty.tools
package dotc
package core

import Contexts._, Types._, Symbols._, Names._, Flags._
import Denotations.SingleDenotation
import Decorators._
import collection.mutable
import config.SourceVersion.future
import config.Feature.sourceVersion

/** Realizability status */
object CheckRealizable {

  sealed abstract class Realizability(val msg: String) {
    def andAlso(other: => Realizability): Realizability =
      if (this == Realizable) other else this
    def mapError(f: Realizability => Realizability): Realizability =
      if (this == Realizable) this else f(this)
  }

  object Realizable extends Realizability("")

  object NotConcrete extends Realizability(" is not a concrete type")

  class NotFinal(sym: Symbol)(using Context)
  extends Realizability(i" refers to nonfinal $sym")

  class HasProblemBounds(name: Name, info: Type)(using Context)
  extends Realizability(i" has a member $name with possibly conflicting bounds ${info.bounds.lo} <: ... <: ${info.bounds.hi}")

  class HasProblemBaseArg(typ: Type, argBounds: TypeBounds)(using Context)
  extends Realizability(i" has a base type $typ with possibly conflicting parameter bounds ${argBounds.lo} <: ... <: ${argBounds.hi}")

  class HasProblemBase(base1: Type, base2: Type)(using Context)
  extends Realizability(i" has conflicting base types $base1 and $base2")

  class HasProblemField(fld: SingleDenotation, problem: Realizability)(using Context)
  extends Realizability(i" has a member $fld which is not a legal path\nsince ${fld.symbol.name}: ${fld.info}${problem.msg}")

  class ProblemInUnderlying(tp: Type, problem: Realizability)(using Context)
  extends Realizability(i"s underlying type ${tp}${problem.msg}") {
    assert(problem != Realizable)
  }

  def realizability(tp: Type)(using Context): Realizability =
    new CheckRealizable().realizability(tp)

  def boundsRealizability(tp: Type)(using Context): Realizability =
    new CheckRealizable().boundsRealizability(tp)

  private val LateInitializedFlags = Lazy | Erased
}

/** Compute realizability status.
  *
  * A type T is realizable iff it is inhabited by non-null values. This ensures that its type members have good bounds
  * (in the sense from DOT papers). A type projection T#L is legal if T is realizable, and can be understood as
  * Scala 2's `v.L forSome { val v: T }`.
  *
  * In general, a realizable type can have multiple inhabitants, hence it need not be stable (in the sense of
  * Type.isStable).
  */
class CheckRealizable(using Context) {
  import CheckRealizable._

  /** A set of all fields that have already been checked. Used
   *  to avoid infinite recursions when analyzing recursive types.
   */
  private val checkedFields: mutable.Set[Symbol] = mutable.LinkedHashSet[Symbol]()

  /** Is symbol's definitition a lazy or erased val?
   *  (note we exclude modules here, because their realizability is ensured separately)
   */
  private def isLateInitialized(sym: Symbol) = sym.isOneOf(LateInitializedFlags, butNot = Module)

  /** The realizability status of given type `tp`*/
  def realizability(tp: Type): Realizability = tp.dealias match {
    /*
     * A `TermRef` for a path `p` is realizable if
     * - `p`'s type is stable and realizable, or
     * - its underlying path is idempotent (that is, *stable*), total, and not null.
     * We don't check yet the "not null" clause: that will require null-safety checking.
     *
     * We assume that stability of tp.prefix is checked elsewhere, since that's necessary for the path to be legal in
     * the first place.
     */
    case tp: TermRef =>
      val sym = tp.symbol
      lazy val tpInfoRealizable = realizability(tp.info)
      if (sym.is(StableRealizable)) realizability(tp.prefix)
      else {
        val r =
          if (sym.isStableMember && !isLateInitialized(sym))
            // it's realizable because we know that a value of type `tp` has been created at run-time
            Realizable
          else if (!sym.isEffectivelyFinal)
            // it's potentially not realizable since it might be overridden with a member of nonrealizable type
            new NotFinal(sym)
          else
            // otherwise we need to look at the info to determine realizability
            // roughly: it's realizable if the info does not have bad bounds
            tpInfoRealizable.mapError(r => new ProblemInUnderlying(tp, r))
        r andAlso {
          if (sym.isStableMember) sym.setFlag(StableRealizable) // it's known to be stable and realizable
          realizability(tp.prefix)
        } mapError { r =>
          // A mutable path is in fact stable and realizable if it has a realizable singleton type.
          if (tp.info.isStable && tpInfoRealizable == Realizable) {
            sym.setFlag(StableRealizable)
            Realizable
          }
          else r
        }
      }
    case _: SingletonType | NoPrefix =>
      Realizable
    case tp =>
      def isConcrete(tp: Type): Boolean = tp.dealias match {
        case tp: TypeRef => tp.symbol.isClass
        case tp: TypeParamRef => false
        case tp: TypeProxy => isConcrete(tp.underlying)
        case tp: AndType => isConcrete(tp.tp1) && isConcrete(tp.tp2)
        case tp: OrType  => isConcrete(tp.tp1) && isConcrete(tp.tp2)
        case tp: FlexibleType => isConcrete(tp.underlying)
        case _ => false
      }
      if (!isConcrete(tp)) NotConcrete
      else boundsRealizability(tp).andAlso(memberRealizability(tp))
  }

  private def refinedNames(tp: Type): Set[Name] = tp.dealias match {
    case tp: RefinedType => refinedNames(tp.parent) + tp.refinedName
    case tp: AndType => refinedNames(tp.tp1) ++ refinedNames(tp.tp2)
    case tp: OrType  => refinedNames(tp.tp1) ++ refinedNames(tp.tp2)
    case tp: TypeProxy => refinedNames(tp.superType)
    case _ => Set.empty
  }

  /** `Realizable` if `tp` has good bounds, a `HasProblem...` instance
   *  pointing to a bad bounds member otherwise. "Has good bounds" means:
   *
   *    - all type members have good bounds (except for opaque helpers)
   *    - all refinements of the underlying type have good bounds (except for opaque companions)
   *    - all base types are class types, and if their arguments are wildcards
   *      they have good bounds.
   *    - base types do not appear in multiple instances with different arguments.
   *      (depending on the simplification scheme for AndTypes employed, this could
   *       also lead to base types with bad bounds).
   */
  private def boundsRealizability(tp: Type) = {

    val memberProblems = withMode(Mode.CheckBoundsOrSelfType) {
      for {
        mbr <- tp.nonClassTypeMembers
        if !(mbr.info.loBound <:< mbr.info.hiBound)
      }
      yield new HasProblemBounds(mbr.name, mbr.info)
    }

    val refinementProblems = withMode(Mode.CheckBoundsOrSelfType) {
      for {
        name <- refinedNames(tp)
        if (name.isTypeName)
        mbr <- tp.member(name).alternatives
        if !(mbr.info.loBound <:< mbr.info.hiBound)
      }
      yield
        new HasProblemBounds(name, mbr.info)
    }

    def baseTypeProblems(base: Type) = base match {
      case AndType(base1, base2) =>
        new HasProblemBase(base1, base2) :: Nil
      case base =>
        base.argInfos.collect {
          case bounds @ TypeBounds(lo, hi) if !(lo <:< hi) =>
            new HasProblemBaseArg(base, bounds)
        }
    }
    val baseProblems =
      tp.baseClasses.map(_.baseTypeOf(tp)).flatMap(baseTypeProblems)

    baseProblems.foldLeft(
      refinementProblems.foldLeft(
        memberProblems.foldLeft(
          Realizable: Realizability)(_ andAlso _))(_ andAlso _))(_ andAlso _)
  }

  /** `Realizable` if all of `tp`'s non-strict fields have realizable types,
   *  a `HasProblemField` instance pointing to a bad field otherwise.
   */
  private def memberRealizability(tp: Type) = {
    def checkField(sofar: Realizability, fld: SingleDenotation): Realizability =
      sofar andAlso {
        if (checkedFields.contains(fld.symbol) || fld.symbol.isOneOf(Private | Mutable | LateInitializedFlags))
          // if field is private it cannot be part of a visible path
          // if field is mutable it cannot be part of a path
          // if field is lazy or erased it does not need to be initialized when the owning object is
          // so in all cases the field does not influence realizability of the enclosing object.
          Realizable
        else {
          checkedFields += fld.symbol
          realizability(fld.info).mapError(r => new HasProblemField(fld, r))
        }
      }
    if sourceVersion.isAtLeast(future) then
      // check fields only from version 3.x.
      // Reason: An embedded field could well be nullable, which means it
      // should not be part of a path and need not be checked; but we cannot recognize
      // this situation until we have a typesystem that tracks nullability.
      tp.fields.foldLeft(Realizable: Realizability)(checkField)
    else
      Realizable
  }
}
