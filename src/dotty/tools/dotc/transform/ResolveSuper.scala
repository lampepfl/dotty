package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import StdNames._
import NameOps._
import ast.Trees._
import util.Positions._
import Names._
import collection.mutable

/** This phase adds super accessors and method overrides where
 *  linearization differs from Java's rule for default methods in interfaces.
 *  In particular:
 *
 *        For every trait M directly implemented by the class (see SymUtils.mixin), in
 *        reverse linearization order, add the following definitions to C:
 *
 *          3.1 (done in `superAccessors`) For every superAccessor
 *              `<mods> def super$f[Ts](ps1)...(psN): U` in M:
 *
 *                <mods> def super$f[Ts](ps1)...(psN): U = super[S].f[Ts](ps1)...(psN)
 *
 *              where `S` is the superclass of `M` in the linearization of `C`.
 *
 *          3.2 (done in `methodOverrides`) For every method
 *              `<mods> def f[Ts](ps1)...(psN): U` in M` that needs to be disambiguated:
 *
 *                <mods> def f[Ts](ps1)...(psN): U = super[M].f[Ts](ps1)...(psN)
 *
 *        A method in M needs to be disambiguated if it is concrete, not overridden in C,
 *        and if it overrides another concrete method.
 *
 *  This is the first part of what was the mixin phase. It is complemented by
 *  Mixin, which runs after erasure.
 */
class ResolveSuper extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "resolveSuper"

  override def treeTransformPhase = thisTransform.next

  override def transformTemplate(impl: Template)(implicit ctx: Context, info: TransformerInfo) = {
    val cls = impl.symbol.owner.asClass
    val ops = new MixinOps(cls, thisTransform)
    import ops._

    /** Returns the symbol that is accessed by a super-accessor in a mixin composition.
     *
     *  @param base       The class in which everything is mixed together
     *  @param member     The symbol statically referred to by the superaccessor in the trait
     */
    def rebindSuper(base: Symbol, acc: Symbol): Symbol = {
      var bcs = cls.info.baseClasses.dropWhile(acc.owner != _).tail
      var sym: Symbol = NoSymbol
      val SuperAccessorName(memberName) = acc.name: Name // dotty deviation: ": Name" needed otherwise pattern type is neither a subtype nor a supertype of selector type
      ctx.debuglog(i"starting rebindsuper from $cls of ${acc.showLocated}: ${acc.info} in $bcs, name = $memberName")
      while (bcs.nonEmpty && sym == NoSymbol) {
        val other = bcs.head.info.nonPrivateDecl(memberName)
        if (ctx.settings.debug.value)
          ctx.log(i"rebindsuper ${bcs.head} $other deferred = ${other.symbol.is(Deferred)}")
        sym = other.matchingDenotation(cls.thisType, cls.thisType.memberInfo(acc)).symbol
        bcs = bcs.tail
      }
      assert(sym.exists)
      sym
    }

    def superAccessors(mixin: ClassSymbol): List[Tree] =
      for (superAcc <- mixin.decls.filter(_ is SuperAccessor).toList)
        yield polyDefDef(implementation(superAcc.asTerm), forwarder(rebindSuper(cls, superAcc)))

    def methodOverrides(mixin: ClassSymbol): List[Tree] = {
      def isOverridden(meth: Symbol) = meth.overridingSymbol(cls).is(Method, butNot = Deferred)
      def needsDisambiguation(meth: Symbol): Boolean =
        meth.is(Method, butNot = PrivateOrDeferred) &&
          !isOverridden(meth) &&
          !meth.allOverriddenSymbols.forall(_ is Deferred)
      for (meth <- mixin.decls.toList if needsDisambiguation(meth))
        yield polyDefDef(implementation(meth.asTerm), forwarder(meth))
    }

    val overrides = mixins.flatMap(mixin => superAccessors(mixin) ::: methodOverrides(mixin))

    cpy.Template(impl)(body = overrides ::: impl.body)
  }
  private val PrivateOrDeferred = Private | Deferred
}
