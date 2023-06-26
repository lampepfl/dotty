package dotty.tools
package dotc
package transform

import core._
import MegaPhase._
import Contexts._
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import StdNames._
import Names._
import NameKinds._
import NameOps._
import ast.Trees._

object Mixin {
  val name: String = "mixin"
  val description: String = "expand trait fields and trait initializers"

  def traitSetterName(getter: TermSymbol)(using Context): TermName =
    extension (name: Name) def qualifiedToSimple = name.replace {
      case n @ AnyQualifiedName(_, _) => n.toSimpleName
    }
    getter.ensureNotPrivate.name
      .qualifiedToSimple  // TODO: Find out why TraitSetterNames can't be defined over QualifiedNames
      .expandedName(getter.owner, TraitSetterName)
      .asTermName.syntheticSetterName
}

/** This phase performs the following transformations:
 *
 *   1. (done in `traitDefs` and `transformSym`) For every concrete trait getter
 *
 *       <mods> def x(): T = expr
 *
 *   make it non-private, and add the definition of its trait setter:
 *
 *       <mods> def TraitName$_setter_$x(v: T): Unit
 *
 *   2. (done in `traitDefs`) Make every concrete trait setter
 *
 *      <mods> def x_=(y: T) = ()
 *
 *     deferred by mapping it to
 *
 *      <mods> def x_=(y: T)
 *
 *   3. (done in `transformSym`) For every module class constructor in traits,
 *      remove its Private flag (but do not expand its name), since it will have
 *      to be instantiated in the classes that mix in the trait.
 *
 *   4. For a non-trait class C:
 *
 *        For every trait M directly implemented by the class (see SymUtils.mixin), in
 *        reverse linearization order, add the following definitions to C:
 *
 *          4.1 (done in `traitInits`) For every parameter accessor `<mods> def x(): T` in M,
 *              in order of textual occurrence, add
 *
 *               <mods> def x() = e
 *
 *              where `e` is the constructor argument in C that corresponds to `x`. Issue
 *              an error if no such argument exists.
 *
 *          4.2 (done in `traitInits`) For every concrete trait getter `<mods> def x(): T` in M
 *              which is not a parameter accessor, in order of textual occurrence, produce the following:
 *
 *              4.2.1 If `x` is also a member of `C`, and is a lazy val,
 *
 *                <mods> lazy val x: T = super[M].x
 *
 *              4.2.2 If `x` is also a member of `C`, and is a module,
 *
 *                <mods> lazy module val x: T = new T$(this)
 *
 *              4.2.3 If `x` is also a member of `C`, and is something else:
 *
 *                <mods> def x(): T = _
 *
 *              4.2.5 If `x` is not a member of `C`, nothing gets added.
 *
 *          4.3 (done in `superCallOpt`) The call:
 *
 *                super[M].$init$()
 *
 *          4.4 (done in `setters`) For every concrete setter `<mods> def x_=(y: T)` in M:
 *
 *                <mods> def x_=(y: T) = ()
 *
 *          4.5 (done in `mixinForwarders`) For every method
 *          `<mods> def f[Ts](ps1)...(psN): U` in M that needs to be disambiguated:
 *
 *                <mods> def f[Ts](ps1)...(psN): U = super[M].f[Ts](ps1)...(psN)
 *
 *          A method in M needs to be disambiguated if it is concrete, not overridden in C,
 *          and if it overrides another concrete method.
 *
 *   5. (done in `transformTemplate` and `transformSym`) Drop all parameters from trait
 *      constructors, and rename them to `nme.TRAIT_CONSTRUCTOR`.
 *
 *   6. (done in `transformSym`) Drop ParamAccessor flag from all parameter accessors in traits.
 *
 *  Conceptually, this is the second half of the previous mixin phase. It needs to run
 *  after erasure because it copies references to possibly private inner classes and objects
 *  into enclosing classes where they are not visible. This can only be done if all references
 *  are symbolic.
 */
class Mixin extends MiniPhase with SymTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = Mixin.name

  override def description: String = Mixin.description

  override def relaxedTypingInGroup: Boolean = true
    // Because it changes number of parameters in trait initializers

  override def runsAfter: Set[String] = Set(
    Erasure.name,
    CompleteJavaEnums.name // This phase changes constructor parameters which Mixin translates into super-calls
  )

  override def changesMembers: Boolean = true  // the phase adds implementions of mixin accessors

  override def transformSym(sym: SymDenotation)(using Context): SymDenotation =
    def ownerIsTrait: Boolean = was(sym.owner, Trait, butNot = JavaDefined)

    if (sym.is(Accessor, butNot = Deferred) && ownerIsTrait) {
      val sym1 =
        if (sym.is(Lazy) || sym.symbol.isConstExprFinalVal) sym
        else sym.copySymDenotation(initFlags = sym.flags &~ (ParamAccessor | Inline) | Deferred)
      sym1.ensureNotPrivate
    }
    else if sym.isAllOf(ModuleClass | Private) && ownerIsTrait then
      // modules in trait will be instantiated in the classes mixing in the trait; they must be made non-private
      // do not use ensureNotPrivate because the `name` must not be expanded in this case
      sym.copySymDenotation(initFlags = sym.flags &~ Private)
    else if (sym.isConstructor && ownerIsTrait)
      sym.copySymDenotation(
        name = nme.TRAIT_CONSTRUCTOR,
        info = MethodType(Nil, sym.info.resultType))
    else if sym.is(Trait, butNot = JavaDefined) then
      val classInfo = sym.asClass.classInfo
      lazy val decls1 = classInfo.decls.cloneScope
      var modified: Boolean = false
      for (decl <- classInfo.decls)
        // !decl.isClass avoids forcing nested traits, preventing cycles
        if !decl.isClass && needsTraitSetter(decl) then
          val setter = makeTraitSetter(decl.asTerm)
          setter.validFor = thisPhase.validFor // validity of setter = next phase up to next transformer afterwards
          decls1.enter(setter)
          modified = true
      if modified then
        sym.copySymDenotation(
          info = classInfo.derivedClassInfo(decls = decls1))
      else
        sym
    else
      sym
  end transformSym

  private def wasOneOf(sym: Symbol, flags: FlagSet)(using Context): Boolean =
    atPhase(thisPhase) { sym.isOneOf(flags) }

  private def was(sym: Symbol, flag: Flag, butNot: FlagSet)(using Context): Boolean =
    atPhase(thisPhase) { sym.is(flag, butNot) }

  private def needsTraitSetter(sym: Symbol)(using Context): Boolean =
    sym.isGetter && !wasOneOf(sym, DeferredOrLazy | ParamAccessor)
      && atPhase(thisPhase) { !sym.setter.exists }
      && !sym.isConstExprFinalVal
      && !sym.owner.isInlineTrait

  private def makeTraitSetter(getter: TermSymbol)(using Context): Symbol =
    getter.copy(
      name = Mixin.traitSetterName(getter),
      flags = Method | Accessor | Deferred,
      info = MethodType(getter.info.resultType :: Nil, defn.UnitType))

  override def transformTemplate(impl: Template)(using Context): Template = {
    val cls = impl.symbol.owner.asClass
    val ops = new MixinOps(cls, thisPhase)
    import ops._

    def traitDefs(stats: List[Tree]): List[Tree] = {
      stats.flatMap {
        case stat: DefDef if needsTraitSetter(stat.symbol) =>
          // add a trait setter for this getter
          stat :: DefDef(stat.symbol.traitSetter.asTerm, EmptyTree) :: Nil
        case stat: DefDef if stat.symbol.isSetter =>
          cpy.DefDef(stat)(rhs = EmptyTree) :: Nil
        case stat =>
          stat :: Nil
      }
    }

    /** Map constructor call to a triple of a supercall, and if the target
     *  is a trait
     *   - a list of val defs used in arguments (these can arise
     *     due to reorderings with named and/or default parameters).
     *   - a list of arguments to be used as initializers of trait parameters
     */
    def transformConstructor(tree: Tree): (Tree, List[Tree], List[Tree]) = tree match {
      case Block(stats, expr) =>
        val (scall, inits, args) = transformConstructor(expr)
        if args.isEmpty then
          (cpy.Block(tree)(stats, scall), inits, args)
        else // it's a trait constructor with parameters, lift all prefix statements to class context
             // so that they precede argument definitions.
          stats.foreach {
            case stat: ValDef =>
              stat.symbol.copySymDenotation(
                owner = cls,
                initFlags = stat.symbol.flags | PrivateLocal
              ).installAfter(thisPhase)
              stat.symbol.enteredAfter(thisPhase)
            case _ =>
          }
          (scall, stats ::: inits, args)
      case _ =>
        val Apply(sel @ Select(New(_), nme.CONSTRUCTOR), args) = tree: @unchecked
        val (callArgs, initArgs) = if (tree.symbol.owner.is(Trait)) (Nil, args) else (args, Nil)
        (superRef(tree.symbol, tree.span).appliedToTermArgs(callArgs), Nil, initArgs)
    }

    val superCallsAndArgs: Map[Symbol, (Tree, List[Tree], List[Tree])] = (
      for (p <- impl.parents; constr = stripBlock(p).symbol if constr.isConstructor)
      yield constr.owner -> transformConstructor(p)
    ).toMap

    def superCallOpt(baseCls: Symbol): List[Tree] = superCallsAndArgs.get(baseCls) match
      case Some((call, _, _)) =>
        if (defn.NotRuntimeClasses.contains(baseCls) || baseCls.isAllOf(NoInitsTrait)) Nil
        else call :: Nil
      case None =>
        if baseCls.isAllOf(NoInitsTrait) || defn.NoInitClasses.contains(baseCls) || defn.isFunctionClass(baseCls) then
          Nil
        else
          //println(i"synth super call ${baseCls.primaryConstructor}: ${baseCls.primaryConstructor.info}")
          transformFollowingDeep(superRef(baseCls.primaryConstructor).appliedToNone) :: Nil

    def traitInits(mixin: ClassSymbol): List[Tree] = {
      if mixin.isInlineTrait then return Nil
      val argsIt = superCallsAndArgs.get(mixin) match
        case Some((_, _, args)) => args.iterator
        case _ => Iterator.empty
      def nextArgument() =
        if argsIt.hasNext then argsIt.next
        else
          assert(
              impl.parents.forall(_.tpe.typeSymbol != mixin),
              i"missing parameters for $mixin from $impl should have been caught in typer")
          report.error(
              em"""parameterized $mixin is indirectly implemented,
                  |needs to be implemented directly so that arguments can be passed""",
              cls.srcPos)
          EmptyTree

      for
        getter <- mixin.info.decls.toList
        if getter.isGetter
           && !getter.isEffectivelyErased
           && !wasOneOf(getter, Deferred)
           && !getter.isConstExprFinalVal
      yield
        if (isInImplementingClass(getter) || getter.name.is(ExpandedName)) {
          val rhs =
            if (wasOneOf(getter, ParamAccessor))
              nextArgument()
            else if (getter.is(Lazy, butNot = Module))
              transformFollowing(superRef(getter).appliedToNone)
            else if (getter.is(Module))
              New(getter.info.resultType, List(This(cls)))
            else
              Underscore(getter.info.resultType)
          // transformFollowing call is needed to make memoize & lazy vals run
          transformFollowing(DefDef(mkForwarderSym(getter.asTerm), rhs))
        }
        else if wasOneOf(getter, ParamAccessor) then
          // mixin parameter field is defined by an override; evaluate the argument and throw it away
          nextArgument()
        else EmptyTree
    }

    def setters(mixin: ClassSymbol): List[Tree] =
      val mixinSetters = mixin.info.decls.filter { sym =>
        sym.isSetter && (!wasOneOf(sym, Deferred) || sym.name.is(TraitSetterName)) && !sym.owner.isInlineTrait
      }
      for (setter <- mixinSetters)
      yield transformFollowing(DefDef(mkForwarderSym(setter.asTerm), unitLiteral.withSpan(cls.span)))

    def mixinForwarders(mixin: ClassSymbol): List[Tree] =
      for (meth <- mixin.info.decls.toList if needsMixinForwarder(meth))
      yield {
        util.Stats.record("mixin forwarders")
        transformFollowing(DefDef(mkForwarderSym(meth.asTerm, Bridge), forwarderRhsFn(meth)))
      }

    cpy.Template(impl)(
      constr =
        if (cls.is(Trait)) cpy.DefDef(impl.constr)(paramss = Nil :: Nil)
        else impl.constr,
      parents = impl.parents.map(p => TypeTree(p.tpe).withSpan(p.span)),
      body =
        if (cls.is(Trait)) traitDefs(impl.body)
        else if (!cls.isPrimitiveValueClass) {
          val mixInits = mixins.flatMap { mixin =>
            val prefix = superCallsAndArgs.get(mixin) match
              case Some((_, inits, _)) => inits
              case _ => Nil
            prefix
            ::: flatten(traitInits(mixin))
            ::: superCallOpt(mixin)
            ::: setters(mixin)
            ::: mixinForwarders(mixin)
          }
          superCallOpt(superCls)
          ::: mixInits
          ::: impl.body
        }
        else impl.body)
  }
}
