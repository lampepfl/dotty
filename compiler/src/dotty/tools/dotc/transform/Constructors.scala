package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.StdNames._
import ast._
import Flags._
import Names.Name
import NameOps._
import NameKinds.{FieldName, ExplicitFieldName}
import SymUtils._
import Symbols._
import Decorators._
import DenotTransformers._
import collection.mutable

object Constructors {
  val name: String = "constructors"
  val description: String = "collect initialization code in primary constructors"
}

/** This transform
 *   - moves initializers from body to constructor.
 *   - makes all supercalls explicit
 *   - also moves private fields that are accessed only from constructor
 *     into the constructor if possible.
 */
class Constructors extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import tpd._

  override def phaseName: String = Constructors.name

  override def description: String = Constructors.description

  override def runsAfter: Set[String] = Set(HoistSuperArgs.name)
  override def runsAfterGroupsOf: Set[String] = Set(Memoize.name)
    // Memoized needs to be finished because we depend on the ownerchain after Memoize
    // when checking whether an ident is an access in a constructor or outside it.
    // This test is done in the right-hand side of a value definition. If Memoize
    // was in the same group as Constructors, the test on the rhs ident would be
    // performed before the rhs undergoes the owner change. This would lead
    // to more symbols being retained as parameters. Test case in run/capturing.scala.

  /** The private vals that are known to be retained as class fields */
  private val retainedPrivateVals = mutable.Set[Symbol]()

  /** The private vals whose definition comes before the current focus */
  private val seenPrivateVals = mutable.Set[Symbol]()

  // Collect all private parameter accessors and value definitions that need
  // to be retained. There are several reasons why a parameter accessor or
  // definition might need to be retained:
  // 1. It is accessed after the constructor has finished
  // 2. It is accessed before it is defined
  // 3. It is accessed on an object other than `this`
  // 4. It is a mutable parameter accessor
  // 5. It is has a wildcard initializer `_`
  private def markUsedPrivateSymbols(tree: RefTree)(using Context): Unit = {

    val sym = tree.symbol
    def retain() = retainedPrivateVals.add(sym)

    if (sym.exists && sym.owner.isClass && mightBeDropped(sym)) {
      val owner = sym.owner.asClass

        tree match {
          case Ident(_) | Select(This(_), _) =>
            def inConstructor = {
              val method = ctx.owner.enclosingMethod
              method.isPrimaryConstructor && ctx.owner.enclosingClass == owner
            }
            if (inConstructor &&
                (sym.is(ParamAccessor) || seenPrivateVals.contains(sym))) {
              // used inside constructor, accessed on this,
              // could use constructor argument instead, no need to retain field
            }
            else retain()
          case _ => retain()
      }
    }
  }

  override def transformIdent(tree: tpd.Ident)(using Context): tpd.Tree = {
    markUsedPrivateSymbols(tree)
    tree
  }

  override def transformSelect(tree: tpd.Select)(using Context): tpd.Tree = {
    markUsedPrivateSymbols(tree)
    tree
  }

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree = {
    if (mightBeDropped(tree.symbol)) seenPrivateVals += tree.symbol
    tree
  }

  /** All initializers for non-lazy fields should be moved into constructor.
   *  All non-abstract methods should be implemented (this is assured for constructors
   *  in this phase and for other methods in memoize).
   */
  override def checkPostCondition(tree: tpd.Tree)(using Context): Unit = {
    def emptyRhsOK(sym: Symbol) =
      sym.isOneOf(DeferredOrLazy) || sym.isConstructor && sym.owner.isAllOf(NoInitsTrait)
    tree match {
      case tree: ValDef if tree.symbol.exists && tree.symbol.owner.isClass && !tree.symbol.is(Lazy) && !tree.symbol.hasAnnotation(defn.ScalaStaticAnnot) =>
        assert(tree.rhs.isEmpty, i"$tree: initializer should be moved to constructors")
      case tree: DefDef if !emptyRhsOK(tree.symbol) =>
        assert(!tree.rhs.isEmpty, i"unimplemented: $tree")
      case _ =>
    }
  }

  /** @return true  if after ExplicitOuter, all references from this tree go via an
   *                outer link, so no parameter accessors need to be rewired to parameters
   */
  private def noDirectRefsFrom(tree: Tree)(using Context) =
    tree.isDef && tree.symbol.isClass

  /** Class members that can be eliminated if referenced only from their own
   *  constructor.
   */
  private def mightBeDropped(sym: Symbol)(using Context) =
    sym.is(Private, butNot = MethodOrLazy) && !sym.isAllOf(MutableParamAccessor)

  private final val MutableParamAccessor = Mutable | ParamAccessor

  override def transformTemplate(tree: Template)(using Context): Tree = {
    val cls = ctx.owner.asClass

    val constr @ DefDef(nme.CONSTRUCTOR, (vparams: List[ValDef] @unchecked) :: Nil, _, EmptyTree) = tree.constr: @unchecked

    // Produce aligned accessors and constructor parameters. We have to adjust
    // for any outer parameters, which are last in the sequence of original
    // parameter accessors but come first in the constructor parameter list.
    val accessors = cls.paramGetters
    val vparamsWithOuterLast = vparams match {
      case vparam :: rest if vparam.name == nme.OUTER => rest ::: vparam :: Nil
      case _ => vparams
    }
    val paramSyms = vparamsWithOuterLast map (_.symbol)

    // Adjustments performed when moving code into the constructor:
    //  (1) Replace references to param accessors by constructor parameters
    //      except possibly references to mutable variables, if `excluded = Mutable`.
    //      (Mutable parameters should be replaced only during the super call)
    //  (2) If the parameter accessor reference was to an alias getter,
    //      drop the () when replacing by the parameter.
    object intoConstr extends TreeMap {
      private var inSuperCall = false
      override def transform(tree: Tree)(using Context): Tree = tree match {
        case Ident(_) | Select(This(_), _) =>
          var sym = tree.symbol
          def isOverridableSelect = tree.isInstanceOf[Select] && !sym.isEffectivelyFinal
          def switchOutsideSupercall = !sym.is(Mutable) && !isOverridableSelect
            // If true, switch to constructor parameters also in the constructor body
            // that follows the super call.
            // Variables need to go through the getter since they might have been updated.
            // References via this need to use the getter as well as long as that getter
            // can be overridden. This is needed to handle overrides correctly. See run/i15723.scala.
            // Note that in a supercall we need to switch to parameters in any case since then
            // calling the virtual getter call would be illegal.
            //
            // Note: We intentionally treat references via this and identifiers differently
            // here. Identifiers in a constructor always bind to the parameter. This is
            // done for backwards compatbility.
          if sym.is(ParamAccessor) && (switchOutsideSupercall || inSuperCall) then
            sym = sym.subst(accessors, paramSyms)
          if sym.maybeOwner.isConstructor then ref(sym).withSpan(tree.span) else tree
        case Apply(fn, Nil) =>
          val fn1 = transform(fn)
          if ((fn1 ne fn) && fn1.symbol.is(Param) && fn1.symbol.owner.isPrimaryConstructor)
            fn1 // in this case, fn1.symbol was an alias for a parameter in a superclass
          else cpy.Apply(tree)(fn1, Nil)
        case _ =>
          if (noDirectRefsFrom(tree)) tree else super.transform(tree)
      }

      def apply(tree: Tree, prevOwner: Symbol)(using Context): Tree =
        inSuperCall = isSuperConstrCall(tree)
        transform(tree).changeOwnerAfter(prevOwner, constr.symbol, thisPhase)
    }

    def isRetained(acc: Symbol) =
      !mightBeDropped(acc) || retainedPrivateVals(acc)

    val constrStats, clsStats = new mutable.ListBuffer[Tree]

    /** Map outer getters $outer and outer accessors $A$B$$$outer to the given outer parameter. */
    def mapOuter(outerParam: Symbol) = new TreeMap {
      override def transform(tree: Tree)(using Context) = tree match {
        case Apply(fn, Nil)
          if (fn.symbol.is(OuterAccessor)
             || fn.symbol.isGetter && fn.symbol.name == nme.OUTER
             ) &&
             fn.symbol.info.resultType.classSymbol == outerParam.info.classSymbol =>
          ref(outerParam)
        case tree: RefTree if tree.symbol.is(ParamAccessor) && tree.symbol.name == nme.OUTER =>
          ref(outerParam)
        case _ =>
          super.transform(tree)
      }
    }

    val dropped = mutable.Set[Symbol]()

    // Split class body into statements that go into constructor and
    // definitions that are kept as members of the class.
    def splitStats(stats: List[Tree]): Unit = stats match {
      case stat :: stats1 =>
        stat match {
          case stat @ ValDef(name, tpt, _) if !stat.symbol.is(Lazy) && !stat.symbol.hasAnnotation(defn.ScalaStaticAnnot) =>
            val sym = stat.symbol
            if (isRetained(sym)) {
              if (!stat.rhs.isEmpty && !isWildcardArg(stat.rhs))
                constrStats += Assign(ref(sym), intoConstr(stat.rhs, sym)).withSpan(stat.span)
              clsStats += cpy.ValDef(stat)(rhs = EmptyTree)
            }
            else if (!stat.rhs.isEmpty) {
              dropped += sym
              sym.copySymDenotation(
                initFlags = sym.flags &~ Private,
                owner = constr.symbol).installAfter(thisPhase)
              constrStats += intoConstr(stat, sym)
            } else
              dropped += sym
          case stat @ DefDef(name, _, tpt, _) if stat.symbol.isGetter && !stat.symbol.is(Lazy) && !stat.symbol.owner.isInlineTrait =>
            val sym = stat.symbol
            assert(isRetained(sym), sym)
            if sym.isConstExprFinalVal then
              if stat.rhs.isInstanceOf[Literal] then
                clsStats += stat
              else
                constrStats += intoConstr(stat.rhs, sym)
                clsStats += cpy.DefDef(stat)(rhs = Literal(sym.constExprFinalValConstantType.value).withSpan(stat.span))
            else if !sym.owner.is(Trait) then
              clsStats += stat
            else
              if !stat.rhs.isEmpty && !isWildcardArg(stat.rhs) then
                /* !!! Work around #9390
                 * This should really just be `sym.setter`. However, if we do that, we'll miss
                 * setters for mixed in `private var`s. Even though the scope clearly contains the
                 * setter symbol with the correct Name structure (since the `find` finds it),
                 * `.decl(setterName)` used by `.setter` through `.accessorNamed` will *not* find it.
                 * Could it be that the hash table of the `Scope` is corrupted?
                 * We still try `sym.setter` first as an optimization, since it will work for all
                 * public vars in traits and all (public or private) vars in classes.
                 */
                val symSetter =
                  if sym.setter.exists then
                    sym.setter
                  else
                    val setterName = sym.asTerm.name.setterName
                    sym.owner.info.decls.find(d => d.is(Accessor) && d.name == setterName)
                val setter =
                  if (symSetter.exists) symSetter
                  else sym.accessorNamed(Mixin.traitSetterName(sym.asTerm))
                constrStats += Apply(ref(setter), intoConstr(stat.rhs, sym).withSpan(stat.span) :: Nil)
              clsStats += cpy.DefDef(stat)(rhs = EmptyTree)
          case DefDef(nme.CONSTRUCTOR, ((outerParam @ ValDef(nme.OUTER, _, _)) :: _) :: Nil, _, _) =>
            clsStats += mapOuter(outerParam.symbol).transform(stat)
          case _: DefTree =>
            clsStats += stat
          case _ =>
            constrStats += intoConstr(stat, tree.symbol)
        }
        splitStats(stats1)
      case Nil =>
    }

    /** Check that we do not have both a private field with name `x` and a private field
     *  with name `FieldName(x)`. These will map to the same JVM name and therefore cause
     *  a duplicate field error. If that case arises (as in i13862.scala), use an explicit
     *  name `x$field` instead of `FieldName(x).
     */
    def checkNoFieldClashes() =
      val fieldNames = mutable.HashSet[Name]()
      for case field: ValDef <- clsStats do
        field.symbol.name match
          case FieldName(_) =>
          case name => fieldNames += name
      for case field: ValDef <- clsStats do
        field.symbol.name match
          case fldName @ FieldName(name) if fieldNames.contains(name) =>
            val newName = ExplicitFieldName(name)
            report.log(i"avoid field/field conflict by renaming $fldName to $newName")
            field.symbol.copySymDenotation(name = newName).installAfter(thisPhase)
          case _ =>

    splitStats(tree.body)
    checkNoFieldClashes()

    // The initializers for the retained accessors */
    val copyParams = accessors flatMap { acc =>
      if (!isRetained(acc)) {
        dropped += acc
        Nil
      }
      else if (!isRetained(acc.field)) { // It may happen for unit fields, tests/run/i6987.scala
        dropped += acc.field
        Nil
      }
      else {
        val param = acc.subst(accessors, paramSyms)
        if (param.hasAnnotation(defn.ConstructorOnlyAnnot))
          report.error(em"${acc.name} is marked `@constructorOnly` but it is retained as a field in ${acc.owner}", acc.srcPos)
        val target = if (acc.is(Method)) acc.field else acc
        if (!target.exists) Nil // this case arises when the parameter accessor is an alias
        else {
          val assigns = Assign(ref(target), ref(param)).withSpan(tree.span) :: Nil
          if (acc.name != nme.OUTER) assigns
          else {
            // insert test: if ($outer eq null) throw new NullPointerException
            val nullTest =
              If(ref(param).select(defn.Object_eq).appliedTo(nullLiteral),
                 Throw(New(defn.NullPointerExceptionClass.typeRef, Nil)),
                 unitLiteral)
            nullTest :: assigns
          }
        }
      }
    }

    // Drop accessors that are not retained from class scope
    if (dropped.nonEmpty) {
      val clsInfo = cls.classInfo
      val decls = clsInfo.decls.filteredScope(!dropped.contains(_))
      val clsInfo2 = clsInfo.derivedClassInfo(decls = decls)
      cls.copySymDenotation(info = clsInfo2).installAfter(thisPhase)
      // TODO: this happens to work only because Constructors is the last phase in group
    }

    val (superCalls, followConstrStats) = splitAtSuper(constrStats.toList)

    val mappedSuperCalls = vparams match {
      case (outerParam @ ValDef(nme.OUTER, _, _)) :: _ =>
        superCalls.map(mapOuter(outerParam.symbol).transform)
      case _ => superCalls
    }

    // Lazy Vals may decide to create an eager val instead of a lazy val
    // this val should be assigned before constructor body code starts running

    val (lazyAssignments, stats) = followConstrStats.partition {
      case Assign(l, r) if l.symbol.name.is(NameKinds.LazyLocalName) => true
      case _ => false
    }

    val finalConstrStats = copyParams ::: mappedSuperCalls ::: lazyAssignments ::: stats
    val expandedConstr =
      if (cls.isAllOf(NoInitsTrait)) {
        assert(finalConstrStats.isEmpty || {
          import dotty.tools.dotc.transform.sjs.JSSymUtils._
          ctx.settings.scalajs.value && cls.isJSType
        })
        constr
      }
      else cpy.DefDef(constr)(rhs = Block(finalConstrStats, unitLiteral))

    cpy.Template(tree)(constr = expandedConstr, body = clsStats.toList)
  }
}
