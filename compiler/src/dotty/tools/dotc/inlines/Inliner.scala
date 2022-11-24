package dotty.tools
package dotc
package inlines

import ast.*, core.*
import Flags.*, Symbols.*, Types.*, Decorators.*, Constants.*, Contexts.*
import transform.SymUtils.*
import StdNames.nme
import typer.*
import Names.Name
import NameKinds.InlineBinderName
import ProtoTypes.shallowSelectionProto
import SymDenotations.SymDenotation
import Inferencing.isFullyDefined
import config.Printers.inlining
import ErrorReporting.errorTree
import util.{SimpleIdentitySet, SrcPos}
import Nullables.computeNullableDeeply

import collection.mutable
import reporting.trace
import util.Spans.Span
import dotty.tools.dotc.transform.Splicer
import quoted.QuoteUtils
import scala.annotation.constructorOnly

/** General support for inlining */
object Inliner:
  import tpd._

  private[inlines] type DefBuffer = mutable.ListBuffer[ValOrDefDef]

  /** Very similar to TreeInfo.isPureExpr, but with the following inliner-only exceptions:
   *  - synthetic case class apply methods, when the case class constructor is empty, are
   *    elideable but not pure. Elsewhere, accessing the apply method might cause the initialization
   *    of a containing object so they are merely idempotent.
   */
  object isElideableExpr:
    def isStatElideable(tree: Tree)(using Context): Boolean = unsplice(tree) match {
      case EmptyTree
         | TypeDef(_, _)
         | Import(_, _)
         | DefDef(_, _, _, _) =>
        true
      case vdef @ ValDef(_, _, _) =>
        if (vdef.symbol.flags is Mutable) false else apply(vdef.rhs)
      case _ =>
        false
    }

    def apply(tree: Tree)(using Context): Boolean = unsplice(tree) match {
      case EmptyTree
         | This(_)
         | Super(_, _)
         | Literal(_) =>
        true
      case Ident(_) =>
        isPureRef(tree) || tree.symbol.isAllOf(InlineParam)
      case Select(qual, _) =>
        if (tree.symbol.is(Erased)) true
        else isPureRef(tree) && apply(qual)
      case New(_) | Closure(_, _, _) =>
        true
      case TypeApply(fn, _) =>
        if (fn.symbol.is(Erased) || fn.symbol == defn.QuotedTypeModule_of) true else apply(fn)
      case Apply(fn, args) =>
        val isCaseClassApply = {
          val cls = tree.tpe.classSymbol
          val meth = fn.symbol
          meth.name == nme.apply &&
          meth.flags.is(Synthetic) &&
          meth.owner.linkedClass.is(Case) &&
          cls.isNoInitsRealClass &&
          funPart(fn).match
            case Select(qual, _) => qual.symbol.is(Synthetic) // e.g: disallow `{ ..; Foo }.apply(..)`
            case meth @ Ident(_) => meth.symbol.is(Synthetic) // e.g: allow `import Foo.{ apply => foo }; foo(..)`
            case _               => false
        }
        if isPureApply(tree, fn) then
          apply(fn) && args.forall(apply)
        else if (isCaseClassApply)
          args.forall(apply)
        else if (fn.symbol.is(Erased)) true
        else false
      case Typed(expr, _) =>
        apply(expr)
      case Block(stats, expr) =>
        apply(expr) && stats.forall(isStatElideable)
      case Inlined(_, bindings, expr) =>
        apply(expr) && bindings.forall(isStatElideable)
      case NamedArg(_, expr) =>
        apply(expr)
      case _ =>
        false
    }
  end isElideableExpr

  // InlineCopier is a more fault-tolerant copier that does not cause errors when
  // function types in applications are undefined. This is necessary since we copy at
  // the same time as establishing the proper context in which the copied tree should
  // be evaluated. This matters for opaque types, see neg/i14653.scala.
  private class InlineCopier() extends TypedTreeCopier:
    override def Apply(tree: Tree)(fun: Tree, args: List[Tree])(using Context): Apply =
      if fun.tpe.widen.exists then super.Apply(tree)(fun, args)
      else untpd.cpy.Apply(tree)(fun, args).withTypeUnchecked(tree.tpe)

  // InlinerMap is a TreeTypeMap with special treatment for inlined arguments:
  // They are generally left alone (not mapped further, and if they wrap a type
  // the type Inlined wrapper gets dropped
  private class InlinerMap(
      typeMap: Type => Type,
      treeMap: Tree => Tree,
      oldOwners: List[Symbol],
      newOwners: List[Symbol],
      substFrom: List[Symbol],
      substTo: List[Symbol])(using Context)
    extends TreeTypeMap(
      typeMap, treeMap, oldOwners, newOwners, substFrom, substTo, InlineCopier()):

    override def copy(
        typeMap: Type => Type,
        treeMap: Tree => Tree,
        oldOwners: List[Symbol],
        newOwners: List[Symbol],
        substFrom: List[Symbol],
        substTo: List[Symbol])(using Context) =
      new InlinerMap(typeMap, treeMap, oldOwners, newOwners, substFrom, substTo)

    override def transformInlined(tree: Inlined)(using Context) =
      if tree.call.isEmpty then
        tree.expansion match
          case expansion: TypeTree => expansion
          case _ => tree
      else super.transformInlined(tree)
  end InlinerMap
end Inliner

/** Produces an inlined version of `call` via its `inlined` method.
 *
 *  @param  call         the original call to an inlineable method
 *  @param  rhsToInline  the body of the inlineable method that replaces the call.
 */
class Inliner(val call: tpd.Tree)(using Context):
  import tpd._
  import Inliner._

  private val methPart = funPart(call)
  protected val callTypeArgs = typeArgss(call).flatten
  protected val callValueArgss = termArgss(call)
  protected val inlinedMethod = methPart.symbol
  private val inlineCallPrefix =
     qualifier(methPart).orElse(This(inlinedMethod.enclosingClass.asClass))

  // Make sure all type arguments to the call are fully determined,
  // but continue if that's not achievable (or else i7459.scala would crash).
  for arg <- callTypeArgs do
    isFullyDefined(arg.tpe, ForceDegree.flipBottom)

  /** A map from parameter names of the inlineable method to references of the actual arguments.
   *  For a type argument this is the full argument type.
   *  For a value argument, it is a reference to either the argument value
   *  (if the argument is a pure expression of singleton type), or to `val` or `def` acting
   *  as a proxy (if the argument is something else).
   */
  private val paramBinding = new mutable.HashMap[Name, Type]

  /** A map from parameter names of the inlineable method to spans of the actual arguments */
  private val paramSpan = new mutable.HashMap[Name, Span]

  /** A map from references to (type and value) parameters of the inlineable method
   *  to their corresponding argument or proxy references, as given by `paramBinding`.
   */
  private[inlines] val paramProxy = new mutable.HashMap[Type, Type]

  /** A map from the classes of (direct and outer) this references in `rhsToInline`
   *  to references of their proxies.
   *  Note that we can't index by the ThisType itself since there are several
   *  possible forms to express what is logicaly the same ThisType. E.g.
   *
   *     ThisType(TypeRef(ThisType(p), cls))
   *
   *  vs
   *
   *     ThisType(TypeRef(TermRef(ThisType(<root>), p), cls))
   *
   *  These are different (wrt ==) types but represent logically the same key
   */
  private val thisProxy = new mutable.HashMap[ClassSymbol, TermRef]

  /** A buffer for bindings that define proxies for actual arguments */
  private val bindingsBuf = new mutable.ListBuffer[ValOrDefDef]

  private[inlines] def newSym(name: Name, flags: FlagSet, info: Type)(using Context): Symbol =
    newSymbol(ctx.owner, name, flags, info, coord = call.span)

  /** A binding for the parameter of an inline method. This is a `val` def for
   *  by-value parameters and a `def` def for by-name parameters. `val` defs inherit
   *  inline annotations from their parameters. The generated `def` is appended
   *  to `buf`.
   *  @param name        the name of the parameter
   *  @param formal      the type of the parameter
   *  @param arg         the argument corresponding to the parameter
   *  @param buf         the buffer to which the definition should be appended
   */
  private[inlines] def paramBindingDef(name: Name, formal: Type, arg0: Tree,
                              buf: DefBuffer)(using Context): ValOrDefDef = {
    val isByName = formal.dealias.isInstanceOf[ExprType]
    val arg = arg0 match {
      case Typed(arg1, tpt) if tpt.tpe.isRepeatedParam && arg1.tpe.derivesFrom(defn.ArrayClass) =>
        wrapArray(arg1, arg0.tpe.elemType)
      case _ => arg0
    }
    val argtpe = arg.tpe.dealiasKeepAnnots.translateFromRepeated(toArray = false)
    val argIsBottom = argtpe.isBottomTypeAfterErasure
    val bindingType =
      if argIsBottom then formal
      else if isByName then ExprType(argtpe.widen)
      else argtpe.widen
    var bindingFlags: FlagSet = InlineProxy
    if formal.widenExpr.hasAnnotation(defn.InlineParamAnnot) then
      bindingFlags |= Inline
    if formal.widenExpr.hasAnnotation(defn.ErasedParamAnnot) then
      bindingFlags |= Erased
    if isByName then
      bindingFlags |= Method
    val boundSym = newSym(InlineBinderName.fresh(name.asTermName), bindingFlags, bindingType).asTerm
    val binding = {
      var newArg = arg.changeOwner(ctx.owner, boundSym)
      if bindingFlags.is(Inline) && argIsBottom then
        newArg = Typed(newArg, TypeTree(formal)) // type ascribe RHS to avoid type errors in expansion. See i8612.scala
      if isByName then DefDef(boundSym, newArg)
      else ValDef(boundSym, newArg)
    }.withSpan(boundSym.span)
    inlining.println(i"parameter binding: $binding, $argIsBottom")
    buf += binding
    binding
  }

  /** Populate `paramBinding` and `buf` by matching parameters with
   *  corresponding arguments. `bindingbuf` will be further extended later by
   *  proxies to this-references. Issue an error if some arguments are missing.
   */
  private def computeParamBindings(
      tp: Type, targs: List[Tree],
      argss: List[List[Tree]], formalss: List[List[Type]],
      buf: DefBuffer): Boolean =
    tp match
      case tp: PolyType =>
        tp.paramNames.lazyZip(targs).foreach { (name, arg) =>
          paramSpan(name) = arg.span
          paramBinding(name) = arg.tpe.stripTypeVar
        }
        computeParamBindings(tp.resultType, targs.drop(tp.paramNames.length), argss, formalss, buf)
      case tp: MethodType =>
        if argss.isEmpty then
          report.error(i"missing arguments for inline method $inlinedMethod", call.srcPos)
          false
        else
          tp.paramNames.lazyZip(formalss.head).lazyZip(argss.head).foreach { (name, formal, arg) =>
            paramSpan(name) = arg.span
            paramBinding(name) = arg.tpe.dealias match
              case _: SingletonType if isIdempotentPath(arg) =>
                arg.tpe
              case _ =>
                paramBindingDef(name, formal, arg, buf).symbol.termRef
          }
          computeParamBindings(tp.resultType, targs, argss.tail, formalss.tail, buf)
      case _ =>
        assert(targs.isEmpty)
        assert(argss.isEmpty)
        true

  /** The number of enclosing classes of this class, plus one */
  private def classNestingLevel(cls: Symbol) = cls.ownersIterator.count(_.isClass)

  // Compute val-definitions for all this-proxies and append them to `bindingsBuf`
  private def computeThisBindings() = {
    // All needed this-proxies, paired-with and sorted-by nesting depth of
    // the classes they represent (innermost first)
    val sortedProxies = thisProxy.toList
      .map((cls, proxy) => (classNestingLevel(cls), proxy.symbol, cls))
      .sortBy(-_._1)

    def outerSelect(prefix: Tree, prefixCls: Symbol, hops: Int, info: Type) =
      val tpt = TypeTree(adaptToPrefix(prefixCls.appliedRef))
      val qual = Typed(prefix, tpt)
      qual.outerSelect(hops, info)

    var lastSelf: Symbol = NoSymbol
    var lastCls: Symbol = NoSymbol
    var lastLevel: Int = 0
    for ((level, selfSym, cls) <- sortedProxies) {
      val rhs = selfSym.info.dealias match
        case info: TermRef
        if info.isStable && (lastSelf.exists || isPureExpr(inlineCallPrefix)) =>
          // If this is the first proxy, optimize to `ref(info)` only if call prefix is pure.
          // Otherwise we might forget side effects. See run/i12829.scala.
          ref(info)
        case info =>
          val rhsClsSym = info.widenDealias.classSymbol
          if rhsClsSym.is(Module) && rhsClsSym.isStatic then
            ref(rhsClsSym.sourceModule)
          else if lastSelf.exists then
            outerSelect(ref(lastSelf), lastCls, lastLevel - level, selfSym.info)
          else
            val pre = inlineCallPrefix match
              case Super(qual, _) => qual
              case pre => pre
            val preLevel = classNestingLevel(inlinedMethod.owner)
            if preLevel > level then outerSelect(pre, inlinedMethod.owner, preLevel - level, selfSym.info)
            else pre

      val binding = accountForOpaques(
        ValDef(selfSym.asTerm, QuoteUtils.changeOwnerOfTree(rhs, selfSym)).withSpan(selfSym.span))
      bindingsBuf += binding
      inlining.println(i"proxy at $level: $selfSym = ${bindingsBuf.last}")
      lastSelf = selfSym
      lastLevel = level
      lastCls = cls
    }
  }

  /** A list of pairs between TermRefs appearing in thisProxy bindings that
   *  refer to objects with opaque type aliases and local proxy symbols
   *  that contain refined versions of these TermRefs where the aliases
   *  are exposed.
   */
  private val opaqueProxies = new mutable.ListBuffer[(TermRef, TermRef)]

  protected def hasOpaqueProxies = opaqueProxies.nonEmpty

  /** Map first halfs of opaqueProxies pairs to second halfs, using =:= as equality */
  private def mapRef(ref: TermRef): Option[TermRef] =
    opaqueProxies.collectFirst {
      case (from, to) if from.symbol == ref.symbol && from =:= ref => to
    }

  /** If `tp` contains TermRefs that refer to objects with opaque
   *  type aliases, add proxy definitions to `opaqueProxies` that expose these aliases.
   */
  private def addOpaqueProxies(tp: Type, span: Span, forThisProxy: Boolean)(using Context): Unit =
    tp.foreachPart {
      case ref: TermRef =>
        for cls <- ref.widen.baseClasses do
          if cls.containsOpaques
             && (forThisProxy || inlinedMethod.isContainedIn(cls))
             && mapRef(ref).isEmpty
          then
            def openOpaqueAliases(selfType: Type): List[(Name, Type)] = selfType match
              case RefinedType(parent, rname, TypeAlias(alias)) =>
                val opaq = cls.info.member(rname).symbol
                if opaq.isOpaqueAlias then
                  (rname, alias.stripLazyRef.asSeenFrom(ref, cls))
                  :: openOpaqueAliases(parent)
                else Nil
              case _ =>
                Nil
            val refinements = openOpaqueAliases(cls.givenSelfType)
            val refinedType = refinements.foldLeft(ref: Type) ((parent, refinement) =>
              RefinedType(parent, refinement._1, TypeAlias(refinement._2))
            )
            val refiningSym = newSym(InlineBinderName.fresh(), Synthetic, refinedType).asTerm
            val refiningDef = ValDef(refiningSym, tpd.ref(ref).cast(refinedType)).withSpan(span)
            inlining.println(i"add opaque alias proxy $refiningDef for $ref in $tp")
            bindingsBuf += refiningDef
            opaqueProxies += ((ref, refiningSym.termRef))
      case _ =>
    }

  /** Map all TermRefs that match left element in `opaqueProxies` to the
   *  corresponding right element.
   */
  private val mapOpaques = TreeTypeMap(
      typeMap = new TypeMap:
          override def stopAt = StopAt.Package
          def apply(t: Type) = mapOver {
            t match
              case ref: TermRef => mapRef(ref).getOrElse(ref)
              case _ => t
          }
    )

  /** If `binding` contains TermRefs that refer to objects with opaque
   *  type aliases, add proxy definitions that expose these aliases
   *  and substitute such TermRefs with theproxies. Example from pos/opaque-inline1.scala:
   *
   *  object refined:
   *    opaque type Positive = Int
   *    inline def Positive(value: Int): Positive = f(value)
   *    def f(x: Positive): Positive = x
   *  def run: Unit = { val x = 9; val nine = refined.Positive(x) }
   *
   *  This generates the following proxies:
   *
   *    val $proxy1: refined.type{type Positive = Int} =
   *      refined.$asInstanceOf$[refined.type{type Positive = Int}]
   *    val refined$_this: ($proxy1 : refined.type{Positive = Int}) =
   *      $proxy1
   *
   *  and every reference to `refined` in the inlined expression is replaced by
   *  `refined_$this`.
   */
  private def accountForOpaques(binding: ValDef)(using Context): ValDef =
    addOpaqueProxies(binding.symbol.info, binding.span, forThisProxy = true)
    if opaqueProxies.isEmpty then binding
    else
      binding.symbol.info = mapOpaques.typeMap(binding.symbol.info)
      mapOpaques.transform(binding).asInstanceOf[ValDef]
        .showing(i"transformed this binding exposing opaque aliases: $result", inlining)
  end accountForOpaques

  /** If value argument contains references to objects that contain opaque types,
   *  map them to their opaque proxies.
   */
  private def mapOpaquesInValueArg(arg: Tree)(using Context): Tree =
    val argType = arg.tpe.widenDealias
    addOpaqueProxies(argType, arg.span, forThisProxy = false)
    if opaqueProxies.nonEmpty then
      val mappedType = mapOpaques.typeMap(argType)
      if mappedType ne argType then arg.cast(AndType(arg.tpe, mappedType))
      else arg
    else arg

  private def canElideThis(tpe: ThisType): Boolean =
    inlineCallPrefix.tpe == tpe && ctx.owner.isContainedIn(tpe.cls)
    || tpe.cls.isContainedIn(inlinedMethod)
    || tpe.cls.is(Package)
    || tpe.cls.isStaticOwner && !(tpe.cls.seesOpaques && inlinedMethod.isContainedIn(tpe.cls))

  private def adaptToPrefix(tp: Type) = tp.asSeenFrom(inlineCallPrefix.tpe, inlinedMethod.owner)

  /** Populate `thisProxy` and `paramProxy` as follows:
   *
   *  1a. If given type refers to a static this, thisProxy binds it to corresponding global reference,
   *  1b. If given type refers to an instance this to a class that is not contained in the
   *      inline method, create a proxy symbol and bind the thistype to refer to the proxy.
   *      The proxy is not yet entered in `bindingsBuf`; that will come later.
   *  2.  If given type refers to a parameter, make `paramProxy` refer to the entry stored
   *      in `paramNames` under the parameter's name. This roundabout way to bind parameter
   *      references to proxies is done because  we don't know a priori what the parameter
   *      references of a method are (we only know the method's type, but that contains TypeParamRefs
   *      and MethodParams, not TypeRefs or TermRefs.
   */
  private def registerType(tpe: Type): Unit = tpe match {
    case tpe: ThisType if !canElideThis(tpe) && !thisProxy.contains(tpe.cls) =>
      val proxyName = s"${tpe.cls.name}_this".toTermName
      val proxyType = inlineCallPrefix.tpe.dealias.tryNormalize match {
        case typeMatchResult if typeMatchResult.exists => typeMatchResult
        case _ => adaptToPrefix(tpe).widenIfUnstable
      }
      thisProxy(tpe.cls) = newSym(proxyName, InlineProxy, proxyType).termRef
      for (param <- tpe.cls.typeParams)
        paramProxy(param.typeRef) = adaptToPrefix(param.typeRef)
    case tpe: NamedType
    if tpe.symbol.is(Param)
        && tpe.symbol.owner == inlinedMethod
        && (tpe.symbol.isTerm || inlinedMethod.paramSymss.exists(_.contains(tpe.symbol)))
          // this test is needed to rule out nested LambdaTypeTree parameters
          // with the same name as the method's parameters. Note that the nested
          // LambdaTypeTree parameters also have the inlineMethod as owner. C.f. i13460.scala.
        && !paramProxy.contains(tpe) =>
      paramBinding.get(tpe.name) match
        case Some(bound) => paramProxy(tpe) = bound
        case _ =>  // can happen for params bound by type-lambda trees.

      // The widened type may contain param types too (see tests/pos/i12379a.scala)
      if tpe.isTerm then registerType(tpe.widenTermRefExpr)
    case _ =>
  }

  private val registerTypes = new TypeTraverser:
    override def stopAt = StopAt.Package
    override def traverse(t: Type) =
      registerType(t)
      traverseChildren(t)

  /** Register type of leaf node */
  private def registerLeaf(tree: Tree): Unit = tree match
    case _: This | _: Ident | _: TypeTree => registerTypes.traverse(tree.typeOpt)
    case _ =>

  /** Make `tree` part of inlined expansion. This means its owner has to be changed
   *  from its `originalOwner`, and, if it comes from outside the inlined method
   *  itself, it has to be marked as an inlined argument.
   */
  private def integrate(tree: Tree, originalOwner: Symbol)(using Context): Tree =
    // assertAllPositioned(tree)   // debug
    tree.changeOwner(originalOwner, ctx.owner)

  def tryConstValue: Tree =
    TypeComparer.constValue(callTypeArgs.head.tpe) match {
      case Some(c) => Literal(c).withSpan(call.span)
      case _ => EmptyTree
    }

  val reducer = new InlineReducer(this)

  /** The Inlined node representing the inlined call */
  def inlined(rhsToInline: tpd.Tree): (List[MemberDef], Tree) =

    inlining.println(i"-----------------------\nInlining $call\nWith RHS $rhsToInline")

    def paramTypess(call: Tree, acc: List[List[Type]]): List[List[Type]] = call match
      case Apply(fn, args) =>
        fn.tpe.widen.match
          case mt: MethodType => paramTypess(fn, mt.instantiateParamInfos(args.tpes) :: acc)
          case _ => Nil
      case TypeApply(fn, _) => paramTypess(fn, acc)
      case _ => acc

    val paramBindings =
      val mappedCallValueArgss = callValueArgss.nestedMapConserve(mapOpaquesInValueArg)
      if mappedCallValueArgss ne callValueArgss then
        inlining.println(i"mapped value args = ${mappedCallValueArgss.flatten}%, %")

      val paramBindingsBuf = new DefBuffer
      // Compute bindings for all parameters, appending them to bindingsBuf
      if !computeParamBindings(
          inlinedMethod.info, callTypeArgs,
          mappedCallValueArgss, paramTypess(call, Nil),
          paramBindingsBuf)
      then
        return (Nil, EmptyTree)

      paramBindingsBuf.toList
    end paramBindings

    // make sure prefix is executed if it is impure
    if !isIdempotentExpr(inlineCallPrefix) then registerType(inlinedMethod.owner.thisType)

    // Register types of all leaves of inlined body so that the `paramProxy` and `thisProxy` maps are defined.
    rhsToInline.foreachSubTree(registerLeaf)

    // Compute bindings for all this-proxies, appending them to bindingsBuf
    computeThisBindings()

    // Parameter bindings come after this bindings, reflecting order of evaluation
    bindingsBuf ++= paramBindings

    val inlineTyper = new InlineTyper(ctx.reporter.errorCount)

    val inlineCtx = inlineContext(call).fresh.setTyper(inlineTyper).setNewScope

    def inlinedFromOutside(tree: Tree)(span: Span): Tree =
      Inlined(EmptyTree, Nil, tree)(using ctx.withSource(inlinedMethod.topLevelClass.source)).withSpan(span)

    // A tree type map to prepare the inlined body for typechecked.
    // The translation maps references to `this` and parameters to
    // corresponding arguments or proxies on the type and term level. It also changes
    // the owner from the inlined method to the current owner.
    val inliner = new InlinerMap(
      typeMap =
        new DeepTypeMap {
          override def stopAt =
            if opaqueProxies.isEmpty then StopAt.Static else StopAt.Package
          def apply(t: Type) = t match {
            case t: ThisType => thisProxy.getOrElse(t.cls, t)
            case t: TypeRef => paramProxy.getOrElse(t, mapOver(t))
            case t: SingletonType =>
              if t.termSymbol.isAllOf(InlineParam) then apply(t.widenTermRefExpr)
              else paramProxy.getOrElse(t, mapOver(t))
            case t => mapOver(t)
          }
        },
      treeMap = {
        case tree: This =>
          tree.tpe match {
            case thistpe: ThisType =>
              thisProxy.get(thistpe.cls) match {
                case Some(t) =>
                  val thisRef = ref(t).withSpan(call.span)
                  inlinedFromOutside(thisRef)(tree.span)
                case None => tree
              }
            case _ => tree
          }
        case tree: Ident =>
          /* Span of the argument. Used when the argument is inlined directly without a binding */
          def argSpan =
            if (tree.name == nme.WILDCARD) tree.span // From type match
            else if (tree.symbol.isTypeParam && tree.symbol.owner.isClass) tree.span // TODO is this the correct span?
            else paramSpan(tree.name)
          val inlinedCtx = ctx.withSource(inlinedMethod.topLevelClass.source)
          paramProxy.get(tree.tpe) match {
            case Some(t) if tree.isTerm && t.isSingleton =>
              val inlinedSingleton = singleton(t).withSpan(argSpan)
              inlinedFromOutside(inlinedSingleton)(tree.span)
            case Some(t) if tree.isType =>
              inlinedFromOutside(TypeTree(t).withSpan(argSpan))(tree.span)
            case _ => tree
          }
        case tree @ Select(qual: This, name) if tree.symbol.is(Private) && tree.symbol.isInlineMethod =>
          // This inline method refers to another (private) inline method (see tests/pos/i14042.scala).
          // We insert upcast to access the private inline method once inlined. This makes the selection
          // keep the symbol when re-typechecking in the InlineTyper. The method is inlined and hence no
          // reference to a private method is kept at runtime.
          cpy.Select(tree)(qual.asInstance(qual.tpe.widen), name)

        case tree => tree
      },
      oldOwners = inlinedMethod :: Nil,
      newOwners = ctx.owner :: Nil,
      substFrom = Nil,
      substTo = Nil
    )(using inlineCtx)

    inlining.println(
      i"""inliner transform with
         |thisProxy = ${thisProxy.toList.map(_._1)}%, % --> ${thisProxy.toList.map(_._2)}%, %
         |paramProxy = ${paramProxy.toList.map(_._1.typeSymbol.showLocated)}%, % --> ${paramProxy.toList.map(_._2)}%, %""")

    // Apply inliner to `rhsToInline`, split off any implicit bindings from result, and
    // make them part of `bindingsBuf`. The expansion is then the tree that remains.
    val expansion = inliner.transform(rhsToInline)

    def issueError() = callValueArgss match {
      case (msgArg :: Nil) :: Nil =>
        val message = msgArg.tpe match {
          case ConstantType(Constant(msg: String)) => msg
          case _ => s"A literal string is expected as an argument to `compiletime.error`. Got ${msgArg.show}"
        }
        // Usually `error` is called from within a rewrite method. In this
        // case we need to report the error at the point of the outermost enclosing inline
        // call. This way, a defensively written rewrite method can always
        // report bad inputs at the point of call instead of revealing its internals.
        val callToReport = if (enclosingInlineds.nonEmpty) enclosingInlineds.last else call
        val ctxToReport = ctx.outersIterator.dropWhile(enclosingInlineds(using _).nonEmpty).next
        // The context in which we report should still use the existing context reporter
        val ctxOrigReporter = ctxToReport.fresh.setReporter(ctx.reporter)
        inContext(ctxOrigReporter) {
          report.error(new reporting.Custom(message), callToReport.srcPos)
        }
      case _ =>
    }

    /** The number of nodes in this tree, excluding code in nested inline
     *  calls and annotations of definitions.
     */
    def treeSize(x: Any): Int =
      var siz = 0
      x match
        case x: Trees.Inlined[_] =>
        case x: Positioned =>
          var i = 0
          while i < x.productArity do
            siz += treeSize(x.productElement(i))
            i += 1
        case x: List[_] =>
          var xs = x
          while xs.nonEmpty do
            siz += treeSize(xs.head)
            xs = xs.tail
        case _ =>
      siz

    trace(i"inlining $call", inlining, show = true) {

      // The normalized bindings collected in `bindingsBuf`
      bindingsBuf.mapInPlace { binding =>
        // Set trees to symbols allow macros to see the definition tree.
        // This is used by `underlyingArgument`.
        val binding1 = reducer.normalizeBinding(binding)(using inlineCtx).setDefTree
        binding1.foreachSubTree {
          case tree: MemberDef => tree.setDefTree
          case _ =>
        }
        binding1
      }

      // Run a typing pass over the inlined tree. See InlineTyper for details.
      val expansion1 = inlineTyper.typed(expansion)(using inlineCtx)

      if (ctx.settings.verbose.value) {
        inlining.println(i"to inline = $rhsToInline")
        inlining.println(i"original bindings = ${bindingsBuf.toList}%\n%")
        inlining.println(i"original expansion = $expansion1")
      }

      // Drop unused bindings
      val (finalBindings, finalExpansion) = dropUnusedDefs(bindingsBuf.toList, expansion1)

      if (inlinedMethod == defn.Compiletime_error) issueError()

      addInlinedTrees(treeSize(finalExpansion))

      (finalBindings, finalExpansion)
    }
  end inlined

  /** An extractor for references to inlineable arguments. These are :
    *   - by-value arguments marked with `inline`
    *   - all by-name arguments
    */
  private object InlineableArg {
    lazy val paramProxies = paramProxy.values.toSet
    def unapply(tree: Trees.Ident[?])(using Context): Option[Tree] = {
      def search(buf: DefBuffer) = buf.find(_.name == tree.name)
      if (paramProxies.contains(tree.typeOpt))
        search(bindingsBuf) match {
          case Some(bind: ValOrDefDef) if bind.symbol.is(Inline) =>
            Some(integrate(bind.rhs, bind.symbol))
          case _ => None
        }
      else None
    }
  }

  private[inlines] def tryInlineArg(tree: Tree)(using Context): Tree = tree match {
    case InlineableArg(rhs) =>
      inlining.println(i"inline arg $tree -> $rhs")
      rhs
    case _ =>
      EmptyTree
  }

  /** A typer for inlined bodies. Beyond standard typing, an inline typer performs
   *  the following functions:
   *
   *  1. Implement constant folding over inlined code
   *  2. Selectively expand ifs with constant conditions
   *  3. Inline arguments that are by-name closures
   *  4. Make sure inlined code is type-correct.
   *  5. Make sure that the tree's typing is idempotent (so that future -Ycheck passes succeed)
   */
  class InlineTyper(initialErrorCount: Int, @constructorOnly nestingLevel: Int = ctx.nestingLevel + 1)
  extends ReTyper(nestingLevel):
    import reducer._

    override def ensureAccessible(tpe: Type, superAccess: Boolean, pos: SrcPos)(using Context): Type = {
      tpe match {
        case tpe: NamedType if tpe.symbol.exists && !tpe.symbol.isAccessibleFrom(tpe.prefix, superAccess) =>
          tpe.info match {
            case TypeAlias(alias) => return ensureAccessible(alias, superAccess, pos)
            case info: ConstantType if tpe.symbol.isStableMember => return info
            case _ =>
          }
        case _ =>
      }
      super.ensureAccessible(tpe, superAccess, pos)
    }

    /** Enter implicits in scope so that they can be found in implicit search.
     *  This is important for non-transparent inlines
     */
    override def index(trees: List[untpd.Tree])(using Context): Context =
      for case tree: untpd.MemberDef <- trees do
        if tree.symbol.isOneOf(Flags.GivenOrImplicit) then
          ctx.scope.openForMutations.enter(tree.symbol)
      ctx

    override def typedIdent(tree: untpd.Ident, pt: Type)(using Context): Tree =
      val tree1 = inlineIfNeeded(
          tryInlineArg(tree.asInstanceOf[tpd.Tree]) `orElse` super.typedIdent(tree, pt)
        )
      tree1 match
        case id: Ident if tpd.needsSelect(id.tpe) =>
          inlining.println(i"expanding $id to selection")
          ref(id.tpe.asInstanceOf[TermRef]).withSpan(id.span)
        case _ =>
          tree1

    override def typedSelect(tree: untpd.Select, pt: Type)(using Context): Tree = {
      val qual1 = typed(tree.qualifier, shallowSelectionProto(tree.name, pt, this))
      val resNoReduce = untpd.cpy.Select(tree)(qual1, tree.name).withType(tree.typeOpt)
      val reducedProjection = reducer.reduceProjection(resNoReduce)
      if reducedProjection.isType then
        //if the projection leads to a typed tree then we stop reduction
        resNoReduce
      else
        val res = constToLiteral(reducedProjection)
        if resNoReduce ne res then
          typed(res, pt) // redo typecheck if reduction changed something
        else if res.symbol.isInlineMethod then
          inlineIfNeeded(res)
        else
          ensureAccessible(res.tpe, tree.qualifier.isInstanceOf[untpd.Super], tree.srcPos)
          res
    }

    override def typedIf(tree: untpd.If, pt: Type)(using Context): Tree =
      val condCtx = if tree.isInline then ctx.addMode(Mode.ForceInline) else ctx
      typed(tree.cond, defn.BooleanType)(using condCtx) match {
        case cond1 @ ConstantValue(b: Boolean) =>
          val selected0 = if (b) tree.thenp else tree.elsep
          val selected = if (selected0.isEmpty) tpd.Literal(Constant(())) else typed(selected0, pt)
          if (isIdempotentExpr(cond1)) selected
          else Block(cond1 :: Nil, selected)
        case cond1 =>
          if (tree.isInline)
            errorTree(tree,
              em"Cannot reduce `inline if` because its condition is not a constant value: $cond1")
          else
            cond1.computeNullableDeeply()
            val if1 = untpd.cpy.If(tree)(cond = untpd.TypedSplice(cond1))
            super.typedIf(if1, pt)
      }

    override def typedValDef(vdef: untpd.ValDef, sym: Symbol)(using Context): Tree =
      val vdef1 =
        if sym.is(Inline) then
          val rhs = typed(vdef.rhs)
          sym.info = rhs.tpe
          untpd.cpy.ValDef(vdef)(vdef.name, untpd.TypeTree(rhs.tpe), untpd.TypedSplice(rhs))
        else vdef
      super.typedValDef(vdef1, sym)

    override def typedApply(tree: untpd.Apply, pt: Type)(using Context): Tree =
      def cancelQuotes(tree: Tree): Tree =
        tree match
          case Quoted(Spliced(inner)) => inner
          case _ => tree
      val res = cancelQuotes(constToLiteral(betaReduce(super.typedApply(tree, pt)))) match {
        case res: Apply if res.symbol == defn.QuotedRuntime_exprSplice
                        && StagingContext.level == 0
                        && !hasInliningErrors =>
          val expanded = expandMacro(res.args.head, tree.srcPos)
          typedExpr(expanded) // Inline calls and constant fold code generated by the macro
        case res =>
          specializeEq(inlineIfNeeded(res))
      }
      res

    override def typedTypeApply(tree: untpd.TypeApply, pt: Type)(using Context): Tree =
      val tree1 = inlineIfNeeded(constToLiteral(betaReduce(super.typedTypeApply(tree, pt))))
      if tree1.symbol.isQuote then
        ctx.compilationUnit.needsStaging = true
      tree1

    override def typedMatch(tree: untpd.Match, pt: Type)(using Context): Tree =
      val tree1 =
        if tree.isInline then
          // TODO this might not be useful if we do not support #11291
          val sel1 = typedExpr(tree.selector)(using ctx.addMode(Mode.ForceInline))
          untpd.cpy.Match(tree)(sel1, tree.cases)
        else tree
      super.typedMatch(tree1, pt)

    override def typedMatchFinish(tree: untpd.Match, sel: Tree, wideSelType: Type, cases: List[untpd.CaseDef], pt: Type)(using Context) =
      if (!tree.isInline || ctx.owner.isInlineMethod) // don't reduce match of nested inline method yet
        super.typedMatchFinish(tree, sel, wideSelType, cases, pt)
      else {
        def selTyped(sel: Tree): Type = sel match {
          case Typed(sel2, _) => selTyped(sel2)
          case Block(Nil, sel2) => selTyped(sel2)
          case Inlined(_, Nil, sel2) => selTyped(sel2)
          case _ => sel.tpe
        }
        val selType = if (sel.isEmpty) wideSelType else selTyped(sel)
        reduceInlineMatch(sel, selType, cases.asInstanceOf[List[CaseDef]], this) match {
          case Some((caseBindings, rhs0)) =>
            // drop type ascriptions/casts hiding pattern-bound types (which are now aliases after reducing the match)
            // note that any actually necessary casts will be reinserted by the typing pass below
            val rhs1 = rhs0 match {
              case Block(stats, t) if t.span.isSynthetic =>
                t match {
                  case Typed(expr, _) =>
                    Block(stats, expr)
                  case TypeApply(sel@Select(expr, _), _) if sel.symbol.isTypeCast =>
                    Block(stats, expr)
                  case _ =>
                    rhs0
                }
              case _ => rhs0
            }
            val (usedBindings, rhs2) = dropUnusedDefs(caseBindings, rhs1)
            val rhs = seq(usedBindings, rhs2)
            inlining.println(i"""--- reduce:
                                |$tree
                                |--- to:
                                |$rhs""")
            typedExpr(rhs, pt)
          case None =>
            def guardStr(guard: untpd.Tree) = if (guard.isEmpty) "" else i" if $guard"
            def patStr(cdef: untpd.CaseDef) = i"case ${cdef.pat}${guardStr(cdef.guard)}"
            val msg =
              if (tree.selector.isEmpty)
                em"""cannot reduce summonFrom with
                   | patterns :  ${tree.cases.map(patStr).mkString("\n             ")}"""
              else
                em"""cannot reduce inline match with
                    | scrutinee:  $sel : ${selType}
                    | patterns :  ${tree.cases.map(patStr).mkString("\n             ")}"""
            errorTree(tree, msg)
        }
      }

    override def newLikeThis(nestingLevel: Int): Typer = new InlineTyper(initialErrorCount, nestingLevel)

    /** True if this inline typer has already issued errors */
    override def hasInliningErrors(using Context) = ctx.reporter.errorCount > initialErrorCount

    private def inlineIfNeeded(tree: Tree)(using Context): Tree =
      val meth = tree.symbol
      if meth.isAllOf(DeferredInline) then
        errorTree(tree, i"Deferred inline ${meth.showLocated} cannot be invoked")
      else if Inlines.needsInlining(tree) then Inlines.inlineCall(tree)
      else tree

    override def typedUnadapted(tree: untpd.Tree, pt: Type, locked: TypeVars)(using Context): Tree =
      super.typedUnadapted(tree, pt, locked) match
        case member: MemberDef => member.setDefTree
        case tree => tree

    private def specializeEq(tree: Tree): Tree =
      tree match
        case Apply(sel @ Select(arg1, opName), arg2 :: Nil)
        if sel.symbol == defn.Any_== || sel.symbol == defn.Any_!= =>
          defn.ScalaValueClasses().find { cls =>
            arg1.tpe.derivesFrom(cls) && arg2.tpe.derivesFrom(cls)
          } match
            case Some(cls) =>
              val newOp = cls.requiredMethod(opName, List(cls.typeRef))
              arg1.select(newOp).withSpan(sel.span).appliedTo(arg2).withSpan(tree.span)
            case None => tree
        case _ =>
          tree
  end InlineTyper

  /** Drop any side-effect-free bindings that are unused in expansion or other reachable bindings.
   *  Inline def bindings that are used only once.
   */
  private def dropUnusedDefs(bindings: List[MemberDef], tree: Tree)(using Context): (List[MemberDef], Tree) = {
    // inlining.println(i"drop unused $bindings%, % in $tree")
    val (termBindings, typeBindings) = bindings.partition(_.symbol.isTerm)
    if (typeBindings.nonEmpty) {
      val typeBindingsSet = typeBindings.foldLeft[SimpleIdentitySet[Symbol]](SimpleIdentitySet.empty)(_ + _.symbol)
      val inlineTypeBindings = new TreeTypeMap(
        typeMap = new TypeMap() {
          override def apply(tp: Type): Type = tp match {
            case tr: TypeRef if tr.prefix.eq(NoPrefix) && typeBindingsSet.contains(tr.symbol) =>
              val TypeAlias(res) = tr.info: @unchecked
              res
            case tp => mapOver(tp)
          }
        },
        treeMap = {
          case ident: Ident if ident.isType && typeBindingsSet.contains(ident.symbol) =>
            val TypeAlias(r) = ident.symbol.info: @unchecked
            TypeTree(r).withSpan(ident.span)
          case tree => tree
        }
      )
      val Block(termBindings1, tree1) = inlineTypeBindings(Block(termBindings, tree))
      dropUnusedDefs(termBindings1.asInstanceOf[List[ValOrDefDef]], tree1)
    }
    else {
      val refCount = MutableSymbolMap[Int]()
      val bindingOfSym = MutableSymbolMap[MemberDef]()

      def isInlineable(binding: MemberDef) = binding match {
        case ddef @ DefDef(_, Nil, _, _) => isElideableExpr(ddef.rhs)
        case vdef @ ValDef(_, _, _) => isElideableExpr(vdef.rhs)
        case _ => false
      }
      for (binding <- bindings if isInlineable(binding)) {
        refCount(binding.symbol) = 0
        bindingOfSym(binding.symbol) = binding
      }

      val countRefs = new TreeTraverser {
        override def traverse(t: Tree)(using Context) = {
          def updateRefCount(sym: Symbol, inc: Int) =
            for (x <- refCount.get(sym)) refCount(sym) = x + inc
          def updateTermRefCounts(t: Tree) =
            t.typeOpt.foreachPart {
              case ref: TermRef => updateRefCount(ref.symbol, 2) // can't be inlined, so make sure refCount is at least 2
              case _ =>
            }

          t match {
            case t: RefTree =>
              updateRefCount(t.symbol, 1)
              updateTermRefCounts(t)
            case _: New | _: TypeTree =>
              updateTermRefCounts(t)
            case _ =>
          }
          traverseChildren(t)
        }
      }
      countRefs.traverse(tree)
      for (binding <- bindings) countRefs.traverse(binding)

      def retain(boundSym: Symbol) = {
        refCount.get(boundSym) match {
          case Some(x) => x > 1 || x == 1 && !boundSym.is(Method)
          case none => true
        }
      } && !boundSym.is(Inline)

      val inlineBindings = new TreeMap {
        override def transform(t: Tree)(using Context) = t match {
          case t: RefTree =>
            val sym = t.symbol
            val t1 = refCount.get(sym) match {
              case Some(1) =>
                bindingOfSym(sym) match {
                  case binding: ValOrDefDef => integrate(binding.rhs, sym)
                }
              case none => t
            }
            super.transform(t1)
          case t: Apply =>
            val t1 = super.transform(t)
            if (t1 `eq` t) t else reducer.betaReduce(t1)
          case Block(Nil, expr) =>
            super.transform(expr)
          case _ =>
            super.transform(t)
        }
      }

      val retained = bindings.filterConserve(binding => retain(binding.symbol))
      if (retained `eq` bindings)
        (bindings, tree)
      else {
        val expanded = inlineBindings.transform(tree)
        dropUnusedDefs(retained, expanded)
      }
    }
  }

  private def expandMacro(body: Tree, splicePos: SrcPos)(using Context) = {
    assert(StagingContext.level == 0)
    val inlinedFrom = enclosingInlineds.last
    val dependencies = macroDependencies(body)
    val suspendable = ctx.compilationUnit.isSuspendable
    if dependencies.nonEmpty && !ctx.reporter.errorsReported then
      for sym <- dependencies do
        if ctx.compilationUnit.source.file == sym.associatedFile then
          report.error(em"Cannot call macro $sym defined in the same source file", call.srcPos)
        if (suspendable && ctx.settings.XprintSuspension.value)
          report.echo(i"suspension triggered by macro call to ${sym.showLocated} in ${sym.associatedFile}", call.srcPos)
      if suspendable then
        ctx.compilationUnit.suspend() // this throws a SuspendException

    val evaluatedSplice = inContext(quoted.MacroExpansion.context(inlinedFrom)) {
      Splicer.splice(body, splicePos, inlinedFrom.srcPos, MacroClassLoader.fromContext)
    }
    val inlinedNormailizer = new TreeMap {
      override def transform(tree: tpd.Tree)(using Context): tpd.Tree = tree match {
        case Inlined(EmptyTree, Nil, expr) if enclosingInlineds.isEmpty => transform(expr)
        case _ => super.transform(tree)
      }
    }
    val normalizedSplice = inlinedNormailizer.transform(evaluatedSplice)
    if (normalizedSplice.isEmpty) normalizedSplice
    else normalizedSplice.withSpan(splicePos.span)
  }

  /** Return the set of symbols that are referred at level -1 by the tree and defined in the current run.
   *  This corresponds to the symbols that will need to be interpreted.
   */
  private def macroDependencies(tree: Tree)(using Context) =
    new TreeAccumulator[List[Symbol]] {
      private var level = -1
      override def apply(syms: List[Symbol], tree: tpd.Tree)(using Context): List[Symbol] =
        if level != -1 then foldOver(syms, tree)
        else tree match {
          case tree: RefTree if tree.isTerm && tree.symbol.isDefinedInCurrentRun && !tree.symbol.isLocal =>
            foldOver(tree.symbol :: syms, tree)
          case Quoted(body) =>
            level += 1
            try apply(syms, body)
            finally level -= 1
          case Spliced(body) =>
            level -= 1
            try apply(syms, body)
            finally level += 1
          case SplicedType(body) =>
            level -= 1
            try apply(syms, body)
            finally level += 1
          case _: TypTree =>
            syms
          case _ =>
            foldOver(syms, tree)
        }
    }.apply(Nil, tree)
end Inliner
