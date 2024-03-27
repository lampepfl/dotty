package dotty.tools
package dotc
package transform

import core.Names.Name
import core.DenotTransformers.*
import core.SymDenotations.*
import core.Contexts.*
import core.Symbols.*
import core.Types.*
import core.Flags.*
import core.StdNames.*
import core.NameKinds.{DocArtifactName, OuterSelectName}
import core.Decorators.*
import core.Phases.*
import core.Mode
import typer.*
import reporting.*
import ast.Trees.*
import ast.{tpd, untpd}
import util.Chars.*
import collection.mutable
import ProtoTypes.*
import staging.StagingLevel
import inlines.Inlines.inInlineMethod
import cc.{isRetainsLike, CaptureAnnotation}

import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions

import scala.util.control.NonFatal

/** Run by -Ycheck option after a given phase, this class retypes all syntax trees
 *  and verifies that the type of each tree node so obtained conforms to the type found in the tree node.
 *  It also performs the following checks:
 *
 *   - The owner of each definition is the same as the owner of the current typing context.
 *   - Ident nodes do not refer to a denotation that would need a select to be accessible
 *     (see tpd.needsSelect).
 *   - After typer, identifiers and select nodes refer to terms only (all types should be
 *     represented as TypeTrees then).
 */
class TreeChecker extends Phase with SymTransformer {
  import ast.tpd.*
  import TreeChecker.*

  private val seenClasses = collection.mutable.HashMap[String, Symbol]()
  private val seenModuleVals = collection.mutable.HashMap[String, Symbol]()

  val NoSuperClassFlags: FlagSet = Trait | Package

  def testDuplicate(sym: Symbol, registry: mutable.Map[String, Symbol], typ: String)(using Context): Unit = {
    val name = sym.javaClassName
    val isDuplicate = this.flatClasses && registry.contains(name)
    assert(!isDuplicate, s"$typ defined twice $sym ${sym.id} ${registry(name).id}")
    registry(name) = sym
  }

  def checkCompanion(symd: SymDenotation)(using Context): Unit = {
    val cur = symd.linkedClass
    val prev = atPhase(ctx.phase.prev) {
      symd.symbol.linkedClass
    }

    if (prev.exists)
      assert(cur.exists || prev.is(ConstructorProxy), i"companion disappeared from $symd")
  }

  def transformSym(symd: SymDenotation)(using Context): SymDenotation = {
    val sym = symd.symbol

    if (sym.isClass && !sym.isAbsent()) {
      val validSuperclass = sym.isPrimitiveValueClass || defn.syntheticCoreClasses.contains(sym) ||
        (sym eq defn.ObjectClass) || sym.isOneOf(NoSuperClassFlags) || (sym.asClass.superClass.exists) ||
        sym.isRefinementClass

      assert(validSuperclass, i"$sym has no superclass set")
      testDuplicate(sym, seenClasses, "class")
    }

    val badDeferredAndPrivate =
      sym.is(Method) && sym.is(Deferred) && sym.is(Private)
      && !sym.hasAnnotation(defn.NativeAnnot)
      && !sym.isEffectivelyErased

    assert(!badDeferredAndPrivate, i"$sym is both Deferred and Private")

    checkCompanion(symd)

    // Signatures are used to disambiguate overloads and need to stay stable
    // until erasure, see the comment above `Compiler#phases`.
    if (ctx.phaseId <= erasurePhase.id) {
      val initial = symd.initial
      assert(symd == initial || symd.signature == initial.signature,
        i"""Signature of ${sym} in ${sym.ownersIterator.toList}%, % changed at phase ${ctx.phase.prev.megaPhase}
           |Initial info: ${initial.info}
           |Initial sig : ${initial.signature}
           |Current info: ${symd.info}
           |Current sig : ${symd.signature}""")
    }

    symd
  }

  def phaseName: String = "Ycheck"

  def run(using Context): Unit =
    if (ctx.settings.YtestPickler.value && ctx.phase.prev.isInstanceOf[Pickler])
      report.echo("Skipping Ycheck after pickling with -Ytest-pickler, the returned tree contains stale symbols")
    else if (ctx.phase.prev.isCheckable)
      check(ctx.base.allPhases.toIndexedSeq, ctx)

  def check(phasesToRun: Seq[Phase], ctx: Context): Tree = {
    val fusedPhase = ctx.phase.prev.megaPhase(using ctx)
    report.echo(s"checking ${ctx.compilationUnit} after phase ${fusedPhase}")(using ctx)

    inContext(ctx) {
      assert(ctx.typerState.constraint.domainLambdas.isEmpty,
        i"non-empty constraint at end of $fusedPhase: ${ctx.typerState.constraint}, ownedVars = ${ctx.typerState.ownedVars.toList}%, %")
      assertSelectWrapsNew(ctx.compilationUnit.tpdTree)
      TreeNodeChecker.traverse(ctx.compilationUnit.tpdTree)
    }

    val checkingCtx = ctx
        .fresh
        .setReporter(new ThrowingReporter(ctx.reporter))

    val checker = inContext(ctx) {
      new Checker(previousPhases(phasesToRun.toList))
    }
    try checker.typedExpr(ctx.compilationUnit.tpdTree)(using checkingCtx)
    catch {
      case NonFatal(ex) =>     //TODO CHECK. Check that we are bootstrapped
        inContext(checkingCtx) {
          println(i"*** error while checking ${ctx.compilationUnit} after phase ${ctx.phase.prev.megaPhase(using ctx)} ***")
        }
        throw ex
    }
  }

  /**
    * Checks that `New` nodes are always wrapped inside `Select` nodes.
    */
  def assertSelectWrapsNew(tree: Tree)(using Context): Unit =
    (new TreeAccumulator[tpd.Tree] {
      override def apply(parent: Tree, tree: Tree)(using Context): Tree = {
        tree match {
          case tree: New if !parent.isInstanceOf[tpd.Select] =>
            assert(assertion = false, i"`New` node must be wrapped in a `Select` of the constructor:\n  parent = ${parent.show}\n  child = ${tree.show}")
          case _: Annotated =>
            // Don't check inside annotations, since they're allowed to contain
            // somewhat invalid trees.
          case _ =>
            foldOver(tree, tree) // replace the parent when folding over the children
        }
        parent // return the old parent so that my siblings see it
      }
    })(tpd.EmptyTree, tree)
}

object TreeChecker {
  /** - Check that TypeParamRefs and MethodParams refer to an enclosing type.
   *  - Check that all type variables are instantiated.
   */
  def checkNoOrphans(tp0: Type, tree: untpd.Tree = untpd.EmptyTree)(using Context): Type = new TypeMap() {
    val definedBinders = new java.util.IdentityHashMap[Type, Any]
    private var inRetainingAnnot = false

    def insideRetainingAnnot[T](op: => T): T =
      val saved = inRetainingAnnot
      inRetainingAnnot = true
      try op finally inRetainingAnnot = saved

    def apply(tp: Type): Type = {
      tp match {
        case tp: BindingType =>
          definedBinders.put(tp, tp)
          mapOver(tp)
          definedBinders.remove(tp)
        case tp: ParamRef =>
          val isValidRef =
            definedBinders.get(tp.binder) != null
              || inRetainingAnnot
              // Inside a normal @retains annotation, the captured references could be ill-formed. See issue #19661.
              // But this is ok since capture checking does not rely on them.
          assert(isValidRef, s"orphan param: ${tp.show}, hash of binder = ${System.identityHashCode(tp.binder)}, tree = ${tree.show}, type = $tp0")
        case tp: TypeVar =>
          assert(tp.isInstantiated, s"Uninstantiated type variable: ${tp.show}, tree = ${tree.show}")
          apply(tp.underlying)
        case tp @ AnnotatedType(underlying, annot) if annot.symbol.isRetainsLike && !annot.isInstanceOf[CaptureAnnotation] =>
          val underlying1 = this(underlying)
          val annot1 = insideRetainingAnnot:
            annot.mapWith(this)
          derivedAnnotatedType(tp, underlying1, annot1)
        case _ =>
          mapOver(tp)
      }
      tp
    }
  }.apply(tp0)

  def checkParents(sym: ClassSymbol, parents: List[tpd.Tree])(using Context): Unit =
    val symbolParents = sym.classInfo.parents.map(_.dealias.typeSymbol)
    val treeParents = parents.map(_.tpe.dealias.typeSymbol)
    assert(symbolParents == treeParents,
      i"""Parents of class symbol differs from the parents in the tree for $sym
          |
          |Parents in symbol: $symbolParents
          |Parents in tree: $treeParents
          |""".stripMargin)
  end checkParents

  /** Run some additional checks on the nodes of the trees.  Specifically:
   *
   *    - TypeTree can only appear in TypeApply args, New, Typed tpt, Closure
   *      tpt, SeqLiteral elemtpt, ValDef tpt, DefDef tpt, and TypeDef rhs.
   */
  object TreeNodeChecker extends untpd.TreeTraverser:
    import untpd.*
    def traverse(tree: Tree)(using Context) = tree match
      case t: TypeTree                      => assert(assertion = false, i"TypeTree not expected: $t")
      case t @ TypeApply(fun, _targs)       => traverse(fun)
      case t @ New(_tpt)                    =>
      case t @ Typed(expr, _tpt)            => traverse(expr)
      case t @ Closure(env, meth, _tpt)     => traverse(env); traverse(meth)
      case t @ SeqLiteral(elems, _elemtpt)  => traverse(elems)
      case t @ ValDef(_, _tpt, _)           => traverse(t.rhs)
      case t @ DefDef(_, paramss, _tpt, _)  => for params <- paramss do traverse(params); traverse(t.rhs)
      case t @ TypeDef(_, _rhs)             =>
      case t @ Template(constr, _, self, _) => traverse(constr); traverse(t.parentsOrDerived); traverse(self); traverse(t.body)
      case t                                => traverseChildren(t)
    end traverse

  private[TreeChecker] def isValidJVMName(name: Name): Boolean = name.toString.forall(isValidJVMChar)

  private[TreeChecker] def isValidJVMMethodName(name: Name): Boolean = name.toString.forall(isValidJVMMethodChar)


  class Checker(phasesToCheck: Seq[Phase]) extends ReTyper with Checking {
    import ast.tpd.*

    protected val nowDefinedSyms = util.HashSet[Symbol]()
    private val patBoundSyms = util.HashSet[Symbol]()
    private val everDefinedSyms = MutableSymbolMap[untpd.Tree]()

    // don't check value classes after typer, as the constraint about constructors doesn't hold after transform
    override def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(using Context): Unit = ()

    def withDefinedSyms[T](trees: List[untpd.Tree])(op: => T)(using Context): T = {
      var locally = List.empty[Symbol]
      for (tree <- trees) {
        val sym = tree.symbol
        tree match {
          case tree: untpd.DefTree =>
            assert(isValidJVMName(sym.name.encode), s"${sym.name.debugString} name is invalid on jvm")
            everDefinedSyms.get(sym) match {
              case Some(t)  =>
                if (t ne tree)
                  report.warning(i"symbol ${sym.fullName} is defined at least twice in different parts of AST")
              // should become an error
              case None =>
                everDefinedSyms(sym) = tree
            }
            assert(!nowDefinedSyms.contains(sym), i"doubly defined symbol: ${sym.fullName} in $tree")

            if (ctx.settings.YcheckMods.value)
              tree match {
                case t: untpd.MemberDef =>
                  if (t.name ne sym.name) report.warning(s"symbol ${sym.fullName} name doesn't correspond to AST: ${t}")
                // todo: compare trees inside annotations
                case _ =>
              }
            locally = sym :: locally
            nowDefinedSyms += sym
          case _ =>
        }
      }
      val res = op
      nowDefinedSyms --= locally
      res
    }

    /** The following invariant holds:
     *
     *  patBoundSyms.contains(sym) <=> sym.isPatternBound
     */
    def withPatSyms[T](syms: List[Symbol])(op: => T)(using Context): T = {
      syms.foreach { sym =>
        assert(
          sym.isPatternBound,
          "patBoundSyms.contains(sym) => sym.isPatternBound is broken." +
          i" Pattern bound symbol $sym has incorrect flags: " + sym.flagsString + ", line " + sym.srcPos.line
        )
      }
      patBoundSyms ++= syms
      val res = op
      patBoundSyms --= syms
      res
    }

    // used to check invariant of lambda encoding
    var nestingBlock: untpd.Block | Null = null
    private def withBlock[T](block: untpd.Block)(op: => T): T = {
      val outerBlock = nestingBlock
      nestingBlock = block
      val res = op
      nestingBlock = outerBlock
      res
    }

    def assertDefined(tree: untpd.Tree)(using Context): Unit =
      if (tree.symbol.maybeOwner.isTerm) {
        val sym = tree.symbol
        def isAllowed = // constructor proxies and context bound companions are flagged at PostTyper
          isSymWithoutDef(sym) && ctx.phase.id < postTyperPhase.id
        assert(
          nowDefinedSyms.contains(sym) || patBoundSyms.contains(sym) || isAllowed,
          i"undefined symbol ${sym} in ${sym.owner} at line " + tree.srcPos.line
        )

        if (!ctx.phase.patternTranslated)
          assert(
            !sym.isPatternBound || patBoundSyms.contains(sym),
            i"sym.isPatternBound => patBoundSyms.contains(sym) is broken, sym = $sym, line " + tree.srcPos.line
          )
      }

    /** assert Java classes are not used as objects */
    def assertIdentNotJavaClass(tree: Tree)(using Context): Unit = tree match {
      case _ : untpd.Ident =>
        assert(!tree.symbol.isAllOf(JavaModule), "Java class can't be used as value: " + tree)
      case _ =>
    }

    /** check Java classes are not used as objects */
    def checkIdentNotJavaClass(tree: Tree)(using Context): Unit = tree match {
      // case tree: untpd.Ident =>
      // case tree: untpd.Select =>
      // case tree: untpd.Bind =>
      case md: ValOrDefDef =>
        md.forceFields()
        assertIdentNotJavaClass(md)
      // case tree: untpd.TypeDef =>
      case Apply(fun, args) =>
        assertIdentNotJavaClass(fun)
        args.foreach(assertIdentNotJavaClass _)
      // case tree: untpd.This =>
      // case tree: untpd.Literal =>
      // case tree: untpd.New =>
      case Typed(expr, _) =>
        assertIdentNotJavaClass(expr)
      case NamedArg(_, arg) =>
        assertIdentNotJavaClass(arg)
      case Assign(_, rhs) =>
        assertIdentNotJavaClass(rhs)
      case Block(stats, expr) =>
        stats.foreach(assertIdentNotJavaClass _)
        assertIdentNotJavaClass(expr)
      case If(_, thenp, elsep) =>
        assertIdentNotJavaClass(thenp)
        assertIdentNotJavaClass(elsep)
      // case tree: untpd.Closure =>
      case Match(selector, cases) =>
        assertIdentNotJavaClass(selector)
        cases.foreach(caseDef => assertIdentNotJavaClass(caseDef.body))
      case Return(expr, _) =>
        assertIdentNotJavaClass(expr)
      case Try(expr, cases, finalizer) =>
        assertIdentNotJavaClass(expr)
        cases.foreach(caseDef => assertIdentNotJavaClass(caseDef.body))
        assertIdentNotJavaClass(finalizer)
      // case tree: TypeApply =>
      // case tree: Super =>
      case SeqLiteral(elems, _) =>
        elems.foreach(assertIdentNotJavaClass)
      // case tree: TypeTree =>
      // case tree: SingletonTypeTree =>
      // case tree: RefinedTypeTree =>
      // case tree: AppliedTypeTree =>
      // case tree: ByNameTypeTree =>
      // case tree: TypeBoundsTree =>
      // case tree: Alternative =>
      // case tree: PackageDef =>
      case Annotated(arg, _) =>
        assertIdentNotJavaClass(arg)
      case _ =>
    }

    def isSymWithoutDef(sym: Symbol)(using Context): Boolean =
      sym.is(ConstructorProxy) || sym.isContextBoundCompanion

    /** Exclude from double definition checks any erased symbols that were
     *  made `private` in phase `UnlinkErasedDecls`. These symbols will be removed
     *  completely in phase `Erasure` if they are defined in a currently compiled unit.
     */
    override def excludeFromDoubleDeclCheck(sym: Symbol)(using Context): Boolean =
      sym.isEffectivelyErased && sym.is(Private) && !sym.initial.is(Private)

    /** Check that all invariants related to Super and SuperType are met */
    def checkSuper(tree: Tree)(implicit ctx: Context): Unit = tree match
      case Super(qual, mix) =>
        tree.tpe match
          case tp @ SuperType(thistpe, supertpe) =>
            if (!mix.isEmpty)
              assert(supertpe.isInstanceOf[TypeRef],
                s"Precondition of pickling violated: the supertpe in $tp is not a TypeRef even though $tree has a non-empty mix")
          case tp =>
            assert(false, s"The type of a Super tree must be a SuperType, but $tree has type $tp")
      case _ =>
        tree.tpe match
          case tp: SuperType =>
            assert(false, s"The type of a non-Super tree must not be a SuperType, but $tree has type $tp")
          case _ =>

    override def typed(tree: untpd.Tree, pt: Type = WildcardType)(using Context): Tree = {
      val tpdTree = super.typed(tree, pt)
      Typer.assertPositioned(tree)
      checkSuper(tpdTree)
      if (ctx.erasedTypes)
        // Can't be checked in earlier phases since `checkValue` is only run in
        // Erasure (because running it in Typer would force too much)
        checkIdentNotJavaClass(tpdTree)
      tpdTree
    }

    override def typedUnadapted(tree: untpd.Tree, pt: Type, locked: TypeVars)(using Context): Tree = {
      val res = tree match {
        case _: untpd.TypedSplice | _: untpd.Thicket | _: EmptyValDef[?] =>
          super.typedUnadapted(tree, pt, locked)
        case _ if tree.isType =>
          promote(tree)
        case _ =>
          val tree1 = super.typedUnadapted(tree, pt, locked)
          def isSubType(tp1: Type, tp2: Type) =
            (tp1 eq tp2) || // accept NoType / NoType
            (tp1 <:< tp2)
          def divergenceMsg(tp1: Type, tp2: Type) =
            s"""Types differ
               |Original type : ${tree.typeOpt.show}
               |After checking: ${tree1.tpe.show}
               |Original tree : ${tree.show}
               |After checking: ${tree1.show}
               |Why different :
             """.stripMargin + core.TypeComparer.explained(_.isSubType(tp1, tp2))
          if (tree.hasType) // it might not be typed because Typer sometimes constructs new untyped trees and resubmits them to typedUnadapted
            assert(isSubType(tree1.tpe, tree.typeOpt), divergenceMsg(tree1.tpe, tree.typeOpt))
          tree1
      }
      checkNoOrphans(res.tpe)
      phasesToCheck.foreach(_.checkPostCondition(res))
      res
    }

    def checkNotRepeated(tree: Tree)(using Context): tree.type = {
      def allowedRepeated = tree.tpe.widen.isRepeatedParam

      assert(!tree.tpe.widen.isRepeatedParam || allowedRepeated, i"repeated parameter type not allowed here: $tree")
      tree
    }

    /** Check that all methods have MethodicType */
    def isMethodType(pt: Type)(using Context): Boolean = pt match {
      case at: AnnotatedType => isMethodType(at.parent)
      case _: MethodicType => true  // MethodType, ExprType, PolyType
      case _ => false
    }

    override def typedIdent(tree: untpd.Ident, pt: Type)(using Context): Tree = {
      assert(tree.isTerm || !ctx.isAfterTyper, tree.show + " at " + ctx.phase)
      assert(tree.isType || ctx.mode.is(Mode.Pattern) && untpd.isWildcardArg(tree) || !needsSelect(tree.typeOpt), i"bad type ${tree.tpe} for $tree # ${tree.uniqueId}")
      assertDefined(tree)

      checkNotRepeated(super.typedIdent(tree, pt))
    }

    /** Makes sure the symbol in the tree can be approximately reconstructed by
     *  calling `member` on the qualifier type.
     *  Approximately means: The two symbols might be different but one still overrides the other.
     */
    override def typedSelect(tree: untpd.Select, pt: Type)(using Context): Tree = {
      assert(tree.isTerm || !ctx.isAfterTyper, tree.show + " at " + ctx.phase)
      val tpe = tree.typeOpt

      // PolyFunction apply method stay structural until Erasure
      val isRefinedFunctionApply = (tree.name eq nme.apply) && tree.qualifier.typeOpt.derivesFrom(defn.PolyFunctionClass)

      // Outer selects are pickled specially so don't require a symbol
      val isOuterSelect = tree.name.is(OuterSelectName)
      val isPrimitiveArrayOp = ctx.erasedTypes && nme.isPrimitiveName(tree.name)
      if !(tree.isType || isRefinedFunctionApply || isOuterSelect || isPrimitiveArrayOp) then
        val denot = tree.denot
        assert(denot.exists, i"Selection $tree with type $tpe does not have a denotation")
        assert(denot.symbol.exists, i"Denotation $denot of selection $tree with type $tpe does not have a symbol, qualifier type = ${tree.qualifier.typeOpt}")

      val sym = tree.symbol
      val symIsFixed = tpe match {
        case tpe: TermRef => ctx.erasedTypes || !tpe.isPrefixDependentMemberRef
        case _ => false
      }
      if (sym.exists && !sym.is(Private) &&
          !symIsFixed &&
          !isOuterSelect) { // outer selects have effectively fixed symbols
        val qualTpe = tree.qualifier.typeOpt
        val member =
          if (sym.is(Private)) qualTpe.member(tree.name)
          else qualTpe.nonPrivateMember(tree.name)
        val memberSyms = member.alternatives.map(_.symbol)
        assert(memberSyms.exists(mbr =>
                 sym == mbr ||
                 sym.overriddenSymbol(mbr.owner.asClass) == mbr ||
                 mbr.overriddenSymbol(sym.owner.asClass) == sym),
               i"""symbols differ for $tree
                  |was                 : $sym
                  |alternatives by type: $memberSyms%, % of types ${memberSyms.map(_.info)}%, %
                  |qualifier type      : ${qualTpe}
                  |tree type           : ${tree.typeOpt} of class ${tree.typeOpt.getClass}""")
      }

      checkNotRepeated(super.typedSelect(tree, pt))
    }

    override def typedThis(tree: untpd.This)(using Context): Tree = {
      val res = super.typedThis(tree)
      val cls = res.symbol
      assert(cls.isStaticOwner || ctx.owner.isContainedIn(cls), i"error while typing $tree, ${ctx.owner} is not contained in $cls")
      res
    }

    override def typedSuper(tree: untpd.Super, pt: Type)(using Context): Tree =
      assert(tree.qual.typeOpt.isInstanceOf[ThisType], i"expect prefix of Super to be This, actual = ${tree.qual}")
      super.typedSuper(tree, pt)

    override def typedNew(tree: untpd.New, pt: Type)(using Context): Tree =
      val tree1 = super.typedNew(tree, pt).asInstanceOf[tpd.New]
      val sym = tree1.tpe.typeSymbol
      if postTyperPhase <= ctx.phase then // postTyper checks that `New` nodes can be instantiated
        assert(!tree1.tpe.isInstanceOf[TermRef], s"New should not have a TermRef type: ${tree1.tpe}")
        assert(
          !sym.is(Module)
          || ctx.erasedTypes // TODO add check for module initialization after erasure (LazyVals transformation)
          || ctx.owner == sym.companionModule,
          i"new of $sym module should only exist in ${sym.companionModule} but was in ${ctx.owner}")
      tree1

    override def typedApply(tree: untpd.Apply, pt: Type)(using Context): Tree = tree match
      case Apply(Select(qual, nme.CONSTRUCTOR), _)
          if !ctx.phase.erasedTypes
            && defn.isSpecializedTuple(qual.typeOpt.typeSymbol) =>
        promote(tree) // e.g. `new Tuple2$mcII$sp(7, 8)` should keep its `(7, 8)` type instead of `Tuple2$mcII$sp`
      case _ => super.typedApply(tree, pt)

    override def typedTyped(tree: untpd.Typed, pt: Type)(using Context): Tree =
      val tpt1 = checkSimpleKinded(typedType(tree.tpt))
      val expr1 = tree.expr match
        case id: untpd.Ident if (ctx.mode is Mode.Pattern) && untpd.isVarPattern(id) && (id.name == nme.WILDCARD || id.name == nme.WILDCARD_STAR) =>
          tree.expr.withType(tpt1.tpe)
        case _ =>
          var pt1 = tpt1.tpe
          if pt1.isRepeatedParam then
            pt1 = pt1.translateFromRepeated(toArray = tree.expr.typeOpt.derivesFrom(defn.ArrayClass))
          val isAfterInlining =
            val inliningPhase = ctx.base.inliningPhase
            inliningPhase.exists && ctx.phase.id > inliningPhase.id
          if isAfterInlining then
            // The staging phase destroys in CrossStageSafety the property that
            // tree.expr.tpe <:< pt1. A test case where this arises is run-macros/enum-nat-macro.
            // We should follow up why this happens. If the problem is fixed, we can
            // drop the isAfterInlining special case. To reproduce the problem, just
            // change the condition from `isAfterInlining` to `false`.
            typed(tree.expr)
          else
            //println(i"typing $tree, ${tree.expr.typeOpt}, $pt1, ${ctx.mode is Mode.Pattern}")
            typed(tree.expr, pt1)
      untpd.cpy.Typed(tree)(expr1, tpt1).withType(tree.typeOpt)

    private def checkOwner(tree: untpd.Tree)(using Context): Unit = {
      def ownerMatches(symOwner: Symbol, ctxOwner: Symbol): Boolean =
        symOwner == ctxOwner ||
        ctxOwner.isWeakOwner && ownerMatches(symOwner, ctxOwner.owner)
      assert(ownerMatches(tree.symbol.owner, ctx.owner),
        i"bad owner; ${tree.symbol} has owner ${tree.symbol.owner}, expected was ${ctx.owner}\n" +
        i"owner chain = ${tree.symbol.ownersIterator.toList}%, %, ctxOwners = ${ctx.outersIterator.map(_.owner).toList}%, %")
    }

    private def checkParents(tree: untpd.TypeDef)(using Context): Unit = {
      val TypeDef(_, impl: Template) = tree: @unchecked
      assert(ctx.owner.isClass)
      val sym = ctx.owner.asClass
      if !sym.isPrimitiveValueClass then
        TreeChecker.checkParents(sym, impl.parents)
    }

    override def typedTypeDef(tdef: untpd.TypeDef, sym: Symbol)(using Context): Tree = {
      assert(sym.info.isInstanceOf[ClassInfo | TypeBounds], i"wrong type, expect a template or type bounds for ${sym.fullName}, but found: ${sym.info}")
      if sym.isClass then checkParents(tdef)
      super.typedTypeDef(tdef, sym)
    }

    override def typedClassDef(cdef: untpd.TypeDef, cls: ClassSymbol)(using Context): Tree = {
      val TypeDef(_, impl @ Template(constr, _, _, _)) = cdef: @unchecked
      assert(cdef.symbol == cls)
      assert(impl.symbol.owner == cls)
      assert(constr.symbol.owner == cls, i"constr ${constr.symbol} in $cdef has wrong owner; should be $cls but is ${constr.symbol.owner}")
      assert(cls.primaryConstructor == constr.symbol, i"mismatch, primary constructor ${cls.primaryConstructor}, in tree = ${constr.symbol}")
      checkOwner(impl)
      checkOwner(impl.constr)

      checkParents(cdef)

      def isNonMagicalMember(x: Symbol) =
        !x.isValueClassConvertMethod &&
        !x.name.is(DocArtifactName) &&
        !(ctx.phase.id >= genBCodePhase.id && x.name == str.MODULE_INSTANCE_FIELD.toTermName)

      val decls   = cls.classInfo.decls.toList.toSet.filter(isNonMagicalMember)
      val defined = impl.body.map(_.symbol)

      val symbolsMissingDefs = (decls -- defined - constr.symbol).filterNot(isSymWithoutDef)

      assert(symbolsMissingDefs.isEmpty,
        i"""$cls tree does not define members: ${symbolsMissingDefs.toList}%, %
           |expected: ${decls.toList}%, %
           |defined: ${defined}%, %""")

      super.typedClassDef(cdef, cls)
    }

    override def typedValDef(vdef: untpd.ValDef, sym: Symbol)(using Context): Tree =
      val tpdTree = super.typedValDef(vdef, sym)
      vdef.tpt.tpe match
        case _: ValueType => () // ok
        case _: ExprType if sym.isOneOf(TermParamOrAccessor) => () // ok
        case _ => assert(false, i"wrong type, expected a value type for ${sym.fullName}, but found: ${sym.info}")
      tpdTree

    override def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(using Context): Tree =
      def defParamss = ddef.paramss.filter(!_.isEmpty).nestedMap(_.symbol)
      def layout(symss: List[List[Symbol]]): String =
        symss.map(syms => i"($syms%, %)").mkString
      assert(ctx.erasedTypes || sym.rawParamss == defParamss,
        i"""param mismatch for ${sym.showLocated}:
           |defined in tree  = ${layout(defParamss)}
           |stored in symbol = ${layout(sym.rawParamss)}""")
      withDefinedSyms(ddef.paramss.flatten) {
        if (!sym.isClassConstructor && !(sym.name eq nme.STATIC_CONSTRUCTOR))
          assert(isValidJVMMethodName(sym.name.encode), s"${sym.name.debugString} name is invalid on jvm")

        ddef.termParamss.foreach(_.foreach { vparam =>
          assert(vparam.symbol.is(Param),
            s"Parameter ${vparam.symbol} of ${sym.fullName} does not have flag `Param` set")
          assert(!vparam.symbol.isOneOf(AccessFlags),
            s"Parameter ${vparam.symbol} of ${sym.fullName} has invalid flag(s): ${(vparam.symbol.flags & AccessFlags).flagsString}")
        })

        val tpdTree = super.typedDefDef(ddef, sym)
        assert(isMethodType(sym.info), i"wrong type, expect a method type for ${sym.fullName}, but found: ${sym.info}")
        tpdTree
      }

    override def typedCase(tree: untpd.CaseDef, sel: Tree, selType: Type, pt: Type)(using Context): CaseDef =
      withPatSyms(tpd.patVars(tree.pat.asInstanceOf[tpd.Tree])) {
        super.typedCase(tree, sel, selType, pt)
      }

    override def typedClosure(tree: untpd.Closure, pt: Type)(using Context): Tree = {
      if (!ctx.phase.lambdaLifted) nestingBlock match {
        case block @ Block((meth : untpd.DefDef) :: Nil, closure: untpd.Closure) =>
          assert(meth.symbol == closure.meth.symbol, "closure.meth symbol not equal to method symbol. Block: " + block.show)

        case block: untpd.Block =>
          assert(false, "function literal are not properly formed as a block of DefDef and Closure. Found: " + tree.show + " Nesting block: " + block.show)

        case null =>
          assert(false, "function literal are not properly formed as a block of DefDef and Closure. Found: " + tree.show + " Nesting block: null")
      }
      super.typedClosure(tree, pt)
    }

    override def typedBlock(tree: untpd.Block, pt: Type)(using Context): Tree =
      withBlock(tree) { withDefinedSyms(tree.stats) { super.typedBlock(tree, pt) } }

    override def typedInlined(tree: untpd.Inlined, pt: Type)(using Context): Tree =
      withDefinedSyms(tree.bindings) { super.typedInlined(tree, pt) }

    /** Check that all defined symbols have legal owners.
     *  An owner is legal if it is either the same as the context's owner
     *  or there's an owner chain of valdefs starting at the context's owner and
     *  reaching up to the symbol's owner. The reason for this relaxed matching
     *  is that we should be able to pull out an expression as an initializer
     *  of a helper value without having to do a change owner traversal of the expression.
     */
    override def typedStats(trees: List[untpd.Tree], exprOwner: Symbol)(using Context): (List[Tree], Context) = {
      for (tree <- trees) tree match {
        case tree: untpd.DefTree => checkOwner(tree)
        case _: untpd.Thicket => assert(false, i"unexpanded thicket $tree in statement sequence $trees%\n%")
        case _ =>
      }
      super.typedStats(trees, exprOwner)
    }

    override def typedLabeled(tree: untpd.Labeled)(using Context): Labeled = {
      checkOwner(tree.bind)
      withDefinedSyms(tree.bind :: Nil) { super.typedLabeled(tree) }
    }

    override def typedReturn(tree: untpd.Return)(using Context): Return = {
      val tree1 = super.typedReturn(tree)
      val from = tree1.from
      val fromSym = from.symbol
      if (fromSym.is(Label))
        assertDefined(from)
      tree1
    }

    override def typedWhileDo(tree: untpd.WhileDo)(using Context): Tree = {
      assert((tree.cond ne EmptyTree) || ctx.phase.refChecked, i"invalid empty condition in while at $tree")
      super.typedWhileDo(tree)
    }

    override def typedPackageDef(tree: untpd.PackageDef)(using Context): Tree =
      if tree.symbol == defn.StdLibPatchesPackage then
        promote(tree) // don't check stdlib patches, since their symbols were highjacked by stdlib classes
      else
        super.typedPackageDef(tree)

    override def typedQuote(tree: untpd.Quote, pt: Type)(using Context): Tree =
      if ctx.phase <= stagingPhase.prev then
        assert(tree.tags.isEmpty, i"unexpected tags in Quote before staging phase: ${tree.tags}")
      else
        assert(!tree.body.isInstanceOf[untpd.Splice] || inInlineMethod, i"missed quote cancellation in $tree")
        assert(!tree.body.isInstanceOf[untpd.Hole] || inInlineMethod, i"missed quote cancellation in $tree")
        if StagingLevel.level != 0 then
          assert(tree.tags.isEmpty, i"unexpected tags in Quote at staging level ${StagingLevel.level}: ${tree.tags}")

      for tag <- tree.tags do
        assert(tag.isInstanceOf[RefTree], i"expected RefTree in Quote but was: $tag")

      val tree1 = super.typedQuote(tree, pt)
      for tag <- tree.tags do
        assert(tag.typeOpt.derivesFrom(defn.QuotedTypeClass), i"expected Quote tag to be of type `Type` but was: ${tag.tpe}")

      tree1 match
        case Quote(body, targ :: Nil) if body.isType =>
          assert(!(body.tpe =:= targ.tpe.select(tpnme.Underlying)), i"missed quote cancellation in $tree1")
        case _ =>

      tree1

    override def typedSplice(tree: untpd.Splice, pt: Type)(using Context): Tree =
      if stagingPhase <= ctx.phase then
        assert(!tree.expr.isInstanceOf[untpd.Quote] || inInlineMethod, i"missed quote cancellation in $tree")
      super.typedSplice(tree, pt)

    override def typedQuotePattern(tree: untpd.QuotePattern, pt: Type)(using Context): Tree =
      assert(ctx.mode.is(Mode.Pattern))
      for binding <- tree.bindings do
        assert(binding.isInstanceOf[untpd.Bind], i"expected Bind in QuotePattern bindings but was: $binding")
      super.typedQuotePattern(tree, pt)

    override def typedSplicePattern(tree: untpd.SplicePattern, pt: Type)(using Context): Tree =
      assert(ctx.mode.isQuotedPattern)
      def isAppliedIdent(rhs: untpd.Tree): Boolean = rhs match
        case _: Ident => true
        case rhs: GenericApply => isAppliedIdent(rhs.fun)
        case _ => false
      def isEtaExpandedIdent(arg: untpd.Tree): Boolean = arg match
        case closureDef(ddef) => isAppliedIdent(ddef.rhs) || isEtaExpandedIdent(ddef.rhs)
        case _ => false
      for arg <- tree.args do
        assert(arg.isInstanceOf[untpd.Ident] || isEtaExpandedIdent(arg), i"HOAS argument expected Ident or eta-expanded Ident but was: $arg")
      super.typedSplicePattern(tree, pt)

    override def typedHole(tree: untpd.Hole, pt: Type)(using Context): Tree = {
      val tree1 @ Hole(isTerm, idx, args, content) = super.typedHole(tree, pt): @unchecked

      assert(idx >= 0, i"hole should not have negative index: $tree")
      assert(isTerm || tree.args.isEmpty, i"type hole should not have arguments: $tree")

      // Check that we only add the captured type `T` instead of a more complex type like `List[T]`.
      // If we have `F[T]` with captured `F` and `T`, we should list `F` and `T` separately in the args.
      for arg <- args do
        assert(arg.isTerm || arg.tpe.isInstanceOf[TypeRef | TermRef | ThisType], "Unexpected type arg in Hole: " + arg.tpe)

      // Check result type of the hole
      if isTerm then assert(tree1.typeOpt <:< pt)
      else assert(tree1.typeOpt =:= pt)

      // Check that the types of the args conform to the types of the contents of the hole
      val argQuotedTypes = args.map { arg =>
        if arg.isTerm then
          val tpe = arg.typeOpt.widenTermRefExpr match
            case _: MethodicType =>
              // Special erasure for captured function references
              // See `SpliceTransformer.transformCapturedApplication`
              defn.AnyType
            case tpe => tpe
          defn.QuotedExprClass.typeRef.appliedTo(tpe)
        else defn.QuotedTypeClass.typeRef.appliedTo(arg.typeOpt)
      }
      val expectedResultType =
        if isTerm then defn.QuotedExprClass.typeRef.appliedTo(tree1.typeOpt)
        else defn.QuotedTypeClass.typeRef.appliedTo(tree1.typeOpt)
      val contextualResult =
        defn.FunctionNOf(List(defn.QuotesClass.typeRef), expectedResultType, isContextual = true)
      val expectedContentType =
        defn.FunctionNOf(argQuotedTypes, contextualResult)
      assert(content.typeOpt =:= expectedContentType, i"unexpected content of hole\nexpected: ${expectedContentType}\nwas: ${content.typeOpt}")

      tree1
    }

    override def ensureNoLocalRefs(tree: Tree, pt: Type, localSyms: => List[Symbol])(using Context): Tree =
      tree

    override def adapt(tree: Tree, pt: Type, locked: TypeVars)(using Context): Tree = {
      def isPrimaryConstructorReturn =
        ctx.owner.isPrimaryConstructor && pt.isRef(ctx.owner.owner) && tree.tpe.isRef(defn.UnitClass)
      if ctx.mode.isExpr
        && !tree.isEmpty
        && !isPrimaryConstructorReturn
        && !pt.isInstanceOf[FunOrPolyProto]
      then
        assert(tree.tpe <:< pt, {
          val mismatch = TypeMismatch(tree.tpe, pt, Some(tree))
          i"""|Type Mismatch:
              |${mismatch.message}
              |tree = $tree ${tree.className}""".stripMargin
        })
      tree
    }

    override def simplify(tree: Tree, pt: Type, locked: TypeVars)(using Context): tree.type = tree
  }

  /** Tree checker that can be applied to a local tree. */
  class LocalChecker(phasesToCheck: Seq[Phase]) extends Checker(phasesToCheck: Seq[Phase]):
    override def assertDefined(tree: untpd.Tree)(using Context): Unit =
      // Only check definitions nested in the local tree
      if nowDefinedSyms.contains(tree.symbol.maybeOwner) then
        super.assertDefined(tree)

  def checkMacroGeneratedTree(original: tpd.Tree, expansion: tpd.Tree)(using Context): Unit =
    if ctx.settings.XcheckMacros.value then
      val checkingCtx = ctx
        .fresh
        .setReporter(new ThrowingReporter(ctx.reporter))
      val phases = ctx.base.allPhases.toList
      val treeChecker = new LocalChecker(previousPhases(phases))

      try treeChecker.typed(expansion)(using checkingCtx)
      catch
        case err: java.lang.AssertionError =>
          val stack =
            if !ctx.settings.Ydebug.value then "\nstacktrace available when compiling with `-Ydebug`"
            else if err.getStackTrace == null then "  no stacktrace"
            else err.getStackTrace.nn.mkString("  ", "  \n", "")

          report.error(
            em"""Malformed tree was found while expanding macro with -Xcheck-macros.
               |The tree does not conform to the compiler's tree invariants.
               |
               |Macro was:
               |${scala.quoted.runtime.impl.QuotesImpl.showDecompiledTree(original)}
               |
               |The macro returned:
               |${scala.quoted.runtime.impl.QuotesImpl.showDecompiledTree(expansion)}
               |
               |Error:
               |${err.getMessage}
               |$stack
               |""",
            original
          )

  private[TreeChecker] def previousPhases(phases: List[Phase])(using Context): List[Phase] = phases match {
    case (phase: MegaPhase) :: phases1 =>
      val subPhases = phase.miniPhases
      val previousSubPhases = previousPhases(subPhases.toList)
      if (previousSubPhases.length == subPhases.length) previousSubPhases ::: previousPhases(phases1)
      else previousSubPhases
    case phase :: phases1 if phase ne ctx.phase =>
      phase :: previousPhases(phases1)
    case _ =>
      Nil
  }
}
