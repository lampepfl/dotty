package dotty.tools.dotc
package core

import Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._

object TypedTrees {

  object tpd {

    type Modifiers = Trees.Modifiers[Type]
    type Tree = Trees.Tree[Type]
    type TypTree = Trees.TypTree[Type]
    type TermTree = Trees.TermTree[Type]
    type PatternTree = Trees.PatternTree[Type]
    type SymTree = Trees.SymTree[Type]
    type ProxyTree = Trees.ProxyTree[Type]
    type NameTree = Trees.NameTree[Type]
    type RefTree = Trees.RefTree[Type]
    type DefTree = Trees.DefTree[Type]

    type TreeCopier = Trees.TreeCopier[Type]
    type TreeAccumulator[T] = Trees.TreeAccumulator[T, Type]
    type TreeTransformer[C] = Trees.TreeTransformer[Type, C]

    type Ident = Trees.Ident[Type]
    type Select = Trees.Select[Type]
    type This = Trees.This[Type]
    type Super = Trees.Super[Type]
    type Apply = Trees.Apply[Type]
    type TypeApply = Trees.TypeApply[Type]
    type Literal = Trees.Literal[Type]
    type New = Trees.New[Type]
    type Pair = Trees.Pair[Type]
    type Typed = Trees.Typed[Type]
    type NamedArg = Trees.NamedArg[Type]
    type Assign = Trees.Assign[Type]
    type Block = Trees.Block[Type]
    type If = Trees.If[Type]
    type Match = Trees.Match[Type]
    type CaseDef = Trees.CaseDef[Type]
    type Return = Trees.Return[Type]
    type Try = Trees.Try[Type]
    type Throw = Trees.Throw[Type]
    type SeqLiteral = Trees.SeqLiteral[Type]
    type TypeTree = Trees.TypeTree[Type]
    type SingletonTypeTree = Trees.SingletonTypeTree[Type]
    type SelectFromTypeTree = Trees.SelectFromTypeTree[Type]
    type AndTypeTree = Trees.AndTypeTree[Type]
    type OrTypeTree = Trees.OrTypeTree[Type]
    type RefineTypeTree = Trees.RefineTypeTree[Type]
    type AppliedTypeTree = Trees.AppliedTypeTree[Type]
    type TypeBoundsTree = Trees.TypeBoundsTree[Type]
    type Bind = Trees.Bind[Type]
    type Alternative = Trees.Alternative[Type]
    type UnApply = Trees.UnApply[Type]
    type ValDef = Trees.ValDef[Type]
    type DefDef = Trees.DefDef[Type]
    type TypeDef = Trees.TypeDef[Type]
    type Template = Trees.Template[Type]
    type ClassDef = Trees.ClassDef[Type]
    type Import = Trees.Import[Type]
    type PackageDef = Trees.PackageDef[Type]
    type Annotated = Trees.Annotated[Type]
    type EmptyTree = Trees.EmptyTree[Type]
    type Shared = Trees.Shared[Type]

    private implicit def pos(implicit ctx: Context): Position = ctx.position

    def defPos(sym: Symbol)(implicit ctx: Context) = ctx.position union sym.coord.toPosition

    def Modifiers(sym: Symbol)(implicit ctx: Context): Modifiers = Trees.Modifiers[Type](
      sym.flags & ModifierFlags,
      if (sym.privateWithin.exists) sym.privateWithin.asType.name else tpnme.EMPTY,
      sym.annotations map (_.tree))

    def Ident(tp: NamedType)(implicit ctx: Context): Ident =
      Trees.Ident(tp.name).withType(tp).checked

    def Select(pre: Tree, tp: NamedType)(implicit ctx: Context): Select =
      Trees.Select(pre, tp.name).withType(tp).checked

    def This(cls: ClassSymbol)(implicit ctx: Context): This =
      Trees.This(cls.name).withType(cls.thisType).checked

    def Super(qual: Tree, mixin: Symbol = NoSymbol)(implicit ctx: Context): Super = {
      val cls = qual.tpe.typeSymbol
      val (owntype, mix) =
        if (mixin.exists) (mixin.typeConstructor, mixin.asType.name)
        else (ctx.glb(cls.info.parents), tpnme.EMPTY)
      Trees.Super(qual, mix).withType(SuperType(qual.tpe, owntype)).checked
    }

    def Apply(fn: Tree, args: List[Tree])(implicit ctx: Context): Apply = {
      val fntpe @ MethodType(pnames, ptypes) = fn.tpe
      assert(sameLength(ptypes, args))
      Trees.Apply(fn, args).withType(fntpe.instantiate(args map (_.tpe))).checked
    }

    def TypeApply(fn: Tree, args: List[Tree])(implicit ctx: Context): TypeApply = {
      val fntpe @ PolyType(pnames) = fn.tpe
      assert(sameLength(pnames, args))
      Trees.TypeApply(fn, args).withType(fntpe.instantiate(args map (_.tpe))).checked
    }

    def Literal(const: Constant)(implicit ctx: Context): Literal =
      Trees.Literal(const).withType(const.tpe).checked

    def New(tp: Type)(implicit ctx: Context): New =
      Trees.New(TypeTree(tp)).withType(tp).checked

    def Pair(left: Tree, right: Tree)(implicit ctx: Context): Pair =
      Trees.Pair(left, right).withType(defn.PairType.appliedTo(left.tpe, right.tpe)).checked

    def Typed(expr: Tree, tpt: Tree)(implicit ctx: Context): Typed =
      Trees.Typed(expr, tpt).withType(tpt.tpe).checked

    def NamedArg(name: TermName, arg: Tree)(implicit ctx: Context) =
      Trees.NamedArg(name, arg).withType(arg.tpe).checked

    def Assign(lhs: Tree, rhs: Tree)(implicit ctx: Context): Assign =
      Trees.Assign(lhs, rhs).withType(defn.UnitType).checked

    def Block(stats: List[Tree], expr: Tree)(implicit ctx: Context): Block = {
      lazy val locals = localSyms(stats)
      val blk = Trees.Block(stats, expr)
      def widen(tp: Type): Type = tp match {
        case tp: TermRef if locals contains tp.symbol =>
          widen(tp.info)
        case _ => tp
      }
      blk.withType(widen(expr.tpe))
    }

    def If(cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: Context): If =
      Trees.If(cond, thenp, elsep).withType(thenp.tpe | elsep.tpe).checked

    def Match(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match =
      Trees.Match(selector, cases).withType(ctx.lub(cases map (_.body.tpe))).checked

    def CaseDef(pat: Tree, guard: Tree, body: Tree)(implicit ctx: Context): CaseDef =
      Trees.CaseDef(pat, guard, body).withType(body.tpe).checked

    def Return(expr: Tree, from: Ident)(implicit ctx: Context): Return =
      Trees.Return(expr, from).withType(defn.NothingType).checked

    def Throw(expr: Tree)(implicit ctx: Context): Throw =
      Trees.Throw(expr).withType(defn.NothingType).checked

    def SeqLiteral(elemtpt: Tree, elems: List[Tree])(implicit ctx: Context): SeqLiteral =
      Trees.SeqLiteral(elemtpt, elems).withType(defn.RepeatedParamType.appliedTo(elemtpt.tpe)).checked

    def TypeTree(tp: Type, original: Tree = EmptyTree)(implicit ctx: Context): TypeTree =
      Trees.TypeTree(original).withType(tp).checked

    def SingletonTypeTree(ref: Tree)(implicit ctx: Context): SingletonTypeTree =
      Trees.SingletonTypeTree(ref).withType(ref.tpe).checked

    def SelectFromTypeTree(qualifier: Tree, tp: TypeRef)(implicit ctx: Context): SelectFromTypeTree =
      Trees.SelectFromTypeTree(qualifier, tp.name).withType(tp).checked

    def AndTypeTree(left: Tree, right: Tree)(implicit ctx: Context): AndTypeTree =
      Trees.AndTypeTree(left, right).withType(left.tpe & right.tpe).checked

    def OrTypeTree(left: Tree, right: Tree)(implicit ctx: Context): OrTypeTree =
      Trees.OrTypeTree(left, right).withType(left.tpe | right.tpe).checked

    def RefineTypeTree(tpt: Tree, refinements: List[DefTree])(implicit ctx: Context): RefineTypeTree = {
      def refineType(tp: Type, refinement: Symbol): Type =
        RefinedType(tp, refinement.name, refinement.info)
      Trees.RefineTypeTree(tpt, refinements)
        .withType((tpt.tpe /: (refinements map (_.symbol)))(refineType)).checked
    }

    def refineType(tp: Type, refinement: Symbol)(implicit ctx: Context): Type =
      RefinedType(tp, refinement.name, refinement.info)

    def AppliedTypeTree(tpt: Tree, args: List[Tree])(implicit ctx: Context): AppliedTypeTree =
      Trees.AppliedTypeTree(tpt, args).withType(tpt.tpe.appliedTo(args map (_.tpe))).checked

    def TypeBoundsTree(lo: Tree, hi: Tree)(implicit ctx: Context): TypeBoundsTree =
      Trees.TypeBoundsTree(lo, hi).withType(TypeBounds(lo.tpe, hi.tpe)).checked

    def Bind(sym: TermSymbol, body: Tree)(implicit ctx: Context): Bind =
      Trees.Bind(sym.name, body)(defPos(sym)).withType(refType(sym)).checked

    def Alternative(trees: List[Tree])(implicit ctx: Context): Alternative =
      Trees.Alternative(trees).withType(ctx.lub(trees map (_.tpe))).checked

    def UnApply(fun: Tree, args: List[Tree])(implicit ctx: Context): UnApply =
      Trees.UnApply(fun, args).withType(fun.tpe.widen match {
        case MethodType(_, paramType :: Nil) => paramType
      }).checked

    def refType(sym: Symbol)(implicit ctx: Context) = NamedType(sym.owner.thisType, sym)

    def ValDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): ValDef =
      Trees.ValDef(Modifiers(sym), sym.name, TypeTree(sym.info), rhs)(defPos(sym))
        .withType(refType(sym)).checked

    def DefDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef = {

      val (tparams, mtp) = sym.info match {
        case tp: PolyType =>
          val tparams = ctx.newTypeParams(sym, tp.paramNames, EmptyFlags, tp.instantiateBounds)
          (tparams, tp.instantiate(tparams map (_.typeConstructor)))
        case tp => (Nil, tp)
      }

      def valueParamss(tp: Type): (List[List[TermSymbol]], Type) = tp match {
        case tp @ MethodType(paramNames, paramTypes) =>
          def valueParam(name: TermName, info: Type): TermSymbol =
            ctx.newSymbol(sym, name, TermParam, info)
          val params = (paramNames, paramTypes).zipped.map(valueParam)
          val (paramss, rtp) = valueParamss(tp.instantiate(params map (_.typeConstructor)))
          (params :: paramss, rtp)
        case tp => (Nil, tp)
      }
      val (vparamss, rtp) = valueParamss(mtp)

      Trees.DefDef(
        Modifiers(sym), sym.name, tparams map TypeDef,
        vparamss map (_ map (ValDef(_))), TypeTree(rtp), rhs)(defPos(sym))
        .withType(refType(sym)).checked
    }

    def TypeDef(sym: TypeSymbol)(implicit ctx: Context): TypeDef =
      Trees.TypeDef(Modifiers(sym), sym.name, TypeTree(sym.info))(defPos(sym))
        .withType(refType(sym)).checked

    def ClassDef(cls: ClassSymbol, typeParams: List[TypeSymbol], body: List[Tree])(implicit ctx: Context): ClassDef = {
      val parents = cls.info.parents map (TypeTree(_))
      val selfType =
        if (cls.selfType eq cls.typeConstructor) EmptyValDef
        else ValDef(ctx.newSelfSym(cls))
      def isOwnTypeParamAccessor(stat: Tree) =
        stat.symbol.owner == cls && (stat.symbol is TypeParam)
      val (tparamAccessors, rest) = body partition isOwnTypeParamAccessor
      val tparams =
        (typeParams map TypeDef) ++
          (tparamAccessors collect {
            case td: TypeDef if !(typeParams contains td.symbol) => td
          })
      val findLocalDummy = new FindLocalDummyAccumulator(cls)
      val localDummy = ((NoSymbol: Symbol) /: body)(findLocalDummy)
        .orElse(ctx.newLocalDummy(cls))
      val impl = Trees.Template(parents, selfType, rest)
        .withType(refType(localDummy)).checked
      Trees.ClassDef(Modifiers(cls), cls.name, tparams, impl)(defPos(cls))
        .withType(refType(cls)).checked
    }

    def Import(expr: Tree, selectors: List[Trees.UntypedTree])(implicit ctx: Context): Import =
      Trees.Import(expr, selectors).withType(refType(ctx.newImportSymbol(Shared(expr)))).checked

    def PackageDef(pid: RefTree, stats: List[Tree])(implicit ctx: Context): PackageDef =
      Trees.PackageDef(pid, stats).withType(refType(pid.symbol)).checked

    def Annotated(annot: Tree, arg: Tree)(implicit ctx: Context): Annotated =
      Trees.Annotated(annot, arg).withType(AnnotatedType(List(Annotation(annot)), arg.tpe)).checked

    val EmptyTree: Tree = Trees.EmptyTree[Type]

    val EmptyValDef: ValDef = Trees.EmptyValDef[Type]

    def Shared(tree: Tree): Shared =
      Trees.Shared(tree).withType(tree.tpe)

    // ------ Creating typed equivalents of trees that exist only in untyped form -------

    def New(tp: Type, args: List[Tree])(implicit ctx: Context): Apply =
      Apply(
        Select(
          New(tp),
          TermRef(tp.normalizedPrefix, tp.typeSymbol.primaryConstructor.asTerm)),
        args)

    /** An object def
     *
     *     object obs extends parents { decls }
     *
     *  gets expanded to
     *
     *     <module> lazy val obj = {
     *       class obj$ extends parents { this: obj.type => decls }
     *       new obj$
     *     }
     *
     *  What's interesting here is that the block is well typed
     *  (because class obj$ is hoistable), but the type of the `obj` val is
     *  not expressible. What needs to happen in general when
     *  inferring the type of a val from its RHS, is: if the type contains
     *  a class that has the val itself as owner, then that class
     *  is remapped to have the val's owner as owner. Remapping could be
     *  done by cloning the class with the new owner and substituting
     *  everywhere in the tree. We know that remapping is safe
     *  because the only way a local class can appear in the RHS of a val is
     *  by being hoisted outside of a block, and the necessary checks are
     *  done at this point already.
     *
     *  On the other hand, for method result type inference, if the type of
     *  the RHS of a method contains a class owned by the method, this would be
     *  an error.
     */
    def ModuleDef(sym: TermSymbol, body: List[Tree])(implicit ctx: Context): ValDef = {
      val modcls = sym.moduleClass.asClass
      val clsdef = ClassDef(modcls, Nil, body)
      val rhs = Block(List(clsdef), New(modcls.typeConstructor))
      ValDef(sym, rhs)
    }

    /** A function def
     *
     *    vparams => expr
     *
     *  gets expanded to
     *
     *    { def $anonfun(vparams) = expr; $anonfun: pt }
     *
     *  where pt is the target type of the expression (FunctionN) unless
     *  otherwise specified.
     */
    def Function(meth: TermSymbol, body: Tree, target: Type = NoType)(implicit ctx: Context): Block = {
      val funtpe =
        if (target.exists) target
        else meth.info match {
          case mt @ MethodType(_, formals) =>
            assert(!mt.isDependent)
            defn.FunctionType(formals, mt.resultType)
        }
      Block(
        DefDef(meth, body) :: Nil,
        Typed(Ident(TermRef(NoPrefix, meth)), TypeTree(funtpe)))
    }

    private class FindLocalDummyAccumulator(cls: ClassSymbol)(implicit ctx: Context) extends TreeAccumulator[Symbol] {
      def apply(sym: Symbol, tree: Tree) =
        if (sym.exists) sym
        else if (tree.isDef) {
          val owner = tree.symbol.owner
          if (owner.isLocalDummy && owner.owner == cls) owner
          else if (owner == cls) foldOver(sym, tree)
          else sym
        } else foldOver(sym, tree)
    }

    implicit class addChecked[T <: Tree](val tree: T) extends AnyVal {
      def checked(implicit ctx: Context): T = {
        if (ctx.settings.YcheckTypedTrees.value) checkType(tree)
        tree
      }
    }
  }

  import Trees._

  def check(p: Boolean)(implicit ctx: Context): Unit = assert(p)

  def checkType(tree: tpd.Tree)(implicit ctx: Context): Unit = tree match {
    case Ident(name) =>
    case Select(qualifier, name) =>
      check(qualifier.isValue)
      val denot = qualifier.tpe.member(name)
      check(denot.hasAltWith(_.symbol == tree.symbol))
    case This(cls) =>
    case Super(qual, mixin) =>
      check(qual.isValue)
      val cls = qual.tpe.typeSymbol
      check(cls.isClass)
      check(mixin == NoSymbol || (cls.asClass.parents map (_.typeSymbol) contains mixin))
    case Apply(fn, args) =>
      def checkArg(arg: tpd.Tree, name: Name, formal: Type): Unit = {
        arg match {
          case NamedArg(argName, _) =>
            check(argName == name)
          case SeqLiteral(_, _) =>
            check(defn.RepeatedParamClasses contains formal.typeSymbol)
          case _ =>
            check(arg.isValue)
        }
        check(arg.tpe <:< formal)
      }
      fn.tpe.widen match {
        case MethodType(paramNames, paramTypes) =>
          (args, paramNames, paramTypes).zipped foreach checkArg
        case _ =>
          check(false)
      }
    case TypeApply(fn, args) =>
      def checkArg(arg: tpd.Tree, bounds: TypeBounds): Unit = {
        check(arg.isType)
        check(bounds contains arg.tpe)
      }
      fn.tpe.widen match {
        case pt: PolyType =>
          (args, pt.instantiateBounds(args map (_.tpe))).zipped foreach checkArg
        case _ =>
          check(false)
      }
    case Literal(const: Constant) =>
    case New(tpt) =>
      check(tpt.isType)
      val cls = tpt.tpe.typeSymbol
      check(cls.isClass)
      check(!(cls is AbstractOrTrait))
    case Pair(left, right) =>
      check(left.isValue)
      check(right.isValue)
    case Typed(expr, tpt) =>
      check(tpt.isType)
      expr.tpe.widen match {
        case tp: MethodType =>
          val cls = tpt.tpe.typeSymbol
          check(cls.isClass)
          check((cls is Trait) ||
                cls.primaryConstructor.info.paramTypess.flatten.isEmpty)
          val absMembers = tpt.tpe.abstractTermMembers
          check(absMembers.size == 1)
          check(tp <:< absMembers.head.info)
        case _ =>
          check(expr.isValueOrPattern)
      }
    case NamedArg(name, arg) =>
    case Assign(lhs, rhs) =>
      check(lhs.isValue); check(rhs.isValue)
      lhs.tpe match {
        case ltpe: TermRef =>
          check(ltpe.symbol is Mutable)
        case _ =>
          check(false)
      }
      check(rhs.tpe <:< lhs.tpe.widen)
    case Block(stats, expr) =>
      var hoisted: Set[Symbol] = Set()
      lazy val locals = localSyms(stats)
      check(expr.isValue)
      def isNonLocal(sym: Symbol): Boolean =
        !(locals contains sym) || isHoistableClass(sym)
      def isHoistableClass(sym: Symbol) =
        sym.isClass && {
          (hoisted contains sym) || {
            hoisted += sym
            noLeaksInClass(sym.asClass)
          }
        }
      def noLeaksIn(tp: Type): Boolean = tp forall {
        case tp: NamedType => isNonLocal(tp.symbol)
        case _ => true
      }
      def noLeaksInClass(sym: ClassSymbol): Boolean =
        (sym.parents forall noLeaksIn) &&
        (sym.decls.toList forall (t => noLeaksIn(t.info)))
      check(noLeaksIn(tree.tpe))
    case If(cond, thenp, elsep) =>
      check(cond.isValue); check(thenp.isValue); check(elsep.isValue)
      check(cond.tpe <:< defn.BooleanType)
    case Match(selector, cases) =>
      check(selector.isValue)
      // are any checks that relate selector and patterns desirable?
    case CaseDef(pat, guard, body) =>
      check(pat.isValueOrPattern); check(guard.isValue); check(body.isValue)
      check(guard.tpe <:< defn.BooleanType)
    case Return(expr, from) =>
      check(expr.isValue); check(from.isTerm)
      check(from.tpe.termSymbol.isSourceMethod)
    case Throw(expr) =>
      check(expr.isValue)
      check(expr.tpe <:< defn.ThrowableType)
    case SeqLiteral(elemtpt, elems) =>
      check(elemtpt.isType);
      for (elem <- elems) {
        check(elem.isValue)
        check(elem.tpe <:< elemtpt.tpe)
      }
    case TypeTree(original) =>
      if (!original.isEmpty) check(original.tpe == tree.tpe)
    case SingletonTypeTree(ref) =>
      check(ref.isValue)
      check(ref.symbol.isStable)
    case SelectFromTypeTree(qualifier, name) =>
      check(qualifier.isType)
      val denot = qualifier.tpe.member(name)
      check(denot.exists)
      check(denot.symbol == tree.symbol)
    case AndTypeTree(left, right) =>
      check(left.isType); check(right.isType)
    case OrTypeTree(left, right) =>
      check(left.isType); check(right.isType)
    case RefineTypeTree(tpt, refinements) =>
      check(tpt.isType)
      def checkRefinements(forbidden: Set[Symbol], rs: List[tpd.DefTree]): Unit = rs match {
        case r :: rs1 =>
          val rsym = r.symbol
          check(rsym.isTerm || rsym.isAbstractType)
          if (rsym.isAbstractType) check(tpt.tpe.member(rsym.name).exists)
          check(rsym.info forall {
            case nt: NamedType => !(forbidden contains nt.symbol)
            case _ => true
          })
          checkRefinements(forbidden - rsym, rs1)
        case nil =>
      }
      checkRefinements(localSyms(refinements).toSet, refinements)
    case AppliedTypeTree(tpt, args) =>
      check(tpt.isType)
      for (arg <- args) check(arg.isType)
      check(sameLength(tpt.tpe.typeParams, args))
    case TypeBoundsTree(lo, hi) =>
      check(lo.isType); check(hi.isType)
      check(lo.tpe <:< hi.tpe)
    case Bind(sym, body) =>
      // missing because it cannot be done easily bottom-up:
      // check that Bind (and Alternative, and UnApply) only occur in patterns
      check(body.isValueOrPattern)
      check(!(tree.symbol is Method))
      body match {
        case Ident(nme.WILDCARD) =>
        case _ => check(body.tpe <:< tree.symbol.info)
      }
    case Alternative(alts) =>
      for (alt <- alts) check(alt.isValueOrPattern)
    case UnApply(fun, args) =>
      check(fun.isTerm)
      val funtpe = fun.tpe.widen match {
        case mt: MethodType => mt
        case _ => check(false); ErrorType
      }
      for (arg <- args) check(arg.isValueOrPattern)
      fun.symbol.name match { // check arg arity
        case nme.unapplySeq =>
          // args need to be wrapped in (...: _*)
          check(args.length == 1)
          check(args.head.tpe.typeSymbol == defn.RepeatedParamClass)
        case nme.unapply =>
          val rtp = funtpe.resultType
          if (rtp == defn.BooleanType)
            check(args.isEmpty)
          else {
            val (tycon, resArgs) = rtp.splitArgs
            check(tycon == defn.OptionType)
            check(resArgs.length == 1)
            val normArgs = {
              val (tp1, args1) = resArgs.head.splitArgs
              if (defn.TupleClasses contains tp1.typeSymbol) args1
              else args.head :: Nil
            }
            check(sameLength(normArgs, args))
          }
      }
    case ValDef(mods, name, tpt, rhs) =>
      check(!(tree.symbol is Method))
      if (!rhs.isEmpty) {
        check(rhs.isValue)
        check(rhs.tpe <:< tpt.tpe)
      }
    case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
      check(tree.symbol is Method)
      if (!rhs.isEmpty) {
        check(rhs.isValue)
        check(rhs.tpe <:< tpt.tpe)
      }
    case TypeDef(mods, name, tpt) =>
      check(tpt.tpe.isInstanceOf[TypeBounds])
    case Template(parents, selfType, body) =>
    case ClassDef(mods, name, tparams, impl) =>
    case Import(expr, selectors) =>
      check(expr.isValue)
      check(expr.tpe.termSymbol.isStable)
    case PackageDef(pid, stats) =>
      check(pid.isValue)
      check(pid.symbol.isPackage)
    case Annotated(annot, arg) =>
      check(annot.symbol.isClassConstructor)
      check(annot.symbol.owner.isSubClass(defn.AnnotationClass))
      check(arg.isType || arg.isValue)
    case Shared(shared) =>
      check(shared.isType || shared.isTerm)
    case Try(block, catches, finalizer) =>
      check(block.isTerm)
      check(finalizer.isTerm)
      def checkType(t: Tree[Type]) = check(t.tpe <:< tree.tpe)
      checkType(block)
      catches foreach checkType
    case EmptyTree() =>
  }

  implicit class TreeInfo(val tree: tpd.Tree) extends AnyVal {
    def isValue(implicit ctx: Context): Boolean =
      tree.isTerm && tree.tpe.widen.isValueType
    def isValueOrPattern(implicit ctx: Context) =
      tree.isValue || tree.isPattern
  }

  // ensure that constructors are fully applied?
  // ensure that normal methods are fully applied?

  def localSyms(stats: List[tpd.Tree])(implicit ctx: Context) =
    for (stat <- stats if (stat.isDef)) yield stat.symbol
}

