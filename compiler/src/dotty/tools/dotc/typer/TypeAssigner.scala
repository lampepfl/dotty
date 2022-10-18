package dotty.tools
package dotc
package typer

import core._
import ast._
import Contexts._, ContextOps._, Constants._, Types._, Symbols._, Names._, Flags._, Decorators._
import ErrorReporting._, Annotations._, Denotations._, SymDenotations._, StdNames._
import util.SrcPos
import NameOps._
import collection.mutable
import reporting._
import Checking.checkNoPrivateLeaks
import cc.CaptureSet

trait TypeAssigner {
  import tpd.*
  import TypeAssigner.*

  /** The qualifying class of a this or super with prefix `qual` (which might be empty).
   *  @param packageOk   The qualifier may refer to a package.
   */
  def qualifyingClass(tree: untpd.Tree, qual: Name, packageOK: Boolean)(using Context): Symbol = {
    def qualifies(sym: Symbol) =
      sym.isClass && (
          qual.isEmpty ||
          sym.name == qual ||
          sym.is(Module) && sym.name.stripModuleClassSuffix == qual)
    ctx.outersIterator.map(_.owner).find(qualifies) match {
      case Some(c) if packageOK || !c.is(Package) =>
        c
      case _ =>
        report.error(
          if (qual.isEmpty) tree.show + " can be used only in a class, object, or template"
          else qual.show + " is not an enclosing class", tree.srcPos)
        NoSymbol
    }
  }

  def avoidingType(expr: Tree, bindings: List[Tree])(using Context): Type =
    TypeOps.avoid(expr.tpe, localSyms(bindings).filterConserve(_.isTerm))

  def avoidPrivateLeaks(sym: Symbol)(using Context): Type =
    if sym.owner.isClass && !sym.isOneOf(JavaOrPrivateOrSynthetic)
    then checkNoPrivateLeaks(sym)
    else sym.info

  private def toRepeated(tree: Tree, from: ClassSymbol)(using Context): Tree =
    Typed(tree, TypeTree(tree.tpe.widen.translateToRepeated(from)))

  def seqToRepeated(tree: Tree)(using Context): Tree = toRepeated(tree, defn.SeqClass)

  def arrayToRepeated(tree: Tree)(using Context): Tree = toRepeated(tree, defn.ArrayClass)

  /** A denotation exists really if it exists and does not point to a stale symbol. */
  final def reallyExists(denot: Denotation)(using Context): Boolean = try
    denot match {
      case denot: SymDenotation =>
        denot.exists && !denot.isAbsent()
      case denot: SingleDenotation =>
        val sym = denot.symbol
        (sym eq NoSymbol) || reallyExists(sym.denot)
      case _ =>
        true
    }
  catch {
    case ex: StaleSymbol => false
  }

  /** If `tpe` is a named type, return the type with those alternatives as denotations
   *  which are accessible (or NoType, if no alternatives are accessible).
   *
   *  Also performs the following normalizations on the type `tpe`.
   *  (1) if the owner of the denotation is a package object, it is assured
   *      that the package object shows up as the prefix.
   *  (2) in Java compilation units, `Object` is replaced by `defn.FromJavaObjectType`
   */
  def accessibleType(tpe: Type, superAccess: Boolean)(using Context): Type =
    tpe match
      case tpe: NamedType =>
        val pre = tpe.prefix
        val name = tpe.name
        def postProcess(d: Denotation) =
          if ctx.isJava && tpe.isAnyRef then defn.FromJavaObjectType
          else TypeOps.makePackageObjPrefixExplicit(tpe withDenot d)
        val d = tpe.denot.accessibleFrom(pre, superAccess)
        if d.exists then postProcess(d)
        else
          // it could be that we found an inaccessible private member, but there is
          // an inherited non-private member with the same name and signature.
          val d2 = pre.nonPrivateMember(name).accessibleFrom(pre, superAccess)
          if reallyExists(d2) then postProcess(d2)
          else NoType
      case tpe => tpe

  /** Try to make `tpe` accessible, emit error if not possible */
  def ensureAccessible(tpe: Type, superAccess: Boolean, pos: SrcPos)(using Context): Type =
    val tpe1 = accessibleType(tpe, superAccess)
    if tpe1.exists then tpe1
    else tpe match
      case tpe: NamedType => inaccessibleErrorType(tpe, superAccess, pos)
      case NoType => tpe

  /** Return a potentially skolemized version of `qualTpe` to be used
   *  as a prefix when selecting `name`.
   *
   *  @see QualSkolemType, TypeOps#asSeenFrom
   */
  def maybeSkolemizePrefix(qualType: Type, name: Name)(using Context): Type =
    if (name.isTermName && !TypeOps.isLegalPrefix(qualType))
      QualSkolemType(qualType)
    else
      qualType

  /** The type of the selection `tree`, where `qual1` is the typed qualifier part. */
  def selectionType(tree: untpd.RefTree, qual1: Tree)(using Context): Type =
    val qualType0 = qual1.tpe.widenIfUnstable
    val qualType =
      if !qualType0.hasSimpleKind && tree.name != nme.CONSTRUCTOR then
        // constructors are selected on typeconstructor, type arguments are passed afterwards
        errorType(em"$qualType0 takes type parameters", qual1.srcPos)
      else if !qualType0.isInstanceOf[TermType] && !qualType0.isError then
        errorType(em"$qualType0 is illegal as a selection prefix", qual1.srcPos)
      else
        qualType0

    def arrayElemType = qual1.tpe.widen match
      case JavaArrayType(elemtp) => elemtp
      case qualType =>
        report.error("Expected Array but was " + qualType.show, tree.srcPos)
        defn.NothingType

    val name = tree.name
    val p = nme.primitive
    name match
      case p.arrayApply  => MethodType(defn.IntType :: Nil, arrayElemType)
      case p.arrayUpdate => MethodType(defn.IntType :: arrayElemType :: Nil, defn.UnitType)
      case p.arrayLength => MethodType(Nil, defn.IntType)
      // Note that we do not need to handle calls to Array[T]#clone() specially:
      // The JLS section 10.7 says "The return type of the clone method of an array type
      // T[] is T[]", but the actual return type at the bytecode level is Object which
      // is casted to T[] by javac. Since the return type of Array[T]#clone() is Array[T],
      // this is exactly what Erasure will do.
      case _ =>
        val pre = maybeSkolemizePrefix(qualType, name)
        val mbr =
          if ctx.isJava then
            ctx.javaFindMember(name, pre)
          else
            qualType.findMember(name, pre)

        if reallyExists(mbr) then qualType.select(name, mbr)
        else if qualType.isErroneous || name.toTermName == nme.ERROR then UnspecifiedErrorType
        else NoType
  end selectionType

  def importSuggestionAddendum(pt: Type)(using Context): String = ""

  def notAMemberErrorType(tree: untpd.Select, qual: Tree)(using Context): ErrorType =
    val qualType = qual.tpe.widenIfUnstable
    def kind = if tree.isType then "type" else "value"
    val foundWithoutNull = qualType match
      case OrNull(qualType1) if qualType1 <:< defn.ObjectType =>
        val name = tree.name
        val pre = maybeSkolemizePrefix(qualType1, name)
        reallyExists(qualType1.findMember(name, pre))
      case _ => false
    def addendum = err.selectErrorAddendum(tree, qual, qualType, importSuggestionAddendum, foundWithoutNull)
    val msg: Message =
      if tree.name == nme.CONSTRUCTOR then ex"$qualType does not have a constructor"
      else NotAMember(qualType, tree.name, kind, addendum)
    errorType(msg, tree.srcPos)

  def inaccessibleErrorType(tpe: NamedType, superAccess: Boolean, pos: SrcPos)(using Context): Type =
    val pre = tpe.prefix
    val name = tpe.name
    val alts = tpe.denot.alternatives.map(_.symbol).filter(_.exists)
    val whatCanNot = alts match
      case Nil =>
        em"$name cannot"
      case sym :: Nil =>
        em"${if (sym.owner == pre.typeSymbol) sym.show else sym.showLocated} cannot"
      case _ =>
        em"none of the overloaded alternatives named $name can"
    val where = if (ctx.owner.exists) s" from ${ctx.owner.enclosingClass}" else ""
    val whyNot = new StringBuffer
    alts.foreach(_.isAccessibleFrom(pre, superAccess, whyNot))
    if tpe.isError then tpe
    else errorType(ex"$whatCanNot be accessed as a member of $pre$where.$whyNot", pos)

  def processAppliedType(tree: untpd.Tree, tp: Type)(using Context): Type = tp match
    case AppliedType(tycon, args) =>
      val constr = tycon.typeSymbol
      if constr == defn.andType then AndType(args(0), args(1))
      else if constr == defn.orType then OrType(args(0), args(1), soft = false)
      else tp
    case _ => tp

  /** Instantiate `tl` with named type arguments `args`.
   *  Issue an error if there are duplicate or unrecognized argument names.
   *  @return  the instantiated return type, type abstracted over all parameters
   *           that do not correspond to a named argument
   */
  def instantiateNamed(tl: TypeLambda, args: List[Tree])(using Context): Type =
    val reordered = reorderArgs(tl.paramNames, args, placeholderTypeParam)
    instantiateWithPlaceholders(tl, reordered)

  /** Instantiate `tycon` with arguments `args`, where some arguments are placeholders `_`.
   *  @return  the instantiated return type, type abstracted over all placeholders
   */
  def instantiateWithPlaceholders(tycon: Type, args: List[Tree])(using Context): Type =
    val tl = tycon match
      case tycon: TypeLambda => tycon
      case _ =>
        val tparams = tycon.typeParams
        (tparams: @unchecked) match
          case (tparam: Symbol) :: _ =>
            val tparamSyms = tparams.asInstanceOf[List[Symbol]]
            HKTypeLambda.apply(tparamSyms.map(_.name.asTypeName))(
              (tl: HKTypeLambda) => tparamSyms.map(_.info.subst(tparamSyms, tl.paramRefs).bounds),
              (tl: HKTypeLambda) => tycon.appliedTo(tl.paramRefs))
          case LambdaParam(tl, _) :: _ => tl

    val gaps = args.zipWithIndex.collect {
      case (arg, n) if isPlaceHolderTypeParam(arg) => n
    }
    // mappedArgTypes contains the type of each named argument in its reordered position.
    // For missing types it contains contiguous parameter references to `tl` starting from 0.
    // These make sense only once tl is mapped to the new lambda which contains
    // a parameter for each missing argument.
    val mappedArgTypes =
      val newIndex = Iterator.from(0)
      args.map { arg =>
        if isPlaceHolderTypeParam(arg) then tl.paramRefs(newIndex.next) else arg.tpe
      }
    val resType = tl.resultType.substParams(tl, mappedArgTypes)
    if gaps.isEmpty then resType
    else tl.derivedLambdaType(
        gaps.map(tl.paramNames),
        gaps.map(n => tl.paramInfos(n).substParams(tl, mappedArgTypes).bounds),
        resType)
  end instantiateWithPlaceholders

  /** Type assignment method. Each method takes as parameters
   *   - an untpd.Tree to which it assigns a type,
   *   - typed child trees it needs to access to cpmpute that type,
   *   - any further information it needs to access to compute that type.
   */
  def assignType(tree: untpd.Ident, tp: Type)(using Context): Ident =
    tree.withType(tp)

  def assignType(tree: untpd.Select, tp: Type)(using Context): Select =
    ConstFold.Select(tree.withType(tp))

  def assignType(tree: untpd.Select, qual: Tree)(using Context): Select =
    val rawType = selectionType(tree, qual)
    val checkedType = ensureAccessible(rawType, qual.isInstanceOf[Super], tree.srcPos)
    val ownType = checkedType.orElse(notAMemberErrorType(tree, qual))
    assignType(tree, ownType)

  /** Normalize type T appearing in a new T by following eta expansions to
   *  avoid higher-kinded types.
   */
  def typeOfNew(tpt: Tree)(using Context): Type = tpt.tpe.dealias match {
    case TypeApplications.EtaExpansion(tycon) => tycon
    case t => tpt.tpe
  }

  def assignType(tree: untpd.New, tpt: Tree)(using Context): New =
    tree.withType(typeOfNew(tpt))

  def assignType(tree: untpd.Literal)(using Context): Literal =
    tree.withType {
      val value = tree.const
      value.tag match {
        case UnitTag => defn.UnitType
        case NullTag => defn.NullType
        case _ => if (ctx.erasedTypes) value.tpe else ConstantType(value)
      }
    }

  def assignType(tree: untpd.This)(using Context): This = {
    val cls = qualifyingClass(tree, tree.qual.name, packageOK = false)
    tree.withType(
        if (cls.isClass) cls.thisType
        else errorType("not a legal qualifying class for this", tree.srcPos))
  }

  def superType(qualType: Type, mix: untpd.Ident, mixinClass: Symbol, pos: SrcPos)(using Context) =
    qualType match
      case err: ErrorType => err
      case qtype @ ThisType(_) =>
        val cls = qtype.cls
        def findMixinSuper(site: Type): Type = site.parents filter (_.typeSymbol.name == mix.name) match {
          case p :: Nil =>
            p.typeConstructor
          case Nil =>
            errorType(SuperQualMustBeParent(mix, cls), pos)
          case p :: q :: _ =>
            errorType("ambiguous parent class qualifier", pos)
        }
        val owntype =
          if (mixinClass.exists) mixinClass.typeRef
          else if (!mix.isEmpty) findMixinSuper(cls.info)
          else if (ctx.erasedTypes) cls.info.firstParent.typeConstructor
          else {
            val ps = cls.classInfo.parents
            if (ps.isEmpty) defn.AnyType else ps.reduceLeft((x: Type, y: Type) => x & y)
          }
        SuperType(cls.thisType, owntype)

  def assignType(tree: untpd.Super, qual: Tree, mixinClass: Symbol = NoSymbol)(using Context): Super =
    untpd.cpy.Super(tree)(qual, tree.mix)
      .withType(superType(qual.tpe, tree.mix, mixinClass, tree.srcPos))

  /** Substitute argument type `argType` for parameter `pref` in type `tp`,
   *  skolemizing the argument type if it is not stable and `pref` occurs in `tp`.
   */
  def safeSubstParam(tp: Type, pref: ParamRef, argType: Type)(using Context): Type = {
    val tp1 = tp.substParam(pref, argType)
    if ((tp1 eq tp) || argType.isStable) tp1
    else tp.substParam(pref, SkolemType(argType.widen))
  }

  /** Substitute types of all arguments `args` for corresponding `params` in `tp`.
   *  The number of parameters `params` may exceed the number of arguments.
   *  In this case, only the common prefix is substituted.
   */
  def safeSubstParams(tp: Type, params: List[ParamRef], argTypes: List[Type])(using Context): Type = argTypes match {
    case argType :: argTypes1 =>
      val tp1 = safeSubstParam(tp, params.head, argType)
      safeSubstParams(tp1, params.tail, argTypes1)
    case Nil =>
      tp
  }

  def safeSubstMethodParams(mt: MethodType, argTypes: List[Type])(using Context): Type =
    if mt.isResultDependent then safeSubstParams(mt.resultType, mt.paramRefs, argTypes)
    else if mt.isCaptureDependent then mt.resultType.substParams(mt, argTypes)
    else mt.resultType

  def assignType(tree: untpd.Apply, fn: Tree, args: List[Tree])(using Context): Apply = {
    val ownType = fn.tpe.widen match {
      case fntpe: MethodType =>
        if (fntpe.paramInfos.hasSameLengthAs(args) || ctx.phase.prev.relaxedTyping)
          safeSubstMethodParams(fntpe, args.tpes)
        else
          errorType(i"wrong number of arguments at ${ctx.phase.prev} for $fntpe: ${fn.tpe}, expected: ${fntpe.paramInfos.length}, found: ${args.length}", tree.srcPos)
      case t =>
        if (ctx.settings.Ydebug.value) new FatalError("").printStackTrace()
        errorType(err.takesNoParamsStr(fn, ""), tree.srcPos)
    }
    ConstFold.Apply(tree.withType(ownType))
  }

  def assignType(tree: untpd.TypeApply, fn: Tree, args: List[Tree])(using Context): TypeApply = {
    def fail = tree.withType(errorType(err.takesNoParamsStr(fn, "type "), tree.srcPos))
    ConstFold(fn.tpe.widen match {
      case tl: TypeLambda =>
        // Make sure arguments don't contain the type `pt` itself.
        // make a copy of the argument if that's the case.
        // This is done to compensate for the fact that normally every
        // reference to a polytype would have to be a fresh copy of that type,
        // but we want to avoid that because it would increase compilation cost.
        // See pos/i6682a.scala for a test case where the defensive copying matters.
        object ensureFresh extends TypeMap, CaptureSet.IdempotentCaptRefMap:
          var freshened = false
          def apply(tp: Type) = mapOver(
            if tp eq tl then
              freshened = true
              tl.newLikeThis(tl.paramNames, tl.paramInfos, tl.resType)
            else
              tp)
        var argTypes = args.tpes
        val freshArgTypes = argTypes.map(ensureFresh)
        if ensureFresh.freshened then
          assignType(tree, fn, args.lazyZip(freshArgTypes).map(_.withType(_)))
        else
          tree.withType {
            if !argTypes.hasSameLengthAs(tl.paramNames) then
              wrongNumberOfTypeArgs(fn.tpe, tl.typeParams, args, tree.srcPos)
            else if hasPlaceholderTypeParams(args) then
              instantiateWithPlaceholders(tl, args)
            else
              tl.instantiate(argTypes)
          }
      case err: ErrorType =>
        tree.withType(err)
      case ref: TermRef if ref.isOverloaded =>
        val disambiguated = ref.denot.suchThat(_.info.isInstanceOf[PolyType])
        if (disambiguated.exists) {
          val fn1 = fn.withType(ref.withDenot(disambiguated))
          val tree1 = untpd.cpy.TypeApply(tree)(fn1, args)
          assignType(tree1, fn1, args)
        }
        else fail
      case _ =>
        //println(i"bad type: $fn: ${fn.symbol} / ${fn.symbol.isType} / ${fn.symbol.info}") // DEBUG
        fail
    })
  }

  def assignType(tree: untpd.Typed, tpt: Tree)(using Context): Typed =
    tree.withType(tpt.tpe)

  def assignType(tree: untpd.NamedArg, arg: Tree)(using Context): NamedArg =
    tree.withType(arg.tpe)

  def assignType(tree: untpd.Assign)(using Context): Assign =
    tree.withType(defn.UnitType)

  def assignType(tree: untpd.Block, stats: List[Tree], expr: Tree)(using Context): Block =
    tree.withType(avoidingType(expr, stats))

  def assignType(tree: untpd.Inlined, bindings: List[Tree], expansion: Tree)(using Context): Inlined =
    tree.withType(avoidingType(expansion, bindings))

  def assignType(tree: untpd.If, thenp: Tree, elsep: Tree)(using Context): If =
    tree.withType(thenp.tpe | elsep.tpe)

  def assignType(tree: untpd.Closure, meth: Tree, target: Tree)(using Context): Closure =
    tree.withType(
      if (target.isEmpty) meth.tpe.widen.toFunctionType(isJava = meth.symbol.is(JavaDefined), tree.env.length)
      else target.tpe)

  def assignType(tree: untpd.CaseDef, pat: Tree, body: Tree)(using Context): CaseDef = {
    val ownType =
      if (body.isType) {
        val getParams = new TreeAccumulator[mutable.ListBuffer[TypeSymbol]] {
          def apply(ps: mutable.ListBuffer[TypeSymbol], t: Tree)(using Context) = t match {
            case t: Bind if t.symbol.isType => foldOver(ps += t.symbol.asType, t)
            case _ => foldOver(ps, t)
          }
        }
        val params1 = getParams(new mutable.ListBuffer[TypeSymbol](), pat).toList
        val params2 = pat.tpe match
          case AppliedType(tycon, args) =>
            val tparams = tycon.typeParamSymbols
            params1.mapconserve { param =>
              val info1 = param.info
              val info2 = info1.subst(tparams, args)
              if info2 eq info1 then param else param.copy(info = info2).asType
            }
          case _ => params1
        val matchCase1 = defn.MatchCase(pat.tpe, body.tpe)
        val matchCase2 = if params2 eq params1 then matchCase1 else matchCase1.substSym(params1, params2)
        HKTypeLambda.fromParams(params2, matchCase2)
      }
      else body.tpe
    tree.withType(ownType)
  }

  def assignType(tree: untpd.Match, scrutinee: Tree, cases: List[CaseDef])(using Context): Match =
    tree.withType(TypeComparer.lub(cases.tpes))

  def assignType(tree: untpd.Labeled)(using Context): Labeled =
    tree.withType(tree.bind.symbol.info)

  def assignType(tree: untpd.Return)(using Context): Return =
    tree.withType(defn.NothingType)

  def assignType(tree: untpd.WhileDo)(using Context): WhileDo =
    tree.withType(if (tree.cond eq EmptyTree) defn.NothingType else defn.UnitType)

  def assignType(tree: untpd.Try, expr: Tree, cases: List[CaseDef])(using Context): Try =
    if (cases.isEmpty) tree.withType(expr.tpe)
    else tree.withType(TypeComparer.lub(expr.tpe :: cases.tpes))

  def assignType(tree: untpd.SeqLiteral, elems: List[Tree], elemtpt: Tree)(using Context): SeqLiteral =
    tree.withType(seqLitType(tree, elemtpt.tpe))

  def assignType(tree: untpd.SingletonTypeTree, ref: Tree)(using Context): SingletonTypeTree =
    tree.withType(ref.tpe)

  /** Assign type of RefinedType.
   *  Refinements are typed as if they were members of refinement class `refineCls`.
   */
  def assignType(tree: untpd.RefinedTypeTree, parent: Tree, refinements: List[Tree], refineCls: ClassSymbol)(using Context): RefinedTypeTree = {
    def addRefinement(parent: Type, refinement: Tree): Type = {
      val rsym = refinement.symbol
      val rinfo = if (rsym.is(Accessor)) rsym.info.resultType else rsym.info
      if (rinfo.isError) rinfo
      else if (!rinfo.exists) parent // can happen after failure in self type definition
      else RefinedType(parent, rsym.name, rinfo)
    }
    val refined = refinements.foldLeft(parent.tpe)(addRefinement)
    tree.withType(RecType.closeOver(rt => refined.substThis(refineCls, rt.recThis)))
  }

  def assignType(tree: untpd.AppliedTypeTree, tycon: Tree, args: List[Tree])(using Context): AppliedTypeTree =
    val tparams = tycon.tpe.typeParams
    val ownType =
      if !tparams.hasSameLengthAs(args) then
        wrongNumberOfTypeArgs(tycon.tpe, tparams, args, tree.srcPos)
      else if hasPlaceholderTypeParams(args) then
        instantiateWithPlaceholders(tycon.tpe, args)
      else
        processAppliedType(tree, tycon.tpe.appliedTo(args.tpes))
    tree.withType(ownType)

  def assignType(tree: untpd.LambdaTypeTree, tparamDefs: List[TypeDef], body: Tree)(using Context): LambdaTypeTree =
    val validParams = tparamDefs.filterConserve { tdef =>
      val ok = tdef.symbol.isType
      if !ok then assert(ctx.reporter.errorsReported)
      ok
    }
    tree.withType(HKTypeLambda.fromParams(validParams.map(_.symbol.asType), body.tpe))

  def assignType(tree: untpd.MatchTypeTree, bound: Tree, scrutinee: Tree, cases: List[CaseDef])(using Context): MatchTypeTree = {
    val boundType = if (bound.isEmpty) defn.AnyType else bound.tpe
    tree.withType(MatchType(boundType, scrutinee.tpe, cases.tpes))
  }

  def assignType(tree: untpd.ByNameTypeTree, result: Tree)(using Context): ByNameTypeTree =
    tree.withType(ExprType(result.tpe))

  def assignType(tree: untpd.TypeBoundsTree, lo: Tree, hi: Tree, alias: Tree)(using Context): TypeBoundsTree =
    tree.withType(
      if !alias.isEmpty then alias.tpe
      else if lo eq hi then
        if lo.tpe.isMatch then MatchAlias(lo.tpe)
        else TypeAlias(lo.tpe)
      else TypeBounds(lo.tpe, hi.tpe))

  def assignType(tree: untpd.Bind, sym: Symbol)(using Context): Bind =
    tree.withType(NamedType(NoPrefix, sym))

  def assignType(tree: untpd.Alternative, trees: List[Tree])(using Context): Alternative =
    tree.withType(TypeComparer.lub(trees.tpes))

  def assignType(tree: untpd.UnApply, proto: Type)(using Context): UnApply =
    tree.withType(proto)

  def assignType(tree: untpd.ValDef, sym: Symbol)(using Context): ValDef =
    tree.withType(if (sym.exists) assertExists(sym.termRef) else NoType)

  def assignType(tree: untpd.DefDef, sym: Symbol)(using Context): DefDef =
    tree.withType(sym.termRef)

  def assignType(tree: untpd.TypeDef, sym: Symbol)(using Context): TypeDef =
    tree.withType(sym.typeRef)

  def assertExists(tp: Type): Type = { assert(tp != NoType); tp }

  def assignType(tree: untpd.Import, sym: Symbol)(using Context): Import =
    tree.withType(sym.termRef)

  def assignType(tree: untpd.Export)(using Context): Export =
    tree.withType(defn.UnitType)

  def assignType(tree: untpd.Annotated, arg: Tree, annot: Tree)(using Context): Annotated = {
    assert(tree.isType) // annotating a term is done via a Typed node, can't use Annotate directly
    tree.withType(AnnotatedType(arg.tpe, Annotation(annot)))
  }

  def assignType(tree: untpd.PackageDef, pid: Tree)(using Context): PackageDef =
    tree.withType(pid.symbol.termRef)

  def assignType(tree: untpd.Hole, tpt: Tree)(using Context): Hole =
    tree.withType(tpt.tpe)

}

object TypeAssigner extends TypeAssigner:
  def seqLitType(tree: untpd.SeqLiteral, elemType: Type)(using Context) = tree match
    case tree: untpd.JavaSeqLiteral => defn.ArrayOf(elemType)
    case _ => if ctx.erasedTypes then defn.SeqType else defn.SeqType.appliedTo(elemType)


