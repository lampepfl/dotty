package dotty.tools
package dotc
package typer

import core.*
import util.Spans.Span
import Contexts.*
import Types.*, Flags.*, Symbols.*, Names.*, StdNames.*, Constants.*
import TypeErasure.{erasure, hasStableErasure}
import Decorators.*
import ProtoTypes.*
import Inferencing.{fullyDefinedType, isFullyDefined}
import ast.untpd
import transform.SyntheticMembers.*
import util.Property
import ast.Trees.genericEmptyTree
import annotation.{tailrec, constructorOnly}
import ast.tpd.*
import Synthesizer.*
import sbt.ExtractDependencies.*
import xsbti.api.DependencyContext.*

/** Synthesize terms for special classes */
class Synthesizer(typer: Typer)(using @constructorOnly c: Context):

  /** Handlers to synthesize implicits for special types */
  type SpecialHandler = (Type, Span) => Context ?=> TreeWithErrors
  private type SpecialHandlers = List[(ClassSymbol, SpecialHandler)]

  val synthesizedClassTag: SpecialHandler = (formal, span) =>
    def instArg(tp: Type): Type = tp.stripTypeVar match
      // Special case to avoid instantiating `Int & S` to `Int & Nothing` in
      // i16328.scala. The intersection comes from an earlier instantiation
      // to an upper bound.
      // The dual situation with unions is harder to trigger because lower
      // bounds are usually widened during instantiation.
      case tp: AndOrType if tp.tp1 =:= tp.tp2 =>
        instArg(tp.tp1)
      case _ =>
        if isFullyDefined(tp, ForceDegree.all) then tp
        else NoType // this happens in tests/neg/i15372.scala

    val tag = formal.argInfos match
      case arg :: Nil =>
        instArg(arg) match
          case defn.ArrayOf(elemTp) =>
            val etag = typer.inferImplicitArg(defn.ClassTagClass.typeRef.appliedTo(elemTp), span)
            if etag.tpe.isError then EmptyTree else etag.select(nme.wrap)
          case tp if hasStableErasure(tp) && !tp.isBottomTypeAfterErasure =>
            val sym = tp.typeSymbol
            val classTagModul = ref(defn.ClassTagModule)
            if defn.SpecialClassTagClasses.contains(sym) then
              classTagModul.select(sym.name.toTermName).withSpan(span)
            else
              val ctype = escapeJavaArray(erasure(tp))
              if ctype.exists then
                classTagModul.select(nme.apply)
                  .appliedToType(tp)
                  .appliedTo(clsOf(ctype))
                  .withSpan(span)
              else EmptyTree
          case _ => EmptyTree
      case _ => EmptyTree
    (tag, Nil)
  end synthesizedClassTag

  val synthesizedTypeTest: SpecialHandler =
    (formal, span) => formal.argInfos match {
      case arg1 :: arg2 :: Nil if !defn.isBottomClass(arg2.typeSymbol) =>
        val srcPos = ctx.source.atSpan(span)
        val tp1 = fullyDefinedType(arg1, "TypeTest argument", srcPos)
        val tp2 = fullyDefinedType(arg2, "TypeTest argument", srcPos).normalized
        val sym2 = tp2.typeSymbol
        if tp1 <:< tp2 then
          // optimization when we know the typetest will always succeed
          withNoErrors(ref(defn.TypeTestModule_identity).appliedToType(tp2).withSpan(span))
        else if sym2 == defn.AnyValClass || sym2 == defn.AnyRefAlias || sym2 == defn.ObjectClass then
          EmptyTreeNoError
        else
          // Generate SAM: (s: <tp1>) => if s.isInstanceOf[<tp2>] then Some(s.asInstanceOf[s.type & <tp2>]) else None
          def body(args: List[Tree]): Tree = {
            val arg :: Nil = args: @unchecked
            val t = arg.tpe & tp2
            If(
              arg.isInstance(tp2),
              ref(defn.SomeClass.companionModule.termRef).select(nme.apply)
                .appliedToType(t)
                .appliedTo(arg.select(nme.asInstanceOf_).appliedToType(t)),
              ref(defn.NoneModule))
          }
          val tpe = MethodType(List(nme.s))(_ => List(tp1), mth => defn.OptionClass.typeRef.appliedTo(mth.newParamRef(0) & tp2))
          val meth = newAnonFun(ctx.owner, tpe, coord = span)
          val typeTestType = defn.TypeTestClass.typeRef.appliedTo(List(tp1, tp2))
          withNoErrors(Closure(meth, tss => body(tss.head).changeOwner(ctx.owner, meth), targetType = typeTestType).withSpan(span))
      case _ =>
        EmptyTreeNoError
    }
  end synthesizedTypeTest

  val synthesizedTupleFunction: SpecialHandler = (formal, span) =>
    formal match
      case AppliedType(_, funArgs @ fun :: tupled :: Nil) =>
        def functionTypeEqual(baseFun: Type, actualArgs: List[Type],
            actualRet: Type, expected: Type) =
          expected =:= defn.FunctionNOf(actualArgs, actualRet,
            defn.isContextFunctionType(baseFun))
        val arity: Int =
          if defn.isFunctionNType(fun) then
            // TupledFunction[(...) => R, ?]
            fun.functionArgInfos match
              case funArgs :+ funRet
              if functionTypeEqual(fun, defn.tupleType(funArgs) :: Nil, funRet, tupled) =>
                // TupledFunction[(...funArgs...) => funRet, ?]
                funArgs.size
              case _ => -1
          else if defn.isFunctionNType(tupled) then
            // TupledFunction[?, (...) => R]
            tupled.functionArgInfos match
              case tupledArgs :: funRet :: Nil =>
                tupledArgs.tupleElementTypes match
                  case Some(funArgs) if functionTypeEqual(tupled, funArgs, funRet, fun) =>
                    // TupledFunction[?, ((...funArgs...)) => funRet]
                    funArgs.size
                  case _ => -1
              case _ => -1
          else
            // TupledFunction[?, ?]
            -1
        if arity == -1 then
          EmptyTreeNoError
        else if arity <= Definitions.MaxImplementedFunctionArity then
          withNoErrors(ref(defn.RuntimeTupleFunctionsModule)
            .select(s"tupledFunction$arity".toTermName)
            .appliedToTypes(funArgs))
        else
          withNoErrors(ref(defn.RuntimeTupleFunctionsModule)
            .select("tupledFunctionXXL".toTermName)
            .appliedToTypes(funArgs))
      case _ =>
        EmptyTreeNoError
  end synthesizedTupleFunction

  /** If `formal` is of the form CanEqual[T, U], try to synthesize an
    *  `CanEqual.canEqualAny[T, U]` as solution.
    */
  val synthesizedCanEqual: SpecialHandler = (formal, span) =>

    /** Is there an `CanEqual[T, T]` instance, assuming -strictEquality? */
    def hasEq(tp: Type)(using Context): Boolean =
      val inst = typer.inferImplicitArg(defn.CanEqualClass.typeRef.appliedTo(tp, tp), span)
      !inst.isEmpty && !inst.tpe.isError

    /** Can we assume the canEqualAny instance for `tp1`, `tp2`?
      *  This is the case if assumedCanEqual(tp1, tp2), or
      *  one of `tp1`, `tp2` has a reflexive `CanEqual` instance.
      */
    def validEqAnyArgs(tp1: Type, tp2: Type)(using Context) =
      typer.assumedCanEqual(tp1, tp2)
       || withMode(Mode.StrictEquality) {
            !hasEq(tp1) && !hasEq(tp2)
          }

    /** Is an `CanEqual[cls1, cls2]` instance assumed for predefined classes `cls1`, cls2`? */
    def canComparePredefinedClasses(cls1: ClassSymbol, cls2: ClassSymbol): Boolean =

      def cmpWithBoxed(cls1: ClassSymbol, cls2: ClassSymbol) =
        cls2 == defn.NothingClass
        || cls2 == defn.boxedClass(cls1)
        || cls1.isNumericValueClass && cls2.derivesFrom(defn.BoxedNumberClass)

      if cls1.isPrimitiveValueClass then
        if cls2.isPrimitiveValueClass then
          cls1 == cls2 || cls1.isNumericValueClass && cls2.isNumericValueClass
        else
          cmpWithBoxed(cls1, cls2)
      else if cls2.isPrimitiveValueClass then
        cmpWithBoxed(cls2, cls1)
      else if cls1 == defn.NullClass then
        cls1 == cls2 || cls2.derivesFrom(defn.ObjectClass)
      else if cls2 == defn.NullClass then
        cls1.derivesFrom(defn.ObjectClass)
      else
        cls1 == defn.NothingClass || cls2 == defn.NothingClass
    end canComparePredefinedClasses

    /** Some simulated `CanEqual` instances for predefined types. It's more efficient
      *  to do this directly instead of setting up a lot of `CanEqual` instances to
      *  interpret.
      */
    def canComparePredefined(tp1: Type, tp2: Type) =
      tp1.classSymbols.exists(cls1 =>
        tp2.classSymbols.exists(cls2 =>
          canComparePredefinedClasses(cls1, cls2)))

    formal.argTypes match
      case args @ (arg1 :: arg2 :: Nil) =>
        List(arg1, arg2).foreach(fullyDefinedType(_, "eq argument", ctx.source.atSpan(span)))
        if canComparePredefined(arg1, arg2)
            || !Implicits.strictEquality && explore(validEqAnyArgs(arg1, arg2))
        then withNoErrors(ref(defn.CanEqual_canEqualAny).appliedToTypes(args).withSpan(span))
        else EmptyTreeNoError
      case _ => EmptyTreeNoError
  end synthesizedCanEqual

  /** Creates a tree that will produce a ValueOf instance for the requested type.
   * An EmptyTreeNoError is returned if materialization fails.
   */
  val synthesizedValueOf: SpecialHandler = (formal, span) =>

    def success(t: Tree) =
      New(defn.ValueOfClass.typeRef.appliedTo(t.tpe), t :: Nil).withSpan(span)
    formal.argInfos match
      case arg :: Nil =>
        fullyDefinedType(arg, "ValueOf argument", ctx.source.atSpan(span)).normalized.dealias match
          case ConstantType(c: Constant) =>
            withNoErrors(success(Literal(c)))
          case tp: TypeRef if tp.isRef(defn.UnitClass) =>
            withNoErrors(success(Literal(Constant(()))))
          case n: TermRef =>
            withNoErrors(success(ref(n)))
          case tp =>
            EmptyTreeNoError
      case _ =>
        EmptyTreeNoError
  end synthesizedValueOf

  /** Create an anonymous class `new Object { type MirroredMonoType = ... }`
   *  and mark it with given attachment so that it is made into a mirror at PostTyper.
   */
  private def anonymousMirror(monoType: Type, impl: MirrorImpl, span: Span)(using Context) =
    if ctx.isAfterTyper then ctx.compilationUnit.needsMirrorSupport = true
    val monoTypeDef = untpd.TypeDef(tpnme.MirroredMonoType, untpd.TypeTree(monoType))
    val newImpl = untpd.Template(
      constr = untpd.emptyConstructor,
      parents = untpd.TypeTree(defn.ObjectType) :: untpd.TypeTree(defn.JavaSerializableClass.typeRef) :: Nil,
      derived = Nil,
      self = EmptyValDef,
      body = monoTypeDef :: Nil
    ).withAttachment(ExtendsSumOrProductMirror, impl)
    typer.typed(untpd.New(newImpl).withSpan(span))

  /** The mirror type
   *
   *     <parent> {
   *       MirroredMonoType = <monoType>
   *       MirroredType = <mirroredType>
   *       MirroredLabel = <label>
   *     }
   */
  private def mirrorCore(parentClass: ClassSymbol, monoType: Type, mirroredType: Type, label: Name)(using Context) =
    parentClass.typeRef
      .refinedWith(tpnme.MirroredMonoType, TypeAlias(monoType))
      .refinedWith(tpnme.MirroredType, TypeAlias(mirroredType))
      .refinedWith(tpnme.MirroredLabel, TypeAlias(ConstantType(Constant(label.toString))))

  /** A path referencing the companion of class type `clsType` */
  private def companionPath(clsType: Type, span: Span)(using Context) =
    val ref = pathFor(clsType.mirrorCompanionRef)
    assert(ref.symbol.is(Module) && (clsType.classSymbol.is(ModuleClass) || (ref.symbol.companionClass == clsType.classSymbol)))
    ref.withSpan(span)

  private def checkFormal(formal: Type)(using Context): Boolean =
    @tailrec
    def loop(tp: Type): Boolean = tp match
      case RefinedType(parent, _, _: TypeBounds) => loop(parent)
      case RefinedType(_, _, _) => false
      case _ => true
    loop(formal)

  private def checkRefinement(formal: Type, name: TypeName, expected: Type, span: Span)(using Context): Unit =
    val actual = formal.lookupRefined(name)
    if actual.exists && !(expected =:= actual)
    then report.error(
      em"$name mismatch, expected: $expected, found: $actual.", ctx.source.atSpan(span))

  extension (formal: Type)
    /** `tp := op; tp <:< formal; formal & tp` */
    private def constrained_&(op: Context ?=> Type)(using Context): Type =
      val tp = op
      tp <:< formal
      formal & tp

  private def mkMirroredMonoType(mirroredType: HKTypeLambda)(using Context): Type =
    val monoMap = new TypeMap:
      def apply(t: Type) = t match
        case TypeParamRef(lambda, n) if lambda eq mirroredType => mirroredType.paramInfos(n)
        case t => mapOver(t)
    monoMap(mirroredType.resultType)

  private[Synthesizer] enum MirrorSource:
    case ClassSymbol(pre: Type, cls: Symbol)
    case Singleton(src: Symbol, tref: TermRef)
    case GenericTuple(tps: List[Type])

    /** Tests that both sides are tuples of the same arity */
    infix def sameTuple(that: MirrorSource)(using Context): Boolean =
      def arity(msrc: MirrorSource): Int = msrc match
        case GenericTuple(tps) => tps.size
        case ClassSymbol(_, cls) if defn.isTupleClass(cls) => cls.typeParams.size // tested in tests/pos/i13859.scala
        case _ => -1
      def equivalent(n: Int, m: Int) =
        n == m && n > 0
      equivalent(arity(this), arity(that))

    /** A comparison that chooses the most specific MirrorSource, this is guided by what is necessary for
     * `Mirror.Product.fromProduct`. i.e. its result type should be compatible with the erasure of `mirroredType`.
     */
    def isSub(that: MirrorSource)(using Context): Boolean =
      (this, that) match
        case (Singleton(src, _), ClassSymbol(_, cls)) => src.info.classSymbol.isSubClass(cls)
        case (ClassSymbol(_, cls1), ClassSymbol(_, cls2)) => cls1.isSubClass(cls2)
        case (Singleton(src1, _), Singleton(src2, _)) => src1 eq src2
        case (_: ClassSymbol, _: Singleton) => false
        case _ => this sameTuple that

    def show(using Context): String = this match
      case ClassSymbol(_, cls) => i"$cls"
      case Singleton(src, _) => i"$src"
      case GenericTuple(tps) =>
        val arity = tps.size
        if arity <= Definitions.MaxTupleArity then s"class Tuple$arity"
        else s"trait Tuple { def size: $arity }"

  private[Synthesizer] object MirrorSource:

    /** Reduces a mirroredType to either its most specific ClassSymbol,
     *  or a TermRef to a singleton value. These are
     *  the base elements required to generate a mirror.
     */
    def reduce(mirroredType: Type)(using Context): Either[String, MirrorSource] = mirroredType match
      case tp: TypeRef =>
        val sym = tp.symbol
        if sym.isClass then // direct ref to a class, not an alias
          if sym.isAllOf(Case | Module) then
            // correct widened module ref. Tested in tests/run/i15234.scala
            val singleton = sym.sourceModule
            Right(MirrorSource.Singleton(singleton, TermRef(tp.prefix, singleton)))
          else
            Right(MirrorSource.ClassSymbol(tp.prefix, sym))
        else
          reduce(tp.superType)
      case tp: TermRef =>
        /** Dealias a path type to extract the underlying definition when it is either
         *  a singleton enum case or a case object.
         */
        def reduceToEnumOrCaseObject(tp: Type)(using Context): Symbol = tp match
          case tp: TermRef =>
            val sym = tp.termSymbol
            if sym.isEnumCase || (sym.isClass && sym.isAllOf(Case | Module)) then sym
            else if sym.exists && !tp.isOverloaded then reduceToEnumOrCaseObject(tp.underlying.widenExpr)
            else NoSymbol
          case _ => NoSymbol

        // capture enum singleton types. Tested in tests/run/i15234.scala
        val singleton = reduceToEnumOrCaseObject(tp)
        if singleton.exists then
          Right(MirrorSource.Singleton(singleton, tp))
        else
          reduce(tp.underlying)
      case tp: HKTypeLambda if tp.resultType.isInstanceOf[HKTypeLambda] =>
        Left(i"its subpart `$tp` is not a supported kind (either `*` or `* -> *`)")
      case tp: TypeProxy =>
        tp match
          case tp @ AppliedType(tref: TypeRef, _) if tref.symbol == defn.PairClass =>
            tp.tupleElementTypes match
              case Some(types) =>
                // avoid type aliases for tuples
                Right(MirrorSource.GenericTuple(types))
              case _ => reduce(tp.underlying)
          case tp: MatchType => reduce(tp.normalized)
          case _ => reduce(tp.superType)
      case tp @ AndType(l, r) =>
        for
          lsrc <- reduce(l)
          rsrc <- reduce(r)
          res <- locally {
            if lsrc.isSub(rsrc) then Right(lsrc)
            else if rsrc.isSub(lsrc) then Right(rsrc)
            else Left(i"its subpart `$tp` is an intersection of unrelated definitions ${lsrc.show} and ${rsrc.show}.")
          }
        yield
          res
      case tp: OrType =>
        Left(i"its subpart `$tp` is a top-level union type.")
      case tp =>
        Left(i"its subpart `$tp` is an unsupported type.")

  end MirrorSource

  private def productMirror(mirroredType: Type, formal: Type, span: Span)(using Context): TreeWithErrors =

    /** `new scala.runtime.TupleMirror(arity)`
     *  using TupleMirror avoids generating anonymous classes for tuple mirrors.
     */
    def newTupleMirror(arity: Int): Tree =
      New(defn.RuntimeTupleMirrorTypeRef, Literal(Constant(arity)) :: Nil)

    def makeProductMirror(pre: Type, cls: Symbol, tps: Option[List[Type]]): TreeWithErrors =
      val accessors = cls.caseAccessors
      val elemLabels = accessors.map(acc => ConstantType(Constant(acc.name.toString)))
      val typeElems = tps.getOrElse(accessors.map(mirroredType.resultType.memberInfo(_).widenExpr))
      val nestedPairs = TypeOps.nestedPairs(typeElems)
      val (monoType, elemsType) = mirroredType match
        case mirroredType: HKTypeLambda =>
          (mkMirroredMonoType(mirroredType), mirroredType.derivedLambdaType(resType = nestedPairs))
        case _ =>
          (mirroredType, nestedPairs)
      val elemsLabels = TypeOps.nestedPairs(elemLabels)
      checkRefinement(formal, tpnme.MirroredElemTypes, elemsType, span)
      checkRefinement(formal, tpnme.MirroredElemLabels, elemsLabels, span)
      val mirrorType = formal.constrained_& {
        mirrorCore(defn.Mirror_ProductClass, monoType, mirroredType, cls.name)
          .refinedWith(tpnme.MirroredElemTypes, TypeAlias(elemsType))
          .refinedWith(tpnme.MirroredElemLabels, TypeAlias(elemsLabels))
      }
      val mirrorRef =
        if cls.useCompanionAsProductMirror then companionPath(mirroredType, span)
        else if defn.isTupleClass(cls) then newTupleMirror(typeElems.size) // TODO: cls == defn.PairClass when > 22
        else anonymousMirror(monoType, MirrorImpl.OfProduct(pre), span)
      withNoErrors(mirrorRef.cast(mirrorType).withSpan(span))
    end makeProductMirror

    MirrorSource.reduce(mirroredType) match
      case Right(msrc) => msrc match
        case MirrorSource.Singleton(_, tref) =>
          val singleton = tref.termSymbol // prefer alias name over the orignal name
          val singletonPath = pathFor(tref).withSpan(span)
          if tref.classSymbol.is(Scala2x) then // could be Scala 3 alias of Scala 2 case object.
            val mirrorType = formal.constrained_& {
              mirrorCore(defn.Mirror_SingletonProxyClass, mirroredType, mirroredType, singleton.name)
            }
            val mirrorRef = New(defn.Mirror_SingletonProxyClass.typeRef, singletonPath :: Nil)
            withNoErrors(mirrorRef.cast(mirrorType).withSpan(span))
          else
            val mirrorType = formal.constrained_& {
              mirrorCore(defn.Mirror_SingletonClass, mirroredType, mirroredType, singleton.name)
            }
            withNoErrors(singletonPath.cast(mirrorType).withSpan(span))
        case MirrorSource.GenericTuple(tps) =>
          val maxArity = Definitions.MaxTupleArity
          val arity = tps.size
          if tps.size <= maxArity then
            val tupleCls = defn.TupleType(arity).nn.classSymbol
            makeProductMirror(tupleCls.owner.reachableThisType, tupleCls, Some(tps))
          else
            val reason = s"it reduces to a tuple with arity $arity, expected arity <= $maxArity"
            withErrors(i"${defn.PairClass} is not a generic product because $reason")
        case MirrorSource.ClassSymbol(pre, cls) =>
          if cls.isGenericProduct then
            if ctx.runZincPhases then
              // The mirror should be resynthesized if the constructor of the
              // case class `cls` changes. See `sbt-test/source-dependencies/mirror-product`.
              val rec = ctx.compilationUnit.depRecorder
              rec.addClassDependency(cls, DependencyByMemberRef)
              rec.addUsedName(cls.primaryConstructor)
            makeProductMirror(pre, cls, None)
          else withErrors(i"$cls is not a generic product because ${cls.whyNotGenericProduct}")
      case Left(msg) =>
        withErrors(i"type `$mirroredType` is not a generic product because $msg")
  end productMirror

  private def sumMirror(mirroredType: Type, formal: Type, span: Span)(using Context): TreeWithErrors =

    val (acceptableMsg, pre, cls) = MirrorSource.reduce(mirroredType) match
      case Right(MirrorSource.Singleton(_, tp)) => (i"its subpart `$tp` is a term reference", NoType, NoSymbol)
      case Right(MirrorSource.ClassSymbol(pre, cls)) => ("", pre, cls)
      case Right(MirrorSource.GenericTuple(tps)) =>
        val arity = tps.size
        val cls = if arity <= Definitions.MaxTupleArity then defn.TupleType(arity).nn.classSymbol else defn.PairClass
        ("", NoType, cls)
      case Left(msg) => (msg, NoType, NoSymbol)

    val clsIsGenericSum = cls.isGenericSum(pre)

    if acceptableMsg.isEmpty && clsIsGenericSum then
      if ctx.runZincPhases then
        // The mirror should be resynthesized if any child of the sealed class
        // `cls` changes. See `sbt-test/source-dependencies/mirror-sum`.
        val rec = ctx.compilationUnit.depRecorder
        rec.addClassDependency(cls, DependencyByMemberRef)
        rec.addUsedName(cls, includeSealedChildren = true)

      val elemLabels = cls.children.map(c => ConstantType(Constant(c.name.toString)))

      def internalError(msg: => String)(using Context): Unit =
        report.error(em"""Internal error when synthesizing sum mirror for $cls:
                         |$msg""", ctx.source.atSpan(span))

      def childPrefix(child: Symbol)(using Context): Type =
        val symPre = TypeOps.childPrefix(pre, cls, child)
        if !symPre.exists then
          internalError(i"unable to refine child prefix for $child from $pre")
        symPre

      def refineAtPrefix(childPre: Type, child: Symbol, original: Type): Type =
        if childPre.exists then original.asSeenFrom(childPre, child.owner) else original

      def solve(childPre: Type, child: Symbol): Type = child match
        case childClass: ClassSymbol =>
          assert(childClass.isOneOf(Case | Sealed))
          if childClass.is(Module) then
            val module = childClass.sourceModule
            refineAtPrefix(childPre, module, module.termRef)
          else
            refineAtPrefix(childPre, childClass, childClass.primaryConstructor.info) match
              case info: PolyType =>
                // Compute the the full child type by solving the subtype constraint
                // `C[X1, ..., Xn] <: P`, where
                //
                //   - P is the current `mirroredType`
                //   - C is the child class, with type parameters X1, ..., Xn
                //
                // Contravariant type parameters are minimized, all other type parameters are maximized.
                def instantiate(using Context) =
                  val poly = constrained(info, untpd.EmptyTree)._1
                  val resType = poly.finalResultType
                  val target = mirroredType match
                    case tp: HKTypeLambda => tp.resultType
                    case tp => tp
                  resType <:< target
                  val tparams = poly.paramRefs
                  val variances = childClass.typeParams.map(_.paramVarianceSign)
                  val instanceTypes = tparams.lazyZip(variances).map((tparam, variance) =>
                    TypeComparer.instanceType(tparam, fromBelow = variance < 0, widenUnions = true)
                  )
                  val instanceType = resType.substParams(poly, instanceTypes)
                  // this is broken in tests/run/i13332intersection.scala,
                  // because type parameters are not correctly inferred.
                  instanceType
                end instantiate
                instantiate(using ctx.fresh.setExploreTyperState().setOwner(childClass))
              case methTpe =>
                methTpe.finalResultType
        case child => refineAtPrefix(childPre, child, child.termRef)
      end solve

      val (childPres, childTypes) = cls.children.map(c =>
        val childPre = childPrefix(c)
        childPre -> solve(childPre, c)
      ).unzip

      val (monoType, elemsType) = mirroredType match
        case mirroredType: HKTypeLambda =>
          val elems = mirroredType.derivedLambdaType(
            resType = TypeOps.nestedPairs(childTypes)
          )
          (mkMirroredMonoType(mirroredType), elems)
        case _ =>
          val elems = TypeOps.nestedPairs(childTypes)
          (mirroredType, elems)

      val mirrorType =
        val labels = TypeOps.nestedPairs(elemLabels)
        formal.constrained_& {
          mirrorCore(defn.Mirror_SumClass, monoType, mirroredType, cls.name)
            .refinedWith(tpnme.MirroredElemTypes, TypeAlias(elemsType))
            .refinedWith(tpnme.MirroredElemLabels, TypeAlias(labels))
        }
      val mirrorRef =
        if cls.useCompanionAsSumMirror then companionPath(mirroredType, span)
        else anonymousMirror(monoType, MirrorImpl.OfSum(childPres), span)
      withNoErrors(mirrorRef.cast(mirrorType))
    else if acceptableMsg.nonEmpty then
      withErrors(i"type `$mirroredType` is not a generic sum because $acceptableMsg")
    else if !clsIsGenericSum then
      withErrors(i"$cls is not a generic sum because ${cls.whyNotGenericSum(pre)}")
      else
      EmptyTreeNoError
  end sumMirror

  def makeMirror
      (synth: (Type, Type, Span) => Context ?=> TreeWithErrors, formal: Type, span: Span)
      (using Context): TreeWithErrors =
    if checkFormal(formal) then
      formal.member(tpnme.MirroredType).info match
        case TypeBounds(mirroredType, _) =>
          val defined = fullyDefinedType(mirroredType, "Mirror.*Of argument", ctx.source.atSpan(span))
          val stripped = TypeOps.stripTypeVars(defined)
          synth(stripped, formal, span)
        case other => EmptyTreeNoError
    else EmptyTreeNoError

  /** An implied instance for a type of the form `Mirror.Product { type MirroredType = T }`
   *  where `T` is a generic product type or a case object or an enum case.
   */
  val synthesizedProductMirror: SpecialHandler = (formal, span) =>
    makeMirror(productMirror, formal, span)

  /** An implied instance for a type of the form `Mirror.Sum { type MirroredType = T }`
   *  where `T` is a generic sum type.
   */
  val synthesizedSumMirror: SpecialHandler = (formal, span) =>
    makeMirror(sumMirror, formal, span)

  /** An implied instance for a type of the form `Mirror { type MirroredType = T }`
   *  where `T` is a generic sum or product or singleton type.
   */
  val synthesizedMirror: SpecialHandler = (formal, span) =>
    orElse(synthesizedProductMirror(formal, span), synthesizedSumMirror(formal, span))

  private def escapeJavaArray(tp: Type)(using Context): Type = tp match
    case JavaArrayType(elemTp) => defn.ArrayOf(escapeJavaArray(elemTp))
    case _                     => tp

  private enum ManifestKind:
    case Full, Opt, Clss

    /** The kind that should be used for an array element, if we are `OptManifest` then this
     *  prevents wildcards arguments of Arrays being converted to `NoManifest`
     */
    def arrayElem = if this == Full then this else Clss

  end ManifestKind

  /** Manifest factory that does enough to satisfy the equality semantics for
   *  - `scala.reflect.OptManifest` (only runtime class is recorded)
   *  - `scala.reflect.Manifest` (runtime class of arguments are recorded, with wildcard upper bounds wrapped)
   *  however,`toString` may be different.
   *
   * There are some differences to `ClassTag`,
   *  e.g. in Scala 2 `manifest[Int @unchecked]` will fail, but `classTag[Int @unchecked]` succeeds.
   */
  private def manifestFactoryOf(kind: ManifestKind): SpecialHandler = (formal, span) =>
    import ManifestKind.*

    /* Creates a tree that calls the factory method called constructor in object scala.reflect.Manifest */
    def factoryManifest(constructor: TermName, tparg: Type, args: Tree*): Tree =
      if args.contains(EmptyTree) then
        EmptyTree
      else
        val factory = if kind == Full then defn.ManifestFactoryModule else defn.ClassManifestFactoryModule
        applyOverloaded(ref(factory), constructor, args.toList, tparg :: Nil, Types.WildcardType).withSpan(span)

    /* Creates a tree representing one of the singleton manifests.*/
    def singletonManifest(name: TermName) =
      ref(defn.ManifestFactoryModule).select(name).ensureApplied.withSpan(span)

    def synthArrayManifest(elemTp: Type, kind: ManifestKind, topLevel: Boolean): Tree =
      factoryManifest(nme.arrayType, elemTp, synthesize(elemTp, kind.arrayElem, topLevel))

    /** manifests generated from wildcards can not equal Int,Long,Any,AnyRef,AnyVal etc,
     *  so we wrap their upper bound.
     */
    def synthWildcardManifest(tp: Manifestable, hi: Type, topLevel: Boolean): Tree =
      factoryManifest(nme.wildcardType, tp, singletonManifest(nme.Nothing), synthesize(hi, Full, topLevel))

    /** `Nil` if not full manifest */
    def synthArgManifests(tp: Manifestable): List[Tree] = tp match
      case AppliedType(_, args) if kind == Full && tp.typeSymbol.isClass =>
        args.map(synthesize(_, Full, topLevel = false))
      case _ =>
        Nil

    /** This type contains all top-level types supported by Scala 2's algorithm */
    type Manifestable =
      ThisType | TermRef | ConstantType | TypeRef | AppliedType | TypeBounds | RecType | RefinedType | AndType

    def canManifest(tp: Manifestable, topLevel: Boolean) =
      val sym = tp.typeSymbol
      !sym.isAbstractType
      && hasStableErasure(tp)
      && !(topLevel && defn.isBottomClassAfterErasure(sym))

    /** adapted from `syntheticClassTag` */
    def synthManifest(tp: Manifestable, kind: ManifestKind, topLevel: Boolean) = tp match
      case defn.ArrayOf(elemTp)              => synthArrayManifest(elemTp, kind, topLevel)
      case TypeBounds(_, hi) if kind == Full => synthWildcardManifest(tp, hi, topLevel)

      case tp if canManifest(tp, topLevel) =>
        val sym = tp.typeSymbol
        if sym.isPrimitiveValueClass || defn.SpecialManifestClasses.contains(sym) then
          singletonManifest(sym.name.toTermName)
        else
          erasure(tp) match
            case JavaArrayType(elemTp) =>
              synthArrayManifest(escapeJavaArray(elemTp), kind, topLevel)

            case etp =>
              val clsArg = clsOf(etp).asInstance(defn.ClassType(tp)) // cast needed to resolve overloading
              factoryManifest(nme.classType, tp, (clsArg :: synthArgManifests(tp))*)

      case _ =>
        EmptyTree

    end synthManifest

    def manifestOfType(tp0: Type, kind: ManifestKind, topLevel: Boolean): Tree = tp0.dealiasKeepAnnots match
      case tp1: Manifestable => synthManifest(tp1, kind, topLevel)
      case tp1               => EmptyTree

    def synthesize(tp: Type, kind: ManifestKind, topLevel: Boolean): Tree =
      manifestOfType(tp, kind, topLevel) match
        case EmptyTree if kind == Opt => ref(defn.NoManifestModule)
        case result                   => result

    formal.argInfos match
      case arg :: Nil =>
        val manifest = synthesize(fullyDefinedType(arg, "Manifest argument", ctx.source.atSpan(span)), kind, topLevel = true)
        if manifest != EmptyTree then
          report.deprecationWarning(
            em"""Compiler synthesis of Manifest and OptManifest is deprecated, instead
                |replace with the type `scala.reflect.ClassTag[$arg]`.
                |Alternatively, consider using the new metaprogramming features of Scala 3,
                |see https://docs.scala-lang.org/scala3/reference/metaprogramming.html""",
            ctx.source.atSpan(span))
        withNoErrors(manifest)
      case _ =>
        EmptyTreeNoError

  end manifestFactoryOf

  val synthesizedManifest: SpecialHandler = manifestFactoryOf(ManifestKind.Full)
  val synthesizedOptManifest: SpecialHandler = manifestFactoryOf(ManifestKind.Opt)

  val specialHandlers = List(
    defn.ClassTagClass        -> synthesizedClassTag,
    defn.TypeTestClass        -> synthesizedTypeTest,
    defn.CanEqualClass        -> synthesizedCanEqual,
    defn.ValueOfClass         -> synthesizedValueOf,
    defn.TupledFunctionClass  -> synthesizedTupleFunction,
    defn.Mirror_ProductClass  -> synthesizedProductMirror,
    defn.Mirror_SumClass      -> synthesizedSumMirror,
    defn.MirrorClass          -> synthesizedMirror,
    defn.ManifestClass        -> synthesizedManifest,
    defn.OptManifestClass     -> synthesizedOptManifest,
  )

  def tryAll(formal: Type, span: Span)(using Context): TreeWithErrors =
    def recur(handlers: SpecialHandlers): TreeWithErrors = handlers match
      case (cls, handler) :: rest =>
        def baseWithRefinements(tp: Type): Type = tp.dealias match
          case tp: RefinedType =>
            tp.derivedRefinedType(parent = baseWithRefinements(tp.parent))
          case _ =>
            tp.baseType(cls)
        val base = baseWithRefinements(formal)
        val result =
          if (base <:< formal.widenExpr)
            // With the subtype test we enforce that the searched type `formal` is of the right form
            handler(base, span)
          else EmptyTreeNoError
        orElse(result, recur(rest))
      case Nil =>
        EmptyTreeNoError
    val result = recur(specialHandlers)
    clearErrorsIfNotEmpty(result)

end Synthesizer

object Synthesizer:

  /** Tuple used to store the synthesis result with a list of errors.  */
  type TreeWithErrors = (Tree, List[String])
  private def withNoErrors(tree: Tree): TreeWithErrors = (tree, List.empty)
  private def withErrors(errors: String*): TreeWithErrors = (EmptyTree, errors.toList)

  private val EmptyTreeNoError: TreeWithErrors = withNoErrors(EmptyTree)

  private def orElse(treeWithErrors1: TreeWithErrors, treeWithErrors2: => TreeWithErrors): TreeWithErrors = treeWithErrors1 match
    case (tree, errors) if tree eq genericEmptyTree =>
      val (tree2, errors2) = treeWithErrors2
      (tree2, errors ::: errors2)
    case _ => treeWithErrors1

  private def clearErrorsIfNotEmpty(treeWithErrors: TreeWithErrors) = treeWithErrors match
    case (tree, _) if tree eq genericEmptyTree => treeWithErrors
    case (tree, _)                             => withNoErrors(tree)

end Synthesizer
