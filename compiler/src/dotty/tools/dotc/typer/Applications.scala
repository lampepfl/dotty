package dotty.tools
package dotc
package typer

import core.*
import ast.{Trees, tpd, untpd, desugar}
import util.Stats.record
import util.{SrcPos, NoSourcePosition}
import Contexts.*
import Flags.*
import Symbols.*
import Denotations.Denotation
import Types.*
import Decorators.*
import ErrorReporting.*
import Trees.*
import Names.*
import StdNames.*
import ContextOps.*
import NameKinds.DefaultGetterName
import ProtoTypes.*
import Inferencing.*
import reporting.*
import Nullables.*, NullOpsDecorator.*
import config.{Feature, SourceVersion}

import collection.mutable
import config.Printers.{overload, typr, unapp}
import TypeApplications.*
import Annotations.Annotation

import Constants.{Constant, IntTag}
import Denotations.SingleDenotation
import annotation.threadUnsafe

import scala.util.control.NonFatal
import dotty.tools.dotc.inlines.Inlines

object Applications {
  import tpd.*

  def extractorMember(tp: Type, name: Name)(using Context): SingleDenotation =
    tp.member(name).suchThat(sym => sym.info.isParameterless && sym.info.widenExpr.isValueType)

  def extractorMemberType(tp: Type, name: Name, errorPos: SrcPos)(using Context): Type = {
    val ref = extractorMember(tp, name)
    if (ref.isOverloaded)
      errorType(em"Overloaded reference to $ref is not allowed in extractor", errorPos)
    ref.info.widenExpr.annotatedToRepeated
  }

  /** Does `tp` fit the "product match" conditions as an unapply result type
   *  for a pattern with `numArgs` subpatterns?
   *  This is the case if `tp` has members `_1` to `_N` where `N == numArgs`.
   */
  def isProductMatch(tp: Type, numArgs: Int, errorPos: SrcPos = NoSourcePosition)(using Context): Boolean =
    numArgs > 0 && productArity(tp, errorPos) == numArgs

  /** Does `tp` fit the "product-seq match" conditions as an unapply result type
   *  for a pattern with `numArgs` subpatterns?
   *  This is the case if (1) `tp` has members `_1` to `_N` where `N <= numArgs + 1`.
   *                      (2) `tp._N` conforms to Seq match
   */
  def isProductSeqMatch(tp: Type, numArgs: Int, errorPos: SrcPos = NoSourcePosition)(using Context): Boolean = {
    val arity = productArity(tp, errorPos)
    arity > 0 && arity <= numArgs + 1 &&
      unapplySeqTypeElemTp(productSelectorTypes(tp, errorPos).last).exists
  }

  /** Does `tp` fit the "get match" conditions as an unapply result type?
   *  This is the case of `tp` has a `get` member as well as a
   *  parameterless `isEmpty` member of result type `Boolean`.
   */
  def isGetMatch(tp: Type, errorPos: SrcPos = NoSourcePosition)(using Context): Boolean =
    extractorMemberType(tp, nme.isEmpty, errorPos).widenSingleton.isRef(defn.BooleanClass) &&
    extractorMemberType(tp, nme.get, errorPos).exists

  /** If `getType` is of the form:
    *  ```
    *  {
    *    def lengthCompare(len: Int): Int // or, def length: Int
    *    def apply(i: Int): T = a(i)
    *    def drop(n: Int): scala.collection.Seq[T]
    *    def toSeq: scala.collection.Seq[T]
    *  }
    *  ```
    *  returns `T`, otherwise NoType.
    */
  def unapplySeqTypeElemTp(getTp: Type)(using Context): Type = {
    def lengthTp = ExprType(defn.IntType)
    def lengthCompareTp = MethodType(List(defn.IntType), defn.IntType)
    def applyTp(elemTp: Type) = MethodType(List(defn.IntType), elemTp)
    def dropTp(elemTp: Type) = MethodType(List(defn.IntType), defn.CollectionSeqType.appliedTo(elemTp))
    def toSeqTp(elemTp: Type) = ExprType(defn.CollectionSeqType.appliedTo(elemTp))

    // the result type of `def apply(i: Int): T`
    val elemTp = getTp.member(nme.apply).suchThat(_.info <:< applyTp(WildcardType)).info.resultType

    def hasMethod(name: Name, tp: Type) =
      getTp.member(name).suchThat(getTp.memberInfo(_) <:< tp).exists

    val isValid =
      elemTp.exists &&
      (hasMethod(nme.lengthCompare, lengthCompareTp) || hasMethod(nme.length, lengthTp)) &&
      hasMethod(nme.drop, dropTp(elemTp)) &&
      hasMethod(nme.toSeq, toSeqTp(elemTp))

    if (isValid) elemTp else NoType
  }

  def productSelectorTypes(tp: Type, errorPos: SrcPos)(using Context): List[Type] = {
    val sels = for (n <- Iterator.from(0)) yield extractorMemberType(tp, nme.selectorName(n), errorPos)
    sels.takeWhile(_.exists).toList
  }

  def tupleComponentTypes(tp: Type)(using Context): List[Type] =
    tp.widenExpr.dealias.normalized match
    case tp: AppliedType =>
      if defn.isTupleClass(tp.tycon.typeSymbol) then
        tp.args
      else if tp.tycon.derivesFrom(defn.PairClass) then
        val List(head, tail) = tp.args
        head :: tupleComponentTypes(tail)
      else
        Nil
    case _ =>
      Nil

  def productArity(tp: Type, errorPos: SrcPos = NoSourcePosition)(using Context): Int =
    if (defn.isProductSubType(tp)) productSelectorTypes(tp, errorPos).size else -1

  def productSelectors(tp: Type)(using Context): List[Symbol] = {
    val sels = for (n <- Iterator.from(0)) yield
      tp.member(nme.selectorName(n)).suchThat(_.info.isParameterless).symbol
    sels.takeWhile(_.exists).toList
  }

  def getUnapplySelectors(tp: Type, args: List[untpd.Tree], pos: SrcPos)(using Context): List[Type] =
    if (args.length > 1 && !(tp.derivesFrom(defn.SeqClass))) {
      val sels = productSelectorTypes(tp, pos)
      if (sels.length == args.length) sels
      else tp :: Nil
    }
    else tp :: Nil

  def productSeqSelectors(tp: Type, argsNum: Int, pos: SrcPos)(using Context): List[Type] = {
    val selTps = productSelectorTypes(tp, pos)
    val arity = selTps.length
    val elemTp = unapplySeqTypeElemTp(selTps.last)
    (0 until argsNum).map(i => if (i < arity - 1) selTps(i) else elemTp).toList
  }

  def unapplyArgs(unapplyResult: Type, unapplyFn: Tree, args: List[untpd.Tree], pos: SrcPos)(using Context): List[Type] = {
    def getName(fn: Tree): Name =
      fn match
        case TypeApply(fn, _) => getName(fn)
        case Apply(fn, _) => getName(fn)
        case fn: RefTree => fn.name
    val unapplyName = getName(unapplyFn) // tolerate structural `unapply`, which does not have a symbol

    def getTp = extractorMemberType(unapplyResult, nme.get, pos)

    def fail = {
      report.error(UnapplyInvalidReturnType(unapplyResult, unapplyName), pos)
      Nil
    }

    def unapplySeq(tp: Type)(fallback: => List[Type]): List[Type] = {
      val elemTp = unapplySeqTypeElemTp(tp)
      if (elemTp.exists) args.map(Function.const(elemTp))
      else if (isProductSeqMatch(tp, args.length, pos)) productSeqSelectors(tp, args.length, pos)
      else if tp.derivesFrom(defn.NonEmptyTupleClass) then foldApplyTupleType(tp)
      else fallback
    }

    if (unapplyName == nme.unapplySeq)
      unapplySeq(unapplyResult) {
        if (isGetMatch(unapplyResult, pos)) unapplySeq(getTp)(fail)
        else fail
      }
    else {
      assert(unapplyName == nme.unapply)
      if (isProductMatch(unapplyResult, args.length, pos))
        productSelectorTypes(unapplyResult, pos)
      else if (isGetMatch(unapplyResult, pos))
        getUnapplySelectors(getTp, args, pos)
      else if (unapplyResult.widenSingleton isRef defn.BooleanClass)
        Nil
      else if (defn.isProductSubType(unapplyResult) && productArity(unapplyResult, pos) != 0)
        productSelectorTypes(unapplyResult, pos)
          // this will cause a "wrong number of arguments in pattern" error later on,
          // which is better than the message in `fail`.
      else if unapplyResult.derivesFrom(defn.NonEmptyTupleClass) then
        foldApplyTupleType(unapplyResult)
      else fail
    }
  }

  def foldApplyTupleType(tp: Type)(using Context): List[Type] =
    object tupleFold extends TypeAccumulator[List[Type]]:
      override def apply(accum: List[Type], t: Type): List[Type] =
        t match
          case AppliedType(tycon, x :: x2 :: Nil) if tycon.typeSymbol == defn.PairClass =>
            apply(x :: accum, x2)
          case x => foldOver(accum, x)
    end tupleFold
    tupleFold(Nil, tp).reverse

  def wrapDefs(defs: mutable.ListBuffer[Tree] | Null, tree: Tree)(using Context): Tree =
    if (defs != null && defs.nonEmpty) tpd.Block(defs.toList, tree) else tree

  /** Optionally, if `sym` is a symbol created by `resolveMapped`, i.e. representing
   *  a mapped alternative, the original prefix of the alternative and the number of
   *  skipped term parameters.
   */
  private def mappedAltInfo(sym: Symbol)(using Context): Option[(Type, Int)] =
    for ann <- sym.getAnnotation(defn.MappedAlternativeAnnot) yield
      val AppliedType(_, pre :: ConstantType(c) :: Nil) = ann.tree.tpe: @unchecked
      (pre, c.intValue)

  /** Find reference to default parameter getter for parameter #n in current
   *  parameter list, or EmptyTree if none was found.
   *  @param fn       the tree referring to the function part of this call
   *  @param n        the index of the parameter in the parameter list of the call
   *  @param testOnly true iff we just to find out whether a getter exists
   */
  def findDefaultGetter(fn: Tree, n: Int, testOnly: Boolean)(using Context): Tree =
    def reifyPrefix(pre: Type): Tree = pre match
      case pre: SingletonType => singleton(pre, needLoad = !testOnly)
      case pre if testOnly =>
        // In this case it is safe to skolemize now; we will produce a stable prefix for the actual call.
        ref(pre.narrow)
      case _ => EmptyTree

    if fn.symbol.hasDefaultParams then
      val meth = fn.symbol.asTerm
      val idx = n + numArgs(fn)
      methPart(fn) match
        case Select(receiver, _) =>
          findDefaultGetter(meth, receiver, idx)
        case mr => mappedAltInfo(meth) match
          case Some((pre, skipped)) =>
            findDefaultGetter(meth, reifyPrefix(pre), idx + skipped)
          case None =>
            findDefaultGetter(meth, reifyPrefix(mr.tpe.normalizedPrefix), idx)
    else EmptyTree // structural applies don't have symbols or defaults
  end findDefaultGetter

  /** Find reference to default parameter getter for method `meth` numbered `idx`
   *  selected from given `receiver`, or EmptyTree if none was found.
   *  @param meth     the called method (can be mapped by resolveMapped)
   *  @param receiver the receiver of the original method call, which determines
   *                  where default getters are found
   *  @param idx      the index of the searched for default getter, as encoded in its name
   */
  def findDefaultGetter(meth: TermSymbol, receiver: Tree, idx: Int)(using Context): Tree =
    val getterPrefix =
      if (meth.is(Synthetic) && meth.name == nme.apply) nme.CONSTRUCTOR else meth.name
    val getterName = DefaultGetterName(getterPrefix, idx)

    if receiver.isEmpty then
      def findGetter(cx: Context): Tree =
        if cx eq NoContext then EmptyTree
        else if cx.scope != cx.outer.scope
            && cx.denotNamed(meth.name).hasAltWith(_.symbol == meth) then
          val denot = cx.denotNamed(getterName)
          if denot.exists then ref(TermRef(cx.owner.thisType, getterName, denot))
          else findGetter(cx.outer)
        else findGetter(cx.outer)
      findGetter(ctx)
    else
      def selectGetter(qual: Tree): Tree =
        val getterDenot = qual.tpe.member(getterName)
          .accessibleFrom(qual.tpe.widenIfUnstable, superAccess = true) // to reset Local
        if (getterDenot.exists) qual.select(TermRef(qual.tpe, getterName, getterDenot))
        else EmptyTree
      if !meth.isClassConstructor then
        selectGetter(receiver)
      else
        // default getters for class constructors are found in the companion object
        val cls = meth.owner
        val companion = cls.companionModule
        if companion.isTerm then
          val prefix = receiver.tpe.baseType(cls).normalizedPrefix
          if prefix.exists then selectGetter(ref(TermRef(prefix, companion.asTerm)))
          else EmptyTree
        else EmptyTree
  end findDefaultGetter

  /** Splice new method reference `meth` into existing application `app` */
  private def spliceMeth(meth: Tree, app: Tree)(using Context): Tree = app match {
    case Apply(fn, args) =>
      // Constructors always have one leading non-implicit parameter list.
      // Empty list is inserted for constructors where the first parameter list is implicit.
      //
      // Therefore, we need to ignore the first empty argument list.
      // This is needed for the test tests/neg/i12344.scala
      //
      // see NamerOps.normalizeIfConstructor
      //
      if args == Nil
         && !fn.isInstanceOf[Apply]
         && app.tpe.isImplicitMethod
         && fn.symbol.isConstructor
      then meth
      else spliceMeth(meth, fn).appliedToArgs(args)
    case TypeApply(fn, targs) =>
      // Note: It is important that the type arguments `targs` are passed in new trees
      // instead of being spliced in literally. Otherwise, a type argument to a default
      // method could be constructed as the definition site of the type variable for
      // that default constructor. This would interpolate type variables too early,
      // causing lots of tests (among them tasty_unpickleScala2) to fail.
      //
      // The test case is in i1757.scala. Here we have a variable `s` and a method `cpy`
      // defined like this:
      //
      //      var s
      //      def cpy[X](b: List[Int] = b): B[X] = new B[X](b)
      //
      // The call `s.cpy()` then gets expanded to
      //
      //      { val $1$: B[Int] = this.s
      //        $1$.cpy[X']($1$.cpy$default$1[X']
      //      }
      //
      // A type variable gets interpolated if it does not appear in the type
      // of the current tree and the current tree contains the variable's "definition".
      // Previously, the polymorphic function tree to which the variable was first added
      // was taken as the variable's definition. But that fails here because that
      // tree was `s.cpy` but got transformed into `$1$.cpy`. We now take the type argument
      // [X'] of the variable as its definition tree, which is more robust. But then
      // it's crucial that the type tree is not copied directly as argument to
      // `cpy$default$1`. If it was, the variable `X'` would already be interpolated
      // when typing the default argument, which is too early.
      spliceMeth(meth, fn).appliedToTypeTrees(targs.map(targ => TypeTree(targ.tpe).withSpan(targ.span)))
    case _ => meth
  }

  def defaultArgument(fn: Tree, n: Int, testOnly: Boolean)(using Context): Tree =
    val getter = findDefaultGetter(fn, n, testOnly)
    if getter.isEmpty then getter
    else spliceMeth(getter.withSpan(fn.span), fn)

  def retypeSignaturePolymorphicFn(fun: Tree, methType: Type)(using Context): Tree =
    val sym1 = fun.symbol
    val flags2 = sym1.flags | NonMember // ensures Select typing doesn't let TermRef#withPrefix revert the type
    val sym2 = sym1.copy(info = methType, flags = flags2) // symbol not entered, to avoid overload resolution problems
    fun.withType(sym2.termRef)
}

trait Applications extends Compatibility {
  self: Typer & Dynamic =>

  import Applications.*
  import tpd.{ cpy => _, _ }
  import untpd.cpy

  /** @tparam Arg       the type of arguments, could be tpd.Tree, untpd.Tree, or Type
   *  @param methRef    the reference to the method of the application
   *  @param funType    the type of the function part of the application
   *  @param args       the arguments of the application
   *  @param resultType the expected result type of the application
   */
  abstract class Application[Arg](methRef: TermRef, funType: Type, args: List[Arg], resultType: Type)(using Context) {

    /** The type of typed arguments: either tpd.Tree or Type */
    type TypedArg

    /** The kind of application that gets typed */
    def applyKind: ApplyKind

    /** Given an original argument and the type of the corresponding formal
     *  parameter, produce a typed argument.
     */
    protected def typedArg(arg: Arg, formal: Type): TypedArg

    /** Turn a typed tree into an argument */
    protected def treeToArg(arg: Tree): Arg

    /** Check that argument corresponds to type `formal` and
     *  possibly add it to the list of adapted arguments
     */
    protected def addArg(arg: TypedArg, formal: Type): Unit

    /** Is this an argument of the form `expr: _*` or a RepeatedParamType
     *  derived from such an argument?
     */
    protected def isVarArg(arg: Arg): Boolean

    /** If constructing trees, turn last `n` processed arguments into a
     *  `SeqLiteral` tree with element type `elemFormal`.
     */
    protected def makeVarArg(n: Int, elemFormal: Type): Unit

    /** If all `args` have primitive numeric types, make sure it's the same one */
    protected def harmonizeArgs(args: List[TypedArg]): List[TypedArg]

    /** Signal failure with given message at position of given argument */
    protected def fail(msg: Message, arg: Arg): Unit

    /** Signal failure with given message at position of the application itself */
    protected def fail(msg: Message): Unit

    protected def appPos: SrcPos

    /** The current function part, which might be affected by lifting.
     */
    protected def normalizedFun: Tree

    protected def typeOfArg(arg: Arg): Type

    /** If constructing trees, pull out all parts of the function
     *  which are not idempotent into separate prefix definitions
     */
    protected def liftFun(): Unit = ()

    /** Whether `liftFun` is needed? It is the case if default arguments are used.
     */
    protected def needLiftFun: Boolean = {
      def requiredArgNum(tp: Type): Int = tp.widen match {
        case funType: MethodType =>
          val paramInfos = funType.paramInfos
          val argsNum = paramInfos.size
          if (argsNum >= 1 && paramInfos.last.isRepeatedParam)
            // Repeated arguments do not count as required arguments
            argsNum - 1
          else
            argsNum
        case funType: PolyType => requiredArgNum(funType.resultType)
        case tp => args.size
      }

      !isJavaAnnotConstr(methRef.symbol) &&
      args.size < requiredArgNum(funType)
    }

    /** A flag signalling that the typechecking the application was so far successful */
    private var _ok = true

    def ok: Boolean = _ok
    def ok_=(x: Boolean): Unit = _ok = x

    /** The function's type after widening and instantiating polytypes
     *  with TypeParamRefs in constraint set
     */
    @threadUnsafe lazy val methType: Type = {
      def rec(t: Type): Type = {
        t.widen match{
          case funType: MethodType => funType
          case funType: PolyType =>
            rec(instantiateWithTypeVars(funType))
          case tp => tp
        }
      }

      rec(liftedFunType)
    }

    @threadUnsafe lazy val liftedFunType: Type =
      if (needLiftFun) {
        liftFun()
        normalizedFun.tpe
      }
      else funType

    /** The arguments re-ordered so that each named argument matches the
     *  same-named formal parameter.
     */
    @threadUnsafe lazy val orderedArgs: List[Arg] =
      if (hasNamedArg(args))
        reorder(args.asInstanceOf[List[untpd.Tree]]).asInstanceOf[List[Arg]]
      else
        args

    protected def init(): Unit = methType match {
      case methType: MethodType =>
        val resultApprox = resultTypeApprox(methType)
        val sym = methRef.symbol
        if ctx.typerState.isCommittable then
          // Here we call `resultType` only to accumulate constraints (even if
          // it fails, we might be able to heal the expression to conform to the
          // result type) so don't check for views since `viewExists` doesn't
          // have any side-effect and would only slow the compiler down (cf #14333).
          NoViewsAllowed.constrainResult(sym, resultApprox, resultType)
        else if !constrainResult(sym, resultApprox, resultType) then
          // Here we actually record that this alternative failed so that
          // overloading resolution might prune it.
          fail(TypeMismatch(methType.resultType, resultType, None))

        // match all arguments with corresponding formal parameters
        matchArgs(orderedArgs, methType.paramInfos, 0)
      case _ =>
        if (methType.isError) ok = false
        else fail(em"$methString does not take parameters")
    }

    /** The application was successful */
    def success: Boolean = ok

    protected def methodType: MethodType = methType.asInstanceOf[MethodType]
    private def methString: String =
      def infoStr = if methType.isErroneous then "" else i": $methType"
      i"${err.refStr(methRef)}$infoStr"

    /** Re-order arguments to correctly align named arguments
     *  Issue errors in the following situations:
     *
     *    - "positional after named argument" if a positional argument follows a named
     *      argument and one of the following is true:
     *
     *        - There is a formal argument before the argument position
     *          that has not yet been instantiated with a previous actual argument,
     *          (either named or positional), or
     *        - The formal parameter at the argument position is also mentioned
     *          in a subsequent named parameter.
     *    - "parameter already instantiated" if a two named arguments have the same name.
     *    - "does not have parameter" if a named parameter does not mention a formal
     *      parameter name.
     */
    def reorder[T <: Untyped](args: List[Trees.Tree[T]]): List[Trees.Tree[T]] = {

      /** @param pnames    The list of parameter names that are missing arguments
       *  @param args      The list of arguments that are not yet passed, or that are waiting to be dropped
       *  @param nameToArg A map from as yet unseen names to named arguments
       *  @param toDrop    A set of names that have already be passed as named arguments
       *
       *  For a well-typed application we have the invariants
       *
       *  1. `(args diff toDrop)` can be reordered to match `pnames`
       *  2. For every `(name -> arg)` in `nameToArg`, `arg` is an element of `args`
       */
      def handleNamed(pnames: List[Name], args: List[Trees.Tree[T]],
                      nameToArg: Map[Name, Trees.NamedArg[T]], toDrop: Set[Name],
                      missingArgs: Boolean): List[Trees.Tree[T]] = pnames match {
        case pname :: pnames1 if nameToArg contains pname =>
          // there is a named argument for this parameter; pick it
          nameToArg(pname) :: handleNamed(pnames1, args, nameToArg - pname, toDrop + pname, missingArgs)
        case _ =>
          def pnamesRest = if (pnames.isEmpty) pnames else pnames.tail
          args match {
            case (arg @ NamedArg(aname, _)) :: args1 =>
              if (toDrop contains aname) // argument is already passed
                handleNamed(pnames, args1, nameToArg, toDrop - aname, missingArgs)
              else if ((nameToArg contains aname) && pnames.nonEmpty) // argument is missing, pass an empty tree
                genericEmptyTree :: handleNamed(pnames.tail, args, nameToArg, toDrop, missingArgs = true)
              else { // name not (or no longer) available for named arg
                def msg =
                  if (methodType.paramNames contains aname)
                    em"parameter $aname of $methString is already instantiated"
                  else
                    em"$methString does not have a parameter $aname"
                fail(msg, arg.asInstanceOf[Arg])
                arg :: handleNamed(pnamesRest, args1, nameToArg, toDrop, missingArgs)
              }
            case arg :: args1 =>
              if toDrop.nonEmpty || missingArgs then
                report.error(i"positional after named argument", arg.srcPos)
              arg :: handleNamed(pnamesRest, args1, nameToArg, toDrop, missingArgs) // unnamed argument; pick it
            case Nil => // no more args, continue to pick up any preceding named args
              if (pnames.isEmpty) Nil
              else handleNamed(pnamesRest, args, nameToArg, toDrop, missingArgs)
          }
      }

      def handlePositional(pnames: List[Name], args: List[Trees.Tree[T]]): List[Trees.Tree[T]] =
        args match {
          case (arg: NamedArg @unchecked) :: _ =>
            val nameAssocs = for (case arg @ NamedArg(name, _) <- args) yield (name, arg)
            handleNamed(pnames, args, nameAssocs.toMap, toDrop = Set(), missingArgs = false)
          case arg :: args1 =>
            arg :: handlePositional(if (pnames.isEmpty) Nil else pnames.tail, args1)
          case Nil => Nil
        }

      handlePositional(methodType.paramNames, args)
    }

    /** Is `sym` a constructor of a Java-defined annotation? */
    def isJavaAnnotConstr(sym: Symbol): Boolean =
      sym.is(JavaDefined) && sym.isConstructor && sym.owner.is(JavaAnnotation)

    /** Match re-ordered arguments against formal parameters
     *  @param n   The position of the first parameter in formals in `methType`.
     */
    def matchArgs(args: List[Arg], formals: List[Type], n: Int): Unit =
      if (success) formals match {
        case formal :: formals1 =>

          def checkNoVarArg(arg: Arg) =
            if !ctx.isAfterTyper && isVarArg(arg) then
              val addendum =
                if formal.isRepeatedParam then
                  i"it is not the only argument to be passed to the corresponding repeated parameter $formal"
                else
                  i"the corresponding parameter has type $formal which is not a repeated parameter type"
              fail(em"Sequence argument type annotation `*` cannot be used here:\n$addendum", arg)

          /** Add result of typing argument `arg` against parameter type `formal`.
           *  @return  The remaining formal parameter types. If the method is parameter-dependent
           *           this means substituting the actual argument type for the current formal parameter
           *           in the remaining formal parameters.
           */
          def addTyped(arg: Arg): List[Type] =
            if !formal.isRepeatedParam then checkNoVarArg(arg)
            addArg(typedArg(arg, formal), formal)
            if methodType.isParamDependent && typeOfArg(arg).exists then
              // `typeOfArg(arg)` could be missing because the evaluation of `arg` produced type errors
              formals1.mapconserve(safeSubstParam(_, methodType.paramRefs(n), typeOfArg(arg)))
            else
              formals1

          def missingArg(n: Int): Unit =
            fail(MissingArgument(methodType.paramNames(n), methString))

          def tryDefault(n: Int, args1: List[Arg]): Unit = {
            val sym = methRef.symbol
            val testOnly = this.isInstanceOf[TestApplication[?]]

            val defaultArg =
              if (isJavaAnnotConstr(sym)) {
                val cinfo = sym.owner.asClass.classInfo
                val pname = methodType.paramNames(n)
                val hasDefault = cinfo.member(pname)
                  .suchThat(d => d.is(Method) && d.hasAnnotation(defn.AnnotationDefaultAnnot)).exists

                // Use `_` as a placeholder for the default value of an
                // annotation parameter, this will be recognized by the backend.
                if (hasDefault)
                  tpd.Underscore(formal)
                else
                  EmptyTree
              }
              else defaultArgument(normalizedFun, n, testOnly)

            def implicitArg = implicitArgTree(formal, appPos.span)

            if !defaultArg.isEmpty then
              defaultArg.tpe.widen match
                case _: MethodOrPoly if testOnly => matchArgs(args1, formals1, n + 1)
                case _ => matchArgs(args1, addTyped(treeToArg(defaultArg)), n + 1)
            else if methodType.isImplicitMethod && ctx.mode.is(Mode.ImplicitsEnabled) then
              matchArgs(args1, addTyped(treeToArg(implicitArg)), n + 1)
            else
              missingArg(n)
          }

          if (formal.isRepeatedParam)
            args match {
              case arg :: Nil if isVarArg(arg) =>
                addTyped(arg)
              case (arg @ Typed(Literal(Constant(null)), _)) :: Nil if ctx.isAfterTyper =>
                addTyped(arg)
              case _ =>
                val elemFormal = formal.widenExpr.argTypesLo.head
                val typedArgs =
                  harmonic(harmonizeArgs, elemFormal) {
                    args.map { arg =>
                      checkNoVarArg(arg)
                      typedArg(arg, elemFormal)
                    }
                  }
                typedArgs.foreach(addArg(_, elemFormal))
                makeVarArg(args.length, elemFormal)
            }
          else args match {
            case EmptyTree :: args1 =>
              tryDefault(n, args1)
            case arg :: args1 =>
              matchArgs(args1, addTyped(arg), n + 1)
            case nil =>
              tryDefault(n, args)
          }

        case nil =>
          args match {
            case arg :: args1 =>
              def msg = arg match
                case untpd.Tuple(Nil)
                if applyKind == ApplyKind.InfixTuple && funType.widen.isNullaryMethod =>
                  em"can't supply unit value with infix notation because nullary $methString takes no arguments; use dotted invocation instead: (...).${methRef.name}()"
                case _ =>
                  em"too many arguments for $methString"
              fail(msg, arg)
            case nil =>
          }
      }
  }

  /** The degree to which an argument has to match a formal parameter */
  enum ArgMatch:
    case Compatible    // argument is compatible with formal
    case CompatibleCAP // capture-converted argument is compatible with formal

  /** Subclass of Application for the cases where we are interested only
   *  in a "can/cannot apply" answer, without needing to construct trees or
   *  issue error messages.
   */
  abstract class TestApplication[Arg](methRef: TermRef, funType: Type, args: List[Arg], resultType: Type, argMatch: ArgMatch)(using Context)
  extends Application[Arg](methRef, funType, args, resultType) {
    type TypedArg = Arg
    type Result = Unit

    def applyKind = ApplyKind.Regular

    protected def argOK(arg: TypedArg, formal: Type): Boolean = argType(arg, formal) match
      case ref: TermRef if ref.denot.isOverloaded =>
        // in this case we could not resolve overloading because no alternative
        // matches expected type
        false
      case argtpe =>
        val argtpe1 = argtpe.widen

        def SAMargOK =
          defn.isFunctionNType(argtpe1) && formal.match
            case SAMType(samMeth, samParent) => argtpe <:< samMeth.toFunctionType(isJava = samParent.classSymbol.is(JavaDefined))
            case _ => false

        isCompatible(argtpe, formal)
        // Only allow SAM-conversion to PartialFunction if implicit conversions
        // are enabled. This is necessary to avoid ambiguity between an overload
        // taking a PartialFunction and one taking a Function1 because
        // PartialFunction extends Function1 but Function1 is SAM-convertible to
        // PartialFunction. Concretely, given:
        //
        //   def foo(a: Int => Int): Unit = println("1")
        //   def foo(a: PartialFunction[Int, Int]): Unit = println("2")
        //
        // - `foo(x => x)` will print 1, because the PartialFunction overload
        //   won't be seen as applicable in the first call to
        //   `resolveOverloaded`, this behavior happens to match what Java does
        //   since PartialFunction is not a SAM type according to Java
        //   (`isDefined` is abstract).
        // - `foo { case x if x % 2 == 0 => x }` will print 2, because both
        //    overloads are applicable, but PartialFunction is a subtype of
        //    Function1 so it's more specific.
        || (!formal.isRef(defn.PartialFunctionClass) || ctx.mode.is(Mode.ImplicitsEnabled)) && SAMargOK
        || argMatch == ArgMatch.CompatibleCAP
            && {
              val argtpe1 = argtpe.widen
              val captured = captureWildcardsCompat(argtpe1, formal.widenExpr)
              captured ne argtpe1
            }

    /** The type of the given argument */
    protected def argType(arg: Arg, formal: Type): Type

    def typedArg(arg: Arg, formal: Type): Arg = arg
    final def addArg(arg: TypedArg, formal: Type): Unit = ok = ok & argOK(arg, formal)
    def makeVarArg(n: Int, elemFormal: Type): Unit = {}
    def fail(msg: Message, arg: Arg): Unit =
      ok = false
    def fail(msg: Message): Unit =
      ok = false
    def appPos: SrcPos = NoSourcePosition
    @threadUnsafe lazy val normalizedFun: Tree = ref(methRef, needLoad = false)
    init()
  }

  /** Subclass of Application for applicability tests with type arguments and value
   *  argument trees.
   */
  class ApplicableToTrees(methRef: TermRef, args: List[Tree], resultType: Type, argMatch: ArgMatch)(using Context)
  extends TestApplication(methRef, methRef.widen, args, resultType, argMatch) {
    def argType(arg: Tree, formal: Type): Type =
      if untpd.isContextualClosure(arg) && defn.isContextFunctionType(formal) then arg.tpe
      else normalize(arg.tpe, formal)
    def treeToArg(arg: Tree): Tree = arg
    def isVarArg(arg: Tree): Boolean = tpd.isWildcardStarArg(arg)
    def typeOfArg(arg: Tree): Type = arg.tpe
    def harmonizeArgs(args: List[Tree]): List[Tree] = harmonize(args)
  }

  /** Subclass of Application for applicability tests with value argument types. */
  class ApplicableToTypes(methRef: TermRef, args: List[Type], resultType: Type, argMatch: ArgMatch)(using Context)
  extends TestApplication(methRef, methRef, args, resultType, argMatch) {
    def argType(arg: Type, formal: Type): Type = arg
    def treeToArg(arg: Tree): Type = arg.tpe
    def isVarArg(arg: Type): Boolean = arg.isRepeatedParam
    def typeOfArg(arg: Type): Type = arg
    def harmonizeArgs(args: List[Type]): List[Type] = harmonizeTypes(args)
  }

  /** Subclass of Application for type checking an Apply node, where
   *  types of arguments are either known or unknown.
   */
  abstract class TypedApply[T <: Untyped](
    app: untpd.Apply, fun: Tree, methRef: TermRef, args: List[Trees.Tree[T]], resultType: Type,
    override val applyKind: ApplyKind)(using Context)
  extends Application(methRef, fun.tpe, args, resultType) {
    type TypedArg = Tree
    def isVarArg(arg: Trees.Tree[T]): Boolean = untpd.isWildcardStarArg(arg)
    private var typedArgBuf = new mutable.ListBuffer[Tree]
    private var liftedDefs: mutable.ListBuffer[Tree] | Null = null
    private var myNormalizedFun: Tree = fun
    init()

    def addArg(arg: Tree, formal: Type): Unit =
      typedArgBuf += adapt(arg, formal.widenExpr)

    def makeVarArg(n: Int, elemFormal: Type): Unit = {
      val args = typedArgBuf.takeRight(n).toList
      typedArgBuf.dropRightInPlace(n)
      val elemtpt = TypeTree(elemFormal)
      typedArgBuf += seqToRepeated(SeqLiteral(args, elemtpt))
    }

    def harmonizeArgs(args: List[TypedArg]): List[Tree] =
      // harmonize args only if resType depends on parameter types
      if (isFullyDefined(methodType.resType, ForceDegree.none)) args
      else harmonize(args)

    override def appPos: SrcPos = app.srcPos

    def fail(msg: Message, arg: Trees.Tree[T]): Unit = {
      report.error(msg, arg.srcPos)
      ok = false
    }

    def fail(msg: Message): Unit = {
      report.error(msg, app.srcPos)
      ok = false
    }

    def normalizedFun:  Tree = myNormalizedFun

    private def lifter(using Context) =
      if (methRef.symbol.hasDefaultParams) LiftComplex else LiftImpure

    override def liftFun(): Unit =
      if (liftedDefs == null) {
        liftedDefs = new mutable.ListBuffer[Tree]
        myNormalizedFun = lifter.liftApp(liftedDefs.uncheckedNN, myNormalizedFun)
      }

    /** The index of the first difference between lists of trees `xs` and `ys`
     *  -1 if there are no differences.
     */
    private def firstDiff[T <: Trees.Tree[?]](xs: List[T], ys: List[T], n: Int = 0): Int = xs match {
      case x :: xs1 =>
        ys match {
          case y :: ys1 => if (x ne y) n else firstDiff(xs1, ys1, n + 1)
          case nil => n
        }
      case nil =>
        ys match {
          case y :: ys1 => n
          case nil => -1
        }
    }
    private def sameSeq[T <: Trees.Tree[?]](xs: List[T], ys: List[T]): Boolean = firstDiff(xs, ys) < 0

    /** An argument is safe if it is a pure expression or a default getter call
     *  If all arguments are safe, no reordering is necessary
     */
    def isSafeArg(arg: Tree) =
      isPureExpr(arg)
      || arg.isInstanceOf[RefTree | Apply | TypeApply] && arg.symbol.name.is(DefaultGetterName)

    val result:   Tree = {
      var typedArgs = typedArgBuf.toList
      def app0 = cpy.Apply(app)(normalizedFun, typedArgs) // needs to be a `def` because typedArgs can change later
      val app1 =
        if (!success || typedArgs.exists(_.tpe.isError)) app0.withType(UnspecifiedErrorType)
        else {
          if !sameSeq(args, orderedArgs)
             && !isJavaAnnotConstr(methRef.symbol)
             && !typedArgs.forall(isSafeArg)
          then
            // need to lift arguments to maintain evaluation order in the
            // presence of argument reorderings.

            liftFun()

            // lift arguments in the definition order
            val argDefBuf = mutable.ListBuffer.empty[Tree]
            typedArgs = lifter.liftArgs(argDefBuf, methType, typedArgs)
            // Lifted arguments ordered based on the original order of typedArgBuf and
            // with all non-explicit default parameters at the end in declaration order.
            val orderedArgDefs = {
              // Indices of original typed arguments that are lifted by liftArgs
              val impureArgIndices = typedArgBuf.zipWithIndex.collect {
                case (arg, idx) if !lifter.noLift(arg) => idx
              }
              def position(arg: Trees.Tree[T]) = {
                val i = args.indexOf(arg)
                if (i >= 0) i else orderedArgs.length
              }
              // The original indices of all ordered arguments, as an array
              val originalIndices = orderedArgs.map(position).toArray
              // Assuming stable sorting all non-explicit default parameters will remain in the end with the same order
              val defaultParamIndex = typedArgs.size
              // A map from lifted argument index to the corresponding position in the original argument list
              def originalIndex(n: Int) =
                if (n < originalIndices.length) originalIndices(n) else orderedArgs.length
              scala.util.Sorting.stableSort[(Tree, Int), Int](
                argDefBuf.zip(impureArgIndices), (arg, idx) => originalIndex(idx)).map(_._1)
            }
            liftedDefs.nn ++= orderedArgDefs
          end if
          if (sameSeq(typedArgs, args)) // trick to cut down on tree copying
            typedArgs = args.asInstanceOf[List[Tree]]
          assignType(app0, normalizedFun, typedArgs)
        }
      wrapDefs(liftedDefs, app1)
    }
  }

  /** Subclass of Application for type checking an Apply node with untyped arguments. */
  class ApplyToUntyped(
    app: untpd.Apply, fun: Tree, methRef: TermRef, proto: FunProto,
    resultType: Type)(using Context)
  extends TypedApply(app, fun, methRef, proto.args, resultType, proto.applyKind) {
    def typedArg(arg: untpd.Tree, formal: Type): TypedArg = proto.typedArg(arg, formal)
    def treeToArg(arg: Tree): untpd.Tree = untpd.TypedSplice(arg)
    def typeOfArg(arg: untpd.Tree): Type = proto.typeOfArg(arg)
  }

  /** Subclass of Application for type checking an Apply node with typed arguments. */
  class ApplyToTyped(
    app: untpd.Apply, fun: Tree, methRef: TermRef, args: List[Tree],
    resultType: Type, applyKind: ApplyKind)(using Context)
  extends TypedApply(app, fun, methRef, args, resultType, applyKind) {
    def typedArg(arg: Tree, formal: Type): TypedArg = arg
    def treeToArg(arg: Tree): Tree = arg
    def typeOfArg(arg: Tree): Type = arg.tpe
  }

  /** If `app` is a `this(...)` constructor call, the this-call argument context,
   *  otherwise the current context.
   */
  def argCtx(app: untpd.Tree)(using Context): Context =
    if (ctx.owner.isClassConstructor && untpd.isSelfConstrCall(app)) ctx.thisCallArgContext
    else ctx

  /** Typecheck application. Result could be an `Apply` node,
   *  or, if application is an operator assignment, also an `Assign` or
   *  Block node.
   */
  def typedApply(tree: untpd.Apply, pt: Type)(using Context): Tree = {

    def realApply(using Context): Tree = {
      val resultProto = tree.fun match
        case Select(New(tpt), _) if pt.isInstanceOf[ValueType] =>
          if tpt.isType && typedAheadType(tpt).tpe.typeSymbol.typeParams.isEmpty then
            IgnoredProto(pt)
          else
            pt // Don't ignore expected value types of `new` expressions with parameterized type.
                // If we have a `new C()` with expected type `C[T]` we want to use the type to
                // instantiate `C` immediately. This is necessary since `C` might _also_ have using
                // clauses that we want to instantiate with the best available type. See i15664.scala.
        case _ => IgnoredProto(pt)
          // Do ignore other expected result types, since there might be an implicit conversion
          // on the result. We could drop this if we disallow unrestricted implicit conversions.
      val originalProto =
        new FunProto(tree.args, resultProto)(this, tree.applyKind)(using argCtx(tree))
      record("typedApply")
      val fun1 = typedExpr(tree.fun, originalProto)

      // If adaptation created a tupled dual of `originalProto`, pick the right version
      // (tupled or not) of originalProto to proceed.
      val proto =
        if originalProto.hasTupledDual && needsTupledDual(fun1.tpe, originalProto)
        then originalProto.tupledDual
        else originalProto

      /** Type application where arguments come from prototype, and no implicits are inserted */
      def simpleApply(fun1: Tree, proto: FunProto)(using Context): Tree =
        methPart(fun1).tpe match {
          case funRef: TermRef if funRef.symbol.isSignaturePolymorphic =>
            // synthesize a method type based on the types at the call site.
            // one can imagine the original signature-polymorphic method as
            // being infinitely overloaded, with each individual overload only
            // being brought into existence as needed
            val originalResultType = funRef.symbol.info.resultType.stripNull
            val resultType =
              if !originalResultType.isRef(defn.ObjectClass) then originalResultType
              else AvoidWildcardsMap()(proto.resultType.deepenProtoTrans) match
                case SelectionProto(nme.asInstanceOf_, PolyProto(_, resTp), _, _, _) => resTp
                case resTp if isFullyDefined(resTp, ForceDegree.all) => resTp
                case _ => defn.ObjectType
            val methType = MethodType(proto.typedArgs().map(_.tpe.widen), resultType)
            val fun2 = Applications.retypeSignaturePolymorphicFn(fun1, methType)
            simpleApply(fun2, proto)
          case funRef: TermRef =>
            val app = ApplyTo(tree, fun1, funRef, proto, pt)
            convertNewGenericArray(
              widenEnumCase(
                postProcessByNameArgs(funRef, app).computeNullable(),
                pt))
          case _ =>
            handleUnexpectedFunType(tree, fun1)
        }

      /** Try same application with an implicit inserted around the qualifier of the function
       *  part. Return an optional value to indicate success.
       */
      def tryWithImplicitOnQualifier(fun1: Tree, proto: FunProto)(using Context): Option[Tree] =
        if ctx.mode.is(Mode.SynthesizeExtMethodReceiver) || proto.hasErrorArg then
          // Suppress insertion of apply or implicit conversion on extension method receiver
          // or if argument is erroneous by itself.
          None
        else
          tryInsertImplicitOnQualifier(fun1, proto, ctx.typerState.ownedVars) flatMap { fun2 =>
            tryEither {
              Some(simpleApply(fun2, proto)): Option[Tree]
            } {
              (_, _) => None
            }
          }

      fun1.tpe match {
        case err: ErrorType => cpy.Apply(tree)(fun1, proto.typedArgs()).withType(err)
        case TryDynamicCallType =>
          val isInsertedApply = fun1 match {
            case Select(_, nme.apply) => fun1.span.isSynthetic
            case TypeApply(sel @ Select(_, nme.apply), _) => sel.span.isSynthetic
            /* TODO Get rid of this case. It is still syntax-based, therefore unreliable.
             * It is necessary for things like `someDynamic[T](...)`, because in that case,
             * somehow typedFunPart returns a tree that was typed as `TryDynamicCallType`,
             * so clearly with the view that an apply insertion was necessary, but doesn't
             * actually insert the apply!
             * This is probably something wrong in apply insertion, but I (@sjrd) am out of
             * my depth there.
             * In the meantime, this makes tests pass.
             */
            case TypeApply(fun, _) => !fun.isInstanceOf[Select]
            case _ => false
          }
          val tree1 = fun1 match
            case Select(_, nme.apply) => tree
            case _ => untpd.Apply(fun1, tree.args)
          typedDynamicApply(tree1, isInsertedApply, pt)
        case _ =>
          if (originalProto.isDropped) fun1
          else if (fun1.symbol == defn.Compiletime_summonFrom)
            // Special handling of `summonFrom { ... }`.
            // We currently cannot use a macro for that since unlike other inline methods
            // summonFrom needs to expand lazily. For instance, in
            //
            //    summonFrom {
            //      case given A[t] =>
            //        summonFrom
            //          case given `t` => ...
            //        }
            //    }
            //
            // the second `summonFrom` should expand only once the first `summonFrom` is
            // evaluated and `t` is bound. But normal inline expansion does not behave that
            // way: arguments to inline function are expanded before the function call.
            // To make this work using regular inlining, we'd need a way to annotate
            // an inline function that it should expand only if there are no enclosing
            // applications of inline functions.
            tree.args match {
              case (arg @ Match(EmptyTree, cases)) :: Nil =>
                cases.foreach {
                  case CaseDef(Typed(_: untpd.Ident, _), _, _) => // OK
                  case CaseDef(Bind(_, Typed(_: untpd.Ident, _)), _, _) => // OK
                  case CaseDef(Ident(name), _, _) if name == nme.WILDCARD => // Ok
                  case CaseDef(pat, _, _) =>
                    report.error(UnexpectedPatternForSummonFrom(pat), pat.srcPos)
                }
                typed(untpd.InlineMatch(EmptyTree, cases).withSpan(tree.span), pt)
              case _ =>
                errorTree(tree, em"argument to summonFrom must be a pattern matching closure")
            }
          else
            tryEither {
              simpleApply(fun1, proto)
            } {
              (failedVal, failedState) =>
                def fail = { failedState.commit(); failedVal }
                // Try once with original prototype and once (if different) with tupled one.
                // The reason we need to try both is that the decision whether to use tupled
                // or not was already taken but might have to be revised when an implicit
                // is inserted on the qualifier.
                tryWithImplicitOnQualifier(fun1, originalProto).getOrElse(
                  if (proto eq originalProto) fail
                  else tryWithImplicitOnQualifier(fun1, proto).getOrElse(fail))
            }
      }
    }

    /** Convert expression like
     *
     *     e += (args)
     *
     *  where the lifted-for-assignment version of e is { val xs = es; e' } to
     *
     *     { val xs = es; e' = e' + args }
     */
    def typedOpAssign(using Context): Tree = {
      val (lhs1, name, rhss) = (tree: @unchecked) match
        case Apply(Select(lhs, name), rhss) => (typedExpr(lhs), name, rhss)
        case Apply(untpd.TypedSplice(Select(lhs1, name)), rhss) => (lhs1, name, rhss)
      val liftedDefs = new mutable.ListBuffer[Tree]
      val lhs2 = untpd.TypedSplice(LiftComplex.liftAssigned(liftedDefs, lhs1))
      val assign = untpd.Assign(lhs2,
          untpd.Apply(untpd.Select(lhs2, name.asSimpleName.dropRight(1)), rhss))
      wrapDefs(liftedDefs, typed(assign))
    }

    val app1 =
      if (untpd.isOpAssign(tree))
        tryEither {
          realApply
        } { (failedVal, failedState) =>
          tryEither {
            typedOpAssign
          } { (_, _) =>
            failedState.commit()
            failedVal
          }
        }
      else {
        val app = tree.fun match
          case _: untpd.SplicePattern => typedAppliedSplice(tree, pt)
          case _ => realApply
        app match {
          case Apply(fn @ Select(left, _), right :: Nil) if fn.hasType =>
            val op = fn.symbol
            if (op == defn.Any_== || op == defn.Any_!=)
              checkCanEqual(left.tpe.widen, right.tpe.widen, app.span)
          case _ =>
        }
        app
      }
    app1 match {
      case Apply(Block(stats, fn), args) =>
        tpd.cpy.Block(app1)(stats, tpd.cpy.Apply(app1)(fn, args))
      case _ =>
        app1
    }
  }

  /** Typecheck an Apply node with a typed function and possibly-typed arguments coming from `proto` */
  def ApplyTo(app: untpd.Apply, fun: tpd.Tree, methRef: TermRef, proto: FunProto, resultType: Type)(using Context): tpd.Tree =
    val typer = ctx.typer
    if (proto.allArgTypesAreCurrent())
      typer.ApplyToTyped(app, fun, methRef, proto.typedArgs(), resultType, proto.applyKind).result
    else
      typer.ApplyToUntyped(app, fun, methRef, proto, resultType)(
        using fun.nullableInArgContext(using argCtx(app))).result

  /** Overridden in ReTyper to handle primitive operations that can be generated after erasure */
  protected def handleUnexpectedFunType(tree: untpd.Apply, fun: Tree)(using Context): Tree =
    if ctx.reporter.errorsReported then
      throw TypeError(em"unexpected function type: ${methPart(fun).tpe}")
    else
      throw Error(i"unexpected type.\n  fun = $fun,\n  methPart(fun) = ${methPart(fun)},\n  methPart(fun).tpe = ${methPart(fun).tpe},\n  tpe = ${fun.tpe}")

  def typedNamedArgs(args: List[untpd.Tree])(using Context): List[NamedArg] =
    for (case arg @ NamedArg(id, argtpt) <- args) yield {
      if !Feature.namedTypeArgsEnabled then
        report.error(
          em"""Named type arguments are experimental,
              |they must be enabled with a `experimental.namedTypeArguments` language import or setting""",
          arg.srcPos)
      val argtpt1 = typedType(argtpt)
      cpy.NamedArg(arg)(id, argtpt1).withType(argtpt1.tpe)
    }

  def typedTypeApply(tree: untpd.TypeApply, pt: Type)(using Context): Tree = {
    if (ctx.mode.is(Mode.Pattern))
      return errorTree(tree, em"invalid pattern")

    val isNamed = hasNamedArg(tree.args)
    val typedArgs = if (isNamed) typedNamedArgs(tree.args) else tree.args.mapconserve(typedType(_))
    record("typedTypeApply")
    typedExpr(tree.fun, PolyProto(typedArgs, pt)) match {
      case fun: TypeApply if !ctx.isAfterTyper =>
        val function = fun.fun
        val args = (fun.args ++ tree.args).map(_.show).mkString(", ")
        errorTree(tree, em"""illegal repeated type application
                            |You might have meant something like:
                            |${function}[${args}]""")
      case typedFn =>
        typedFn.tpe.widen match {
          case pt: PolyType =>
            if (typedArgs.length <= pt.paramInfos.length && !isNamed)
              if (typedFn.symbol == defn.Predef_classOf && typedArgs.nonEmpty) {
                val arg = typedArgs.head
                if (!arg.symbol.is(Module)) // Allow `classOf[Foo.type]` if `Foo` is an object
                  checkClassType(arg.tpe, arg.srcPos, traitReq = false, stablePrefixReq = false)
              }
          case _ =>
        }
        def tryDynamicTypeApply(): Tree = typedFn match {
          case typedFn: Select if !pt.isInstanceOf[FunProto] => typedDynamicSelect(typedFn, typedArgs.map(untpd.TypedSplice(_)), pt)
          case _                                             => tree.withType(TryDynamicCallType)
        }
        if (typedFn.tpe eq TryDynamicCallType) tryDynamicTypeApply()
        else assignType(cpy.TypeApply(tree)(typedFn, typedArgs), typedFn, typedArgs)
    }
  }

  /** Rewrite `new Array[T](....)` if T is an unbounded generic to calls to newGenericArray.
   *  It is performed during typer as creation of generic arrays needs a classTag.
   *  we rely on implicit search to find one.
   */
  def convertNewGenericArray(tree: Tree)(using Context): Tree = tree match {
    case Apply(TypeApply(tycon, targs@(targ :: Nil)), args) if tycon.symbol == defn.ArrayConstructor =>
      fullyDefinedType(tree.tpe, "array", tree.srcPos)

      def newGenericArrayCall =
        ref(defn.DottyArraysModule)
          .select(defn.newGenericArrayMethod).withSpan(tree.span)
          .appliedToTypeTrees(targs).appliedToTermArgs(args)

      if (TypeErasure.isGeneric(targ.tpe))
        newGenericArrayCall
      else tree
    case _ =>
      tree
  }

  /** Is `tp` a unary function type or an overloaded type with with only unary function
   *  types as alternatives?
   */
  def isUnary(tp: Type)(using Context): Boolean = tp match {
    case tp: MethodicType =>
      tp.firstParamTypes match {
        case ptype :: Nil => !ptype.isRepeatedParam
        case _ => false
      }
    case tp: TermRef =>
      tp.denot.alternatives.forall(alt => isUnary(alt.info))
    case _ =>
      false
  }

  /** Should we tuple or untuple the argument before application?
    *  If auto-tupling is enabled then
    *
    *   - we tuple n-ary arguments where n > 0 if the function consists
    *     only of unary alternatives
    *   - we untuple tuple arguments of infix operations if the function
    *     does not consist only of unary alternatives.
    */
  def needsTupledDual(funType: Type, pt: FunProto)(using Context): Boolean =
    pt.args match
      case untpd.Tuple(elems) :: Nil =>
        elems.length > 1
        && pt.applyKind == ApplyKind.InfixTuple
        && !isUnary(funType)
      case args =>
        args.lengthCompare(1) > 0
        && isUnary(funType)
        && Feature.autoTuplingEnabled

  /** If `tree` is a complete application of a compiler-generated `apply`
   *  or `copy` method of an enum case, widen its type to the underlying
   *  type by means of a type ascription, as long as the widened type is
   *  still compatible with the expected type.
   *  The underlying type is the intersection of all class parents of the
   *  original type.
   */
  def widenEnumCase(tree: Tree, pt: Type)(using Context): Tree =
    val sym = tree.symbol
    def isEnumCopy = sym.name == nme.copy && sym.owner.isEnumCase
    def isEnumApply = sym.name == nme.apply && sym.owner.linkedClass.isEnumCase
    if sym.is(Synthetic) && (isEnumApply || isEnumCopy)
       && tree.tpe.classSymbol.isEnumCase
       && tree.tpe.widen.isValueType
    then
      val widened = TypeComparer.dropTransparentTraits(
        tree.tpe.parents.reduceLeft(TypeComparer.andType(_, _)),
        pt)
      if widened <:< pt then Typed(tree, TypeTree(widened))
      else tree
    else tree

  /** Does `state` contain a  "NotAMember" or "MissingIdent" message as
   *  first pending error message? That message would be
   *  `$memberName is not a member of ...` or `Not found: $memberName`.
   *  If memberName is empty, any name will do.
   */
  def saysNotFound(state: TyperState, memberName: Name)(using Context): Boolean =
    state.reporter.pendingMessages match
      case dia :: _ =>
        dia.msg match
          case msg: NotFoundMsg => memberName.isEmpty || msg.name == memberName
          case _ => false
      case _ => false

  def typedUnApply(tree: untpd.Apply, selType: Type)(using Context): Tree = {
    record("typedUnApply")
    val Apply(qual, args) = tree

    def notAnExtractor(tree: Tree): Tree =
      // prefer inner errors
      // e.g. report not found ident instead of not an extractor in tests/neg/i2950.scala
      if (!tree.tpe.isError && tree.tpe.isErroneous) tree
      else errorTree(tree, NotAnExtractor(qual))

    /** Report errors buffered in state.
     *  @pre state has errors to report
     *  If the last reported error states that "unapply" is not a member, report
     *  the more informative `NotAnExtractor` message instead.
     *  If the last reported error states that the qualifier was not found, report
     *  the more informative `ExtractorNotFound` message instead.
     */
    def reportErrors(tree: Tree, state: TyperState): Tree =
      assert(state.reporter.hasErrors)
      if saysNotFound(state, nme.unapply) then
        notAnExtractor(tree)
      else qual match
        case qual: Ident if saysNotFound(state, qual.name) =>
          report.error(ExtractorNotFound(qual.name), tree.srcPos)
          tree
        case _ =>
          state.reporter.flush()
          tree

    /** If this is a term ref tree, try to typecheck with its type name.
     *  If this refers to a type alias, follow the alias, and if
     *  one finds a class, reference the class companion module.
     */
    def followTypeAlias(tree: untpd.Tree): untpd.Tree = {
      tree match {
        case tree: untpd.RefTree =>
          val nestedCtx = ctx.fresh.setNewTyperState()
          val ttree =
            typedType(untpd.rename(tree, tree.name.toTypeName))(using nestedCtx)
          ttree.tpe match {
            case alias: TypeRef if alias.info.isTypeAlias && !nestedCtx.reporter.hasErrors =>
              Inferencing.companionRef(alias) match {
                case companion: TermRef => return untpd.ref(companion).withSpan(tree.span)
                case _ =>
              }
            case _ =>
          }
        case _ =>
      }
      untpd.EmptyTree
    }

    /** A typed qual.unapply or qual.unapplySeq tree, if this typechecks.
     *  Otherwise fallBack with (maltyped) qual.unapply as argument
     *  Note: requires special handling for overloaded occurrences of
     *  unapply or unapplySeq. We first try to find a non-overloaded
     *  method which matches any type. If that fails, we try to find an
     *  overloaded variant which matches one of the argument types.
     *  In fact, overloaded unapply's are problematic because a non-
     *  overloaded unapply does *not* need to be applicable to its argument
     *  whereas overloaded variants need to have a conforming variant.
     */
    def trySelectUnapply(qual: untpd.Tree)(fallBack: (Tree, TyperState) => Tree): Tree = {
      // try first for non-overloaded, then for overloaded occurrences
      def tryWithName(name: TermName)(fallBack: (Tree, TyperState) => Tree)(using Context): Tree =

        def tryWithProto(qual: untpd.Tree, targs: List[Tree], pt: Type)(using Context) =
          val proto = UnapplyFunProto(pt, this)
          val unapp = untpd.Select(qual, name)
          val result =
            if targs.isEmpty then typedExpr(unapp, proto)
            else typedExpr(unapp, PolyProto(targs, proto)).appliedToTypeTrees(targs)
          if !result.symbol.exists
             || result.symbol.name == name
             || ctx.reporter.hasErrors
          then result
          else notAnExtractor(result)
          	// It might be that the result of typedExpr is an `apply` selection or implicit conversion.
          	// Reject in this case.

        def tryWithTypeArgs(qual: untpd.Tree, targs: List[Tree])(fallBack: (Tree, TyperState) => Tree): Tree =
          tryEither {
            tryWithProto(qual, targs, selType)
          } {
            (sel, state) =>
              tryEither {
                tryWithProto(qual, targs, WildcardType)
              } {
                (_, _) => fallBack(sel, state)
              }
          }

        qual match
          case TypeApply(qual1, targs) =>
            tryWithTypeArgs(qual1, targs.mapconserve(typedType(_)))((t, ts) =>
              tryWithTypeArgs(qual, Nil)(fallBack))
          case _ =>
            tryWithTypeArgs(qual, Nil)(fallBack)
      end tryWithName

      // try first for unapply, then for unapplySeq
      tryWithName(nme.unapply) {
        (sel, state) =>
          tryWithName(nme.unapplySeq) {
            (sel2, state2) =>
              // if both fail, return unapply error, unless that is simply a
              // "not a member", and the unapplySeq error is more refined.
              if saysNotFound(state, nme.unapply) && !saysNotFound(state2, nme.unapplySeq)
              then fallBack(sel2, state2)
              else fallBack(sel, state)
          }
      }
    }

    /** Produce a typed qual.unapply or qual.unapplySeq tree, or
     *  else if this fails follow a type alias and try again.
     */
    var unapplyFn =
      trySelectUnapply(qual) {
        (sel, state) =>
          val qual1 = followTypeAlias(qual)
          if (qual1.isEmpty) reportErrors(sel, state)
          else trySelectUnapply(qual1) {
            (_, state) => reportErrors(sel, state)
          }
      }

    /** Inlines the unapply function before the dummy argument
     *
     *  A call `P.unapply[...](using l1, ..)(`dummy`)(using t1, ..)` becomes
     *  ```
     *  {
     *    class $anon {
     *      def unapply(s: S)(using t1: T1, ..): R =
     *        ... // inlined code for: P.unapply[...](using l1, ..)(s)(using t1, ..)
     *    }
     *    new $anon
     *  }.unapply(`dummy`)(using t1, ..)
     *  ```
     */
    def inlinedUnapplyFnAndApp(dummyArg: Tree, unapplyAppCall: Tree): (Tree, Tree) =
      def rec(unapp: Tree): (Tree, Tree) =
        unapp match
          case DynamicUnapply(_) =>
            report.error(em"Structural unapply is not supported", unapplyFn.srcPos)
            (unapplyFn, unapplyAppCall)
          case Apply(fn, `dummyArg` :: Nil) =>
            val inlinedUnapplyFn = Inlines.inlinedUnapplyFun(fn)
            (inlinedUnapplyFn, inlinedUnapplyFn.appliedToArgs(`dummyArg` :: Nil))
          case Apply(fn, args) =>
            val (fn1, app) = rec(fn)
            (fn1, tpd.cpy.Apply(unapp)(app, args))

      if unapplyAppCall.symbol.isAllOf(Transparent | Inline) then rec(unapplyAppCall)
      else (unapplyFn, unapplyAppCall)
    end inlinedUnapplyFnAndApp

    def unapplyImplicits(dummyArg: Tree, unapp: Tree): List[Tree] =
      val res = List.newBuilder[Tree]
      def loop(unapp: Tree): Unit = unapp match
        case Apply(Apply(unapply, `dummyArg` :: Nil), args2) => assert(args2.nonEmpty); res ++= args2
        case Apply(unapply, `dummyArg` :: Nil) =>
        case Inlined(u, _, _) => loop(u)
        case DynamicUnapply(_) => report.error(em"Structural unapply is not supported", unapplyFn.srcPos)
        case Apply(fn, args) => assert(args.nonEmpty); loop(fn); res ++= args
        case _ => ().assertingErrorsReported

      loop(unapp)
      res.result()
    end unapplyImplicits

    /** Add a `Bind` node for each `bound` symbol in a type application `unapp` */
    def addBinders(unapp: Tree, bound: List[Symbol]) = unapp match {
      case TypeApply(fn, args) =>
        var remain = bound.toSet
        def addBinder(arg: Tree) = arg.tpe.stripTypeVar match {
          case ref: TypeRef if remain.contains(ref.symbol) =>
            remain -= ref.symbol
            tpd.Bind(ref.symbol, Ident(ref))
          case _ =>
            arg
        }
        tpd.cpy.TypeApply(unapp)(fn, args.mapConserve(addBinder))
      case _ =>
        unapp
    }

    unapplyFn.tpe.widen match {
      case mt: MethodType if mt.paramInfos.length == 1 =>
        val unapplyArgType = mt.paramInfos.head
        unapp.println(i"unapp arg tpe = $unapplyArgType, pt = $selType")
        val ownType =
          if selType <:< unapplyArgType then
            unapp.println(i"case 1 $unapplyArgType ${ctx.typerState.constraint}")
            fullyDefinedType(unapplyArgType, "pattern selector", tree.srcPos)
            selType.dropAnnot(defn.UncheckedAnnot) // need to drop @unchecked. Just because the selector is @unchecked, the pattern isn't.
          else
            if !ctx.mode.is(Mode.InTypeTest) then
              checkMatchable(selType, tree.srcPos, pattern = true)
            // We ignore whether constraining the pattern succeeded.
            // Constraining only fails if the pattern cannot possibly match,
            // but useless pattern checks detect more such cases, so we simply rely on them instead.
            withMode(Mode.GadtConstraintInference)(TypeComparer.constrainPatternType(unapplyArgType, selType))
            val patternBound = maximizeType(unapplyArgType, unapplyFn.span.endPos)
            if (patternBound.nonEmpty) unapplyFn = addBinders(unapplyFn, patternBound)
            unapp.println(i"case 2 $unapplyArgType ${ctx.typerState.constraint}")
            unapplyArgType

        val dummyArg = dummyTreeOfType(ownType)
        val (newUnapplyFn, unapplyApp) =
          val unapplyAppCall =
            typedExpr(untpd.TypedSplice(Apply(unapplyFn, dummyArg :: Nil)))
          inlinedUnapplyFnAndApp(dummyArg, unapplyAppCall)

        var argTypes = unapplyArgs(unapplyApp.tpe, unapplyFn, args, tree.srcPos)
        for (argType <- argTypes) assert(!isBounds(argType), unapplyApp.tpe.show)
        val bunchedArgs = argTypes match {
          case argType :: Nil =>
            if (args.lengthCompare(1) > 0 && Feature.autoTuplingEnabled && defn.isTupleNType(argType)) untpd.Tuple(args) :: Nil
            else args
          case _ => args
        }
        if (argTypes.length != bunchedArgs.length) {
          report.error(UnapplyInvalidNumberOfArguments(qual, argTypes), tree.srcPos)
          argTypes = argTypes.take(args.length) ++
            List.fill(argTypes.length - args.length)(WildcardType)
        }
        val unapplyPatterns = bunchedArgs.lazyZip(argTypes) map (typed(_, _))
        val result = assignType(cpy.UnApply(tree)(newUnapplyFn, unapplyImplicits(dummyArg, unapplyApp), unapplyPatterns), ownType)
        unapp.println(s"unapply patterns = $unapplyPatterns")
        if (ownType.stripped eq selType.stripped) || ownType.isError then result
        else tryWithTypeTest(Typed(result, TypeTree(ownType)), selType)
      case tp =>
        val unapplyErr = if (tp.isError) unapplyFn else notAnExtractor(unapplyFn)
        val typedArgsErr = args mapconserve (typed(_, defn.AnyType))
        cpy.UnApply(tree)(unapplyErr, Nil, typedArgsErr) withType unapplyErr.tpe
    }
  }

  /** A typed unapply hook, can be overridden by re any-typers between frontend
   *  and pattern matcher.
   */
  def typedUnApply(tree: untpd.UnApply, selType: Type)(using Context): UnApply =
    throw new UnsupportedOperationException("cannot type check an UnApply node")

  /** Is given method reference applicable to argument trees `args`?
   *  @param  resultType   The expected result type of the application
   */
  def isApplicableMethodRef(methRef: TermRef, args: List[Tree], resultType: Type, keepConstraint: Boolean, argMatch: ArgMatch)(using Context): Boolean = {
    def isApp(using Context): Boolean =
      new ApplicableToTrees(methRef, args, resultType, argMatch).success
    if (keepConstraint) isApp else explore(isApp)
  }

  /** Is given method reference applicable to argument types `args`?
   *  @param  resultType   The expected result type of the application
   */
  def isApplicableMethodRef(methRef: TermRef, args: List[Type], resultType: Type, argMatch: ArgMatch)(using Context): Boolean =
    explore(new ApplicableToTypes(methRef, args, resultType, argMatch).success)

  /** Is given type applicable to argument trees `args`, possibly after inserting an `apply`?
   *  @param  resultType   The expected result type of the application
   */
  def isApplicableType(tp: Type, args: List[Tree], resultType: Type, keepConstraint: Boolean)(using Context): Boolean =
    onMethod(tp, args.nonEmpty) {
      isApplicableMethodRef(_, args, resultType, keepConstraint, ArgMatch.Compatible)
    }

  /** Is given type applicable to argument types `args`, possibly after inserting an `apply`?
   *  @param  resultType   The expected result type of the application
   */
  def isApplicableType(tp: Type, args: List[Type], resultType: Type)(using Context): Boolean =
    onMethod(tp, args.nonEmpty) {
      isApplicableMethodRef(_, args, resultType, ArgMatch.Compatible)
    }

  private def onMethod(tp: Type, followApply: Boolean)(p: TermRef => Boolean)(using Context): Boolean = tp match {
    case methRef: TermRef if methRef.widenSingleton.isInstanceOf[MethodicType] =>
      p(methRef)
    case mt: MethodicType =>
      p(mt.narrow)
    case _ =>
      followApply && tp.member(nme.apply).hasAltWith(d => p(TermRef(tp, nme.apply, d)))
  }

  /** Does `tp` have an extension method named `xname` with this-argument `argType` and
   *  result matching `resultType`?
   */
  def hasExtensionMethodNamed(tp: Type, xname: TermName, argType: Type, resultType: Type)(using Context) = {
    def qualifies(mbr: Denotation) =
      mbr.exists
      && isApplicableType(
            normalize(tp.select(xname, mbr), WildcardType),
            argType :: Nil, resultType)
    tp.memberBasedOnFlags(xname, required = ExtensionMethod).hasAltWithInline(qualifies)
  }

  /** Drop any leading type or implicit parameter sections */
  def stripInferrable(tp: Type)(using Context): Type = tp match {
    case mt: MethodType if mt.isImplicitMethod =>
      stripInferrable(resultTypeApprox(mt))
    case pt: PolyType =>
      stripInferrable(pt.resType)
    case _ =>
      tp
  }

  /** Drop any leading implicit parameter sections */
  def stripImplicit(tp: Type, wildcardOnly: Boolean = false)(using Context): Type = tp match {
    case mt: MethodType if mt.isImplicitMethod =>
      stripImplicit(resultTypeApprox(mt, wildcardOnly))
    case pt: PolyType =>
      pt.derivedLambdaType(pt.paramNames, pt.paramInfos,
          stripImplicit(pt.resultType, wildcardOnly = true))
            // can't use TypeParamRefs for parameter references in `resultTypeApprox`
            // since their bounds can refer to type parameters in `pt` that are not
            // bound by the constraint. This can lead to hygiene violations if subsequently
            // `pt` itself is added to the constraint. Test case is run/enrich-gentraversable.scala.
        .asInstanceOf[PolyType].flatten
    case _ =>
      tp
  }

  /** Compare owner inheritance level.
    *  @param    sym1 The first owner
    *  @param    sym2 The second owner
    *  @return    1   if `sym1` properly derives from `sym2`
    *            -1   if `sym2` properly derives from `sym1`
    *             0   otherwise
    *  Module classes also inherit the relationship from their companions. This means,
    *  if no direct derivation exists between `sym1` and `sym2` also perform the following
    *  tests:
    *   - If both sym1 and sym1 are module classes that have companion classes,
    *     and sym2 does not inherit implicit members from a base class (#),
    *     compare the companion classes.
    *   - If sym1 is a module class with a companion, and sym2 is a normal class or trait,
    *     compare the companion with sym2.
    *
    *  Condition (#) is necessary to make `compareOwner(_, _) > 0` a transitive relation.
    *  For instance:
    *
    *    class A extends B
    *    object A { given a ... }
    *    class B
    *    object B extends C { given b ... }
    *    class C { given c }
    *
    *  Then without (#), and taking A$ for the module class of A,
    *  compareOwner(A$, B$) = 1 and compareOwner(B$, C) == 1,
    *  but compareOwner(A$, C) == 0.
    *  Violating transitivity in this way is bad, since it makes implicit search
    *  outcomes compilation order dependent. E.g. if we compare `b` with `c`
    *  first, we pick `b`. Then, if we compare `a` and `b`, we pick `a` as
    *  solution of the search. But if we start with comparing `a` with `c`,
    *  we get an ambiguity.
    *
    *  With the added condition (#), compareOwner(A$, B$) == 0.
    *  This means we get an ambiguity between `a` and `b` in all cases.
    */
  def compareOwner(sym1: Symbol, sym2: Symbol)(using Context): Int =
    if sym1 == sym2 then 0
    else if sym1.isSubClass(sym2) then 1
    else if sym2.isSubClass(sym1) then -1
    else if sym1.is(Module) then
      val cls1 = sym1.companionClass
      if sym2.is(Module) then
        if sym2.thisType.implicitMembers.forall(_.symbol.owner == sym2) then // test for (#)
          compareOwner(cls1, sym2.companionClass)
        else 0
      else compareOwner(cls1, sym2)
    else 0

  /** Compare two alternatives of an overloaded call or an implicit search.
   *
   *  @param  alt1, alt2      Non-overloaded references indicating the two choices
   *  @param  preferGeneral   When comparing two value types, prefer the more general one
   *                          over the more specific one iff `preferGeneral` is true.
   *                          `preferGeneral` is set to `true` when we compare two given values, since
   *                          then we want the most general evidence that matches the target
   *                          type. It is set to `false` for overloading resolution, when we want the
   *                          most specific type instead.
   *  @return  1   if 1st alternative is preferred over 2nd
   *          -1   if 2nd alternative is preferred over 1st
   *           0   if neither alternative is preferred over the other
   *
   *  Normal symbols are always preferred over constructor proxies. Otherwise,
   *  an alternative A1 is preferred over an alternative A2 if it wins in a tournament
   *  that awards one point for each of the following:
   *
   *   - A1's owner derives from A2's owner.
   *   - A1's type is more specific than A2's type.
   *
   *  If that tournament yields a draw, a tiebreak is applied where
   *  an alternative that takes more implicit parameters wins over one
   *  that takes fewer.
   */
  def compare(alt1: TermRef, alt2: TermRef, preferGeneral: Boolean = false)(using Context): Int = trace(i"compare($alt1, $alt2)", overload) {
    record("resolveOverloaded.compare")

    val compareGivens = alt1.symbol.is(Given) || alt2.symbol.is(Given)

    /** Is alternative `alt1` with type `tp1` as good as alternative
     *  `alt2` with type `tp2` ?
     *
     *    1. A method `alt1` of type `(p1: T1, ..., pn: Tn)U` is as good as `alt2`
     *       if `alt1` is nullary or `alt2` is applicable to arguments (p1, ..., pn) of
     *       types T1,...,Tn. If the last parameter `pn` has a vararg type T*, then
     *       `alt1` must be applicable to arbitrary numbers of `T` parameters (which
     *       implies that it must be a varargs method as well).
     *    2. A polymorphic member of type [a1 >: L1 <: U1, ..., an >: Ln <: Un]T is as
     *       good as `alt2` of type `tp2` if T is as good as `tp2` under the
     *       assumption that for i = 1,...,n each ai is an abstract type name bounded
     *       from below by Li and from above by Ui.
     *    3. A member of any other type `tp1` is:
     *       a. always as good as a method or a polymorphic method.
     *       b. as good as a member of any other type `tp2` is `asGoodValueType(tp1, tp2) = true`
     */
    def isAsGood(alt1: TermRef, tp1: Type, alt2: TermRef, tp2: Type): Boolean = trace(i"isAsSpecific $tp1 $tp2", overload) {
      tp1 match
        case tp1: MethodType => // (1)
          tp1.paramInfos.isEmpty && tp2.isInstanceOf[LambdaType]
          || {
            if tp1.isVarArgsMethod then
              tp2.isVarArgsMethod
              && isApplicableMethodRef(alt2, tp1.paramInfos.map(_.repeatedToSingle), WildcardType, ArgMatch.Compatible)
            else
              isApplicableMethodRef(alt2, tp1.paramInfos, WildcardType, ArgMatch.Compatible)
          }
        case tp1: PolyType => // (2)
          inContext(ctx.fresh.setExploreTyperState()) {
            // Fully define the PolyType parameters so that the infos of the
            // tparams created below never contain TypeRefs whose underling types
            // contain uninstantiated TypeVars, this could lead to cycles in
            // `isSubType` as a TypeVar might get constrained by a TypeRef it's
            // part of.
            val tp1Params = tp1.newLikeThis(tp1.paramNames, tp1.paramInfos, defn.AnyType)
            fullyDefinedType(tp1Params, "type parameters of alternative", alt1.symbol.srcPos)

            val tparams = newTypeParams(alt1.symbol, tp1.paramNames, EmptyFlags, tp1.instantiateParamInfos(_))
            isAsGood(alt1, tp1.instantiate(tparams.map(_.typeRef)), alt2, tp2)
          }
        case _ => // (3)
          tp2 match
            case tp2: MethodType => true // (3a)
            case tp2: PolyType if tp2.resultType.isInstanceOf[MethodType] => true // (3a)
            case tp2: PolyType => // (3b)
              explore(isAsGoodValueType(tp1, instantiateWithTypeVars(tp2)))
            case _ => // 3b)
              isAsGoodValueType(tp1, tp2)
    }

    /** Test whether value type `tp1` is as good as value type `tp2`.
     *  Let's abbreviate this to `tp1 <:p tp2`. The behavior depends on the Scala version
     *  and mode.
     *
     *   - In Scala 2, `<:p` was the same as `<:`. This behavior is still
     *     available in 3.0-migration if mode `Mode.OldImplicitResolution` is turned on as well.
     *     It is used to highlight differences between Scala 2 and 3 behavior.
     *
     *   - In Scala 3.0-3.4, the behavior is as follows: `T <:p U` iff there is an impliit conversion
     *     from `T` to `U`, or
     *
     *        flip(T) <: flip(U)
     *
     *     where `flip` changes covariant occurrences of contravariant type parameters to
     *     covariant ones. Intuitively `<:p` means subtyping `<:`, except that all arguments
     *     to contravariant parameters are compared as if they were covariant. E.g. given class
     *
     *         class Cmp[-X]
     *
     *     `Cmp[T] <:p Cmp[U]` if `T <: U`. On the other hand, non-variant occurrences
     *     of parameters are not affected. So `T <: U` would imply `Set[Cmp[U]] <:p Set[Cmp[T]]`,
     *     as usual, because `Set` is non-variant.
     *
     *   - From Scala 3.5, `T <:p U` means `T <: U` or `T` convertible to `U`
     *     for overloading resolution (when `preferGeneral is false), and the opposite relation
     *     `U <: T` or `U convertible to `T` for implicit disambiguation between givens
     *     (when `preferGeneral` is true). For old-style implicit values, the 3.4 behavior is kept.
     *
     *   - In Scala 3.5-migration, use the 3.5 scheme normally, and the 3.4 scheme if
     *     `Mode.OldImplicitResolution` is on. This is used to highlight differences in the
     *     two resolution schemes.
     *
     *  Also and only for given resolution: If a compared type refers to a given or its module class, use
     *  the intersection of its parent classes instead.
     */
    def isAsGoodValueType(tp1: Type, tp2: Type)(using Context) =
      val oldResolution = ctx.mode.is(Mode.OldImplicitResolution)
      if !preferGeneral || Feature.migrateTo3 && oldResolution then
        // Normal specificity test for overloading resolution (where `preferGeneral` is false)
        // and in mode Scala3-migration when we compare with the old Scala 2 rules.
        isCompatible(tp1, tp2)
      else
        def prepare(tp: Type) = tp.stripTypeVar match
          case tp: NamedType if tp.symbol.is(Module) && tp.symbol.sourceModule.is(Given) =>
            tp.widen.widenToParents
          case _ =>
            tp

        val tp1p = prepare(tp1)
        val tp2p = prepare(tp2)

        if Feature.sourceVersion.isAtMost(SourceVersion.`3.4`)
            || oldResolution
            || !compareGivens
        then
          // Intermediate rules: better means specialize, but map all type arguments downwards
          // These are enabled for 3.0-3.4, and for all comparisons between old-style implicits,
          // and in 3.5-migration when we compare with previous rules.
          val flip = new TypeMap:
            def apply(t: Type) = t match
              case t @ AppliedType(tycon, args) =>
                def mapArg(arg: Type, tparam: TypeParamInfo) =
                  if (variance > 0 && tparam.paramVarianceSign < 0) defn.FunctionNOf(arg :: Nil, defn.UnitType)
                  else arg
                mapOver(t.derivedAppliedType(tycon, args.zipWithConserve(tycon.typeParams)(mapArg)))
              case _ => mapOver(t)
          (flip(tp1p) relaxed_<:< flip(tp2p)) || viewExists(tp1, tp2)
        else
          // New rules: better means generalize
          (tp2p relaxed_<:< tp1p) || viewExists(tp2, tp1)
    end isAsGoodValueType

    /** Widen the result type of synthetic given methods from the implementation class to the
     *  type that's implemented. Example
     *
     *      given I[X]: T with { ... }
     *
     *  This desugars to
     *
     *      class I[X] extends T { ... }
     *      implicit def I[X]: I[X] = new I[X]
     *
     *  To compare specificity we should compare with `T`, not with its implementation `I[X]`.
     *  No such widening is performed for given aliases, which are not synthetic. E.g.
     *
     *      given J[X]: T = rhs
     *
     *  already has the right result type `T`. Neither is widening performed for given
     *  objects, since these are anyway taken to be more specific than methods
     *  (by condition 3a above).
     */
    def widenGiven(tp: Type, alt: TermRef): Type = tp match {
      case mt: MethodType if mt.isImplicitMethod =>
        mt.derivedLambdaType(mt.paramNames, mt.paramInfos, widenGiven(mt.resultType, alt))
      case pt: PolyType =>
        pt.derivedLambdaType(pt.paramNames, pt.paramInfos, widenGiven(pt.resultType, alt))
      case rt =>
        if alt.symbol.isCoDefinedGiven(rt.typeSymbol) then tp.widenToParents
        else tp
    }

    def compareWithTypes(tp1: Type, tp2: Type) = {
      val ownerScore = compareOwner(alt1.symbol.maybeOwner, alt2.symbol.maybeOwner)
      val winsType1 = isAsGood(alt1, tp1, alt2, tp2)
      def winsType2 = isAsGood(alt2, tp2, alt1, tp1)

      overload.println(i"compare($alt1, $alt2)? $tp1 $tp2 $ownerScore $winsType1 $winsType2")
      if winsType1 && winsType2
          && alt1.widenExpr.isStable && (alt1.widenExpr frozen_=:= alt2.widenExpr)
      then
        // alternatives are the same after following ExprTypes, pick one of them
        // (prefer the one that is not a method, but that's arbitrary).
        if alt1.widenExpr =:= alt2 then -1 else 1
      else if ownerScore == 1 then
        if winsType1 || !winsType2 then 1 else 0
      else if ownerScore == -1 then
        if winsType2 || !winsType1 then -1 else 0
      else if winsType1 then
        if winsType2 then 0 else 1
      else
        if winsType2 then -1 else 0
    }

    if alt1.symbol.is(ConstructorProxy) && !alt2.symbol.is(ConstructorProxy) then -1
    else if alt2.symbol.is(ConstructorProxy) && !alt1.symbol.is(ConstructorProxy) then 1
    else
      val fullType1 = widenGiven(alt1.widen, alt1)
      val fullType2 = widenGiven(alt2.widen, alt2)
      val strippedType1 = stripImplicit(fullType1)
      val strippedType2 = stripImplicit(fullType2)

      val result = compareWithTypes(strippedType1, strippedType2)
      if (result != 0) result
      else if (strippedType1 eq fullType1)
        if (strippedType2 eq fullType2) 0         // no implicits either side: its' a draw
        else 1                                    // prefer 1st alternative with no implicits
      else if (strippedType2 eq fullType2) -1     // prefer 2nd alternative with no implicits
      else compareWithTypes(fullType1, fullType2) // continue by comparing implicits parameters
  }
  end compare

  def narrowMostSpecific(alts: List[TermRef])(using Context): List[TermRef] = {
    record("narrowMostSpecific")
    alts match {
      case Nil => alts
      case _ :: Nil => alts
      case alt1 :: alt2 :: Nil =>
        compare(alt1, alt2) match {
          case  1 => alt1 :: Nil
          case -1 => alt2 :: Nil
          case  0 => alts
        }
      case alt :: alts1 =>
        def survivors(previous: List[TermRef], alts: List[TermRef]): List[TermRef] = alts match {
          case alt :: alts1 =>
            compare(previous.head, alt) match {
              case  1 => survivors(previous, alts1)
              case -1 => survivors(alt :: previous.tail, alts1)
              case  0 => survivors(alt :: previous, alts1)
            }
          case Nil => previous
        }
        val best :: rest = survivors(alt :: Nil, alts1): @unchecked
        def asGood(alts: List[TermRef]): List[TermRef] = alts match {
          case alt :: alts1 =>
            if (compare(alt, best) < 0) asGood(alts1) else alt :: asGood(alts1)
          case nil =>
            Nil
        }
        best :: asGood(rest)
    }
  }

  /** Resolve overloaded alternative `alts`, given expected type `pt`.
   *  Two trials: First, without implicits or SAM conversions enabled. Then,
   *  if the first finds no eligible candidates, with implicits and SAM conversions enabled.
   */
  def resolveOverloaded(alts: List[TermRef], pt: Type)(using Context): List[TermRef] =
    record("resolveOverloaded")

    /** Is `alt` a method or polytype whose result type after the first value parameter
     *  section conforms to the expected type `resultType`? If `resultType`
     *  is a `IgnoredProto`, pick the underlying type instead.
     */
    def resultConforms(altSym: Symbol, altType: Type, resultType: Type)(using Context): Boolean =
      resultType.revealIgnored match {
        case resultType: ValueType =>
          altType.widen match {
            case tp: PolyType => resultConforms(altSym, instantiateWithTypeVars(tp), resultType)
            case tp: MethodType => constrainResult(altSym, tp.resultType, resultType)
            case _ => true
          }
        case _ => true
      }

    /** If the `chosen` alternative has a result type incompatible with the expected result
     *  type `pt`, run overloading resolution again on all alternatives that do match `pt`.
     *  If the latter succeeds with a single alternative, return it, otherwise
     *  fallback to `chosen`.
     *
     *  Note this order of events is done for speed. One might be tempted to
     *  preselect alternatives by result type. But this is slower, because it discriminates
     *  less. The idea is when searching for a best solution, as is the case in overloading
     *  resolution, we should first try criteria which are cheap and which have a high
     *  probability of pruning the search. result type comparisons are neither cheap nor
     *  do they prune much, on average.
     */
    def adaptByResult(chosen: TermRef, alts: List[TermRef]) = pt match {
      case pt: FunProto if !explore(resultConforms(chosen.symbol, chosen, pt.resultType)) =>
        val conformingAlts = alts.filterConserve(alt =>
          (alt ne chosen) && explore(resultConforms(alt.symbol, alt, pt.resultType)))
        conformingAlts match {
          case Nil => chosen
          case alt2 :: Nil => alt2
          case alts2 =>
            resolveOverloaded(alts2, pt) match {
              case alt2 :: Nil => alt2
              case _ => chosen
            }
        }
      case _ => chosen
    }

    def resolve(alts: List[TermRef]): List[TermRef] =
      pt match
        case pt: FunProto =>
          if pt.applyKind == ApplyKind.Using then
            val alts0 = alts.filterConserve(_.widen.stripPoly.isImplicitMethod)
            if alts0 ne alts then return resolve(alts0)
          else if alts.exists(_.widen.stripPoly.isContextualMethod) then
            return resolveMapped(alts, alt => stripImplicit(alt.widen), pt)
        case _ =>

      var found = withoutMode(Mode.ImplicitsEnabled)(resolveOverloaded1(alts, pt))
      if found.isEmpty && ctx.mode.is(Mode.ImplicitsEnabled) then
        found = resolveOverloaded1(alts, pt)
      found match
        case alt :: Nil => adaptByResult(alt, alts) :: Nil
        case _ => found
    end resolve

    /** Try an apply method, if
     *   - the result is applied to value arguments and alternative is not a method, or
     *   - the result is applied to type arguments and alternative is not polymorphic
     */
    val tryApply: Type => Boolean = alt => pt match {
      case pt: FunProto => !alt.widen.stripPoly.isInstanceOf[MethodType]
      case pt: PolyProto => !alt.widen.isInstanceOf[PolyType]
      case _ => false
    }

    /** Replace each alternative by its apply members where necessary */
    def applyMembers(alt: TermRef): List[TermRef] =
      if (tryApply(alt)) {
        val qual = alt.widen match {
          case pt: PolyType =>
            wildApprox(pt.resultType)
          case _ =>
            alt
        }
        qual.member(nme.apply).alternatives.map(TermRef(alt, nme.apply, _))
      }
      else alt :: Nil

    /** Fall back from an apply method to its original alternative */
    def retract(alt: TermRef): TermRef =
      if (alt.name == nme.apply && !alts.contains(alt))
        alts.find(_.symbol == alt.prefix.termSymbol).getOrElse(alt)
      else alt

    if (alts.exists(tryApply)) {
      val expanded = alts.flatMap(applyMembers)
      resolve(expanded).map(retract)
    }
    else resolve(alts)
  end resolveOverloaded

  /** This private version of `resolveOverloaded` does the bulk of the work of
   *  overloading resolution, but does neither result adaptation nor apply insertion.
   *  It might be called twice from the public `resolveOverloaded` method, once with
   *  implicits and SAM conversions enabled, and once without.
   */
  private def resolveOverloaded1(alts: List[TermRef], pt: Type)(using Context): List[TermRef] =
    trace(i"resolve over $alts%, %, pt = $pt", typr, show = true) {
    record(s"resolveOverloaded1", alts.length)

    def isDetermined(alts: List[TermRef]) = alts.isEmpty || alts.tail.isEmpty

    /** The shape of given tree as a type; cannot handle named arguments. */
    def typeShape(tree: untpd.Tree): Type = tree match {
      case untpd.Function(args, body) =>
        defn.FunctionNOf(
          args.map(Function.const(defn.AnyType)), typeShape(body),
          isContextual = untpd.isContextualClosure(tree))
      case Match(EmptyTree, _) =>
        defn.PartialFunctionClass.typeRef.appliedTo(defn.AnyType :: defn.NothingType :: Nil)
      case _ =>
        defn.NothingType
    }

    /** The shape of given tree as a type; is more expensive than
     *  typeShape but can can handle named arguments.
     */
    def treeShape(tree: untpd.Tree): Tree = tree match {
      case NamedArg(name, arg) =>
        val argShape = treeShape(arg)
        cpy.NamedArg(tree)(name, argShape).withType(argShape.tpe)
      case _ =>
        dummyTreeOfType(typeShape(tree))
    }

    def narrowByTypes(alts: List[TermRef], argTypes: List[Type], resultType: Type): List[TermRef] =
      alts.filterConserve(isApplicableMethodRef(_, argTypes, resultType, ArgMatch.CompatibleCAP))

    /** Normalization steps before checking arguments:
     *
     *                 { expr }   -->   expr
     *    (x1, ..., xn) => expr   -->   ((x1, ..., xn)) => expr
     *       if n != 1, no alternative has a corresponding formal parameter that
     *       is an n-ary function, and at least one alternative has a corresponding
     *       formal parameter that is a unary function.
     */
    def normArg(alts: List[TermRef], arg: untpd.Tree, idx: Int): untpd.Tree = arg match
      case Block(Nil, expr) if !expr.isEmpty => normArg(alts, expr, idx)
      case untpd.Function(args: List[untpd.ValDef] @unchecked, body) =>

        // If ref refers to a method whose parameter at index `idx` is a function type,
        // the arity of that function, otherise -1.
        def paramCount(ref: TermRef) =
          val formals = ref.widen.firstParamTypes
          if formals.length > idx then
            formals(idx).dealias match
              case defn.FunctionNOf(args, _, _) => args.length
              case _ => -1
          else -1

        val numArgs = args.length
        if numArgs != 1
           && !alts.exists(paramCount(_) == numArgs)
           && alts.exists(paramCount(_) == 1)
        then
          desugar.makeTupledFunction(args, body, isGenericTuple = true)
            // `isGenericTuple = true` is the safe choice here. It means the i'th tuple
            // element is selected with `(i)` instead of `_i`, which gives the same code
            // in the end, but the compilation time and the ascribed type are more involved.
            // It also means that -Ytest-pickler -Xprint-types fails for sources exercising
            // the idiom since after pickling the target is known, so _i is used directly.
        else arg
      case _ => arg
    end normArg

    val candidates = pt match {
      case pt @ FunProto(args, resultType) =>
        val numArgs = args.length
        def sizeFits(alt: TermRef): Boolean = alt.widen.stripPoly match {
          case tp: MethodType =>
            val ptypes = tp.paramInfos
            val numParams = ptypes.length
            def isVarArgs = ptypes.nonEmpty && ptypes.last.isRepeatedParam
            def numDefaultParams =
              if alt.symbol.hasDefaultParams then
                val fn = ref(alt, needLoad = false)
                ptypes.indices.count(n => !findDefaultGetter(fn, n, testOnly = true).isEmpty)
              else 0
            if numParams < numArgs then isVarArgs
            else if numParams == numArgs then true
            else
              val numNecessaryArgs = numParams - numDefaultParams
              if numNecessaryArgs <= numArgs then true
              else if numNecessaryArgs == numArgs + 1 then isVarArgs
              else false
          case _ =>
            numArgs == 0
        }

        def narrowBySize(alts: List[TermRef]): List[TermRef] =
          alts.filterConserve(sizeFits(_))

        def narrowByShapes(alts: List[TermRef]): List[TermRef] =
          if args.exists(untpd.isFunctionWithUnknownParamType) then
            val normArgs = args.mapWithIndexConserve(normArg(alts, _, _))
            if hasNamedArg(args) then narrowByTrees(alts, normArgs.map(treeShape), resultType)
            else narrowByTypes(alts, normArgs.map(typeShape), resultType)
          else
            alts

        def narrowByTrees(alts: List[TermRef], args: List[Tree], resultType: Type): List[TermRef] =
          alts.filterConserve(alt =>
            isApplicableMethodRef(alt, args, resultType, keepConstraint = false, ArgMatch.CompatibleCAP)
          )

        record("resolveOverloaded.FunProto", alts.length)
        val alts1 = narrowBySize(alts)
        overload.println(i"narrowed by size: ${alts1.map(_.symbol.showDcl)}%, %")
        if isDetermined(alts1) then alts1
        else
          record("resolveOverloaded.narrowedBySize", alts1.length)
          val alts2 = narrowByShapes(alts1)
          overload.println(i"narrowed by shape: ${alts2.map(_.symbol.showDcl)}%, %")
          if isDetermined(alts2) then alts2
          else
            record("resolveOverloaded.narrowedByShape", alts2.length)
            pretypeArgs(alts2, pt)
            narrowByTrees(alts2, pt.typedArgs(normArg(alts2, _, _)), resultType)

      case pt @ PolyProto(targs1, pt1) =>
        val alts1 = alts.filterConserve(pt.canInstantiate)
        if isDetermined(alts1) then alts1
        else
          def withinBounds(alt: TermRef) = alt.widen match
            case tp: PolyType =>
              TypeOps.boundsViolations(targs1, tp.paramInfos, _.substParams(tp, _), NoType).isEmpty
          val alts2 = alts1.filter(withinBounds)
          if isDetermined(alts2) then alts2
          else resolveMapped(alts1, _.widen.appliedTo(targs1.tpes), pt1)

      case pt =>
        val compat = alts.filterConserve(normalizedCompatible(_, pt, keepConstraint = false))
        if compat.isEmpty then
          pt match
            case SAMType(mtp, _) =>
              // If we have a SAM type as expected type, treat it as if the expression was eta-expanded
              // Note 1: No need to do that for function types, the previous normalizedCompatible test already
              // handles those.
              // Note 2: This case should not be moved to the enclosing match
              // since fSAM types must be considered only if there are no candidates.
              // For example, the second f should be chosen for the following code:
              //    def f(x: String): Unit = ???
              //    def f: java.io.OutputStream = ???
              //    new java.io.ObjectOutputStream(f)
              narrowByTypes(alts, mtp.paramInfos, mtp.resultType)
            case _ =>
              // pick any alternatives that are not methods since these might be convertible
              // to the expected type, or be used as extension method arguments.
              val convertible = alts.filterNot(alt =>
                  normalize(alt, IgnoredProto(pt)).widenSingleton.isInstanceOf[MethodType])
              if convertible.length == 1 then convertible else compat
        else compat
    }

    /** The type of alternative `alt` after instantiating its first parameter
     *  clause with `argTypes`. In addition, if the resulting type is a PolyType
     *  and `typeArgs` matches its parameter list, instantiate the result with `typeArgs`.
     */
    def skipParamClause(argTypes: List[Type], typeArgs: List[Type])(alt: TermRef): Type =
      def skip(tp: Type): Type = tp match {
        case tp: PolyType =>
          skip(tp.resultType) match
            case NoType =>
              NoType
            case rt: PolyType if typeArgs.length == rt.paramInfos.length =>
              tp.derivedLambdaType(resType = rt.instantiate(typeArgs))
            case rt =>
              tp.derivedLambdaType(resType = rt).asInstanceOf[PolyType].flatten
        case tp: MethodType =>
          tp.instantiate(argTypes)
        case _ =>
          NoType
      }
      skip(alt.widen)

    def resultIsMethod(tp: Type): Boolean = tp.widen.stripPoly match
      case tp: MethodType => stripInferrable(tp.resultType).isInstanceOf[MethodType]
      case _ => false

    record("resolveOverloaded.narrowedApplicable", candidates.length)
    if pt.unusableForInference then
      // `pt` might have become erroneous by typing arguments of FunProtos.
      // If `pt` is erroneous, don't try to go further; report the error in `pt` instead.
      candidates
    else
      val found = narrowMostSpecific(candidates)
      if found.length <= 1 then found
      else
        val deepPt = pt.deepenProto
        deepPt match
          case pt @ FunProto(_, PolyProto(targs, resType)) =>
            // try to narrow further with snd argument list and following type params
            resolveMapped(candidates,
              skipParamClause(pt.typedArgs().tpes, targs.tpes), resType)
          case pt @ FunProto(_, resType: FunOrPolyProto) =>
            // try to narrow further with snd argument list
            resolveMapped(candidates,
              skipParamClause(pt.typedArgs().tpes, Nil), resType)
          case _ =>
            // prefer alternatives that need no eta expansion
            val noCurried = alts.filterConserve(!resultIsMethod(_))
            val noCurriedCount = noCurried.length
            if noCurriedCount == 1 then
              noCurried
            else if noCurriedCount > 1 && noCurriedCount < alts.length then
              resolveOverloaded1(noCurried, pt)
            else
              // prefer alternatves that match without default parameters
              val noDefaults = alts.filterConserve(!_.symbol.hasDefaultParams)
              val noDefaultsCount = noDefaults.length
              if noDefaultsCount == 1 then
                noDefaults
              else if noDefaultsCount > 1 && noDefaultsCount < alts.length then
                resolveOverloaded1(noDefaults, pt)
              else if deepPt ne pt then
                // try again with a deeper known expected type
                resolveOverloaded1(alts, deepPt)
              else
                candidates
    }
  end resolveOverloaded1

  /** The largest suffix of `paramss` that has the same first parameter name as `t`,
   *  plus the number of term parameters in `paramss` that come before that suffix.
   */
  def trimParamss(t: Type, paramss: List[List[Symbol]])(using Context): (List[List[Symbol]], Int) = t match
    case MethodType(Nil) => trimParamss(t.resultType, paramss)
    case t: MethodOrPoly =>
      val firstParamName = t.paramNames.head
      def recur(pss: List[List[Symbol]], skipped: Int): (List[List[Symbol]], Int) =
        (pss: @unchecked) match
          case (ps @ (p :: _)) :: pss1 =>
            if p.name == firstParamName then (pss, skipped)
            else recur(pss1, if p.name.isTermName then skipped + ps.length else skipped)
          case Nil =>
            (pss, skipped)
      recur(paramss, 0)
    case _ => (Nil, 0)

  /** Resolve overloading by mapping to a different problem where each alternative's
   *  type is mapped with `f`, alternatives with non-existing types are dropped, and the
   *  expected type is `pt`. Map the results back to the original alternatives.
   */
  def resolveMapped(alts: List[TermRef], f: TermRef => Type, pt: Type)(using Context): List[TermRef] =
    val reverseMapping = alts.flatMap { alt =>
      val t = f(alt)
      if t.exists then
        val (trimmed, skipped) = trimParamss(t.stripPoly, alt.symbol.rawParamss)
        val mappedSym = alt.symbol.asTerm.copy(info = t)
        mappedSym.rawParamss = trimmed
        val (pre, totalSkipped) = mappedAltInfo(alt.symbol) match
          case Some((pre, prevSkipped)) =>
            mappedSym.removeAnnotation(defn.MappedAlternativeAnnot)
            (pre, skipped + prevSkipped)
          case None =>
            (alt.prefix, skipped)
        mappedSym.addAnnotation(
          Annotation(TypeTree(
            defn.MappedAlternativeAnnot.typeRef.appliedTo(
              pre, ConstantType(Constant(totalSkipped))))))
        Some((TermRef(NoPrefix, mappedSym), alt))
      else
        None
    }
    val mapped = reverseMapping.map(_._1)
    overload.println(i"resolve mapped: ${mapped.map(_.widen)}%, % with $pt")
    resolveOverloaded(mapped, pt)(using ctx.retractMode(Mode.SynthesizeExtMethodReceiver))
      .map(reverseMapping.toMap)

  /** Try to typecheck any arguments in `pt` that are function values missing a
   *  parameter type. If the formal parameter types corresponding to a closure argument
   *  all agree on their argument types, typecheck the argument with an expected
   *  function or partial function type that contains these argument types,
   *  The result of the typecheck is stored in `pt`, to be retrieved when its `typedArgs` are selected.
   *  The benefit of doing this is to allow idioms like this:
   *
   *     def map(f: Char => Char): String = ???
   *     def map[U](f: Char => U): Seq[U] = ???
   *     map(x => x.toUpper)
   *
   *  Without `pretypeArgs` we'd get a "missing parameter type" error for `x`.
   *  With `pretypeArgs`, we use the `Char => ?` as the expected type of the
   *  closure `x => x.toUpper`, which makes the code typecheck.
   */
  private def pretypeArgs(alts: List[TermRef], pt: FunProto)(using Context): Unit = {
    def recur(altFormals: List[List[Type]], args: List[untpd.Tree]): Unit = args match {
      case arg :: args1 if !altFormals.exists(_.isEmpty) =>
        def isUniform[T](xs: List[T])(p: (T, T) => Boolean) = xs.forall(p(_, xs.head))
        val formalsForArg: List[Type] = altFormals.map(_.head)
        def argTypesOfFormal(formal: Type): List[Type] =
          formal.dealias match {
            case defn.FunctionOf(args, result, isImplicit) => args
            case defn.PartialFunctionOf(arg, result) => arg :: Nil
            case _ => Nil
          }
        val formalParamTypessForArg: List[List[Type]] =
          formalsForArg.map(argTypesOfFormal)
        if (formalParamTypessForArg.forall(_.nonEmpty) &&
            isUniform(formalParamTypessForArg)((x, y) => x.length == y.length)) {
          val commonParamTypes = formalParamTypessForArg.transpose.map(ps =>
            // Given definitions above, for i = 1,...,m,
            //   ps(i) = List(p_i_1, ..., p_i_n)  -- i.e. a column
            // If all p_i_k's are the same, assume the type as formal parameter
            // type of the i'th parameter of the closure.
            if (isUniform(ps)(_ frozen_=:= _)) ps.head
            else WildcardType)
          /** Should we generate a partial function for the arg ? */
          def isPartial = untpd.functionWithUnknownParamType(arg) match
            case Some(_: untpd.Match) =>
              formalsForArg.exists(_.isRef(defn.PartialFunctionClass))
            case _ =>
              false
          val commonFormal =
            if (isPartial) defn.PartialFunctionOf(commonParamTypes.head, WildcardType)
            else defn.FunctionNOf(commonParamTypes, WildcardType, isContextual = untpd.isContextualClosure(arg))
          overload.println(i"pretype arg $arg with expected type $commonFormal")
          if (commonParamTypes.forall(isFullyDefined(_, ForceDegree.flipBottom)))
            withMode(Mode.ImplicitsEnabled) {
              // We can cache the adapted argument here because the expected type
              // is a common type shared by all overloading candidates.
              pt.cacheArg(arg, pt.typedArg(arg, commonFormal))
            }
        }
        recur(altFormals.map(_.tail), args1)
      case _ =>
    }
    recur(alts.map(_.widen.firstParamTypes), pt.args)
  }

  private def harmonizeWith[T <: AnyRef](ts: List[T])(tpe: T => Type, adapt: (T, Type) => T)(using Context): List[T] = {
    def targetClass(ts: List[T], cls: Symbol, intLitSeen: Boolean): Symbol = ts match {
      case t :: ts1 =>
        tpe(t).widenTermRefExpr match {
          case ConstantType(c: Constant) if c.tag == IntTag =>
            targetClass(ts1, cls, true)
          case t =>
            val sym = t.classSymbol
            if (!sym.isNumericValueClass || cls.exists && cls != sym) NoSymbol
            else targetClass(ts1, sym, intLitSeen)
        }
      case Nil =>
        if (cls != defn.IntClass && intLitSeen) cls else NoSymbol
    }
    val cls = targetClass(ts, NoSymbol, false)
    if (cls.exists) {
      def lossOfPrecision(n: Int): Boolean =
        cls == defn.FloatClass && n.toFloat.toInt != n
      var canAdapt = true
      val ts1 = ts.mapConserve { t =>
        tpe(t).widenTermRefExpr match {
          case ConstantType(c: Constant) if c.tag == IntTag =>
            canAdapt &= c.convertTo(cls.typeRef) != null && !lossOfPrecision(c.intValue)
            if (canAdapt) adapt(t, cls.typeRef) else t
          case _ => t
        }
      }
      if (canAdapt) ts1 else ts
    }
    else ts
  }

  /** If `trees` all have numeric value types, and they do not have all the same type,
   *  pick a common numeric supertype and try to convert all constant Int literals to this type.
   *  If the resulting trees all have the same type, return them instead of the original ones.
   */
  def harmonize(trees: List[Tree])(using Context): List[Tree] = {
    def adaptDeep(tree: Tree, pt: Type): Tree = tree match {
      case cdef: CaseDef => tpd.cpy.CaseDef(cdef)(body = adaptDeep(cdef.body, pt))
      case _ => adapt(tree, pt)
    }
    if (ctx.isAfterTyper) trees else harmonizeWith(trees)(_.tpe, adaptDeep)
  }

  /** Apply a transformation `harmonize` on the results of operation `op`,
   *  unless the expected type `pt` is fully defined.
   *  If the result is different (wrt eq) from the original results of `op`,
   *  revert back to the constraint in force before computing `op`.
   *  This reset is needed because otherwise the original results might
   *  have added constraints to type parameters which are no longer
   *  implied after harmonization. No essential constraints are lost by this because
   *  the result of harmonization will be compared again with the expected type.
   *  Test cases where this matters are in neg/harmomize.scala and run/weak-conformance.scala.
   *
   *  Note: this assumes that the internal typing of the arguments using `op` does
   *  not leave any constraints, so the only info that is reset is the relationship
   *  between the argument's types and the expected type. I am not sure this will
   *  always be the case. If that property does not hold, we risk forgetting constraints
   *  which could lead to unsoundness.
   */
  def harmonic[T](harmonize: List[T] => List[T], pt: Type)(op: => List[T])(using Context): List[T] =
    if (!isFullyDefined(pt, ForceDegree.none)) {
      val origConstraint = ctx.typerState.constraint
      val origElems = op
      val harmonizedElems = harmonize(origElems)
      if (harmonizedElems ne origElems) ctx.typerState.constraint = origConstraint
      harmonizedElems
    }
    else op

  /** If all `types` are numeric value types, and they are not all the same type,
   *  pick a common numeric supertype and widen any constant types in `tpes` to it.
   *  If the resulting types are all the same, return them instead of the original ones.
   */
  private def harmonizeTypes(tpes: List[Type])(using Context): List[Type] =
    harmonizeWith(tpes)(identity, (tp, pt) => pt)

  /** The typed application
   *
   *   <methodRef>(<receiver>)    or
   *   <methodRef>[<type-args>](<receiver>)
   *
   *  where <type-args> comes from `pt` if it is a (possibly ignored) PolyProto.
   */
  def extMethodApply(methodRef: untpd.Tree, receiver: Tree, pt: Type)(using Context): Tree = {
    /** Integrate the type arguments (if any) from `currentPt` into `tree`, and produce
     *  an expected type that hides the appropriate amount of information through IgnoreProto.
     */
    def normalizePt(tree: untpd.Tree, currentPt: Type): (untpd.Tree, Type) = currentPt match
      // Always reveal expected arguments to guide inference (needed for i9509.scala)
      case IgnoredProto(ignored: FunOrPolyProto) =>
        normalizePt(tree, ignored)
      // Always hide expected member to allow for chained extensions (needed for i6900.scala)
      case _: SelectionProto =>
        (tree, IgnoredProto(currentPt))
      case _ =>
        (tree, currentPt)

    val (core, pt1) = normalizePt(methodRef, pt)
    withMode(Mode.SynthesizeExtMethodReceiver) {
      typed(
        untpd.Apply(core, untpd.TypedSplice(receiver, isExtensionReceiver = true) :: Nil),
        pt1, ctx.typerState.ownedVars)
    }
  }

  /** Assuming methodRef is a reference to an extension method defined e.g. as
   *
   *  extension [T1, T2](using A)(using B, C)(receiver: R)(using D)
   *    def foo[T3](using E)(f: F): G = ???
   *
   *  return the tree representing methodRef partially applied to the receiver
   *  and all the implicit parameters preceding it (A, B, C)
   *  with the type parameters of the extension (T1, T2) inferred.
   *  None is returned if the implicit search fails for any of the leading implicit parameters
   *  or if the receiver has a wrong type (note that in general the type of the receiver
   *  might depend on the exact types of the found instances of the proceeding implicits).
   *  No implicit search is tried for implicits following the receiver or for parameters of the def (D, E).
   */
  def tryApplyingExtensionMethod(methodRef: TermRef, receiver: Tree)(using Context): Option[Tree] =
    // Drop all parameters sections of an extension method following the receiver.
    // The return type after truncation is not important
    def truncateExtension(tp: Type)(using Context): Type = tp match
      case poly: PolyType =>
        poly.newLikeThis(poly.paramNames, poly.paramInfos, truncateExtension(poly.resType))
      case meth: MethodType if meth.isContextualMethod =>
        meth.newLikeThis(meth.paramNames, meth.paramInfos, truncateExtension(meth.resType))
      case meth: MethodType =>
        meth.newLikeThis(meth.paramNames, meth.paramInfos, defn.AnyType)

    def replaceCallee(inTree: Tree, replacement: Tree)(using Context): Tree = inTree match
      case Apply(fun, args) => Apply(replaceCallee(fun, replacement), args)
      case TypeApply(fun, args) => TypeApply(replaceCallee(fun, replacement), args)
      case _ => replacement

    val methodRefTree = ref(methodRef, needLoad = false)
    val truncatedSym = methodRef.symbol.asTerm.copy(info = truncateExtension(methodRef.info))
    val truncatedRefTree = untpd.TypedSplice(ref(truncatedSym)).withSpan(receiver.span)
    val newCtx = ctx.fresh.setNewScope.setReporter(new reporting.ThrowingReporter(ctx.reporter))

    try
      val appliedTree = inContext(newCtx) {
        // Introducing an auxiliary symbol in a temporary scope.
        // Entering the symbol indirectly by `newCtx.enter`
        // could instead add the symbol to the enclosing class
        // which could break the REPL.
        newCtx.scope.openForMutations.enter(truncatedSym)
        newCtx.typer.extMethodApply(truncatedRefTree, receiver, WildcardType)
      }
      if appliedTree.tpe.exists && !appliedTree.tpe.isError then
        Some(replaceCallee(appliedTree, methodRefTree))
      else
        None
    catch
      case ex: UnhandledError => None

  def isApplicableExtensionMethod(methodRef: TermRef, receiverType: Type)(using Context): Boolean =
    methodRef.symbol.is(ExtensionMethod) && !receiverType.isBottomType &&
      tryApplyingExtensionMethod(methodRef, nullLiteral.asInstance(receiverType)).nonEmpty

  def captureWildcardsCompat(tp: Type, pt: Type)(using Context): Type =
    val captured = captureWildcards(tp)
    if (captured ne tp) && isCompatible(captured, pt) then captured
    else tp
}
