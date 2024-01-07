package dotty.tools.dotc
package util

import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.ast.Positioned
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.defn

import ast.Trees.*
import ast.tpd
import core.Contexts.*
import core.Denotations.{SingleDenotation, Denotation}
import core.Flags
import core.Names.*
import core.NameKinds
import core.Types.*
import core.Symbols.{NoSymbol, isLocalToBlock}
import interactive.Interactive
import util.Spans.Span
import reporting.*


object Signatures {

  /**
   * Represent a method signature.
   *
   * @param name       The name of the method
   * @param tparams    The type parameters and their bounds
   * @param paramss    The parameter lists of this method
   * @param returnType The return type of this method, if this is not a constructor.
   * @param doc        The documentation for this method.
   * @param denot      The function denotation
   */
  case class Signature(
    name: String,
    paramss: List[List[Param]],
    returnType: Option[String],
    doc: Option[String] = None,
    denot: Option[SingleDenotation] = None
  )

  sealed trait Param:
    def show: String
    val doc: Option[String] = None

  /**
   * Represent a method's parameter.
   *
   * @param name        The name of the parameter
   * @param tpe         The type of the parameter
   * @param doc         The documentation of this parameter
   * @param isImplicit  Is this parameter implicit?
   * @param isReordered Is the parameter reordered in its parameter list?
   */
  case class MethodParam(
    name: String,
    tpe: String,
    override val doc: Option[String] = None,
    isImplicit: Boolean = false,
    isReordered: Boolean = false
  ) extends Param:
    def show: String = if name.nonEmpty && !isReordered then s"$name: $tpe"
    else if name.nonEmpty then s"[$name: $tpe]"
    else tpe

  /**
   * Represent a type parameter.
   *
   * @param tpe         The type of the parameter
   * @param doc         The documentation of this parameter
   */
  case class TypeParam(tpe: String, override val doc: Option[String] = None) extends Param:
    def show = tpe

  /**
   * Extract (current parameter index, function index, functions) method call for given position.
   *
   * @param path The path to the function application
   * @param pos  The position of the cursor
   *
   * @return     A triple containing the index of the parameter being edited, the index of function
   *         being called, the list of overloads of this function).
   */
  def signatureHelp(path: List[tpd.Tree], pos: Span)(using Context): (Int, Int, List[Signature]) =
    computeSignatureHelp(path, pos)

  /**
   * Computes call info (current parameter index, function index, functions) for a method call.
   *
   * @param path The path to the function application
   * @param span The position of the cursor
   *
   * @return A triple containing the index of the parameter being edited, the index of the function
   *         being called, the list of overloads of this function).
   */
  def computeSignatureHelp(path: List[tpd.Tree], span: Span)(using Context): (Int, Int, List[Signature]) =
    findEnclosingApply(path, span) match
      case Apply(fun, params) => applyCallInfo(span, params, fun, false)
      case UnApply(fun, _, patterns) => unapplyCallInfo(span, fun, patterns)
      case appliedTypeTree @ AppliedTypeTree(_, types) => appliedTypeTreeCallInfo(span, appliedTypeTree, types)
      case tp @ TypeApply(fun, types) => applyCallInfo(span, types, fun, isTypeApply = true)
      case _ => (0, 0, Nil)


  def isEnclosingApply(tree: tpd.Tree, span: Span)(using Context): Boolean =
    tree match
      case apply @ Apply(fun, _) => !fun.span.contains(span) && isValid(apply)
      case unapply @ UnApply(fun, _, _) =>
        !fun.span.contains(span) && !ctx.definitions.isFunctionNType(tree.tpe) // we want to show tuples in unapply
      case typeTree @ AppliedTypeTree(fun, _) => !fun.span.contains(span) && isValid(typeTree)
      case typeApply @ TypeApply(fun, _) => !fun.span.contains(span) && isValid(typeApply)
      case _ => false


  /**
   * Finds enclosing application from given `path` for `span`.
   *
   * @param path The path to the function application
   * @param span The position of the cursor
   *
   * @return Tree which encloses closest application containing span.
   *         In case if cursor is pointing on closing parenthesis and
   *         next subsequent application exists, it returns the latter
   */
  private def findEnclosingApply(path: List[tpd.Tree], span: Span)(using Context): tpd.Tree =
    import tpd.TreeOps

    val filteredPath = path.filter:
      case block @ Block(stats, expr) =>
        block.existsSubTree(tree => isEnclosingApply(tree, span) && tree.span.contains(span))
      case other => isEnclosingApply(other, span)

    filteredPath match
      case Nil => tpd.EmptyTree
      case tpd.Block(stats, expr) :: _ => // potential block containing lifted args

        val enclosingFunction = stats.collectFirst:
          case defdef: tpd.DefDef if defdef.rhs.span.contains(span) => defdef

        val enclosingTree = enclosingFunction.getOrElse(expr)
        findEnclosingApply(Interactive.pathTo(enclosingTree, span), span)

      case direct :: _ => direct


  private def isClosingSymbol(ch: Char) = ch == ')' || ch == ']'

  /**
   * Extracts call information for applied type tree:
   *
   * @param types Currently applied function type parameters
   * @param fun   Function tree which is being applied
   */
  private def appliedTypeTreeCallInfo(
    span: Span,
    fun: tpd.Tree,
    types: List[tpd.Tree]
  )(using Context): (Int, Int, List[Signature]) =
    val typeName = fun.symbol.name.show
    val typeParams = fun.symbol.typeRef.typeParams.map(_.paramName.show).map(TypeParam.apply(_))
    val denot = fun.denot.asSingleDenotation
    val activeParameter = findCurrentParamIndex(types, span, typeParams.length - 1)

    val signature = Signature(typeName, List(typeParams), Some(typeName) , None, Some(denot))
    (activeParameter, 0, List(signature))

  private def findOutermostCurriedApply(untpdPath: List[untpd.Tree]): Option[untpd.GenericApply] =
    val trimmedPath = untpdPath.dropWhile(!_.isInstanceOf[untpd.GenericApply])
    val maybeCurriedTrees = (trimmedPath zip trimmedPath.drop(1)).takeWhile:
      case (currentTree: untpd.GenericApply, nextTree: untpd.GenericApply) =>
        nextTree.fun == currentTree
      case _ => false
    .map(_._2)

    maybeCurriedTrees.lastOption.orElse:
      trimmedPath.headOption
    .collect:
      case genericApply: untpd.GenericApply => genericApply

  /**
   * Extracts call information for a function application and type application.
   *
   * @param span        The position of the cursor
   * @param params      Current function parameters
   * @param fun         Function tree which is being applied
   * @param isTypeApply Is a type application
   * @return A triple containing the index of the parameter being edited, the index of the function
   *         being called, the list of overloads of this function).
   */
  private def applyCallInfo(
    span: Span,
    params: List[tpd.Tree],
    fun: tpd.Tree,
    isTypeApply: Boolean = false
  )(using Context): (Int, Int, List[Signature]) =
    def treeQualifier(tree: tpd.Tree): tpd.Tree = tree match
        case Apply(qual, _) => treeQualifier(qual)
        case TypeApply(qual, _) => treeQualifier(qual)
        case AppliedTypeTree(qual, _) => treeQualifier(qual)
        case Select(qual, _) => qual
        case _ => tree

    val paramssListIndex = findParamssIndex(fun)
    val (alternativeIndex, alternatives) = fun.tpe match
      case err: ErrorType =>
        val (alternativeIndex, alternatives) = alternativesFromError(err, params, paramssListIndex) match
          // if we have no alternatives from error, we have to fallback to function denotation
          // Check `partialyFailedCurriedFunctions` test for example
          case (_, Nil) =>
            val denot = fun.denot
            if denot.exists then
              (0, List(denot.asSingleDenotation))
            else
              (0, Nil)
          case other => other
        (alternativeIndex, alternatives)
      case _ =>
        val funSymbol = fun.symbol
        val alternatives = if funSymbol.isLocalToBlock then List(funSymbol.denot) else
          funSymbol.owner.info.member(funSymbol.name).alternatives
        val alternativeIndex = bestAlternative(alternatives, params, paramssListIndex)
        (alternativeIndex, alternatives)

    if alternativeIndex < alternatives.length then
      val alternativeSymbol = alternatives(alternativeIndex).symbol
      val safeParamssListIndex = paramssListIndex min (alternativeSymbol.paramSymss.length - 1)
      val previousArgs = alternativeSymbol.paramSymss.take(safeParamssListIndex).foldLeft(0)(_ + _.length)

      val untpdPath: List[untpd.Tree] = NavigateAST
        .untypedPath(fun, false).collect { case untpdTree: untpd.Tree => untpdTree }

      val untpdArgs = untpdPath match
        case (Ident(_) | Select(_, _)) :: New(_) :: Select(_, name) :: untpd.Apply(untpdFun, args) :: _
          if name.isConstructorName => args
        case _ :: untpd.Apply(_, args) :: _ => args
        case _ :: untpd.TypeApply(_, args) :: _ => args
        case _ => Nil

      val currentParamsIndex =
        findCurrentParamIndex(untpdArgs, span, alternativeSymbol.paramSymss(safeParamssListIndex).length - 1)

      val pre = treeQualifier(fun)
      val alternativesWithTypes = alternatives.map(_.asSeenFrom(pre.tpe.widenTermRefExpr))
      val alternativeSignatures = alternativesWithTypes
        .flatMap(toApplySignature(_, findOutermostCurriedApply(untpdPath), safeParamssListIndex))

      val finalParamIndex = currentParamsIndex + previousArgs
      (finalParamIndex, alternativeIndex, alternativeSignatures)
    else
      (0, 0, Nil)

  /** Finds current parameter index
   *  @param args      List of currently applied arguments
   *  @param span      The position of the cursor
   *  @param maxIndex  The maximum index of the parameter in the current apply list
   *
   *  @return Index of the current parameter
   */
  private def findCurrentParamIndex(args: List[Positioned], span: Span, maxIndex: Int)(using Context): Int =
    (args.indexWhere(_.span.contains(span)) match
      case -1 if args.isEmpty => 0
      case -1 =>
        commaIndex(args, span) match
          // comma is before CURSOR, so we are in parameter b example: test("a", CURSOR)
          case Some(index) if index <= span.end => args.takeWhile(_.span.end < span.start).length
          // comma is after CURSOR, so we are in parameter a example: test("a"   CURSOR,)
          case Some(index) => args.takeWhile(_.span.start < span.end).length - 1
          // we are either in first or last parameter
          case None =>
            if args.head.span.start >= span.end then 0
            else args.length - 1 max 0

      case n => n
    ) min maxIndex

  /** Parser ignores chars between arguments, we have to manually find the index of comma
   *  @param untpdArgs List of applied untyped arguments
   *  @param span      The position of the cursor
   *
   *  @return None if we are in first or last parameter, comma index otherwise
   */
  private def commaIndex(untpdArgs: List[Positioned], span: Span)(using Context): Option[Int] =
    val previousArgIndex = untpdArgs.lastIndexWhere(_.span.end < span.end)
    for
      previousArg <- untpdArgs.lift(previousArgIndex)
      nextArg = untpdArgs.lift(previousArgIndex + 1)
      text = ctx.source.content.slice(previousArg.span.end - 1, nextArg.map(_.span.start).getOrElse(span.end))
      commaIndex = text.indexOf(',')
      if commaIndex != -1
    yield
      commaIndex + previousArg.span.end

  /**
   * Extracts call informatioin for function in unapply context.
   *
   * @param span   The position of the cursor
   * @param params Current function parameters
   * @param fun    Unapply function tree
   *
   * @return A triple containing the index of the parameter being edited, the index of the function
   *         being called, the list of overloads of this function).
   */
  private def unapplyCallInfo(
    span: Span,
    fun: tpd.Tree,
    patterns: List[tpd.Tree]
  )(using Context): (Int, Int, List[Signature]) =
    val resultType = unapplyMethodResult(fun)
    val denot = fun.denot.mapInfo(_ => resultType)

    val paramTypes = extractParamTypess(resultType, denot, patterns.size).flatten.map(stripAllAnnots)
    val paramNames = extractParamNamess(resultType, denot).flatten

    val activeParameter = findCurrentParamIndex(patterns, span, paramTypes.length - 1)
    val unapplySignature = toUnapplySignature(denot.asSingleDenotation, paramNames, paramTypes).toList

    (activeParameter, 0, unapplySignature)


  private def isUnapplySeq(denot: Denotation)(using Context): Boolean =
    denot.name == core.Names.termName("unapplySeq")

  /**
   * Extract parameter names from `resultType` only if `resultType` is case class and `denot` is synthetic.
   *
   * @param resultType Function result type
   * @param denot      Function denotation
   *
   * @return List of lists of names of parameters if `resultType` is case class without overriden unapply
   */
  private def extractParamNamess(resultType: Type, denot: Denotation)(using Context): List[List[Name]] =
    if resultType.typeSymbol.flags.is(Flags.CaseClass) && denot.symbol.flags.is(Flags.Synthetic) then
      resultType.typeSymbol.primaryConstructor.paramInfo.paramNamess
    else
      Nil

  /**
   * Extract parameter types from `resultType` in unapply context.
   *
   * @param resultType   Function result type
   * @param denot        Function denotation
   * @param patternsSize Number of pattern trees present in function tree
   *
   * @return List of lists of types present in unapply clause
   */
  private def extractParamTypess(
    resultType: Type,
    denot: Denotation,
    patternsSize: Int
  )(using Context): List[List[Type]] =
    resultType match
      // unapply(_$1: Any): CustomClass
      case ref: TypeRef if !ref.symbol.isPrimitiveValueClass => mapOptionLessUnapply(ref, patternsSize, isUnapplySeq(denot))
      // unapply(_$1: Any): Option[T[_]]
      case AppliedType(TypeRef(_, cls), (appliedType @ AppliedType(tycon, args)) :: Nil)
          if (cls == ctx.definitions.OptionClass || cls == ctx.definitions.SomeClass) =>
        tycon match
          // unapply[T](_$1: Any): Option[(T1, T2 ... Tn)]
          case typeRef: TypeRef if ctx.definitions.isTupleClass(typeRef.symbol) => List(args)
          case _ => List(List(appliedType))
      // unapply[T](_$1: Any): CustomClass[T]
      case appliedType: AppliedType => mapOptionLessUnapply(appliedType, patternsSize, isUnapplySeq(denot))
      case _ => Nil

  /**
   * Recursively strips annotations from given type
   *
   * @param tpe Type to strip annotations from
   * @return Type with stripped annotations
   */
  private def stripAllAnnots(tpe: Type)(using Context): Type = tpe match
    case AppliedType(t, args) => AppliedType(stripAllAnnots(t), args.map(stripAllAnnots))
    case other => other.stripAnnots

  /**
   * Checks if tree is valid for signatureHelp. Skipped trees are either tuple or function applies
   *
   * @param tree tree to validate
   */
  private def isValid(tree: tpd.Tree)(using Context): Boolean =
    val isTupleApply =
      tree.symbol.name == nme.apply
        && tree.symbol.exists
        && ctx.definitions.isTupleClass(tree.symbol.owner.companionClass)

    val isFunctionNApply =
      tree.symbol.name == nme.apply
        && tree.symbol.exists
        && ctx.definitions.isFunctionSymbol(tree.symbol.owner)

    !isTupleApply && !isFunctionNApply

  /**
   * Get unapply method result type omiting unknown types and another method calls.
   *
   * @param fun Unapply tree
   *
   * @return Proper unapply method type after extracting result from method types and omiting unknown types.
   */
  private def unapplyMethodResult(fun: tpd.Tree)(using Context): Type =
    val typeWithoutBinds = fun match
      case TypeApply(_, Bind(_, _) :: _) => fun.symbol.asSingleDenotation.info
      case other => other.tpe

    typeWithoutBinds.finalResultType.widenDealias match
      case methodType: MethodType => methodType.resultType.widen
      case other => other

  /**
   *  Maps type by checking if given match is single match, name-based match, sequence match or product sequence match.
   *  The precedence in higher priority - higher index order for fixed-arity extractors is:
   *    1. Single match
   *    2. Name based match
   *  For variadic extractors:
   *    1. Sequence match
   *    2. Product sequence match
   *
   *  @see [[https://docs.scala-lang.org/scala3/reference/changed-features/pattern-matching.html]]
   *
   *  @param resultType   Final result type for unapply
   *  @param patternCount Currently applied patterns to unapply function
   *  @param isUnapplySeq true if unapply name equals "unapplySeq", false otherwise
   *
   *  @return List of List of types dependent on option less extractor type.
   */
  private def mapOptionLessUnapply(
    resultType: Type,
    patternCount: Int,
    isUnapplySeq: Boolean
  )(using Context): List[List[Type]] =
    val productAccessors = resultType.memberDenots(
      underscoreMembersFilter,
      (name, buf) => buf += resultType.member(name).asSingleDenotation
    )

    val getMethod = resultType.member(core.Names.termName("get"))
    val dropMethod = resultType.member(core.Names.termName("drop"))

    val availableExtractors =
      if isUnapplySeq && dropMethod.exists then
        List(dropMethod)
      else if getMethod.exists && patternCount <= 1 then
        List(getMethod)
      else
        productAccessors

    List(availableExtractors.map(_.info.finalResultType).toList)

  /**
   * Filter returning only members starting with underscore followed with number
   */
  private object underscoreMembersFilter extends NameFilter {
    def apply(pre: Type, name: Name)(using Context): Boolean =
      name.startsWith("_") && name.toString.drop(1).toIntOption.isDefined
    def isStable = true
  }

  /**
   * Creates signature for apply method.
   *
   * @param denot Function denotation for which apply signature is returned.
   *
   * @return Signature if denot is a function, None otherwise
   */
  private def toApplySignature(
    denot: SingleDenotation,
    untpdFun: Option[untpd.GenericApply],
    paramssIndex: Int
  )(using Context): Option[Signature] = {
    val symbol = denot.symbol
    val docComment = ParsedComment.docOf(symbol)

    def isDummyImplicit(res: MethodType): Boolean =
      res.resultType.isParameterless &&
        res.isImplicitMethod &&
        (
          res.paramNames.forall(name =>
            name.startsWith(NameKinds.ContextBoundParamName.separator) ||
            name.startsWith(NameKinds.ContextFunctionParamName.separator)) ||
          res.paramInfos.forall(info =>
            info.classSymbol.derivesFrom(ctx.definitions.DummyImplicitClass))
        )

    def toApplyList(tree: untpd.GenericApply): List[untpd.GenericApply] =
      tree match
        case untpd.GenericApply(fun: untpd.GenericApply, args) => toApplyList(fun) :+ tree
        case _ => List(tree)

    def toMethodTypeList(tpe: Type): List[Type] =
      tpe.resultType match
        case res: MethodOrPoly => toMethodTypeList(res) :+ tpe
        case res => List(tpe)

    def isSyntheticEvidence(name: String) =
      name.startsWith(NameKinds.ContextBoundParamName.separator)
      && symbol.paramSymss.flatten.find(_.name.show == name).exists(_.flags.is(Flags.Implicit))

    def toTypeParam(tpe: PolyType): List[Param] =
      val evidenceParams = (tpe.paramNamess.flatten zip tpe.paramInfoss.flatten).flatMap:
        case (name, AppliedType(tpe, (ref: TypeParamRef) :: _)) if isSyntheticEvidence(name.show) =>
          Some(ref.paramName -> tpe)
        case _ => None

      val tparams = tpe.paramNames.zip(tpe.paramInfos)
      tparams.map: (name, info) =>
        evidenceParams.find((evidenceName: TypeName, _: Type) => name == evidenceName).flatMap:
          case (_, tparam) => tparam.show.split('.').lastOption
        match
          case Some(evidenceTypeName) => TypeParam(s"${name.show}: ${evidenceTypeName}")
          case None => TypeParam(name.show + info.show)

    def toParamss(tp: Type, fun: Option[untpd.GenericApply])(using Context): List[List[Param]] =
      val paramSymss = symbol.paramSymss

      def reduceToParamss(applies: List[untpd.Tree], types: List[Type], paramList: Int = 0): List[List[Param]] =
        applies -> types match
          case ((_: untpd.TypeApply) :: restTrees, (poly: PolyType) :: restTypes) =>
            toTypeParam(poly) :: reduceToParamss(restTrees, restTypes, paramList + 1)
          case (restTrees, (poly: PolyType) :: restTypes) =>
            toTypeParam(poly) :: reduceToParamss(restTrees, restTypes, paramList + 1)
          case ((apply: untpd.GenericApply) :: other, tpe :: otherType) =>
            toParams(tpe, Some(apply), paramList) :: reduceToParamss(other, otherType, paramList + 1)
          case (other, (tpe @ MethodTpe(names, _, _)) :: otherType) if !isDummyImplicit(tpe) =>
            toParams(tpe, None, paramList) :: reduceToParamss(other, otherType, paramList + 1)
          case _ => Nil

      def toParams(tp: Type, apply: Option[untpd.GenericApply], paramList: Int)(using Context): List[Param] =
        val currentParams = (paramSymss.lift(paramList), tp.paramInfoss.headOption) match
          case (Some(params), Some(infos)) => params zip infos
          case _ => Nil

        val params = currentParams.map: (symbol, info) =>
          // TODO after we migrate ShortenedTypePrinter into the compiler, it should rely on its api
          val name = if symbol.isAllOf(Flags.Given | Flags.Param) && symbol.name.startsWith("x$") then nme.EMPTY else symbol.name.asTermName

          Signatures.MethodParam(
            name.show,
            info.widenTermRefExpr.show,
            docComment.flatMap(_.paramDoc(name)),
            isImplicit = tp.isImplicitMethod,
          )

        val finalParams = apply.map: apply =>
          apply.args match
            case Nil => params
            case n if n.length > params.length => params
            case _ =>
              // map argument order with their corresponding order so
              //     def foo(a: Int, b: Int, c: Int, d: Int)
              //     foo(b = 0, a = 0, d = 0, c = 0) order is List(1, 0, 3, 2)
              //   and if there are missing arguments, they are set to -1 so for the same method:
              //     foo(b= 0, d = 0, ) order is List(1, 3, -1, -1)
              val argsWithOriginalIndices = apply.args.map:
                case untpd.NamedArg(name, _) =>
                  params.indexWhere(_.name == name.toString)
                case param => -1
              .padTo(params.length, -1)

              var remainingParams = params.zipWithIndex.filter: (param, index) =>
                !argsWithOriginalIndices.contains(index)
              .map(_._1)

              val result = argsWithOriginalIndices.map:
                case -1 =>
                  val h = remainingParams.head
                  remainingParams = remainingParams.tail
                  h
                case n => params(n)

              val isReordered = params != result
              val (ordered, reordered) = params.zip(result).span: (definitionMember, reorderedMember) =>
                definitionMember == reorderedMember

              ordered.map(_._2) ++ reordered.map(_._2.copy(isReordered = isReordered))


        finalParams.getOrElse(params)
      end toParams

      val applies = untpdFun.map(toApplyList).getOrElse(Nil)
      val types = toMethodTypeList(tp).reverse

      reduceToParamss(applies, types)

    val paramss = toParamss(denot.info, untpdFun)
    val (name, returnType) =
      if (symbol.isConstructor) then
        (symbol.owner.name.show, None)
      else
        denot.symbol.defTree match
          // if there is an error in denotation type, we will fallback to source tree
          case defn: tpd.DefDef if denot.info.isErroneous => (denot.name.show, Some(defn.tpt.show))
          case _ => (denot.name.show, Some(denot.info.finalResultType.widenTermRefExpr.show))
    Some(Signatures.Signature(name, paramss, returnType, docComment.map(_.mainDoc), Some(denot)))
  }

  /**
   * Creates signature for unapply method. It is different from apply one as it should not show function name,
   * return type and type parameters. Instead we show function in the following pattern (_$1: T1, _$2: T2, _$n: Tn),
   * where _$n is only present for synthetic product extractors such as case classes.
   * In rest of cases signature skips them resulting in pattern (T1, T2, T3, Tn)
   *
   * @param denot      Unapply denotation
   * @param paramNames Parameter names for unapply final result type.
   *                     It non empty only when unapply returns synthetic product as for case classes.
   * @param paramTypes Parameter types for unapply final result type.
   *
   * @return Signature if paramTypes is non empty, None otherwise
   */
  private def toUnapplySignature(denot: SingleDenotation, paramNames: List[Name], paramTypes: List[Type])(using Context): Option[Signature] =
    val params = if paramNames.length == paramTypes.length then
      (paramNames zip paramTypes).map((name, info) => MethodParam(name.show, info.show))
    else
      // even if we only show types of arguments, they are still method params
      paramTypes.map(info => MethodParam("", info.show))

    if params.nonEmpty then Some(Signature("", List(params), None, None, Some(denot)))
    else None

  /**
   * The number of params lists before `tree` application.
   * It handles currying, so for an application such as `foo(1, 2)(3)`, the result of
   * `findParamssIndex` should be 1.
   *
   * @param tree           The tree to inspect.
   * @param alreadyCurried Number of subsequent Apply trees before current tree
   *
   * @return The index of paramss we are currently in.
   */
  private def findParamssIndex(tree: tpd.Tree, alreadyCurried: Int = 0)(using Context): Int =
    tree match
      case GenericApply(fun, params)
        if params.nonEmpty && params.forall(_.isInstanceOf[untpd.SearchFailureIdent]) =>
          findParamssIndex(fun, alreadyCurried)
      case GenericApply(fun, params) => findParamssIndex(fun, alreadyCurried + 1)
      case _ => alreadyCurried

  /**
   * Inspect `err` to determine, if it is an error related to application of an overloaded
   * function, what were the possible alternatives.
   *
   * If several alternatives are found, determines what is the best suited alternatives
   * given the parameters `params`: The alternative that has the most formal parameters
   * matching the given arguments is chosen.
   *
   * @param err            The error message to inspect.
   * @param params         The parameters that were given at the call site.
   * @param alreadyCurried Index of paramss we are currently in.
   *
   * @return A pair composed of the index of the best alternative (0 if no alternatives
   *         were found), and the list of alternatives.
   */
  private def alternativesFromError(err: ErrorType, params: List[tpd.Tree], paramssIndex: Int)(using Context): (Int, List[SingleDenotation]) = {
    val alternatives =
      err.msg match
        case msg: AmbiguousOverload  => msg.alternatives
        case msg: NoMatchingOverload => msg.alternatives
        case _                       => Nil


    // Assign a score to each alternative (how many parameters are correct so far), and
    // use that to determine what is the current active signature.
    val alternativeIndex = bestAlternative(alternatives, params, paramssIndex)
    (alternativeIndex, alternatives)
  }

  /**
   * Given a list of alternatives, and a list of parameters, returns the index of the best
   * alternative, i.e. the alternative that has the most formal parameters matching the given
   * arguments and the least number of formal parameters.
   *
   * @param alternatives  The list of alternatives to inspect.
   * @param params        The parameters that were given at the call site.
   * @param paramssIndex  Index of paramss we are currently in.
   *
   * @return The index of the best alternative.
   */
  private def bestAlternative(alternatives: List[SingleDenotation], params: List[tpd.Tree], paramssIndex: Int)(using Context): Int =
    val userParamsTypes = params.map(
      _.tpe match
        case e: PreviousErrorType =>
          /**
            * In case:
            *  def foo(i: Int, s: String): Unit = ???
            *  def foo(i: Boolean, s: Int, x: Double): Unit = ???
            *  foo(false, @@)
            *
            * `false` has error type: `Required: Int, Found: Boolean`
            */
          e.msg match
            case tm: TypeMismatch =>
              tm.found
            case _ => e
        case t => t
    )
    val alternativesScores = alternatives.map { alt =>
      val alreadyCurriedBonus = if (alt.symbol.paramSymss.length > paramssIndex) 1 else 0
        alt.info.stripPoly match
          case tpe: MethodType =>
            val score = alreadyCurriedBonus +
              userParamsTypes
                .zip(tpe.paramInfos)
                .takeWhile { case (t0, t1) =>t0 <:< t1 }
                .size
            (score, -tpe.paramInfos.length)
          case _ => (0, 0)
    }
    if (alternativesScores.isEmpty) 0
    else alternativesScores.zipWithIndex.maxBy(_._1)._2
}


