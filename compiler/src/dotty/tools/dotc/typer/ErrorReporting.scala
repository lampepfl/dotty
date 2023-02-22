package dotty.tools
package dotc
package typer

import ast._
import core._
import Types._, ProtoTypes._, Contexts._, Decorators._, Denotations._, Symbols._
import Implicits._, Flags._, Constants.Constant
import Trees._
import NameOps._
import util.SrcPos
import config.Feature
import reporting._
import collection.mutable


object ErrorReporting {

  import tpd._

  def errorTree(tree: untpd.Tree, msg: Message, pos: SrcPos)(using Context): tpd.Tree =
    tree.withType(errorType(msg, pos))

  def errorTree(tree: untpd.Tree, msg: Message)(using Context): tpd.Tree =
    errorTree(tree, msg, tree.srcPos)

  def errorTree(tree: untpd.Tree, msg: TypeError, pos: SrcPos)(using Context): tpd.Tree =
    tree.withType(errorType(msg, pos))

  def errorType(msg: Message, pos: SrcPos)(using Context): ErrorType = {
    report.error(msg, pos)
    ErrorType(msg)
  }

  def errorType(ex: TypeError, pos: SrcPos)(using Context): ErrorType = {
    report.error(ex, pos)
    ErrorType(ex.toMessage)
  }

  def wrongNumberOfTypeArgs(fntpe: Type, expectedArgs: List[ParamInfo], actual: List[untpd.Tree], pos: SrcPos)(using Context): ErrorType =
    errorType(WrongNumberOfTypeArgs(fntpe, expectedArgs, actual), pos)

  def missingArgs(tree: Tree, mt: Type)(using Context): Unit =
    val meth = err.exprStr(methPart(tree))
    mt match
      case mt: MethodType if mt.paramNames.isEmpty =>
        report.error(MissingEmptyArgumentList(meth), tree.srcPos)
      case _ =>
        report.error(em"missing arguments for $meth", tree.srcPos)

  def matchReductionAddendum(tps: Type*)(using Context): String =
    val collectMatchTrace = new TypeAccumulator[String]:
      def apply(s: String, tp: Type): String =
        if s.nonEmpty then s
        else tp match
          case tp: AppliedType if tp.isMatchAlias => MatchTypeTrace.record(tp.tryNormalize)
          case tp: MatchType => MatchTypeTrace.record(tp.tryNormalize)
          case _ => foldOver(s, tp)
    tps.foldLeft("")(collectMatchTrace)

  class Errors(using Context) {

    /** An explanatory note to be added to error messages
     *  when there's a problem with abstract var defs */
    def abstractVarMessage(sym: Symbol): String =
      if (sym.underlyingSymbol.is(Mutable))
        "\n(Note that variables need to be initialized to be defined)"
      else ""

    /** Reveal arguments in FunProtos that are proteted by an IgnoredProto but were
     *  revealed during type inference. This gives clearer error messages for overloading
     *  resolution errors that need to show argument lists after the first. We do not
     *  reveal other kinds of ignored prototypes since these might be misleading because
     *  there might be a possible implicit conversion on the result.
     */
    def revealDeepenedArgs(tp: Type): Type = tp match
      case tp @ IgnoredProto(deepTp: FunProto) if tp.wasDeepened => deepTp
      case _ => tp

    def expectedTypeStr(tp: Type): String = tp match {
      case tp: PolyProto =>
        i"type arguments [${tp.targs.tpes}%, %] and ${expectedTypeStr(revealDeepenedArgs(tp.resultType))}"
      case tp: FunProto =>
        def argStr(tp: FunProto): String =
          val result = revealDeepenedArgs(tp.resultType) match {
            case restp: FunProto => argStr(restp)
            case _: WildcardType | _: IgnoredProto => ""
            case tp => i" and expected result type $tp"
          }
          i"(${tp.typedArgs().tpes}%, %)$result"
        s"arguments ${argStr(tp)}"
      case _ =>
        i"expected type $tp"
    }

    def anonymousTypeMemberStr(tpe: Type): String = {
      val kind = tpe match {
        case _: TypeBounds => "type with bounds"
        case _: MethodOrPoly => "method"
        case _ => "value of type"
      }
      i"$kind $tpe"
    }

    def overloadedAltsStr(alts: List[SingleDenotation]): String =
      i"""overloaded alternatives of ${denotStr(alts.head)} with types
         | ${alts map (_.info)}%\n %"""

    def denotStr(denot: Denotation): String =
      if (denot.isOverloaded) overloadedAltsStr(denot.alternatives)
      else if (denot.symbol.exists) denot.symbol.showLocated
      else anonymousTypeMemberStr(denot.info)

    def refStr(tp: Type): String = tp match {
      case tp: NamedType =>
        if tp.denot.symbol.exists then tp.denot.symbol.showLocated
        else
          val kind = tp.info match
            case _: MethodOrPoly | _: ExprType => "method"
            case _ => if tp.isType then "type" else "value"
          s"$kind ${tp.name}"
      case _ => anonymousTypeMemberStr(tp)
    }

    /** Explain info of symbol `sym` as a member of class `base`.
     *   @param  showLocation  if true also show sym's location.
     */
    def infoString(sym: Symbol, base: Type, showLocation: Boolean): String =
      val sym1 = sym.underlyingSymbol
      def info = base.memberInfo(sym1)
      val infoStr =
        if sym1.isAliasType then i", which equals ${info.bounds.hi}"
        else if sym1.isAbstractOrParamType && info != TypeBounds.empty then i" with bounds$info"
        else if sym1.is(Module) then ""
        else if sym1.isTerm then i" of type $info"
        else ""
      i"${if showLocation then sym1.showLocated else sym1}$infoStr"

    def infoStringWithLocation(sym: Symbol, base: Type) =
      infoString(sym, base, showLocation = true)

    def exprStr(tree: Tree): String = refStr(tree.tpe)

    def takesNoParamsMsg(tree: Tree, kind: String): Message =
      if (tree.tpe.widen.exists)
        em"${exprStr(tree)} does not take ${kind}parameters"
      else {
        em"undefined: $tree # ${tree.uniqueId}: ${tree.tpe.toString} at ${ctx.phase}"
      }

    def patternConstrStr(tree: Tree): String = ???

    def typeMismatch(tree: Tree, pt: Type, implicitFailure: SearchFailureType = NoMatchingImplicits): Tree = {
      val normTp = normalize(tree.tpe, pt)
      val normPt = normalize(pt, pt)

      def contextFunctionCount(tp: Type): Int = tp.stripped match
        case ContextFunctionType(_, restp, _) => 1 + contextFunctionCount(restp)
        case _ => 0
      def strippedTpCount = contextFunctionCount(tree.tpe) - contextFunctionCount(normTp)
      def strippedPtCount = contextFunctionCount(pt) - contextFunctionCount(normPt)

      val (treeTp, expectedTp) =
        if normTp <:< normPt || strippedTpCount != strippedPtCount
        then (tree.tpe, pt)
        else (normTp, normPt)
        // use normalized types if that also shows an error, and both sides stripped
        // the same number of context functions. Use original types otherwise.

      def missingElse = tree match
        case If(_, _, elsep @ Literal(Constant(()))) if elsep.span.isSynthetic =>
          "\nMaybe you are missing an else part for the conditional?"
        case _ => ""

      errorTree(tree, TypeMismatch(treeTp, expectedTp, Some(tree), implicitFailure.whyNoConversion, missingElse))
    }

    /** A subtype log explaining why `found` does not conform to `expected` */
    def whyNoMatchStr(found: Type, expected: Type): String =
      val header =
        i"""I tried to show that
          |  $found
          |conforms to
          |  $expected
          |but the comparison trace ended with `false`:
          |"""
      val c = ctx.typerState.constraint
      val constraintText =
        if c.domainLambdas.isEmpty then
          "the empty constraint"
        else
          i"""a constraint with:
             |$c"""
      i"""${TypeComparer.explained(_.isSubType(found, expected), header)}
         |
         |The tests were made under $constraintText"""

    def whyFailedStr(fail: FailedExtension) =
      i"""
         |
         |    failed with:
         |
         |${fail.whyFailed.message.indented(8)}"""

    def selectErrorAddendum
      (tree: untpd.RefTree, qual1: Tree, qualType: Type, suggestImports: Type => String, foundWithoutNull: Boolean = false)
      (using Context): String =

      val attempts = mutable.ListBuffer[(Tree, String)]()
      val nested = mutable.ListBuffer[NestedFailure]()
      for
        failures <- qual1.getAttachment(Typer.HiddenSearchFailure)
        failure <- failures
      do
        failure.reason match
          case fail: NestedFailure => nested += fail
          case fail: FailedExtension => attempts += ((failure.tree, whyFailedStr(fail)))
          case fail: Implicits.NoMatchingImplicits => // do nothing
          case _ => attempts += ((failure.tree, ""))
      if foundWithoutNull then
        i""".
          |Since explicit-nulls is enabled, the selection is rejected because
          |${qualType.widen} could be null at runtime.
          |If you want to select ${tree.name} without checking for a null value,
          |insert a .nn before .${tree.name} or import scala.language.unsafeNulls."""
      else if qualType.derivesFrom(defn.DynamicClass) then
        "\npossible cause: maybe a wrong Dynamic method signature?"
      else if attempts.nonEmpty then
        val attemptStrings =
          attempts.toList
            .map((tree, whyFailed) => (tree.showIndented(4), whyFailed))
            .distinctBy(_._1)
            .map((treeStr, whyFailed) =>
              i"""
                 |    $treeStr$whyFailed""")
        val extMethods =
          if attemptStrings.length > 1 then "Extension methods were"
          else "An extension method was"
        i""".
           |$extMethods tried, but could not be fully constructed:
           |$attemptStrings%\n%"""
      else if nested.nonEmpty then
        i""".
           |Extension methods were tried, but the search failed with:
           |
           |${nested.head.explanation.indented(4)}"""
      else if tree.hasAttachment(desugar.MultiLineInfix) then
        i""".
           |Note that `${tree.name}` is treated as an infix operator in Scala 3.
           |If you do not want that, insert a `;` or empty line in front
           |or drop any spaces behind the operator."""
      else if qualType.isExactlyNothing then
        ""
      else
        val add = suggestImports(
          ViewProto(qualType.widen,
            SelectionProto(tree.name, WildcardType, NoViewsAllowed, privateOK = false)))
        if add.isEmpty then ""
        else ", but could be made available as an extension method." ++ add
    end selectErrorAddendum
  }

  def substitutableTypeSymbolsInScope(sym: Symbol)(using Context): List[Symbol] =
    sym.ownersIterator.takeWhile(!_.is(Flags.Package)).flatMap { ownerSym =>
      ownerSym.paramSymss.flatten.filter(_.isType) ++
      ownerSym.typeRef.nonClassTypeMembers.map(_.symbol)
    }.toList

  def dependentMsg =
    """Term-dependent types are experimental,
      |they must be enabled with a `experimental.dependent` language import or setting""".stripMargin.toMessage

  def err(using Context): Errors = new Errors
}
