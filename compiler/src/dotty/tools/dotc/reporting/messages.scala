package dotty.tools
package dotc
package reporting

import core.*
import Contexts.*
import Decorators.*, Symbols.*, Names.*, NameOps.*, Types.*, Flags.*, Phases.*
import Denotations.SingleDenotation
import SymDenotations.SymDenotation
import NameKinds.{WildcardParamName, ContextFunctionParamName}
import parsing.Scanners.Token
import parsing.Tokens
import printing.Highlighting.*
import printing.Formatting
import ErrorMessageID.*
import ast.Trees
import config.{Feature, ScalaVersion}
import typer.ErrorReporting.{err, matchReductionAddendum, substitutableTypeSymbolsInScope}
import typer.ProtoTypes.{ViewProto, SelectionProto, FunProto}
import typer.Implicits.*
import typer.Inferencing
import scala.util.control.NonFatal
import StdNames.nme
import printing.Formatting.hl
import ast.Trees.*
import ast.untpd
import ast.tpd
import scala.util.matching.Regex
import java.util.regex.Matcher.quoteReplacement
import cc.CaptureSet.IdentityCaptRefMap
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.SourcePosition
import scala.jdk.CollectionConverters.*
import dotty.tools.dotc.util.SourceFile
import DidYouMean.*

/**  Messages
  *  ========
  *  The role of messages is to provide the necessary details for a simple to
  *  understand diagnostic event. Each message can be turned into a message
  *  container (one of the above) by calling the appropriate method on them.
  *  For instance:
  *
  *  ```scala
  *  EmptyCatchBlock(tree).error(pos)   // res: Error
  *  EmptyCatchBlock(tree).warning(pos) // res: Warning
  *  ```
  */

abstract class SyntaxMsg(errorId: ErrorMessageID)(using Context) extends Message(errorId):
  def kind = MessageKind.Syntax

abstract class TypeMsg(errorId: ErrorMessageID)(using Context) extends Message(errorId):
  def kind = MessageKind.Type

trait ShowMatchTrace(tps: Type*)(using Context) extends Message:
  override def msgPostscript(using Context): String =
    super.msgPostscript ++ matchReductionAddendum(tps*)

abstract class TypeMismatchMsg(found: Type, expected: Type)(errorId: ErrorMessageID)(using Context)
extends Message(errorId), ShowMatchTrace(found, expected):
  def kind = MessageKind.TypeMismatch
  def explain(using Context) = err.whyNoMatchStr(found, expected)
  override def canExplain = true

abstract class NamingMsg(errorId: ErrorMessageID)(using Context) extends Message(errorId), NoDisambiguation:
  def kind = MessageKind.Naming

abstract class DeclarationMsg(errorId: ErrorMessageID)(using Context) extends Message(errorId):
  def kind = MessageKind.Declaration

/** A simple not found message (either for idents, or member selection.
 *  Messages of this class are sometimes dropped in favor of other, more
 *  specific messages.
 */
abstract class NotFoundMsg(errorId: ErrorMessageID)(using Context) extends Message(errorId):
  def kind = MessageKind.NotFound
  def name: Name

abstract class PatternMatchMsg(errorId: ErrorMessageID)(using Context) extends Message(errorId):
  def kind = MessageKind.PatternMatch

abstract class CyclicMsg(errorId: ErrorMessageID)(using Context) extends Message(errorId):
  def kind = MessageKind.Cyclic

  val ex: CyclicReference
  protected def cycleSym = ex.denot.symbol

  protected def debugInfo =
    if ctx.settings.YdebugCyclic.value then
      "\n\nStacktrace:" ++ ex.getStackTrace().nn.mkString("\n    ", "\n    ", "")
    else "\n\n Run with both -explain-cyclic and -Ydebug-cyclic to see full stack trace."

  protected def context: String = ex.optTrace match
    case Some(trace) =>
      s"\n\nThe error occurred while trying to ${
        trace.map((prefix, sym, suffix) => i"$prefix$sym$suffix").mkString("\n  which required to ")
      }$debugInfo"
    case None =>
      "\n\n Run with -explain-cyclic for more details."
end CyclicMsg

abstract class ReferenceMsg(errorId: ErrorMessageID)(using Context) extends Message(errorId):
  def kind = MessageKind.Reference

abstract class EmptyCatchOrFinallyBlock(tryBody: untpd.Tree, errNo: ErrorMessageID)(using Context)
extends SyntaxMsg(errNo) {
  def explain(using Context) = {
    val tryString = tryBody match {
      case Block(Nil, untpd.EmptyTree) => "{}"
      case _ => tryBody.show
    }

    val code1 =
      s"""|import scala.util.control.NonFatal
          |
          |try $tryString catch {
          |  case NonFatal(e) => ???
          |}""".stripMargin

    val code2 =
      s"""|try $tryString finally {
          |  // perform your cleanup here!
          |}""".stripMargin

    i"""|A ${hl("try")} expression should be followed by some mechanism to handle any exceptions
        |thrown. Typically a ${hl("catch")} expression follows the ${hl("try")} and pattern matches
        |on any expected exceptions. For example:
        |
        |$code1
        |
        |It is also possible to follow a ${hl("try")} immediately by a ${hl("finally")} - letting the
        |exception propagate - but still allowing for some clean up in ${hl("finally")}:
        |
        |$code2
        |
        |It is recommended to use the ${hl("NonFatal")} extractor to catch all exceptions as it
        |correctly handles transfer functions like ${hl("return")}."""
  }
}

class EmptyCatchBlock(tryBody: untpd.Tree)(using Context)
extends EmptyCatchOrFinallyBlock(tryBody, EmptyCatchBlockID) {
  def msg(using Context) =
    i"""|The ${hl("catch")} block does not contain a valid expression, try
        |adding a case like - ${hl("case e: Exception =>")} to the block"""
}

class EmptyCatchAndFinallyBlock(tryBody: untpd.Tree)(using Context)
extends EmptyCatchOrFinallyBlock(tryBody, EmptyCatchAndFinallyBlockID) {
  def msg(using Context) =
    i"""|A ${hl("try")} without ${hl("catch")} or ${hl("finally")} is equivalent to putting
        |its body in a block; no exceptions are handled."""
}

class DeprecatedWithOperator(rewrite: String)(using Context)
extends SyntaxMsg(DeprecatedWithOperatorID) {
  def msg(using Context) =
    i"""${hl("with")} as a type operator has been deprecated; use ${hl("&")} instead$rewrite"""
  def explain(using Context) =
    i"""|Dotty introduces intersection types - ${hl("&")} types. These replace the
        |use of the ${hl("with")} keyword. There are a few differences in
        |semantics between intersection types and using ${hl("with")}."""
}

class CaseClassMissingParamList(cdef: untpd.TypeDef)(using Context)
extends SyntaxMsg(CaseClassMissingParamListID) {
  def msg(using Context) =
    i"""|A ${hl("case class")} must have at least one parameter list"""

  def explain(using Context) =
    i"""|${cdef.name} must have at least one parameter list, if you would rather
        |have a singleton representation of ${cdef.name}, use a "${hl("case object")}".
        |Or, add an explicit ${hl("()")} as a parameter list to ${cdef.name}."""
}

class AnonymousFunctionMissingParamType(param: untpd.ValDef,
                                        tree: untpd.Function,
                                        inferredType: Type,
                                        expectedType: Type,
                                        )
                                        (using Context)
extends TypeMsg(AnonymousFunctionMissingParamTypeID) {
  def msg(using Context) = {
    val paramDescription =
      if param.name.is(WildcardParamName)
          || param.name.is(ContextFunctionParamName)
          || MethodType.syntheticParamNames(tree.args.length + 1).contains(param.name)
      then i"\nin expanded function:\n  $tree"
      else ""

    val inferred =
      if inferredType == WildcardType then ""
      else i"\nWhat I could infer was: $inferredType"

    val expected =
      if expectedType == WildcardType then ""
      else i"\nExpected type for the whole anonymous function:\n  $expectedType"

    i"""Missing parameter type
       |
       |I could not infer the type of the parameter ${param.name}$paramDescription$inferred$expected"""
  }

  def explain(using Context) = ""
}

class WildcardOnTypeArgumentNotAllowedOnNew()(using Context)
extends SyntaxMsg(WildcardOnTypeArgumentNotAllowedOnNewID) {
  def msg(using Context) = "Type argument must be fully defined"
  def explain(using Context) =
    val code1: String =
      """
        |object TyperDemo {
        |  class Team[A]
        |  val team = new Team[?]
        |}
      """.stripMargin

    val code2: String =
      """
        |object TyperDemo {
        |  class Team[A]
        |  val team = new Team[Int]
        |}
      """.stripMargin
    i"""|Wildcard on arguments is not allowed when declaring a new type.
        |
        |Given the following example:
        |
        |$code1
        |
        |You must complete all the type parameters, for instance:
        |
        |$code2 """
}


// Type Errors ------------------------------------------------------------ //
class DuplicateBind(bind: untpd.Bind, tree: untpd.CaseDef)(using Context)
extends NamingMsg(DuplicateBindID) {
  def msg(using Context) = i"duplicate pattern variable: ${bind.name}"

  def explain(using Context) = {
    val pat = tree.pat.show
    val guard = tree.guard match
      case untpd.EmptyTree => ""
      case guard => s"if ${guard.show}"

    val body = tree.body match {
      case Block(Nil, untpd.EmptyTree) => ""
      case body => s" ${body.show}"
    }

    val caseDef = s"case $pat$guard => $body"

    i"""|For each ${hl("case")} bound variable names have to be unique. In:
        |
        |$caseDef
        |
        |${bind.name} is not unique. Rename one of the bound variables!"""
  }
}

class MissingIdent(tree: untpd.Ident, treeKind: String, val name: Name, proto: Type)(using Context)
extends NotFoundMsg(MissingIdentID) {
  def msg(using Context) =
    val missing = name.show
    val addendum =
      didYouMean(
        inScopeCandidates(name.isTypeName, isApplied = proto.isInstanceOf[FunProto], rootImportOK = true)
          .closestTo(missing),
        proto, "")

    i"Not found: $treeKind$name$addendum"
  def explain(using Context) = {
    i"""|Each identifier in Scala needs a matching declaration. There are two kinds of
        |identifiers: type identifiers and value identifiers. Value identifiers are introduced
        |by `val`, `def`, or `object` declarations. Type identifiers are introduced by `type`,
        |`class`, `enum`, or `trait` declarations.
        |
        |Identifiers refer to matching declarations in their environment, or they can be
        |imported from elsewhere.
        |
        |Possible reasons why no matching declaration was found:
        | - The declaration or the use is mis-spelt.
        | - An import is missing."""
  }
}

class TypeMismatch(val found: Type, expected: Type, inTree: Option[untpd.Tree], addenda: => String*)(using Context)
  extends TypeMismatchMsg(found, expected)(TypeMismatchID):

  def msg(using Context) =
    // replace constrained TypeParamRefs and their typevars by their bounds where possible
    // and the bounds are not f-bounds.
    // The idea is that if the bounds are also not-subtypes of each other to report
    // the type mismatch on the bounds instead of the original TypeParamRefs, since
    // these are usually easier to analyze. We exclude F-bounds since these would
    // lead to a recursive infinite expansion.
    object reported extends TypeMap, IdentityCaptRefMap:
      def setVariance(v: Int) = variance = v
      val constraint = mapCtx.typerState.constraint
      var fbounded = false
      def apply(tp: Type): Type = tp match
        case tp: TypeParamRef =>
          constraint.entry(tp) match
            case bounds: TypeBounds =>
              if variance < 0 then apply(TypeComparer.fullUpperBound(tp))
              else if variance > 0 then apply(TypeComparer.fullLowerBound(tp))
              else tp
            case NoType => tp
            case instType => apply(instType)
        case tp: TypeVar =>
          apply(tp.stripTypeVar)
        case tp: LazyRef =>
          fbounded = true
          tp
        case _ =>
          mapOver(tp)

    val found1 = reported(found)
    reported.setVariance(-1)
    val expected1 = reported(expected)
    val (found2, expected2) =
      if (found1 frozen_<:< expected1) || reported.fbounded then (found, expected)
      else (found1, expected1)
    val (foundStr, expectedStr) = Formatting.typeDiff(found2, expected2)
    i"""|Found:    $foundStr
        |Required: $expectedStr"""
  end msg

  override def msgPostscript(using Context) =
    def importSuggestions =
      if expected.isTopType || found.isBottomType then ""
      else ctx.typer.importSuggestionAddendum(ViewProto(found.widen, expected))
    super.msgPostscript
    ++ addenda.dropWhile(_.isEmpty).headOption.getOrElse(importSuggestions)

  override def explain(using Context) =
    val treeStr = inTree.map(x => s"\nTree: ${x.show}").getOrElse("")
    treeStr + "\n" + super.explain

end TypeMismatch

class NotAMember(site: Type, val name: Name, selected: String, proto: Type, addendum: => String = "")(using Context)
extends NotFoundMsg(NotAMemberID), ShowMatchTrace(site) {
  //println(i"site = $site, decls = ${site.decls}, source = ${site.typeSymbol.sourceFile}") //DEBUG

  def msg(using Context) = {
    val missing = name.show

    val enumClause =
      if ((name eq nme.values) || (name eq nme.valueOf)) && site.classSymbol.companionClass.isEnumClass then
        val kind = if name eq nme.values then i"${nme.values} array" else i"${nme.valueOf} lookup method"
        // an assumption is made here that the values and valueOf methods were not generated
        // because the enum defines non-singleton cases
        i"""
            |Although ${site.classSymbol.companionClass} is an enum, it has non-singleton cases,
            |meaning a $kind is not defined"""
      else
        ""

    def prefixEnumClause(addendum: String) =
      if enumClause.nonEmpty then s".$enumClause$addendum" else addendum

    val finalAddendum =
      if addendum.nonEmpty then prefixEnumClause(addendum)
      else
        val hint = didYouMean(
          memberCandidates(site, name.isTypeName, isApplied = proto.isInstanceOf[FunProto])
            .closestTo(missing)
            .map((d, sym) => (d, Binding(sym.name, sym, site))),
          proto,
          prefix = site match
            case site: NamedType => i"${site.name}."
            case site => i"$site."
        )
        if hint.isEmpty then prefixEnumClause("")
        else hint ++ enumClause

    i"$selected $name is not a member of ${site.widen}$finalAddendum"
  }

  def explain(using Context) = ""
}

class EarlyDefinitionsNotSupported()(using Context)
extends SyntaxMsg(EarlyDefinitionsNotSupportedID) {
  def msg(using Context) = "Early definitions are not supported; use trait parameters instead"

  def explain(using Context) = {
    val code1 =
      """|trait Logging {
          |  val f: File
          |  f.open()
          |  onExit(f.close())
          |  def log(msg: String) = f.write(msg)
          |}
          |
          |class B extends Logging {
          |  val f = new File("log.data") // triggers a NullPointerException
          |}
          |
          |// early definition gets around the NullPointerException
          |class C extends {
          |  val f = new File("log.data")
          |} with Logging""".stripMargin

    val code2 =
      """|trait Logging(f: File) {
          |  f.open()
          |  onExit(f.close())
          |  def log(msg: String) = f.write(msg)
          |}
          |
          |class C extends Logging(new File("log.data"))""".stripMargin

    i"""|Earlier versions of Scala did not support trait parameters and "early
        |definitions" (also known as "early initializers") were used as an alternative.
        |
        |Example of old syntax:
        |
        |$code1
        |
        |The above code can now be written as:
        |
        |$code2
        |"""
  }
}

class TopLevelImplicitClass(cdef: untpd.TypeDef)(using Context)
extends SyntaxMsg(TopLevelImplicitClassID) {
  def msg(using Context) = i"""An ${hl("implicit class")} may not be top-level"""

  def explain(using Context) = {
    val TypeDef(name, impl @ Template(constr0, parents, self, _)) = cdef: @unchecked
    val exampleArgs =
      if(constr0.termParamss.isEmpty) "..."
      else constr0.termParamss(0).map(_.withMods(untpd.Modifiers()).show).mkString(", ")
    def defHasBody[T] = impl.body.exists(!_.isEmpty)
    val exampleBody = if (defHasBody) "{\n ...\n }" else ""
    i"""|There may not be any method, member or object in scope with the same name as
        |the implicit class and a case class automatically gets a companion object with
        |the same name created by the compiler which would cause a naming conflict if it
        |were allowed.
        |           |
        |To resolve the conflict declare ${cdef.name} inside of an ${hl("object")} then import the class
        |from the object at the use site if needed, for example:
        |
        |object Implicits {
        |  implicit class ${cdef.name}($exampleArgs)$exampleBody
        |}
        |
        |// At the use site:
        |import Implicits.${cdef.name}"""
  }
}

class ImplicitCaseClass(cdef: untpd.TypeDef)(using Context)
extends SyntaxMsg(ImplicitCaseClassID) {
  def msg(using Context) = i"""A ${hl("case class")} may not be defined as ${hl("implicit")}"""

  def explain(using Context) =
    i"""|Implicit classes may not be case classes. Instead use a plain class:
        |
        |implicit class ${cdef.name}...
        |
        |"""
}

class ImplicitClassPrimaryConstructorArity()(using Context)
extends SyntaxMsg(ImplicitClassPrimaryConstructorArityID){
  def msg(using Context) = "Implicit classes must accept exactly one primary constructor parameter"
  def explain(using Context) = {
    val example = "implicit class RichDate(date: java.util.Date)"
    i"""Implicit classes may only take one non-implicit argument in their constructor. For example:
        |
        | $example
        |
        |While it’s possible to create an implicit class with more than one non-implicit argument,
        |such classes aren’t used during implicit lookup.
        |"""
  }
}

class ObjectMayNotHaveSelfType(mdef: untpd.ModuleDef)(using Context)
extends SyntaxMsg(ObjectMayNotHaveSelfTypeID) {
  def msg(using Context) = i"""${hl("object")}s must not have a self ${hl("type")}"""

  def explain(using Context) = {
    val untpd.ModuleDef(name, tmpl) = mdef
    val ValDef(_, selfTpt, _) = tmpl.self
    i"""|${hl("object")}s must not have a self ${hl("type")}:
        |
        |Consider these alternative solutions:
        |  - Create a trait or a class instead of an object
        |  - Let the object extend a trait containing the self type:
        |
        |    object $name extends ${selfTpt.show}"""
  }
}

class RepeatedModifier(modifier: String, source: SourceFile, span: Span)(implicit ctx:Context)
extends SyntaxMsg(RepeatedModifierID) {
  def msg(using Context) = i"""Repeated modifier $modifier"""

  def explain(using Context) = {
    val code1 = "private private val Origin = Point(0, 0)"
    val code2 = "private final val Origin = Point(0, 0)"
    i"""This happens when you accidentally specify the same modifier twice.
        |
        |Example:
        |
        |$code1
        |
        |instead of
        |
        |$code2
        |
        |"""
  }

  override def actions(using Context) =
    import scala.language.unsafeNulls
    List(
      CodeAction(title = s"""Remove repeated modifier: "$modifier"""",
        description = None,
        patches = List(
          ActionPatch(SourcePosition(source, span), "")
        )
      )
    )
}

class InterpolatedStringError()(implicit ctx:Context)
extends SyntaxMsg(InterpolatedStringErrorID) {
  def msg(using Context) = "Error in interpolated string: identifier or block expected"
  def explain(using Context) = {
    val code1 = "s\"$new Point(0, 0)\""
    val code2 = "s\"${new Point(0, 0)}\""
    i"""|This usually happens when you forget to place your expressions inside curly braces.
        |
        |$code1
        |
        |should be written as
        |
        |$code2
        |"""
  }
}

class UnboundPlaceholderParameter()(implicit ctx:Context)
extends SyntaxMsg(UnboundPlaceholderParameterID) {
  def msg(using Context) = i"""Unbound placeholder parameter; incorrect use of ${hl("_")}"""
  def explain(using Context) =
    i"""|The ${hl("_")} placeholder syntax was used where it could not be bound.
        |Consider explicitly writing the variable binding.
        |
        |This can be done by replacing ${hl("_")} with a variable (eg. ${hl("x")})
        |and adding ${hl("x =>")} where applicable.
        |
        |Example before:
        |
        |${hl("{ _ }")}
        |
        |Example after:
        |
        |${hl("x => { x }")}
        |
        |Another common occurrence for this error is defining a val with ${hl("_")}:
        |
        |${hl("val a = _")}
        |
        |But this val definition isn't very useful, it can never be assigned
        |another value. And thus will always remain uninitialized.
        |Consider replacing the ${hl("val")} with ${hl("var")}:
        |
        |${hl("var a = _")}
        |
        |Note that this use of ${hl("_")} is not placeholder syntax,
        |but an uninitialized var definition.
        |Only fields can be left uninitialized in this manner; local variables
        |must be initialized.
        |
        |Another occurrence for this error is self type definition.
        |The ${hl("_")} can be replaced with ${hl("this")}.
        |
        |Example before:
        |
        |${hl("trait A { _: B => ... ")}
        |
        |Example after:
        |
        |${hl("trait A { this: B => ... ")}
        |"""
}

class IllegalStartSimpleExpr(illegalToken: String)(using Context)
extends SyntaxMsg(IllegalStartSimpleExprID) {
  def msg(using Context) = i"expression expected but ${Red(illegalToken)} found"
  def explain(using Context) = {
    i"""|An expression cannot start with ${Red(illegalToken)}."""
  }
}

class MissingReturnType()(implicit ctx:Context)
extends SyntaxMsg(MissingReturnTypeID) {
  def msg(using Context) = "Missing return type"
  def explain(using Context) =
    i"""|An abstract declaration must have a return type. For example:
        |
        |trait Shape:
        |  ${hl("def area: Double")} // abstract declaration returning a Double"""
}

class MissingReturnTypeWithReturnStatement(method: Symbol)(using Context)
extends SyntaxMsg(MissingReturnTypeWithReturnStatementID) {
  def msg(using Context) = i"$method has a return statement; it needs a result type"
  def explain(using Context) =
    i"""|If a method contains a ${hl("return")} statement, it must have an
        |explicit return type. For example:
        |
        |${hl("def good: Int /* explicit return type */ = return 1")}"""
}

class YieldOrDoExpectedInForComprehension()(using Context)
extends SyntaxMsg(YieldOrDoExpectedInForComprehensionID) {
  def msg(using Context) = i"${hl("yield")} or ${hl("do")} expected"

  def explain(using Context) =
    i"""|When the enumerators in a for comprehension are not placed in parentheses or
        |braces, a ${hl("do")} or ${hl("yield")} statement is required after the enumerators
        |section of the comprehension.
        |
        |You can save some keystrokes by omitting the parentheses and writing
        |
        |${hl("val numbers = for i <- 1 to 3 yield i")}
        |
        |  instead of
        |
        |${hl("val numbers = for (i <- 1 to 3) yield i")}
        |
        |but the ${hl("yield")} keyword is still required.
        |
        |For comprehensions that simply perform a side effect without yielding anything
        |can also be written without parentheses but a ${hl("do")} keyword has to be
        |included. For example,
        |
        |${hl("for (i <- 1 to 3) println(i)")}
        |
        |can be written as
        |
        |${hl("for i <- 1 to 3 do println(i) // notice the 'do' keyword")}
        |
        |"""
}

class ProperDefinitionNotFound()(using Context)
extends Message(ProperDefinitionNotFoundID) {
  def kind = MessageKind.DocComment
  def msg(using Context) = i"""Proper definition was not found in ${hl("@usecase")}"""

  def explain(using Context) = {
    val noUsecase =
      "def map[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That"

    val usecase =
      """|/** Map from List[A] => List[B]
          |  *
          |  * @usecase def map[B](f: A => B): List[B]
          |  */
          |def map[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That
          |""".stripMargin

    i"""|Usecases are only supported for ${hl("def")}s. They exist because with Scala's
        |advanced type-system, we sometimes end up with seemingly scary signatures.
        |The usage of these methods, however, needs not be - for instance the ${hl("map")}
        |function
        |
        |${hl("List(1, 2, 3).map(2 * _) // res: List(2, 4, 6)")}
        |
        |is easy to understand and use - but has a rather bulky signature:
        |
        |$noUsecase
        |
        |to mitigate this and ease the usage of such functions we have the ${hl("@usecase")}
        |annotation for docstrings. Which can be used like this:
        |
        |$usecase
        |
        |When creating the docs, the signature of the method is substituted by the
        |usecase and the compiler makes sure that it is valid. Because of this, you're
        |only allowed to use ${hl("def")}s when defining usecases."""
  }
}

class ByNameParameterNotSupported(tpe: untpd.Tree)(using Context)
extends SyntaxMsg(ByNameParameterNotSupportedID) {
  def msg(using Context) = i"By-name parameter type ${tpe} not allowed here."

  def explain(using Context) =
    i"""|By-name parameters act like functions that are only evaluated when referenced,
        |allowing for lazy evaluation of a parameter.
        |
        |An example of using a by-name parameter would look like:
        |${hl("def func(f: => Boolean) = f // 'f' is evaluated when referenced within the function")}
        |
        |An example of the syntax of passing an actual function as a parameter:
        |${hl("def func(f: (Boolean => Boolean)) = f(true)")}
        |
        |or:
        |
        |${hl("def func(f: Boolean => Boolean) = f(true)")}
        |
        |And the usage could be as such:
        |${hl("func(bool => // do something...)")}
        |"""
}

class WrongNumberOfTypeArgs(fntpe: Type, expectedArgs: List[ParamInfo], actual: List[untpd.Tree])(using Context)
extends SyntaxMsg(WrongNumberOfTypeArgsID) {

  private val expectedCount = expectedArgs.length
  private val actualCount = actual.length
  private val msgPrefix = if (actualCount > expectedCount) "Too many" else "Not enough"

  def msg(using Context) =
    val expectedArgString = expectedArgs
      .map(_.paramName.unexpandedName.show)
      .mkString("[", ", ", "]")
    val actualArgString = actual.map(_.show).mkString("[", ", ", "]")
    val prettyName =
      try fntpe.termSymbol match
        case NoSymbol => fntpe.show
        case symbol   => symbol.showFullName
      catch case NonFatal(ex) => fntpe.show
    i"""|$msgPrefix type arguments for $prettyName$expectedArgString
        |expected: $expectedArgString
        |actual:   $actualArgString"""

  def explain(using Context) = {
    val tooManyTypeParams =
      """|val tuple2: (Int, String) = (1, "one")
          |val list: List[(Int, String)] = List(tuple2)""".stripMargin

    if (actualCount > expectedCount)
      i"""|You have supplied too many type parameters
          |
          |For example List takes a single type parameter (List[A])
          |If you need to hold more types in a list then you need to combine them
          |into another data type that can contain the number of types you need,
          |In this example one solution would be to use a Tuple:
          |
          |${tooManyTypeParams}"""
    else
      i"""|You have not supplied enough type parameters
          |If you specify one type parameter then you need to specify every type parameter."""
  }
}

class IllegalVariableInPatternAlternative(name: Name)(using Context)
extends SyntaxMsg(IllegalVariableInPatternAlternativeID) {
  def msg(using Context) = i"Illegal variable $name in pattern alternative"
  def explain(using Context) = {
    val varInAlternative =
      """|def g(pair: (Int,Int)): Int = pair match {
          |  case (1, n) | (n, 1) => n
          |  case _ => 0
          |}""".stripMargin

    val fixedVarInAlternative =
      """|def g(pair: (Int,Int)): Int = pair match {
          |  case (1, n) => n
          |  case (n, 1) => n
          |  case _ => 0
          |}""".stripMargin

    i"""|Variables are not allowed within alternate pattern matches. You can workaround
        |this issue by adding additional cases for each alternative. For example, the
        |illegal function:
        |
        |$varInAlternative
        |could be implemented by moving each alternative into a separate case:
        |
        |$fixedVarInAlternative"""
  }
}

class IdentifierExpected(identifier: String)(using Context)
extends SyntaxMsg(IdentifierExpectedID) {
  def msg(using Context) = "identifier expected"
  def explain(using Context) = {
    val wrongIdentifier = i"def foo: $identifier = {...}"
    val validIdentifier = i"def foo = {...}"
    i"""|An identifier expected, but $identifier found. This could be because
        |$identifier is not a valid identifier. As a workaround, the compiler could
        |infer the type for you. For example, instead of:
        |
        |$wrongIdentifier
        |
        |Write your code like:
        |
        |$validIdentifier
        |
        |"""
  }
}

class AuxConstructorNeedsNonImplicitParameter()(implicit ctx:Context)
extends SyntaxMsg(AuxConstructorNeedsNonImplicitParameterID) {
  def msg(using Context) = "Auxiliary constructor needs non-implicit parameter list"
  def explain(using Context) =
    i"""|Only the primary constructor is allowed an ${hl("implicit")} parameter list;
        |auxiliary constructors need non-implicit parameter lists. When a primary
        |constructor has an implicit argslist, auxiliary constructors that call the
        |primary constructor must specify the implicit value.
        |
        |To resolve this issue check for:
        | - Forgotten parenthesis on ${hl("this")} (${hl("def this() = { ... }")})
        | - Auxiliary constructors specify the implicit value
        |"""
}

class IllegalLiteral()(using Context)
extends SyntaxMsg(IllegalLiteralID) {
  def msg(using Context) = "Illegal literal"
  def explain(using Context) =
    i"""|Available literals can be divided into several groups:
        | - Integer literals: 0, 21, 0xFFFFFFFF, -42L
        | - Floating Point Literals: 0.0, 1e30f, 3.14159f, 1.0e-100, .1
        | - Boolean Literals: true, false
        | - Character Literals: 'a', '\u0041', '\n'
        | - String Literals: "Hello, World!"
        | - null
        |"""
}

class LossyWideningConstantConversion(sourceType: Type, targetType: Type)(using Context)
extends Message(LossyWideningConstantConversionID):
  def kind = MessageKind.LossyConversion
  def msg(using Context) = i"""|Widening conversion from $sourceType to $targetType loses precision.
                |Write `.to$targetType` instead."""
  def explain(using Context) = ""

class PatternMatchExhaustivity(uncoveredCases: Seq[String], tree: untpd.Match)(using Context)
extends Message(PatternMatchExhaustivityID) {
  def kind = MessageKind.PatternMatchExhaustivity

  private val hasMore = uncoveredCases.lengthCompare(6) > 0
  val uncovered = uncoveredCases.take(6).mkString(", ")

  def msg(using Context) =
    val addendum = if hasMore then "(More unmatched cases are elided)" else ""
    i"""|${hl("match")} may not be exhaustive.
        |
        |It would fail on pattern case: $uncovered
        |$addendum"""


  def explain(using Context) =
    i"""|There are several ways to make the match exhaustive:
        | - Add missing cases as shown in the warning
        | - If an extractor always return ${hl("Some(...)")}, write ${hl("Some[X]")} for its return type
        | - Add a ${hl("case _ => ...")} at the end to match all remaining cases
        |"""

  override def actions(using Context) =
    import scala.language.unsafeNulls
    val endPos = tree.cases.lastOption.map(_.endPos)
      .getOrElse(tree.selector.endPos)
    val startColumn = tree.cases.lastOption
      .map(_.startPos.startColumn)
      .getOrElse(tree.selector.startPos.startColumn + 2)

    val pathes = List(
      ActionPatch(
        srcPos = endPos,
        replacement = uncoveredCases.map(c => indent(s"case $c => ???", startColumn))
          .mkString("\n", "\n", "")
      ),
    )
    List(
      CodeAction(title = s"Insert missing cases (${uncoveredCases.size})",
        description = None,
        patches = pathes
      )
    )


  private def indent(text:String, margin: Int): String = {
    import scala.language.unsafeNulls
    " " * margin + text
  }
}

class UncheckedTypePattern(argType: Type, whyNot: String)(using Context)
  extends PatternMatchMsg(UncheckedTypePatternID) {
  def msg(using Context) = i"the type test for $argType cannot be checked at runtime because $whyNot"
  def explain(using Context) =
    i"""|Type arguments and type refinements are erased during compile time, thus it's
        |impossible to check them at run-time.
        |
        |You can either replace the type arguments by ${hl("_")} or use `@unchecked`.
        |"""
}

class MatchCaseUnreachable()(using Context)
extends Message(MatchCaseUnreachableID) {
  def kind = MessageKind.MatchCaseUnreachable
  def msg(using Context) = "Unreachable case"
  def explain(using Context) = ""
}

class MatchCaseOnlyNullWarning()(using Context)
extends PatternMatchMsg(MatchCaseOnlyNullWarningID) {
  def msg(using Context) = i"""Unreachable case except for ${hl("null")} (if this is intentional, consider writing ${hl("case null =>")} instead)."""
  def explain(using Context) = ""
}

class MatchableWarning(tp: Type, pattern: Boolean)(using Context)
extends TypeMsg(MatchableWarningID) {
  def msg(using Context) =
    val kind = if pattern then "pattern selector" else "value"
    i"""${kind} should be an instance of Matchable,,
       |but it has unmatchable type $tp instead"""

  def explain(using Context) =
    if pattern then
      i"""A value of type $tp cannot be the selector of a match expression
         |since it is not constrained to be `Matchable`. Matching on unconstrained
         |values is disallowed since it can uncover implementation details that
         |were intended to be hidden and thereby can violate paramtetricity laws
         |for reasoning about programs.
         |
         |The restriction can be overridden by appending `.asMatchable` to
         |the selector value. `asMatchable` needs to be imported from
         |scala.compiletime. Example:
         |
         |    import compiletime.asMatchable
         |    def f[X](x: X) = x.asMatchable match { ... }"""
    else
      i"""The value can be converted to a `Matchable` by appending `.asMatchable`.
         |`asMatchable` needs to be imported from scala.compiletime."""
}

class SeqWildcardPatternPos()(using Context)
extends SyntaxMsg(SeqWildcardPatternPosID) {
  def msg(using Context) = i"""${hl("*")} can be used only for last argument"""
  def explain(using Context) = {
    val code =
      """def sumOfTheFirstTwo(list: List[Int]): Int = list match {
        |  case List(first, second, x*) => first + second
        |  case _ => 0
        |}"""
    i"""|Sequence wildcard pattern is expected at the end of an argument list.
        |This pattern matches any remaining elements in a sequence.
        |Consider the following example:
        |
        |$code
        |
        |Calling:
        |
        |${hl("sumOfTheFirstTwo(List(1, 2, 10))")}
        |
        |would give 3 as a result"""
  }
}

class IllegalStartOfSimplePattern()(using Context)
extends SyntaxMsg(IllegalStartOfSimplePatternID) {
  def msg(using Context) = "pattern expected"
  def explain(using Context) = {
    val sipCode =
      """def f(x: Int, y: Int) = x match
        |    case `y` => ...""".stripMargin
    val constructorPatternsCode =
      """case class Person(name: String, age: Int)
        |
        |  def test(p: Person) = p match
        |    case Person(name, age) => ...""".stripMargin
    val tuplePatternsCode =
      """def swap(tuple: (String, Int)): (Int, String) = tuple match
        |    case (text, number) => (number, text)""".stripMargin
    val patternSequencesCode =
      """def getSecondValue(list: List[Int]): Int = list match
        |    case List(_, second, x*) => second
        |    case _ => 0""".stripMargin
    i"""|Simple patterns can be divided into several groups:
        |- Variable Patterns: ${hl("case x => ...")} or ${hl("case _ => ...")}
        |  It matches any value, and binds the variable name to that value.
        |  A special case is the wild-card pattern _ which is treated as if it was a fresh
        |  variable on each occurrence.
        |
        |- Typed Patterns: ${hl("case x: Int => ...")} or ${hl("case _: Int => ...")}
        |  This pattern matches any value matched by the specified type; it binds the variable
        |  name to that value.
        |
        |- Given Patterns: ${hl("case given ExecutionContext => ...")}
        |  This pattern matches any value matched by the specified type; it binds a ${hl("given")}
        |  instance with the same type to that value.
        |
        |- Literal Patterns: ${hl("case 123 => ...")} or ${hl("case 'A' => ...")}
        |  This type of pattern matches any value that is equal to the specified literal.
        |
        |- Stable Identifier Patterns:
        |
        |  ${hl(sipCode)}
        |
        |  the match succeeds only if the x argument and the y argument of f are equal.
        |
        |- Constructor Patterns:
        |
        |  ${hl(constructorPatternsCode)}
        |
        |  The pattern binds all object's fields to the variable names (name and age, in this
        |  case).
        |
        |- Tuple Patterns:
        |
        |  ${hl(tuplePatternsCode)}
        |
        |  Calling:
        |
        |  ${hl("""swap(("Luftballons", 99))""")}
        |
        |  would give ${hl("""(99, "Luftballons")""")} as a result.
        |
        |- Pattern Sequences:
        |
        |  ${hl(patternSequencesCode)}
        |
        |  Calling:
        |
        |  ${hl("getSecondValue(List(1, 10, 2))")}
        |
        |  would give 10 as a result.
        |  This pattern is possible because a companion object for the List class has a method
        |  with the following signature:
        |
        |  ${hl("def unapplySeq[A](x: List[A]): Some[List[A]]")}
        |"""
  }
}

class PkgDuplicateSymbol(existing: Symbol)(using Context)
extends NamingMsg(PkgDuplicateSymbolID) {
  def msg(using Context) = i"Trying to define package with same name as $existing"
  def explain(using Context) = ""
}

class ExistentialTypesNoLongerSupported()(using Context)
extends SyntaxMsg(ExistentialTypesNoLongerSupportedID) {
  def msg(using Context) =
    i"""|Existential types are no longer supported -
        |use a wildcard or dependent type instead"""
  def explain(using Context) =
    i"""|The use of existential types is no longer supported.
        |
        |You should use a wildcard or dependent type instead.
        |
        |For example:
        |
        |Instead of using ${hl("forSome")} to specify a type variable
        |
        |${hl("List[T forSome { type T }]")}
        |
        |Try using a wildcard type variable
        |
        |${hl("List[?]")}
        |"""
}

class UnboundWildcardType()(using Context)
extends SyntaxMsg(UnboundWildcardTypeID) {
  def msg(using Context) = "Unbound wildcard type"
  def explain(using Context) =
    i"""|The wildcard type syntax (${hl("_")}) was used where it could not be bound.
        |Replace ${hl("_")} with a non-wildcard type. If the type doesn't matter,
        |try replacing ${hl("_")} with ${hl("Any")}.
        |
        |Examples:
        |
        |- Parameter lists
        |
        |  Instead of:
        |    ${hl("def foo(x: _) = ...")}
        |
        |  Use ${hl("Any")} if the type doesn't matter:
        |    ${hl("def foo(x: Any) = ...")}
        |
        |- Type arguments
        |
        |  Instead of:
        |    ${hl("val foo = List[?](1, 2)")}
        |
        |  Use:
        |    ${hl("val foo = List[Int](1, 2)")}
        |
        |- Type bounds
        |
        |  Instead of:
        |    ${hl("def foo[T <: _](x: T) = ...")}
        |
        |  Remove the bounds if the type doesn't matter:
        |    ${hl("def foo[T](x: T) = ...")}
        |
        |- ${hl("val")} and ${hl("def")} types
        |
        |  Instead of:
        |    ${hl("val foo: _ = 3")}
        |
        |  Use:
        |    ${hl("val foo: Int = 3")}
        |"""
}

class OverridesNothing(member: Symbol)(using Context)
extends DeclarationMsg(OverridesNothingID) {
  def msg(using Context) = i"""${member} overrides nothing"""

  def explain(using Context) =
    i"""|There must be a field or method with the name ${member.name} in a super
        |class of ${member.owner} to override it. Did you misspell it?
        |Are you extending the right classes?
        |"""
}

class OverridesNothingButNameExists(member: Symbol, existing: List[Denotations.SingleDenotation])(using Context)
extends DeclarationMsg(OverridesNothingButNameExistsID) {
  def msg(using Context) =
    val what =
      if !existing.exists(_.symbol.hasTargetName(member.targetName))
      then "target name"
      else "signature"
    i"""${member} has a different $what than the overridden declaration"""
  def explain(using Context) =
    val existingDecl: String = existing.map(_.showDcl).mkString("  \n")
    i"""|There must be a non-final field or method with the name ${member.name} and the
        |same parameter list in a super class of ${member.owner} to override it.
        |
        |  ${member.showDcl}
        |
        |The super classes of ${member.owner} contain the following members
        |named ${member.name}:
        |  ${existingDecl}
        |"""
}

class OverrideError(
    core: Context ?=> String, base: Type,
    member: Symbol, other: Symbol,
    memberTp: Type, otherTp: Type)(using Context)
extends DeclarationMsg(OverrideErrorID), NoDisambiguation:
  def msg(using Context) =
    val isConcreteOverAbstract =
      (other.owner isSubClass member.owner) && other.is(Deferred) && !member.is(Deferred)
    def addendum =
      if isConcreteOverAbstract then
        i"""|
            |(Note that ${err.infoStringWithLocation(other, base)} is abstract,
            |and is therefore overridden by concrete ${err.infoStringWithLocation(member, base)})"""
        else ""
    i"""error overriding ${err.infoStringWithLocation(other, base)};
        |  ${err.infoString(member, base, showLocation = member.owner != base.typeSymbol)} $core$addendum"""
  override def canExplain =
    memberTp.exists && otherTp.exists
  def explain(using Context) =
    if canExplain then err.whyNoMatchStr(memberTp, otherTp) else ""

class ForwardReferenceExtendsOverDefinition(value: Symbol, definition: Symbol)(using Context)
extends ReferenceMsg(ForwardReferenceExtendsOverDefinitionID) {
  def msg(using Context) = i"${definition.name} is a forward reference extending over the definition of ${value.name}"

  def explain(using Context) =
    i"""|${definition.name} is used before you define it, and the definition of ${value.name}
        |appears between that use and the definition of ${definition.name}.
        |
        |Forward references are allowed only, if there are no value definitions between
        |the reference and the referred method definition.
        |
        |Define ${definition.name} before it is used,
        |or move the definition of ${value.name} so it does not appear between
        |the declaration of ${definition.name} and its use,
        |or define ${value.name} as lazy.
        |"""
}

class ExpectedTokenButFound(expected: Token, found: Token, prefix: String = "")(using Context)
extends SyntaxMsg(ExpectedTokenButFoundID) {

  private def foundText = Tokens.showToken(found)

  def msg(using Context) =
    val expectedText =
      if (Tokens.isIdentifier(expected)) "an identifier"
      else Tokens.showToken(expected)
    i"""$prefix$expectedText expected, but $foundText found"""

  def explain(using Context) =
    if (Tokens.isIdentifier(expected) && Tokens.isKeyword(found))
      s"""
        |If you want to use $foundText as identifier, you may put it in backticks: `${Tokens.tokenString(found)}`.""".stripMargin
    else
      ""
}

class MixedLeftAndRightAssociativeOps(op1: Name, op2: Name, op2LeftAssoc: Boolean)(using Context)
extends SyntaxMsg(MixedLeftAndRightAssociativeOpsID) {
  def msg(using Context) =
    val op1Asso: String = if (op2LeftAssoc) "which is right-associative" else "which is left-associative"
    val op2Asso: String = if (op2LeftAssoc) "which is left-associative" else "which is right-associative"
    i"${op1} (${op1Asso}) and ${op2} ($op2Asso) have same precedence and may not be mixed"
  def explain(using Context) =
    s"""|The operators ${op1} and ${op2} are used as infix operators in the same expression,
        |but they bind to different sides:
        |${op1} is applied to the operand to its ${if (op2LeftAssoc) "right" else "left"}
        |${op2} is applied to the operand to its ${if (op2LeftAssoc) "left" else "right"}
        |As both have the same precedence the compiler can't decide which to apply first.
        |
        |You may use parenthesis to make the application order explicit,
        |or use method application syntax operand1.${op1}(operand2).
        |
        |Operators ending in a colon ${hl(":")} are right-associative. All other operators are left-associative.
        |
        |Infix operator precedence is determined by the operator's first character. Characters are listed
        |below in increasing order of precedence, with characters on the same line having the same precedence.
        |  (all letters)
        |  |
        |  ^
        |  &
        |  = !
        |  < >
        |  :
        |  + -
        |  * / %
        |  (all other special characters)
        |Operators starting with a letter have lowest precedence, followed by operators starting with `|`, etc.
        |""".stripMargin
}

class CantInstantiateAbstractClassOrTrait(cls: Symbol, isTrait: Boolean)(using Context)
extends TypeMsg(CantInstantiateAbstractClassOrTraitID) {
  private val traitOrAbstract = if (isTrait) "a trait" else "abstract"
  def msg(using Context) = i"""${cls.name} is ${traitOrAbstract}; it cannot be instantiated"""
  def explain(using Context) =
    i"""|Abstract classes and traits need to be extended by a concrete class or object
        |to make their functionality accessible.
        |
        |You may want to create an anonymous class extending ${cls.name} with
        |  ${s"class ${cls.name} { }"}
        |
        |or add a companion object with
        |  ${s"object ${cls.name} extends ${cls.name}"}
        |
        |You need to implement any abstract members in both cases.
        |"""
}

class UnreducibleApplication(tycon: Type)(using Context) extends TypeMsg(UnreducibleApplicationID):
  def msg(using Context) = i"unreducible application of higher-kinded type $tycon to wildcard arguments"
  def explain(using Context) =
    i"""|An abstract type constructor cannot be applied to wildcard arguments.
        |Such applications are equivalent to existential types, which are not
        |supported in Scala 3."""

class OverloadedOrRecursiveMethodNeedsResultType(val ex: CyclicReference)(using Context)
extends CyclicMsg(OverloadedOrRecursiveMethodNeedsResultTypeID) {
  def msg(using Context) = i"""Overloaded or recursive $cycleSym needs return type$context"""
  def explain(using Context) =
    i"""Case 1: $cycleSym is overloaded
        |If there are multiple methods named $cycleSym and at least one definition of
        |it calls another, you need to specify the calling method's return type.
        |
        |Case 2: $cycleSym is recursive
        |If $cycleSym calls itself on any path (even through mutual recursion), you need to specify the return type
        |of $cycleSym or of a definition it's mutually recursive with.
        |"""
}

class RecursiveValueNeedsResultType(val ex: CyclicReference)(using Context)
extends CyclicMsg(RecursiveValueNeedsResultTypeID) {
  def msg(using Context) = i"""Recursive $cycleSym needs type$context"""
  def explain(using Context) =
    i"""The definition of $cycleSym is recursive and you need to specify its type.
        |"""
}

class CyclicReferenceInvolving(val ex: CyclicReference)(using Context)
extends CyclicMsg(CyclicReferenceInvolvingID) {
  def msg(using Context) =
    val where = if ex.denot.exists then s" involving ${ex.denot}" else ""
    i"Cyclic reference$where$context"
  def explain(using Context) =
    i"""|${ex.denot} is declared as part of a cycle which makes it impossible for the
        |compiler to decide upon ${ex.denot.name}'s type.
        |To avoid this error, try giving ${ex.denot.name} an explicit type.
        |"""
}

class CyclicReferenceInvolvingImplicit(val ex: CyclicReference)(using Context)
extends CyclicMsg(CyclicReferenceInvolvingImplicitID) {
  def msg(using Context) = i"""Cyclic reference involving implicit $cycleSym$context"""
  def explain(using Context) =
    i"""|$cycleSym is declared as part of a cycle which makes it impossible for the
        |compiler to decide upon ${cycleSym.name}'s type.
        |This might happen when the right hand-side of $cycleSym's definition involves an implicit search.
        |To avoid this error, try giving ${cycleSym.name} an explicit type.
        |"""
}

class SkolemInInferred(tree: tpd.Tree, pt: Type, argument: tpd.Tree)(using Context)
extends TypeMsg(SkolemInInferredID):
  def msg(using Context) =
    def argStr =
      if argument.isEmpty then ""
      else i" from argument of type ${argument.tpe.widen}"
    i"""Failure to generate given instance for type $pt$argStr)
        |
        |I found: $tree
        |But the part corresponding to `<skolem>` is not a reference that can be generated.
        |This might be because resolution yielded as given instance a function that is not
        |known to be total and side-effect free."""
  def explain(using Context) =
    i"""The part of given resolution that corresponds to `<skolem>` produced a term that
        |is not a stable reference. Therefore a given instance could not be generated.
        |
        |To trouble-shoot the problem, try to supply an explicit expression instead of
        |relying on implicit search at this point."""

class SuperQualMustBeParent(qual: untpd.Ident, cls: ClassSymbol)(using Context)
extends ReferenceMsg(SuperQualMustBeParentID) {
  def msg(using Context) = i"""|$qual does not name a parent of $cls"""
  def explain(using Context) =
    val parents: Seq[String] = (cls.info.parents map (_.typeSymbol.name.show)).sorted
    i"""|When a qualifier ${hl("T")} is used in a ${hl("super")} prefix of the form ${hl("C.super[T]")},
        |${hl("T")} must be a parent type of ${hl("C")}.
        |
        |In this case, the parents of $cls are:
        |${parents.mkString("  - ", "\n  - ", "")}
        |"""
}

class VarArgsParamMustComeLast()(using Context)
extends SyntaxMsg(VarArgsParamMustComeLastID) {
  def msg(using Context) = i"""${hl("varargs")} parameter must come last"""
  def explain(using Context) =
    i"""|The ${hl("varargs")} field must be the last field in the method signature.
        |Attempting to define a field in a method signature after a ${hl("varargs")} field is an error.
        |"""
}

class VarArgsParamCannotBeGiven(isGiven: Boolean)(using Context)
extends SyntaxMsg(VarArgsParamCannotBeGivenID) {
  def msg(using Context) = i"repeated parameters are not allowed in a ${if isGiven then "using" else "implicit"} clause"
  def explain(using Context) =
    "It is not possible to define a given with a repeated parameter type. This hypothetical given parameter could always be satisfied by providing 0 arguments, which defeats the purpose of a given argument."
}


import typer.Typer.BindingPrec

class ConstrProxyShadows(proxy: TermRef, shadowed: Type, shadowedIsApply: Boolean)(using Context)
  extends ReferenceMsg(ConstrProxyShadowsID), NoDisambiguation:

  def clsString(using Context) = proxy.symbol.companionClass.showLocated
  def shadowedString(using Context) = shadowed.termSymbol.showLocated
  def appClause = if shadowedIsApply then " the apply method of" else ""
  def appSuffix = if shadowedIsApply then ".apply" else ""

  def msg(using Context) =
    i"""Reference to constructor proxy for $clsString
       |shadows outer reference to $shadowedString
       |
       |The instance needs to be created with an explicit `new`."""

  def explain(using Context) =
    i"""There is an ambiguity in the meaning of the call
       |
       |   ${proxy.symbol.name}(...)
       |
       |It could mean creating an instance of $clsString with
       |
       |   new ${proxy.symbol.companionClass.name}(...)
       |
       |Or it could mean calling$appClause $shadowedString as in
       |
       |   ${shadowed.termSymbol.name}$appSuffix(...)
       |
       |To disambiguate, use an explicit `new` if you mean the former,
       |or use a full prefix for ${shadowed.termSymbol.name} if you mean the latter."""
end ConstrProxyShadows

class AmbiguousReference(
    name: Name, newPrec: BindingPrec, prevPrec: BindingPrec, prevCtx: Context, isExtension: => Boolean = false)(using Context)
  extends ReferenceMsg(AmbiguousReferenceID), NoDisambiguation {

  /** A string which explains how something was bound; Depending on `prec` this is either
    *      imported by <tree>
    *  or  defined in <symbol>
    */
  private def bindingString(prec: BindingPrec, whereFound: Context, qualifier: String = "")(using Context) = {
    val howVisible = prec match {
      case BindingPrec.Definition => "defined"
      case BindingPrec.Inheritance => "inherited"
      case BindingPrec.NamedImport => "imported by name"
      case BindingPrec.WildImport => "imported"
      case BindingPrec.PackageClause => "found"
      case BindingPrec.NothingBound => assert(false)
    }
    if (prec.isImportPrec) {
      i"""$howVisible$qualifier by ${whereFound.importInfo}"""
    } else
      i"""$howVisible$qualifier in ${whereFound.owner}"""
  }

  def importHint =
    if (newPrec == BindingPrec.NamedImport || newPrec == BindingPrec.WildImport)
        && prevPrec == newPrec
        && isExtension
    then i"\n\n Hint: This error may arise if extension method `$name` is called as a normal method."
    else ""

  def msg(using Context) =
    i"""|Reference to $name is ambiguous.
        |It is both ${bindingString(newPrec, ctx)}
        |and ${bindingString(prevPrec, prevCtx, " subsequently")}$importHint"""

  def explain(using Context) =
    val precedent =
      if newPrec == prevPrec then                 """two name bindings of equal precedence
        |were introduced in the same scope.""".stripMargin
      else                                        """a name binding of lower precedence
        |in an inner scope cannot shadow a binding with higher precedence in
        |an outer scope.""".stripMargin

    i"""|The identifier $name is ambiguous because $precedent
        |
        |The precedence of the different kinds of name bindings, from highest to lowest, is:
        | - Definitions in an enclosing scope
        | - Inherited definitions and top-level definitions in packages
        | - Names introduced by import of a specific name
        | - Names introduced by wildcard import
        | - Definitions from packages in other files
        |Note:
        | - As a rule, definitions take precedence over imports.
        | - Definitions in an enclosing scope take precedence over inherited definitions,
        |   which can result in ambiguities in nested classes.
        | - When importing, you can avoid naming conflicts by renaming:
        |   ${hl("import")} scala.{$name => ${name.show}Tick}
        |"""
}

class MethodDoesNotTakeParameters(tree: tpd.Tree)(using Context)
extends TypeMsg(MethodDoesNotTakeParametersId) {
  def methodSymbol(using Context): Symbol =
    def recur(t: tpd.Tree): Symbol =
      val sym = tpd.methPart(t).symbol
      if sym == defn.Any_typeCast then
        t match
          case TypeApply(Select(qual, _), _) => recur(qual)
          case _ => sym
      else sym
    recur(tree)

  def msg(using Context) = {
    val more = if (tree.isInstanceOf[tpd.Apply]) " more" else ""
    val meth = methodSymbol
    val methStr = if (meth.exists) meth.showLocated else "expression"
    i"$methStr does not take$more parameters"
  }

  def explain(using Context) = {
    val isNullary = methodSymbol.info.isInstanceOf[ExprType]
    val addendum =
      if (isNullary) "\nNullary methods may not be called with parenthesis"
      else ""

    "You have specified more parameter lists than defined in the method definition(s)." + addendum
  }

}

class AmbiguousOverload(tree: tpd.Tree, val alternatives: List[SingleDenotation], pt: Type, addendum: String = "")(
  implicit ctx: Context)
extends ReferenceMsg(AmbiguousOverloadID), NoDisambiguation {
  private def all = if (alternatives.length == 2) "both" else "all"
  def msg(using Context) =
    i"""|Ambiguous overload. The ${err.overloadedAltsStr(alternatives)}
        |$all match ${err.expectedTypeStr(pt)}$addendum"""
  def explain(using Context) =
    i"""|There are ${alternatives.length} methods that could be referenced as the compiler knows too little
        |about the expected type.
        |You may specify the expected type e.g. by
        |- assigning it to a value with a specified type, or
        |- adding a type ascription as in ${hl("instance.myMethod: String => Int")}
        |"""
}

class AmbiguousExtensionMethod(tree: untpd.Tree, expansion1: tpd.Tree, expansion2: tpd.Tree)(using Context)
  extends ReferenceMsg(AmbiguousExtensionMethodID), NoDisambiguation:
  def msg(using Context) =
    i"""Ambiguous extension methods:
       |both $expansion1
       |and  $expansion2
       |are possible expansions of $tree"""
  def explain(using Context) = ""

class ReassignmentToVal(name: Name)(using Context)
  extends TypeMsg(ReassignmentToValID) {
  def msg(using Context) = i"""Reassignment to val $name"""
  def explain(using Context) =
    i"""|You can not assign a new value to $name as values can't be changed.
        |Keep in mind that every statement has a value, so you may e.g. use
        |  ${hl("val")} $name ${hl("= if (condition) 2 else 5")}
        |In case you need a reassignable name, you can declare it as
        |variable
        |  ${hl("var")} $name ${hl("=")} ...
        |"""
}

class TypeDoesNotTakeParameters(tpe: Type, params: List[untpd.Tree])(using Context)
  extends TypeMsg(TypeDoesNotTakeParametersID) {
  private def fboundsAddendum(using Context) =
    if tpe.typeSymbol.isAllOf(Provisional | TypeParam) then
      "\n(Note that F-bounds of type parameters may not be type lambdas)"
    else ""
  def msg(using Context) = i"$tpe does not take type parameters$fboundsAddendum"
  def explain(using Context) =
    val ps =
      if (params.size == 1) s"a type parameter ${params.head}"
      else s"type parameters ${params.map(_.show).mkString(", ")}"
    i"""You specified ${NoColor(ps)} for $tpe, which is not
        |declared to take any.
        |"""
}

class VarValParametersMayNotBeCallByName(name: TermName, mutable: Boolean)(using Context)
  extends SyntaxMsg(VarValParametersMayNotBeCallByNameID) {
  def varOrVal = if mutable then hl("var") else hl("val")
  def msg(using Context) = s"$varOrVal parameters may not be call-by-name"
  def explain(using Context) =
    i"""${hl("var")} and ${hl("val")} parameters of classes and traits may no be call-by-name. In case you
        |want the parameter to be evaluated on demand, consider making it just a parameter
        |and a ${hl("def")} in the class such as
        |  ${s"class MyClass(${name}Tick: => String) {"}
        |  ${s"  def $name() = ${name}Tick"}
        |  ${hl("}")}
        |"""
}

class MissingTypeParameterFor(tpe: Type)(using Context)
  extends SyntaxMsg(MissingTypeParameterForID) {
  def msg(using Context) =
    if tpe.derivesFrom(defn.AnyKindClass)
    then i"$tpe cannot be used as a value type"
    else i"Missing type parameter for $tpe"
  def explain(using Context) = ""
}

class MissingTypeParameterInTypeApp(tpe: Type)(using Context)
  extends TypeMsg(MissingTypeParameterInTypeAppID) {
  def numParams = tpe.typeParams.length
  def parameters = if (numParams == 1) "parameter" else "parameters"
  def msg(using Context) = i"Missing type $parameters for $tpe"
  def explain(using Context) = i"A fully applied type is expected but $tpe takes $numParams $parameters"
}

class MissingArgument(pname: Name, methString: String)(using Context)
  extends TypeMsg(MissingArgumentID):
  def msg(using Context) =
    if pname.firstPart contains '$' then s"not enough arguments for $methString"
    else s"missing argument for parameter $pname of $methString"
  def explain(using Context) = ""

class MissingArgumentList(method: String, sym: Symbol)(using Context)
  extends TypeMsg(MissingArgumentListID) {
  def msg(using Context) =
    val symDcl = if sym.exists then "\n\n  " + hl(sym.showDcl(using ctx.withoutColors)) else ""
    i"missing argument list for $method$symDcl"
  def explain(using Context) = {
    i"""Unapplied methods are only converted to functions when a function type is expected."""
  }
}

class DoesNotConformToBound(tpe: Type, which: String, bound: Type)(using Context)
  extends TypeMismatchMsg(
    if which == "lower" then bound else tpe,
    if which == "lower" then tpe else bound)(DoesNotConformToBoundID):
  private def isBounds = tpe match
    case TypeBounds(lo, hi) => lo ne hi
    case _ => false
  override def canExplain = !isBounds
  def msg(using Context) =
    if isBounds then
      i"Type argument ${tpe} does not overlap with $which bound $bound"
    else
      i"Type argument ${tpe} does not conform to $which bound $bound"

class DoesNotConformToSelfType(category: String, selfType: Type, cls: Symbol,
                                otherSelf: Type, relation: String, other: Symbol)(
  implicit ctx: Context)
  extends TypeMismatchMsg(selfType, otherSelf)(DoesNotConformToSelfTypeID) {
  def msg(using Context) = i"""$category: self type $selfType of $cls does not conform to self type $otherSelf
                |of $relation $other"""
}

class DoesNotConformToSelfTypeCantBeInstantiated(tp: Type, selfType: Type)(
  implicit ctx: Context)
  extends TypeMismatchMsg(tp, selfType)(DoesNotConformToSelfTypeCantBeInstantiatedID) {
  def msg(using Context) = i"""$tp does not conform to its self type $selfType; cannot be instantiated"""
}

class IllegalParameterInit(found: Type, expected: Type, param: Symbol, cls: Symbol)(using Context)
  extends TypeMismatchMsg(found, expected)(IllegalParameterInitID):
  def msg(using Context) =
    i"""illegal parameter initialization of $param.
        |
        |  The argument passed for $param has type: $found
        |  but $cls expects $param to have type: $expected"""

class AbstractMemberMayNotHaveModifier(sym: Symbol, flag: FlagSet)(
  implicit ctx: Context)
  extends SyntaxMsg(AbstractMemberMayNotHaveModifierID) {
  def msg(using Context) = i"""${hl("abstract")} $sym may not have `${flag.flagsString}` modifier"""
  def explain(using Context) = ""
}

class TypesAndTraitsCantBeImplicit()(using Context)
  extends SyntaxMsg(TypesAndTraitsCantBeImplicitID) {
  def msg(using Context) = i"""${hl("implicit")} modifier cannot be used for types or traits"""
  def explain(using Context) = ""
}

class OnlyClassesCanBeAbstract(sym: Symbol)(
  implicit ctx: Context)
  extends SyntaxMsg(OnlyClassesCanBeAbstractID) {
  def explain(using Context) = ""
  def msg(using Context) = i"""${hl("abstract")} modifier can be used only for classes; it should be omitted for abstract members"""
}

class AbstractOverrideOnlyInTraits(sym: Symbol)(
  implicit ctx: Context)
  extends SyntaxMsg(AbstractOverrideOnlyInTraitsID) {
  def msg(using Context) = i"""${hl("abstract override")} modifier only allowed for members of traits"""
  def explain(using Context) = ""
}

class TraitsMayNotBeFinal(sym: Symbol)(
  implicit ctx: Context)
  extends SyntaxMsg(TraitsMayNotBeFinalID) {
  def msg(using Context) = i"""$sym may not be ${hl("final")}"""
  def explain(using Context) =
    "A trait can never be final since it is abstract and must be extended to be useful."
}

class NativeMembersMayNotHaveImplementation(sym: Symbol)(
  implicit ctx: Context)
  extends SyntaxMsg(NativeMembersMayNotHaveImplementationID) {
  def msg(using Context) = i"""${hl("@native")} members may not have an implementation"""
  def explain(using Context) = ""
}

class TraitMayNotDefineNativeMethod(sym: Symbol)(
  implicit ctx: Context)
  extends SyntaxMsg(TraitMayNotDefineNativeMethodID) {
  def msg(using Context) = i"""A trait cannot define a ${hl("@native")} method."""
  def explain(using Context) = ""
}

class OnlyClassesCanHaveDeclaredButUndefinedMembers(sym: Symbol)(
  implicit ctx: Context)
  extends SyntaxMsg(OnlyClassesCanHaveDeclaredButUndefinedMembersID) {

  def msg(using Context) = i"""Declaration of $sym not allowed here: only classes can have declared but undefined members"""
  def explain(using Context) =
    if sym.is(Mutable) then "Note that variables need to be initialized to be defined."
    else ""
}

class CannotExtendAnyVal(sym: Symbol)(using Context)
  extends SyntaxMsg(CannotExtendAnyValID) {
  def msg(using Context) = i"""$sym cannot extend ${hl("AnyVal")}"""
  def explain(using Context) =
    if sym.is(Trait) then
      i"""Only classes (not traits) are allowed to extend ${hl("AnyVal")}, but traits may extend
          |${hl("Any")} to become ${Green("\"universal traits\"")} which may only have ${hl("def")} members.
          |Universal traits can be mixed into classes that extend ${hl("AnyVal")}.
          |"""
    else if sym.is(Module) then
      i"""Only classes (not objects) are allowed to extend ${hl("AnyVal")}.
          |"""
    else ""
}

class CannotExtendJavaEnum(sym: Symbol)(using Context)
  extends SyntaxMsg(CannotExtendJavaEnumID) {
    def msg(using Context) = i"""$sym cannot extend ${hl("java.lang.Enum")}: only enums defined with the ${hl("enum")} syntax can"""
    def explain(using Context) = ""
  }

class CannotExtendContextFunction(sym: Symbol)(using Context)
  extends SyntaxMsg(CannotExtendFunctionID) {
    def msg(using Context) = i"""$sym cannot extend a context function class"""
    def explain(using Context) = ""
  }

class JavaEnumParentArgs(parent: Type)(using Context)
  extends TypeMsg(JavaEnumParentArgsID) {
    def msg(using Context) = i"""not enough arguments for constructor Enum: ${hl("(name: String, ordinal: Int)")}: ${hl(parent.show)}"""
    def explain(using Context) = ""
  }

class CannotHaveSameNameAs(sym: Symbol, cls: Symbol, reason: CannotHaveSameNameAs.Reason)(using Context)
  extends NamingMsg(CannotHaveSameNameAsID) {
  import CannotHaveSameNameAs.*
  def reasonMessage(using Context): String = reason match {
    case CannotBeOverridden => "class definitions cannot be overridden"
    case DefinedInSelf(self) =>
      s"""cannot define ${sym.showKind} member with the same name as a ${cls.showKind} member in self reference ${self.name}.
          |(Note: this can be resolved by using another name)
          |""".stripMargin
  }

  def msg(using Context) = i"""$sym cannot have the same name as ${cls.showLocated} -- """ + reasonMessage
  def explain(using Context) = ""
}
object CannotHaveSameNameAs {
  sealed trait Reason
  case object CannotBeOverridden extends Reason
  case class DefinedInSelf(self: tpd.ValDef) extends Reason
}

class ValueClassesMayNotDefineInner(valueClass: Symbol, inner: Symbol)(using Context)
  extends SyntaxMsg(ValueClassesMayNotDefineInnerID) {
  def msg(using Context) = i"""Value classes may not define an inner class"""
  def explain(using Context) = ""
}

class ValueClassesMayNotDefineNonParameterField(valueClass: Symbol, field: Symbol)(using Context)
  extends SyntaxMsg(ValueClassesMayNotDefineNonParameterFieldID) {
  def msg(using Context) = i"""Value classes may not define non-parameter field"""
  def explain(using Context) = ""
}

class ValueClassesMayNotDefineASecondaryConstructor(valueClass: Symbol, constructor: Symbol)(using Context)
  extends SyntaxMsg(ValueClassesMayNotDefineASecondaryConstructorID) {
  def msg(using Context) = i"""Value classes may not define a secondary constructor"""
  def explain(using Context) = ""
}

class ValueClassesMayNotContainInitalization(valueClass: Symbol)(using Context)
  extends SyntaxMsg(ValueClassesMayNotContainInitalizationID) {
  def msg(using Context) = i"""Value classes may not contain initialization statements"""
  def explain(using Context) = ""
}

class ValueClassesMayNotBeAbstract(valueClass: Symbol)(using Context)
  extends SyntaxMsg(ValueClassesMayNotBeAbstractID) {
  def msg(using Context) = i"""Value classes may not be ${hl("abstract")}"""
  def explain(using Context) = ""
}

class ValueClassesMayNotBeContainted(valueClass: Symbol)(using Context)
  extends SyntaxMsg(ValueClassesMayNotBeContaintedID) {
  private def localOrMember = if (valueClass.owner.isTerm) "local class" else "member of another class"
  def msg(using Context) = s"""Value classes may not be a $localOrMember"""
  def explain(using Context) = ""
}

class ValueClassesMayNotWrapAnotherValueClass(valueClass: Symbol)(using Context)
  extends SyntaxMsg(ValueClassesMayNotWrapAnotherValueClassID) {
  def msg(using Context) = """A value class may not wrap another user-defined value class"""
  def explain(using Context) = ""
}

class ValueClassParameterMayNotBeAVar(valueClass: Symbol, param: Symbol)(using Context)
  extends SyntaxMsg(ValueClassParameterMayNotBeAVarID) {
  def msg(using Context) = i"""A value class parameter may not be a ${hl("var")}"""
  def explain(using Context) =
    i"""A value class must have exactly one ${hl("val")} parameter."""
}

class ValueClassNeedsOneValParam(valueClass: Symbol)(using Context)
  extends SyntaxMsg(ValueClassNeedsExactlyOneValParamID) {
  def msg(using Context) = i"""Value class needs one ${hl("val")} parameter"""
  def explain(using Context) = ""
}

class ValueClassParameterMayNotBeCallByName(valueClass: Symbol, param: Symbol)(using Context)
  extends SyntaxMsg(ValueClassParameterMayNotBeCallByNameID) {
  def msg(using Context) = s"Value class parameter `${param.name}` may not be call-by-name"
  def explain(using Context) = ""
}

class SuperCallsNotAllowedInlineable(symbol: Symbol)(using Context)
  extends SyntaxMsg(SuperCallsNotAllowedInlineableID) {
  def msg(using Context) = i"Super call not allowed in inlineable $symbol"
  def explain(using Context) = "Method inlining prohibits calling superclass methods, as it may lead to confusion about which super is being called."
}

class NotAPath(tp: Type, usage: String)(using Context) extends TypeMsg(NotAPathID):
  def msg(using Context) = i"$tp is not a valid $usage, since it is not an immutable path"
  def explain(using Context) =
    i"""An immutable path is
        | - a reference to an immutable value, or
        | - a reference to `this`, or
        | - a selection of an immutable path with an immutable value."""

class WrongNumberOfParameters(tree: untpd.Tree, foundCount: Int, pt: Type, expectedCount: Int)(using Context)
  extends SyntaxMsg(WrongNumberOfParametersID) {
  def msg(using Context) = s"Wrong number of parameters, expected: $expectedCount"
  def explain(using Context) =
    val ending = if foundCount == 1 then "" else "s"
    i"""The function literal
       |
       |    $tree
       |
       |has $foundCount parameter$ending. But the expected type
       |
       |    $pt
       |
       |requires a function with $expectedCount parameters."""
}

class DuplicatePrivateProtectedQualifier()(using Context)
  extends SyntaxMsg(DuplicatePrivateProtectedQualifierID) {
  def msg(using Context) = "Duplicate private/protected qualifier"
  def explain(using Context) =
    i"It is not allowed to combine `private` and `protected` modifiers even if they are qualified to different scopes"
}

class ExpectedStartOfTopLevelDefinition()(using Context)
  extends SyntaxMsg(ExpectedStartOfTopLevelDefinitionID) {
  def msg(using Context) = "Expected start of definition"
  def explain(using Context) =
    i"You have to provide either ${hl("class")}, ${hl("trait")}, ${hl("object")}, or ${hl("enum")} definitions after qualifiers"
}

class NoReturnFromInlineable(owner: Symbol)(using Context)
  extends SyntaxMsg(NoReturnFromInlineableID) {
  def msg(using Context) = i"No explicit ${hl("return")} allowed from inlineable $owner"
  def explain(using Context) =
    i"""Methods marked with ${hl("inline")} modifier may not use ${hl("return")} statements.
        |Instead, you should rely on the last expression's value being
        |returned from a method.
        |"""
}

class ReturnOutsideMethodDefinition(owner: Symbol)(using Context)
  extends SyntaxMsg(ReturnOutsideMethodDefinitionID) {
  def msg(using Context) = i"${hl("return")} outside method definition"
  def explain(using Context) =
    i"""You used ${hl("return")} in ${owner}.
        |${hl("return")} is a keyword and may only be used within method declarations.
        |"""
}

class ExtendFinalClass(clazz:Symbol, finalClazz: Symbol)(using Context)
  extends SyntaxMsg(ExtendFinalClassID) {
  def msg(using Context) = i"$clazz cannot extend ${hl("final")} $finalClazz"
  def explain(using Context) =
    i"""A class marked with the ${hl("final")} keyword cannot be extended"""
}

class ExpectedTypeBoundOrEquals(found: Token)(using Context)
  extends SyntaxMsg(ExpectedTypeBoundOrEqualsID) {
  def msg(using Context) = i"${hl("=")}, ${hl(">:")}, or ${hl("<:")} expected, but ${Tokens.showToken(found)} found"

  def explain(using Context) =
    i"""Type parameters and abstract types may be constrained by a type bound.
        |Such type bounds limit the concrete values of the type variables and possibly
        |reveal more information about the members of such types.
        |
        |A lower type bound ${hl("B >: A")} expresses that the type variable ${hl("B")}
        |refers to a supertype of type ${hl("A")}.
        |
        |An upper type bound ${hl("T <: A")} declares that type variable ${hl("T")}
        |refers to a subtype of type ${hl("A")}.
        |"""
}

class ClassAndCompanionNameClash(cls: Symbol, other: Symbol)(using Context)
  extends NamingMsg(ClassAndCompanionNameClashID) {
  def msg(using Context) =
    val name = cls.name.stripModuleClassSuffix
    i"Name clash: both ${cls.owner} and its companion object defines $name"
  def explain(using Context) =
    i"""|A ${cls.kindString} and its companion object cannot both define a ${hl("class")}, ${hl("trait")} or ${hl("object")} with the same name:
        |  - ${cls.owner} defines ${cls}
        |  - ${other.owner} defines ${other}"""
}

class TailrecNotApplicable(symbol: Symbol)(using Context)
  extends SyntaxMsg(TailrecNotApplicableID) {
  def msg(using Context) = {
    val reason =
      if !symbol.is(Method) then i"$symbol isn't a method"
      else if symbol.is(Deferred) then i"$symbol is abstract"
      else if !symbol.isEffectivelyFinal then i"$symbol is neither ${hl("private")} nor ${hl("final")} so can be overridden"
      else i"$symbol contains no recursive calls"

    s"TailRec optimisation not applicable, $reason"
  }
  def explain(using Context) = ""
}

class FailureToEliminateExistential(tp: Type, tp1: Type, tp2: Type, boundSyms: List[Symbol], classRoot: Symbol)(using Context)
  extends Message(FailureToEliminateExistentialID) {
  def kind = MessageKind.Compatibility
  def msg(using Context) =
    val originalType = ctx.printer.dclsText(boundSyms, "; ").show
    i"""An existential type that came from a Scala-2 classfile for $classRoot
        |cannot be mapped accurately to a Scala-3 equivalent.
        |original type    : $tp forSome ${originalType}
        |reduces to       : $tp1
        |type used instead: $tp2
        |This choice can cause follow-on type errors or hide type errors.
        |Proceed at own risk."""
  def explain(using Context) =
    i"""Existential types in their full generality are no longer supported.
        |Scala-3 does applications of class types to wildcard type arguments.
        |Other forms of existential types that come from Scala-2 classfiles
        |are only approximated in a best-effort way."""
}

class OnlyFunctionsCanBeFollowedByUnderscore(tp: Type, tree: untpd.PostfixOp)(using Context)
  extends SyntaxMsg(OnlyFunctionsCanBeFollowedByUnderscoreID) {
  def msg(using Context) = i"Only function types can be followed by ${hl("_")} but the current expression has type $tp"
  def explain(using Context) =
    i"""The syntax ${hl("x _")} is no longer supported if ${hl("x")} is not a function.
        |To convert to a function value, you need to explicitly write ${hl("() => x")}"""

  override def actions(using Context) =
    import scala.language.unsafeNulls
    val untpd.PostfixOp(qual, Ident(nme.WILDCARD)) = tree: @unchecked
    List(
      CodeAction(title = "Rewrite to function value",
        description = None,
        patches = List(
          ActionPatch(SourcePosition(tree.source, Span(tree.span.start)), "(() => "),
          ActionPatch(SourcePosition(tree.source, Span(qual.span.end, tree.span.end)), ")")
        )
      )
    )
}

class MissingEmptyArgumentList(method: String, tree: tpd.Tree)(using Context)
  extends SyntaxMsg(MissingEmptyArgumentListID) {
  def msg(using Context) = i"$method must be called with ${hl("()")} argument"
  def explain(using Context) = {
    val codeExample =
      """def next(): T = ...
        |next     // is expanded to next()"""

    i"""Previously an empty argument list () was implicitly inserted when calling a nullary method without arguments. E.g.
        |
        |$codeExample
        |
        |In Dotty, this idiom is an error. The application syntax has to follow exactly the parameter syntax.
        |Excluded from this rule are methods that are defined in Java or that override methods defined in Java."""
  }

  override def actions(using Context) =
    import scala.language.unsafeNulls
    List(
      CodeAction(title = "Insert ()",
        description = None,
        patches = List(
          ActionPatch(SourcePosition(tree.source, tree.span.endPos), "()"),
        )
      )
    )
}

class DuplicateNamedTypeParameter(name: Name)(using Context)
  extends SyntaxMsg(DuplicateNamedTypeParameterID) {
  def msg(using Context) = i"Type parameter $name was defined multiple times."
  def explain(using Context) = ""
}

class UndefinedNamedTypeParameter(undefinedName: Name, definedNames: List[Name])(using Context)
  extends SyntaxMsg(UndefinedNamedTypeParameterID) {
  def msg(using Context) = i"Type parameter $undefinedName is undefined. Expected one of ${definedNames.map(_.show).mkString(", ")}."
  def explain(using Context) = ""
}

class IllegalStartOfStatement(what: String, isModifier: Boolean, isStat: Boolean)(using Context) extends SyntaxMsg(IllegalStartOfStatementID) {
  def msg(using Context) =
    if isStat then
      "this kind of statement is not allowed here"
    else
      val addendum = if isModifier then ": this modifier is not allowed here" else ""
      s"Illegal start of $what$addendum"
  def explain(using Context) =
    i"""A statement is an import or export, a definition or an expression.
        |Some statements are only allowed in certain contexts"""
}

class TraitIsExpected(symbol: Symbol)(using Context) extends SyntaxMsg(TraitIsExpectedID) {
  def msg(using Context) = i"$symbol is not a trait"
  def explain(using Context) = {
    val errorCodeExample =
      """class A
        |class B
        |
        |val a = new A with B // will fail with a compile error - class B is not a trait""".stripMargin
    val codeExample =
      """class A
        |trait B
        |
        |val a = new A with B // compiles normally""".stripMargin

    i"""Only traits can be mixed into classes using a ${hl("with")} keyword.
        |Consider the following example:
        |
        |$errorCodeExample
        |
        |The example mentioned above would fail because B is not a trait.
        |But if you make B a trait it will be compiled without any errors:
        |
        |$codeExample
        |"""
  }
}

class TraitRedefinedFinalMethodFromAnyRef(method: Symbol)(using Context) extends SyntaxMsg(TraitRedefinedFinalMethodFromAnyRefID) {
  def msg(using Context) = i"Traits cannot redefine final $method from ${hl("class AnyRef")}."
  def explain(using Context) = ""
}

class AlreadyDefined(name: Name, owner: Symbol, conflicting: Symbol)(using Context)
extends NamingMsg(AlreadyDefinedID):
  def msg(using Context) =
    def where: String =
      if conflicting.effectiveOwner.is(Package) && conflicting.associatedFile != null then
        i" in ${conflicting.associatedFile}"
      else if conflicting.owner == owner then ""
      else i" in ${conflicting.owner}"
    def note =
      if owner.is(Method) || conflicting.is(Method) then
        "\n\nNote that overloaded methods must all be defined in the same group of toplevel definitions"
      else ""
    if conflicting.isTerm != name.isTermName then
      i"$name clashes with $conflicting$where; the two must be defined together"
    else
      i"$name is already defined as $conflicting$where$note"
  def explain(using Context) = ""

class PackageNameAlreadyDefined(pkg: Symbol)(using Context) extends NamingMsg(PackageNameAlreadyDefinedID) {
  def msg(using Context) =
    def where = if pkg.associatedFile == null then "" else s" in ${pkg.associatedFile}"
    i"""${pkg.name} is the name of $pkg$where.
        |It cannot be used at the same time as the name of a package."""
  def explain(using Context) =
    def or = if pkg.associatedFile == null then "" else " or delete the containing class file"
    i"""An ${hl("object")} or other toplevel definition cannot have the same name as an existing ${hl("package")}.
        |Rename either one of them$or."""
}

class UnapplyInvalidNumberOfArguments(qual: untpd.Tree, argTypes: List[Type])(using Context)
  extends SyntaxMsg(UnapplyInvalidNumberOfArgumentsID) {
  def msg(using Context) = i"Wrong number of argument patterns for $qual; expected: ($argTypes%, %)"
  def explain(using Context) =
    i"""The Unapply method of $qual was used with incorrect number of arguments.
        |Expected usage would be something like:
        |case $qual(${argTypes.map(_ => '_')}%, %) => ...
        |
        |where subsequent arguments would have following types: ($argTypes%, %).
        |"""
}

class UnapplyInvalidReturnType(unapplyResult: Type, unapplyName: Name)(using Context)
  extends DeclarationMsg(UnapplyInvalidReturnTypeID) {
  def msg(using Context) =
    val addendum =
      if Feature.migrateTo3 && unapplyName == nme.unapplySeq
      then "\nYou might want to try to rewrite the extractor to use `unapply` instead."
      else ""
    i"""| ${Red(i"$unapplyResult")} is not a valid result type of an $unapplyName method of an ${Magenta("extractor")}.$addendum"""
  def explain(using Context) = if (unapplyName.show == "unapply")
    i"""
        |To be used as an extractor, an unapply method has to return a type that either:
        | - has members ${Magenta("isEmpty: Boolean")} and ${Magenta("get: S")} (usually an ${Green("Option[S]")})
        | - is a ${Green("Boolean")}
        | - is a ${Green("Product")} (like a ${Magenta("Tuple2[T1, T2]")}) of arity i with i >= 1, and has members _1 to _i
        |
        |See: https://docs.scala-lang.org/scala3/reference/changed-features/pattern-matching.html#fixed-arity-extractors
        |
        |Examples:
        |
        |class A(val i: Int)
        |
        |object B {
        |  def unapply(a: A): ${Green("Option[Int]")} = Some(a.i)
        |}
        |
        |object C {
        |  def unapply(a: A): ${Green("Boolean")} = a.i == 2
        |}
        |
        |object D {
        |  def unapply(a: A): ${Green("(Int, Int)")} = (a.i, a.i)
        |}
        |
        |object Test {
        |  def test(a: A) = a match {
        |    ${Magenta("case B(1)")} => 1
        |    ${Magenta("case a @ C()")} => 2
        |    ${Magenta("case D(3, 3)")} => 3
        |  }
        |}
      """
  else
    i"""
        |To be used as an extractor, an unapplySeq method has to return a type which has members
        |${Magenta("isEmpty: Boolean")} and ${Magenta("get: S")} where ${Magenta("S <: Seq[V]")} (usually an ${Green("Option[Seq[V]]")}):
        |
        |object CharList {
        |  def unapplySeq(s: String): ${Green("Option[Seq[Char]")} = Some(s.toList)
        |
        |  "example" match {
        |    ${Magenta("case CharList(c1, c2, c3, c4, _, _, _)")} =>
        |      println(s"$$c1,$$c2,$$c3,$$c4")
        |    case _ =>
        |      println("Expected *exactly* 7 characters!")
        |  }
        |}
      """
}

class StaticFieldsOnlyAllowedInObjects(member: Symbol)(using Context) extends SyntaxMsg(StaticFieldsOnlyAllowedInObjectsID) {
  def msg(using Context) = i"${hl("@static")} $member in ${member.owner} must be defined inside a static ${hl("object")}."
  def explain(using Context) =
    i"${hl("@static")} members are only allowed inside objects."
}

class StaticFieldsShouldPrecedeNonStatic(member: Symbol, defns: List[tpd.Tree])(using Context) extends SyntaxMsg(StaticFieldsShouldPrecedeNonStaticID) {
  def msg(using Context) = i"${hl("@static")} $member in ${member.owner} must be defined before non-static fields."
  def explain(using Context) = {
    val nonStatics = defns.takeWhile(_.symbol != member).take(3).filter(_.isInstanceOf[tpd.ValDef])
    val codeExample = s"""object ${member.owner.name.firstPart} {
                      |  @static ${member} = ...
                      |  ${nonStatics.map(m => s"${m.symbol} = ...").mkString("\n  ")}
                      |  ...
                      |}"""
    i"""The fields annotated with @static should precede any non @static fields.
      |This ensures that we do not introduce surprises for users in initialization order of this class.
      |Static field are initialized when class loading the code of Foo.
      |Non static fields are only initialized the first  time that Foo is accessed.
      |
      |The definition of ${member.name} should have been before the non ${hl("@static val")}s:
      |$codeExample
      |"""
  }
}

class CyclicInheritance(symbol: Symbol, addendum: => String)(using Context) extends SyntaxMsg(CyclicInheritanceID) {
  def msg(using Context) = i"Cyclic inheritance: $symbol extends itself$addendum"
  def explain(using Context) = {
    val codeExample = "class A extends A"

    i"""Cyclic inheritance is prohibited in Dotty.
        |Consider the following example:
        |
        |$codeExample
        |
        |The example mentioned above would fail because this type of inheritance hierarchy
        |creates a "cycle" where a not yet defined class A extends itself which makes
        |impossible to instantiate an object of this class"""
  }
}

class BadSymbolicReference(denot: SymDenotation)(using Context)
extends ReferenceMsg(BadSymbolicReferenceID) {
  def msg(using Context) = {
    val denotationOwner = denot.owner
    val denotationName = ctx.fresh.setSetting(ctx.settings.YdebugNames, true).printer.nameString(denot.name)
    val file = denot.symbol.associatedFile
    val (location, src) =
      if (file != null) (s" in $file", file.toString)
      else ("", "the signature")

    i"""Bad symbolic reference. A signature$location
        |refers to $denotationName in ${denotationOwner.showKind} ${denotationOwner.showFullName} which is not available.
        |It may be completely missing from the current classpath, or the version on
        |the classpath might be incompatible with the version used when compiling $src."""
  }

  def explain(using Context) = ""
}

class UnableToExtendSealedClass(pclazz: Symbol)(using Context) extends SyntaxMsg(UnableToExtendSealedClassID) {
  def msg(using Context) = i"Cannot extend ${hl("sealed")} $pclazz in a different source file"
  def explain(using Context) = "A sealed class or trait can only be extended in the same file as its declaration"
}

class SymbolHasUnparsableVersionNumber(symbol: Symbol, errorMessage: String)(using Context)
extends SyntaxMsg(SymbolHasUnparsableVersionNumberID) {
  def msg(using Context) = i"${symbol.showLocated} has an unparsable version number: $errorMessage"
  def explain(using Context) =
    i"""The ${symbol.showLocated} is marked with ${hl("@migration")} indicating it has changed semantics
        |between versions and the ${hl("-Xmigration")} settings is used to warn about constructs
        |whose behavior may have changed since version change."""
}

class SymbolChangedSemanticsInVersion(
  symbol: Symbol,
  migrationVersion: ScalaVersion,
  migrationMessage: String
)(using Context) extends SyntaxMsg(SymbolChangedSemanticsInVersionID) {
  def msg(using Context) = i"${symbol.showLocated} has changed semantics in version $migrationVersion: $migrationMessage"
  def explain(using Context) =
    i"""The ${symbol.showLocated} is marked with ${hl("@migration")} indicating it has changed semantics
        |between versions and the ${hl("-Xmigration")} settings is used to warn about constructs
        |whose behavior may have changed since version change."""
}

class UnableToEmitSwitch()(using Context)
extends SyntaxMsg(UnableToEmitSwitchID) {
  def msg(using Context) = i"Could not emit switch for ${hl("@switch")} annotated match"
  def explain(using Context) = {
    val codeExample =
      """val ConstantB = 'B'
        |final val ConstantC = 'C'
        |def tokenMe(ch: Char) = (ch: @switch) match {
        |  case '\t' | '\n' => 1
        |  case 'A'         => 2
        |  case ConstantB   => 3  // a non-literal may prevent switch generation: this would not compile
        |  case ConstantC   => 4  // a constant value is allowed
        |  case _           => 5
        |}""".stripMargin

    i"""If annotated with ${hl("@switch")}, the compiler will verify that the match has been compiled to a
        |tableswitch or lookupswitch and issue an error if it instead compiles into a series of conditional
        |expressions. Example usage:
        |
        |$codeExample
        |
        |The compiler will not apply the optimisation if:
        |- the matched value is not of type ${hl("Int")}, ${hl("Byte")}, ${hl("Short")} or ${hl("Char")}
        |- the matched value is not a constant literal
        |- there are less than three cases"""
  }
}

class MissingCompanionForStatic(member: Symbol)(using Context)
extends SyntaxMsg(MissingCompanionForStaticID) {
  def msg(using Context) = i"${member.owner} does not have a companion class"
  def explain(using Context) =
    i"An object that contains ${hl("@static")} members must have a companion class."
}

class PolymorphicMethodMissingTypeInParent(rsym: Symbol, parentSym: Symbol)(using Context)
extends SyntaxMsg(PolymorphicMethodMissingTypeInParentID) {
  def msg(using Context) = i"Polymorphic refinement $rsym without matching type in parent $parentSym is no longer allowed"
  def explain(using Context) =
    i"""Polymorphic $rsym is not allowed in the structural refinement of $parentSym because
        |$rsym does not override any method in $parentSym. Structural refinement does not allow for
        |polymorphic methods."""
}

class ParamsNoInline(owner: Symbol)(using Context)
  extends SyntaxMsg(ParamsNoInlineID) {
  def msg(using Context) = i"""${hl("inline")} modifier can only be used for parameters of inline methods"""
  def explain(using Context) = ""
}

class JavaSymbolIsNotAValue(symbol: Symbol)(using Context) extends TypeMsg(JavaSymbolIsNotAValueID) {
  def msg(using Context) =
    val kind =
      if symbol is Package then i"$symbol"
      else i"Java defined ${hl("class " + symbol.name)}"
    s"$kind is not a value"
  def explain(using Context) = ""
}

class DoubleDefinition(decl: Symbol, previousDecl: Symbol, base: Symbol)(using Context)
extends NamingMsg(DoubleDefinitionID) {
  def msg(using Context) = {
    def nameAnd = if (decl.name != previousDecl.name) " name and" else ""
    def erasedType = if ctx.erasedTypes then i" ${decl.info}" else ""
    def details(using Context): String =
      if (decl.isRealMethod && previousDecl.isRealMethod) {
        import Signature.MatchDegree.*

        // compare the signatures when both symbols represent methods
        decl.signature.matchDegree(previousDecl.signature) match {
          case NoMatch =>
            // If the signatures don't match at all at the current phase, then
            // they might match after erasure.
            if ctx.phase.id <= elimErasedValueTypePhase.id then
              atPhase(elimErasedValueTypePhase.next)(details)
            else
              "" // shouldn't be reachable
          case ParamMatch =>
            "have matching parameter types."
          case MethodNotAMethodMatch =>
            "neither has parameters."
          case FullMatch =>
            val hint =
              if !decl.hasAnnotation(defn.TargetNameAnnot)
                  && !previousDecl.hasAnnotation(defn.TargetNameAnnot)
              then
                i"""
                    |
                    |Consider adding a @targetName annotation to one of the conflicting definitions
                    |for disambiguation."""
              else ""
            i"have the same$nameAnd type$erasedType after erasure.$hint"
        }
      }
      else ""
    def symLocation(sym: Symbol) = {
      val lineDesc =
        if (sym.span.exists && sym.span != sym.owner.span)
          s" at line ${sym.srcPos.line + 1}"
        else ""
      i"in ${sym.owner}${lineDesc}"
    }
    val clashDescription =
      if (decl.owner eq previousDecl.owner)
        "Double definition"
      else if ((decl.owner eq base) || (previousDecl eq base))
        "Name clash between defined and inherited member"
      else
        "Name clash between inherited members"

    atPhase(typerPhase) {
      i"""$clashDescription:
          |${previousDecl.showDcl} ${symLocation(previousDecl)} and
          |${decl.showDcl} ${symLocation(decl)}
          |"""
    } + details
  }
  def explain(using Context) = ""
}

class ImportedTwice(sel: Name)(using Context) extends SyntaxMsg(ImportedTwiceID) {
  def msg(using Context) = s"${sel.show} is imported twice on the same import line."
  def explain(using Context) = ""
}

class UnimportedAndImported(sel: Name, isImport: Boolean)(using Context) extends SyntaxMsg(UnimportedAndImportedID) {
  def msg(using Context) =
    val otherStr = if isImport then "and imported" else "twice"
    s"${sel.show} is unimported $otherStr on the same import line."
  def explain(using Context) = ""
}

class TypeTestAlwaysDiverges(scrutTp: Type, testTp: Type)(using Context) extends SyntaxMsg(TypeTestAlwaysDivergesID) {
  def msg(using Context) =
    s"This type test will never return a result since the scrutinee type ${scrutTp.show} does not contain any value."
  def explain(using Context) = ""
}

// Relative of CyclicReferenceInvolvingImplicit and RecursiveValueNeedsResultType
class TermMemberNeedsResultTypeForImplicitSearch(val ex: CyclicReference)(using Context)
  extends CyclicMsg(TermMemberNeedsNeedsResultTypeForImplicitSearchID) {
  def msg(using Context) = i"""$cycleSym needs result type because its right-hand side attempts implicit search$context"""
  def explain(using Context) =
    i"""|The right hand-side of $cycleSym's definition requires an implicit search at the highlighted position.
        |To avoid this error, give `$cycleSym` an explicit type.
        |"""
}

class ClassCannotExtendEnum(cls: Symbol, parent: Symbol)(using Context) extends SyntaxMsg(ClassCannotExtendEnumID) {
  def msg(using Context) = i"""$cls in ${cls.owner} extends enum ${parent.name}, but extending enums is prohibited."""
  def explain(using Context) = ""
}

class NotAnExtractor(tree: untpd.Tree)(using Context) extends PatternMatchMsg(NotAnExtractorID) {
  def msg(using Context) = i"$tree cannot be used as an extractor in a pattern because it lacks an unapply or unapplySeq method"
  def explain(using Context) =
    i"""|An ${hl("unapply")} method should be defined in an ${hl("object")} as follow:
        |  - If it is just a test, return a ${hl("Boolean")}. For example ${hl("case even()")}
        |  - If it returns a single sub-value of type T, return an ${hl("Option[T]")}
        |  - If it returns several sub-values T1,...,Tn, group them in an optional tuple ${hl("Option[(T1,...,Tn)]")}
        |
        |Sometimes, the number of sub-values isn't fixed and we would like to return a sequence.
        |For this reason, you can also define patterns through ${hl("unapplySeq")} which returns ${hl("Option[Seq[T]]")}.
        |This mechanism is used for instance in pattern ${hl("case List(x1, ..., xn)")}"""
}

class ExtractorNotFound(val name: Name)(using Context) extends NotFoundMsg(ExtractorNotFoundID):
  def msg(using Context) = i"no pattern match extractor named $name was found"
  def explain(using Context) =
    i"""An application $name(...) in a pattern can refer to an extractor
       |which defines an unapply or unapplySeq method. Example:
       |
       |  object split:
       |    def unapply(x: String) =
       |      val (leading, trailing) = x.splitAt(x.length / 2)
       |      Some((leading, trailing))
       |
       |  val split(fst, snd) = "HiHo"
       |
       |The extractor pattern `split(fst, snd)` defines `fst` as the first half "Hi" and
       |`snd` as the second half "Ho" of the right hand side "HiHo". Case classes and
       |enum cases implicitly define extractors with the name of the class or enum case.
       |Here, no extractor named $name was found, so the pattern could not be typed."""

class MemberWithSameNameAsStatic()(using Context)
  extends SyntaxMsg(MemberWithSameNameAsStaticID) {
  def msg(using Context) = i"Companion classes cannot define members with same name as a ${hl("@static")} member"
  def explain(using Context) = ""
}

class PureExpressionInStatementPosition(stat: untpd.Tree, val exprOwner: Symbol)(using Context)
  extends Message(PureExpressionInStatementPositionID) {
  def kind = MessageKind.PotentialIssue
  def msg(using Context) = "A pure expression does nothing in statement position"
  def explain(using Context) =
    i"""The pure expression $stat doesn't have any side effect and its result is not assigned elsewhere.
        |It can be removed without changing the semantics of the program. This may indicate an error."""
}

class PureUnitExpression(stat: untpd.Tree, tpe: Type)(using Context)
  extends Message(PureUnitExpressionID) {
  def kind = MessageKind.PotentialIssue
  def msg(using Context) = i"Discarded non-Unit value of type ${tpe.widen}. You may want to use `()`."
  def explain(using Context) =
    i"""As this expression is not of type Unit, it is desugared into `{ $stat; () }`.
       |Here the `$stat` expression is a pure statement that can be discarded.
       |Therefore the expression is effectively equivalent to `()`."""
}

class UnqualifiedCallToAnyRefMethod(stat: untpd.Tree, method: Symbol)(using Context)
  extends Message(UnqualifiedCallToAnyRefMethodID) {
  def kind = MessageKind.PotentialIssue
  def msg(using Context) = i"Suspicious top-level unqualified call to ${hl(method.name.toString)}"
  def explain(using Context) =
    val getClassExtraHint =
      if method.name == nme.getClass_ && ctx.settings.classpath.value.contains("scala3-staging") then
        i"""\n\n
           |This class should not be used to get the classloader for `scala.quoted.staging.Compile.make`."""
      else ""
    i"""Top-level unqualified calls to ${hl("AnyRef")} or ${hl("Any")} methods such as ${hl(method.name.toString)} are
       |resolved to calls on ${hl("Predef")} or on imported methods. This might not be what
       |you intended.$getClassExtraHint"""
}

class SynchronizedCallOnBoxedClass(stat: tpd.Tree)(using Context)
  extends Message(SynchronizedCallOnBoxedClassID) {
  def kind = MessageKind.PotentialIssue
  def msg(using Context) = i"Suspicious ${hl("synchronized")} call on boxed class"
  def explain(using Context) =
    i"""|You called the ${hl("synchronized")} method on a boxed primitive. This might not be what
        |you intended."""
}

class ExtensionNullifiedByMember(method: Symbol, target: Symbol)(using Context)
  extends Message(ExtensionNullifiedByMemberID):
  def kind = MessageKind.PotentialIssue
  def msg(using Context) =
    i"""Extension method ${hl(method.name.toString)} will never be selected
       |because ${hl(target.name.toString)} already has a member with the same name."""
  def explain(using Context) =
    i"""An extension method can be invoked as a regular method, but if that is intended,
       |it should not be defined as an extension.
       |Although extensions can be overloaded, they do not overload existing member methods."""

class TraitCompanionWithMutableStatic()(using Context)
  extends SyntaxMsg(TraitCompanionWithMutableStaticID) {
  def msg(using Context) = i"Companion of traits cannot define mutable @static fields"
  def explain(using Context) = ""
}

class LazyStaticField()(using Context)
  extends SyntaxMsg(LazyStaticFieldID) {
  def msg(using Context) = i"Lazy @static fields are not supported"
  def explain(using Context) = ""
}

class StaticOverridingNonStaticMembers()(using Context)
  extends SyntaxMsg(StaticOverridingNonStaticMembersID) {
  def msg(using Context) = i"${hl("@static")} members cannot override or implement non-static ones"
  def explain(using Context) = ""
}

class OverloadInRefinement(rsym: Symbol)(using Context)
  extends DeclarationMsg(OverloadInRefinementID) {
  def msg(using Context) = "Refinements cannot introduce overloaded definitions"
  def explain(using Context) =
    i"""The refinement `$rsym` introduces an overloaded definition.
        |Refinements cannot contain overloaded definitions."""
}

class NoMatchingOverload(val alternatives: List[SingleDenotation], pt: Type)(using Context)
  extends TypeMsg(NoMatchingOverloadID) {
  def msg(using Context) =
    i"""None of the ${err.overloadedAltsStr(alternatives)}
        |match ${err.expectedTypeStr(pt)}"""
  def explain(using Context) = ""
}
class StableIdentPattern(tree: untpd.Tree, pt: Type)(using Context)
  extends TypeMsg(StableIdentPatternID) {
  def msg(using Context) =
    i"""Stable identifier required, but $tree found"""
  def explain(using Context) = ""
}

class IllegalSuperAccessor(base: Symbol, memberName: Name, targetName: Name,
    acc: Symbol, accTp: Type,
    other: Symbol, otherTp: Type)(using Context) extends DeclarationMsg(IllegalSuperAccessorID) {
  def msg(using Context) = {
    // The mixin containing a super-call that requires a super-accessor
    val accMixin = acc.owner
    // The class or trait that the super-accessor should resolve too in `base`
    val otherMixin = other.owner
    // The super-call in `accMixin`
    val superCall = hl(i"super.$memberName")
    // The super-call that the super-accesors in `base` forwards to
    val resolvedSuperCall = hl(i"super[${otherMixin.name}].$memberName")
    // The super-call that we would have called if `super` in traits behaved like it
    // does in classes, i.e. followed the linearization of the trait itself.
    val staticSuperCall = {
      val staticSuper = accMixin.asClass.info.parents.reverse
        .find(_.nonPrivateMember(memberName)
          .matchingDenotation(accMixin.thisType, acc.info, targetName).exists)
      val staticSuperName = staticSuper match {
        case Some(parent) =>
          parent.classSymbol.name.show
        case None => // Might be reachable under separate compilation
          "SomeParent"
      }
      hl(i"super[$staticSuperName].$memberName")
    }
    i"""$base cannot be defined due to a conflict between its parents when
       |implementing a super-accessor for $memberName in $accMixin:
       |
       |1. One of its parent (${accMixin.name}) contains a call $superCall in its body,
       |   and when a super-call in a trait is written without an explicit parent
       |   listed in brackets, it is implemented by a generated super-accessor in
       |   the class that extends this trait based on the linearization order of
       |   the class.
       |2. Because ${otherMixin.name} comes before ${accMixin.name} in the linearization
       |   order of ${base.name}, and because ${otherMixin.name} overrides $memberName,
       |   the super-accessor in ${base.name} is implemented as a call to
       |   $resolvedSuperCall.
       |3. However,
       |   ${otherTp.widenExpr} (the type of $resolvedSuperCall in ${base.name})
       |   is not a subtype of
       |   ${accTp.widenExpr} (the type of $memberName in $accMixin).
       |   Hence, the super-accessor that needs to be generated in ${base.name}
       |   is illegal.
       |
       |Here are two possible ways to resolve this:
       |
       |1. Change the linearization order of ${base.name} such that
       |   ${accMixin.name} comes before ${otherMixin.name}.
       |2. Alternatively, replace $superCall in the body of $accMixin by a
       |   super-call to a specific parent, e.g. $staticSuperCall
       |"""
  }
  def explain(using Context) = ""
}

class TraitParameterUsedAsParentPrefix(cls: Symbol)(using Context)
  extends DeclarationMsg(TraitParameterUsedAsParentPrefixID) {
  def msg(using Context) =
    s"${cls.show} cannot extend from a parent that is derived via its own parameters"
  def explain(using Context) =
    i"""
       |The parent class/trait that ${cls.show} extends from is obtained from
       |the parameter of ${cls.show}. This is disallowed in order to prevent
       |outer-related Null Pointer Exceptions in Scala.
       |
       |In order to fix this issue consider directly extending from the parent rather
       |than obtaining it from the parameters of ${cls.show}.
       |"""
}

class UnknownNamedEnclosingClassOrObject(name: TypeName)(using Context)
  extends ReferenceMsg(UnknownNamedEnclosingClassOrObjectID) {
  def msg(using Context) =
    i"""no enclosing class or object is named '${hl(name.show)}'"""
  def explain(using Context) =
    i"""
    |The class or object named '${hl(name.show)}' was used as a visibility
    |modifier, but could not be resolved. Make sure that
    |'${hl(name.show)}' is not misspelled and has been imported into the
    |current scope.
    """
  }

class IllegalCyclicTypeReference(val ex: CyclicReference, sym: Symbol, where: String, lastChecked: Type)(using Context)
  extends CyclicMsg(IllegalCyclicTypeReferenceID) {
  def msg(using Context) =
    val lastCheckedStr =
      try lastChecked.show
      catch case ex: CyclicReference => "..."
    i"illegal cyclic type reference: ${where} ${hl(lastCheckedStr)} of $sym refers back to the type itself$context"
  def explain(using Context) = ""
}

class ErasedTypesCanOnlyBeFunctionTypes()(using Context)
  extends SyntaxMsg(ErasedTypesCanOnlyBeFunctionTypesID) {
  def msg(using Context) = "Types with erased keyword can only be function types `(erased ...) => ...`"
  def explain(using Context) = ""
}

class CaseClassMissingNonImplicitParamList(cdef: untpd.TypeDef)(using Context)
  extends SyntaxMsg(CaseClassMissingNonImplicitParamListID) {
  def msg(using Context) =
    i"""|A ${hl("case class")} must have at least one leading non-implicit parameter list"""

  def explain(using Context) =
    i"""|${cdef.name} must have at least one leading non-implicit parameter list,
        | if you're aiming to have a case class parametrized only by implicit ones, you should
        | add an explicit ${hl("()")} as the first parameter list to ${cdef.name}."""
}

class EnumerationsShouldNotBeEmpty(cdef: untpd.TypeDef)(using Context)
  extends SyntaxMsg(EnumerationsShouldNotBeEmptyID) {
  def msg(using Context) = "Enumerations must contain at least one case"

  def explain(using Context) =
    i"""|Enumeration ${cdef.name} must contain at least one case
        |Example Usage:
        | ${hl("enum")} ${cdef.name} {
        |    ${hl("case")} Option1, Option2
        | }
        |"""
}

class TypedCaseDoesNotExplicitlyExtendTypedEnum(enumDef: Symbol, caseDef: untpd.TypeDef)(using Context)
  extends SyntaxMsg(TypedCaseDoesNotExplicitlyExtendTypedEnumID) {
  def msg(using Context) = i"explicit extends clause needed because both enum case and enum class have type parameters"

  def explain(using Context) =
    i"""Enumerations where the enum class as well as the enum case have type parameters need
        |an explicit extends.
        |for example:
        | ${hl("enum")} ${enumDef.name}[T] {
        |  ${hl("case")} ${caseDef.name}[U](u: U) ${hl("extends")} ${enumDef.name}[U]
        | }
        |"""
}

class IllegalRedefinitionOfStandardKind(kindType: String, name: Name)(using Context)
  extends SyntaxMsg(IllegalRedefinitionOfStandardKindID) {
  def msg(using Context) = i"illegal redefinition of standard $kindType $name"
  def explain(using Context) =
    i"""| "$name" is a standard Scala core `$kindType`
        | Please choose a different name to avoid conflicts
        |"""
}

class NoExtensionMethodAllowed(mdef: untpd.DefDef)(using Context)
  extends SyntaxMsg(NoExtensionMethodAllowedID) {
  def msg(using Context) = i"No extension method allowed here, since collective parameters are given"
  def explain(using Context) =
    i"""|Extension method:
        |  `${mdef}`
        |is defined inside an extension clause which has collective parameters.
        |"""
}

class ExtensionMethodCannotHaveTypeParams(mdef: untpd.DefDef)(using Context)
  extends SyntaxMsg(ExtensionMethodCannotHaveTypeParamsID) {
  def msg(using Context) = i"Extension method cannot have type parameters since some were already given previously"

  def explain(using Context) =
    i"""|Extension method:
        |  `${mdef}`
        |has type parameters `[${mdef.leadingTypeParams.map(_.show).mkString(",")}]`, while the extension clause has
        |it's own type parameters. Please consider moving these to the extension clause's type parameter list.
        |"""
}

class ExtensionCanOnlyHaveDefs(mdef: untpd.Tree)(using Context)
  extends SyntaxMsg(ExtensionCanOnlyHaveDefsID) {
  def msg(using Context) = i"Only methods allowed here, since collective parameters are given"
  def explain(using Context) =
    i"""Extension clauses can only have `def`s
        | `${mdef.show}` is not a valid expression here.
        |"""
}

class UnexpectedPatternForSummonFrom(tree: Tree[?])(using Context)
  extends SyntaxMsg(UnexpectedPatternForSummonFromID) {
  def msg(using Context) = i"Unexpected pattern for summonFrom. Expected ${hl("`x: T`")} or ${hl("`_`")}"
  def explain(using Context) =
    i"""|The pattern "${tree.show}" provided in the ${hl("case")} expression of the ${hl("summonFrom")},
        | needs to be of the form ${hl("`x: T`")} or ${hl("`_`")}.
        |
        | Example usage:
        | inline def a = summonFrom {
        |  case x: T => ???
        | }
        |
        | or
        | inline def a = summonFrom {
        |  case _ => ???
        | }
        |"""
}

class AnonymousInstanceCannotBeEmpty(impl:  untpd.Template)(using Context)
  extends SyntaxMsg(AnonymousInstanceCannotBeEmptyID) {
  def msg(using Context) = i"anonymous instance must implement a type or have at least one extension method"
  def explain(using Context) =
    i"""|Anonymous instances cannot be defined with an empty body. The block
        |`${impl.show}` should either contain an implemented type or at least one extension method.
        |"""
}

class ModifierNotAllowedForDefinition(flag: Flag, explanation: String = "")(using Context)
  extends SyntaxMsg(ModifierNotAllowedForDefinitionID) {
  def msg(using Context) = i"Modifier ${hl(flag.flagsString)} is not allowed for this definition"
  def explain(using Context) = explanation
}

class RedundantModifier(flag: Flag)(using Context)
  extends SyntaxMsg(RedundantModifierID) {
  def msg(using Context) = i"Modifier ${hl(flag.flagsString)} is redundant for this definition"
  def explain(using Context) = ""
}

class InvalidReferenceInImplicitNotFoundAnnotation(typeVar: String, owner: String)(using Context)
  extends ReferenceMsg(InvalidReferenceInImplicitNotFoundAnnotationID) {
  def msg(using Context) = i"""|Invalid reference to a type variable ${hl(typeVar)} found in the annotation argument.
                |The variable does not occur as a parameter in the scope of ${hl(owner)}.
                |"""
  def explain(using Context) = ""
}

class CaseClassInInlinedCode(tree: tpd.Tree)(using Context)
  extends SyntaxMsg(CaseClassInInlinedCodeID) {

  def defKind = if tree.symbol.is(Module) then "object" else "class"
  def msg(using Context) = s"Case $defKind definitions are not allowed in inline methods or quoted code. Use a normal $defKind instead."
  def explain(using Context) =
    i"""Case class/object definitions generate a considerable footprint in code size.
        |Inlining such definition would multiply this footprint for each call site.
        |"""
}

class ImplicitSearchTooLargeWarning(limit: Int, openSearchPairs: List[(Candidate, Type)])(using Context)
  extends TypeMsg(ImplicitSearchTooLargeID):
  override def showAlways = true
  def showQuery(query: (Candidate, Type))(using Context): String =
    i"  ${query._1.ref.symbol.showLocated}  for  ${query._2}}"
  def msg(using Context) =
    i"""Implicit search problem too large.
        |an implicit search was terminated with failure after trying $limit expressions.
        |The root candidate for the search was:
        |
        |${showQuery(openSearchPairs.last)}
        |
        |You can change the behavior by setting the `-Ximplicit-search-limit` value.
        |Smaller values cause the search to fail faster.
        |Larger values might make a very large search problem succeed.
        |"""
  def explain(using Context) =
    i"""The overflow happened with the following lists of tried expressions and target types,
        |starting with the root query:
        |
        |${openSearchPairs.reverse.map(showQuery)}%\n%
      """

class TargetNameOnTopLevelClass(symbol: Symbol)(using Context)
extends SyntaxMsg(TargetNameOnTopLevelClassID):
  def msg(using Context) = i"${hl("@targetName")} annotation not allowed on top-level $symbol"
  def explain(using Context) =
    val annot = symbol.getAnnotation(defn.TargetNameAnnot).get
    i"""The @targetName annotation may be applied to a top-level ${hl("val")} or ${hl("def")}, but not
        |a top-level ${hl("class")}, ${hl("trait")}, or ${hl("object")}.
        |
        |This restriction is due to the naming convention of Java classfiles, whose filenames
        |are based on the name of the class defined within. If @targetName were permitted
        |here, the name of the classfile would be based on the target name, and the compiler
        |could not associate that classfile with the Scala-visible defined name of the class.
        |
        |If your use case requires @targetName, consider wrapping $symbol in an ${hl("object")}
        |(and possibly exporting it), as in the following example:
        |
        |${hl("object Wrapper:")}
        |  $annot $symbol { ... }
        |
        |${hl("export")} Wrapper.${symbol.name}  ${hl("// optional")}"""

class NotClassType(tp: Type)(using Context)
extends TypeMsg(NotClassTypeID), ShowMatchTrace(tp):
  def msg(using Context) = i"$tp is not a class type"
  def explain(using Context) =
    i"""A class type includes classes and traits in a specific order. Defining a class, even an anonymous class,
        |requires specifying a linearization order for the traits it extends. For example, `A & B` is not a class type
        |because it doesn't specify which trait takes precedence, A or B. For more information about class types, please see the Scala Language Specification.
        |Class types also can't have refinements."""

class NotConstant(suffix: String, tp: Type)(using Context)
extends TypeMsg(NotConstantID), ShowMatchTrace(tp):
  def msg(using Context) =
    i"$tp is not a constant type"
    + (if suffix.isEmpty then "" else i"; $suffix")
  def explain(using Context) = ""

class MissingImplicitArgument(
    arg: tpd.Tree,
    pt: Type,
    where: String,
    paramSymWithMethodCallTree: Option[(Symbol, tpd.Tree)] = None,
    ignoredInstanceNormalImport: => Option[SearchSuccess],
    ignoredConvertibleImplicits: => Iterable[TermRef]
  )(using Context) extends TypeMsg(MissingImplicitArgumentID), ShowMatchTrace(pt):

  arg.tpe match
    case ambi: AmbiguousImplicits => withoutDisambiguation()
    case _ =>

  /** Format `raw` implicitNotFound or implicitAmbiguous argument, replacing
   *  all occurrences of `${X}` where `X` is in `paramNames` with the
   *  corresponding shown type in `args`.
   */
  def userDefinedErrorString(raw: String, paramNames: List[String], args: List[Type])(using Context): String =
    def translate(name: String): Option[String] =
      val idx = paramNames.indexOf(name)
      if (idx >= 0) Some(i"${args(idx)}") else None
    """\$\{\s*([^}\s]+)\s*\}""".r.replaceAllIn(raw, (_: Regex.Match) match
      case Regex.Groups(v) => quoteReplacement(translate(v).getOrElse("")).nn
    )

  /** @param rawMsg           Message template with variables, e.g. "Variable A is ${A}"
   *  @param sym              Symbol of the annotated type or of the method whose parameter was annotated
   *  @param substituteType   Function substituting specific types for abstract types associated with variables, e.g A -> Int
   */
  def formatAnnotationMessage(rawMsg: String, sym: Symbol, substituteType: Type => Type)(using Context): String =
    val substitutableTypesSymbols = substitutableTypeSymbolsInScope(sym)
    userDefinedErrorString(
      rawMsg,
      paramNames = substitutableTypesSymbols.map(_.name.unexpandedName.toString),
      args = substitutableTypesSymbols.map(_.typeRef).map(substituteType)
    )

  /** Extract a user defined error message from a symbol `sym`
   *  with an annotation matching the given class symbol `cls`.
   */
  def userDefinedMsg(sym: Symbol, cls: Symbol)(using Context) =
    for
      ann <- sym.getAnnotation(cls)
      msg <- ann.argumentConstantString(0)
    yield msg

  def userDefinedImplicitNotFoundTypeMessageFor(sym: Symbol)(using Context): Option[String] =
    for
      rawMsg <- userDefinedMsg(sym, defn.ImplicitNotFoundAnnot)
      if Feature.migrateTo3 || sym != defn.Function1
        // Don't inherit "No implicit view available..." message if subtypes of Function1 are not treated as implicit conversions anymore
    yield
      val substituteType = (_: Type).asSeenFrom(pt, sym)
      formatAnnotationMessage(rawMsg, sym, substituteType)

  /** Extracting the message from a method parameter, e.g. in
   *
   *  trait Foo
   *
   *  def foo(implicit @annotation.implicitNotFound("Foo is missing") foo: Foo): Any = ???
   */
  def userDefinedImplicitNotFoundParamMessage(using Context): Option[String] =
    paramSymWithMethodCallTree.flatMap: (sym, applTree) =>
      userDefinedMsg(sym, defn.ImplicitNotFoundAnnot).map: rawMsg =>
        val fn = tpd.funPart(applTree)
        val targs = tpd.typeArgss(applTree).flatten
        val methodOwner = fn.symbol.owner
        val methodOwnerType = tpd.qualifier(fn).tpe
        val methodTypeParams = fn.symbol.paramSymss.flatten.filter(_.isType)
        val methodTypeArgs = targs.map(_.tpe)
        val substituteType = (_: Type).asSeenFrom(methodOwnerType, methodOwner).subst(methodTypeParams, methodTypeArgs)
        formatAnnotationMessage(rawMsg, sym.owner, substituteType)

  def userDefinedImplicitNotFoundTypeMessage(using Context): Option[String] =
    def recur(tp: Type): Option[String] = tp match
      case tp: TypeRef =>
        val sym = tp.symbol
        userDefinedImplicitNotFoundTypeMessageFor(sym).orElse(recur(tp.info))
      case tp: ClassInfo =>
        tp.baseClasses.iterator
          .map(userDefinedImplicitNotFoundTypeMessageFor)
          .find(_.isDefined).flatten
      case tp: TypeProxy =>
        recur(tp.superType)
      case tp: AndType =>
        recur(tp.tp1).orElse(recur(tp.tp2))
      case _ =>
        None
    recur(pt)

  /** The implicitNotFound annotation on the parameter, or else on the type.
   *  implicitNotFound message strings starting with `explain=` are intended for
   *  additional explanations, not the message proper. The leading `explain=` is
   *  dropped in this case.
   *  @param explain  The message is used for an additional explanation, not
   *                  the message proper.
   */
  def userDefinedImplicitNotFoundMessage(explain: Boolean)(using Context): Option[String] =
    val explainTag = "explain="
    def filter(msg: Option[String]) = msg match
      case Some(str) =>
        if str.startsWith(explainTag) then
          if explain then Some(str.drop(explainTag.length)) else None
        else if explain then None
        else msg
      case None => None
    filter(userDefinedImplicitNotFoundParamMessage)
      .orElse(filter(userDefinedImplicitNotFoundTypeMessage))

  object AmbiguousImplicitMsg {
    def unapply(search: SearchSuccess): Option[String] =
      userDefinedMsg(search.ref.symbol, defn.ImplicitAmbiguousAnnot)
  }

  def msg(using Context): String =

    def formatMsg(shortForm: String)(headline: String = shortForm) = arg match
      case arg: Trees.SearchFailureIdent[?] =>
        arg.tpe match
          case _: NoMatchingImplicits => headline
          case tpe: SearchFailureType =>
            i"$headline. ${tpe.explanation}"
          case _ => headline
      case _ =>
        arg.tpe match
          case tpe: SearchFailureType =>
            val original = arg match
              case Inlined(call, _, _) => call
              case _ => arg
            i"""$headline.
              |I found:
              |
              |    ${original.show.replace("\n", "\n    ")}
              |
              |But ${tpe.explanation}."""
          case _ => headline

    def location(preposition: String) = if (where.isEmpty) "" else s" $preposition $where"

    def defaultAmbiguousImplicitMsg(ambi: AmbiguousImplicits) =
      s"Ambiguous given instances: ${ambi.explanation}${location("of")}"

    def defaultImplicitNotFoundMessage =
      i"No given instance of type $pt was found${location("for")}"

    /** Construct a custom error message given an ambiguous implicit
     *  candidate `alt` and a user defined message `raw`.
     */
    def userDefinedAmbiguousImplicitMsg(alt: SearchSuccess, raw: String) = {
      val params = alt.ref.underlying match {
        case p: PolyType => p.paramNames.map(_.toString)
        case _           => Nil
      }
      def resolveTypes(targs: List[tpd.Tree])(using Context) =
        targs.map(a => Inferencing.fullyDefinedType(a.tpe, "type argument", a.srcPos))

      // We can extract type arguments from:
      //   - a function call:
      //     @implicitAmbiguous("msg A=${A}")
      //     implicit def f[A](): String = ...
      //     implicitly[String] // found: f[Any]()
      //
      //   - an eta-expanded function:
      //     @implicitAmbiguous("msg A=${A}")
      //     implicit def f[A](x: Int): String = ...
      //     implicitly[Int => String] // found: x => f[Any](x)

      val call = tpd.closureBody(alt.tree) // the tree itself if not a closure
      val targs = tpd.typeArgss(call).flatten
      val args = resolveTypes(targs)(using ctx.fresh.setTyperState(alt.tstate))
      userDefinedErrorString(raw, params, args)
    }

    /** Extracting the message from a type, e.g. in
     *
     *  @annotation.implicitNotFound("Foo is missing")
     *  trait Foo
     *
     *  def foo(implicit foo: Foo): Any = ???
     */
    arg.tpe match
      case ambi: AmbiguousImplicits =>
        (ambi.alt1, ambi.alt2) match
          case (alt @ AmbiguousImplicitMsg(msg), _) =>
            userDefinedAmbiguousImplicitMsg(alt, msg)
          case (_, alt @ AmbiguousImplicitMsg(msg)) =>
            userDefinedAmbiguousImplicitMsg(alt, msg)
          case _ =>
            defaultAmbiguousImplicitMsg(ambi)
      case ambi @ TooUnspecific(target) =>
        i"""No implicit search was attempted${location("for")}
            |since the expected type $target is not specific enough"""
      case _ =>
        val shortMessage = userDefinedImplicitNotFoundMessage(explain = false)
          .getOrElse(defaultImplicitNotFoundMessage)
        formatMsg(shortMessage)()
  end msg

  override def msgPostscript(using Context) =
    arg.tpe match
      case _: AmbiguousImplicits =>
        ""  // show no disambiguation
      case _: TooUnspecific =>
        super.msgPostscript // show just disambigutation and match type trace
      case _ =>
        // show all available additional info
        def hiddenImplicitNote(s: SearchSuccess) =
          i"\n\nNote: ${s.ref.symbol.showLocated} was not considered because it was not imported with `import given`."
        def showImplicitAndConversions(imp: TermRef, convs: Iterable[TermRef]) =
          i"\n- ${imp.symbol.showDcl}${convs.map(c => "\n    - " + c.symbol.showDcl).mkString}"
        def noChainConversionsNote(ignoredConvertibleImplicits: Iterable[TermRef]): Option[String] =
          Option.when(ignoredConvertibleImplicits.nonEmpty)(
            i"\n\nNote: implicit conversions are not automatically applied to arguments of using clauses. " +
            i"You will have to pass the argument explicitly.\n" +
            i"The following implicits in scope can be implicitly converted to ${pt.show}:" +
            ignoredConvertibleImplicits.map { imp => s"\n- ${imp.symbol.showDcl}"}.mkString
          )
        def importSuggestionAddendum: String =
          arg.tpe match
            // If the failure was caused by an underlying NoMatchingImplicits, compute the addendum for its expected type
            case noMatching: NoMatchingImplicits => // FIXME also handle SynthesisFailure
              ctx.typer.importSuggestionAddendum(noMatching.expectedType)
            case _ =>
              ctx.typer.importSuggestionAddendum(pt)
        super.msgPostscript
        ++ ignoredInstanceNormalImport.map(hiddenImplicitNote)
            .orElse(noChainConversionsNote(ignoredConvertibleImplicits))
            .getOrElse(importSuggestionAddendum)

  def explain(using Context) = userDefinedImplicitNotFoundMessage(explain = true)
    .getOrElse("")
end MissingImplicitArgument

class CannotBeAccessed(tpe: NamedType, superAccess: Boolean)(using Context)
extends ReferenceMsg(CannotBeAccessedID):
  def msg(using Context) =
    val pre = tpe.prefix
    val name = tpe.name
    val alts = tpe.denot.alternatives.map(_.symbol).filter(_.exists)
    val whatCanNot = alts match
      case Nil =>
        i"$name cannot"
      case sym :: Nil =>
        i"${if (sym.owner == pre.typeSymbol) sym.show else sym.showLocated} cannot"
      case _ =>
        i"none of the overloaded alternatives named $name can"
    val where = if (ctx.owner.exists) i" from ${ctx.owner.enclosingClass}" else ""
    val whyNot = new StringBuffer
    for alt <- alts do
      val cls = alt.owner.enclosingSubClass
      val owner = if cls.exists then cls else alt.owner
      val location: String =
        if alt.is(Protected) then
          if alt.privateWithin.exists && alt.privateWithin != owner then
            if owner.is(Final) then alt.privateWithin.showLocated
            else alt.privateWithin.showLocated + ", or " + owner.showLocated + " or one of its subclasses"
          else
            if owner.is(Final) then owner.showLocated
            else owner.showLocated + " or one of its subclasses"
        else
          alt.privateWithin.orElse(owner).showLocated
      val accessMod = if alt.is(Protected) then "protected" else "private"
      val within = if alt.privateWithin.exists then i"[${alt.privateWithin.name}]"
        else ""
      whyNot.append(i"""
          |  $accessMod$within $alt can only be accessed from $location.""")
    i"$whatCanNot be accessed as a member of $pre$where.$whyNot"
  def explain(using Context) = ""

class InlineGivenShouldNotBeFunction()(using Context)
extends SyntaxMsg(InlineGivenShouldNotBeFunctionID):
  def msg(using Context) =
    i"""An inline given alias with a function value as right-hand side can significantly increase
       |generated code size. You should either drop the `inline` or rewrite the given with an
       |explicit `apply` method."""
  def explain(using Context) =
    i"""A function value on the right-hand side of an inline given alias expands to
       |an anonymous class. Each application of the inline given will then create a
       |fresh copy of that class, which can increase code size in surprising ways.
       |For that reason, functions are discouraged as right hand sides of inline given aliases.
       |You should either drop `inline` or rewrite to an explicit `apply` method. E.g.
       |
       |    inline given Conversion[A, B] = x => x.toB
       |
       |should be re-formulated as
       |
       |    given Conversion[A, B] with
       |      inline def apply(x: A) = x.toB
     """

class ValueDiscarding(tp: Type)(using Context)
  extends Message(ValueDiscardingID):
    def kind = MessageKind.PotentialIssue
    def msg(using Context) = i"discarded non-Unit value of type $tp"
    def explain(using Context) = ""

class UnusedNonUnitValue(tp: Type)(using Context)
  extends Message(UnusedNonUnitValueID):
    def kind = MessageKind.PotentialIssue
    def msg(using Context) = i"unused value of type $tp"
    def explain(using Context) = ""

class MatchTypeNoCases(casesText: String)(using Context) extends TypeMsg(MatchTypeNoCasesID):
  def msg(using Context) = i"Match type reduction $casesText"
  def explain(using Context) = ""

class MatchTypeScrutineeCannotBeHigherKinded(tp: Type)(using Context)
  extends TypeMsg(MatchTypeScrutineeCannotBeHigherKindedID) :
    def msg(using Context) = i"the scrutinee of a match type cannot be higher-kinded"
    def explain(using Context) = ""

class MatchTypeLegacyPattern(errorText: String)(using Context) extends TypeMsg(MatchTypeLegacyPatternID):
  def msg(using Context) = errorText
  def explain(using Context) = ""

class ClosureCannotHaveInternalParameterDependencies(mt: Type)(using Context)
  extends TypeMsg(ClosureCannotHaveInternalParameterDependenciesID):
    def msg(using Context) =
      i"""cannot turn method type $mt into closure
         |because it has internal parameter dependencies"""
    def explain(using Context) = ""

class ImplausiblePatternWarning(pat: tpd.Tree, selType: Type)(using Context)
  extends TypeMsg(ImplausiblePatternWarningID):
    def msg(using Context) =
      i"""|Implausible pattern:
          |$pat  could match selector of type  $selType
          |only if there is an `equals` method identifying elements of the two types."""
    def explain(using Context) = ""

class UnstableInlineAccessor(accessed: Symbol, accessorTree: tpd.Tree)(using Context)
  extends Message(UnstableInlineAccessorID) {
  def kind = MessageKind.Compatibility

  def msg(using Context) =
    i"""Unstable inline accessor ${accessor.name} was generated in $where."""

  def explain(using Context) =
    i"""Access to non-public $accessed causes the automatic generation of an accessor.
       |This accessor is not stable, its name may change or it may disappear
       |if not needed in a future version.
       |
       |To make sure that the inlined code is binary compatible you must make sure that
       |$accessed is public in the binary API.
       | * Option 1: Annotate $accessed with @publicInBinary
       | * Option 2: Make $accessed public
       |
       |This change may break binary compatibility if a previous version of this
       |library was compiled with generated accessors. Binary compatibility should
       |be checked using MiMa. If binary compatibility is broken, you should add the
       |old accessor explicitly in the source code. The following code should be
       |added to $where:
       |  @publicInBinary private[$within] ${accessorTree.show}
       |"""

  private def accessor = accessorTree.symbol

  private def where =
    if accessor.owner.name.isPackageObjectName then s"package ${within}"
    else if accessor.owner.is(Module) then s"object $within"
    else s"class $within"

  private def within =
    if accessor.owner.name.isPackageObjectName then accessor.owner.owner.name.stripModuleClassSuffix
    else accessor.owner.name.stripModuleClassSuffix
}

class VolatileOnVal()(using Context)
extends SyntaxMsg(VolatileOnValID):
  protected def msg(using Context): String = "values cannot be volatile"
  protected def explain(using Context): String = ""
