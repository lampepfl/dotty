package dotty.tools
package dotc
package reporting

import core._
import Contexts._
import Decorators._, Symbols._, Names._, NameOps._, Types._, Flags._, Phases._
import Denotations.SingleDenotation
import SymDenotations.SymDenotation
import NameKinds.WildcardParamName
import parsing.Scanners.Token
import parsing.Tokens
import printing.Highlighting._
import printing.Formatting
import ErrorMessageID._
import ast.Trees
import config.{Feature, ScalaVersion}
import typer.ErrorReporting.{err, matchReductionAddendum}
import typer.ProtoTypes.ViewProto
import typer.Implicits.Candidate
import scala.util.control.NonFatal
import StdNames.nme
import printing.Formatting.hl
import ast.Trees._
import ast.untpd
import ast.tpd
import transform.SymUtils._

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

  abstract class SyntaxMsg(errorId: ErrorMessageID) extends Message(errorId):
    def kind = MessageKind.Syntax

  abstract class TypeMsg(errorId: ErrorMessageID) extends Message(errorId):
    def kind = MessageKind.Type

  trait ShowMatchTrace(tps: Type*)(using Context) extends Message:
    override def msgSuffix: String = matchReductionAddendum(tps*)

  abstract class TypeMismatchMsg(found: Type, expected: Type)(errorId: ErrorMessageID)(using Context)
  extends Message(errorId), ShowMatchTrace(found, expected):
    def kind = MessageKind.TypeMismatch
    def explain = err.whyNoMatchStr(found, expected)
    override def canExplain = true

  abstract class NamingMsg(errorId: ErrorMessageID) extends Message(errorId):
    def kind = MessageKind.Naming

  abstract class DeclarationMsg(errorId: ErrorMessageID) extends Message(errorId):
    def kind = MessageKind.Declaration

  /** A simple not found message (either for idents, or member selection.
   *  Messages of this class are sometimes dropped in favor of other, more
   *  specific messages.
   */
  abstract class NotFoundMsg(errorId: ErrorMessageID) extends Message(errorId):
    def kind = MessageKind.NotFound
    def name: Name

  abstract class PatternMatchMsg(errorId: ErrorMessageID) extends Message(errorId):
    def kind = MessageKind.PatternMatch

  abstract class CyclicMsg(errorId: ErrorMessageID) extends Message(errorId):
    def kind = MessageKind.Cyclic

  abstract class ReferenceMsg(errorId: ErrorMessageID) extends Message(errorId):
    def kind = MessageKind.Reference

  abstract class EmptyCatchOrFinallyBlock(tryBody: untpd.Tree, errNo: ErrorMessageID)(using Context)
  extends SyntaxMsg(errNo) {
    def explain = {
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

      em"""|A ${hl("try")} expression should be followed by some mechanism to handle any exceptions
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
    def msg =
      em"""|The ${hl("catch")} block does not contain a valid expression, try
           |adding a case like - ${hl("case e: Exception =>")} to the block"""
  }

  class EmptyCatchAndFinallyBlock(tryBody: untpd.Tree)(using Context)
  extends EmptyCatchOrFinallyBlock(tryBody, EmptyCatchAndFinallyBlockID) {
    def msg =
      em"""|A ${hl("try")} without ${hl("catch")} or ${hl("finally")} is equivalent to putting
           |its body in a block; no exceptions are handled."""
  }

  class DeprecatedWithOperator()(using Context)
  extends SyntaxMsg(DeprecatedWithOperatorID) {
    def msg =
      em"""${hl("with")} as a type operator has been deprecated; use ${hl("&")} instead"""
    def explain =
      em"""|Dotty introduces intersection types - ${hl("&")} types. These replace the
           |use of the ${hl("with")} keyword. There are a few differences in
           |semantics between intersection types and using ${hl("with")}."""
  }

  class CaseClassMissingParamList(cdef: untpd.TypeDef)(using Context)
  extends SyntaxMsg(CaseClassMissingParamListID) {
    def msg =
      em"""|A ${hl("case class")} must have at least one parameter list"""

    def explain =
      em"""|${cdef.name} must have at least one parameter list, if you would rather
           |have a singleton representation of ${cdef.name}, use a "${hl("case object")}".
           |Or, add an explicit ${hl("()")} as a parameter list to ${cdef.name}."""
  }

  class AnonymousFunctionMissingParamType(param: untpd.ValDef,
                                          tree: untpd.Function,
                                          pt: Type)
                                          (using Context)
  extends TypeMsg(AnonymousFunctionMissingParamTypeID) {
    def msg = {
      val ofFun =
        if param.name.is(WildcardParamName)
           || (MethodType.syntheticParamNames(tree.args.length + 1) contains param.name)
        then i" of expanded function:\n$tree"
        else ""

      val inferred =
        if (pt == WildcardType) ""
        else i"\nWhat I could infer was: $pt"

      i"""Missing parameter type
         |
         |I could not infer the type of the parameter ${param.name}$ofFun.$inferred"""
    }

    def explain = ""
  }

  class WildcardOnTypeArgumentNotAllowedOnNew()(using Context)
  extends SyntaxMsg(WildcardOnTypeArgumentNotAllowedOnNewID) {
    def msg = "Type argument must be fully defined"
    def explain =
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
      em"""|Wildcard on arguments is not allowed when declaring a new type.
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
    def msg = em"duplicate pattern variable: ${bind.name}"

    def explain = {
      val pat = tree.pat.show
      val guard = tree.guard match {
        case untpd.EmptyTree => ""
        case guard => s"if ${guard.show}"
      }

      val body = tree.body match {
        case Block(Nil, untpd.EmptyTree) => ""
        case body => s" ${body.show}"
      }

      val caseDef = s"case $pat$guard => $body"

      em"""|For each ${hl("case")} bound variable names have to be unique. In:
           |
           |$caseDef
           |
           |${bind.name} is not unique. Rename one of the bound variables!"""
    }
  }

  class MissingIdent(tree: untpd.Ident, treeKind: String, val name: Name)(using Context)
  extends NotFoundMsg(MissingIdentID) {
    def msg = em"Not found: $treeKind$name"
    def explain = {
      em"""|The identifier for `$treeKind$name` is not bound.
           |That is, no declaration for this identifier can be found.
           |That can happen if an import for the identifier is missing,
           |if the definition of `$name` has an error, or if there is a misspelling
           |of `$name` either where it is defined or where it is used."""
    }
  }

  class TypeMismatch(found: Type,  expected: Type, inTree: Option[untpd.Tree],  addenda: => String*)(using Context)
    extends TypeMismatchMsg(found, expected)(TypeMismatchID):

    // replace constrained TypeParamRefs and their typevars by their bounds where possible
    // and the bounds are not f-bounds.
    // The idea is that if the bounds are also not-subtypes of each other to report
    // the type mismatch on the bounds instead of the original TypeParamRefs, since
    // these are usually easier to analyze. We exclude F-bounds since these would
    // lead to a recursive infinite expansion.
    object reported extends TypeMap:
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

    def msg =
      val found1 = reported(found)
      reported.setVariance(-1)
      val expected1 = reported(expected)
      val (found2, expected2) =
        if (found1 frozen_<:< expected1) || reported.fbounded then (found, expected)
        else (found1, expected1)
      val postScript = addenda.find(!_.isEmpty) match
        case Some(p) => p
        case None =>
          if expected.isTopType || found.isBottomType
          then ""
          else ctx.typer.importSuggestionAddendum(ViewProto(found.widen, expected))
      val (where, printCtx) = Formatting.disambiguateTypes(found2, expected2)
      val whereSuffix = if (where.isEmpty) where else s"\n\n$where"
      val (foundStr, expectedStr) = Formatting.typeDiff(found2, expected2)(using printCtx)
      s"""|Found:    $foundStr
          |Required: $expectedStr""".stripMargin
        + whereSuffix + postScript

    override def explain =
      val treeStr = inTree.map(x => s"\nTree: ${x.show}").getOrElse("")
      treeStr + "\n" + super.explain


  end TypeMismatch

  class NotAMember(site: Type, val name: Name, selected: String, addendum: => String = "")(using Context)
  extends NotFoundMsg(NotAMemberID), ShowMatchTrace(site) {
    //println(i"site = $site, decls = ${site.decls}, source = ${site.typeSymbol.sourceFile}") //DEBUG

    def msg = {
      import core.Flags._
      val maxDist = 3  // maximal number of differences to be considered for a hint
      val missing = name.show

      // The symbols of all non-synthetic, non-private members of `site`
      // that are of the same type/term kind as the missing member.
      def candidates: Set[Symbol] =
        for
          bc <- site.widen.baseClasses.toSet
          sym <- bc.info.decls.filter(sym =>
            sym.isType == name.isTypeName
            && !sym.isConstructor
            && !sym.flagsUNSAFE.isOneOf(Synthetic | Private))
        yield sym

      // Calculate Levenshtein distance
      def distance(s1: String, s2: String): Int =
        val dist = Array.ofDim[Int](s2.length + 1, s1.length + 1)
        for
          j <- 0 to s2.length
          i <- 0 to s1.length
        do
          dist(j)(i) =
            if j == 0 then i
            else if i == 0 then j
            else if s2(j - 1) == s1(i - 1) then dist(j - 1)(i - 1)
            else (dist(j - 1)(i) min dist(j)(i - 1) min dist(j - 1)(i - 1)) + 1
        dist(s2.length)(s1.length)

      // A list of possible candidate symbols with their Levenstein distances
      // to the name of the missing member
      def closest: List[(Int, Symbol)] = candidates
        .toList
        .map(sym => (distance(sym.name.show, missing), sym))
        .filter((d, sym) => d <= maxDist && d < missing.length && d < sym.name.show.length)
        .sortBy((d, sym) => (d, sym.name.show))  // sort by distance first, alphabetically second

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
        else closest match
          case (d, sym) :: _ =>
            val siteName = site match
              case site: NamedType => site.name.show
              case site => i"$site"
            val showName =
              // Add .type to the name if it is a module
              if sym.is(ModuleClass) then s"${sym.name.show}.type"
              else sym.name.show
            s" - did you mean $siteName.$showName?$enumClause"
          case Nil => prefixEnumClause("")

      ex"$selected $name is not a member of ${site.widen}$finalAddendum"
    }

    def explain = ""
  }

  class EarlyDefinitionsNotSupported()(using Context)
  extends SyntaxMsg(EarlyDefinitionsNotSupportedID) {
    def msg = "Early definitions are not supported; use trait parameters instead"

    def explain = {
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

      em"""|Earlier versions of Scala did not support trait parameters and "early
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
    def msg = em"""An ${hl("implicit class")} may not be top-level"""

    def explain = {
      val TypeDef(name, impl @ Template(constr0, parents, self, _)) = cdef: @unchecked
      val exampleArgs =
        if(constr0.termParamss.isEmpty) "..."
        else constr0.termParamss(0).map(_.withMods(untpd.Modifiers()).show).mkString(", ")
      def defHasBody[T] = impl.body.exists(!_.isEmpty)
      val exampleBody = if (defHasBody) "{\n ...\n }" else ""
      em"""|There may not be any method, member or object in scope with the same name as
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
    def msg = em"""A ${hl("case class")} may not be defined as ${hl("implicit")}"""

    def explain =
      em"""|Implicit classes may not be case classes. Instead use a plain class:
           |
           |implicit class ${cdef.name}...
           |
           |"""
  }

  class ImplicitClassPrimaryConstructorArity()(using Context)
  extends SyntaxMsg(ImplicitClassPrimaryConstructorArityID){
    def msg = "Implicit classes must accept exactly one primary constructor parameter"
    def explain = {
      val example = "implicit class RichDate(date: java.util.Date)"
      em"""Implicit classes may only take one non-implicit argument in their constructor. For example:
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
    def msg = em"""${hl("object")}s must not have a self ${hl("type")}"""

    def explain = {
      val untpd.ModuleDef(name, tmpl) = mdef
      val ValDef(_, selfTpt, _) = tmpl.self
      em"""|${hl("object")}s must not have a self ${hl("type")}:
           |
           |Consider these alternative solutions:
           |  - Create a trait or a class instead of an object
           |  - Let the object extend a trait containing the self type:
           |
           |    object $name extends ${selfTpt.show}"""
    }
  }

  class RepeatedModifier(modifier: String)(implicit ctx:Context)
  extends SyntaxMsg(RepeatedModifierID) {
    def msg = em"""Repeated modifier $modifier"""

    def explain = {
      val code1 = em"""private private val Origin = Point(0, 0)"""
      val code2 = em"""private final val Origin = Point(0, 0)"""
      em"""This happens when you accidentally specify the same modifier twice.
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
  }

  class InterpolatedStringError()(implicit ctx:Context)
  extends SyntaxMsg(InterpolatedStringErrorID) {
    def msg = "Error in interpolated string: identifier or block expected"
    def explain = {
      val code1 = "s\"$new Point(0, 0)\""
      val code2 = "s\"${new Point(0, 0)}\""
      em"""|This usually happens when you forget to place your expressions inside curly braces.
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
    def msg = em"""Unbound placeholder parameter; incorrect use of ${hl("_")}"""
    def explain =
      em"""|The ${hl("_")} placeholder syntax was used where it could not be bound.
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
    def msg = em"expression expected but ${Red(illegalToken)} found"
    def explain = {
      em"""|An expression cannot start with ${Red(illegalToken)}."""
    }
  }

  class MissingReturnType()(implicit ctx:Context)
  extends SyntaxMsg(MissingReturnTypeID) {
    def msg = "Missing return type"
    def explain =
      em"""|An abstract declaration must have a return type. For example:
           |
           |trait Shape:
           |  ${hl("def area: Double")} // abstract declaration returning a Double"""
  }

  class MissingReturnTypeWithReturnStatement(method: Symbol)(using Context)
  extends SyntaxMsg(MissingReturnTypeWithReturnStatementID) {
    def msg = em"$method has a return statement; it needs a result type"
    def explain =
      em"""|If a method contains a ${hl("return")} statement, it must have an
           |explicit return type. For example:
           |
           |${hl("def good: Int /* explicit return type */ = return 1")}"""
  }

  class YieldOrDoExpectedInForComprehension()(using Context)
  extends SyntaxMsg(YieldOrDoExpectedInForComprehensionID) {
    def msg = em"${hl("yield")} or ${hl("do")} expected"

    def explain =
      em"""|When the enumerators in a for comprehension are not placed in parentheses or
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
    def msg = em"""Proper definition was not found in ${hl("@usecase")}"""

    def explain = {
      val noUsecase =
        "def map[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That"

      val usecase =
        """|/** Map from List[A] => List[B]
           |  *
           |  * @usecase def map[B](f: A => B): List[B]
           |  */
           |def map[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That
           |""".stripMargin

      em"""|Usecases are only supported for ${hl("def")}s. They exist because with Scala's
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

  class ByNameParameterNotSupported(tpe: untpd.TypTree)(using Context)
  extends SyntaxMsg(ByNameParameterNotSupportedID) {
    def msg = em"By-name parameter type ${tpe} not allowed here."

    def explain =
      em"""|By-name parameters act like functions that are only evaluated when referenced,
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

    def msg =
      val expectedArgString = expectedArgs
        .map(_.paramName.unexpandedName.show)
        .mkString("[", ", ", "]")
      val actualArgString = actual.map(_.show).mkString("[", ", ", "]")
      val prettyName =
        try fntpe.termSymbol match
          case NoSymbol => fntpe.show
          case symbol   => symbol.showFullName
        catch case NonFatal(ex) => fntpe.show
      em"""|$msgPrefix type arguments for $prettyName$expectedArgString
           |expected: $expectedArgString
           |actual:   $actualArgString""".stripMargin

    def explain = {
      val tooManyTypeParams =
        """|val tuple2: (Int, String) = (1, "one")
           |val list: List[(Int, String)] = List(tuple2)""".stripMargin

      if (actualCount > expectedCount)
        em"""|You have supplied too many type parameters
             |
             |For example List takes a single type parameter (List[A])
             |If you need to hold more types in a list then you need to combine them
             |into another data type that can contain the number of types you need,
             |In this example one solution would be to use a Tuple:
             |
             |${tooManyTypeParams}"""
      else
        em"""|You have not supplied enough type parameters
             |If you specify one type parameter then you need to specify every type parameter."""
    }
  }

  class IllegalVariableInPatternAlternative(name: Name)(using Context)
  extends SyntaxMsg(IllegalVariableInPatternAlternativeID) {
    def msg = em"Illegal variable $name in pattern alternative"
    def explain = {
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

      em"""|Variables are not allowed within alternate pattern matches. You can workaround
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
    def msg = "identifier expected"
    def explain = {
      val wrongIdentifier = em"def foo: $identifier = {...}"
      val validIdentifier = em"def foo = {...}"
      em"""|An identifier expected, but $identifier found. This could be because
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
    def msg = "Auxiliary constructor needs non-implicit parameter list"
    def explain =
      em"""|Only the primary constructor is allowed an ${hl("implicit")} parameter list;
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
    def msg = "Illegal literal"
    def explain =
      em"""|Available literals can be divided into several groups:
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
    def msg = em"""|Widening conversion from $sourceType to $targetType loses precision.
                   |Write `.to$targetType` instead.""".stripMargin
    def explain = ""

  class PatternMatchExhaustivity(uncoveredFn: => String, hasMore: Boolean)(using Context)
  extends Message(PatternMatchExhaustivityID) {
    def kind = MessageKind.PatternMatchExhaustivity
    lazy val uncovered = uncoveredFn
    def msg =
      val addendum = if hasMore then "(More unmatched cases are elided)" else ""
      em"""|${hl("match")} may not be exhaustive.
           |
           |It would fail on pattern case: $uncovered
           |$addendum"""


    def explain =
      em"""|There are several ways to make the match exhaustive:
           | - Add missing cases as shown in the warning
           | - If an extractor always return ${hl("Some(...)")}, write ${hl("Some[X]")} for its return type
           | - Add a ${hl("case _ => ...")} at the end to match all remaining cases
           |"""
  }

  class UncheckedTypePattern(msgFn: => String)(using Context)
    extends PatternMatchMsg(UncheckedTypePatternID) {
    def msg = msgFn
    def explain =
      em"""|Type arguments and type refinements are erased during compile time, thus it's
           |impossible to check them at run-time.
           |
           |You can either replace the type arguments by ${hl("_")} or use `@unchecked`.
           |"""
  }

  class MatchCaseUnreachable()(using Context)
  extends Message(MatchCaseUnreachableID) {
    def kind = MessageKind.MatchCaseUnreachable
    def msg = "Unreachable case"
    def explain = ""
  }

  class MatchCaseOnlyNullWarning()(using Context)
  extends PatternMatchMsg(MatchCaseOnlyNullWarningID) {
    def msg = em"""Unreachable case except for ${hl("null")} (if this is intentional, consider writing ${hl("case null =>")} instead)."""
    def explain = ""
  }

  class MatchableWarning(tp: Type, pattern: Boolean)(using Context)
  extends TypeMsg(MatchableWarningID) {
    def msg =
      val kind = if pattern then "pattern selector" else "value"
      em"""${kind} should be an instance of Matchable,,
          |but it has unmatchable type $tp instead"""

    def explain =
      if pattern then
        em"""A value of type $tp cannot be the selector of a match expression
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
        em"""The value can be converted to a `Matchable` by appending `.asMatchable`.
            |`asMatchable` needs to be imported from scala.compiletime."""
  }

  class SeqWildcardPatternPos()(using Context)
  extends SyntaxMsg(SeqWildcardPatternPosID) {
    def msg = em"""${hl("*")} can be used only for last argument"""
    def explain = {
      val code =
        """def sumOfTheFirstTwo(list: List[Int]): Int = list match {
          |  case List(first, second, x*) => first + second
          |  case _ => 0
          |}"""
      em"""|Sequence wildcard pattern is expected at the end of an argument list.
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
    def msg = "pattern expected"
    def explain = {
      val sipCode =
        """def f(x: Int, y: Int) = x match {
          |  case `y` => ...
          |}
        """
      val constructorPatternsCode =
        """case class Person(name: String, age: Int)
          |
          |def test(p: Person) = p match {
          |  case Person(name, age) => ...
          |}
        """
      val tupplePatternsCode =
        """def swap(tuple: (String, Int)): (Int, String) = tuple match {
          |  case (text, number) => (number, text)
          |}
        """
      val patternSequencesCode =
        """def getSecondValue(list: List[Int]): Int = list match {
          |  case List(_, second, x:_*) => second
          |  case _ => 0
          |}"""
      em"""|Simple patterns can be divided into several groups:
           |- Variable Patterns: ${hl("case x => ...")}.
           |  It matches any value, and binds the variable name to that value.
           |  A special case is the wild-card pattern _ which is treated as if it was a fresh
           |  variable on each occurrence.
           |
           |- Typed Patterns: ${hl("case x: Int => ...")} or ${hl("case _: Int => ...")}.
           |  This pattern matches any value matched by the specified type; it binds the variable
           |  name to that value.
           |
           |- Literal Patterns: ${hl("case 123 => ...")} or ${hl("case 'A' => ...")}.
           |  This type of pattern matches any value that is equal to the specified literal.
           |
           |- Stable Identifier Patterns:
           |
           |  $sipCode
           |
           |  the match succeeds only if the x argument and the y argument of f are equal.
           |
           |- Constructor Patterns:
           |
           |  $constructorPatternsCode
           |
           |  The pattern binds all object's fields to the variable names (name and age, in this
           |  case).
           |
           |- Tuple Patterns:
           |
           |  $tupplePatternsCode
           |
           |  Calling:
           |
           |  ${hl("""swap(("Luftballons", 99)""")}
           |
           |  would give ${hl("""(99, "Luftballons")""")} as a result.
           |
           |- Pattern Sequences:
           |
           |  $patternSequencesCode
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
    def msg = em"Trying to define package with same name as $existing"
    def explain = ""
  }

  class ExistentialTypesNoLongerSupported()(using Context)
  extends SyntaxMsg(ExistentialTypesNoLongerSupportedID) {
    def msg =
      em"""|Existential types are no longer supported -
           |use a wildcard or dependent type instead"""
    def explain =
      em"""|The use of existential types is no longer supported.
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
    def msg = "Unbound wildcard type"
    def explain =
      em"""|The wildcard type syntax (${hl("_")}) was used where it could not be bound.
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
    def msg = em"""${member} overrides nothing"""

    def explain =
      em"""|There must be a field or method with the name ${member.name} in a super
           |class of ${member.owner} to override it. Did you misspell it?
           |Are you extending the right classes?
           |"""
  }

  class OverridesNothingButNameExists(member: Symbol, existing: List[Denotations.SingleDenotation])(using Context)
  extends DeclarationMsg(OverridesNothingButNameExistsID) {
    def msg =
      val what =
        if !existing.exists(_.symbol.hasTargetName(member.targetName))
        then "target name"
        else "signature"
      em"""${member} has a different $what than the overridden declaration"""
    def explain =
      val existingDecl: String = existing.map(_.showDcl).mkString("  \n")
      em"""|There must be a non-final field or method with the name ${member.name} and the
           |same parameter list in a super class of ${member.owner} to override it.
           |
           |  ${member.showDcl}
           |
           |The super classes of ${member.owner} contain the following members
           |named ${member.name}:
           |  ${existingDecl}
           |"""
  }

  class OverrideError(override val msg: String) extends DeclarationMsg(OverrideErrorID):
    def explain = ""

  class OverrideTypeMismatchError(override val msg: String, memberTp: Type, otherTp: Type)(using Context)
  extends DeclarationMsg(OverrideTypeMismatchErrorID):
    def explain = err.whyNoMatchStr(memberTp, otherTp)
    override def canExplain = true

  class ForwardReferenceExtendsOverDefinition(value: Symbol, definition: Symbol)(using Context)
  extends ReferenceMsg(ForwardReferenceExtendsOverDefinitionID) {
    def msg = em"${definition.name} is a forward reference extending over the definition of ${value.name}"

    def explain =
      em"""|${definition.name} is used before you define it, and the definition of ${value.name}
           |appears between that use and the definition of ${definition.name}.
           |
           |Forward references are allowed only, if there are no value definitions between
           |the reference and the referred method definition.
           |
           |Define ${definition.name} before it is used,
           |or move the definition of ${value.name} so it does not appear between
           |the declaration of ${definition.name} and its use,
           |or define ${value.name} as lazy.
           |""".stripMargin
  }

  class ExpectedTokenButFound(expected: Token, found: Token)(using Context)
  extends SyntaxMsg(ExpectedTokenButFoundID) {

    private lazy val foundText = Tokens.showToken(found)

    def msg =
      val expectedText =
        if (Tokens.isIdentifier(expected)) "an identifier"
        else Tokens.showToken(expected)
      em"""${expectedText} expected, but ${foundText} found"""

    def explain =
      if (Tokens.isIdentifier(expected) && Tokens.isKeyword(found))
        s"""
         |If you want to use $foundText as identifier, you may put it in backticks: `${Tokens.tokenString(found)}`.""".stripMargin
      else
        ""
  }

  class ExpectedTokenButFoundSoftToken(expected: Token, found: Token, soft: Name)(using Context) extends SyntaxMsg(ExpectedTokenButFoundID):
    def msg =
      val expectedText =
        if (Tokens.isIdentifier(expected)) "an identifier"
        else Tokens.showToken(expected)
      val what = if Tokens.isIdentifier(found) || expected == Tokens.COLONop then "an identifier" else "the soft token"
      em"""${expectedText} expected, but ${Tokens.showToken(found)} found.
          |The soft token `${soft}` was taken as $what in this context.""".stripMargin

    def explain = s"The soft token `${soft}` has special meaning only in certain contexts."
  end ExpectedTokenButFoundSoftToken

  class MixedLeftAndRightAssociativeOps(op1: Name, op2: Name, op2LeftAssoc: Boolean)(using Context)
  extends SyntaxMsg(MixedLeftAndRightAssociativeOpsID) {
    def msg =
      val op1Asso: String = if (op2LeftAssoc) "which is right-associative" else "which is left-associative"
      val op2Asso: String = if (op2LeftAssoc) "which is left-associative" else "which is right-associative"
      em"${op1} (${op1Asso}) and ${op2} ($op2Asso) have same precedence and may not be mixed"
    def explain =
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
    def msg = em"""${cls.name} is ${traitOrAbstract}; it cannot be instantiated"""
    def explain =
      em"""|Abstract classes and traits need to be extended by a concrete class or object
           |to make their functionality accessible.
           |
           |You may want to create an anonymous class extending ${cls.name} with
           |  ${s"class ${cls.name} { }"}
           |
           |or add a companion object with
           |  ${s"object ${cls.name} extends ${cls.name}"}
           |
           |You need to implement any abstract members in both cases.
           |""".stripMargin
  }

  class UnreducibleApplication(tycon: Type)(using Context) extends TypeMsg(UnreducibleApplicationID):
    def msg = em"unreducible application of higher-kinded type $tycon to wildcard arguments"
    def explain =
      em"""|An abstract type constructor cannot be applied to wildcard arguments.
           |Such applications are equivalent to existential types, which are not
           |supported in Scala 3."""

  class OverloadedOrRecursiveMethodNeedsResultType(cycleSym: Symbol)(using Context)
  extends CyclicMsg(OverloadedOrRecursiveMethodNeedsResultTypeID) {
    def msg = em"""Overloaded or recursive $cycleSym needs return type"""
    def explain =
      em"""Case 1: $cycleSym is overloaded
          |If there are multiple methods named $cycleSym and at least one definition of
          |it calls another, you need to specify the calling method's return type.
          |
          |Case 2: $cycleSym is recursive
          |If $cycleSym calls itself on any path (even through mutual recursion), you need to specify the return type
          |of $cycleSym or of a definition it's mutually recursive with.
          |""".stripMargin
  }

  class RecursiveValueNeedsResultType(cycleSym: Symbol)(using Context)
  extends CyclicMsg(RecursiveValueNeedsResultTypeID) {
    def msg = em"""Recursive $cycleSym needs type"""
    def explain =
      em"""The definition of $cycleSym is recursive and you need to specify its type.
          |""".stripMargin
  }

  class CyclicReferenceInvolving(denot: SymDenotation)(using Context)
  extends CyclicMsg(CyclicReferenceInvolvingID) {
    def msg =
      val where = if denot.exists then s" involving $denot" else ""
      em"Cyclic reference$where"
    def explain =
      em"""|$denot is declared as part of a cycle which makes it impossible for the
           |compiler to decide upon ${denot.name}'s type.
           |To avoid this error, try giving ${denot.name} an explicit type.
           |""".stripMargin
  }

  class CyclicReferenceInvolvingImplicit(cycleSym: Symbol)(using Context)
  extends CyclicMsg(CyclicReferenceInvolvingImplicitID) {
    def msg = em"""Cyclic reference involving implicit $cycleSym"""
    def explain =
      em"""|$cycleSym is declared as part of a cycle which makes it impossible for the
           |compiler to decide upon ${cycleSym.name}'s type.
           |This might happen when the right hand-side of $cycleSym's definition involves an implicit search.
           |To avoid this error, try giving ${cycleSym.name} an explicit type.
           |""".stripMargin
  }

  class SkolemInInferred(tree: tpd.Tree, pt: Type, argument: tpd.Tree)(using Context)
  extends TypeMsg(SkolemInInferredID):
    private def argStr =
      if argument.isEmpty then ""
      else i" from argument of type ${argument.tpe.widen}"
    def msg =
      em"""Failure to generate given instance for type $pt$argStr)
          |
          |I found: $tree
          |But the part corresponding to `<skolem>` is not a reference that can be generated.
          |This might be because resolution yielded as given instance a function that is not
          |known to be total and side-effect free."""
    def explain =
      em"""The part of given resolution that corresponds to `<skolem>` produced a term that
          |is not a stable reference. Therefore a given instance could not be generated.
          |
          |To trouble-shoot the problem, try to supply an explicit expression instead of
          |relying on implicit search at this point."""

  class SuperQualMustBeParent(qual: untpd.Ident, cls: ClassSymbol)(using Context)
  extends ReferenceMsg(SuperQualMustBeParentID) {
    def msg = em"""|$qual does not name a parent of $cls"""
    def explain =
      val parents: Seq[String] = (cls.info.parents map (_.typeSymbol.name.show)).sorted
      em"""|When a qualifier ${hl("T")} is used in a ${hl("super")} prefix of the form ${hl("C.super[T]")},
           |${hl("T")} must be a parent type of ${hl("C")}.
           |
           |In this case, the parents of $cls are:
           |${parents.mkString("  - ", "\n  - ", "")}
           |""".stripMargin
  }

  class VarArgsParamMustComeLast()(using Context)
  extends SyntaxMsg(VarArgsParamMustComeLastID) {
    def msg = em"""${hl("varargs")} parameter must come last"""
    def explain =
      em"""|The ${hl("varargs")} field must be the last field in the method signature.
           |Attempting to define a field in a method signature after a ${hl("varargs")} field is an error.
           |"""
  }

  import typer.Typer.BindingPrec

  class AmbiguousReference(name: Name, newPrec: BindingPrec, prevPrec: BindingPrec, prevCtx: Context)(using Context)
    extends ReferenceMsg(AmbiguousReferenceID) {

    /** A string which explains how something was bound; Depending on `prec` this is either
      *      imported by <tree>
      *  or  defined in <symbol>
      */
    private def bindingString(prec: BindingPrec, whereFound: Context, qualifier: String = "") = {
      val howVisible = prec match {
        case BindingPrec.Definition => "defined"
        case BindingPrec.Inheritance => "inherited"
        case BindingPrec.NamedImport => "imported by name"
        case BindingPrec.WildImport => "imported"
        case BindingPrec.PackageClause => "found"
        case BindingPrec.NothingBound => assert(false)
      }
      if (prec.isImportPrec) {
        ex"""$howVisible$qualifier by ${em"${whereFound.importInfo}"}"""
      } else
        ex"""$howVisible$qualifier in ${em"${whereFound.owner}"}"""
    }

    def msg =
      i"""|Reference to ${em"$name"} is ambiguous,
          |it is both ${bindingString(newPrec, ctx)}
          |and ${bindingString(prevPrec, prevCtx, " subsequently")}"""

    def explain =
      em"""|The compiler can't decide which of the possible choices you
           |are referencing with $name: A definition of lower precedence
           |in an inner scope, or a definition with higher precedence in
           |an outer scope.
           |Note:
           | - Definitions in an enclosing scope take precedence over inherited definitions
           | - Definitions take precedence over imports
           | - Named imports take precedence over wildcard imports
           | - You may replace a name when imported using
           |   ${hl("import")} scala.{ $name => ${name.show + "Tick"} }
           |"""
  }

  class MethodDoesNotTakeParameters(tree: tpd.Tree)(using Context)
  extends TypeMsg(MethodDoesNotTakeParametersId) {
    def methodSymbol: Symbol =
      def recur(t: tpd.Tree): Symbol =
        val sym = tpd.methPart(t).symbol
        if sym == defn.Any_typeCast then
          t match
            case TypeApply(Select(qual, _), _) => recur(qual)
            case _ => sym
        else sym
      recur(tree)

    def msg = {
      val more = if (tree.isInstanceOf[tpd.Apply]) " more" else ""
      val meth = methodSymbol
      val methStr = if (meth.exists) meth.showLocated else "expression"
      em"$methStr does not take$more parameters"
    }

    def explain = {
      val isNullary = methodSymbol.info.isInstanceOf[ExprType]
      val addendum =
        if (isNullary) "\nNullary methods may not be called with parenthesis"
        else ""

      "You have specified more parameter lists than defined in the method definition(s)." + addendum
    }

  }

  class AmbiguousOverload(tree: tpd.Tree, val alternatives: List[SingleDenotation], pt: Type, addendum: String = "")(
    implicit ctx: Context)
  extends ReferenceMsg(AmbiguousOverloadID) {
    private def all = if (alternatives.length == 2) "both" else "all"
    def msg =
      em"""|Ambiguous overload. The ${err.overloadedAltsStr(alternatives)}
           |$all match ${err.expectedTypeStr(pt)}$addendum""".stripMargin
    def explain =
      em"""|There are ${alternatives.length} methods that could be referenced as the compiler knows too little
           |about the expected type.
           |You may specify the expected type e.g. by
           |- assigning it to a value with a specified type, or
           |- adding a type ascription as in ${hl("instance.myMethod: String => Int")}
           |"""
  }

  class ReassignmentToVal(name: Name)(using Context)
    extends TypeMsg(ReassignmentToValID) {
    def msg = em"""Reassignment to val $name"""
    def explain =
      em"""|You can not assign a new value to $name as values can't be changed.
           |Keep in mind that every statement has a value, so you may e.g. use
           |  ${hl("val")} $name ${hl("= if (condition) 2 else 5")}
           |In case you need a reassignable name, you can declare it as
           |variable
           |  ${hl("var")} $name ${hl("=")} ...
           |""".stripMargin
  }

  class TypeDoesNotTakeParameters(tpe: Type, params: List[Trees.Tree[Trees.Untyped]])(using Context)
    extends TypeMsg(TypeDoesNotTakeParametersID) {
    private def fboundsAddendum =
      if tpe.typeSymbol.isAllOf(Provisional | TypeParam) then
        "\n(Note that F-bounds of type parameters may not be type lambdas)"
      else ""
    def msg = em"$tpe does not take type parameters$fboundsAddendum"
    def explain =
      val ps =
        if (params.size == 1) s"a type parameter ${params.head}"
        else s"type parameters ${params.map(_.show).mkString(", ")}"
      i"""You specified ${NoColor(ps)} for ${em"$tpe"}, which is not
         |declared to take any.
         |"""
  }

  class ParameterizedTypeLacksArguments(psym: Symbol)(using Context)
    extends TypeMsg(ParameterizedTypeLacksArgumentsID) {
    def msg = em"Parameterized $psym lacks argument list"
    def explain =
      em"""The $psym is declared with non-implicit parameters, you may not leave
          |out the parameter list when extending it.
          |"""
  }

  class VarValParametersMayNotBeCallByName(name: TermName, mutable: Boolean)(using Context)
    extends SyntaxMsg(VarValParametersMayNotBeCallByNameID) {
    def varOrVal = if (mutable) em"${hl("var")}" else em"${hl("val")}"
    def msg = s"$varOrVal parameters may not be call-by-name"
    def explain =
      em"""${hl("var")} and ${hl("val")} parameters of classes and traits may no be call-by-name. In case you
          |want the parameter to be evaluated on demand, consider making it just a parameter
          |and a ${hl("def")} in the class such as
          |  ${s"class MyClass(${name}Tick: => String) {"}
          |  ${s"  def $name() = ${name}Tick"}
          |  ${hl("}")}
          |"""
  }

  class MissingTypeParameterFor(tpe: Type)(using Context)
    extends SyntaxMsg(MissingTypeParameterForID) {
    def msg =
      if (tpe.derivesFrom(defn.AnyKindClass)) em"${tpe} cannot be used as a value type"
      else em"Missing type parameter for ${tpe}"
    def explain = ""
  }

  class MissingTypeParameterInTypeApp(tpe: Type)(using Context)
    extends TypeMsg(MissingTypeParameterInTypeAppID) {
    def numParams = tpe.typeParams.length
    def parameters = if (numParams == 1) "parameter" else "parameters"
    def msg = em"Missing type $parameters for $tpe"
    def explain = em"A fully applied type is expected but $tpe takes $numParams $parameters"
  }

  class DoesNotConformToBound(tpe: Type, which: String, bound: Type)(using Context)
    extends TypeMismatchMsg(tpe, bound)(DoesNotConformToBoundID) {
    def msg = em"Type argument ${tpe} does not conform to $which bound $bound"
  }

  class DoesNotConformToSelfType(category: String, selfType: Type, cls: Symbol,
                                 otherSelf: Type, relation: String, other: Symbol)(
    implicit ctx: Context)
    extends TypeMismatchMsg(selfType, otherSelf)(DoesNotConformToSelfTypeID) {
    def msg = em"""$category: self type $selfType of $cls does not conform to self type $otherSelf
                  |of $relation $other"""
  }

  class DoesNotConformToSelfTypeCantBeInstantiated(tp: Type, selfType: Type)(
    implicit ctx: Context)
    extends TypeMismatchMsg(tp, selfType)(DoesNotConformToSelfTypeCantBeInstantiatedID) {
    def msg = em"""$tp does not conform to its self type $selfType; cannot be instantiated"""
  }

  class IllegalParameterInit(found: Type, expected: Type, param: Symbol, cls: Symbol)(using Context)
    extends TypeMismatchMsg(found, expected)(IllegalParameterInitID):
    def msg =
      em"""illegal parameter initialization of $param.
          |
          |  The argument passed for $param has type: $found
          |  but $cls expects $param to have type: $expected"""

  class AbstractMemberMayNotHaveModifier(sym: Symbol, flag: FlagSet)(
    implicit ctx: Context)
    extends SyntaxMsg(AbstractMemberMayNotHaveModifierID) {
    def msg = em"""${hl("abstract")} $sym may not have `${flag.flagsString}` modifier"""
    def explain = ""
  }

  class TypesAndTraitsCantBeImplicit()(using Context)
    extends SyntaxMsg(TypesAndTraitsCantBeImplicitID) {
    def msg = em"""${hl("implicit")} modifier cannot be used for types or traits"""
    def explain = ""
  }

  class OnlyClassesCanBeAbstract(sym: Symbol)(
    implicit ctx: Context)
    extends SyntaxMsg(OnlyClassesCanBeAbstractID) {
    def explain = ""
    def msg = em"""${hl("abstract")} modifier can be used only for classes; it should be omitted for abstract members"""
  }

  class AbstractOverrideOnlyInTraits(sym: Symbol)(
    implicit ctx: Context)
    extends SyntaxMsg(AbstractOverrideOnlyInTraitsID) {
    def msg = em"""${hl("abstract override")} modifier only allowed for members of traits"""
    def explain = ""
  }

  class TraitsMayNotBeFinal(sym: Symbol)(
    implicit ctx: Context)
    extends SyntaxMsg(TraitsMayNotBeFinalID) {
    def msg = em"""$sym may not be ${hl("final")}"""
    def explain =
      "A trait can never be final since it is abstract and must be extended to be useful."
  }

  class NativeMembersMayNotHaveImplementation(sym: Symbol)(
    implicit ctx: Context)
    extends SyntaxMsg(NativeMembersMayNotHaveImplementationID) {
    def msg = em"""${hl("@native")} members may not have an implementation"""
    def explain = ""
  }

  class TraitMayNotDefineNativeMethod(sym: Symbol)(
    implicit ctx: Context)
    extends SyntaxMsg(TraitMayNotDefineNativeMethodID) {
    def msg = em"""A trait cannot define a ${hl("@native")} method."""
    def explain = ""
  }

  class OnlyClassesCanHaveDeclaredButUndefinedMembers(sym: Symbol)(
    implicit ctx: Context)
    extends SyntaxMsg(OnlyClassesCanHaveDeclaredButUndefinedMembersID) {

    private def varNote =
      if (sym.is(Mutable)) "Note that variables need to be initialized to be defined."
      else ""
    def msg = em"""Declaration of $sym not allowed here: only classes can have declared but undefined members"""
    def explain = s"$varNote"
  }

  class CannotExtendAnyVal(sym: Symbol)(using Context)
    extends SyntaxMsg(CannotExtendAnyValID) {
    def msg = em"""$sym cannot extend ${hl("AnyVal")}"""
    def explain =
      em"""Only classes (not traits) are allowed to extend ${hl("AnyVal")}, but traits may extend
          |${hl("Any")} to become ${Green("\"universal traits\"")} which may only have ${hl("def")} members.
          |Universal traits can be mixed into classes that extend ${hl("AnyVal")}.
          |"""
  }

  class CannotExtendJavaEnum(sym: Symbol)(using Context)
    extends SyntaxMsg(CannotExtendJavaEnumID) {
      def msg = em"""$sym cannot extend ${hl("java.lang.Enum")}: only enums defined with the ${hl("enum")} syntax can"""
      def explain = ""
    }

  class CannotExtendContextFunction(sym: Symbol)(using Context)
    extends SyntaxMsg(CannotExtendFunctionID) {
      def msg = em"""$sym cannot extend a context function class"""
      def explain = ""
    }

  class JavaEnumParentArgs(parent: Type)(using Context)
    extends TypeMsg(JavaEnumParentArgsID) {
      def msg = em"""not enough arguments for constructor Enum: ${hl("(name: String, ordinal: Int)")}: ${hl(parent.show)}"""
      def explain = ""
    }

  class CannotHaveSameNameAs(sym: Symbol, cls: Symbol, reason: CannotHaveSameNameAs.Reason)(using Context)
    extends SyntaxMsg(CannotHaveSameNameAsID) {
    import CannotHaveSameNameAs._
    def reasonMessage: String = reason match {
      case CannotBeOverridden => "class definitions cannot be overridden"
      case DefinedInSelf(self) =>
        s"""cannot define ${sym.showKind} member with the same name as a ${cls.showKind} member in self reference ${self.name}.
           |(Note: this can be resolved by using another name)
           |""".stripMargin
    }

    def msg = em"""$sym cannot have the same name as ${cls.showLocated} -- """ + reasonMessage
    def explain = ""
  }
  object CannotHaveSameNameAs {
    sealed trait Reason
    case object CannotBeOverridden extends Reason
    case class DefinedInSelf(self: tpd.ValDef) extends Reason
  }

  class ValueClassesMayNotDefineInner(valueClass: Symbol, inner: Symbol)(using Context)
    extends SyntaxMsg(ValueClassesMayNotDefineInnerID) {
    def msg = em"""Value classes may not define an inner class"""
    def explain = ""
  }

  class ValueClassesMayNotDefineNonParameterField(valueClass: Symbol, field: Symbol)(using Context)
    extends SyntaxMsg(ValueClassesMayNotDefineNonParameterFieldID) {
    def msg = em"""Value classes may not define non-parameter field"""
    def explain = ""
  }

  class ValueClassesMayNotDefineASecondaryConstructor(valueClass: Symbol, constructor: Symbol)(using Context)
    extends SyntaxMsg(ValueClassesMayNotDefineASecondaryConstructorID) {
    def msg = em"""Value classes may not define a secondary constructor"""
    def explain = ""
  }

  class ValueClassesMayNotContainInitalization(valueClass: Symbol)(using Context)
    extends SyntaxMsg(ValueClassesMayNotContainInitalizationID) {
    def msg = em"""Value classes may not contain initialization statements"""
    def explain = ""
  }

  class ValueClassesMayNotBeAbstract(valueClass: Symbol)(using Context)
    extends SyntaxMsg(ValueClassesMayNotBeAbstractID) {
    def msg = em"""Value classes may not be ${hl("abstract")}"""
    def explain = ""
  }

  class ValueClassesMayNotBeContainted(valueClass: Symbol)(using Context)
    extends SyntaxMsg(ValueClassesMayNotBeContaintedID) {
    private def localOrMember = if (valueClass.owner.isTerm) "local class" else "member of another class"
    def msg = s"""Value classes may not be a $localOrMember"""
    def explain = ""
  }

  class ValueClassesMayNotWrapAnotherValueClass(valueClass: Symbol)(using Context)
    extends SyntaxMsg(ValueClassesMayNotWrapAnotherValueClassID) {
    def msg = """A value class may not wrap another user-defined value class"""
    def explain = ""
  }

  class ValueClassParameterMayNotBeAVar(valueClass: Symbol, param: Symbol)(using Context)
    extends SyntaxMsg(ValueClassParameterMayNotBeAVarID) {
    def msg = em"""A value class parameter may not be a ${hl("var")}"""
    def explain =
      em"""A value class must have exactly one ${hl("val")} parameter."""
  }

  class ValueClassNeedsOneValParam(valueClass: Symbol)(using Context)
    extends SyntaxMsg(ValueClassNeedsExactlyOneValParamID) {
    def msg = em"""Value class needs one ${hl("val")} parameter"""
    def explain = ""
  }

  class ValueClassParameterMayNotBeCallByName(valueClass: Symbol, param: Symbol)(using Context)
    extends SyntaxMsg(ValueClassParameterMayNotBeCallByNameID) {
    def msg = s"Value class parameter `${param.name}` may not be call-by-name"
    def explain = ""
  }

  class SuperCallsNotAllowedInlineable(symbol: Symbol)(using Context)
    extends SyntaxMsg(SuperCallsNotAllowedInlineableID) {
    def msg = em"Super call not allowed in inlineable $symbol"
    def explain = "Method inlining prohibits calling superclass methods, as it may lead to confusion about which super is being called."
  }

  class NotAPath(tp: Type, usage: String)(using Context) extends TypeMsg(NotAPathID):
    def msg = em"$tp is not a valid $usage, since it is not an immutable path"
    def explain =
      i"""An immutable path is
         | - a reference to an immutable value, or
         | - a reference to `this`, or
         | - a selection of an immutable path with an immutable value."""

  class WrongNumberOfParameters(expected: Int)(using Context)
    extends SyntaxMsg(WrongNumberOfParametersID) {
    def msg = s"Wrong number of parameters, expected: $expected"
    def explain = ""
  }

  class DuplicatePrivateProtectedQualifier()(using Context)
    extends SyntaxMsg(DuplicatePrivateProtectedQualifierID) {
    def msg = "Duplicate private/protected qualifier"
    def explain =
      em"It is not allowed to combine `private` and `protected` modifiers even if they are qualified to different scopes"
  }

  class ExpectedStartOfTopLevelDefinition()(using Context)
    extends SyntaxMsg(ExpectedStartOfTopLevelDefinitionID) {
    def msg = "Expected start of definition"
    def explain =
      em"You have to provide either ${hl("class")}, ${hl("trait")}, ${hl("object")}, or ${hl("enum")} definitions after qualifiers"
  }

  class NoReturnFromInlineable(owner: Symbol)(using Context)
    extends SyntaxMsg(NoReturnFromInlineableID) {
    def msg = em"No explicit ${hl("return")} allowed from inlineable $owner"
    def explain =
      em"""Methods marked with ${hl("inline")} modifier may not use ${hl("return")} statements.
          |Instead, you should rely on the last expression's value being
          |returned from a method.
          |"""
  }

  class ReturnOutsideMethodDefinition(owner: Symbol)(using Context)
    extends SyntaxMsg(ReturnOutsideMethodDefinitionID) {
    def msg = em"${hl("return")} outside method definition"
    def explain =
      em"""You used ${hl("return")} in ${owner}.
          |${hl("return")} is a keyword and may only be used within method declarations.
          |"""
  }

  class ExtendFinalClass(clazz:Symbol, finalClazz: Symbol)(using Context)
    extends SyntaxMsg(ExtendFinalClassID) {
    def msg = em"$clazz cannot extend ${hl("final")} $finalClazz"
    def explain =
      em"""A class marked with the ${hl("final")} keyword cannot be extended"""
  }

  class ExpectedTypeBoundOrEquals(found: Token)(using Context)
    extends SyntaxMsg(ExpectedTypeBoundOrEqualsID) {
    def msg = em"${hl("=")}, ${hl(">:")}, or ${hl("<:")} expected, but ${Tokens.showToken(found)} found"

    def explain =
      em"""Type parameters and abstract types may be constrained by a type bound.
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
    def msg =
      val name = cls.name.stripModuleClassSuffix
      em"Name clash: both ${cls.owner} and its companion object defines $name"
    def explain =
      em"""|A ${cls.kindString} and its companion object cannot both define a ${hl("class")}, ${hl("trait")} or ${hl("object")} with the same name:
           |  - ${cls.owner} defines ${cls}
           |  - ${other.owner} defines ${other}"""
  }

  class TailrecNotApplicable(symbol: Symbol)(using Context)
    extends SyntaxMsg(TailrecNotApplicableID) {
    def msg = {
      val reason =
        if (!symbol.is(Method)) em"$symbol isn't a method"
        else if (symbol.is(Deferred)) em"$symbol is abstract"
        else if (!symbol.isEffectivelyFinal) em"$symbol is neither ${hl("private")} nor ${hl("final")} so can be overridden"
        else em"$symbol contains no recursive calls"

      s"TailRec optimisation not applicable, $reason"
    }
    def explain = ""
  }

  class FailureToEliminateExistential(tp: Type, tp1: Type, tp2: Type, boundSyms: List[Symbol], classRoot: Symbol)(using Context)
    extends Message(FailureToEliminateExistentialID) {
    def kind = MessageKind.Compatibility
    def msg =
      val originalType = ctx.printer.dclsText(boundSyms, "; ").show
      em"""An existential type that came from a Scala-2 classfile for $classRoot
          |cannot be mapped accurately to a Scala-3 equivalent.
          |original type    : $tp forSome ${originalType}
          |reduces to       : $tp1
          |type used instead: $tp2
          |This choice can cause follow-on type errors or hide type errors.
          |Proceed at own risk."""
    def explain =
      em"""Existential types in their full generality are no longer supported.
          |Scala-3 does applications of class types to wildcard type arguments.
          |Other forms of existential types that come from Scala-2 classfiles
          |are only approximated in a best-effort way."""
  }

  class OnlyFunctionsCanBeFollowedByUnderscore(tp: Type)(using Context)
    extends SyntaxMsg(OnlyFunctionsCanBeFollowedByUnderscoreID) {
    def msg = em"Only function types can be followed by ${hl("_")} but the current expression has type $tp"
    def explain =
      em"""The syntax ${hl("x _")} is no longer supported if ${hl("x")} is not a function.
          |To convert to a function value, you need to explicitly write ${hl("() => x")}"""
  }

  class MissingEmptyArgumentList(method: String)(using Context)
    extends SyntaxMsg(MissingEmptyArgumentListID) {
    def msg = em"$method must be called with ${hl("()")} argument"
    def explain = {
      val codeExample =
        """def next(): T = ...
          |next     // is expanded to next()"""

      em"""Previously an empty argument list () was implicitly inserted when calling a nullary method without arguments. E.g.
          |
          |$codeExample
          |
          |In Dotty, this idiom is an error. The application syntax has to follow exactly the parameter syntax.
          |Excluded from this rule are methods that are defined in Java or that override methods defined in Java."""
    }
  }

  class DuplicateNamedTypeParameter(name: Name)(using Context)
    extends SyntaxMsg(DuplicateNamedTypeParameterID) {
    def msg = em"Type parameter $name was defined multiple times."
    def explain = ""
  }

  class UndefinedNamedTypeParameter(undefinedName: Name, definedNames: List[Name])(using Context)
    extends SyntaxMsg(UndefinedNamedTypeParameterID) {
    def msg = em"Type parameter $undefinedName is undefined. Expected one of ${definedNames.map(_.show).mkString(", ")}."
    def explain = ""
  }

  class IllegalStartOfStatement(what: String, isModifier: Boolean, isStat: Boolean)(using Context) extends SyntaxMsg(IllegalStartOfStatementID) {
    def msg =
      if isStat then
        "this kind of statement is not allowed here"
      else
        val addendum = if isModifier then ": this modifier is not allowed here" else ""
        s"Illegal start of $what$addendum"
    def explain =
      i"""A statement is an import or export, a definition or an expression.
         |Some statements are only allowed in certain contexts"""
  }

  class TraitIsExpected(symbol: Symbol)(using Context) extends SyntaxMsg(TraitIsExpectedID) {
    def msg = em"$symbol is not a trait"
    def explain = {
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

      em"""Only traits can be mixed into classes using a ${hl("with")} keyword.
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
    def msg = em"Traits cannot redefine final $method from ${hl("class AnyRef")}."
    def explain = ""
  }

  class AlreadyDefined(name: Name, owner: Symbol, conflicting: Symbol)(using Context) extends NamingMsg(AlreadyDefinedID):
    private def where: String =
      if conflicting.effectiveOwner.is(Package) && conflicting.associatedFile != null then
        i" in ${conflicting.associatedFile}"
      else if conflicting.owner == owner then ""
      else i" in ${conflicting.owner}"
    private def note =
      if owner.is(Method) || conflicting.is(Method) then
        "\n\nNote that overloaded methods must all be defined in the same group of toplevel definitions"
      else ""
    def msg =
      if conflicting.isTerm != name.isTermName then
        em"$name clashes with $conflicting$where; the two must be defined together"
      else
        em"$name is already defined as $conflicting$where$note"
    def explain = ""

  class PackageNameAlreadyDefined(pkg: Symbol)(using Context) extends NamingMsg(PackageNameAlreadyDefinedID) {
    lazy val (where, or) =
      if pkg.associatedFile == null then ("", "")
      else (s" in ${pkg.associatedFile}", " or delete the containing class file")
    def msg = em"""${pkg.name} is the name of $pkg$where.
                          |It cannot be used at the same time as the name of a package."""
    def explain =
      em"""An ${hl("object")} or other toplevel definition cannot have the same name as an existing ${hl("package")}.
          |Rename either one of them$or."""
  }

  class UnapplyInvalidNumberOfArguments(qual: untpd.Tree, argTypes: List[Type])(using Context)
    extends SyntaxMsg(UnapplyInvalidNumberOfArgumentsID) {
    def msg = em"Wrong number of argument patterns for $qual; expected: ($argTypes%, %)"
    def explain =
      em"""The Unapply method of $qual was used with incorrect number of arguments.
          |Expected usage would be something like:
          |case $qual(${argTypes.map(_ => '_')}%, %) => ...
          |
        |where subsequent arguments would have following types: ($argTypes%, %).
        |""".stripMargin
  }

  class UnapplyInvalidReturnType(unapplyResult: Type, unapplyName: Name)(using Context)
    extends DeclarationMsg(UnapplyInvalidReturnTypeID) {
    def msg =
      val addendum =
        if Feature.migrateTo3 && unapplyName == nme.unapplySeq
        then "\nYou might want to try to rewrite the extractor to use `unapply` instead."
        else ""
      em"""| ${Red(i"$unapplyResult")} is not a valid result type of an $unapplyName method of an ${Magenta("extractor")}.$addendum"""
    def explain = if (unapplyName.show == "unapply")
      em"""
          |To be used as an extractor, an unapply method has to return a type that either:
          | - has members ${Magenta("isEmpty: Boolean")} and ${Magenta("get: S")} (usually an ${Green("Option[S]")})
          | - is a ${Green("Boolean")}
          | - is a ${Green("Product")} (like a ${Magenta("Tuple2[T1, T2]")})
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
        """.stripMargin
    else
      em"""
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
        """.stripMargin
  }

  class StaticFieldsOnlyAllowedInObjects(member: Symbol)(using Context) extends SyntaxMsg(StaticFieldsOnlyAllowedInObjectsID) {
    def msg = em"${hl("@static")} $member in ${member.owner} must be defined inside a static ${hl("object")}."
    def explain =
      em"${hl("@static")} members are only allowed inside objects."
  }

  class StaticFieldsShouldPrecedeNonStatic(member: Symbol, defns: List[tpd.Tree])(using Context) extends SyntaxMsg(StaticFieldsShouldPrecedeNonStaticID) {
    def msg = em"${hl("@static")} $member in ${member.owner} must be defined before non-static fields."
    def explain = {
      val nonStatics = defns.takeWhile(_.symbol != member).take(3).filter(_.isInstanceOf[tpd.ValDef])
      val codeExample = s"""object ${member.owner.name.firstPart} {
                        |  @static ${member} = ...
                        |  ${nonStatics.map(m => s"${m.symbol} = ...").mkString("\n  ")}
                        |  ...
                        |}"""
      em"""The fields annotated with @static should precede any non @static fields.
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
    def msg = em"Cyclic inheritance: $symbol extends itself$addendum"
    def explain = {
      val codeExample = "class A extends A"

      em"""Cyclic inheritance is prohibited in Dotty.
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
    def msg = {
      val denotationOwner = denot.owner
      val denotationName = ctx.fresh.setSetting(ctx.settings.YdebugNames, true).printer.nameString(denot.name)
      val file = denot.symbol.associatedFile
      val (location, src) =
        if (file != null) (s" in $file", file.toString)
        else ("", "the signature")

      em"""Bad symbolic reference. A signature$location
          |refers to $denotationName in ${denotationOwner.showKind} ${denotationOwner.showFullName} which is not available.
          |It may be completely missing from the current classpath, or the version on
          |the classpath might be incompatible with the version used when compiling $src."""
    }

    def explain = ""
  }

  class UnableToExtendSealedClass(pclazz: Symbol)(using Context) extends SyntaxMsg(UnableToExtendSealedClassID) {
    def msg = em"Cannot extend ${hl("sealed")} $pclazz in a different source file"
    def explain = "A sealed class or trait can only be extended in the same file as its declaration"
  }

  class SymbolHasUnparsableVersionNumber(symbol: Symbol, errorMessage: String)(using Context)
  extends SyntaxMsg(SymbolHasUnparsableVersionNumberID) {
    def msg = em"${symbol.showLocated} has an unparsable version number: $errorMessage"
    def explain =
      em"""The ${symbol.showLocated} is marked with ${hl("@migration")} indicating it has changed semantics
          |between versions and the ${hl("-Xmigration")} settings is used to warn about constructs
          |whose behavior may have changed since version change."""
  }

  class SymbolChangedSemanticsInVersion(
    symbol: Symbol,
    migrationVersion: ScalaVersion,
    migrationMessage: String
  )(using Context) extends SyntaxMsg(SymbolChangedSemanticsInVersionID) {
    def msg = em"${symbol.showLocated} has changed semantics in version $migrationVersion: $migrationMessage"
    def explain =
      em"""The ${symbol.showLocated} is marked with ${hl("@migration")} indicating it has changed semantics
          |between versions and the ${hl("-Xmigration")} settings is used to warn about constructs
          |whose behavior may have changed since version change."""
  }

  class UnableToEmitSwitch()(using Context)
  extends SyntaxMsg(UnableToEmitSwitchID) {
    def msg = em"Could not emit switch for ${hl("@switch")} annotated match"
    def explain = {
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

      em"""If annotated with ${hl("@switch")}, the compiler will verify that the match has been compiled to a
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
    def msg = em"${member.owner} does not have a companion class"
    def explain =
      em"An object that contains ${hl("@static")} members must have a companion class."
  }

  class PolymorphicMethodMissingTypeInParent(rsym: Symbol, parentSym: Symbol)(using Context)
  extends SyntaxMsg(PolymorphicMethodMissingTypeInParentID) {
    def msg = em"Polymorphic refinement $rsym without matching type in parent $parentSym is no longer allowed"
    def explain =
      em"""Polymorphic $rsym is not allowed in the structural refinement of $parentSym because
          |$rsym does not override any method in $parentSym. Structural refinement does not allow for
          |polymorphic methods."""
  }

  class ParamsNoInline(owner: Symbol)(using Context)
    extends SyntaxMsg(ParamsNoInlineID) {
    def msg = em"""${hl("inline")} modifier can only be used for parameters of inline methods"""
    def explain = ""
  }

  class JavaSymbolIsNotAValue(symbol: Symbol)(using Context) extends TypeMsg(JavaSymbolIsNotAValueID) {
    def msg = {
      val kind =
        if (symbol is Package) em"$symbol"
        else em"Java defined ${hl("class " + symbol.name)}"

      s"$kind is not a value"
    }
    def explain = ""
  }

  class DoubleDefinition(decl: Symbol, previousDecl: Symbol, base: Symbol)(using Context) extends NamingMsg(DoubleDefinitionID) {
    def msg = {
      def nameAnd = if (decl.name != previousDecl.name) " name and" else ""
      def erasedType = if ctx.erasedTypes then i" ${decl.info}" else ""
      def details(using Context): String =
        if (decl.isRealMethod && previousDecl.isRealMethod) {
          import Signature.MatchDegree._

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
        em"""$clashDescription:
            |${previousDecl.showDcl} ${symLocation(previousDecl)} and
            |${decl.showDcl} ${symLocation(decl)}
            |"""
      } + details
    }
    def explain = ""
  }

  class ImportRenamedTwice(ident: untpd.Ident)(using Context) extends SyntaxMsg(ImportRenamedTwiceID) {
    def msg = s"${ident.show} is renamed twice on the same import line."
    def explain = ""
  }

  class TypeTestAlwaysDiverges(scrutTp: Type, testTp: Type)(using Context) extends SyntaxMsg(TypeTestAlwaysDivergesID) {
    def msg =
      s"This type test will never return a result since the scrutinee type ${scrutTp.show} does not contain any value."
    def explain = ""
  }

  // Relative of CyclicReferenceInvolvingImplicit and RecursiveValueNeedsResultType
  class TermMemberNeedsResultTypeForImplicitSearch(cycleSym: Symbol)(using Context)
    extends CyclicMsg(TermMemberNeedsNeedsResultTypeForImplicitSearchID) {
    def msg = em"""$cycleSym needs result type because its right-hand side attempts implicit search"""
    def explain =
      em"""|The right hand-side of $cycleSym's definition requires an implicit search at the highlighted position.
           |To avoid this error, give `$cycleSym` an explicit type.
           |""".stripMargin
  }

  class ClassCannotExtendEnum(cls: Symbol, parent: Symbol)(using Context) extends SyntaxMsg(ClassCannotExtendEnumID) {
    def msg = em"""$cls in ${cls.owner} extends enum ${parent.name}, but extending enums is prohibited."""
    def explain = ""
  }

  class NotAnExtractor(tree: untpd.Tree)(using Context) extends SyntaxMsg(NotAnExtractorID) {
    def msg = em"$tree cannot be used as an extractor in a pattern because it lacks an unapply or unapplySeq method"
    def explain =
      em"""|An ${hl("unapply")} method should be defined in an ${hl("object")} as follow:
           |  - If it is just a test, return a ${hl("Boolean")}. For example ${hl("case even()")}
           |  - If it returns a single sub-value of type T, return an ${hl("Option[T]")}
           |  - If it returns several sub-values T1,...,Tn, group them in an optional tuple ${hl("Option[(T1,...,Tn)]")}
           |
           |Sometimes, the number of sub-values isn't fixed and we would like to return a sequence.
           |For this reason, you can also define patterns through ${hl("unapplySeq")} which returns ${hl("Option[Seq[T]]")}.
           |This mechanism is used for instance in pattern ${hl("case List(x1, ..., xn)")}""".stripMargin
  }

  class MemberWithSameNameAsStatic()(using Context)
    extends SyntaxMsg(MemberWithSameNameAsStaticID) {
    def msg = em"Companion classes cannot define members with same name as a ${hl("@static")} member"
    def explain = ""
  }

  class PureExpressionInStatementPosition(stat: untpd.Tree, val exprOwner: Symbol)(using Context)
    extends Message(PureExpressionInStatementPositionID) {
    def kind = MessageKind.PotentialIssue
    def msg = "A pure expression does nothing in statement position; you may be omitting necessary parentheses"
    def explain =
      em"""The pure expression $stat doesn't have any side effect and its result is not assigned elsewhere.
          |It can be removed without changing the semantics of the program. This may indicate an error.""".stripMargin
  }

  class TraitCompanionWithMutableStatic()(using Context)
    extends SyntaxMsg(TraitCompanionWithMutableStaticID) {
    def msg = em"Companion of traits cannot define mutable @static fields"
    def explain = ""
  }

  class LazyStaticField()(using Context)
    extends SyntaxMsg(LazyStaticFieldID) {
    def msg = em"Lazy @static fields are not supported"
    def explain = ""
  }

  class StaticOverridingNonStaticMembers()(using Context)
    extends SyntaxMsg(StaticOverridingNonStaticMembersID) {
    def msg = em"${hl("@static")} members cannot override or implement non-static ones"
    def explain = ""
  }

  class OverloadInRefinement(rsym: Symbol)(using Context)
    extends DeclarationMsg(OverloadInRefinementID) {
    def msg = "Refinements cannot introduce overloaded definitions"
    def explain =
      em"""The refinement `$rsym` introduces an overloaded definition.
          |Refinements cannot contain overloaded definitions.""".stripMargin
  }

  class NoMatchingOverload(val alternatives: List[SingleDenotation], pt: Type)(using Context)
    extends TypeMsg(NoMatchingOverloadID) {
    def msg =
      em"""None of the ${err.overloadedAltsStr(alternatives)}
          |match ${err.expectedTypeStr(pt)}"""
    def explain = ""
  }
  class StableIdentPattern(tree: untpd.Tree, pt: Type)(using Context)
    extends TypeMsg(StableIdentPatternID) {
    def msg =
      em"""Stable identifier required, but $tree found"""
    def explain = ""
  }

  class IllegalSuperAccessor(base: Symbol, memberName: Name, targetName: Name,
      acc: Symbol, accTp: Type,
      other: Symbol, otherTp: Type)(using Context) extends DeclarationMsg(IllegalSuperAccessorID) {
    def msg = {
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
      ex"""$base cannot be defined due to a conflict between its parents when
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
          |""".stripMargin
    }
    def explain = ""
  }

  class TraitParameterUsedAsParentPrefix(cls: Symbol)(using Context)
    extends DeclarationMsg(TraitParameterUsedAsParentPrefixID) {
    def msg =
      s"${cls.show} cannot extend from a parent that is derived via its own parameters"
    def explain =
      ex"""
          |The parent class/trait that ${cls.show} extends from is obtained from
          |the parameter of ${cls.show}. This is disallowed in order to prevent
          |outer-related Null Pointer Exceptions in Scala.
          |
          |In order to fix this issue consider directly extending from the parent rather
          |than obtaining it from the parameters of ${cls.show}.
          |""".stripMargin
  }

  class UnknownNamedEnclosingClassOrObject(name: TypeName)(using Context)
    extends ReferenceMsg(UnknownNamedEnclosingClassOrObjectID) {
    def msg =
      em"""no enclosing class or object is named '${hl(name.show)}'"""
    def explain =
      ex"""
      |The class or object named '${hl(name.show)}' was used as a visibility
      |modifier, but could not be resolved. Make sure that
      |'${hl(name.show)}' is not misspelled and has been imported into the
      |current scope.
      """.stripMargin
    }

  class IllegalCyclicTypeReference(sym: Symbol, where: String, lastChecked: Type)(using Context)
    extends CyclicMsg(IllegalCyclicTypeReferenceID) {
    def msg =
      val lastCheckedStr =
        try lastChecked.show
        catch case ex: CyclicReference => "..."
      i"illegal cyclic type reference: ${where} ${hl(lastCheckedStr)} of $sym refers back to the type itself"
    def explain = ""
  }

  class ErasedTypesCanOnlyBeFunctionTypes()(using Context)
    extends SyntaxMsg(ErasedTypesCanOnlyBeFunctionTypesID) {
    def msg = "Types with erased keyword can only be function types `(erased ...) => ...`"
    def explain = ""
  }

  class CaseClassMissingNonImplicitParamList(cdef: untpd.TypeDef)(using Context)
    extends SyntaxMsg(CaseClassMissingNonImplicitParamListID) {
    def msg =
      em"""|A ${hl("case class")} must have at least one leading non-implicit parameter list"""

    def explain =
      em"""|${cdef.name} must have at least one leading non-implicit parameter list,
           | if you're aiming to have a case class parametrized only by implicit ones, you should
           | add an explicit ${hl("()")} as the first parameter list to ${cdef.name}.""".stripMargin
  }

  class EnumerationsShouldNotBeEmpty(cdef: untpd.TypeDef)(using Context)
    extends SyntaxMsg(EnumerationsShouldNotBeEmptyID) {
    def msg = "Enumerations must contain at least one case"

    def explain =
      em"""|Enumeration ${cdef.name} must contain at least one case
           |Example Usage:
           | ${hl("enum")} ${cdef.name} {
           |    ${hl("case")} Option1, Option2
           | }
           |""".stripMargin
  }

  class TypedCaseDoesNotExplicitlyExtendTypedEnum(enumDef: Symbol, caseDef: untpd.TypeDef)(using Context)
    extends SyntaxMsg(TypedCaseDoesNotExplicitlyExtendTypedEnumID) {
    def msg = i"explicit extends clause needed because both enum case and enum class have type parameters"

    def explain =
      em"""Enumerations where the enum class as well as the enum case have type parameters need
          |an explicit extends.
          |for example:
          | ${hl("enum")} ${enumDef.name}[T] {
          |  ${hl("case")} ${caseDef.name}[U](u: U) ${hl("extends")} ${enumDef.name}[U]
          | }
          |""".stripMargin
  }

  class IllegalRedefinitionOfStandardKind(kindType: String, name: Name)(using Context)
    extends SyntaxMsg(IllegalRedefinitionOfStandardKindID) {
    def msg = em"illegal redefinition of standard $kindType $name"
    def explain =
      em"""| "$name" is a standard Scala core `$kindType`
           | Please choose a different name to avoid conflicts
           |""".stripMargin
  }

  class NoExtensionMethodAllowed(mdef: untpd.DefDef)(using Context)
    extends SyntaxMsg(NoExtensionMethodAllowedID) {
    def msg = em"No extension method allowed here, since collective parameters are given"
    def explain =
      em"""|Extension method:
           |  `${mdef}`
           |is defined inside an extension clause which has collective parameters.
           |""".stripMargin
  }

  class ExtensionMethodCannotHaveTypeParams(mdef: untpd.DefDef)(using Context)
    extends SyntaxMsg(ExtensionMethodCannotHaveTypeParamsID) {
    def msg = i"Extension method cannot have type parameters since some were already given previously"

    def explain =
      em"""|Extension method:
           |  `${mdef}`
           |has type parameters `[${mdef.leadingTypeParams.map(_.show).mkString(",")}]`, while the extension clause has
           |it's own type parameters. Please consider moving these to the extension clause's type parameter list.
           |""".stripMargin
  }

  class ExtensionCanOnlyHaveDefs(mdef: untpd.Tree)(using Context)
    extends SyntaxMsg(ExtensionCanOnlyHaveDefsID) {
    def msg = em"Only methods allowed here, since collective parameters are given"
    def explain =
      em"""Extension clauses can only have `def`s
          | `${mdef.show}` is not a valid expression here.
          |""".stripMargin
  }

  class UnexpectedPatternForSummonFrom(tree: Tree[_])(using Context)
    extends SyntaxMsg(UnexpectedPatternForSummonFromID) {
    def msg = em"Unexpected pattern for summonFrom. Expected ${hl("`x: T`")} or ${hl("`_`")}"
    def explain =
      em"""|The pattern "${tree.show}" provided in the ${hl("case")} expression of the ${hl("summonFrom")},
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
           |""".stripMargin
  }

  class AnonymousInstanceCannotBeEmpty(impl:  untpd.Template)(using Context)
    extends SyntaxMsg(AnonymousInstanceCannotBeEmptyID) {
    def msg = i"anonymous instance must implement a type or have at least one extension method"
    def explain =
      em"""|Anonymous instances cannot be defined with an empty body. The block
           |`${impl.show}` should either contain an implemented type or at least one extension method.
           |""".stripMargin
  }

  class ModifierNotAllowedForDefinition(flag: Flag)(using Context)
    extends SyntaxMsg(ModifierNotAllowedForDefinitionID) {
    def msg = em"Modifier ${hl(flag.flagsString)} is not allowed for this definition"
    def explain = ""
  }

  class RedundantModifier(flag: Flag)(using Context)
    extends SyntaxMsg(RedundantModifierID) {
    def msg = em"Modifier ${hl(flag.flagsString)} is redundant for this definition"
    def explain = ""
  }

  class InvalidReferenceInImplicitNotFoundAnnotation(typeVar: String, owner: String)(using Context)
    extends ReferenceMsg(InvalidReferenceInImplicitNotFoundAnnotationID) {
    def msg = em"""|Invalid reference to a type variable ${hl(typeVar)} found in the annotation argument.
                   |The variable does not occur as a parameter in the scope of ${hl(owner)}.
                   |""".stripMargin
    def explain = ""
  }

  class CaseClassInInlinedCode(tree: tpd.Tree)(using Context)
    extends SyntaxMsg(CaseClassInInlinedCodeID) {

    def defKind = if tree.symbol.is(Module) then "object" else "class"
    def msg = s"Case $defKind definitions are not allowed in inline methods or quoted code. Use a normal $defKind instead."
    def explain =
      em"""Case class/object definitions generate a considerable footprint in code size.
          |Inlining such definition would multiply this footprint for each call site.
          |""".stripMargin
  }

  class ImplicitSearchTooLargeWarning(limit: Int, openSearchPairs: List[(Candidate, Type)])(using Context)
    extends TypeMsg(ImplicitSearchTooLargeID):
    override def showAlways = true
    def showQuery(query: (Candidate, Type)): String =
      i"  ${query._1.ref.symbol.showLocated}  for  ${query._2}}"
    def msg =
      em"""Implicit search problem too large.
          |an implicit search was terminated with failure after trying $limit expressions.
          |The root candidate for the search was:
          |
          |${showQuery(openSearchPairs.last)}
          |
          |You can change the behavior by setting the `-Ximplicit-search-limit` value.
          |Smaller values cause the search to fail faster.
          |Larger values might make a very large search problem succeed.
          |"""
    def explain =
      em"""The overflow happened with the following lists of tried expressions and target types,
          |starting with the root query:
          |
          |${openSearchPairs.reverse.map(showQuery)}%\n%
        """

  class TargetNameOnTopLevelClass(symbol: Symbol)(using Context)
  extends SyntaxMsg(TargetNameOnTopLevelClassID):
    def msg = em"${hl("@targetName")} annotation not allowed on top-level $symbol"
    def explain =
      val annot = symbol.getAnnotation(defn.TargetNameAnnot).get
      em"""The @targetName annotation may be applied to a top-level ${hl("val")} or ${hl("def")}, but not
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
    def msg = ex"$tp is not a class type"
    def explain = ""
