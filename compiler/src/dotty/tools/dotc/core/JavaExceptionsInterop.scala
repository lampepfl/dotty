package dotty.tools.dotc.core

import Contexts.*
import Types.*
import Symbols.*
import Flags.JavaDefined
import Names.*
import dotty.tools.dotc.config.Feature
import dotty.tools.dotc.config.Printers.saferExceptions
import Decorators.i
import dotty.tools.dotc.ast.Trees.ValOrDefDef

/*
TODO : Functionnality purposes
  1 - Fetch from the java source (.java or .class) a list of all the thrown exceptions
  2 - Create an disjonction type between all of them (use OrType to do that)
  3 - Fix the TermName of the synthetic using clause generated in JavaExceptionsInterop::curryCanThrow

TODO : Debuging purposes
  1 - Add a new Printer to print information on "safer exceptions"
  2 - Use the reporter to report warnings or error that may happen in this step
*/

object JavaExceptionsInterop :

  /**
   * Return the ClassSymbol that corresponds to
   *   'scala.CanThrow' class
   * @return
   */
  private inline def canThrowType(using Context) = defn.CanThrowClass

  /**
   * Checks if the "safer exceptions" feature is enables in a file
   * To enable "safer exceptions", use this import :
   *   'import scala.language.experimental.saferExceptions'
   * @return (Boolean) - true is the feature is enabled. False otherwise
   */
  def isEnabled(using Context) = Feature.enabled(Feature.saferExceptions)

  /**
   *
   *
   * @param sym
   * @param tp
   * @return
   */
  def canThrowMember(sym: Symbol, tp: Type, exceptions : List[Type])(using Context) : Type =
    //assert(sym.is(JavaDefined), "can only decorate java defined methods")
    assert(isEnabled, "saferExceptions is disabled")
    assert(exceptions.nonEmpty, "Cannot append empty exceptions")
    saferExceptions.println(i"Synthesizing '$canThrowType' clause for symbol $sym of type: $tp")
    println(i"Exceptions to add to the symbol are $exceptions")
    println(tp)
    tp match
      case mtd@MethodType(terms) =>
        val synthTp = curryCanThrow(mtd, terms, exceptions)
        saferExceptions.println(i"Compiler synthesized type '$synthTp' for symbol $sym")
        synthTp
      case _ =>
        tp


  /**
   * Add a curried parameters of type CanThrow[Excpetion] to the end of the parameter list
   * @rtnType should be any other type
   * */
  def curryCanThrow(mtd : MethodType, param : List[TermName], exceptions : List[Type])(using Context) : Type =
    mtd.resType match
      case a@MethodType(terms) =>
        CachedMethodType(terms)(_ => mtd.paramInfos, _ => curryCanThrow(a, terms, exceptions), mtd.companion)
      case t =>
        CachedMethodType(param)(_ => mtd.paramInfos, _ => canThrowMethodType(t, exceptions), mtd.companion)

  def canThrowMethodType(resType : Type, exceptions : List[Type])(using Context) =
    // First join all the expections with a disjonction
    val reduced_exceptions = exceptions.reduce(OrType(_, _, true)) // TODO : Not sure what true does, to check...
    saferExceptions.println(i"Joining all the Exception types into one disjonction type '$reduced_exceptions'")

    // Apply the disjonction to the CanThrow type parameter
    saferExceptions.println(i"Applying the exceptions '$reduced_exceptions' to the polymorphic type '$canThrowType'")
    val genericType = canThrowType.typeRef.applyIfParameterized(reduced_exceptions :: Nil)
    saferExceptions.println(i"Type of the synthetic '$canThrowType' clause is '$genericType'")

    // Build the MethodTypeCompanion
    val isErased = canThrowType is Flags.Erased
    val companion = MethodType.companion(isContextual = true, isErased = isErased)

    // TODO : Maybe use MethodType::fromSymbols instead of creating it from scratch ?
    val syntheticClause = CachedMethodType(companion.syntheticParamNames(1))(_ => genericType :: Nil, _ => resType, companion)
    saferExceptions.println(i"The synthetic clause is '$syntheticClause'")
    syntheticClause
