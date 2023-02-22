package dotty.tools
package dotc
package transform

import core._
import Scopes.newScope
import Contexts._, Symbols._, Types._, Flags._, Decorators._, StdNames._, Constants._
import MegaPhase._
import SymUtils._
import NullOpsDecorator._
import ast.untpd

/** Expand SAM closures that cannot be represented by the JVM as lambdas to anonymous classes.
 *  These fall into five categories
 *
 *   1. Partial function closures, we need to generate isDefinedAt and applyOrElse methods for these.
 *   2. Closures implementing non-trait classes
 *   3. Closures implementing classes that inherit from a class other than Object
 *      (a lambda cannot not be a run-time subtype of such a class)
 *   4. Closures that implement traits which run initialization code.
 *   5. Closures that get synthesized abstract methods in the transformation pipeline. These methods can be
 *      (1) superaccessors, (2) outer references, (3) accessors for fields.
 *
 *  However, implicit function types do not count as SAM types.
 */
object ExpandSAMs:
  val name: String = "expandSAMs"
  val description: String = "expand SAM closures to anonymous classes"

  /** Is the SAMType `cls` also a SAM under the rules of the platform? */
  def isPlatformSam(cls: ClassSymbol)(using Context): Boolean =
    ctx.platform.isSam(cls)

  def needsWrapperClass(tpe: Type)(using Context): Boolean =
    tpe.classSymbol match
      case cls: ClassSymbol => !isPlatformSam(cls) || cls == defn.PartialFunctionClass
      case _ => false

class ExpandSAMs extends MiniPhase:
  import ast.tpd._

  override def phaseName: String = ExpandSAMs.name

  override def description: String = ExpandSAMs.description

  override def transformBlock(tree: Block)(using Context): Tree = tree match {
    case Block(stats @ (fn: DefDef) :: Nil, Closure(_, fnRef, tpt)) if fnRef.symbol == fn.symbol =>
      tpt.tpe match {
        case NoType =>
          tree // it's a plain function
        case tpe if tpe.isContextFunctionType =>
          tree
        case tpe @ SAMType(_) if tpe.isRef(defn.PartialFunctionClass) =>
          val tpe1 = checkRefinements(tpe, fn)
          toPartialFunction(tree, tpe1)
        case tpe @ SAMType(_) if ExpandSAMs.isPlatformSam(tpe.classSymbol.asClass) =>
          checkRefinements(tpe, fn)
          tree
        case tpe =>
          val tpe1 = checkRefinements(tpe.stripNull, fn)
          val Seq(samDenot) = tpe1.possibleSamMethods
          cpy.Block(tree)(stats,
              AnonClass(tpe1 :: Nil, fn.symbol.asTerm :: Nil, samDenot.symbol.asTerm.name :: Nil))
      }
    case _ =>
      tree
  }

  private def checkNoContextFunction(tpt: Tree)(using Context): Unit =
    if tpt.tpe.isContextFunctionType then
      report.error(
        em"""Implementation restriction: cannot convert this expression to
            |partial function with context function result type $tpt""",
        tpt.srcPos)

  /** A partial function literal:
   *
   *  ```
   *  val x: PartialFunction[A, B] = { case C1 => E1; ...; case Cn => En }
   *  ```
   *
   *  which desugars to:
   *
   *  ```
   *  val x: PartialFunction[A, B] = {
   *    def $anonfun(x: A): B = x match { case C1 => E1; ...; case Cn => En }
   *    closure($anonfun: PartialFunction[A, B])
   *  }
   *  ```
   *
   *  is expanded to an anomymous class:
   *
   *  ```
   *  val x: PartialFunction[A, B] = {
   *    class $anon extends AbstractPartialFunction[A, B] {
   *      final def isDefinedAt(x: A): Boolean = x match {
   *        case C1 => true
   *        ...
   *        case Cn => true
   *        case _  => false
   *      }
   *
   *      final def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = x match {
   *        case C1 => E1
   *        ...
   *        case Cn => En
   *        case _  => default(x)
   *      }
   *    }
   *
   *    new $anon
   *  }
   *  ```
   */
  private def toPartialFunction(tree: Block, tpe: Type)(using Context): Tree = {
    val closureDef(anon @ DefDef(_, List(List(param)), _, _)) = tree: @unchecked

    checkNoContextFunction(anon.tpt)

    // The right hand side from which to construct the partial function. This is always a Match.
    // If the original rhs is already a Match (possibly in braces), return that.
    // Otherwise construct a match `x match case _ => rhs` where `x` is the parameter of the closure.
    def partialFunRHS(tree: Tree): Match = tree match
      case m: Match => m
      case Block(Nil, expr) => partialFunRHS(expr)
      case _ =>
        Match(ref(param.symbol),
          CaseDef(untpd.Ident(nme.WILDCARD).withType(param.symbol.info), EmptyTree, tree) :: Nil)

    val pfRHS = partialFunRHS(anon.rhs)
    val anonSym = anon.symbol
    val anonTpe = anon.tpe.widen
    val parents = List(
      defn.AbstractPartialFunctionClass.typeRef.appliedTo(anonTpe.firstParamTypes.head, anonTpe.resultType),
      defn.SerializableType)

    AnonClass(anonSym.owner, parents, tree.span) { pfSym =>
      def overrideSym(sym: Symbol) = sym.copy(
        owner = pfSym,
        flags = Synthetic | Method | Final | Override,
        info = tpe.memberInfo(sym),
        coord = tree.span).asTerm.entered
      val isDefinedAtFn = overrideSym(defn.PartialFunction_isDefinedAt)
      val applyOrElseFn = overrideSym(defn.PartialFunction_applyOrElse)

      def translateMatch(tree: Match, pfParam: Symbol, cases: List[CaseDef], defaultValue: Tree)(using Context) = {
        val selector = tree.selector
        val selectorTpe = selector.tpe.widen
        val defaultSym = newSymbol(pfParam.owner, nme.WILDCARD, SyntheticCase, selectorTpe)
        val defaultCase =
          CaseDef(
            Bind(defaultSym, Underscore(selectorTpe)),
            EmptyTree,
            defaultValue)
        val unchecked = selector.annotated(New(ref(defn.UncheckedAnnot.typeRef)))
        cpy.Match(tree)(unchecked, cases :+ defaultCase)
          .subst(param.symbol :: Nil, pfParam :: Nil)
            // Needed because  a partial function can be written as:
            // param => param match { case "foo" if foo(param) => param }
            // And we need to update all references to 'param'
      }

      def isDefinedAtRhs(paramRefss: List[List[Tree]])(using Context) = {
        val tru = Literal(Constant(true))
        def translateCase(cdef: CaseDef) =
          cpy.CaseDef(cdef)(body = tru).changeOwner(anonSym, isDefinedAtFn)
        val paramRef = paramRefss.head.head
        val defaultValue = Literal(Constant(false))
        translateMatch(pfRHS, paramRef.symbol, pfRHS.cases.map(translateCase), defaultValue)
      }

      def applyOrElseRhs(paramRefss: List[List[Tree]])(using Context) = {
        val List(paramRef, defaultRef) = paramRefss(1)
        def translateCase(cdef: CaseDef) =
          cdef.changeOwner(anonSym, applyOrElseFn)
        val defaultValue = defaultRef.select(nme.apply).appliedTo(paramRef)
        translateMatch(pfRHS, paramRef.symbol, pfRHS.cases.map(translateCase), defaultValue)
      }

      val isDefinedAtDef = transformFollowingDeep(DefDef(isDefinedAtFn, isDefinedAtRhs(_)(using ctx.withOwner(isDefinedAtFn))))
      val applyOrElseDef = transformFollowingDeep(DefDef(applyOrElseFn, applyOrElseRhs(_)(using ctx.withOwner(applyOrElseFn))))
      List(isDefinedAtDef, applyOrElseDef)
    }
  }

  private def checkRefinements(tpe: Type, tree: Tree)(using Context): Type = tpe.dealias match {
    case RefinedType(parent, name, _) =>
      if (name.isTermName && tpe.member(name).symbol.ownersIterator.isEmpty) // if member defined in the refinement
        report.error(em"Lambda does not define $name", tree.srcPos)
      checkRefinements(parent, tree)
    case tpe =>
      tpe
  }
end ExpandSAMs
