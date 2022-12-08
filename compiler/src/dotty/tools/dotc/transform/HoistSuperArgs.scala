package dotty.tools.dotc
package transform

import MegaPhase._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import ast.TreeTypeMap
import core.Types._
import core.Flags._
import core.Decorators._
import collection.mutable
import ast.Trees._
import core.NameKinds.SuperArgName
import SymUtils._
import core.Decorators.*

object HoistSuperArgs {
  val name: String = "hoistSuperArgs"
  val description: String = "hoist complex arguments of supercalls to enclosing scope"
}

/** This phase hoists complex arguments of supercalls and this-calls out of the enclosing class.
 *  Example:
 *
 *      class B(y: Int) extends A({ def f(x: Int) = x * x; f(y)})
 *
 *  is translated to
 *
 *      class B(y: Int) extends A(B#B$superArg$1(this.y)) {
 *        private <static> def B$superArg$1(y: Int): Int = {
 *          def f(x: Int): Int = x.*(x); f(y)
 *        }
 *      }
 *
 *  An argument is complex if it contains a method or template definition, a this or a new,
 *  or it contains an identifier which needs a `this` prefix to be accessed. This is the case
 *  if the identifier has neither a global reference nor a reference to a parameter of the enclosing class.
 *  @see needsHoist for an implementation.
 *
 *  A hoisted argument definition gets the parameters of the class it is hoisted from
 *  as method parameters. The definition is installed in the scope enclosing the class,
 *  or, if that is a package, it is made a static method of the class itself.
 */
class HoistSuperArgs extends MiniPhase, IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = HoistSuperArgs.name

  override def description: String = HoistSuperArgs.description

  override def runsAfter: Set[String] = Set(ElimByName.name)
    // By name closures need to be introduced first in order to be hoisted out here.

  /** Defines methods for hoisting complex supercall arguments out of
   *  parent super calls and constructor definitions.
   *  Hoisted superarg methods are collected in `superArgDefs`
   */
  class Hoister(cls: Symbol)(using Context) {
    val superArgDefs: mutable.ListBuffer[DefDef] = new mutable.ListBuffer

    /** If argument is complex, hoist it out into its own method and refer to the
     *  method instead.
     *  @param   arg    The argument that might be hoisted
     *  @param   cdef   The definition of the constructor from which the call is made
     *  @param   lifted Argument definitions that were lifted out in a call prefix
     *  @return  The argument after possible hoisting
     */
    private def hoistSuperArg(arg: Tree, cdef: DefDef, lifted: List[Symbol]): Tree = {
      val constr = cdef.symbol
      lazy val origParams = // The parameters that can be accessed in the supercall
        if (constr == cls.primaryConstructor)
          cls.info.decls.filter(d => d.is(TypeParam) || d.is(ParamAccessor) && !d.isSetter)
        else
          allParamSyms(cdef)

      /** The parameter references defined by the constructor info */
      def allParamRefs(tp: Type): List[ParamRef] = tp match {
        case tp: LambdaType => tp.paramRefs ++ allParamRefs(tp.resultType)
        case _              => Nil
      }

      /** Splice `restpe` in final result type position of `tp` */
      def replaceResult(tp: Type, restpe: Type): Type = tp match {
        case tp: LambdaType =>
          tp.derivedLambdaType(resType = replaceResult(tp.resultType, restpe))
        case _ => restpe
      }

      /** A method representing a hoisted supercall argument */
      def newSuperArgMethod(argType: Type) = {
        val (staticFlag, methOwner) =
          if (cls.owner.is(Package)) (JavaStatic, cls) else (EmptyFlags, cls.owner)
        val argTypeWrtConstr = argType.widenTermRefExpr.subst(origParams, allParamRefs(constr.info))
        // argType with references to paramRefs of the primary constructor instead of
        // local parameter accessors
        val abstractedArgType =
          if lifted.isEmpty then argTypeWrtConstr
          else MethodType.fromSymbols(lifted, argTypeWrtConstr)
        newSymbol(
          owner = methOwner,
          name = SuperArgName.fresh(cls.name.toTermName),
          flags = Synthetic | Private | Method | staticFlag,
          info = replaceResult(constr.info, abstractedArgType),
          coord = constr.coord
        ).enteredAfter(thisPhase)
      }

      /** Type of a reference implies that it needs to be hoisted */
      def refNeedsHoist(tp: Type): Boolean = tp match {
        case tp: ThisType => !tp.cls.isStaticOwner && !cls.isContainedIn(tp.cls)
        case tp: TermRef  => refNeedsHoist(tp.prefix)
        case _            => false
      }

      /** Super call argument is complex, needs to be hoisted */
      def needsHoist(tree: Tree) = tree match
        case _: DefDef            => true
        case _: Template          => true
        case _: New               => !tree.tpe.typeSymbol.isStatic
        case _: RefTree | _: This => refNeedsHoist(tree.tpe)
        case _                    => false

      /** Only rewire types that are owned by the current Hoister and is an param or accessor */
      def needsRewire(tp: Type)(using Context) = tp match {
        case ntp: NamedType =>
          val owner = ntp.symbol.maybeOwner
          (owner == cls || owner == constr) && ntp.symbol.isParamOrAccessor
          || lifted.contains(ntp.symbol)
        case _ => false
      }

      // begin hoistSuperArg
      arg match {
        case _ if arg.existsSubTree(needsHoist) =>
          val superMeth = newSuperArgMethod(arg.tpe)
          val superArgDef = DefDef(superMeth, prefss => {
            val paramSyms = prefss.flatten.map(pref =>
              if pref.isType then pref.tpe.typeSymbol else pref.symbol)
            val tmap = new TreeTypeMap(
              typeMap = new TypeMap {
                lazy val origToParam = (origParams ::: lifted).zip(paramSyms).toMap
                def apply(tp: Type) = tp match {
                  case tp: NamedType if needsRewire(tp) =>
                    origToParam.get(tp.symbol) match {
                      case Some(mappedSym) => if (tp.symbol.isType) mappedSym.typeRef else mappedSym.termRef
                      case None => mapOver(tp)
                    }
                  case _ =>
                    mapOver(tp)
                }
              },
              treeMap = {
                case tree: RefTree if needsRewire(tree.tpe) =>
                  cpy.Ident(tree)(tree.name).withType(tree.tpe)
                case tree =>
                  tree
              })
            tmap(arg).changeOwnerAfter(constr, superMeth, thisPhase)
          })
          superArgDefs += superArgDef
          def termParamRefs(tp: Type, params: List[Symbol]): List[List[Tree]] = tp match {
            case tp: PolyType =>
              termParamRefs(tp.resultType, params)
            case tp: MethodType =>
              val (thisParams, otherParams) = params.splitAt(tp.paramNames.length)
              thisParams.map(ref) :: termParamRefs(tp.resultType, otherParams)
            case _ =>
              Nil
          }
          val (typeParams, termParams) = origParams.span(_.isType)
          var res = ref(superMeth)
            .appliedToTypes(typeParams.map(_.typeRef))
            .appliedToArgss(termParamRefs(constr.info, termParams))
          if lifted.nonEmpty then
            res = res.appliedToArgs(lifted.map(ref))
          report.log(i"hoist $arg, cls = $cls = $res")
          res
        case _ => arg
      }
    }

    /** Hoist complex arguments in super call out of the class. */
    def hoistSuperArgsFromCall(superCall: Tree, cdef: DefDef, lifted: mutable.ListBuffer[Symbol]): Tree = superCall match
      case Block(defs, expr) if !expr.symbol.owner.is(Scala2x) =>
        // MO: The guard avoids the crash for #16351.
        // It would be good to dig deeper, but I won't have the time myself to do it.
        cpy.Block(superCall)(
          stats = defs.mapconserve {
            case vdef: ValDef =>
              try cpy.ValDef(vdef)(rhs = hoistSuperArg(vdef.rhs, cdef, lifted.toList))
              finally lifted += vdef.symbol
            case ddef: DefDef =>
              try cpy.DefDef(ddef)(rhs = hoistSuperArg(ddef.rhs, cdef, lifted.toList))
              finally lifted += ddef.symbol
            case stat =>
              stat
          },
          expr = hoistSuperArgsFromCall(expr, cdef, lifted))
      case Apply(fn, args) =>
        cpy.Apply(superCall)(
          hoistSuperArgsFromCall(fn, cdef, lifted),
          args.mapconserve(hoistSuperArg(_, cdef, lifted.toList)))
      case _ =>
        superCall

    /** Hoist complex arguments in this-constructor call of secondary constructor out of the class. */
    def hoistSuperArgsFromConstr(stat: Tree): Tree = stat match {
      case constr: DefDef if constr.symbol.isClassConstructor =>
        val lifted = new mutable.ListBuffer[Symbol]
        cpy.DefDef(constr)(rhs =
          constr.rhs match
            case Block(stats @ (superCall :: stats1), expr: Literal) =>
              cpy.Block(constr.rhs)(
                stats.derivedCons(hoistSuperArgsFromCall(superCall, constr, lifted), stats1),
                expr)
            case _ =>
              hoistSuperArgsFromCall(constr.rhs, constr, lifted)
          )
      case _ =>
        stat
    }
  }

  override def transformTypeDef(tdef: TypeDef)(using Context): Tree =
    tdef.rhs match {
      case impl @ Template(cdef, superCall :: others, _, _) =>
        val hoist = new Hoister(tdef.symbol)
        val hoistedSuperCall = hoist.hoistSuperArgsFromCall(superCall, cdef, new mutable.ListBuffer)
        val hoistedBody = impl.body.mapconserve(hoist.hoistSuperArgsFromConstr)
        if (hoist.superArgDefs.isEmpty) tdef
        else {
          val (staticSuperArgDefs, enclSuperArgDefs) =
            hoist.superArgDefs.toList.partition(_.symbol.is(JavaStatic))
          flatTree(
              cpy.TypeDef(tdef)(
                  rhs = cpy.Template(impl)(
                      parents = hoistedSuperCall :: others,
                      body = hoistedBody ++ staticSuperArgDefs)) ::
              enclSuperArgDefs)
        }
      case _ =>
        tdef
    }
}
