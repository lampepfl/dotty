package dotty.tools.dotc
package transform
package init

import core.*
import Contexts.*
import Types.*
import Symbols.*
import StdNames.*
import ast.tpd.*

import reporting.trace as log
import config.Printers.init as printer

import Trace.*

object Util:
  /** Utility definition used for better error-reporting of argument errors */
  case class TraceValue[T](value: T, trace: Trace)

  def typeRefOf(tp: Type)(using Context): TypeRef = tp.dealias.typeConstructor match
    case tref: TypeRef => tref
    case RefinedType(parent, _, _) => typeRefOf(parent)
    case hklambda: HKTypeLambda => typeRefOf(hklambda.resType)


  opaque type Arg  = Tree | ByNameArg
  case class ByNameArg(tree: Tree)

  object Arg:
    def apply(tree: Tree): Arg = tree

  extension (arg: Arg)
    def isByName = arg.isInstanceOf[ByNameArg]
    def tree: Tree = arg match
      case t: Tree      => t
      case ByNameArg(t) => t

  object Call:

    def unapply(tree: Tree)(using Context): Option[(Tree, List[List[Arg]])] =
      tree match
      case Apply(fn, args) =>
        val argTps = fn.tpe.widen match
          case mt: MethodType => mt.paramInfos
        val normArgs: List[Arg] = args.zip(argTps).map {
          case (arg, _: ExprType) => ByNameArg(arg)
          case (arg, _)           => arg
        }
        unapply(fn) match
        case Some((ref, args0)) => Some((ref, args0 :+ normArgs))
        case None => None

      case TypeApply(fn, targs) =>
        unapply(fn)

      case ref: RefTree if ref.tpe.widenSingleton.isInstanceOf[MethodicType] =>
        Some((ref, Nil))

      case _ => None

  object NewExpr:
    def unapply(tree: Tree)(using Context): Option[(TypeRef, New, Symbol, List[List[Arg]])] =
      tree match
      case Call(fn @ Select(newTree: New, init), argss) if init == nme.CONSTRUCTOR =>
        val tref = typeRefOf(newTree.tpe)
        Some((tref, newTree, fn.symbol, argss))
      case _ => None

  object PolyFun:
    def unapply(tree: Tree)(using Context): Option[DefDef] =
      tree match
      case Block((cdef: TypeDef) :: Nil, Typed(NewExpr(tref, _, _, _), _))
      if tref.symbol.isAnonymousClass && tref <:< defn.PolyFunctionType
      =>
        val body = cdef.rhs.asInstanceOf[Template].body
        val apply = body.head.asInstanceOf[DefDef]
        Some(apply)
      case _ =>
        None

  object TypeCast:
    def unapply(tree: Tree)(using Context): Option[(Tree, Type)] =
      tree match
        case TypeApply(Select(qual, _), typeArgs) if tree.symbol.isTypeCast =>
          Some(qual, typeArgs.head.tpe)
        case _ => None

  def resolve(cls: ClassSymbol, sym: Symbol)(using Context): Symbol = log("resove " + cls + ", " + sym, printer, (_: Symbol).show):
    if sym.isEffectivelyFinal then sym
    else sym.matchingMember(cls.appliedRef)

  extension (sym: Symbol)
    def hasSource(using Context): Boolean = !sym.defTree.isEmpty

    def isStaticObject(using Context) =
      sym.is(Flags.Module, butNot = Flags.Package) && sym.isStatic

  def isConcreteClass(cls: ClassSymbol)(using Context) =
    val instantiable: Boolean =
      cls.is(Flags.Module) ||
      !cls.isOneOf(Flags.AbstractOrTrait) && {
        // see `Checking.checkInstantiable` in typer
        val tp = cls.appliedRef
        val stp = SkolemType(tp)
        val selfType = cls.givenSelfType.asSeenFrom(stp, cls)
        !selfType.exists || stp <:< selfType
      }

    // A concrete class may not be instantiated if the self type is not satisfied
    instantiable && cls.enclosingPackageClass != defn.StdLibPatchesPackage.moduleClass

  /** Whether the class or its super class/trait contains any mutable fields? */
  def isMutable(cls: ClassSymbol)(using Context): Boolean =
    cls.classInfo.decls.exists(_.is(Flags.Mutable)) ||
    cls.parentSyms.exists(parentCls => isMutable(parentCls.asClass))
