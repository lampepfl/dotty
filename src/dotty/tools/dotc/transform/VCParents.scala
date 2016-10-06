package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Flags._, Trees._, Types._, StdNames._, Symbols._
import Constants.Constant
import Denotations._, SymDenotations._
import DenotTransformers._, TreeTransforms._, Phases.Phase
import ValueClasses._

/** This phase makes value classes extend VCXPrototype and make their companions extend VCXCompanion.
 *
 *  (For a value class class V whose erased underlying type is U, X is "U" if U is a primitive
 *  type and is "Object" otherwise).
 *
 *  Furthermore, this phase also make VCPrototype extend AnyVal instead of AnyRef to preserve the
 *  invariant that value classes should always extend AnyVal.
 */
class VCParents extends MiniPhaseTransform with DenotTransformer {
  import tpd._

  override def phaseName: String = "vcParents"

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation =
    ref match {
      case moduleClass: ClassDenotation if moduleClass.is(ModuleClass) =>
        moduleClass.linkedClass match {
          case valueClass: ClassSymbol if isDerivedValueClass(valueClass) =>
            val moduleSym = moduleClass.symbol.asClass
            val cinfo = moduleClass.classInfo
            val decls1 = cinfo.decls.cloneScope

            val underlying = deepUnderlyingOfValueClass(valueClass)
            // TODO: what should we do if these symbols already exist (box and runtimeClassSym)?
            val boxParamTpe = if (underlying.classSymbol.isPrimitiveValueClass) underlying else defn.ObjectType
            val boxSymTpe = if (underlying.classSymbol.isPrimitiveValueClass) valueClass.typeRef else defn.ObjectType
            val boxSym = ctx.newSymbol(moduleClass.symbol, nme.box,
              Synthetic | Override | Method, MethodType(List(nme.x_0), List(boxParamTpe), boxSymTpe))
            val runtimeClassSym = ctx.newSymbol(moduleClass.symbol, nme.runtimeClass,
              Synthetic | Override | Method, MethodType(Nil, defn.ClassClass.typeRef))
            decls1.enter(boxSym)
            decls1.enter(runtimeClassSym)

            val superType = tpd.ref(defn.vcDeepCompanionOf(valueClass))
              .select(nme.CONSTRUCTOR)
              .appliedToType(valueClass.typeRef)
              .tpe
              .resultType

            moduleClass.copySymDenotation(info =
              cinfo.derivedClassInfo(decls = decls1, classParents =
                ctx.normalizeToClassRefs(List(superType), moduleSym, decls1)))
          case _ =>
            moduleClass
        }
      case valueClass: ClassDenotation if isDerivedValueClass(valueClass) =>
        val cinfo = valueClass.classInfo
        val superType = defn.vcDeepPrototypeOf(valueClass).typeRef

        val (p :: ps) = cinfo.classParents
        //TODO: remove assert to fix issue with i705-inner-value-class.scala
        assert(p.isRef(defn.AnyValClass))
        val parents = superType :: ps
        valueClass.copySymDenotation(info = cinfo.derivedClassInfo(classParents = parents))
      // This rewiring is required for cases:
      // case class Meter(x: Int) extends AnyVal
      // val x: AnyVal = Meter(3) //-Ycheck:all
      case proto: ClassDenotation if proto.symbol eq defn.VCPrototypeClass =>
        // After this phase, value classes extend VCXPrototype which extends VCPrototype,
        // so we make VCPrototype extend AnyVal to preserve existing subtyping relations.
        // We could make VCPrototype extend AnyVal earlier than this phase, but then we
        // would need to be careful to not treat it like a real value class.
        val cinfo = proto.classInfo
        val (p :: ps) = cinfo.classParents
        assert(p.isRef(defn.ObjectClass))
        proto.copySymDenotation(info =
          cinfo.derivedClassInfo(classParents = defn.AnyValClass.typeRef :: ps))
      case proto: ClassDenotation if defn.vcPrototypeValues.contains(proto.symbol) =>
        // We need to copy the ClassDenotations of the VCXPrototype classes to reset
        // their cache of base classes, the cache is no longer valid because these
        // classes extend VCPrototype and we changed the superclass of VCPrototype
        // in this phase.
        proto.copySymDenotation(info = proto.info)
      case _ =>
        ref
    }

  private def boxDefDef(vc: ClassDenotation)(implicit ctx: Context) = {
    val sym = vc.linkedClass.info.decl(nme.box).symbol.asTerm
    DefDef(sym, ref(defn.ScalaPredefModule).select(nme.???))
  }

  private def runtimeClassDefDef(vc: ClassDenotation)(implicit ctx: Context) = {
    val vctp = vc.typeRef
    val sym = vc.linkedClass.info.decl(nme.runtimeClass).symbol.asTerm
    DefDef(sym, { _ =>
      Literal(Constant(TypeErasure.erasure(vctp)))
    })
  }

  override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    ctx.owner.denot match {
      case moduleClass: ClassDenotation if moduleClass.is(ModuleClass) =>
        moduleClass.linkedClass match {
          case valueClass: ClassSymbol if isDerivedValueClass(valueClass) =>
            val rcDef = runtimeClassDefDef(valueClass)
            val boxDef = boxDefDef(valueClass)

            val superCall = New(defn.vcDeepCompanionOf(valueClass).typeRef)
              .select(nme.CONSTRUCTOR)
              .appliedToType(valueClass.typeRef)
              .appliedToNone

            val (p :: ps) = tree.parents
            // TODO: We shouldn't disallow extending companion objects of value classes
            // with classes other than AnyRef
            assert(p.tpe.isRef(defn.ObjectClass))
            cpy.Template(tree)(parents = superCall :: ps, body = boxDef :: rcDef :: tree.body)
          case _ =>
            tree
        }
      case valueClass: ClassDenotation if isDerivedValueClass(valueClass) =>
        val prototype = defn.vcDeepPrototypeOf(valueClass).typeRef
        val underlyingSym = valueClassUnbox(valueClass)

        def deepUndExpr(valueClass: Symbol): List[Symbol] = {
          val vcMethod = valueClassUnbox(valueClass.asClass)
          valueClass match {
            case _ if isDerivedValueClass(vcMethod.info.resultType.classSymbol) =>
              vcMethod :: deepUndExpr(vcMethod.info.resultType.classSymbol)
            case _ => List(vcMethod)
          }
        }
        val deepUnderlyingSyms = deepUndExpr(valueClass.symbol)
        //TODO: add null checkings
        val deepExpr = deepUnderlyingSyms.tail.foldLeft(ref(deepUnderlyingSyms.head))(_.select(_))
        val superCallExpr = if (deepUnderlyingSyms.last.info.classSymbol.isPrimitiveValueClass) deepExpr
        else deepExpr.ensureConforms(defn.ObjectType) //ensureConforms is required in case of Any is underlying type (-Ycheck)
        val superCall = New(prototype, List(superCallExpr))
        // TODO: manually do parameter forwarding: the prototype has a local field
        // so we don't need a field inside the value class

        val (p :: ps) = tree.parents
        assert(p.tpe.isRef(defn.AnyValClass))
        cpy.Template(tree)(parents = superCall :: ps)
      case _ =>
        tree
    }

  private var scala2ClassTagModule: Symbol = null
  private var scala2ClassTagApplyMethod: Symbol = null

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    scala2ClassTagModule = ctx.requiredModule("scala.reflect.ClassTag")
    scala2ClassTagApplyMethod = scala2ClassTagModule.requiredMethod(nme.apply)
    this
  }

  //change class tag for derived value class, companion is class tag
  override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    tree match {
      case Apply(TypeApply(_, tptr :: _), _) if (tree.fun.symbol eq scala2ClassTagApplyMethod) => {
        val claz = tptr.tpe.classSymbol
        if (ValueClasses.isDerivedValueClass (claz) )
          //TODO: add ensureConforms to fix -Ycheck:all for array of instances of case class X[T](val x: T) extends AnyVal
          ref(claz.companionModule).ensureConforms(tree.tpe) else tree
        }
      case _ => tree
    }
  }
}