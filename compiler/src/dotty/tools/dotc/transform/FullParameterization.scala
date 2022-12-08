package dotty.tools.dotc
package transform

import core._
import Types._
import Contexts._
import Symbols._
import Decorators._
import TypeUtils._
import StdNames.nme
import ast._

/** Provides methods to produce fully parameterized versions of instance methods,
 *  where the `this` of the enclosing class is abstracted out in an extra leading
 *  `$this` parameter and type parameters of the class become additional type
 *  parameters of the fully parameterized method.
 *
 *  Example usage scenarios are:
 *
 *    - extension methods of value classes
 *    - implementations of trait methods
 *    - static protected accessors
 *    - local methods produced by tailrec transform
 *
 *  Note that the methods lift out type parameters of the class containing
 *  the instance method, but not type parameters of enclosing classes. The
 *  fully instantiated method therefore needs to be put in a scope "close"
 *  to the original method, i.e. they need to share the same outer pointer.
 *  Examples of legal positions are: in the companion object, or as a local
 *  method inside the original method.
 *
 *  Note: The scheme does not handle yet methods where type parameter bounds
 *  depend on value parameters of the enclosing class, as in:
 *
 *      class C(val a: String) extends AnyVal {
 *        def foo[U <: a.type]: Unit = ...
 *      }
 *
 *  The expansion of method `foo` would lead to
 *
 *      def foo$extension[U <: $this.a.type]($this: C): Unit = ...
 *
 *  which is not typable. Not clear yet what to do. Maybe allow PolyTypes
 *  to follow method parameters and translate to the following:
 *
 *      def foo$extension($this: C)[U <: $this.a.type]: Unit = ...
 *
 *  @see class-dependent-extension-method.scala in pending/pos.
 */
trait FullParameterization {

  import tpd._

  /** If references to original symbol `referenced` from within fully parameterized method
   *  `derived` should be rewired to some fully parameterized method, the rewiring target symbol,
   *  otherwise NoSymbol.
   */
  protected def rewiredTarget(referenced: Symbol, derived: Symbol)(using Context): Symbol

  /** If references to some original symbol from given tree node within fully parameterized method
   *  `derived` should be rewired to some fully parameterized method, the rewiring target symbol,
   *  otherwise NoSymbol. By default implemented as
   *
   *      rewiredTarget(tree.symbol, derived)
   *
   *  but can be overridden.
   */
  protected def rewiredTarget(tree: Tree, derived: Symbol)(using Context): Symbol =
    rewiredTarget(tree.symbol, derived)

  /** Converts the type `info` of a member of class `clazz` to a method type that
   *  takes the `this` of the class and any type parameters of the class
   *  as additional parameters. Example:
   *
   *    class Foo[+A <: AnyRef](val xs: List[A]) extends AnyVal {
   *      def baz[B >: A](x: B): List[B] = ...
   *    }
   *
   *  leads to:
   *
   *    object Foo {
   *      def extension$baz[B >: A <: Any, A >: Nothing <: AnyRef]($this: Foo[A])(x: B): List[B]
   *    }
   *
   *  If a self type is present, $this has this self type as its type.
   *
   *  @param abstractOverClass  if true, include the type parameters of the class in the method's list of type parameters.
   *  @param liftThisType       if true, require created $this to be $this: (Foo[A] & Foo,this).
   *                            This is needed if created member stays inside scope of Foo(as in tailrec)
   */
  def fullyParameterizedType(info: Type, clazz: ClassSymbol, abstractOverClass: Boolean = true, liftThisType: Boolean = false)(using Context): Type = {
    val (mtparamCount, origResult) = info match {
      case info: PolyType => (info.paramNames.length, info.resultType)
      case info: ExprType => (0, info.resultType)
      case _ => (0, info)
    }
    val ctparams = if (abstractOverClass) clazz.typeParams else Nil
    val ctnames = ctparams.map(_.name)

    /** The method result type */
    def resultType(mapClassParams: Type => Type) = {
      val thisParamType = mapClassParams(clazz.classInfo.selfType)
      val firstArgType = if (liftThisType) thisParamType & clazz.thisType else thisParamType
      MethodType(nme.SELF :: Nil)(
          mt => firstArgType :: Nil,
          mt => mapClassParams(origResult).substThisUnlessStatic(clazz, mt.newParamRef(0)))
    }

    /** Replace class type parameters by the added type parameters of the polytype `pt` */
    def mapClassParams(tp: Type, pt: PolyType): Type = {
      val classParamsRange = (mtparamCount until mtparamCount + ctparams.length).toList
      tp.subst(ctparams, classParamsRange map (pt.paramRefs(_)))
    }

    /** The bounds for the added type parameters of the polytype `pt` */
    def mappedClassBounds(pt: PolyType): List[TypeBounds] =
      ctparams.map(tparam => mapClassParams(tparam.info, pt).bounds)

    info match {
      case info: PolyType =>
        PolyType(info.paramNames ++ ctnames)(
          pt =>
            (info.paramInfos.map(mapClassParams(_, pt).bounds) ++
             mappedClassBounds(pt)).mapConserve(_.subst(info, pt).bounds),
          pt => resultType(mapClassParams(_, pt)).subst(info, pt))
      case _ =>
        if (ctparams.isEmpty) resultType(identity)
        else PolyType(ctnames)(mappedClassBounds, pt => resultType(mapClassParams(_, pt)))
    }
  }

  /** The type parameters (skolems) of the method definition `originalDef`,
   *  followed by the class parameters of its enclosing class.
   */
  private def allInstanceTypeParams(originalDef: DefDef, abstractOverClass: Boolean)(using Context): List[Symbol] =
    if (abstractOverClass)
      originalDef.leadingTypeParams.map(_.symbol) ::: originalDef.symbol.enclosingClass.typeParams
    else
      originalDef.leadingTypeParams.map(_.symbol)

  /** Given an instance method definition `originalDef`, return a
   *  fully parameterized method definition derived from `originalDef`, which
   *  has `derived` as symbol and `fullyParameterizedType(originalDef.symbol.info)`
   *  as info.
   *  `abstractOverClass` defines weather the DefDef should abstract over type parameters
   *  of class that contained original defDef
   */
  def fullyParameterizedDef(derived: TermSymbol, originalDef: DefDef, abstractOverClass: Boolean = true)(using Context): Tree =
    DefDef(derived, prefss => {
      val (trefs, vrefss) = splitArgs(prefss)
      val origMeth = originalDef.symbol
      val origClass = origMeth.enclosingClass.asClass
      val origLeadingTypeParamSyms = allInstanceTypeParams(originalDef, abstractOverClass)
      val origOtherParamSyms = originalDef.trailingParamss.flatten.map(_.symbol)
      val thisRef :: argRefs = vrefss.flatten: @unchecked

      /** If tree should be rewired, the rewired tree, otherwise EmptyTree.
       *  @param   targs  Any type arguments passed to the rewired tree.
       */
      def rewireTree(tree: Tree, targs: List[Tree])(using Context): Tree = {
        def rewireCall(thisArg: Tree): Tree = {
          val rewired = rewiredTarget(tree, derived)
          if (rewired.exists) {
            val base = thisArg.tpe.baseType(origClass)
            assert(base.exists)
            ref(rewired.termRef)
              .appliedToTypeTrees(targs ++ base.argInfos.map(TypeTree(_)))
              .appliedTo(thisArg)
          } else EmptyTree
        }
        tree match {
          case Return(expr, from) if !from.isEmpty =>
            val rewired = rewiredTarget(from, derived)
            if (rewired.exists)
              tpd.cpy.Return(tree)(expr, Ident(rewired.termRef))
            else
              EmptyTree
          case Ident(_) => rewireCall(thisRef)
          case Select(qual, _) => rewireCall(qual)
          case tree @ TypeApply(fn, targs1) =>
            assert(targs.isEmpty)
            rewireTree(fn, targs1)
          case _ => EmptyTree
        }
      }

      /** Type rewiring is needed because a previous reference to an instance
       *  method might still persist in the types of enclosing nodes. Example:
       *
       *     if (true) this.imeth else this.imeth
       *
       *  is rewritten to
       *
       *      if (true) xmeth($this) else xmeth($this)
       *
       *  but the type `this.imeth` still persists as the result type of the `if`,
       *  because it is kept by the `cpy` operation of the tree transformer.
       *  It needs to be rewritten to the common result type of `imeth` and `xmeth`.
       */
      def rewireType(tpe: Type)(using Context) = tpe match {
        case tpe: TermRef if rewiredTarget(tpe.symbol, derived).exists => tpe.widen
        case _ => tpe
      }

      new TreeTypeMap(
        typeMap = rewireType(_)
          .subst(origLeadingTypeParamSyms ++ origOtherParamSyms, (trefs ++ argRefs).tpes)
          .substThisUnlessStatic(origClass, thisRef.tpe),
        treeMap = {
          case tree: This if tree.symbol == origClass => thisRef
          case tree => rewireTree(tree, Nil) orElse tree
        },
        oldOwners = origMeth :: Nil,
        newOwners = derived :: Nil
      ).transform(originalDef.rhs)
    })

  /** A forwarder expression which calls `derived`, passing along
   *  - if `abstractOverClass` the type parameters and enclosing class parameters of originalDef`,
   *  - the `this` of the enclosing class,
   *  - the value parameters of the original method `originalDef`.
   */
  def forwarder(derived: TermSymbol, originalDef: DefDef, abstractOverClass: Boolean = true, liftThisType: Boolean = false)(using Context): Tree = {
    val fun: Tree =
      ref(derived.termRef)
        .appliedToTypes(allInstanceTypeParams(originalDef, abstractOverClass).map(_.typeRef))
        .appliedTo(This(originalDef.symbol.enclosingClass.asClass))
    val fwd =
      if !liftThisType then
        fun.appliedToArgss(originalDef.trailingParamss.nestedMap(param => ref(param.symbol)))
      else
        // this type could have changed on forwarding. Need to insert a cast.
        originalDef.trailingParamss.foldLeft(fun)((acc, params) => {
        val meth = acc.tpe.asInstanceOf[MethodType]
        val paramTypes = meth.instantiateParamInfos(params.tpes)
        acc.appliedToArgs(
          params.lazyZip(paramTypes).map((param, paramType) => {
            assert(param.tpe <:< paramType.widen) // type should still conform to widened type
            ref(param.symbol).ensureConforms(paramType)
          }))
       })
    fwd.withSpan(originalDef.rhs.span)
  }
}

object FullParameterization {

  /** Assuming `info` is a result of a `fullyParameterizedType` call, the signature of the
   *  original method type `X` after stripping its leading type parameters section,
   *  such that:
   *    info.stripPoly.ensureMethodic = fullyParameterizedType(X, ...).stripPoly.ensureMethodic
   *
   *  NOTE: Keeping the polymorphic part of the signature would be more precise,
   *  but we cannot distinguish which type parameters of `info` are also type
   *  parameters of`X`. This could be fixed by using a specific NameKind for the
   *  extra type parameters, but that wouldn't help for extension methods
   *  unpickled from Scala 2 (because Scala 2 extmeths phase happens before
   *  pickling, which is maybe something we should change for 2.14).
   */
  def memberSignature(info: Type)(using Context): Signature = info match {
    case info: PolyType =>
      memberSignature(info.resultType)
    case MethodTpe(nme.SELF :: Nil, _, restpe) =>
      restpe.ensureMethodic.signature
    case info @ MethodTpe(nme.SELF :: otherNames, thisType :: otherTypes, restpe) =>
      info.derivedLambdaType(otherNames, otherTypes, restpe).signature
    case _ =>
      Signature.NotAMethod
  }
}
