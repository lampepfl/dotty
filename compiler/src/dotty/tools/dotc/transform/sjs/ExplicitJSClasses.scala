package dotty.tools
package dotc
package transform
package sjs

import scala.collection.mutable

import MegaPhase._
import core.Annotations._
import core.Constants._
import core.Denotations._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Phases._
import core.Types._
import core.Flags._
import core.Decorators._
import core.StdNames.nme
import core.SymDenotations.SymDenotation
import core.Names._
import core.NameKinds._
import core.NameOps._
import ast.Trees._
import SymUtils._
import dotty.tools.dotc.ast.tpd

import util.Store

import util.Lst; // import Lst.::
import util.Lst.{NIL, +:, toLst}

import dotty.tools.backend.sjs.JSDefinitions.jsdefn

import JSSymUtils._

/** This phase makes all JS classes explicit (their definitions and references to them).
 *
 *  This phase is the equivalent of the two phases `ExplicitInnerJS` and
 *  `ExplicitLocalJS` from Scala 2. It performs the following transformations:
 *
 *  (A) For every inner JS class `Inner` in a class or trait `Outer`, create a
 *      field `Outer.Inner\$jsclass` to hold the JS class value of `Inner`.
 *  (B) For every exposed object `Inner` in a static owner `Outer`, create an
 *      explicit exposed getter `Outer.Inner\$jsobject`.
 *  (C) For every local JS class `Local`, create a local val `Local\$jsclass`
 *      to hold the JS class value of `Local`.
 *  (D) Desugar calls like `x.isInstanceOf[C]` into
 *      `js.special.instanceof(x, js.constructorOf[C])` when `C` is a nested
 *      JS class.
 *  (E) Wrap every `new C` call and `super[C]` reference of a nested JS class
 *      `C` with `withContextualJSClassValue(js.constructorOf[C], ...)`.
 *  (F) Desugar calls to `js.constructorOf[C]` (including those generated by
 *      the previous transformations) into either `runtime.constructorOf` or
 *      access to the `\$jsclass` fields/vals.
 *  (G) Adjust the `NoInits` flag of traits:
 *      - for JS traits, always add the flag
 *      - for Scala trait that contain a JS class, remove the flag
 *
 *  Note that in this comment, and more largely in this phase, by "class" we
 *  mean *only* `class`es. `trait`s and `object`s are not implied.
 *
 *  --------------------------------------
 *
 *  (A) `Inner\$jsclass` fields
 *
 *  Roughly, for every inner JS class of the form:
 *  {{{
 *  class Outer {
 *    class Inner extends ParentJSClass
 *  }
 *  }}}
 *  this phase creates a field `Inner\$jsclass` in `Outer` to hold the JS class
 *  value for `Inner`. The rhs of that field is a call to a magic method, used
 *  to retain information that the back-end will need.
 *  {{{
 *  class Outer {
 *    <synthetic> val Inner\$jsclass: AnyRef =
 *      createJSClass(classOf[Inner], js.constructorOf[ParentJSClass])
 *
 *    class Inner extends ParentJSClass
 *  }
 *  }}}
 *
 *  These fields will be read by code generated in step (F).
 *
 *  A `\$jsclass` field is also generated for classes declared inside *static
 *  JS objects*. Indeed, even though those classes have a unique, globally
 *  accessible class value, that class value needs to be *exposed* as a field
 *  of the enclosing object. In those cases, the rhs of the field is a direct
 *  call to `js.constructorOf[Inner]`, which becomes
 *  `runtime.constructorOf(classOf[Inner])`.
 *
 *  For the following input:
 *  {{{
 *  object Outer extends js.Object {
 *    class InnerClass extends ParentJSClass
 *  }
 *  }}}
 *  this phase will generate
 *  {{{
 *  object Outer extends js.Object {
 *    @ExposedJSMember @JSName("InnerClass")
 *    val InnerClass\$jsclass: AnyRef = runtime.constructorOf(classOf[InnerClass])
 *  }
 *  }}}
 *
 *  The `\$jsclass` fields must also be added to outer classes and traits
 *  coming from separate compilation, therefore this phase is an
 *  `InfoTransform`.
 *
 *  --------------------------------------
 *
 *  (B) `Inner\$jsobject` exposed getters
 *
 *  For *modules* declared inside static JS objects, we generate an explicit
 *  exposed getter as well. For non-static objects, dotc already generates a
 *  getter with the `@ExposedJSMember` annotation, so we do not need to do
 *  anything. But for static objects, it doesn't, so we have to do it ourselves
 *  here.
 *
 *  For the following input:
 *  {{{
 *  object Outer extends js.Object {
 *    object InnerObject extends ParentJSClass
 *  }
 *  }}}
 *  this phase will generate
 *  {{{
 *  object Outer extends js.Object {
 *    @ExposedJSMember @JSName("InnerObject")
 *    def InnerObject\$jsobject: AnyRef = InnerObject
 *  }
 *  }}}
 *
 *  --------------------------------------
 *
 *  (C) `Local\$jsclass` vals and vars
 *
 *  Similarly to how step (A) creates explicit fields in the enclosing
 *  templates of inner JS classes and traits to hold the JS class values, this
 *  phase creates local vals for local JS classes in the enclosing statement
 *  list.
 *
 *  For every local JS class of the form:
 *  {{{
 *  def outer() = {
 *    class Local extends ParentJSClass
 *  }
 *  }}}
 *  this phase creates a local `val Local\$jslass` in the body of `outer()` to
 *  hold the JS class value for `Local`. The rhs of that val is a call to a
 *  magic method, used to retain information that the back-end will need:
 *
 *  - A reified reference to `class Local`, in the form of a `classOf`
 *  - An explicit reference to the super JS class value, i.e., the desugaring
 *    of `js.constructorOf[ParentJSClass]`
 *  - An array of fake `new` expressions for all overloaded constructors.
 *
 *  The latter will be augmented by `LambdaLift` with the appropriate actual
 *  parameters for the captures of `Local`, which will be needed by the
 *  back-end. In code, this looks like:
 *  {{{
 *  def outer() = {
 *    class Local extends ParentJSClass
 *    val Local\$jsclass: AnyRef = createLocalJSClass(
 *        classOf[Local],
 *        js.constructorOf[ParentJSClass],
 *        ???)
 *  }
 *  }}}
 *
 *  The third argument `???` is a placeholder, which will be filled in by
 *  `AddLocalJSFakeNews` with fake new invocations for the all the constructors
 *  of `Local`. We cannot do it at this phase because that would require
 *  inventing sound type arguments for the type parameters of `Local` out of
 *  thin air.
 *
 *  If the body of `Local` references itself, then the `val Local\$jsclass` is
 *  instead declared as a `var` to work around the cyclic dependency:
 *  {{{
 *  def outer() = {
 *    var Local\$jsclass: AnyRef = null
 *    class Local extends ParentJSClass {
 *      def newLocal = new Local // self-reference
 *    }
 *    Local\$jsclass = createLocalJSClass(...)
 *  }
 *  }}}
 *
 *  --------------------------------------
 *
 *  (D) Insertion of `withContextualJSClassValue` calls
 *
 *  For any nested JS class `C`, this phase performs the following
 *  transformations:
 *
 *  - `new C[...Ts](...args)` desugars into
 *    `withContextualJSClassValue(js.constructorOf[C], new C[...Ts](...args))`,
 *    so that the back-end receives a reified reference to the JS class value.
 *  - In the same spirit, for `D extends C`, `D.super[C].m[...Ts](...args)`
 *    desugars into
 *    `withContextualJSClassValue(js.constructorOf[C], D.super[C].m[...Ts](...args))`.
 *
 *  For any nested JS *object*, their (only) instantiation point of the form
 *  `new O$()` is rewritten as
 *  `withContextualJSClassValue(js.constructorOf[ParentClassOfO], new O$())`,
 *  so that the back-end receives a reified reference to the parent class of
 *  `O`.
 *
 *  A similar treatment is applied on anonymous JS classes, which basically
 *  define something very similar to an `object`, although without their own JS
 *  class.
 *
 *  --------------------------------------
 *
 *  (E) Desugar `x.isInstanceOf[C]` for nested JS classes
 *
 *  They are desugared into `js.special.instanceof(x, js.constructorOf[C])`.
 *
 *  --------------------------------------
 *
 *  (F) Desugar `js.constructorOf[C]`
 *
 *  Finally, this phase rewrites all calls to `js.constructorOf[C]`, including
 *  the ones generated by the previous steps. The transformation depends on the
 *  nature of `C`:
 *
 *  - If `C` is a statically accessible class, desugar to
 *    `runtime.constructorOf(classOf[C])` so that the reified symbol survives
 *    erasure and reaches the back-end.
 *  - If `C` is an inner JS class, it must be of the form `path.D` for some
 *    pair (`path`, `D`), and we desugar it to `path.D\$jsclass`, using the
 *    field created by step (A) (it is an error if `C` is of the form
 *    `Enclosing#D`).
 *  - If `C` is a local JS class, desugar to `C\$jsclass`, using the local val
 *    created by step (C).
 */
class ExplicitJSClasses extends MiniPhase with InfoTransformer { thisPhase =>
  import ExplicitJSClasses._
  import ast.tpd._

  override def phaseName: String = ExplicitJSClasses.name

  private var MyState: Store.Location[MyState] = _
  private def myState(using Context) = ctx.store(MyState)

  override def initContext(ctx: FreshContext): Unit =
    MyState = ctx.addLocation[MyState]()

  override def isEnabled(using Context): Boolean =
    ctx.settings.scalajs.value

  override def runsAfter: Set[String] = Set(PatternMatcher.name, HoistSuperArgs.name)

  override def changesMembers: Boolean = true // the phase adds fields for inner JS classes

  /** Is the given symbol an owner that might receive `\$jsclass` and/or `\$jsobject` fields?
   *
   *  This applies if either or both of the following are true:
   *  - It is not a static owner, or
   *  - It is a non-native JS object.
   *
   *  The latter is necessary for scala-js/scala-js#4086.
   */
  private def mayNeedJSClassOrJSObjectFields(sym: Symbol)(using Context): Boolean = {
    !sym.isStaticOwner
      || (sym.is(ModuleClass) && sym.hasAnnotation(jsdefn.JSTypeAnnot) && !sym.hasAnnotation(jsdefn.JSNativeAnnot))
  }

  /** Is the given symbol a JS class (that is not a trait nor an object)? */
  private def isJSClass(sym: Symbol)(using Context): Boolean = {
    sym.isClass &&
    !sym.isOneOf(Trait | Module) &&
    sym.hasAnnotation(jsdefn.JSTypeAnnot)
  }

  /** Is the given symbol a Module that should be exposed? */
  private def isExposedModule(sym: Symbol)(using Context): Boolean =
    sym.is(Module) && sym.hasAnnotation(jsdefn.ExposedJSMemberAnnot)

  /** Is the gen clazz an inner or local JS class? */
  private def isInnerOrLocalJSClass(sym: Symbol)(using Context): Boolean =
    isInnerJSClass(sym) || isLocalJSClass(sym)

  /** Is the given clazz an inner JS class? */
  private def isInnerJSClass(clazz: Symbol)(using Context): Boolean =
    isInnerJSClassOrObject(clazz) && !isConsideredAnObject(clazz)

  /** Is the given clazz a local JS class? */
  private def isLocalJSClass(clazz: Symbol)(using Context): Boolean =
    isLocalJSClassOrObject(clazz) && !isConsideredAnObject(clazz)

  /** Is the gen clazz an inner or local JS class or object? */
  private def isInnerOrLocalJSClassOrObject(sym: Symbol)(using Context): Boolean =
    isInnerJSClassOrObject(sym) || isLocalJSClassOrObject(sym)

  /** Is the given clazz an inner JS class or object? */
  private def isInnerJSClassOrObject(clazz: Symbol)(using Context): Boolean = {
    clazz.hasAnnotation(jsdefn.JSTypeAnnot)
      && !clazz.isOneOf(PackageClass | Trait)
      && !clazz.isStatic
      && !clazz.isLocalToBlock
  }

  /** Is the given clazz a local JS class or object? */
  private def isLocalJSClassOrObject(clazz: Symbol)(using Context): Boolean =
    clazz.isLocalToBlock && !clazz.is(Trait) && clazz.hasAnnotation(jsdefn.JSTypeAnnot)

  /** Is the given clazz an inner or local JS object? */
  private def isInnerOrLocalJSObject(clazz: Symbol)(using Context): Boolean =
    isInnerOrLocalJSClassOrObject(clazz) && isConsideredAnObject(clazz)

  /** Is the given clazz considered to be an object for the purposes of this phase?
   *  This is true for module classes and for anonymous JS classes.
   */
  private def isConsideredAnObject(clazz: Symbol)(using Context): Boolean =
    clazz.is(ModuleClass) || clazz.isAnonymousClass

  private def jsclassFieldName(clazzName: TypeName): TermName =
    clazzName.toTermName ++ "$jsname"

  private def jsclassAccessorFor(clazz: Symbol)(using Context): TermSymbol =
    clazz.owner.info.decls.lookup(jsclassFieldName(clazz.name.asTypeName)).asTerm

  private def jsobjectGetterName(moduleName: TermName): TermName =
    moduleName ++ "$jsobject"

  private def jsobjectGetterNameFor(moduleSym: Symbol)(using Context): TermName =
    jsobjectGetterName(moduleSym.name.asTermName)

  private def makeJSNameAnnotation(argument: String)(using Context): Annotation = {
    val annotClass = jsdefn.JSNameAnnot
    val stringCtor = annotClass.info.decl(nme.CONSTRUCTOR).suchThat { ctor =>
      ctor.info match {
        case mt: MethodType => mt.paramInfos.nonEmpty && mt.paramInfos.head.derivesFrom(defn.StringClass)
        case _              => false
      }
    }.symbol.asTerm
    Annotation(New(annotClass.typeRef, stringCtor, Lst(Literal(Constant(argument)))))
  }

  override def transformInfo(tp: Type, sym: Symbol)(using Context): Type = tp match {
    case tp @ ClassInfo(_, cls, _, decls, _) if !cls.is(JavaDefined) && mayNeedJSClassOrJSObjectFields(cls) =>
      val innerJSClasses = decls.filter(isJSClass)

      val innerObjectsForAdHocExposed =
        if (!cls.isStaticOwner) Nil // those already have a module accessor
        else decls.filter(isExposedModule)

      if (innerJSClasses.isEmpty && innerObjectsForAdHocExposed.isEmpty) {
        tp
      } else {
        def addAnnots(sym: Symbol, symForName: Symbol): Unit = {
          val jsNameAnnot = symForName.getAnnotation(jsdefn.JSNameAnnot).getOrElse {
            makeJSNameAnnotation(symForName.defaultJSName)
          }
          sym.addAnnotation(jsNameAnnot)
          sym.addAnnotation(jsdefn.ExposedJSMemberAnnot)
        }

        val clsIsJSClass = cls.hasAnnotation(jsdefn.JSTypeAnnot)

        val decls1 = decls.cloneScope

        for (innerJSClass <- innerJSClasses) {
          def addAnnotsIfInJSClass(sym: Symbol): Unit = {
            if (clsIsJSClass)
              addAnnots(sym, innerJSClass)
          }

          val fieldName = jsclassFieldName(innerJSClass.name.asTypeName)
          val fieldFlags = Synthetic | Artifact
          val field = newSymbol(cls, fieldName, fieldFlags, defn.AnyRefType, coord = innerJSClass.coord)
          addAnnotsIfInJSClass(field)
          decls1.enter(field)
        }

        // scala-js/scala-js#4086 Create exposed getters for exposed objects in static JS objects
        for (innerObject <- innerObjectsForAdHocExposed) {
          assert(clsIsJSClass && cls.is(ModuleClass) && cls.isStatic,
              i"trying to ad-hoc expose objects in non-JS static object ${cls.fullName}")

          val getterName = jsobjectGetterNameFor(innerObject)
          val getterFlags = Method | Synthetic | Artifact
          val getter = newSymbol(cls, getterName, getterFlags, ExprType(defn.AnyRefType), coord = innerObject.coord)
          addAnnots(getter, innerObject)
          decls1.enter(getter)
        }

        tp.derivedClassInfo(decls = decls1)
      }

    case _ =>
      tp
  }

  /** Adjust the `NoInits` flag of Scala traits containing a JS class and of JS traits. */
  override def transform(ref: SingleDenotation)(using Context): SingleDenotation = {
    super.transform(ref) match {
      case ref1: SymDenotation if ref1.is(Trait, butNot = JavaDefined) =>
        val isJSType = ref1.hasAnnotation(jsdefn.JSTypeAnnot)
        if (ref1.is(NoInits)) {
          // If one of the decls is a JS class, there is now some initialization code to create the JS class
          if (!isJSType && ref1.info.decls.exists(isJSClass))
            ref1.copySymDenotation(initFlags = ref1.flags &~ NoInits)
          else
            ref1
        } else {
          // JS traits never have an initializer, no matter what dotc thinks
          if (isJSType)
            ref1.copySymDenotation(initFlags = ref1.flags | NoInits)
          else
            ref1
        }
      case ref1 =>
        ref1
    }
  }

  override def infoMayChange(sym: Symbol)(using Context): Boolean =
    sym.isClass && !sym.is(JavaDefined)

  override def prepareForUnit(tree: Tree)(using Context): Context =
    ctx.fresh.updateStore(MyState, new MyState())

  /** Populate `nestedObject2superTypeConstructor` for inner objects at the start of
   *  a `Block` or `Template`, so that they are visible even before their
   *  definition (in their enclosing scope).
   */
  private def populateNestedObject2superClassTpe(stats: List[Tree])(using Context): Unit = {
    for (stat <- stats) {
      stat match {
        case cd @ TypeDef(_, rhs) if cd.isClassDef && isInnerOrLocalJSObject(cd.symbol) =>
          myState.nestedObject2superTypeConstructor(cd.symbol) = extractSuperTypeConstructor(rhs)
        case _ =>
      }
    }
  }

  override def prepareForBlock(tree: Block)(using Context): Context = {
    populateNestedObject2superClassTpe(tree.stats.toList)
    ctx
  }

  override def prepareForTemplate(tree: Template)(using Context): Context = {
    populateNestedObject2superClassTpe(tree.body.toList)
    ctx
  }

  // This method implements steps (A) and (B)
  override def transformTemplate(tree: Template)(using Context): Tree = {
    val cls = ctx.owner.asClass

    /* The `parents` of a Template have the same trees as `new` invocations
     * of the parent classes and traits. That means that `transformApply` may
     * have wrapped them in a `withContextualJSClassValue`, not knowing where
     * they belong in the larger tree.
     * We now unwrap those, canceling out that effect.
     * TODO Is there a better way to do this?
     */
    val fixedParents =
      if (!cls.isJSType) tree.parents // fast path
      else tree.parents.mapConserve(unwrapWithContextualJSClassValue(_))

    if (!mayNeedJSClassOrJSObjectFields(cls)) {
      if (fixedParents eq tree.parents) tree
      else cpy.Template(tree)(parents = fixedParents)
    } else {
      val newStats = List.newBuilder[Tree]
      for (stat <- tree.body) {
        stat match {
          case stat: TypeDef if stat.isClassDef && isJSClass(stat.symbol) =>
            val innerClassSym = stat.symbol.asClass
            val jsclassAccessor = jsclassAccessorFor(innerClassSym)

            val rhs = if (cls.hasAnnotation(jsdefn.JSNativeAnnot)) {
              ref(jsdefn.JSPackage_native)
            } else {
              val clazzValue = clsOf(innerClassSym.typeRef)
              if (cls.isStaticOwner) {
                // scala-js/scala-js#4086
                ref(jsdefn.Runtime_constructorOf).appliedTo(clazzValue)
              } else {
                val parentTpe = extractSuperTypeConstructor(stat.rhs)
                val superClassCtor = genJSConstructorOf(tree, parentTpe)
                ref(jsdefn.Runtime_createInnerJSClass).appliedTo(clazzValue, superClassCtor)
              }
            }

            newStats += ValDef(jsclassAccessor, rhs)

          case stat: ValDef if cls.isStaticOwner && isExposedModule(stat.symbol) =>
            // scala-js/scala-js#4086
            val moduleSym = stat.symbol
            val getter = cls.info.decls.lookup(jsobjectGetterNameFor(moduleSym)).asTerm
            newStats += DefDef(getter, ref(moduleSym))

          case _ =>
            () // nothing to do
        }

        newStats += stat
      }

      cpy.Template(tree)(tree.constr, fixedParents, Nil, tree.self, newStats.result().toLst)
    }
  }

  // This method, together with transformTypeDef, implements step (C)
  override def prepareForTypeDef(tree: TypeDef)(using Context): Context = {
    val sym = tree.symbol
    if (sym.isClass && isLocalJSClass(sym)) {
      val jsclassValName = LocalJSClassValueName.fresh(sym.name.toTermName)
      val jsclassVal = newSymbol(ctx.owner, jsclassValName, EmptyFlags, defn.AnyRefType, coord = tree.span)
      myState.localClass2jsclassVal(sym) = jsclassVal
      myState.notYetReferencedLocalClasses += sym
    }
    ctx
  }

  // This method, together with prepareForTypeDef, implements step (C)
  override def transformTypeDef(tree: TypeDef)(using Context): Tree = {
    val sym = tree.symbol
    if (sym.isClass && isLocalJSClass(sym)) {
      val cls = sym.asClass

      val rhs = {
        val typeRef = tree.tpe
        val clazzValue = clsOf(typeRef)
        val superClassCtor = genJSConstructorOf(tree, extractSuperTypeConstructor(tree.rhs))
        ref(jsdefn.Runtime_createLocalJSClass).appliedTo(clazzValue, superClassCtor, ref(defn.Predef_undefined))
      }

      val jsclassVal = myState.localClass2jsclassVal(sym)
      if (myState.notYetReferencedLocalClasses.remove(cls)) {
        Thicket(Lst(tree, ValDef(jsclassVal, rhs)))
      } else {
        /* We are using `jsclassVal` inside the definition of the class.
         * We need to declare it as var before and initialize it after the class definition.
         */
        jsclassVal.setFlag(Mutable)
        Thicket(Lst(
            ValDef(jsclassVal, Literal(Constant(null))),
            tree,
            Assign(ref(jsclassVal), rhs)
        ))
      }
    } else {
      tree
    }
  }

  // This method, together with transformTypeApply and transformSelect, implements step (E)
  override def transformApply(tree: Apply)(using Context): Tree = {
    if (!isFullyApplied(tree)) {
      tree
    } else {
      val sym = tree.symbol

      if (sym.isConstructor) {
        /* Wrap `new`s to inner and local JS classes and objects with
         * `withContextualJSClassValue`, to preserve a reified reference to
         * the necessary JS class value (the class itself for classes, or the
         * super class for objects).
         */
        val cls = sym.owner
        if (isInnerOrLocalJSClassOrObject(cls)) {
          if (!isConsideredAnObject(cls)) {
            methPart(tree) match {
              case Select(n @ New(tpt), _) =>
                val jsclassValue = genJSConstructorOf(tpt, n.tpe)
                wrapWithContextualJSClassValue(jsclassValue)(tree)
              case _ =>
                // Super constructor call or this()-constructor call
                tree
            }
          } else {
            wrapWithContextualJSClassValue(myState.nestedObject2superTypeConstructor(cls))(tree)
          }
        } else {
          tree
        }
      } else {
        maybeWrapSuperCallWithContextualJSClassValue(tree)
      }
    }
  }

  // This method, together with transformApply and transformSelect, implements step (E)
  // It also implements step (D) and (F)
  override def transformTypeApply(tree: TypeApply)(using Context): Tree = {
    if (!isFullyApplied(tree)) {
      tree
    } else {
      val sym = tree.symbol

      def isTypeTreeForInnerOrLocalJSClass(tpeArg: Tree): Boolean = {
        val tpeSym = tpeArg.tpe.typeSymbol
        tpeSym.exists && isInnerOrLocalJSClass(tpeSym)
      }

      tree match {
        // Desugar js.constructorOf[T]
        case TypeApply(fun, Lst(tpt)) if sym == jsdefn.JSPackage_constructorOf =>
          genJSConstructorOf(tree, tpt.tpe).cast(jsdefn.JSDynamicType)

        // Translate x.isInstanceOf[T] for inner and local JS classes
        case TypeApply(fun @ Select(obj, _), Lst(tpeArg))
            if sym == defn.Any_isInstanceOf && isTypeTreeForInnerOrLocalJSClass(tpeArg) =>
          val jsCtorOf = genJSConstructorOf(tree, tpeArg.tpe)
          ref(jsdefn.Special_instanceof).appliedTo(obj, jsCtorOf)

        case _ =>
          maybeWrapSuperCallWithContextualJSClassValue(tree)
      }
    }
  }

  // This method, together with transformApply and transformTypeApply, implements step (E)
  override def transformSelect(tree: Select)(using Context): Tree = {
    if (!isFullyApplied(tree)) {
      tree
    } else {
      maybeWrapSuperCallWithContextualJSClassValue(tree)
    }
  }

  /** Tests whether this tree is fully applied, i.e., it does not need any
   *  additional `TypeApply` or `Apply` to lead to a value.
   *
   *  In this phase, `transformApply`, `transformTypeApply` and `transformSelect`
   *  must only operate on fully applied selections and applications.
   */
  private def isFullyApplied(tree: Tree)(using Context): Boolean =
    !tree.tpe.widenTermRefExpr.isInstanceOf[MethodOrPoly]

  /** Wraps `super` calls to inner and local JS classes with
   *  `withContextualJSClassValue`, to preserve a reified reference to the
   *  necessary JS class value (that of the super class).
   */
  private def maybeWrapSuperCallWithContextualJSClassValue(tree: Tree)(using Context): Tree = {
    methPart(tree) match {
      case Select(sup: Super, _) if isInnerOrLocalJSClass(sup.symbol.asClass.superClass) =>
        wrapWithContextualJSClassValue(sup.symbol.asClass.superClass.typeRef)(tree)
      case _ =>
        tree
    }
  }

  /** Generates the desugared version of `js.constructorOf[tpe]`.
   *
   *  This is the meat of step (F).
   */
  private def genJSConstructorOf(tree: Tree, tpe0: Type)(using Context): Tree = {
    val tpe = tpe0.underlyingClassRef(refinementOK = false) match {
      case typeRef: TypeRef => typeRef
      case _ =>
        // This should not have passed the checks in PrepJSInterop
        report.error(i"class type required but found $tpe0", tree)
        jsdefn.JSObjectType
    }
    val cls = tpe.typeSymbol

    // This should not have passed the checks in PrepJSInterop
    assert(!cls.isOneOf(Trait | ModuleClass),
        i"non-trait class type required but $tpe found for genJSConstructorOf at ${tree.sourcePos}")

    if (isInnerJSClass(cls)) {
      // Use the $jsclass field in the outer instance
      val prefix: Type = tpe.prefix
      if (prefix.isStable) {
        val jsclassAccessor = jsclassAccessorFor(cls)
        ref(NamedType(prefix, jsclassAccessor.name, jsclassAccessor.denot))
      } else {
        report.error(i"stable reference to a JS class required but $tpe found", tree)
        ref(defn.Predef_undefined)
      }
    } else if (isLocalJSClass(cls)) {
      // Use the local `val` that stores the JS class value
      val state = myState
      val jsclassVal = state.localClass2jsclassVal(cls)
      state.notYetReferencedLocalClasses -= cls
      ref(jsclassVal)
    } else {
      // Defer translation to `LoadJSConstructor` to the back-end
      ref(jsdefn.Runtime_constructorOf).appliedTo(clsOf(tpe))
    }
  }

  private def wrapWithContextualJSClassValue(jsClassType: Type)(tree: Tree)(using Context): Tree =
    wrapWithContextualJSClassValue(genJSConstructorOf(tree, jsClassType))(tree)

  private def wrapWithContextualJSClassValue(jsClassValue: Tree)(tree: Tree)(using Context): Tree =
    ref(jsdefn.Runtime_withContextualJSClassValue).appliedToType(tree.tpe).appliedTo(jsClassValue, tree)

  private def unwrapWithContextualJSClassValue(tree: Tree)(using Context): Tree = tree match {
    case Apply(fun, Lst(jsClassValue, actualTree))
        if fun.symbol == jsdefn.Runtime_withContextualJSClassValue =>
      actualTree
    case _ =>
      tree
  }

  /** Extracts the super type constructor of a `Template`, without type
   *  parameters, so that the type is well-formed outside of the `Template`,
   *  i.e., at the same level where the corresponding `TypeDef` is defined.
   *
   *  For example, for the Template of a class definition like
   *  {{{
   *  class Foo[...Ts] extends pre.Parent[...Us](...args) with ... { ... }
   *  }}}
   *  we extract the type constructor `pre.Parent`, without its type
   *  parameters.
   *
   *  Since the result is not necessarily *-kinded, its applicability is
   *  limited. It seems to be sufficient to put in a `classOf`, though, which
   *  is what we care about.
   */
  private def extractSuperTypeConstructor(typeDefRhs: Tree)(using Context): Type =
    typeDefRhs.asInstanceOf[Template].parents.head.tpe.dealias.typeConstructor
}

object ExplicitJSClasses {
  val name: String = "explicitJSClasses"

  val LocalJSClassValueName: UniqueNameKind = new UniqueNameKind("$jsclass")

  private final class MyState {
    val nestedObject2superTypeConstructor = new MutableSymbolMap[Type]
    val localClass2jsclassVal = new MutableSymbolMap[TermSymbol]
    val notYetReferencedLocalClasses = new util.HashSet[Symbol]
  }
}
