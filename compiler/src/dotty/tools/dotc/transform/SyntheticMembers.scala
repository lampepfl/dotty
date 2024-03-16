package dotty.tools.dotc
package transform

import core.*
import Symbols.*, Types.*, Contexts.*, Names.*, StdNames.*, Constants.*
import Flags.*
import DenotTransformers.*
import Decorators.*
import NameOps.*
import Annotations.Annotation
import typer.ProtoTypes.constrained
import ast.untpd

import util.Property
import util.Spans.Span
import config.Printers.derive
import NullOpsDecorator.*

object SyntheticMembers {

  enum MirrorImpl:
    case OfProduct(pre: Type)
    case OfSum(childPres: List[Type])

  /** Attachment marking an anonymous class as a singleton case that will extend from Mirror.Singleton */
  val ExtendsSingletonMirror: Property.StickyKey[Unit] = new Property.StickyKey

  /** Attachment recording that an anonymous class should extend Mirror.Product */
  val ExtendsSumOrProductMirror: Property.StickyKey[MirrorImpl] = new Property.StickyKey
}

/** Synthetic method implementations for case classes, case objects,
 *  and value classes.
 *
 *  Selectively added to case classes/objects, unless a non-default
 *  implementation already exists:
 *    def equals(other: Any): Boolean
 *    def hashCode(): Int
 *    def canEqual(other: Any): Boolean
 *    def toString(): String
 *    def productElement(i: Int): Any
 *    def productArity: Int
 *    def productPrefix: String
 *
 *  Add to serializable static objects, unless an implementation
 *  already exists:
 *    private def writeReplace(): AnyRef
 *
 *  Selectively added to value classes, unless a non-default
 *  implementation already exists:
 *    def equals(other: Any): Boolean
 *    def hashCode(): Int
 */
class SyntheticMembers(thisPhase: DenotTransformer) {
  import SyntheticMembers.*
  import ast.tpd.*

  private var myValueSymbols: List[Symbol] = Nil
  private var myCaseSymbols: List[Symbol] = Nil
  private var myCaseModuleSymbols: List[Symbol] = Nil
  private var myEnumValueSymbols: List[Symbol] = Nil
  private var myNonJavaEnumValueSymbols: List[Symbol] = Nil

  private def initSymbols(using Context) =
    if (myValueSymbols.isEmpty) {
      myValueSymbols = List(defn.Any_hashCode, defn.Any_equals)
      myCaseSymbols = defn.caseClassSynthesized
      myCaseModuleSymbols = myCaseSymbols.filter(_ ne defn.Any_equals)
      myEnumValueSymbols = List(defn.Product_productPrefix)
      myNonJavaEnumValueSymbols = myEnumValueSymbols :+ defn.Any_toString :+ defn.Enum_ordinal
    }

  def valueSymbols(using Context): List[Symbol] = { initSymbols; myValueSymbols }
  def caseSymbols(using Context): List[Symbol] = { initSymbols; myCaseSymbols }
  def caseModuleSymbols(using Context): List[Symbol] = { initSymbols; myCaseModuleSymbols }
  def enumValueSymbols(using Context): List[Symbol] = { initSymbols; myEnumValueSymbols }
  def nonJavaEnumValueSymbols(using Context): List[Symbol] = { initSymbols; myNonJavaEnumValueSymbols }

  private def existingDef(sym: Symbol, clazz: ClassSymbol)(using Context): Symbol =
    val existing = sym.matchingMember(clazz.thisType)
    if existing != sym && !existing.is(Deferred) then existing else NoSymbol

  private def synthesizeDef(sym: TermSymbol, rhsFn: List[List[Tree]] => Context ?=> Tree)(using Context): Tree =
    DefDef(sym, rhsFn(_)(using ctx.withOwner(sym))).withSpan(ctx.owner.span.focus)

  /** If this is a case or value class, return the appropriate additional methods,
   *  otherwise return nothing.
   */
  def caseAndValueMethods(clazz: ClassSymbol)(using Context): List[Tree] = {
    val clazzType = clazz.appliedRef
    lazy val accessors =
      if clazz.isDerivedValueClass then clazz.paramAccessors.take(1) // Tail parameters can only be `erased`
      else clazz.caseAccessors
    val isEnumValue = clazz.isAnonymousClass && clazz.info.parents.head.classSymbol.is(Enum)
    val isSimpleEnumValue = isEnumValue && !clazz.owner.isAllOf(EnumCase)
    val isJavaEnumValue = isEnumValue && clazz.derivesFrom(defn.JavaEnumClass)
    val isNonJavaEnumValue = isEnumValue && !isJavaEnumValue

    val symbolsToSynthesize: List[Symbol] =
      if clazz.is(Case) then
        if clazz.is(Module) then caseModuleSymbols
        else caseSymbols
      else if isNonJavaEnumValue then nonJavaEnumValueSymbols
      else if isEnumValue then enumValueSymbols
      else if clazz.isDerivedValueClass then valueSymbols
      else Nil

    def syntheticDefIfMissing(sym: Symbol): List[Tree] =
      if (existingDef(sym, clazz).exists) Nil else syntheticDef(sym) :: Nil

    def syntheticDef(sym: Symbol): Tree = {
      val synthetic = sym.copy(
        owner = clazz,
        flags = sym.flags &~ Deferred | Synthetic | Override,
        info = clazz.thisType.memberInfo(sym),
        coord = clazz.coord).enteredAfter(thisPhase).asTerm

      def forwardToRuntime(vrefs: List[Tree]): Tree =
        ref(defn.runtimeMethodRef("_" + sym.name.toString)).appliedToTermArgs(This(clazz) :: vrefs)

      def ownName: Tree =
        Literal(Constant(clazz.name.stripModuleClassSuffix.toString))

      def nameRef: Tree =
        if isJavaEnumValue then
          val name = Select(This(clazz), nme.name).ensureApplied
          if ctx.explicitNulls then name.cast(defn.StringType) else name
        else
          identifierRef

      def identifierRef: Tree =
        if isSimpleEnumValue then // owner is `def $new(_$ordinal: Int, $name: String) = new MyEnum { ... }`
          ref(clazz.owner.paramSymss.head.find(_.name == nme.nameDollar).get)
        else // assume owner is `val Foo = new MyEnum { def ordinal = 0 }`
          Literal(Constant(clazz.owner.name.toString))

      def ordinalRef: Tree =
        if isSimpleEnumValue then // owner is `def $new(_$ordinal: Int, $name: String) = new MyEnum { ... }`
          ref(clazz.owner.paramSymss.head.find(_.name == nme.ordinalDollar_).get)
        else // val CaseN = new MyEnum { ... def ordinal: Int = n }
          val vdef = clazz.owner
          val parentEnum = vdef.owner.companionClass
          val children = parentEnum.children.zipWithIndex
          val candidate: Option[Int] = children.collectFirst { case (child, idx) if child == vdef => idx }
          assert(candidate.isDefined, i"could not find child for $vdef in ${parentEnum.children}%, % of $parentEnum")
          Literal(Constant(candidate.get))

      def toStringBody(vrefss: List[List[Tree]]): Tree =
        if (clazz.is(ModuleClass)) ownName
        else if (isNonJavaEnumValue) identifierRef
        else forwardToRuntime(vrefss.head)

      def syntheticRHS(vrefss: List[List[Tree]])(using Context): Tree = synthetic.name match {
        case nme.hashCode_ if isDerivedValueClass(clazz) => valueHashCodeBody
        case nme.hashCode_ => chooseHashcode
        case nme.toString_ => toStringBody(vrefss)
        case nme.equals_ => equalsBody(vrefss.head.head)
        case nme.canEqual_ => canEqualBody(vrefss.head.head, synthetic.span)
        case nme.ordinal => ordinalRef
        case nme.productArity => Literal(Constant(accessors.length))
        case nme.productPrefix if isEnumValue => nameRef
        case nme.productPrefix => ownName
        case nme.productElement =>
          if ctx.settings.YcompileScala2Library.value then productElementBodyForScala2Compat(accessors.length, vrefss.head.head)
          else productElementBody(accessors.length, vrefss.head.head)
        case nme.productElementName => productElementNameBody(accessors.length, vrefss.head.head)
      }
      report.log(s"adding $synthetic to $clazz at ${ctx.phase}")
      synthesizeDef(synthetic, syntheticRHS)
    }

    /** The class
     *
     *  ```
     *  case class C(x: T, y: T)
     *  ```
     *
     *  gets the `productElement` method:
     *
     *  ```
     *  def productElement(index: Int): Any = index match {
     *    case 0 => this._1
     *    case 1 => this._2
     *    case _ => throw new IndexOutOfBoundsException(index.toString)
     *  }
     *  ```
     */
    def productElementBody(arity: Int, index: Tree)(using Context): Tree = {
      // case N => this._${N + 1}
      val cases = 0.until(arity).map { i =>
        val sel = This(clazz).select(nme.selectorName(i), _.info.isParameterless)
        CaseDef(Literal(Constant(i)), EmptyTree, sel)
      }

      Match(index, (cases :+ generateIOBECase(index)).toList)
    }

    /** The class
     *
     *  ```
     *  case class C(x: T, y: T)
     *  ```
     *
     *  gets the `productElement` method:
     *
     *  ```
     *  def productElement(index: Int): Any = index match {
     *    case 0 => this.x
     *    case 1 => this.y
     *    case _ => throw new IndexOutOfBoundsException(index.toString)
     *  }
     *  ```
     */
    def productElementBodyForScala2Compat(arity: Int, index: Tree)(using Context): Tree = {
      val caseParams = ctx.owner.owner.caseAccessors
      // case N => this.${paramNames(N)}
      val cases = caseParams.zipWithIndex.map { (caseParam, i) =>
        val sel = This(clazz).select(caseParam)
        CaseDef(Literal(Constant(i)), EmptyTree, sel)
      }

      Match(index, (cases :+ generateIOBECase(index)).toList)
    }

    /** The class
     *
     *  ```
     *  case class C(x: T, y: T)
     *  ```
     *
     *  gets the `productElementName` method:
     *
     *  ```
     *  def productElementName(index: Int): String = index match {
     *    case 0 => "x"
     *    case 1 => "y"
     *    case _ => throw new IndexOutOfBoundsException(index.toString)
     *  }
     *  ```
     */
    def productElementNameBody(arity: Int, index: Tree)(using Context): Tree = {
      // case N => // name for case arg N
      val cases = 0.until(arity).map { i =>
        CaseDef(Literal(Constant(i)), EmptyTree, Literal(Constant(accessors(i).name.toString)))
      }

      Match(index, (cases :+ generateIOBECase(index)).toList)
    }

    def generateIOBECase(index: Tree): CaseDef = {
      val ioob = defn.IndexOutOfBoundsException.typeRef
      // Second constructor of ioob that takes a String argument
      def filterStringConstructor(s: Symbol): Boolean = s.info match {
        case m: MethodType if s.isConstructor && m.paramInfos.size == 1 =>
          m.paramInfos.head.stripNull == defn.StringType
        case _ => false
      }
      val constructor = ioob.typeSymbol.info.decls.find(filterStringConstructor _).asTerm
      val stringIndex = Apply(Select(index, nme.toString_), Nil)
      val error = Throw(New(ioob, constructor, List(stringIndex)))

      // case _ => throw new IndexOutOfBoundsException(i.toString)
      CaseDef(Underscore(defn.IntType), EmptyTree, error)
    }

    /** The class
     *
     *  ```
     *  case class C(x: T, y: U)
     *  ```
     *
     *  gets the `equals` method:
     *
     *  ```
     *  def equals(that: Any): Boolean =
     *    (this eq that) || {
     *      that match {
     *        case x$0 @ (_: C @unchecked) => this.x == this$0.x && this.y == x$0.y && that.canEqual(this)
     *        case _ => false
     *     }
     *  ```
     *
     *  If `C` is a value class, the initial `eq` test is omitted.
     *  The `canEqual` test can be omitted if it is known that `canEqual` return always true.
     *
     *  `@unchecked` is needed for parametric case classes.
     *
     */
    def equalsBody(that: Tree)(using Context): Tree = {
      val thatAsClazz = newSymbol(ctx.owner, nme.x_0, SyntheticCase, clazzType, coord = ctx.owner.span) // x$0
      def wildcardAscription(tp: Type) = Typed(Underscore(tp), TypeTree(tp))
      val pattern = Bind(thatAsClazz, wildcardAscription(AnnotatedType(clazzType, Annotation(defn.UncheckedAnnot, thatAsClazz.span)))) // x$0 @ (_: C @unchecked)
      // compare primitive fields first, slow equality checks of non-primitive fields can be skipped when primitives differ
      val sortedAccessors = accessors.sortBy(accessor => if (accessor.info.typeSymbol.isPrimitiveValueClass) 0 else 1)
      val comparisons = sortedAccessors.map { accessor =>
        This(clazz).withSpan(ctx.owner.span.focus).select(accessor).equal(ref(thatAsClazz).select(accessor)) }
      var rhs = // this.x == this$0.x && this.y == x$0.y && that.canEqual(this)
        if comparisons.isEmpty then Literal(Constant(true)) else comparisons.reduceBalanced(_ and _)
      val canEqualMeth = existingDef(defn.Product_canEqual, clazz)
      if !clazz.is(Final) || canEqualMeth.exists && !canEqualMeth.is(Synthetic) then
        rhs = rhs.and(
            ref(thatAsClazz)
            .select(canEqualMeth.orElse(defn.Product_canEqual))
            .appliedTo(This(clazz)))
      val matchingCase = CaseDef(pattern, EmptyTree, rhs) // case x$0 @ (_: C) => this.x == this$0.x && this.y == x$0.y
      val defaultCase = CaseDef(Underscore(defn.AnyType), EmptyTree, Literal(Constant(false))) // case _ => false
      val matchExpr = Match(that, List(matchingCase, defaultCase))
      if (isDerivedValueClass(clazz)) matchExpr
      else {
        val eqCompare = This(clazz).select(defn.Object_eq).appliedTo(that.cast(defn.ObjectType))
        eqCompare or matchExpr
      }
    }

    /** The class
     *
     *  ```
     *  class C(x: T) extends AnyVal
     *  ```
     *
     *  gets the `hashCode` method:
     *
     *  ```
     *  def hashCode: Int = x.hashCode()
     *  ```
     */
    def valueHashCodeBody(using Context): Tree = {
      assert(accessors.nonEmpty)
      ref(accessors.head).select(nme.hashCode_).ensureApplied
    }

    /** The class
     *
     *  ```
     *  case object C
     *  ```
     *
     *  gets the `hashCode` method:
     *
     *  ```
     *  def hashCode: Int = "C".hashCode // constant folded
     *  ```
     *
     *  The class
     *
     *  ```
     *  case class C(x: T, y: U)
     *  ```
     *
     *  if none of `T` or `U` are primitive types, gets the `hashCode` method:
     *
     *  ```
     *  def hashCode: Int = ScalaRunTime._hashCode(this)
     *  ```
     *
     *  else if either `T` or `U` are primitive, gets the `hashCode` method implemented by [[caseHashCodeBody]]
     */
    def chooseHashcode(using Context) =
      if (clazz.is(ModuleClass))
        Literal(Constant(clazz.name.stripModuleClassSuffix.toString.hashCode))
      else if (accessors.exists(_.info.finalResultType.classSymbol.isPrimitiveValueClass))
        caseHashCodeBody
      else
        ref(defn.ScalaRuntime__hashCode).appliedTo(This(clazz))

    /** The class
     *
     *  ```
     *  case class C(x: Int, y: T)
     *  ```
     *
     *  gets the `hashCode` method:
     *
     *  ```
     *  def hashCode: Int = {
     *    <synthetic> var acc: Int = 0xcafebabe
     *    acc = Statics.mix(acc, this.productPrefix.hashCode());
     *    acc = Statics.mix(acc, x);
     *    acc = Statics.mix(acc, Statics.this.anyHash(y));
     *    Statics.finalizeHash(acc, 2)
     *  }
     *  ```
     */
    def caseHashCodeBody(using Context): Tree = {
      val acc = newSymbol(ctx.owner, nme.acc, Mutable | Synthetic, defn.IntType, coord = ctx.owner.span)
      val accDef = ValDef(acc, Literal(Constant(0xcafebabe)))
      val mixPrefix = Assign(ref(acc),
        ref(defn.staticsMethod("mix")).appliedTo(ref(acc), This(clazz).select(defn.Product_productPrefix).select(defn.Any_hashCode).appliedToNone))
      val mixes = for (accessor <- accessors) yield
        Assign(ref(acc), ref(defn.staticsMethod("mix")).appliedTo(ref(acc), hashImpl(accessor)))
      val finish = ref(defn.staticsMethod("finalizeHash")).appliedTo(ref(acc), Literal(Constant(accessors.size)))
      Block(accDef :: mixPrefix :: mixes, finish)
    }

    /** The `hashCode` implementation for given symbol `sym`. */
    def hashImpl(sym: Symbol)(using Context): Tree =
      defn.scalaClassName(sym.info.finalResultType) match {
        case tpnme.Unit | tpnme.Null               => Literal(Constant(0))
        case tpnme.Boolean                         => If(ref(sym), Literal(Constant(1231)), Literal(Constant(1237)))
        case tpnme.Int                             => ref(sym)
        case tpnme.Short | tpnme.Byte | tpnme.Char => ref(sym).select(nme.toInt)
        case tpnme.Long                            => ref(defn.staticsMethod("longHash")).appliedTo(ref(sym))
        case tpnme.Double                          => ref(defn.staticsMethod("doubleHash")).appliedTo(ref(sym))
        case tpnme.Float                           => ref(defn.staticsMethod("floatHash")).appliedTo(ref(sym))
        case _                                     => ref(defn.staticsMethod("anyHash")).appliedTo(ref(sym))
      }

    /** The class
     *
     *  ```
     *  case class C(...)
     *  ```
     *
     *  gets the `canEqual` method
     *
     *  ```
     *  def canEqual(that: Any) = that.isInstanceOf[C @unchecked]
     *  ```
     *
     *  `@unchecked` is needed for parametric case classes.
     */
    def canEqualBody(that: Tree, span: Span): Tree = that.isInstance(AnnotatedType(clazzType, Annotation(defn.UncheckedAnnot, span)))

    symbolsToSynthesize.flatMap(syntheticDefIfMissing)
  }

  private def hasWriteReplace(clazz: ClassSymbol)(using Context): Boolean =
    clazz.membersNamed(nme.writeReplace)
      .filterWithPredicate(s => s.signature == Signature(defn.AnyRefType, sourceLanguage = SourceLanguage.Scala3))
      .exists

  private def hasReadResolve(clazz: ClassSymbol)(using Context): Boolean =
    clazz.membersNamed(nme.readResolve)
      .filterWithPredicate(s => s.signature == Signature(defn.AnyRefType, sourceLanguage = SourceLanguage.Scala3))
      .exists

  private def writeReplaceDef(clazz: ClassSymbol)(using Context): TermSymbol =
    newSymbol(clazz, nme.writeReplace, PrivateMethod | Synthetic,
        MethodType(Nil, defn.AnyRefType), coord = clazz.coord).entered.asTerm

  private def readResolveDef(clazz: ClassSymbol)(using Context): TermSymbol =
    newSymbol(clazz, nme.readResolve, PrivateMethod | Synthetic,
        MethodType(Nil, defn.AnyRefType), coord = clazz.coord).entered.asTerm

  /** If this is a static object `Foo`, add the method:
   *
   *      private def writeReplace(): AnyRef =
   *        new scala.runtime.ModuleSerializationProxy(classOf[Foo.type])
   *
   *  unless an implementation already exists, otherwise do nothing.
   *
   *  All static objects receive the `Serializable` flag in the back-end, so
   *  we do that even for objects that are not serializable at this phase.
   */
  def serializableObjectMethod(clazz: ClassSymbol)(using Context): List[Tree] =
    if clazz.is(Module)
      && clazz.isStatic
      && !hasWriteReplace(clazz)
      && ctx.platform.shouldReceiveJavaSerializationMethods(clazz)
    then
      List(
        DefDef(writeReplaceDef(clazz),
          _ => New(defn.ModuleSerializationProxyClass.typeRef,
                   defn.ModuleSerializationProxyConstructor,
                   List(Literal(Constant(clazz.sourceModule.termRef)))))
          .withSpan(ctx.owner.span.focus))
    else
      Nil

  /** Is this an anonymous class deriving from an enum definition? */
  extension (cls: ClassSymbol) private def isEnumValueImplementation(using Context): Boolean =
    cls.isAnonymousClass && cls.info.parents.head.typeSymbol.is(Enum) // asserted in Typer

  /** If this is the class backing a serializable singleton enum value with base class `MyEnum`,
   *  and not deriving from `java.lang.Enum` add the method:
   *
   *      private def readResolve(): AnyRef =
   *        MyEnum.fromOrdinal(this.ordinal)
   *
   *  unless an implementation already exists, otherwise do nothing.
   */
   def serializableEnumValueMethod(clazz: ClassSymbol)(using Context): List[Tree] =
    if clazz.isEnumValueImplementation
      && !clazz.derivesFrom(defn.JavaEnumClass)
      && clazz.isSerializable
      && !hasReadResolve(clazz)
      && ctx.platform.shouldReceiveJavaSerializationMethods(clazz)
    then
      List(
        DefDef(readResolveDef(clazz),
          _ => ref(clazz.owner.owner.sourceModule)
                .select(nme.fromOrdinal)
                .appliedTo(This(clazz).select(nme.ordinal).ensureApplied))
          .withSpan(ctx.owner.span.focus))
    else
      Nil

  /** The class
   *
   *  ```
   *  case class C[T <: U](x: T, y: String*)
   *  ```
   *
   *  gets the `fromProduct` method:
   *
   *  ```
   *  def fromProduct(x$0: Product): MirroredMonoType =
   *    new C[U](
   *      x$0.productElement(0).asInstanceOf[U],
   *      x$0.productElement(1).asInstanceOf[Seq[String]]: _*)
   *  ```
   *  where
   *  ```
   *  type MirroredMonoType = C[?]
   *  ```
   */
  def fromProductBody(caseClass: Symbol, param: Tree, optInfo: Option[MirrorImpl.OfProduct])(using Context): Tree =
    def extractParams(tpe: Type): List[Type] =
      tpe.asInstanceOf[MethodType].paramInfos

    def computeFromCaseClass: (Type, List[Type]) =
      val (baseRef, baseInfo) =
        val rawRef = caseClass.typeRef
        // TODO: HERE!!
        val rawInfo = caseClass.primaryConstructor.info
        optInfo match
          case Some(info) =>
            (rawRef.asSeenFrom(info.pre, caseClass.owner), rawInfo.asSeenFrom(info.pre, caseClass.owner))
          case _ =>
            (rawRef, rawInfo)
      baseInfo match
        case tl: PolyType =>
          val tvars = constrained(tl)
          val targs = for tvar <- tvars yield
            tvar.instantiate(fromBelow = false)
          (baseRef.appliedTo(targs), extractParams(tl.instantiate(targs)))
        case methTpe =>
          (baseRef, extractParams(methTpe))
    end computeFromCaseClass

    val (classRefApplied, paramInfos) = computeFromCaseClass
    val elems =
      for ((formal, idx) <- paramInfos.zipWithIndex) yield
        val elem =
          param.select(defn.Product_productElement).appliedTo(Literal(Constant(idx)))
            .ensureConforms(formal.translateFromRepeated(toArray = false))
        if (formal.isRepeatedParam) ctx.typer.seqToRepeated(elem) else elem
    New(classRefApplied, elems)
  end fromProductBody

  /** For an enum T:
   *
   *     def ordinal(x: MirroredMonoType) = x.ordinal
   *
   *  For  sealed trait with children of normalized types C_1, ..., C_n:
   *
   *     def ordinal(x: MirroredMonoType) = x match {
   *        case _: C_1 => 0
   *        ...
   *        case _: C_n => n - 1
   *     }
   *
   *  Here, the normalized type of a class C is C[?, ...., ?] with
   *  a wildcard for each type parameter. The normalized type of an object
   *  O is O.type.
   */
  def ordinalBody(cls: Symbol, param: Tree, optInfo: Option[MirrorImpl.OfSum])(using Context): Tree =
    if cls.is(Enum) then
      param.select(nme.ordinal).ensureApplied
    else
      def computeChildTypes: List[Type] =
        def rawRef(child: Symbol): Type =
          if (child.isTerm) child.reachableTermRef else child.reachableRawTypeRef
        optInfo match
          case Some(info) => info
            .childPres
            .lazyZip(cls.children)
            .map((pre, child) => rawRef(child).asSeenFrom(pre, child.owner))
          case _ =>
            cls.children.map(rawRef)

      val childTypes = computeChildTypes
      val cases =
        for (patType, idx) <- childTypes.zipWithIndex yield
          val pat = Typed(untpd.Ident(nme.WILDCARD).withType(patType), TypeTree(patType))
          CaseDef(pat, EmptyTree, Literal(Constant(idx)))

      Match(param.annotated(New(defn.UncheckedAnnot.typeRef, Nil)), cases)
  end ordinalBody

  /** - If `impl` is the companion of a generic sum, add `deriving.Mirror.Sum` parent
   *    and `MirroredMonoType` and `ordinal` members.
   *  - If `impl` is the companion of a generic product, add `deriving.Mirror.Product` parent
   *    and `MirroredMonoType` and `fromProduct` members.
   *  - If `impl` is marked with one of the attachments ExtendsSingletonMirror or ExtendsSumOfProductMirror,
   *    remove the attachment and generate the corresponding mirror support,
   *    On this case the represented class or object is referred to in a pre-existing `MirroredMonoType`
   *    member of the template.
   */
  def addMirrorSupport(impl: Template)(using Context): Template = {
    val clazz = ctx.owner.asClass

    var newBody = impl.body
    var newParents = impl.parents
    def addParent(parent: Type): Unit = {
      newParents = newParents :+ TypeTree(parent)
      val oldClassInfo = clazz.classInfo
      val newClassInfo = oldClassInfo.derivedClassInfo(
        declaredParents = oldClassInfo.declaredParents :+ parent)
      clazz.copySymDenotation(info = newClassInfo).installAfter(thisPhase)
    }
    def addMethod(name: TermName, info: Type, cls: Symbol, body: (Symbol, Tree) => Context ?=> Tree): Unit = {
      val meth = newSymbol(clazz, name, Synthetic | Method, info, coord = clazz.coord)
      if (!existingDef(meth, clazz).exists) {
        meth.enteredAfter(thisPhase)
        newBody = newBody :+
          synthesizeDef(meth, vrefss => body(cls, vrefss.head.head))
      }
    }
    val linked = clazz.linkedClass
    lazy val monoType = {
      val existing = clazz.info.member(tpnme.MirroredMonoType).symbol
      if (existing.exists && !existing.is(Deferred)) existing
      else {
        val monoType =
          newSymbol(clazz, tpnme.MirroredMonoType, Synthetic, TypeAlias(linked.reachableRawTypeRef), coord = clazz.coord)
        newBody = newBody :+ TypeDef(monoType).withSpan(ctx.owner.span.focus)
        monoType.enteredAfter(thisPhase)
      }
    }
    def makeSingletonMirror() =
      addParent(defn.Mirror_SingletonClass.typeRef)
    def makeProductMirror(cls: Symbol, optInfo: Option[MirrorImpl.OfProduct]) = {
      addParent(defn.Mirror_ProductClass.typeRef)
      addMethod(nme.fromProduct, MethodType(defn.ProductClass.typeRef :: Nil, monoType.typeRef), cls,
        fromProductBody(_, _, optInfo).ensureConforms(monoType.typeRef))  // t4758.scala or i3381.scala are examples where a cast is needed
    }
    def makeSumMirror(cls: Symbol, optInfo: Option[MirrorImpl.OfSum]) = {
      addParent(defn.Mirror_SumClass.typeRef)
      addMethod(nme.ordinal, MethodType(monoType.typeRef :: Nil, defn.IntType), cls,
        ordinalBody(_, _, optInfo))
    }

    if (clazz.is(Module)) {
      if (clazz.is(Case)) makeSingletonMirror()
      else if (linked.isGenericProduct) makeProductMirror(linked, None)
      else if (linked.isGenericSum(NoType)) makeSumMirror(linked, None)
      else if (linked.is(Sealed))
        derive.println(i"$linked is not a sum because ${linked.whyNotGenericSum(NoType)}")
    }
    else if (impl.removeAttachment(ExtendsSingletonMirror).isDefined)
      makeSingletonMirror()
    else
      impl.removeAttachment(ExtendsSumOrProductMirror).match
        case Some(prodImpl: MirrorImpl.OfProduct) =>
          makeProductMirror(monoType.typeRef.dealias.classSymbol, Some(prodImpl))
        case Some(sumImpl: MirrorImpl.OfSum) =>
          makeSumMirror(monoType.typeRef.dealias.classSymbol, Some(sumImpl))
        case _ =>

    cpy.Template(impl)(parents = newParents, body = newBody)
  }

  def addSyntheticMembers(impl: Template)(using Context): Template = {
    val clazz = ctx.owner.asClass
    val syntheticMembers = serializableObjectMethod(clazz) ::: serializableEnumValueMethod(clazz) ::: caseAndValueMethods(clazz)
    checkInlining(syntheticMembers)
    val impl1 = cpy.Template(impl)(body = syntheticMembers ::: impl.body)
    if ctx.settings.YcompileScala2Library.value then impl1
    else addMirrorSupport(impl1)
  }

  private def checkInlining(syntheticMembers: List[Tree])(using Context): Unit =
    if syntheticMembers.exists(_.existsSubTree {
      case tree: GenericApply => tree.symbol.isAllOf(InlineMethod)
      case tree: Select => tree.symbol.isAllOf(InlineMethod)
      case _ => false
    }) then ctx.compilationUnit.needsInlining = true
}
