package dotty.tools
package dotc
package transform

import core._
import StdNames.nme
import Types._
import transform.MegaPhase._
import Flags._
import Contexts._
import Symbols._
import Decorators._
import Denotations._, SymDenotations._
import DenotTransformers._
import NullOpsDecorator._

object ElimRepeated {
  val name: String = "elimRepeated"
  val description: String = "rewrite vararg parameters and arguments"
}

/** A transformer that eliminates repeated parameters (T*) from all types, replacing
 *  them with Seq or Array types and adapting repeated arguments to conform to
 *  the transformed type if needed.
 */
class ElimRepeated extends MiniPhase with InfoTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = ElimRepeated.name

  override def description: String = ElimRepeated.description

  override def changesMembers: Boolean = true // the phase adds vararg forwarders

  def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
    elimRepeated(tp, sym.is(JavaDefined))

  /** Create forwarder symbols for the methods that are annotated
   *  with `@varargs` or that override java varargs.
   *
   *  The definitions (DefDef) for these symbols are created by transformDefDef.
   */
  override def transform(ref: SingleDenotation)(using Context): SingleDenotation =
    def transformVarArgs(sym: Symbol, isJavaVarargsOverride: Boolean): Unit =
      val hasAnnotation = hasVarargsAnnotation(sym)
      val hasRepeatedParam = hasRepeatedParams(sym)
      if hasRepeatedParam then
        val parentHasAnnotation = parentHasVarargsAnnotation(sym)
        if isJavaVarargsOverride || hasAnnotation || parentHasAnnotation then
          // java varargs are more restrictive than scala's
          // see https://github.com/scala/bug/issues/11714
          val validJava = isValidJavaVarArgs(sym.info)
          if !validJava then
            report.error(em"""To generate java-compatible varargs:
                      |  - there must be a single repeated parameter
                      |  - it must be the last argument in the last parameter list
                      |""",
              sym.sourcePos)
          else
            addVarArgsForwarder(sym, isJavaVarargsOverride, hasAnnotation, parentHasAnnotation)
      else if hasAnnotation then
        report.error("A method without repeated parameters cannot be annotated with @varargs", sym.sourcePos)
    end

    super.transform(ref) match
      case ref1: SymDenotation if ref1.is(Method, butNot = JavaDefined) =>
        val sym = ref1.symbol
        val isJavaVarargsOverride = (ref1 ne ref) && overridesJava(sym)
        transformVarArgs(sym, isJavaVarargsOverride)
        if isJavaVarargsOverride then
          // This method won't override the corresponding Java method at the end of this phase,
          // only the forwarder added by `addVarArgsForwarder` will.
          ref1.copySymDenotation(initFlags = ref1.flags &~ Override)
        else
          ref1
      case ref1 =>
        ref1

  override def infoMayChange(sym: Symbol)(using Context): Boolean = sym.is(Method)

  /** Does `sym` override a symbol defined in a Java class? One might think that
   *  this can be expressed as
   *
   *      sym.allOverriddenSymbols.exists(_.is(JavaDefined))
   *
   *  but that does not work, since `allOverriddenSymbols` gets confused because the
   *  signatures of a Java varargs method and a Scala varargs override are not the same.
   */
  private def overridesJava(sym: Symbol)(using Context) =
    sym.owner.info.baseClasses.drop(1).exists { bc =>
      bc.is(JavaDefined) && {
        val other = bc.info.nonPrivateDecl(sym.name)
        other.hasAltWith { alt =>
          sym.owner.thisType.memberInfo(alt.symbol).matchesLoosely(sym.info)
        }
      }
    }

  private def hasVarargsAnnotation(sym: Symbol)(using Context) = sym.hasAnnotation(defn.VarargsAnnot)

  private def parentHasVarargsAnnotation(sym: Symbol)(using Context) = sym.allOverriddenSymbols.exists(hasVarargsAnnotation)

  private def isVarargsMethod(sym: Symbol)(using Context) =
    hasVarargsAnnotation(sym)
    || hasRepeatedParams(sym)
        && (overridesJava(sym) || sym.allOverriddenSymbols.exists(hasVarargsAnnotation))

  /** Eliminate repeated parameters from method types. */
  private def elimRepeated(tp: Type, isJava: Boolean)(using Context): Type = tp.stripTypeVar match
    case tp @ MethodTpe(paramNames, paramTypes, resultType) =>
      val resultType1 = elimRepeated(resultType, isJava)
      val paramTypes1 =
        val lastIdx = paramTypes.length - 1
        if lastIdx >= 0 then
          val last = paramTypes(lastIdx)
          if last.isRepeatedParam then
            paramTypes.updated(lastIdx, last.translateFromRepeated(toArray = isJava))
          else paramTypes
        else paramTypes
      tp.derivedLambdaType(paramNames, paramTypes1, resultType1)
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, elimRepeated(tp.resultType, isJava))
    case tp =>
      tp

  override def transformApply(tree: Apply)(using Context): Tree =
    val args = tree.args.mapConserve { arg =>
      if isWildcardStarArg(arg) then
        val expr = arg match
          case t: Typed => t.expr
          case _ => arg // if the argument has been lifted it's not a Typed (often it's an Ident)

        val isJavaDefined = tree.fun.symbol.is(JavaDefined)
        if isJavaDefined then
          adaptToArray(expr)
        else if expr.tpe.derivesFrom(defn.ArrayClass) then
          arrayToSeq(expr)
        else
          expr
      else
        arg
    }
    cpy.Apply(tree)(tree.fun, args)

  private def adaptToArray(tree: Tree)(implicit ctx: Context): Tree = tree match
    case SeqLiteral(elems, elemtpt) =>
      JavaSeqLiteral(elems, elemtpt).withSpan(tree.span)
    case _ =>
      val elemTp = tree.tpe.elemType
      val adapted =
        if tree.tpe.derivesFrom(defn.ArrayClass) then
          tree
        else
          ref(defn.DottyArraysModule)
          .select(nme.seqToArray)
          .appliedToType(elemTp)
          .appliedTo(tree, clsOf(elemTp))
      // This seemingly redundant type ascription is needed because the result
      // type of `adapted` might be erased to `Object`, but we need to keep
      // the precise result type at erasure for `Erasure.Boxing.cast` to adapt
      // a primitive array into a reference array if needed.
      // Test case in tests/run/t1360.scala.
      Typed(adapted, TypeTree(defn.ArrayOf(elemTp)))

  /** Convert an Array into a scala.Seq */
  private def arrayToSeq(tree: Tree)(using Context): Tree =
    wrapArray(tree, tree.tpe.elemType)

  /** Generate the method definitions for the varargs forwarders created in transform */
  override def transformDefDef(tree: DefDef)(using Context): Tree =
    // If transform reported an error, don't go further
    if ctx.reporter.hasErrors then
      return tree

    val sym = tree.symbol
    val isVarArgs = atPhase(thisPhase)(isVarargsMethod(sym))
    if isVarArgs then
      // Get the symbol generated in transform
      val forwarderType = atPhase(thisPhase)(toJavaVarArgs(sym.info))
      val forwarderSym = currentClass.info.decl(sym.name).alternatives
        .find(_.info.matches(forwarderType))
        .get
        .symbol.asTerm
      // Generate the method
      val forwarderDef = DefDef(forwarderSym, prefss => {
        val init :+ (last :+ vararg) = prefss: @unchecked
        // Can't call `.argTypes` here because the underlying array type is of the
        // form `Array[? <: SomeType]`, so we need `.argInfos` to get the `TypeBounds`.
        val elemtp = vararg.tpe.widen.argInfos.head
        ref(sym.termRef)
          .appliedToArgss(init)
          .appliedToTermArgs(last :+ wrapArray(vararg, elemtp))
        })
      Thicket(tree, forwarderDef)
    else
      tree

  /** Is there a repeated parameter in some parameter list? */
  private def hasRepeatedParams(sym: Symbol)(using Context): Boolean =
    sym.info.paramInfoss.nestedExists(_.isRepeatedParam)

  /** Is this the type of a method that has a repeated parameter type as
   *  its last parameter in the last parameter list?
   */
  private def isValidJavaVarArgs(tp: Type)(using Context): Boolean = tp match
    case mt: MethodType =>
      val initp :+ lastp = mt.paramInfoss: @unchecked
      initp.forall(_.forall(!_.isRepeatedParam)) &&
      lastp.nonEmpty &&
      lastp.init.forall(!_.isRepeatedParam) &&
      lastp.last.isRepeatedParam
    case pt: PolyType =>
      isValidJavaVarArgs(pt.resultType)
    case _ =>
      throw new Exception("Match error in @varargs checks. This should not happen, please open an issue " + tp)

  /** Add the symbol of a Java varargs forwarder to the scope.
   *  It retains all the flags of the original method.
   *
   *  @param original the original method symbol
   *  @param isBridge true if we are generating a "bridge" (synthetic override forwarder)
   *  @param hasAnnotation true if the method is annotated with `@varargs`
   *  @param parentHasAnnotation true if the method overrides a method that is annotated with `@varargs`
   *
   *  A forwarder is necessary because the following holds:
   *    - the varargs in `original` will change from `RepeatedParam[T]` to `Seq[T]` after this phase
   *    - _but_ the callers of the method expect its varargs to be changed to `Array[? <: T]`
   *  The solution is to add a method that converts its argument from `Array[? <: T]` to `Seq[T]` and
   *  forwards it to the original method.
   */
  private def addVarArgsForwarder(original: Symbol, isBridge: Boolean, hasAnnotation: Boolean, parentHasAnnotation: Boolean)(using Context): Unit =
    val owner = original.owner
    if !owner.isClass then
      report.error("inner methods cannot be annotated with @varargs", original.sourcePos)
      return

    val classInfo = owner.info

    // For simplicity we always set the varargs flag,
    // although it's not strictly necessary for overrides.
    val flags = original.flags | JavaVarargs

    // The java-compatible forwarder symbol
    val forwarder =
      original.copy(
        flags =
          if isBridge then flags | Artifact
          else if hasAnnotation && !parentHasAnnotation then flags &~ Override
          else flags,
        info = toJavaVarArgs(original.info)
      ).asTerm

    // Find methods that would conflict with the forwarder if the latter existed.
    // This needs to be done at thisPhase so that parent @varargs don't conflict.
    val conflicts =
      classInfo.member(original.name).altsWith { s =>
        s.matches(forwarder) && !(isBridge && s.is(JavaDefined))
      }
    conflicts match
      case conflict :: _ =>
        val src =
          if hasAnnotation then "@varargs"
          else if isBridge then "overriding a java varargs method"
          else "@varargs (on overridden method)"
        report.error(em"$src produces a forwarder method that conflicts with ${conflict.showDcl}", original.srcPos)
      case Nil =>
        forwarder.enteredAfter(thisPhase)
  end addVarArgsForwarder

  /** Convert type from Scala to Java varargs method */
  private def toJavaVarArgs(tp: Type)(using Context): Type = tp match
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, toJavaVarArgs(tp.resultType))
    case tp: MethodType =>
      tp.resultType match
        case m: MethodType => // multiple param lists
          tp.derivedLambdaType(tp.paramNames, tp.paramInfos, toJavaVarArgs(m))
        case _ =>
          val init :+ last = tp.paramInfos: @unchecked
          val vararg = varargArrayType(last)
          tp.derivedLambdaType(tp.paramNames, init :+ vararg, tp.resultType)

  /** Translate a repeated type T* to an `Array[? <: Upper]`
   *  such that it is compatible with java varargs.
   *
   *  When necessary we set `Upper = T & AnyRef`
   *  to prevent the erasure of `Array[? <: Upper]` to Object,
   *  which would break the varargs from Java.
   */
  private def varargArrayType(tp: Type)(using Context): Type =
    val array = tp.translateFromRepeated(toArray = true) // Array[? <: T]
    val element = array.elemType.hiBound // T

    if element <:< defn.AnyRefType
      || ctx.mode.is(Mode.SafeNulls) && element.stripNull <:< defn.AnyRefType
      || element.typeSymbol.isPrimitiveValueClass
    then array
    else defn.ArrayOf(TypeBounds.upper(AndType(element, defn.AnyRefType))) // Array[? <: T & AnyRef]
}
