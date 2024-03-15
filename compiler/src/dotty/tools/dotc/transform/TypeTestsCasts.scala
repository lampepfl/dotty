package dotty.tools
package dotc
package transform

import scala.language.unsafeNulls as _

import core.*
import Contexts.*, Symbols.*, Types.*, Constants.*, StdNames.*, Decorators.*
import ast.untpd
import Erasure.Boxing.*
import TypeErasure.*

import core.Flags.*
import util.Spans.*
import reporting.*
import config.Printers.{ transforms => debug }

import patmat.Typ
import dotty.tools.dotc.util.SrcPos

/** This transform normalizes type tests and type casts,
 *  also replacing type tests with singleton argument type with reference equality check
 *  Any remaining type tests
 *   - use the object methods $isInstanceOf and $asInstanceOf
 *   - have a reference type as receiver
 *   - can be translated directly to machine instructions
 *
 * Unfortunately this phase ended up being not Y-checkable unless types are erased. A cast to an ConstantType(3) or x.type
 * cannot be rewritten before erasure. That's why TypeTestsCasts is called from Erasure.
 */
object TypeTestsCasts {
  import ast.tpd.*
  import typer.Inferencing.maximizeType
  import typer.ProtoTypes.constrained

  /** Tests whether `(x: X).isInstanceOf[P]` is uncheckable at runtime, returning the reason,
   *  or the empty string if it is checkable.
   *
   *  First do the following substitution:
   *  (a) replace `T @unchecked` and pattern binder types (e.g., `_$1`) in P with WildcardType
   *
   *  Then check:
   *
   *  1. if `X <:< P`, ""
   *  2. if `P` is a singleton type, ""
   *  3. if `P` refers to an abstract type member or type parameter, "it refers to an abstract type member or type parameter"
   *  4. if `P = Array[T]`, checkable(E, T) where `E` is the element type of `X`, defaults to `Any`.
   *  5. if `P` is `pre.F[Ts]` and `pre.F` refers to a class which is not `Array`:
   *     (a) replace `Ts` with fresh type variables `Xs`
   *     (b) constrain `Xs` with `pre.F[Xs] <:< X`
   *     (c) maximize `pre.F[Xs]`
   *     (d) if !`pre.F[Xs] <:< P`, "its type arguments can't be determined from $X"
   *  6. if `P = T1 | T2` or `P = T1 & T2`, checkable(X, T1) && checkable(X, T2).
   *  7. if `P` is a refinement type, "it's a refinement type"
   *  8. if `P` is a local class which is not statically reachable from the scope where `X` is defined, "it's a local class"
   *  9. if `X` is `T1 | T2`, checkable(T1, P) && checkable(T2, P).
   *  10. otherwise, ""
   */
  def whyUncheckable(X: Type, P: Type, span: Span)(using Context): String = atPhase(Phases.refchecksPhase.next) {
    extension (inline s1: String) inline def &&(inline s2: String): String = if s1 == "" then s2 else s1
    extension (inline b: Boolean) inline def |||(inline s: String): String = if b then "" else s

    // Run just before ElimOpaque transform (which follows RefChecks)
    def isAbstract(P: Type) = !P.dealias.typeSymbol.isClass

    def replaceP(tp: Type)(using Context) = new TypeMap {
      def apply(tp: Type) = tp match {
        case tref: TypeRef if tref.typeSymbol.isPatternBound =>
          WildcardType
        case AnnotatedType(_, annot) if annot.symbol == defn.UncheckedAnnot =>
          WildcardType
        case _ => mapOver(tp)
      }
    }.apply(tp)

    /** Returns true if the type arguments of `P` can be determined from `X` */
    def typeArgsDeterminable(X: Type, P: AppliedType)(using Context) = inContext(ctx.fresh.setExploreTyperState().setFreshGADTBounds) {
      val AppliedType(tycon, _) = P

      def underlyingLambda(tp: Type): TypeLambda = tp.ensureLambdaSub match {
        case tp: TypeLambda => tp
        case tp: TypeProxy => underlyingLambda(tp.superType)
      }
      val typeLambda = underlyingLambda(tycon)
      val tvars = constrained(typeLambda)
      val P1 = tycon.appliedTo(tvars)

      debug.println("before " + ctx.typerState.constraint.show)
      debug.println("P : " + P.show)
      debug.println("P1 : " + P1.show)
      debug.println("X : " + X.show)

      // It does not matter whether P1 is a subtype of X or not.
      // It just tries to infer type arguments of P1 from X if the value x
      // conforms to the type skeleton pre.F[_]. Then it goes on to check
      // if P1 <: P, which means the type arguments in P are trivial,
      // thus no runtime checks are needed for them.
      withMode(Mode.GadtConstraintInference) {
        // Why not widen type arguments here? Given the following program
        //
        //    trait Tree[-T] class Ident[-T] extends Tree[T]
        //
        //    def foo1(tree: Tree[Int]) = tree.isInstanceOf[Ident[Int]]
        //
        // In checking whether the test tree.isInstanceOf[Ident[Int]]
        // is realizable, we want to constrain Ident[X] <: Tree[Int],
        // such that we can infer X = Int and Ident[X] <:< Ident[Int].
        //
        // If we perform widening, we will get X = Nothing, and we don't have
        // Ident[X] <:< Ident[Int] any more.
        TypeComparer.constrainPatternType(P1, X, forceInvariantRefinement = true)
        debug.println(
          TypeComparer.explained(_.constrainPatternType(P1, X, forceInvariantRefinement = true))
        )
      }

      // Maximization of the type means we try to cover all possible values
      // which conform to the skeleton pre.F[_] and X. Then we have to make
      // sure all of them are actually of the type P, which implies that the
      // type arguments in P are trivial (no runtime check needed).
      maximizeType(P1, span)

      debug.println("after " + ctx.typerState.constraint.show)

      val res = P1 <:< P

      debug.println(TypeComparer.explained(_.isSubType(P1, P)))
      debug.println("P1 : " + P1.show)
      debug.println("P1 <:< P = " + res)

      res

    }

    def recur(X: Type, P: Type): String = trace(s"recur(${X.show}, ${P.show})") {
      (X <:< P) ||| P.dealias.match
      case _: SingletonType     => ""
      case _: TypeProxy
      if isAbstract(P)          => i"it refers to an abstract type member or type parameter"
      case defn.ArrayOf(tpT)    =>
        X match {
          case defn.ArrayOf(tpE)   => recur(tpE, tpT)
          case _                   => recur(defn.AnyType, tpT)
        }
      case tpe @ AppliedType(tycon, targs)     =>
        X.widenDealias match {
          case OrType(tp1, tp2) =>
            // This case is required to retrofit type inference,
            // which cut constraints in the following two cases:
            //   - T1 <:< T2 | T3
            //   - T1 & T2 <:< T3
            // See TypeComparer#either
            recur(tp1, P) && recur(tp2, P)
          case tpX: FlexibleType =>
            recur(tpX.underlying, P)
          case x =>
            // always false test warnings are emitted elsewhere
            // provablyDisjoint wants fully applied types as input; because we're in the middle of erasure, we sometimes get raw types here
            val xApplied =
              val tparams = x.typeParams
              if tparams.isEmpty then x else x.appliedTo(tparams.map(_ => WildcardType))
            TypeComparer.provablyDisjoint(xApplied, tpe.derivedAppliedType(tycon, targs.map(_ => WildcardType)))
            || typeArgsDeterminable(X, tpe)
            ||| i"its type arguments can't be determined from $X"
        }
      case AndType(tp1, tp2)    => recur(X, tp1) && recur(X, tp2)
      case OrType(tp1, tp2)     => recur(X, tp1) && recur(X, tp2)
      case AnnotatedType(t, _)  => recur(X, t)
      case tp2: RefinedType     => recur(X, tp2.parent)
        && (TypeComparer.hasMatchingMember(tp2.refinedName, X, tp2) ||| i"it's a refinement type")
      case tp2: RecType         => recur(X, tp2.parent)
      case _
      if P.classSymbol.isLocal && foundClasses(X).exists(P.classSymbol.isInaccessibleChildOf) => // 8
        i"it's a local class"
      case _                    => ""
    }

    val res = recur(X.widen, replaceP(P))

    debug.println(i"checking  $X isInstanceOf $P = $res")

    res
  }

  def interceptTypeApply(tree: TypeApply)(using Context): Tree = trace(s"transforming ${tree.show}", show = true) {
    /** Intercept `expr.xyz[XYZ]` */
    def interceptWith(expr: Tree): Tree =
      if (expr.isEmpty) tree
      else {
        val sym = tree.symbol

        def isPrimitive(tp: Type) = tp.classSymbol.isPrimitiveValueClass

        def derivedTree(expr1: Tree, sym: Symbol, tp: Type) =
          cpy.TypeApply(tree)(expr1.select(sym).withSpan(expr.span), List(TypeTree(tp)))

        def inMatch =
          tree.fun.symbol == defn.Any_typeTest ||  // new scheme
          expr.symbol.is(Case)                // old scheme

        def transformIsInstanceOf(
            expr: Tree, testType: Type,
            unboxedTestType: Type, flagUnrelated: Boolean): Tree = {
          def testCls = effectiveClass(testType.widen)
          def unboxedTestCls = effectiveClass(unboxedTestType.widen)

          def unreachable(why: => String)(using Context): Boolean = {
            if (flagUnrelated)
              if (inMatch) report.error(em"this case is unreachable since $why", expr.srcPos)
              else report.warning(em"this will always yield false since $why", expr.srcPos)
            false
          }

          /** Are `foundCls` and `testCls` classes that allow checks
           *  whether a test would be always false?
           */
          def isCheckable(foundCls: Symbol) =
            foundCls.isClass && testCls.isClass &&
            !(testCls.isPrimitiveValueClass && !foundCls.isPrimitiveValueClass) &&
               // if `test` is primitive but `found` is not, we might have a case like
               // found = java.lang.Integer, test = Int, which could be true
               // (not sure why that is so, but scalac behaves the same way)
            !(!testCls.isPrimitiveValueClass && foundCls.isPrimitiveValueClass) &&
               // foundCls can be `Boolean`, while testCls is `Integer`
               // it can happen in `(3: Boolean | Int).isInstanceOf[Int]`
            !foundCls.isDerivedValueClass && !testCls.isDerivedValueClass
               // we don't have the logic to handle derived value classes

          /** Check whether a runtime test that a value of `foundCls` can be a `testCls`
           *  can be true in some cases. Issues a warning or an error otherwise.
           */
          def checkSensical(foundClasses: List[Symbol])(using Context): Boolean =
            def exprType = i"type ${expr.tpe.widen.stripped}"
            def check(foundCls: Symbol): Boolean =
              if (!isCheckable(foundCls)) true
              else if (!foundCls.derivesFrom(testCls)) {
                val unrelated =
                  !testCls.derivesFrom(foundCls)
                  && !unboxedTestCls.derivesFrom(foundCls)
                  && (testCls.is(Final) || !testCls.is(Trait) && !foundCls.is(Trait))
                if (foundCls.is(Final))
                  unreachable(i"$exprType is not a subclass of $testCls")
                else if (unrelated)
                  unreachable(i"$exprType and $testCls are unrelated")
                else true
              }
              else true
            end check

            val foundEffectiveClass = effectiveClass(expr.tpe.widen)

            if foundEffectiveClass.isPrimitiveValueClass && !testCls.isPrimitiveValueClass then
              report.error(em"cannot test if value of $exprType is a reference of $testCls", tree.srcPos)
              false
            else foundClasses.exists(check)
          end checkSensical

          if (expr.tpe <:< testType) && inMatch then
            if expr.tpe.isNotNull then constant(expr, Literal(Constant(true)))
            else expr.testNotNull
          else {
            if expr.tpe.isBottomType then
              report.warning(TypeTestAlwaysDiverges(expr.tpe, testType), tree.srcPos)
            val nestedCtx = ctx.fresh.setNewTyperState()
            val foundClsSyms = foundClasses(expr.tpe.widen)
            val sensical = checkSensical(foundClsSyms)(using nestedCtx)
            if (!sensical) {
              nestedCtx.typerState.commit()
              constant(expr, Literal(Constant(false)))
            }
            else if (testCls.isPrimitiveValueClass)
              foundClsSyms match
                case List(cls) if cls.isPrimitiveValueClass =>
                  constant(expr, Literal(Constant(foundClsSyms.head == testCls)))
                case _ =>
                  transformIsInstanceOf(
                    expr, defn.boxedType(testCls.typeRef), testCls.typeRef, flagUnrelated)
            else
              derivedTree(expr, defn.Any_isInstanceOf, testType)
          }
        }

        def transformAsInstanceOf(testType: Type): Tree = {
          def testCls = effectiveClass(testType.widen)
          def foundClsSymPrimitive = {
            val foundClsSyms = foundClasses(expr.tpe.widen)
            foundClsSyms.size == 1 && foundClsSyms.head.isPrimitiveValueClass
          }
          if (erasure(expr.tpe) <:< testType)
            Typed(expr, tree.args.head) // Replace cast by type ascription (which does not generate any bytecode)
          else if (testCls eq defn.BoxedUnitClass)
            // as a special case, casting to Unit always successfully returns Unit
            Block(expr :: Nil, unitLiteral).withSpan(expr.span)
          else if (foundClsSymPrimitive)
            if (testCls.isPrimitiveValueClass) primitiveConversion(expr, testCls)
            else derivedTree(box(expr), defn.Any_asInstanceOf, testType)
          else if (testCls.isPrimitiveValueClass)
            unbox(expr.ensureConforms(defn.ObjectType), testType)
          else if (isDerivedValueClass(testCls))
            expr // adaptToType in Erasure will do the necessary type adaptation
          else if (testCls eq defn.NothingClass) {
            // In the JVM `x.asInstanceOf[Nothing]` would throw a class cast exception except when `x eq null`.
            // To avoid this loophole we execute `x` and then regardless of the result throw a `ClassCastException`
            val throwCCE = Throw(New(defn.ClassCastExceptionClass.typeRef, defn.ClassCastExceptionClass_stringConstructor,
                Literal(Constant("Cannot cast to scala.Nothing")) :: Nil))
            Block(expr :: Nil, throwCCE).withSpan(expr.span)
          }
          else
            derivedTree(expr, defn.Any_asInstanceOf, testType)
        }

        /** Transform isInstanceOf
         *
         *    expr.isInstanceOf[A | B]          ~~>  expr.isInstanceOf[A] | expr.isInstanceOf[B]
         *    expr.isInstanceOf[A & B]          ~~>  expr.isInstanceOf[A] & expr.isInstanceOf[B]
         *    expr.isInstanceOf[Tuple]          ~~>  scala.runtime.Tuples.isInstanceOfTuple(expr)
         *    expr.isInstanceOf[EmptyTuple]     ~~>  scala.runtime.Tuples.isInstanceOfEmptyTuple(expr)
         *    expr.isInstanceOf[NonEmptyTuple]  ~~>  scala.runtime.Tuples.isInstanceOfNonEmptyTuple(expr)
         *    expr.isInstanceOf[*:[_, _]]       ~~>  scala.runtime.Tuples.isInstanceOfNonEmptyTuple(expr)
         *
         *  The transform happens before erasure of `testType`, thus cannot be merged
         *  with `transformIsInstanceOf`, which depends on erased type of `testType`.
         */
        def transformTypeTest(expr: Tree, testType: Type, flagUnrelated: Boolean): Tree = testType.dealias match {
          case tref: TermRef if tref.symbol == defn.EmptyTupleModule =>
            ref(defn.RuntimeTuples_isInstanceOfEmptyTuple).appliedTo(expr)
          case _: SingletonType =>
            expr.isInstance(testType).withSpan(tree.span)
          case OrType(tp1, tp2) =>
            evalOnce(expr) { e =>
              transformTypeTest(e, tp1, flagUnrelated = false)
                .or(transformTypeTest(e, tp2, flagUnrelated = false))
            }
          case AndType(tp1, tp2) =>
            evalOnce(expr) { e =>
              transformTypeTest(e, tp1, flagUnrelated)
                .and(transformTypeTest(e, tp2, flagUnrelated))
            }
          case defn.MultiArrayOf(elem, ndims) if isGenericArrayElement(elem, isScala2 = false) =>
            def isArrayTest(arg: Tree) =
              ref(defn.runtimeMethodRef(nme.isArray)).appliedTo(arg, Literal(Constant(ndims)))
            if (ndims == 1) isArrayTest(expr)
            else evalOnce(expr) { e =>
              derivedTree(e, defn.Any_isInstanceOf, e.tpe)
                .and(isArrayTest(e))
            }
          case tref: TypeRef if tref.symbol == defn.TupleClass =>
            ref(defn.RuntimeTuples_isInstanceOfTuple).appliedTo(expr)
          case tref: TypeRef if tref.symbol == defn.NonEmptyTupleClass =>
            ref(defn.RuntimeTuples_isInstanceOfNonEmptyTuple).appliedTo(expr)
          case AppliedType(tref: TypeRef, _) if tref.symbol == defn.PairClass =>
            ref(defn.RuntimeTuples_isInstanceOfNonEmptyTuple).appliedTo(expr)
          case _ =>
            val testWidened = testType.widen
            defn.untestableClasses.find(testWidened.isRef(_)) match
              case Some(untestable) =>
                report.error(em"$untestable cannot be used in runtime type tests", tree.srcPos)
                constant(expr, Literal(Constant(false)))
              case _ =>
                val erasedTestType = erasure(testType)
                transformIsInstanceOf(expr, erasedTestType, erasedTestType, flagUnrelated)
        }

        if (sym.isTypeTest) {
          val argType = tree.args.head.tpe
          val isTrusted = tree.hasAttachment(PatternMatcher.TrustedTypeTestKey)
          if !isTrusted then
            checkTypePattern(expr.tpe, argType, expr.srcPos)
          transformTypeTest(expr, argType,
            flagUnrelated = enclosingInlineds.isEmpty) // if test comes from inlined code, dont't flag it even if it always false
        }
        else if (sym.isTypeCast)
          transformAsInstanceOf(erasure(tree.args.head.tpe))
        else tree
      }
    val expr = tree.fun match {
      case Select(expr, _) => expr
      case i: Ident =>
        val expr = desugarIdentPrefix(i)
        if (expr.isEmpty) expr
        else expr.withSpan(i.span)
      case _ => EmptyTree
    }
    interceptWith(expr)
  }

  /** After PatternMatcher, only Bind nodes are present in simple try-catch trees
   *  See i19013
   */
  def checkBind(tree: Bind)(using Context) =
    checkTypePattern(defn.ThrowableType, tree.body.tpe, tree.srcPos)

  private def checkTypePattern(exprTpe: Type, castTpe: Type, pos: SrcPos)(using Context) =
    val isUnchecked = exprTpe.widenTermRefExpr.hasAnnotation(defn.UncheckedAnnot)
    if !isUnchecked then
      val whyNot = whyUncheckable(exprTpe, castTpe, pos.span)
      if whyNot.nonEmpty then
        report.uncheckedWarning(UncheckedTypePattern(castTpe, whyNot), pos)

  private def effectiveClass(tp: Type)(using Context): Symbol =
    if tp.isRef(defn.PairClass) then effectiveClass(erasure(tp))
    else if tp.isRef(defn.AnyValClass) then defn.AnyClass
    else tp.classSymbol

  private[transform] def foundClasses(tp: Type)(using Context): List[Symbol] =
    def go(tp: Type, acc: List[Type])(using Context): List[Type] = tp.dealias match
      case  OrType(tp1, tp2) => go(tp2, go(tp1, acc))
      case AndType(tp1, tp2) => (for t1 <- go(tp1, Nil); t2 <- go(tp2, Nil) yield AndType(t1, t2)) ::: acc
      case _                 => tp :: acc
    go(tp, Nil).map(effectiveClass)
}
