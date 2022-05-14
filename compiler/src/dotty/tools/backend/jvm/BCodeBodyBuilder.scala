package dotty.tools
package backend
package jvm

import scala.language.unsafeNulls

import scala.annotation.switch
import scala.collection.mutable.SortedMap

import scala.tools.asm
import scala.tools.asm.{Handle, Opcodes}
import BCodeHelpers.InvokeStyle

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Flags.{Label => LabelFlag, _}
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.StdNames.{nme, str}
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.Erasure
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.util.Spans._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.report

/*
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
trait BCodeBodyBuilder extends BCodeSkelBuilder {
  // import global._
  // import definitions._
  import tpd._
  import int.{_, given}
  import DottyBackendInterface.symExtensions
  import bTypes._
  import coreBTypes._
  import BCodeBodyBuilder._

  protected val primitives: DottyPrimitives

  /*
   * Functionality to build the body of ASM MethodNode, except for `synchronized` and `try` expressions.
   */
  abstract class PlainBodyBuilder(cunit: CompilationUnit) extends PlainSkelBuilder(cunit) {

    import Primitives.TestOp

    /* ---------------- helper utils for generating methods and code ---------------- */

    def emit(opc: Int): Unit = { mnode.visitInsn(opc) }

    def emitZeroOf(tk: BType): Unit = {
      tk match {
        case BOOL => bc.boolconst(false)
        case BYTE  |
             SHORT |
             CHAR  |
             INT     => bc.iconst(0)
        case LONG    => bc.lconst(0)
        case FLOAT   => bc.fconst(0)
        case DOUBLE  => bc.dconst(0)
        case UNIT    => ()
        case _ => emit(asm.Opcodes.ACONST_NULL)
      }
    }

    /*
     * Emits code that adds nothing to the operand stack.
     * Two main cases: `tree` is an assignment,
     * otherwise an `adapt()` to UNIT is performed if needed.
     */
    def genStat(tree: Tree): Unit = {
      lineNumber(tree)

      tree match {
        case Assign(lhs @ DesugaredSelect(qual, _), rhs) =>
          val isStatic = lhs.symbol.isStaticMember
          if (!isStatic) { genLoadQualifier(lhs) }
          genLoad(rhs, symInfoTK(lhs.symbol))
          lineNumber(tree)
          // receiverClass is used in the bytecode to access the field. using sym.owner may lead to IllegalAccessError
          val receiverClass = qual.tpe.typeSymbol
          fieldStore(lhs.symbol, receiverClass)

        case Assign(lhs, rhs) =>
          val s = lhs.symbol
          val Local(tk, _, idx, _) = locals.getOrMakeLocal(s)

          rhs match {
            case Apply(Select(larg: Ident, nme.ADD), Literal(x) :: Nil)
            if larg.symbol == s && tk.isIntSizedType && x.isShortRange =>
              lineNumber(tree)
              bc.iinc(idx, x.intValue)

            case Apply(Select(larg: Ident, nme.SUB), Literal(x) :: Nil)
            if larg.symbol == s && tk.isIntSizedType && Constant(-x.intValue).isShortRange =>
              lineNumber(tree)
              bc.iinc(idx, -x.intValue)

            case _ =>
              genLoad(rhs, tk)
              lineNumber(tree)
              bc.store(idx, tk)
          }

        case _ =>
          genLoad(tree, UNIT)
      }
    }

    /* Generate code for primitive arithmetic operations. */
    def genArithmeticOp(tree: Tree, code: Int): BType = tree match{
      case Apply(fun @ DesugaredSelect(larg, _), args) =>
      var resKind = tpeTK(larg)

      assert(resKind.isNumericType || (resKind == BOOL),
             s"$resKind is not a numeric or boolean type [operation: ${fun.symbol}]")

      import ScalaPrimitivesOps._

      args match {
        // unary operation
        case Nil =>
          genLoad(larg, resKind)
          code match {
            case POS => () // nothing
            case NEG => bc.neg(resKind)
            case NOT => bc.genPrimitiveArithmetic(Primitives.NOT, resKind)
            case _ => abort(s"Unknown unary operation: ${fun.symbol.showFullName} code: $code")
          }

        // binary operation
        case rarg :: Nil =>
          val isShift = isShiftOp(code)
          resKind = tpeTK(larg).maxType(if (isShift) INT else tpeTK(rarg))

          if (isShift || isBitwiseOp(code)) {
            assert(resKind.isIntegralType || (resKind == BOOL),
                   s"$resKind incompatible with arithmetic modulo operation.")
          }

          genLoad(larg, resKind)
          genLoad(rarg, if (isShift) INT else resKind)

          (code: @switch) match {
            case ADD => bc add resKind
            case SUB => bc sub resKind
            case MUL => bc mul resKind
            case DIV => bc div resKind
            case MOD => bc rem resKind

            case OR  | XOR | AND => bc.genPrimitiveLogical(code, resKind)

            case LSL | LSR | ASR => bc.genPrimitiveShift(code, resKind)

            case _                   => abort(s"Unknown primitive: ${fun.symbol}[$code]")
          }

        case _ =>
          abort(s"Too many arguments for primitive function: $tree")
      }
      lineNumber(tree)
      resKind
    }

    /* Generate primitive array operations. */
    def genArrayOp(tree: Tree, code: Int, expectedType: BType): BType = tree match{

      case Apply(DesugaredSelect(arrayObj, _), args) =>
      import ScalaPrimitivesOps._
      val k = tpeTK(arrayObj)
      genLoad(arrayObj, k)
      val elementType = typeOfArrayOp.getOrElse[bTypes.BType](code, abort(s"Unknown operation on arrays: $tree code: $code"))

      var generatedType = expectedType

      if (isArrayGet(code)) {
        // load argument on stack
        assert(args.length == 1, s"Too many arguments for array get operation: $tree");
        genLoad(args.head, INT)
        generatedType = k.asArrayBType.componentType
        bc.aload(elementType)
      }
      else if (isArraySet(code)) {
        val List(a1, a2) = args
        genLoad(a1, INT)
        genLoad(a2)
        generatedType = UNIT
        bc.astore(elementType)
      } else {
        generatedType = INT
        emit(asm.Opcodes.ARRAYLENGTH)
      }
      lineNumber(tree)

      generatedType
    }

    def genLoadIfTo(tree: If, expectedType: BType, dest: LoadDestination): BType = tree match{
      case If(condp, thenp, elsep) =>

      val success = new asm.Label
      val failure = new asm.Label

      val hasElse = !elsep.isEmpty && (elsep match {
        case Literal(value) if value.tag == UnitTag => false
        case _ => true
      })

      genCond(condp, success, failure, targetIfNoJump = success)
      markProgramPoint(success)

      if dest == LoadDestination.FallThrough then
        if hasElse then
          val thenKind      = tpeTK(thenp)
          val elseKind      = tpeTK(elsep)
          def hasUnitBranch = (thenKind == UNIT || elseKind == UNIT) && expectedType == UNIT
          val resKind       = if (hasUnitBranch) UNIT else tpeTK(tree)

          val postIf = new asm.Label
          genLoadTo(thenp, resKind, LoadDestination.Jump(postIf))
          markProgramPoint(failure)
          genLoadTo(elsep, resKind, LoadDestination.FallThrough)
          markProgramPoint(postIf)
          resKind
        else
          genLoad(thenp, UNIT)
          markProgramPoint(failure)
          UNIT
        end if
      else
        genLoadTo(thenp, expectedType, dest)
        markProgramPoint(failure)
        if hasElse then
          genLoadTo(elsep, expectedType, dest)
        else
          genAdaptAndSendToDest(UNIT, expectedType, dest)
        expectedType
      end if
    }

    def genPrimitiveOp(tree: Apply, expectedType: BType): BType = (tree: @unchecked) match {
      case Apply(fun @ DesugaredSelect(receiver, _), _) =>
      val sym = tree.symbol

      val code = primitives.getPrimitive(tree, receiver.tpe)

      import ScalaPrimitivesOps._

      if (isArithmeticOp(code))                genArithmeticOp(tree, code)
      else if (code == CONCAT) genStringConcat(tree)
      else if (code == HASH)   genScalaHash(receiver)
      else if (isArrayOp(code))                genArrayOp(tree, code, expectedType)
      else if (isLogicalOp(code) || isComparisonOp(code)) {
        val success, failure, after = new asm.Label
        genCond(tree, success, failure, targetIfNoJump = success)
        // success block
        markProgramPoint(success)
        bc boolconst true
        bc goTo after
        // failure block
        markProgramPoint(failure)
        bc boolconst false
        // after
        markProgramPoint(after)

        BOOL
      }
      else if (isCoercion(code)) {
        genLoad(receiver)
        lineNumber(tree)
        genCoercion(code)
        coercionTo(code)
      }
      else abort(
        s"Primitive operation not handled yet: ${sym.showFullName}(${fun.symbol.name}) at: ${tree.span}"
      )
    }

    def genLoad(tree: Tree): Unit = {
      genLoad(tree, tpeTK(tree))
    }

    /* Generate code for trees that produce values on the stack */
    def genLoad(tree: Tree, expectedType: BType): Unit =
      genLoadTo(tree, expectedType, LoadDestination.FallThrough)

    /* Generate code for trees that produce values, sent to a given `LoadDestination`. */
    def genLoadTo(tree: Tree, expectedType: BType, dest: LoadDestination): Unit =
      var generatedType = expectedType
      var generatedDest = LoadDestination.FallThrough

      lineNumber(tree)

      tree match {
        case tree@ValDef(_, _, _) =>
          val sym = tree.symbol
          /* most of the time, !locals.contains(sym), unless the current activation of genLoad() is being called
             while duplicating a finalizer that contains this ValDef. */
          val loc = locals.getOrMakeLocal(sym)
          val Local(tk, _, idx, isSynth) = loc
          if (tree.rhs == tpd.EmptyTree) { emitZeroOf(tk) }
          else { genLoad(tree.rhs, tk) }
          bc.store(idx, tk)
          val localVarStart = currProgramPoint()
          if (!isSynth) { // there are case <synthetic> ValDef's emitted by patmat
            varsInScope ::= (sym -> localVarStart)
          }
          generatedType = UNIT

        case t @ If(_, _, _) =>
          generatedType = genLoadIfTo(t, expectedType, dest)
          generatedDest = dest

        case t @ Labeled(_, _) =>
          generatedType = genLabeledTo(t, expectedType, dest)
          generatedDest = dest

        case r: Return =>
          genReturn(r)
          generatedDest = LoadDestination.Return

        case t @ WhileDo(_, _) =>
          generatedDest = genWhileDo(t)
          generatedType = UNIT

        case t @ Try(_, _, _) =>
          generatedType = genLoadTry(t)

        case t: Apply if t.fun.symbol eq defn.throwMethod =>
          val thrownExpr = t.args.head
          val thrownKind = tpeTK(thrownExpr)
          genLoadTo(thrownExpr, thrownKind, LoadDestination.Throw)
          generatedDest = LoadDestination.Throw

        case New(tpt) =>
          abort(s"Unexpected New(${tpt.tpe.showSummary()}/$tpt) reached GenBCode.\n" +
                "  Call was genLoad" + ((tree, expectedType)))

        case t @ Closure(env, call, tpt) =>
          val functionalInterface: Symbol =
            if !tpt.isEmpty then tpt.tpe.classSymbol
            else t.tpe.classSymbol
          val (fun, args) = call match {
            case Apply(fun, args) => (fun, args)
            case t @ DesugaredSelect(_, _) => (t, Nil) // TODO: use Select
            case t @ Ident(_) => (t, Nil)
          }

          if (!fun.symbol.isStaticMember) {
            // load receiver of non-static implementation of lambda

            // darkdimius: I haven't found in spec `this` reference should go
            // but I was able to derrive it by reading
            // AbstractValidatingLambdaMetafactory.validateMetafactoryArgs

            val DesugaredSelect(prefix, _) = fun
            genLoad(prefix)
          }

          genLoadArguments(env, fun.symbol.info.firstParamTypes map toTypeKind)
          generatedType = genInvokeDynamicLambda(NoSymbol, fun.symbol, env.size, functionalInterface)

        case app @ Apply(_, _) =>
          generatedType = genApply(app, expectedType)

        case This(qual) =>
          val symIsModuleClass = tree.symbol.is(ModuleClass)
          assert(tree.symbol == claszSymbol || symIsModuleClass,
                 s"Trying to access the this of another class: tree.symbol = ${tree.symbol}, class symbol = $claszSymbol compilation unit: $cunit")
          if (symIsModuleClass && tree.symbol != claszSymbol) {
            generatedType = genLoadModule(tree)
          }
          else {
            mnode.visitVarInsn(asm.Opcodes.ALOAD, 0)
            // When compiling Array.scala, the constructor invokes `Array.this.super.<init>`. The expectedType
            // is `[Object` (computed by typeToBType, the type of This(Array) is `Array[T]`). If we would set
            // the generatedType to `Array` below, the call to adapt at the end would fail. The situation is
            // similar for primitives (`I` vs `Int`).
            if (tree.symbol != defn.ArrayClass && !tree.symbol.isPrimitiveValueClass) {
              generatedType = classBTypeFromSymbol(claszSymbol)
            }
          }

        case DesugaredSelect(Ident(nme.EMPTY_PACKAGE), module) =>
          assert(tree.symbol.is(Module), s"Selection of non-module from empty package: $tree sym: ${tree.symbol} at: ${tree.span}")
          genLoadModule(tree)

        case DesugaredSelect(qualifier, _) =>
          val sym = tree.symbol
          generatedType = symInfoTK(sym)
          val qualSafeToElide = tpd.isIdempotentExpr(qualifier)

          def genLoadQualUnlessElidable(): Unit = { if (!qualSafeToElide) { genLoadQualifier(tree) } }

          // receiverClass is used in the bytecode to access the field. using sym.owner may lead to IllegalAccessError
          def receiverClass = qualifier.tpe.typeSymbol
          if (sym.is(Module)) {
            genLoadQualUnlessElidable()
            genLoadModule(tree)
          } else if (sym.isStaticMember) {
            genLoadQualUnlessElidable()
            fieldLoad(sym, receiverClass)
          } else {
            genLoadQualifier(tree)
            fieldLoad(sym, receiverClass)
          }

        case t @ Ident(name) =>
          val sym = tree.symbol
          val tk = symInfoTK(sym)
          generatedType = tk

          val desugared = cachedDesugarIdent(t)
          desugared match {
            case None =>
              if (!sym.is(Package)) {
                if (sym.is(Module)) genLoadModule(sym)
                else locals.load(sym)
              }
            case Some(t) =>
              genLoad(t, generatedType)
          }

        case Literal(value) =>
          if (value.tag != UnitTag) (value.tag, expectedType) match {
            case (IntTag,   LONG  ) => bc.lconst(value.longValue);       generatedType = LONG
            case (FloatTag, DOUBLE) => bc.dconst(value.doubleValue);     generatedType = DOUBLE
            case (NullTag,  _     ) => bc.emit(asm.Opcodes.ACONST_NULL); generatedType = RT_NULL
            case _                  => genConstant(value);               generatedType = tpeTK(tree)
          }

        case blck @ Block(stats, expr) =>
          if(stats.isEmpty)
            genLoadTo(expr, expectedType, dest)
          else
            genBlockTo(blck, expectedType, dest)
          generatedDest = dest

        case Typed(Super(_, _), _) =>
          genLoadTo(tpd.This(claszSymbol.asClass), expectedType, dest)
          generatedDest = dest

        case Typed(expr, _) =>
          genLoadTo(expr, expectedType, dest)
          generatedDest = dest

        case Assign(_, _) =>
          generatedType = UNIT
          genStat(tree)

        case av @ ArrayValue(_, _) =>
          generatedType = genArrayValue(av)

        case mtch @ Match(_, _) =>
          generatedType = genMatchTo(mtch, expectedType, dest)
          generatedDest = dest

        case tpd.EmptyTree => if (expectedType != UNIT) { emitZeroOf(expectedType) }


        case t: TypeApply => // dotty specific
          generatedType = genTypeApply(t)

        case _ => abort(s"Unexpected tree in genLoad: $tree/${tree.getClass} at: ${tree.span}")
      }

      // emit conversion and send to the right destination
      if generatedDest == LoadDestination.FallThrough then
        genAdaptAndSendToDest(generatedType, expectedType, dest)
    end genLoadTo

    def genAdaptAndSendToDest(generatedType: BType, expectedType: BType, dest: LoadDestination): Unit =
      if generatedType != expectedType then
        adapt(generatedType, expectedType)

      dest match
        case LoadDestination.FallThrough =>
          ()
        case LoadDestination.Jump(label) =>
          bc goTo label
        case LoadDestination.Return =>
          bc emitRETURN returnType
        case LoadDestination.Throw =>
          val thrownType = expectedType
          // `throw null` is valid although scala.Null (as defined in src/libray-aux) isn't a subtype of Throwable.
          // Similarly for scala.Nothing (again, as defined in src/libray-aux).
          assert(thrownType.isNullType || thrownType.isNothingType || thrownType.asClassBType.isSubtypeOf(ThrowableReference))
          emit(asm.Opcodes.ATHROW)
    end genAdaptAndSendToDest

    // ---------------- field load and store ----------------

    /*
     * must-single-thread
     */
    def fieldLoad( field: Symbol, hostClass: Symbol = null): Unit = fieldOp(field, isLoad = true,  hostClass)

    /*
     * must-single-thread
     */
    def fieldStore(field: Symbol, hostClass: Symbol = null): Unit = fieldOp(field, isLoad = false, hostClass)

    /*
     * must-single-thread
     */
    private def fieldOp(field: Symbol, isLoad: Boolean, specificReceiver: Symbol): Unit = {
      val useSpecificReceiver = specificReceiver != null && !field.isScalaStatic

      val owner      = internalName(if (useSpecificReceiver) specificReceiver else field.owner)
      val fieldJName = field.javaSimpleName
      val fieldDescr = symInfoTK(field).descriptor
      val isStatic   = field.isStaticMember
      val opc =
        if (isLoad) { if (isStatic) asm.Opcodes.GETSTATIC else asm.Opcodes.GETFIELD }
        else        { if (isStatic) asm.Opcodes.PUTSTATIC else asm.Opcodes.PUTFIELD }
      mnode.visitFieldInsn(opc, owner, fieldJName, fieldDescr)

    }

    // ---------------- emitting constant values ----------------

    /*
     * For ClazzTag:
     *   must-single-thread
     * Otherwise it's safe to call from multiple threads.
     */
    def genConstant(const: Constant): Unit = {
      (const.tag/*: @switch*/) match {

        case BooleanTag => bc.boolconst(const.booleanValue)

        case ByteTag    => bc.iconst(const.byteValue)
        case ShortTag   => bc.iconst(const.shortValue)
        case CharTag    => bc.iconst(const.charValue)
        case IntTag     => bc.iconst(const.intValue)

        case LongTag    => bc.lconst(const.longValue)
        case FloatTag   => bc.fconst(const.floatValue)
        case DoubleTag  => bc.dconst(const.doubleValue)

        case UnitTag    => ()

        case StringTag  =>
          assert(const.value != null, const) // TODO this invariant isn't documented in `case class Constant`
          mnode.visitLdcInsn(const.stringValue) // `stringValue` special-cases null, but not for a const with StringTag

        case NullTag    => emit(asm.Opcodes.ACONST_NULL)

        case ClazzTag   =>
          val tp = toTypeKind(const.typeValue)
          if tp.isPrimitive then
            val boxedClass = boxedClassOfPrimitive(tp.asPrimitiveBType)
            mnode.visitFieldInsn(
              asm.Opcodes.GETSTATIC,
              boxedClass.internalName,
              "TYPE", // field name
              jlClassRef.descriptor
            )
          else
            mnode.visitLdcInsn(tp.toASMType)

        case _ => abort(s"Unknown constant value: $const")
      }
    }

    private def genLabeledTo(tree: Labeled, expectedType: BType, dest: LoadDestination): BType = tree match {
      case Labeled(bind, expr) =>

      val labelSym = bind.symbol

      if dest == LoadDestination.FallThrough then
        val resKind = tpeTK(tree)
        val jumpTarget = new asm.Label
        registerJumpDest(labelSym, resKind, LoadDestination.Jump(jumpTarget))
        genLoad(expr, resKind)
        markProgramPoint(jumpTarget)
        resKind
      else
        registerJumpDest(labelSym, expectedType, dest)
        genLoadTo(expr, expectedType, dest)
        expectedType
      end if
    }

    private def genReturn(r: Return): Unit = {
      val expr: Tree = r.expr
      val fromSym: Symbol = if (r.from.symbol.is(LabelFlag)) r.from.symbol else NoSymbol

      if (NoSymbol == fromSym) {
        // return from enclosing method
        cleanups match {
          case Nil =>
            // not an assertion: !shouldEmitCleanup (at least not yet, pendingCleanups() may still have to run, and reset `shouldEmitCleanup`.
            genLoadTo(expr, returnType, LoadDestination.Return)
          case nextCleanup :: rest =>
            genLoad(expr, returnType)
            lineNumber(r)
            val saveReturnValue = (returnType != UNIT)
            if (saveReturnValue) {
              // regarding return value, the protocol is: in place of a `return-stmt`, a sequence of `adapt, store, jump` are inserted.
              if (earlyReturnVar == null) {
                earlyReturnVar = locals.makeLocal(returnType, "earlyReturnVar", expr.tpe, expr.span)
              }
              locals.store(earlyReturnVar)
            }
            bc goTo nextCleanup
            shouldEmitCleanup = true
        }
      } else {
        // return from labeled
        assert(fromSym.is(LabelFlag), fromSym)
        assert(!fromSym.is(Method), fromSym)

        /* TODO At the moment, we disregard cleanups, because by construction we don't have return-from-labels
         * that cross cleanup boundaries. However, in theory such crossings are valid, so we should take care
         * of them.
         */
        val (exprExpectedType, exprDest) = findJumpDest(fromSym)
        genLoadTo(expr, exprExpectedType, exprDest)
      }
    } // end of genReturn()

    def genWhileDo(tree: WhileDo): LoadDestination = tree match{
      case WhileDo(cond, body) =>

      val isInfinite = cond == tpd.EmptyTree

      val loop = new asm.Label
      markProgramPoint(loop)

      if isInfinite then
        val dest = LoadDestination.Jump(loop)
        genLoadTo(body, UNIT, dest)
        dest
      else
        body match
          case Literal(value) if value.tag == UnitTag =>
            // this is the shape of do..while loops
            val exitLoop = new asm.Label
            genCond(cond, loop, exitLoop, targetIfNoJump = exitLoop)
            markProgramPoint(exitLoop)
          case _ =>
            val success = new asm.Label
            val failure = new asm.Label
            genCond(cond, success, failure, targetIfNoJump = success)
            markProgramPoint(success)
            genLoadTo(body, UNIT, LoadDestination.Jump(loop))
            markProgramPoint(failure)
        end match
        LoadDestination.FallThrough
    }

    def genTypeApply(t: TypeApply): BType = (t: @unchecked) match {
      case TypeApply(fun@DesugaredSelect(obj, _), targs) =>

        val sym = fun.symbol
        val cast =
          if (sym == defn.Any_isInstanceOf) false
          else if (sym == defn.Any_asInstanceOf) true
          else abort(s"Unexpected type application $fun[sym: ${sym.showFullName}] in: $t")
        val l = tpeTK(obj)
        val r = tpeTK(targs.head)
        genLoadQualifier(fun)

        // TODO @lry make pattern match
        if (l.isPrimitive && r.isPrimitive)
          genConversion(l, r, cast)
        else if (l.isPrimitive) {
          bc drop l
          if (cast) {
            mnode.visitTypeInsn(asm.Opcodes.NEW, classCastExceptionReference.internalName)
            bc dup ObjectReference
            emit(asm.Opcodes.ATHROW)
          } else {
            bc boolconst false
          }
        }
        else if (r.isPrimitive && cast) {
          abort(s"Erasure should have added an unboxing operation to prevent this cast. Tree: $t")
        }
        else if (r.isPrimitive) {
          bc isInstance boxedClassOfPrimitive(r.asPrimitiveBType)
        }
        else {
          assert(r.isRef, r) // ensure that it's not a method
          genCast(r.asRefBType, cast)
        }

        if (cast) r else BOOL
    } // end of genTypeApply()


    private def mkArrayConstructorCall(arr: ArrayBType, app: Apply, args: List[Tree]) = {
      val dims     = arr.dimension
      var elemKind = arr.elementType
      val argsSize = args.length
      if (argsSize > dims) {
        report.error(s"too many arguments for array constructor: found ${args.length} but array has only $dims dimension(s)", ctx.source.atSpan(app.span))
      }
      if (argsSize < dims) {
        /* In one step:
         *   elemKind = new BType(BType.ARRAY, arr.off + argsSize, arr.len - argsSize)
         * however the above does not enter a TypeName for each nested arrays in chrs.
         */
        for (i <- args.length until dims) elemKind = ArrayBType(elemKind)
      }
      genLoadArguments(args, List.fill(args.size)(INT))
      (argsSize /*: @switch*/) match {
        case 1 => bc newarray elemKind
        case _ =>
          val descr = ("[" * argsSize) + elemKind.descriptor // denotes the same as: arrayN(elemKind, argsSize).descriptor
          mnode.visitMultiANewArrayInsn(descr, argsSize)
      }
    }


    /** Generate bootstrap method arguments for a case class
      *
      * @param classSym case class
      */
    private def genCaseBootstrapMethodArgs(classSym: Symbol): Array[AnyRef] = {
      val classTpe = classBTypeFromSymbol(classSym).toASMType

      // Boostrap method arguments: `Class<?>` of the receiver and `MethodHandle` of getters
      val bsmArgs = Array.newBuilder[AnyRef]
      bsmArgs += classTpe
      for (accessor <- classSym.caseAccessors) {

        /* When possible, using a field method handle will be more efficient.
         * However, a virtual call to the getter will always work if we can't
         * otherwise be certain the case class field won't be overriden.
         */
        val useField = claszSymbol.is(Final) || accessor.is(Final)
        val handleDescriptor = if (useField) {
          toTypeKind(accessor.info.finalResultType).toASMType.getDescriptor
        } else {
          asmMethodType(accessor).descriptor
        }
        bsmArgs += new asm.Handle(
          if (useField) Opcodes.H_GETFIELD else Opcodes.H_INVOKEVIRTUAL,
          classTpe.getInternalName,
          accessor.javaSimpleName,
          handleDescriptor,
          false // isInterface
        )
      }

      bsmArgs.result()
    }


    private def genApply(app: Apply, expectedType: BType): BType = {
      var generatedType = expectedType
      lineNumber(app)
      app match {
        case Apply(_, args) if app.symbol eq defn.newArrayMethod =>
          val List(elemClaz, Literal(c: Constant), ArrayValue(_, dims)) = args

          generatedType = toTypeKind(c.typeValue)
          mkArrayConstructorCall(generatedType.asArrayBType, app, dims)
        case Apply(t :TypeApply, _) =>
          generatedType =
            if (t.symbol ne defn.Object_synchronized) genTypeApply(t)
            else genSynchronized(app, expectedType)

        case Apply(fun @ DesugaredSelect(Super(superQual, _), _), args) =>
          // 'super' call: Note: since constructors are supposed to
          // return an instance of what they construct, we have to take
          // special care. On JVM they are 'void', and Scala forbids (syntactically)
          // to call super constructors explicitly and/or use their 'returned' value.
          // therefore, we can ignore this fact, and generate code that leaves nothing
          // on the stack (contrary to what the type in the AST says).

          // scala/bug#10290: qual can be `this.$outer()` (not just `this`), so we call genLoad (not just ALOAD_0)
          genLoad(superQual)
          genLoadArguments(args, paramTKs(app))
          generatedType = genCallMethod(fun.symbol, InvokeStyle.Super, app.span)

        // 'new' constructor call: Note: since constructors are
        // thought to return an instance of what they construct,
        // we have to 'simulate' it by DUPlicating the freshly created
        // instance (on JVM, <init> methods return VOID).
        case Apply(fun @ DesugaredSelect(New(tpt), nme.CONSTRUCTOR), args) =>
          val ctor = fun.symbol
          assert(ctor.isClassConstructor, s"'new' call to non-constructor: ${ctor.name}")

          generatedType = toTypeKind(tpt.tpe)
          assert(generatedType.isRef, s"Non reference type cannot be instantiated: $generatedType")

          generatedType match {
            case arr: ArrayBType =>
              mkArrayConstructorCall(arr, app, args)

            case rt: ClassBType =>
              assert(classBTypeFromSymbol(ctor.owner) == rt, s"Symbol ${ctor.owner.showFullName} is different from $rt")
              mnode.visitTypeInsn(asm.Opcodes.NEW, rt.internalName)
              bc dup generatedType
              genLoadArguments(args, paramTKs(app))
              genCallMethod(ctor, InvokeStyle.Special, app.span)

            case _ =>
              abort(s"Cannot instantiate $tpt of kind: $generatedType")
          }

        case Apply(fun, List(expr)) if Erasure.Boxing.isBox(fun.symbol) && fun.symbol.denot.owner != defn.UnitModuleClass =>
          val nativeKind = tpeTK(expr)
          genLoad(expr, nativeKind)
          val MethodNameAndType(mname, methodType) = asmBoxTo(nativeKind)
          bc.invokestatic(BoxesRunTime.internalName, mname, methodType.descriptor, itf = false)
          generatedType = boxResultType(fun.symbol) // was toTypeKind(fun.symbol.tpe.resultType)

        case Apply(fun, List(expr)) if Erasure.Boxing.isUnbox(fun.symbol) && fun.symbol.denot.owner != defn.UnitModuleClass =>
          genLoad(expr)
          val boxType = unboxResultType(fun.symbol) // was toTypeKind(fun.symbol.owner.linkedClassOfClass.tpe)
          generatedType = boxType
          val MethodNameAndType(mname, methodType) = asmUnboxTo(boxType)
          bc.invokestatic(BoxesRunTime.internalName, mname, methodType.descriptor, itf = false)


        case Apply(fun, List(arg @ This(clazz))) if fun.symbol == defn.ScalaCaseClassMethodsModule_caseHashCode =>
          genLoad(arg)
          mnode.visitInvokeDynamicInsn(
            "hashCode",
            "(" + toTypeKind(arg.tpe).descriptor + ")I",
            caseClassMethodsBootstrapHandle,
            genCaseBootstrapMethodArgs(clazz.symbol): _*
          )

        case Apply(fun, List(arg @ This(clazz), other)) if fun.symbol == defn.ScalaCaseClassMethodsModule_caseEquals =>
          genLoad(arg)
          genLoad(other)
          mnode.visitInvokeDynamicInsn(
            "equals",
            "(" + toTypeKind(arg.tpe).descriptor + "Ljava/lang/Object;)Z",
            caseClassMethodsBootstrapHandle,
            genCaseBootstrapMethodArgs(clazz.symbol): _*
          )

        case Apply(fun, List(arg @ This(clazz), index)) if fun.symbol == defn.ScalaCaseClassMethodsModule_caseProductElement =>
          genLoad(arg)
          genLoad(index)
          mnode.visitInvokeDynamicInsn(
            "productElement",
            "(" + toTypeKind(arg.tpe).descriptor + "I)Ljava/lang/Object;",
            caseClassMethodsBootstrapHandle,
            genCaseBootstrapMethodArgs(clazz.symbol): _*
          )

        case app @ Apply(fun, args) =>
          val sym = fun.symbol

          if (isPrimitive(fun)) { // primitive method call
            generatedType = genPrimitiveOp(app, expectedType)
          } else { // normal method call
            val invokeStyle =
              if (sym.isStaticMember) InvokeStyle.Static
              else if (sym.is(Private) || sym.isClassConstructor) InvokeStyle.Special
              else if (app.hasAttachment(BCodeHelpers.UseInvokeSpecial)) InvokeStyle.Special
              else InvokeStyle.Virtual

            if (invokeStyle.hasInstance) genLoadQualifier(fun)
            genLoadArguments(args, paramTKs(app))

            val DesugaredSelect(qual, name) = fun // fun is a Select, also checked in genLoadQualifier
            val isArrayClone = name == nme.clone_ && qual.tpe.widen.isInstanceOf[JavaArrayType]
            if (isArrayClone) {
              // Special-case Array.clone, introduced in 36ef60e. The goal is to generate this call
              // as "[I.clone" instead of "java/lang/Object.clone". This is consistent with javac.
              // Arrays have a public method `clone` (jls 10.7).
              //
              // The JVMS is not explicit about this, but that receiver type can be an array type
              // descriptor (instead of a class internal name):
              //   invokevirtual  #2; //Method "[I".clone:()Ljava/lang/Object
              //
              // Note that using `Object.clone()` would work as well, but only because the JVM
              // relaxes protected access specifically if the receiver is an array:
              //   http://hg.openjdk.java.net/jdk8/jdk8/hotspot/file/87ee5ee27509/src/share/vm/interpreter/linkResolver.cpp#l439
              // Example: `class C { override def clone(): Object = "hi" }`
              // Emitting `def f(c: C) = c.clone()` as `Object.clone()` gives a VerifyError.
              val target: String = tpeTK(qual).asRefBType.classOrArrayType
              val methodBType = asmMethodType(sym)
              bc.invokevirtual(target, sym.javaSimpleName, methodBType.descriptor)
              generatedType = methodBType.returnType
            } else {
              val receiverClass = if (!invokeStyle.isVirtual) null else {
                // receiverClass is used in the bytecode to as the method receiver. using sym.owner
                // may lead to IllegalAccessErrors, see 9954eaf / aladdin bug 455.
                val qualSym = qual.tpe.typeSymbol
                if (qualSym == defn.ArrayClass) {
                  // For invocations like `Array(1).hashCode` or `.wait()`, use Object as receiver
                  // in the bytecode. Using the array descriptor (like we do for clone above) seems
                  // to work as well, but it seems safer not to change this. Javac also uses Object.
                  // Note that array apply/update/length are handled by isPrimitive (above).
                  assert(sym.owner == defn.ObjectClass, s"unexpected array call: $app")
                  defn.ObjectClass
                } else qualSym
              }
              generatedType = genCallMethod(sym, invokeStyle, app.span, receiverClass)
            }
          }
      }

      generatedType
    } // end of genApply()

    private def genArrayValue(av: tpd.JavaSeqLiteral): BType = {
      val ArrayValue(tpt, elems) = av

      lineNumber(av)
      genArray(elems, tpt)
    }

    private def genArray(elems: List[Tree], elemType: Type): BType = {
      val elmKind       = toTypeKind(elemType)
      val generatedType = ArrayBType(elmKind)

      bc iconst   elems.length
      bc newarray elmKind

      var i = 0
      var rest = elems
      while (!rest.isEmpty) {
        bc dup     generatedType
        bc iconst  i
        genLoad(rest.head, elmKind)
        bc astore  elmKind
        rest = rest.tail
        i = i + 1
      }

      generatedType
    }

    /* A Match node contains one or more case clauses, each case clause lists one or more
     * Int/String values to use as keys, and a code block. The exception is the "default" case
     * clause which doesn't list any key (there is exactly one of these per match).
     */
    private def genMatchTo(tree: Match, expectedType: BType, dest: LoadDestination): BType = tree match {
      case Match(selector, cases) =>
      lineNumber(tree)

      val (generatedType, postMatch, postMatchDest) =
        if dest == LoadDestination.FallThrough then
          val postMatch = new asm.Label
          (tpeTK(tree), postMatch, LoadDestination.Jump(postMatch))
        else
          (expectedType, null, dest)

      // Only two possible selector types exist in `Match` trees at this point: Int and String
      if (tpeTK(selector) == INT) {

        /* On a first pass over the case clauses, we flatten the keys and their
         * targets (the latter represented with asm.Labels). That representation
         * allows JCodeMethodV to emit a lookupswitch or a tableswitch.
         *
         * On a second pass, we emit the switch blocks, one for each different target.
         */

        var flatKeys: List[Int]       = Nil
        var targets:  List[asm.Label] = Nil
        var default:  asm.Label       = null
        var switchBlocks: List[(asm.Label, Tree)] = Nil

        genLoad(selector, INT)

        // collect switch blocks and their keys, but don't emit yet any switch-block.
        for (caze @ CaseDef(pat, guard, body) <- cases) {
          assert(guard == tpd.EmptyTree, guard)
          val switchBlockPoint = new asm.Label
          switchBlocks ::= (switchBlockPoint, body)
          pat match {
            case Literal(value) =>
              flatKeys ::= value.intValue
              targets  ::= switchBlockPoint
            case Ident(nme.WILDCARD) =>
              assert(default == null, s"multiple default targets in a Match node, at ${tree.span}")
              default = switchBlockPoint
            case Alternative(alts) =>
              alts foreach {
                case Literal(value) =>
                  flatKeys ::= value.intValue
                  targets  ::= switchBlockPoint
                case _ =>
                  abort(s"Invalid alternative in alternative pattern in Match node: $tree at: ${tree.span}")
              }
            case _ =>
              abort(s"Invalid pattern in Match node: $tree at: ${tree.span}")
          }
        }

        bc.emitSWITCH(mkArrayReverse(flatKeys), mkArrayL(targets.reverse), default, MIN_SWITCH_DENSITY)

        // emit switch-blocks.
        for (sb <- switchBlocks.reverse) {
          val (caseLabel, caseBody) = sb
          markProgramPoint(caseLabel)
          genLoadTo(caseBody, generatedType, postMatchDest)
        }
      } else {

        /* Since the JVM doesn't have a way to switch on a string, we  switch
         * on the `hashCode` of the string then do an `equals` check (with a
         * possible second set of jumps if blocks can be reach from multiple
         * string alternatives).
         *
         * This mirrors the way that Java compiles `switch` on Strings.
         */

        var default:  asm.Label       = null
        var indirectBlocks: List[(asm.Label, Tree)] = Nil


        // Cases grouped by their hashCode
        val casesByHash = SortedMap.empty[Int, List[(String, Either[asm.Label, Tree])]]
        var caseFallback: Tree = null

        for (caze @ CaseDef(pat, guard, body) <- cases) {
          assert(guard == tpd.EmptyTree, guard)
          pat match {
            case Literal(value) =>
              val strValue = value.stringValue
              casesByHash.updateWith(strValue.##) { existingCasesOpt =>
                val newCase = (strValue, Right(body))
                Some(newCase :: existingCasesOpt.getOrElse(Nil))
              }
            case Ident(nme.WILDCARD) =>
              assert(default == null, s"multiple default targets in a Match node, at ${tree.span}")
              default = new asm.Label
              indirectBlocks ::= (default, body)
            case Alternative(alts) =>
              // We need an extra basic block since multiple strings can lead to this code
              val indirectCaseGroupLabel = new asm.Label
              indirectBlocks ::= (indirectCaseGroupLabel, body)
              alts foreach {
                case Literal(value) =>
                  val strValue = value.stringValue
                  casesByHash.updateWith(strValue.##) { existingCasesOpt =>
                    val newCase = (strValue, Left(indirectCaseGroupLabel))
                    Some(newCase :: existingCasesOpt.getOrElse(Nil))
                  }
                case _ =>
                  abort(s"Invalid alternative in alternative pattern in Match node: $tree at: ${tree.span}")
              }

            case _ =>
              abort(s"Invalid pattern in Match node: $tree at: ${tree.span}")
          }
        }

        // Organize the hashCode options into switch cases
        var flatKeys: List[Int]       = Nil
        var targets:  List[asm.Label] = Nil
        var hashBlocks: List[(asm.Label, List[(String, Either[asm.Label, Tree])])] = Nil
        for ((hashValue, hashCases) <- casesByHash) {
          val switchBlockPoint = new asm.Label
          hashBlocks ::= (switchBlockPoint, hashCases)
          flatKeys ::= hashValue
          targets  ::= switchBlockPoint
        }

        // Push the hashCode of the string (or `0` it is `null`) onto the stack and switch on it
        genLoadIfTo(
          If(
            tree.selector.select(defn.Any_==).appliedTo(nullLiteral),
            Literal(Constant(0)),
            tree.selector.select(defn.Any_hashCode).appliedToNone
          ),
          INT,
          LoadDestination.FallThrough
        )
        bc.emitSWITCH(mkArrayReverse(flatKeys), mkArrayL(targets.reverse), default, MIN_SWITCH_DENSITY)

        // emit blocks for each hash case
        for ((hashLabel, caseAlternatives) <- hashBlocks.reverse) {
          markProgramPoint(hashLabel)
          for ((caseString, indirectLblOrBody) <- caseAlternatives) {
            val comparison = if (caseString == null) defn.Any_== else defn.Any_equals
            val condp = Literal(Constant(caseString)).select(defn.Any_==).appliedTo(tree.selector)
            val keepGoing = new asm.Label
            indirectLblOrBody match {
              case Left(jump) =>
                genCond(condp, jump, keepGoing, targetIfNoJump = keepGoing)

              case Right(caseBody) =>
                val thisCaseMatches = new asm.Label
                genCond(condp, thisCaseMatches, keepGoing, targetIfNoJump = thisCaseMatches)
                markProgramPoint(thisCaseMatches)
                genLoadTo(caseBody, generatedType, postMatchDest)
            }
            markProgramPoint(keepGoing)
          }
          bc goTo default
        }

        // emit blocks for common patterns
        for ((caseLabel, caseBody) <- indirectBlocks.reverse) {
          markProgramPoint(caseLabel)
          genLoadTo(caseBody, generatedType, postMatchDest)
        }
      }

      if postMatch != null then
        markProgramPoint(postMatch)
      generatedType
    }

    def genBlockTo(tree: Block, expectedType: BType, dest: LoadDestination): Unit = tree match {
      case Block(stats, expr) =>

      val savedScope = varsInScope
      varsInScope = Nil
      stats foreach genStat
      genLoadTo(expr, expectedType, dest)
      emitLocalVarScopes()
      varsInScope = savedScope
    }

    /** Add entries to the `LocalVariableTable` JVM attribute for all the vars in
     *  `varsInScope`, ending at the current program point.
     */
    def emitLocalVarScopes(): Unit =
      if (emitVars) {
        val end = currProgramPoint()
        for ((sym, start) <- varsInScope.reverse) {
          emitLocalVarScope(sym, start, end)
        }
      }
    end emitLocalVarScopes

    def adapt(from: BType, to: BType): Unit = {
      if (!from.conformsTo(to)) {
        to match {
          case UNIT => bc drop from
          case _    => bc.emitT2T(from, to)
        }
      } else if (from.isNothingType) {
        /* There are two possibilities for from.isNothingType: emitting a "throw e" expressions and
         * loading a (phantom) value of type Nothing.
         *
         * The Nothing type in Scala's type system does not exist in the JVM. In bytecode, Nothing
         * is mapped to scala.runtime.Nothing$. To the JVM, a call to Predef.??? looks like it would
         * return an object of type Nothing$. We need to do something with that phantom object on
         * the stack. "Phantom" because it never exists: such methods always throw, but the JVM does
         * not know that.
         *
         * Note: The two verifiers (old: type inference, new: type checking) have different
         * requirements. Very briefly:
         *
         * Old (http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.10.2.1): at
         * each program point, no matter what branches were taken to get there
         *   - Stack is same size and has same typed values
         *   - Local and stack values need to have consistent types
         *   - In practice, the old verifier seems to ignore unreachable code and accept any
         *     instructions after an ATHROW. For example, there can be another ATHROW (without
         *     loading another throwable first).
         *
         * New (http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.10.1)
         *   - Requires consistent stack map frames. GenBCode generates stack frames if -target:jvm-1.6
         *     or higher.
         *   - In practice: the ASM library computes stack map frames for us (ClassWriter). Emitting
         *     correct frames after an ATHROW is probably complex, so ASM uses the following strategy:
         *       - Every time when generating an ATHROW, a new basic block is started.
         *       - During classfile writing, such basic blocks are found to be dead: no branches go there
         *       - Eliminating dead code would probably require complex shifts in the output byte buffer
         *       - But there's an easy solution: replace all code in the dead block with with
         *         `nop; nop; ... nop; athrow`, making sure the bytecode size stays the same
         *       - The corresponding stack frame can be easily generated: on entering a dead the block,
         *         the frame requires a single Throwable on the stack.
         *       - Since there are no branches to the dead block, the frame requirements are never violated.
         *
         * To summarize the above: it does matter what we emit after an ATHROW.
         *
         * NOW: if we end up here because we emitted a load of a (phantom) value of type Nothing$,
         * there was no ATHROW emitted. So, we have to make the verifier happy and do something
         * with that value. Since Nothing$ extends Throwable, the easiest is to just emit an ATHROW.
         *
         * If we ended up here because we generated a "throw e" expression, we know the last
         * emitted instruction was an ATHROW. As explained above, it is OK to emit a second ATHROW,
         * the verifiers will be happy.
         */
        if (lastInsn.getOpcode != asm.Opcodes.ATHROW)
          emit(asm.Opcodes.ATHROW)
      } else if (from.isNullType) {
        /* After loading an expression of type `scala.runtime.Null$`, introduce POP; ACONST_NULL.
         * This is required to pass the verifier: in Scala's type system, Null conforms to any
         * reference type. In bytecode, the type Null is represented by scala.runtime.Null$, which
         * is not a subtype of all reference types. Example:
         *
         *   def nl: Null = null // in bytecode, nl has return type scala.runtime.Null$
         *   val a: String = nl  // OK for Scala but not for the JVM, scala.runtime.Null$ does not conform to String
         *
         * In order to fix the above problem, the value returned by nl is dropped and ACONST_NULL is
         * inserted instead - after all, an expression of type scala.runtime.Null$ can only be null.
         */
        if (lastInsn.getOpcode != asm.Opcodes.ACONST_NULL) {
          bc drop from
          emit(asm.Opcodes.ACONST_NULL)
        }
      }
      else (from, to) match  {
        case (BYTE, LONG) | (SHORT, LONG) | (CHAR, LONG) | (INT, LONG) => bc.emitT2T(INT, LONG)
        case _ => ()
      }
    }

    /* Emit code to Load the qualifier of `tree` on top of the stack. */
    def genLoadQualifier(tree: Tree): Unit = {
      lineNumber(tree)
      tree match {
        case DesugaredSelect(qualifier, _) => genLoad(qualifier)
        case t: Ident             => // dotty specific
          cachedDesugarIdent(t) match {
            case Some(sel) => genLoadQualifier(sel)
            case None =>
              assert(t.symbol.owner == this.claszSymbol)
          }
        case _                    => abort(s"Unknown qualifier $tree")
      }
    }

    def genLoadArguments(args: List[Tree], btpes: List[BType]): Unit =
      args match
        case arg :: args1 =>
          btpes match
            case btpe :: btpes1 =>
              genLoad(arg, btpe)
              genLoadArguments(args1, btpes1)
            case _ =>
        case _ =>

    def genLoadModule(tree: Tree): BType = {
      val module = (
        if (!tree.symbol.is(PackageClass)) tree.symbol
        else tree.symbol.info.member(nme.PACKAGE).symbol match {
          case NoSymbol => abort(s"SI-5604: Cannot use package as value: $tree")
          case s        => abort(s"SI-5604: found package class where package object expected: $tree")
        }
      )
      lineNumber(tree)
      genLoadModule(module)
      symInfoTK(module)
    }

    def genLoadModule(module: Symbol): Unit = {
      def inStaticMethod = methSymbol != null && methSymbol.isStaticMember
      if (claszSymbol == module.moduleClass && jMethodName != "readResolve" && !inStaticMethod) {
        mnode.visitVarInsn(asm.Opcodes.ALOAD, 0)
      } else {
        val mbt = symInfoTK(module).asClassBType
        mnode.visitFieldInsn(
          asm.Opcodes.GETSTATIC,
          mbt.internalName /* + "$" */ ,
          str.MODULE_INSTANCE_FIELD,
          mbt.descriptor // for nostalgics: toTypeKind(module.tpe).descriptor
        )
      }
    }

    def genConversion(from: BType, to: BType, cast: Boolean): Unit = {
      if (cast) { bc.emitT2T(from, to) }
      else {
        bc drop from
        bc boolconst (from == to)
      }
    }

    def genCast(to: RefBType, cast: Boolean): Unit = {
      if (cast) { bc checkCast  to }
      else      { bc isInstance to }
    }

    /* Is the given symbol a primitive operation? */
    def isPrimitive(fun: Tree): Boolean = {
      primitives.isPrimitive(fun)
    }

    /* Generate coercion denoted by "code" */
    def genCoercion(code: Int): Unit = {
      import ScalaPrimitivesOps._
      (code: @switch) match {
        case B2B | S2S | C2C | I2I | L2L | F2F | D2D => ()
        case _ =>
          val from = coercionFrom(code)
          val to   = coercionTo(code)
          bc.emitT2T(from, to)
      }
    }

    /* Generate string concatenation
     *
     * On JDK 8: create and append using `StringBuilder`
     * On JDK 9+: use `invokedynamic` with `StringConcatFactory`
     */
    def genStringConcat(tree: Tree): BType = {
      lineNumber(tree)
      liftStringConcat(tree) match {
        // Optimization for expressions of the form "" + x
        case List(Literal(Constant("")), arg) =>
          genLoad(arg, ObjectReference)
          genCallMethod(defn.String_valueOf_Object, InvokeStyle.Static)

        case concatenations =>
          val concatArguments = concatenations.view
            .filter {
              case Literal(Constant("")) => false // empty strings are no-ops in concatenation
              case _ => true
            }
            .map {
              case Apply(boxOp, value :: Nil) if Erasure.Boxing.isBox(boxOp.symbol) && boxOp.symbol.denot.owner != defn.UnitModuleClass =>
                // Eliminate boxing of primitive values. Boxing is introduced by erasure because
                // there's only a single synthetic `+` method "added" to the string class.
                value
              case other => other
            }
            .toList

          // `StringConcatFactory` only got added in JDK 9, so use `StringBuilder` for lower
          if (classfileVersion < asm.Opcodes.V9) {

            // Estimate capacity needed for the string builder
            val approxBuilderSize = concatArguments.view.map {
              case Literal(Constant(s: String)) => s.length
              case Literal(c @ Constant(_)) if c.isNonUnitAnyVal => String.valueOf(c).length
              case _ => 0
            }.sum
            bc.genNewStringBuilder(approxBuilderSize)

            for (elem <- concatArguments) {
              val elemType = tpeTK(elem)
              genLoad(elem, elemType)
              bc.genStringBuilderAppend(elemType)
            }
            bc.genStringBuilderEnd
          } else {

            /* `StringConcatFactory#makeConcatWithConstants` accepts max 200 argument slots. If
             * the string concatenation is longer (unlikely), we spill into multiple calls
             */
            val MaxIndySlots = 200
            val TagArg = '\u0001'    // indicates a hole (in the recipe string) for an argument
            val TagConst = '\u0002'  // indicates a hole (in the recipe string) for a constant

            val recipe = new StringBuilder()
            val argTypes = Seq.newBuilder[asm.Type]
            val constVals = Seq.newBuilder[String]
            var totalArgSlots = 0
            var countConcats = 1     // ie. 1 + how many times we spilled

            for (elem <- concatArguments) {
              val tpe = tpeTK(elem)
              val elemSlots = tpe.size

              // Unlikely spill case
              if (totalArgSlots + elemSlots >= MaxIndySlots) {
                bc.genIndyStringConcat(recipe.toString, argTypes.result(), constVals.result())
                countConcats += 1
                totalArgSlots = 0
                recipe.setLength(0)
                argTypes.clear()
                constVals.clear()
              }

              elem match {
                case Literal(Constant(s: String)) =>
                  if (s.contains(TagArg) || s.contains(TagConst)) {
                    totalArgSlots += elemSlots
                    recipe.append(TagConst)
                    constVals += s
                  } else {
                    recipe.append(s)
                  }

                case other =>
                  totalArgSlots += elemSlots
                  recipe.append(TagArg)
                  val tpe = tpeTK(elem)
                  argTypes += tpe.toASMType
                  genLoad(elem, tpe)
              }
            }
            bc.genIndyStringConcat(recipe.toString, argTypes.result(), constVals.result())

            // If we spilled, generate one final concat
            if (countConcats > 1) {
              bc.genIndyStringConcat(
                TagArg.toString * countConcats,
                Seq.fill(countConcats)(StringRef.toASMType),
                Seq.empty
              )
            }
          }
      }
      StringRef
    }

    /**
     * Generate a method invocation. If `specificReceiver != null`, it is used as receiver in the
     * invocation instruction, otherwise `method.owner`. A specific receiver class is needed to
     * prevent an IllegalAccessError, (aladdin bug 455).
     */
    def genCallMethod(method: Symbol, style: InvokeStyle, pos: Span = NoSpan, specificReceiver: Symbol = null): BType = {
      val methodOwner = method.owner

      // the class used in the invocation's method descriptor in the classfile
      val receiverClass = {
        if (specificReceiver != null)
          assert(style.isVirtual || specificReceiver == methodOwner, s"specificReceiver can only be specified for virtual calls. $method - $specificReceiver")

        val useSpecificReceiver = specificReceiver != null && !defn.isBottomClass(specificReceiver) && !method.isScalaStatic
        val receiver = if (useSpecificReceiver) specificReceiver else methodOwner

        // workaround for a JVM bug: https://bugs.openjdk.java.net/browse/JDK-8154587
        // when an interface method overrides a member of Object (note that all interfaces implicitly
        // have superclass Object), the receiver needs to be the interface declaring the override (and
        // not a sub-interface that inherits it). example:
        //   trait T { override def clone(): Object = "" }
        //   trait U extends T
        //   class C extends U
        //   class D { def f(u: U) = u.clone() }
        // The invocation `u.clone()` needs `T` as a receiver:
        //   - using Object is illegal, as Object.clone is protected
        //   - using U results in a `NoSuchMethodError: U.clone. This is the JVM bug.
        // Note that a mixin forwarder is generated, so the correct method is executed in the end:
        //   class C { override def clone(): Object = super[T].clone() }
        val isTraitMethodOverridingObjectMember = {
          receiver != methodOwner && // fast path - the boolean is used to pick either of these two, if they are the same it does not matter
            style.isVirtual &&
            isEmittedInterface(receiver) &&
            defn.ObjectType.decl(method.name).symbol.exists && { // fast path - compute overrideChain on the next line only if necessary
              val syms = method.allOverriddenSymbols.toList
              !syms.isEmpty && syms.last.owner == defn.ObjectClass
            }
        }
        if (isTraitMethodOverridingObjectMember) methodOwner else receiver
      }

      receiverClass.info // ensure types the type is up to date; erasure may add lateINTERFACE to traits
      val receiverName = internalName(receiverClass)

      val jname    = method.javaSimpleName
      val bmType   = asmMethodType(method)
      val mdescr   = bmType.descriptor

      val isInterface = isEmittedInterface(receiverClass)
      import InvokeStyle._
      if (style == Super) {
        if (isInterface && !method.is(JavaDefined)) {
          val args = new Array[BType](bmType.argumentTypes.length + 1)
          val ownerBType = toTypeKind(method.owner.info)
          bmType.argumentTypes.copyToArray(args, 1)
          val staticDesc = MethodBType(ownerBType :: bmType.argumentTypes, bmType.returnType).descriptor
          val staticName = traitSuperAccessorName(method)
          bc.invokestatic(receiverName, staticName, staticDesc, isInterface)
        } else {
          bc.invokespecial(receiverName, jname, mdescr, isInterface)
        }
      } else {
        val opc = style match {
          case Static => Opcodes.INVOKESTATIC
          case Special => Opcodes.INVOKESPECIAL
          case Virtual => if (isInterface) Opcodes.INVOKEINTERFACE else Opcodes.INVOKEVIRTUAL
        }
        bc.emitInvoke(opc, receiverName, jname, mdescr, isInterface)
      }

      bmType.returnType
    } // end of genCallMethod()

    /* Generate the scala ## method. */
    def genScalaHash(tree: Tree): BType = {
      genLoad(tree, ObjectReference)
      genCallMethod(NoSymbol, InvokeStyle.Static) // used to dispatch ## on primitives to ScalaRuntime.hash. Should be implemented by a miniphase
    }

    /*
     * Returns a list of trees that each should be concatenated, from left to right.
     * It turns a chained call like "a".+("b").+("c") into a list of arguments.
     */
    def liftStringConcat(tree: Tree): List[Tree] = tree match {
      case tree @ Apply(fun @ DesugaredSelect(larg, method), rarg) =>
        if (isPrimitive(fun) &&
            primitives.getPrimitive(tree, larg.tpe) == ScalaPrimitivesOps.CONCAT)
          liftStringConcat(larg) ::: rarg
        else
          tree :: Nil
      case _ =>
        tree :: Nil
    }

    /* Emit code to compare the two top-most stack values using the 'op' operator. */
    private def genCJUMP(success: asm.Label, failure: asm.Label, op: TestOp, tk: BType, targetIfNoJump: asm.Label, negated: Boolean = false): Unit = {
      if (targetIfNoJump == success) genCJUMP(failure, success, op.negate(), tk, targetIfNoJump, negated = !negated)
      else {
        if (tk.isIntSizedType) { // BOOL, BYTE, CHAR, SHORT, or INT
          bc.emitIF_ICMP(op, success)
        } else if (tk.isRef) { // REFERENCE(_) | ARRAY(_)
          bc.emitIF_ACMP(op, success)
        } else {
          import Primitives._
          def useCmpG = if (negated) op == GT || op == GE else op == LT || op == LE
          (tk: @unchecked) match {
            case LONG   => emit(asm.Opcodes.LCMP)
            case FLOAT  => emit(if (useCmpG) asm.Opcodes.FCMPG else asm.Opcodes.FCMPL)
            case DOUBLE => emit(if (useCmpG) asm.Opcodes.DCMPG else asm.Opcodes.DCMPL)
          }
          bc.emitIF(op, success)
        }
        if (targetIfNoJump != failure) bc goTo failure
      }
    }

    /* Emits code to compare (and consume) stack-top and zero using the 'op' operator */
    private def genCZJUMP(success: asm.Label, failure: asm.Label, op: TestOp, tk: BType, targetIfNoJump: asm.Label, negated: Boolean = false): Unit = {
      import Primitives._
      if (targetIfNoJump == success) genCZJUMP(failure, success, op.negate(), tk, targetIfNoJump, negated = !negated)
      else {
        if (tk.isIntSizedType) { // BOOL, BYTE, CHAR, SHORT, or INT
          bc.emitIF(op, success)
        } else if (tk.isRef) { // REFERENCE(_) | ARRAY(_)
          (op: @unchecked) match { // references are only compared with EQ and NE
            case EQ => bc emitIFNULL    success
            case NE => bc emitIFNONNULL success
          }
        } else {
          def useCmpG = if (negated) op == GT || op == GE else op == LT || op == LE
          (tk: @unchecked) match {
            case LONG   =>
              emit(asm.Opcodes.LCONST_0)
              emit(asm.Opcodes.LCMP)
            case FLOAT  =>
              emit(asm.Opcodes.FCONST_0)
              emit(if (useCmpG) asm.Opcodes.FCMPG else asm.Opcodes.FCMPL)
            case DOUBLE =>
              emit(asm.Opcodes.DCONST_0)
              emit(if (useCmpG) asm.Opcodes.DCMPG else asm.Opcodes.DCMPL)
          }
          bc.emitIF(op, success)
        }
        if (targetIfNoJump != failure) bc goTo failure
      }
    }

    def testOpForPrimitive(primitiveCode: Int) = (primitiveCode: @switch) match {
       case ScalaPrimitivesOps.ID => Primitives.EQ
       case ScalaPrimitivesOps.NI => Primitives.NE
       case ScalaPrimitivesOps.EQ => Primitives.EQ
       case ScalaPrimitivesOps.NE => Primitives.NE
       case ScalaPrimitivesOps.LT => Primitives.LT
       case ScalaPrimitivesOps.LE => Primitives.LE
       case ScalaPrimitivesOps.GT => Primitives.GT
       case ScalaPrimitivesOps.GE => Primitives.GE
     }

    /*
     * Generate code for conditional expressions.
     * The jump targets success/failure of the test are `then-target` and `else-target` resp.
     */
    private def genCond(tree: Tree, success: asm.Label, failure: asm.Label, targetIfNoJump: asm.Label): Unit = {

      def genComparisonOp(l: Tree, r: Tree, code: Int): Unit = {
        val op = testOpForPrimitive(code)
        def isNull(t: Tree): Boolean = t match {
          case Literal(Constant(null)) => true
          case _ => false
        }
        def ifOneIsNull(l: Tree, r: Tree): Tree = if (isNull(l)) r else if (isNull(r)) l else null
        val nonNullSide = if (ScalaPrimitivesOps.isReferenceEqualityOp(code)) ifOneIsNull(l, r) else null
        if (nonNullSide != null) {
          // special-case reference (in)equality test for null (null eq x, x eq null)
          genLoad(nonNullSide, ObjectReference)
          genCZJUMP(success, failure, op, ObjectReference, targetIfNoJump)
        } else {
          val tk = tpeTK(l).maxType(tpeTK(r))
          genLoad(l, tk)
          genLoad(r, tk)
          genCJUMP(success, failure, op, tk, targetIfNoJump)
        }
      }

      def loadAndTestBoolean() = {
        genLoad(tree, BOOL)
        genCZJUMP(success, failure, Primitives.NE, BOOL, targetIfNoJump)
      }

      lineNumber(tree)
      tree match {

        case tree @ Apply(fun, args) if primitives.isPrimitive(fun.symbol) =>
          import ScalaPrimitivesOps.{ ZNOT, ZAND, ZOR, EQ }

          // lhs and rhs of test
          lazy val DesugaredSelect(lhs, _) = fun
          val rhs = if (args.isEmpty) tpd.EmptyTree else args.head // args.isEmpty only for ZNOT

          def genZandOrZor(and: Boolean): Unit = {
            // reaching "keepGoing" indicates the rhs should be evaluated too (ie not short-circuited).
            val keepGoing = new asm.Label

            if (and) genCond(lhs, keepGoing, failure, targetIfNoJump = keepGoing)
            else     genCond(lhs, success,   keepGoing, targetIfNoJump = keepGoing)

            markProgramPoint(keepGoing)
            genCond(rhs, success, failure, targetIfNoJump)
          }

          primitives.getPrimitive(fun.symbol) match {
            case ZNOT   => genCond(lhs, failure, success, targetIfNoJump)
            case ZAND   => genZandOrZor(and = true)
            case ZOR    => genZandOrZor(and = false)
            case code   =>
              if (ScalaPrimitivesOps.isUniversalEqualityOp(code) && tpeTK(lhs).isClass) {
                // rewrite `==` to null tests and `equals`. not needed for arrays (`equals` is reference equality).
                if (code == EQ) genEqEqPrimitive(lhs, rhs, success, failure, targetIfNoJump)
                else            genEqEqPrimitive(lhs, rhs, failure, success, targetIfNoJump)
              } else if (ScalaPrimitivesOps.isComparisonOp(code)) {
                genComparisonOp(lhs, rhs, code)
              } else
                loadAndTestBoolean()
          }

        case Block(stats, expr) =>
          /* Push the decision further down the `expr`.
           * This is particularly effective for the shape of do..while loops.
           */
          val savedScope = varsInScope
          varsInScope = Nil
          stats foreach genStat
          genCond(expr, success, failure, targetIfNoJump)
          emitLocalVarScopes()
          varsInScope = savedScope

        case If(condp, thenp, elsep) =>
          val innerSuccess = new asm.Label
          val innerFailure = new asm.Label
          genCond(condp, innerSuccess, innerFailure, targetIfNoJump = innerSuccess)
          markProgramPoint(innerSuccess)
          genCond(thenp, success, failure, targetIfNoJump = innerFailure)
          markProgramPoint(innerFailure)
          genCond(elsep, success, failure, targetIfNoJump)

        case _ => loadAndTestBoolean()
      }

    } // end of genCond()

    /*
     * Generate the "==" code for object references. It is equivalent of
     * if (l eq null) r eq null else l.equals(r);
     *
     * @param l       left-hand-side  of the '=='
     * @param r       right-hand-side of the '=='
     */
    def genEqEqPrimitive(l: Tree, r: Tree, success: asm.Label, failure: asm.Label, targetIfNoJump: asm.Label): Unit = {

      /* True if the equality comparison is between values that require the use of the rich equality
       * comparator (scala.runtime.Comparator.equals). This is the case when either side of the
       * comparison might have a run-time type subtype of java.lang.Number or java.lang.Character.
       * When it is statically known that both sides are equal and subtypes of Number of Character,
       * not using the rich equality is possible (their own equals method will do ok.)
       */
      val mustUseAnyComparator: Boolean = {
        val areSameFinals = l.tpe.typeSymbol.is(Final) && r.tpe.typeSymbol.is(Final) && (l.tpe =:= r.tpe)
        // todo: remove
        def isMaybeBoxed(sym: Symbol): Boolean = {
          (sym == defn.ObjectClass) ||
            (sym == defn.JavaSerializableClass) ||
            (sym == defn.ComparableClass) ||
            (sym derivesFrom defn.BoxedNumberClass) ||
            (sym derivesFrom defn.BoxedCharClass) ||
            (sym derivesFrom defn.BoxedBooleanClass)
        }
        !areSameFinals && isMaybeBoxed(l.tpe.typeSymbol) && isMaybeBoxed(r.tpe.typeSymbol)
      }
      def isNull(t: Tree): Boolean = t match {
        case Literal(Constant(null)) => true
        case _ => false
      }
      def isNonNullExpr(t: Tree): Boolean = t.isInstanceOf[Literal] || ((t.symbol ne null) && t.symbol.is(Module))

      if (mustUseAnyComparator) {
        val equalsMethod: Symbol = {
          if (l.tpe <:< defn.BoxedNumberClass.info) {
            if (r.tpe <:< defn.BoxedNumberClass.info) defn.BoxesRunTimeModule.requiredMethod(nme.equalsNumNum)
            else if (r.tpe <:< defn.BoxedCharClass.info) NoSymbol // ctx.requiredMethod(BoxesRunTimeTypeRef, nme.equalsNumChar) // this method is private
            else defn.BoxesRunTimeModule.requiredMethod(nme.equalsNumObject)
          } else defn.BoxesRunTimeModule_externalEquals
        }

        genLoad(l, ObjectReference)
        genLoad(r, ObjectReference)
        genCallMethod(equalsMethod, InvokeStyle.Static)
        genCZJUMP(success, failure, Primitives.NE, BOOL, targetIfNoJump)
      }
      else {
        if (isNull(l)) {
          // null == expr -> expr eq null
          genLoad(r, ObjectReference)
          genCZJUMP(success, failure, Primitives.EQ, ObjectReference, targetIfNoJump)
        } else if (isNull(r)) {
          // expr == null -> expr eq null
          genLoad(l, ObjectReference)
          genCZJUMP(success, failure, Primitives.EQ, ObjectReference, targetIfNoJump)
        } else if (isNonNullExpr(l)) {
          // SI-7852 Avoid null check if L is statically non-null.
          genLoad(l, ObjectReference)
          genLoad(r, ObjectReference)
          genCallMethod(defn.Any_equals, InvokeStyle.Virtual)
          genCZJUMP(success, failure, Primitives.NE, BOOL, targetIfNoJump)
        } else {
          // l == r -> if (l eq null) r eq null else l.equals(r)
          val eqEqTempLocal = locals.makeLocal(ObjectReference, nme.EQEQ_LOCAL_VAR.mangledString, defn.ObjectType, r.span)
          val lNull    = new asm.Label
          val lNonNull = new asm.Label

          genLoad(l, ObjectReference)
          genLoad(r, ObjectReference)
          locals.store(eqEqTempLocal)
          bc dup ObjectReference
          genCZJUMP(lNull, lNonNull, Primitives.EQ, ObjectReference, targetIfNoJump = lNull)

          markProgramPoint(lNull)
          bc drop ObjectReference
          locals.load(eqEqTempLocal)
          genCZJUMP(success, failure, Primitives.EQ, ObjectReference, targetIfNoJump = lNonNull)

          markProgramPoint(lNonNull)
          locals.load(eqEqTempLocal)
          genCallMethod(defn.Any_equals, InvokeStyle.Virtual)
          genCZJUMP(success, failure, Primitives.NE, BOOL, targetIfNoJump)
        }
      }
    }


    def genSynchronized(tree: Apply, expectedType: BType): BType
    def genLoadTry(tree: Try): BType

    def genInvokeDynamicLambda(ctor: Symbol, lambdaTarget: Symbol, environmentSize: Int, functionalInterface: Symbol): BType = {
      import java.lang.invoke.LambdaMetafactory.{FLAG_BRIDGES, FLAG_SERIALIZABLE}

      report.debuglog(s"Using invokedynamic rather than `new ${ctor.owner}`")
      val generatedType = classBTypeFromSymbol(functionalInterface)
      // Lambdas should be serializable if they implement a SAM that extends Serializable or if they
      // implement a scala.Function* class.
      val isSerializable = functionalInterface.isSerializable || defn.isFunctionClass(functionalInterface)
      val isInterface = isEmittedInterface(lambdaTarget.owner)
      val invokeStyle =
        if (lambdaTarget.isStaticMember) asm.Opcodes.H_INVOKESTATIC
        else if (lambdaTarget.is(Private) || lambdaTarget.isClassConstructor) asm.Opcodes.H_INVOKESPECIAL
        else if (isInterface) asm.Opcodes.H_INVOKEINTERFACE
        else asm.Opcodes.H_INVOKEVIRTUAL

      val targetHandle =
        new asm.Handle(invokeStyle,
          classBTypeFromSymbol(lambdaTarget.owner).internalName,
          lambdaTarget.javaSimpleName,
          asmMethodType(lambdaTarget).descriptor,
          /* itf = */ isInterface)

      val (a,b) = lambdaTarget.info.firstParamTypes.splitAt(environmentSize)
      var (capturedParamsTypes, lambdaParamTypes) = (a,b)

      if (invokeStyle != asm.Opcodes.H_INVOKESTATIC) capturedParamsTypes = lambdaTarget.owner.info :: capturedParamsTypes

      // Requires https://github.com/scala/scala-java8-compat on the runtime classpath
      val returnUnit = lambdaTarget.info.resultType.typeSymbol == defn.UnitClass
      val functionalInterfaceDesc: String = generatedType.descriptor
      val desc = capturedParamsTypes.map(tpe => toTypeKind(tpe)).mkString(("("), "", ")") + functionalInterfaceDesc
      // TODO specialization
      val instantiatedMethodType = new MethodBType(lambdaParamTypes.map(p => toTypeKind(p)), toTypeKind(lambdaTarget.info.resultType)).toASMType

      val samMethod = atPhase(erasurePhase) {
        val samMethods = toDenot(functionalInterface).info.possibleSamMethods.toList
        samMethods match {
          case x :: Nil => x.symbol
          case Nil => abort(s"${functionalInterface.show} is not a functional interface. It doesn't have abstract methods")
          case xs => abort(s"${functionalInterface.show} is not a functional interface. " +
            s"It has the following abstract methods: ${xs.map(_.name).mkString(", ")}")
        }
      }

      val methodName = samMethod.javaSimpleName
      val samMethodType = asmMethodType(samMethod).toASMType
      // scala/bug#10334: make sure that a lambda object for `T => U` has a method `apply(T)U`, not only the `(Object)Object`
      // version. Using the lambda a structural type `{def apply(t: T): U}` causes a reflective lookup for this method.
      val needsGenericBridge = samMethodType != instantiatedMethodType
      val bridgeMethods = atPhase(erasurePhase){
        samMethod.allOverriddenSymbols.toList
      }
      val overriddenMethodTypes = bridgeMethods.map(b => asmMethodType(b).toASMType)

      // any methods which `samMethod` overrides need bridges made for them
      // this is done automatically during erasure for classes we generate, but LMF needs to have them explicitly mentioned
      // so we have to compute them at this relatively late point.
      val bridgeTypes = (
        if (needsGenericBridge)
          instantiatedMethodType +: overriddenMethodTypes
        else
          overriddenMethodTypes
      ).distinct.filterNot(_ == samMethodType)

      val needsBridges = bridgeTypes.nonEmpty

      def flagIf(b: Boolean, flag: Int): Int = if (b) flag else 0
      val flags = flagIf(isSerializable, FLAG_SERIALIZABLE) | flagIf(needsBridges, FLAG_BRIDGES)

      val bsmArgs0 = Seq(samMethodType, targetHandle, instantiatedMethodType)
      val bsmArgs1 = if (flags != 0) Seq(Int.box(flags)) else Seq.empty
      val bsmArgs2 = if needsBridges then bridgeTypes.length +: bridgeTypes else Seq.empty

      val bsmArgs = bsmArgs0 ++ bsmArgs1 ++ bsmArgs2

      val metafactory =
        if (flags != 0)
          lambdaMetaFactoryAltMetafactoryHandle // altMetafactory required to be able to pass the flags and additional arguments if needed
        else
          lambdaMetaFactoryMetafactoryHandle

      bc.jmethod.visitInvokeDynamicInsn(methodName, desc, metafactory, bsmArgs: _*)

      generatedType
    }
  }

  /** Does this symbol actually correspond to an interface that will be emitted?
   *  In the backend, this should be preferred over `isInterface` because it
   *  also returns true for the symbols of the fake companion objects we
   *  create for Java-defined classes as well as for Java annotations
   *  which we represent as classes.
   */
  private def isEmittedInterface(sym: Symbol): Boolean = sym.isInterface ||
    sym.is(JavaDefined) && (toDenot(sym).isAnnotation || sym.is(ModuleClass) && (sym.companionClass.is(PureInterface)) || sym.companionClass.is(Trait))

}

object BCodeBodyBuilder {
  val lambdaMetaFactoryMetafactoryHandle = new Handle(
    Opcodes.H_INVOKESTATIC,
    "java/lang/invoke/LambdaMetafactory",
    "metafactory",
    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;",
    /* itf = */ false)

  val lambdaMetaFactoryAltMetafactoryHandle = new Handle(
    Opcodes.H_INVOKESTATIC,
    "java/lang/invoke/LambdaMetafactory",
    "altMetafactory",
    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;",
    /* itf = */ false)

  val lambdaDeserializeBootstrapHandle = new Handle(
    Opcodes.H_INVOKESTATIC,
    "scala/runtime/LambdaDeserialize",
    "bootstrap",
    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/invoke/MethodHandle;)Ljava/lang/invoke/CallSite;",
    /* itf = */ false)

  val caseClassMethodsBootstrapHandle = new Handle(
    Opcodes.H_INVOKESTATIC,
    "scala/runtime/CaseClassMethods",
    "bootstrap",
    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/Class;[Ljava/lang/invoke/MethodHandle;)Ljava/lang/invoke/ConstantCallSite;",
    /* itf = */ false)

}
