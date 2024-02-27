package dotty.tools
package dotc
package transform
package init

import core.*
import Contexts.*
import Symbols.*
import Types.*
import Denotations.*, SymDenotations.*
import StdNames.*
import Names.TermName
import NameKinds.OuterSelectName
import NameKinds.SuperAccessorName

import ast.tpd.*
import util.{ SourcePosition, NoSourcePosition }
import config.Printers.init as printer
import reporting.StoreReporter
import reporting.trace as log
import typer.Applications.*

import Errors.*
import Trace.*
import Util.*

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.annotation.tailrec
import scala.annotation.constructorOnly

/** Check initialization safety of static objects
 *
 *  The problem is illustrated by the example below:
 *
 *      class Foo(val opposite: Foo)
 *      case object A extends Foo(B)     // A -> B
 *      case object B extends Foo(A)     // B -> A
 *
 *  In the code above, the initialization of object `A` depends on `B` and vice versa. There is no
 *  correct way to initialize the code above. The current checker issues a warning for the code
 *  above.
 *
 *  At the high-level, the analysis has the following characteristics:
 *
 *  1. The check enforces the principle of "initialization-time irrelevance", which means that the
 *     time when an object is initialized should not change program semantics. For that purpose, it
 *     enforces the following rule:
 *
 *         The initialization of a static object should not directly or indirectly read or write
 *         mutable state of another static object.
 *
 *     This principle not only put initialization of static objects on a solid foundation, but also
 *     avoids whole-program analysis.
 *
 *  2. The design is based on the concept of "cold aliasing" --- a cold alias may not be actively
 *     used during initialization, i.e., it's forbidden to call methods or access fields of a cold
 *     alias. Method arguments are cold aliases by default unless specified to be sensitive. Method
 *     parameters captured in lambdas or inner classes are always cold aliases.
 *
 *  3. It is inter-procedural and flow-sensitive.
 *
 *  4. It is object-sensitive by default and parameter-sensitive on-demand.
 *
 *  5. The check is modular in the sense that each object is checked separately and there is no
 *     whole-program analysis. However, the check is not modular in terms of project boundaries.
 *
 */
object Objects:

  // ----------------------------- abstract domain -----------------------------

  /** Syntax for the data structure abstraction used in abstract domain:
   *
   * ve ::= ObjectRef(class)                                             // global object
   *      | OfClass(class, vs[outer], ctor, args, env)                   // instance of a class
   *      | OfArray(object[owner], regions)
   *      | Fun(..., env)                                                // value elements that can be contained in ValueSet
   * vs ::= ValueSet(ve)                                                 // set of abstract values
   * Bottom ::= ValueSet(Empty)
   * val ::= ve | Cold | vs                                              // all possible abstract values in domain
   * Ref ::= ObjectRef | OfClass                                         // values that represent a reference to some (global or instance) object
   * ThisValue ::= Ref | Cold                                            // possible values for 'this'
   *
   * refMap = Ref -> ( valsMap, varsMap, outersMap )                     // refMap stores field informations of an object or instance
   * valsMap = valsym -> val                                             // maps immutable fields to their values
   * varsMap = valsym -> addr                                            // each mutable field has an abstract address
   * outersMap = class -> val                                            // maps outer objects to their values
   *
   * arrayMap = OfArray -> addr                                          // an array has one address that stores the join value of every element
   *
   * heap = addr -> val                                                  // heap is mutable
   *
   * env = (valsMap, Option[env])                                        // stores local variables in the residing method, and possibly outer environments
   *
   * addr ::= localVarAddr(regions, valsym, owner)
   *        | fieldVarAddr(regions, valsym, owner)                       // independent of OfClass/ObjectRef
   *        | arrayAddr(regions, owner)                                  // independent of array element type
   *
   * regions ::= List(sourcePosition)
   */

  sealed abstract class Value:
    def show(using Context): String

  /** ValueElement are elements that can be contained in a RefSet */
  sealed abstract class ValueElement extends Value

  /**
   * A reference caches the values for outers and immutable fields.
   */
  sealed abstract class Ref(
    valsMap: mutable.Map[Symbol, Value],
    varsMap: mutable.Map[Symbol, Heap.Addr],
    outersMap: mutable.Map[ClassSymbol, Value])
  extends ValueElement:
    protected val vals: mutable.Map[Symbol, Value] = valsMap
    protected val vars: mutable.Map[Symbol, Heap.Addr] = varsMap
    protected val outers: mutable.Map[ClassSymbol, Value] = outersMap

    def isObjectRef: Boolean = this.isInstanceOf[ObjectRef]

    def klass: ClassSymbol

    def valValue(sym: Symbol): Value = vals(sym)

    def varAddr(sym: Symbol): Heap.Addr = vars(sym)

    def outerValue(cls: ClassSymbol): Value = outers(cls)

    def hasVal(sym: Symbol): Boolean = vals.contains(sym)

    def hasVar(sym: Symbol): Boolean = vars.contains(sym)

    def hasOuter(cls: ClassSymbol): Boolean = outers.contains(cls)

    def initVal(field: Symbol, value: Value)(using Context) = log("Initialize " + field.show + " = " + value + " for " + this, printer) {
      assert(!field.is(Flags.Mutable), "Field is mutable: " + field.show)
      assert(!vals.contains(field), "Field already set: " + field.show)
      vals(field) = value
    }

    def initVar(field: Symbol, addr: Heap.Addr)(using Context) = log("Initialize " + field.show + " = " + addr + " for " + this, printer) {
      assert(field.is(Flags.Mutable), "Field is not mutable: " + field.show)
      assert(!vars.contains(field), "Field already set: " + field.show)
      vars(field) = addr
    }

    def initOuter(cls: ClassSymbol, value: Value)(using Context) = log("Initialize outer " + cls.show + " = " + value + " for " + this, printer) {
      assert(!outers.contains(cls), "Outer already set: " + cls)
      outers(cls) = value
    }

  /** A reference to a static object */
  case class ObjectRef(klass: ClassSymbol)
  extends Ref(valsMap = mutable.Map.empty, varsMap = mutable.Map.empty, outersMap = mutable.Map.empty):
    val owner = klass

    def show(using Context) = "ObjectRef(" + klass.show + ")"

  /**
   * Represents values that are instances of the specified class.
   *
   * Note that the 2nd parameter block does not take part in the definition of equality.
   */
  case class OfClass private (
    klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value], env: Env.Data)(
    valsMap: mutable.Map[Symbol, Value], varsMap: mutable.Map[Symbol, Heap.Addr], outersMap: mutable.Map[ClassSymbol, Value])
  extends Ref(valsMap, varsMap, outersMap):
    def widenedCopy(outer: Value, args: List[Value], env: Env.Data): OfClass =
      new OfClass(klass, outer, ctor, args, env)(this.valsMap, this.varsMap, this.outersMap)

    def show(using Context) =
      val valFields = vals.map(_.show +  " -> " +  _.show)
      "OfClass(" + klass.show + ", outer = " + outer + ", args = " + args.map(_.show) + ", vals = " + valFields + ")"

  object OfClass:
    def apply(
      klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value], env: Env.Data)(
      using Context
    ): OfClass =
      val instance = new OfClass(klass, outer, ctor, args, env)(
        valsMap = mutable.Map.empty, varsMap = mutable.Map.empty, outersMap = mutable.Map.empty
      )
      instance.initOuter(klass, outer)
      instance

  /**
   * Represents arrays.
   *
   * Note that the 2nd parameter block does not take part in the definition of equality.
   *
   * Different arrays are distinguished by the context. Currently the default context is the static
   * object whose initialization triggers the creation of the array.
   *
   * In the future, it is possible that we introduce a mechanism for end-users to mark the context.
   *
   * @param owner The static object whose initialization creates the array.
   */
  case class OfArray(owner: ClassSymbol, regions: Regions.Data)(using @constructorOnly ctx: Context) extends ValueElement:
    val klass: ClassSymbol = defn.ArrayClass
    val addr: Heap.Addr = Heap.arrayAddr(regions, owner)
    def show(using Context) = "OfArray(owner = " + owner.show + ")"

  /**
   * Represents a lambda expression
   */
  case class Fun(code: Tree, thisV: ThisValue, klass: ClassSymbol, env: Env.Data) extends ValueElement:
    def show(using Context) = "Fun(" + code.show + ", " + thisV.show + ", " + klass.show + ")"

  /**
   * Represents a set of values
   *
   * It comes from `if` expressions.
   */
  case class ValueSet(values: ListSet[ValueElement]) extends Value:
    def show(using Context) = values.map(_.show).mkString("[", ",", "]")

  /** A cold alias which should not be used during initialization.
   *
   *  Cold is not ValueElement since RefSet containing Cold is equivalent to Cold
   */
  case object Cold extends Value:
    def show(using Context) = "Cold"

  val Bottom = ValueSet(ListSet.empty)

  /** Possible types for 'this' */
  type ThisValue = Ref | Cold.type

  /** Checking state  */
  object State:
    class Data:
      // objects under check
      private[State] val checkingObjects = new mutable.ArrayBuffer[ObjectRef]
      private[State] val checkedObjects = new mutable.ArrayBuffer[ObjectRef]
      private[State] val pendingTraces = new mutable.ArrayBuffer[Trace]
    end Data

    def currentObject(using data: Data): ClassSymbol = data.checkingObjects.last.klass

    private def doCheckObject(classSym: ClassSymbol)(using ctx: Context, data: Data) =
      val tpl = classSym.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]

      var count = 0
      given Cache.Data = new Cache.Data

      @tailrec
      def iterate()(using Context): ObjectRef =
        count += 1

        given Trace = Trace.empty.add(classSym.defTree)
        given Env.Data = Env.emptyEnv(tpl.constr.symbol)
        given Heap.MutableData = Heap.empty()
        given returns: Returns.Data = Returns.empty()
        given regions: Regions.Data = Regions.empty // explicit name to avoid naming conflict

        val obj = ObjectRef(classSym)
        log("Iteration " + count) {
          data.checkingObjects += obj
          init(tpl, obj, classSym)
          assert(data.checkingObjects.last.klass == classSym, "Expect = " + classSym.show + ", found = " + data.checkingObjects.last.klass)
          data.checkingObjects.remove(data.checkingObjects.size - 1)
        }

        val hasError = ctx.reporter.pendingMessages.nonEmpty
        if cache.hasChanged && !hasError then
          cache.prepareForNextIteration()
          iterate()
        else
          data.checkedObjects += obj
          obj
      end iterate

      val reporter = new StoreReporter(ctx.reporter)
      val obj = iterate()(using ctx.fresh.setReporter(reporter))
      for warning <- reporter.pendingMessages do
        ctx.reporter.report(warning)

      obj
    end doCheckObject

    def checkObjectAccess(clazz: ClassSymbol)(using data: Data, ctx: Context, pendingTrace: Trace): ObjectRef =
      val index = data.checkingObjects.indexOf(ObjectRef(clazz))

      if index != -1 then
        if data.checkingObjects.size - 1 > index then
          // Only report errors for non-trivial cycles, ignore self cycles.
          val joinedTrace = data.pendingTraces.slice(index + 1, data.checkingObjects.size).foldLeft(pendingTrace) { (a, acc) => acc ++ a }
          val callTrace = Trace.buildStacktrace(joinedTrace, "Calling trace:\n")
          val cycle = data.checkingObjects.slice(index, data.checkingObjects.size)
          val pos = clazz.defTree.sourcePos.focus
          report.warning("Cyclic initialization: " + cycle.map(_.klass.show).mkString(" -> ") + " -> " + clazz.show + ". " + callTrace, pos)
        end if
        data.checkingObjects(index)
      else
        val objOpt = data.checkedObjects.find(_.klass == clazz)
        objOpt match
        case Some(obj) => obj

        case None =>
          data.pendingTraces += pendingTrace
          val obj = doCheckObject(clazz)
          data.pendingTraces.remove(data.pendingTraces.size - 1)
          obj
    end checkObjectAccess
  end State

  /** Environment for parameters */
  object Env:
    abstract class Data:
      private[Env] def getVal(x: Symbol)(using Context): Option[Value]
      private[Env] def getVar(x: Symbol)(using Context): Option[Heap.Addr]

      def widen(height: Int)(using Context): Data

      def level: Int

      def show(using Context): String

    /** Local environments can be deeply nested, therefore we need `outer`.
     *
     *  For local variables in rhs of class field definitions, the `meth` is the primary constructor.
     */
    private case class LocalEnv
      (private[Env] val params: Map[Symbol, Value], meth: Symbol, outer: Data)
      (valsMap: mutable.Map[Symbol, Value], varsMap: mutable.Map[Symbol, Heap.Addr])
      (using Context)
    extends Data:
      val level = outer.level + 1

      if (level > 3)
        report.warning("[Internal error] Deeply nested environment, level =  " + level + ", " + meth.show + " in " + meth.enclosingClass.show, meth.defTree)

      private[Env] val vals: mutable.Map[Symbol, Value] = valsMap
      private[Env] val vars: mutable.Map[Symbol, Heap.Addr] = varsMap

      private[Env] def getVal(x: Symbol)(using Context): Option[Value] =
        if x.is(Flags.Param) then params.get(x)
        else vals.get(x)

      private[Env] def getVar(x: Symbol)(using Context): Option[Heap.Addr] =
        vars.get(x)

      def widen(height: Int)(using Context): Data =
        new LocalEnv(params.map(_ -> _.widen(height)), meth, outer.widen(height))(this.vals, this.vars)

      def show(using Context) =
        "owner: " + meth.show + "\n" +
        "params: " + params.map(_.show + " ->" + _.show).mkString("{", ", ", "}") + "\n" +
        "vals: " + vals.map(_.show + " ->" + _.show).mkString("{", ", ", "}") + "\n" +
        "vars: " + vars.map(_.show + " ->" + _).mkString("{", ", ", "}") + "\n" +
        "outer = {\n" + outer.show + "\n}"

    end LocalEnv

    object NoEnv extends Data:
      val level = 0

      private[Env] def getVal(x: Symbol)(using Context): Option[Value] =
        throw new RuntimeException("Invalid usage of non-existent env")

      private[Env] def getVar(x: Symbol)(using Context): Option[Heap.Addr] =
        throw new RuntimeException("Invalid usage of non-existent env")

      def widen(height: Int)(using Context): Data = this

      def show(using Context): String = "NoEnv"
    end NoEnv

    /** An empty environment can be used for non-method environments, e.g., field initializers.
     *
     *  The owner for the local environment for field initializers is the primary constructor of the
     *  enclosing class.
     */
    def emptyEnv(meth: Symbol)(using Context): Data =
      new LocalEnv(Map.empty, meth, NoEnv)(valsMap = mutable.Map.empty, varsMap = mutable.Map.empty)

    def valValue(x: Symbol)(using data: Data, ctx: Context, trace: Trace): Value =
      data.getVal(x) match
      case Some(theValue) =>
        theValue
      case _ =>
        report.warning("[Internal error] Value not found " + x.show + "\nenv = " + data.show + ". " + Trace.show, Trace.position)
        Bottom

    def getVal(x: Symbol)(using data: Data, ctx: Context): Option[Value] = data.getVal(x)

    def getVar(x: Symbol)(using data: Data, ctx: Context): Option[Heap.Addr] = data.getVar(x)

    def of(ddef: DefDef, args: List[Value], outer: Data)(using Context): Data =
      val params = ddef.termParamss.flatten.map(_.symbol)
      assert(args.size == params.size, "arguments = " + args.size + ", params = " + params.size)
      assert(ddef.symbol.owner.isClass ^ (outer != NoEnv), "ddef.owner = " + ddef.symbol.owner.show + ", outer = " + outer + ", " + ddef.source)
      new LocalEnv(params.zip(args).toMap, ddef.symbol, outer)(valsMap = mutable.Map.empty, varsMap = mutable.Map.empty)

    def setLocalVal(x: Symbol, value: Value)(using data: Data, ctx: Context): Unit =
      assert(!x.isOneOf(Flags.Param | Flags.Mutable), "Only local immutable variable allowed")
      data match
      case localEnv: LocalEnv =>
        assert(!localEnv.vals.contains(x), "Already initialized local " + x.show)
        localEnv.vals(x) = value
      case _ =>
        throw new RuntimeException("Incorrect local environment for initializing " + x.show)

    def setLocalVar(x: Symbol, addr: Heap.Addr)(using data: Data, ctx: Context): Unit =
      assert(x.is(Flags.Mutable, butNot = Flags.Param), "Only local mutable variable allowed")
      data match
      case localEnv: LocalEnv =>
        assert(!localEnv.vars.contains(x), "Already initialized local " + x.show)
        localEnv.vars(x) = addr
      case _ =>
        throw new RuntimeException("Incorrect local environment for initializing " + x.show)

    /**
     * Resolve the environment owned by the given method.
     *
     * The method could be located in outer scope with intermixed classes between its definition
     * site and usage site.
     *
     * Due to widening, the corresponding environment might not exist. As a result reading the local
     * variable will return `Cold` and it's forbidden to write to the local variable.
     *
     * @param meth  The method which owns the environment
     * @param thisV The value for `this` of the enclosing class where the local variable is referenced.
     * @param env   The local environment where the local variable is referenced.
     *
     * @return the environment and value for `this` owned by the given method.
     */
    def resolveEnv(meth: Symbol, thisV: ThisValue, env: Data)(using Context): Option[(ThisValue, Data)] = log("Resolving env for " + meth.show + ", this = " + thisV.show + ", env = " + env.show, printer) {
      env match
      case localEnv: LocalEnv =>
        if localEnv.meth == meth then Some(thisV -> env)
        else resolveEnv(meth, thisV, localEnv.outer)
      case NoEnv =>
        thisV match
        case ref: OfClass =>
          ref.outer match
          case outer : ThisValue =>
            resolveEnv(meth, outer, ref.env)
          case _ =>
            // TODO: properly handle the case where ref.outer is ValueSet
            None
        case _ =>
          None
    }

    def withEnv[T](env: Data)(fn: Data ?=> T): T = fn(using env)
  end Env

  /** Abstract heap for mutable fields
   */
  object Heap:
    abstract class Addr:
      /** The static object which owns the mutable slot */
      def owner: ClassSymbol

    /** The address for mutable fields of objects. */
    private case class FieldAddr(regions: Regions.Data, field: Symbol, owner: ClassSymbol) extends Addr

    /** The address for mutable local variables . */
    private case class LocalVarAddr(regions: Regions.Data, sym: Symbol, owner: ClassSymbol) extends Addr

    /** Immutable heap data used in the cache.
     *
     *  We need to use structural equivalence so that in different iterations the cache can be effective.
     *
     *  TODO: speed up equality check for heap.
     */
    opaque type Data = Map[Addr, Value]

    /** Store the heap as a mutable field to avoid threading it through the program. */
    class MutableData(private[Heap] var heap: Data):
      private[Heap] def writeJoin(addr: Addr, value: Value): Unit =
        heap.get(addr) match
        case None =>
          heap = heap.updated(addr, value)

        case Some(current) =>
          val value2 = value.join(current)
          if value2 != current then
            heap = heap.updated(addr, value2)
    end MutableData

    def empty(): MutableData = new MutableData(Map.empty)

    def contains(addr: Addr)(using mutable: MutableData): Boolean =
      mutable.heap.contains(addr)

    def read(addr: Addr)(using mutable: MutableData): Value =
      mutable.heap(addr)

    def writeJoin(addr: Addr, value: Value)(using mutable: MutableData): Unit =
      mutable.writeJoin(addr, value)

    def localVarAddr(regions: Regions.Data, sym: Symbol, owner: ClassSymbol): Addr =
      LocalVarAddr(regions, sym, owner)

    def fieldVarAddr(regions: Regions.Data, sym: Symbol, owner: ClassSymbol): Addr =
      FieldAddr(regions, sym, owner)

    def arrayAddr(regions: Regions.Data, owner: ClassSymbol)(using Context): Addr =
      FieldAddr(regions, defn.ArrayClass, owner)

    def getHeapData()(using mutable: MutableData): Data = mutable.heap

  /** Cache used to terminate the check  */
  object Cache:
    case class Config(thisV: Value, env: Env.Data, heap: Heap.Data)
    case class Res(value: Value, heap: Heap.Data)

    class Data extends Cache[Config, Res]:
      def get(thisV: Value, expr: Tree)(using Heap.MutableData, Env.Data): Option[Value] =
        val config = Config(thisV, summon[Env.Data], Heap.getHeapData())
        super.get(config, expr).map(_.value)

      def cachedEval(thisV: ThisValue, expr: Tree, cacheResult: Boolean)(fun: Tree => Value)(using Heap.MutableData, Env.Data): Value =
        val config = Config(thisV, summon[Env.Data], Heap.getHeapData())
        val result = super.cachedEval(config, expr, cacheResult, default = Res(Bottom, Heap.getHeapData())) { expr =>
          Res(fun(expr), Heap.getHeapData())
        }
        result.value
  end Cache

  /**
   * Region context for mutable states
   *
   * By default, the region context is empty.
   */
  object Regions:
    opaque type Data = List[SourcePosition]
    val empty: Data = Nil
    def extend(pos: SourcePosition)(using data: Data): Data = pos :: data
    def exists(pos: SourcePosition)(using data: Data): Boolean = data.indexOf(pos) >= 0
    def show(using data: Data, ctx: Context): String = data.map(_.show).mkString("[", ", ", "]")

  inline def cache(using c: Cache.Data): Cache.Data = c


  /**
   * Handle return statements in methods and non-local returns in functions.
   */
  object Returns:
    private class ReturnData(val method: Symbol, val values: mutable.ArrayBuffer[Value])
    opaque type Data = mutable.ArrayBuffer[ReturnData]

    def empty(): Data = mutable.ArrayBuffer()

    def installHandler(meth: Symbol)(using data: Data): Unit =
      data.addOne(ReturnData(meth, mutable.ArrayBuffer()))

    def popHandler(meth: Symbol)(using data: Data): Value =
      val returnData = data.remove(data.size - 1)
      assert(returnData.method == meth, "Symbol mismatch in return handlers, expect = " + meth + ", found = " + returnData.method)
      returnData.values.join

    def handle(meth: Symbol, value: Value)(using data: Data, trace: Trace, ctx: Context): Unit =
      data.findLast(_.method == meth) match
        case Some(returnData) =>
          returnData.values.addOne(value)

        case None =>
          report.warning("[Internal error] Unhandled return for method " + meth + " in " + meth.owner.show + ". Trace:\n" + Trace.show, Trace.position)

  type Contextual[T] = (Context, State.Data, Env.Data, Cache.Data, Heap.MutableData, Regions.Data, Returns.Data, Trace) ?=> T

  // --------------------------- domain operations -----------------------------

  case class ArgInfo(value: Value, trace: Trace, tree: Tree)

  extension (a: Value)
    def join(b: Value): Value =
      (a, b) match
      case (Cold, _)                              => Cold
      case (_, Cold)                              => Cold
      case (Bottom, b)                            => b
      case (a, Bottom)                            => a
      case (ValueSet(values1), ValueSet(values2)) => ValueSet(values1 ++ values2)
      case (a : ValueElement, ValueSet(values))   => ValueSet(values + a)
      case (ValueSet(values), b : ValueElement)   => ValueSet(values + b)
      case (a : ValueElement, b : ValueElement)   => ValueSet(ListSet(a, b))

    def widen(height: Int)(using Context): Value =
      if height == 0 then Cold
      else
        a match
          case Bottom => Bottom

          case ValueSet(values) =>
            values.map(ref => ref.widen(height)).join

          case Fun(code, thisV, klass, env) =>
            Fun(code, thisV.widenRefOrCold(height), klass, env.widen(height - 1))

          case ref @ OfClass(klass, outer, _, args, env) =>
            val outer2 = outer.widen(height - 1)
            val args2 = args.map(_.widen(height - 1))
            val env2 = env.widen(height - 1)
            ref.widenedCopy(outer2, args2, env2)

          case _ => a

  extension (value: Ref | Cold.type)
    def widenRefOrCold(height : Int)(using Context) : Ref | Cold.type = value.widen(height).asInstanceOf[ThisValue]

  extension (values: Iterable[Value])
    def join: Value = if values.isEmpty then Bottom else values.reduce { (v1, v2) => v1.join(v2) }

    def widen(height: Int): Contextual[List[Value]] = values.map(_.widen(height)).toList

  /** Handle method calls `e.m(args)`.
   *
   * @param value        The value for the receiver.
   * @param meth         The symbol of the target method (could be virtual or abstract method).
   * @param args         Arguments of the method call (all parameter blocks flatten to a list).
   * @param receiver     The type of the receiver.
   * @param superType    The type of the super in a super call. NoType for non-super calls.
   * @param needResolve  Whether the target of the call needs resolution?
   */
  def call(value: Value, meth: Symbol, args: List[ArgInfo], receiver: Type, superType: Type, needResolve: Boolean = true): Contextual[Value] = log("call " + meth.show + ", this = " + value.show + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
    value match
    case Cold =>
      report.warning("Using cold alias. " + Trace.show, Trace.position)
      Bottom

    case Bottom =>
      Bottom

    case arr: OfArray =>
      val target = resolve(defn.ArrayClass, meth)

      if target == defn.Array_apply || target == defn.Array_clone then
        if arr.addr.owner == State.currentObject then
          Heap.read(arr.addr)
        else
          errorReadOtherStaticObject(State.currentObject, arr.addr.owner)
          Bottom
      else if target == defn.Array_update then
        assert(args.size == 2, "Incorrect number of arguments for Array update, found = " + args.size)
        if arr.addr.owner != State.currentObject then
          errorMutateOtherStaticObject(State.currentObject, arr.addr.owner)
        else
          Heap.writeJoin(arr.addr, args.tail.head.value)
        Bottom
      else
        // Array.length is OK
        Bottom

    case ref: Ref =>
      val isLocal = !meth.owner.isClass
      val target =
        if !needResolve then
          meth
        else if superType.exists then
          meth
        else if meth.name.is(SuperAccessorName) then
          ResolveSuper.rebindSuper(ref.klass, meth)
        else
          resolve(ref.klass, meth)

      if target.isOneOf(Flags.Method) then
        if target.owner == defn.ArrayModuleClass && target.name == nme.apply then
          val arr = OfArray(State.currentObject, summon[Regions.Data])
          Heap.writeJoin(arr.addr, args.map(_.value).join)
          arr
        else if target.hasSource then
          val cls = target.owner.enclosingClass.asClass
          val ddef = target.defTree.asInstanceOf[DefDef]
          val meth = ddef.symbol

          val (thisV : ThisValue, outerEnv) =
            if meth.owner.isClass then
              (ref, Env.NoEnv)
            else
              Env.resolveEnv(meth.owner.enclosingMethod, ref, summon[Env.Data]).getOrElse(Cold -> Env.NoEnv)

          val env2 = Env.of(ddef, args.map(_.value), outerEnv)
          extendTrace(ddef) {
            given Env.Data = env2
            cache.cachedEval(ref, ddef.rhs, cacheResult = true) { expr =>
              Returns.installHandler(meth)
              val res = cases(expr, thisV, cls)
              val returns = Returns.popHandler(meth)
              res.join(returns)
            }
          }
        else
          Bottom
      else if target.exists then
        select(ref, target, receiver, needResolve = false)
      else
        if ref.klass.isSubClass(receiver.widenSingleton.classSymbol) then
          report.warning("[Internal error] Unexpected resolution failure: ref.klass = " + ref.klass.show + ", meth = " + meth.show + Trace.show, Trace.position)
          Bottom
        else
          // This is possible due to incorrect type cast.
          // See tests/init/pos/Type.scala
          Bottom

    case Fun(code, thisV, klass, env) =>
      // meth == NoSymbol for poly functions
      if meth.name == nme.tupled then
        value // a call like `fun.tupled`
      else
        code match
        case ddef: DefDef =>
          if meth.name == nme.apply then
            given Env.Data = Env.of(ddef, args.map(_.value), env)
            extendTrace(code) { eval(ddef.rhs, thisV, klass, cacheResult = true) }
          else
            // The methods defined in `Any` and `AnyRef` are trivial and don't affect initialization.
            if meth.owner == defn.AnyClass || meth.owner == defn.ObjectClass then
              value
            else
              // In future, we will have Tasty for stdlib classes and can abstractly interpret that Tasty.
              // For now, return `Cold` to ensure soundness and trigger a warning.
              Cold
            end if
          end if

        case _ =>
          // by-name closure
          given Env.Data = env
          extendTrace(code) { eval(code, thisV, klass, cacheResult = true) }

    case ValueSet(vs) =>
      vs.map(v => call(v, meth, args, receiver, superType)).join
  }

  /** Handle constructor calls `<init>(args)`.
   *
   * @param value        The value for the receiver.
   * @param ctor         The symbol of the target method.
   * @param args         Arguments of the constructor call (all parameter blocks flatten to a list).
   */
  def callConstructor(value: Value, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = log("call " + ctor.show + ", args = " + args.map(_.value.show), printer, (_: Value).show) {

    value match
    case ref: Ref =>
      if ctor.hasSource then
        val cls = ctor.owner.enclosingClass.asClass
        val ddef = ctor.defTree.asInstanceOf[DefDef]
        val argValues = args.map(_.value)

        given Env.Data = Env.of(ddef, argValues, Env.NoEnv)
        if ctor.isPrimaryConstructor then
          val tpl = cls.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
          extendTrace(cls.defTree) { eval(tpl, ref, cls, cacheResult = true) }
        else
          extendTrace(ddef) { // The return values for secondary constructors can be ignored
            Returns.installHandler(ctor)
            eval(ddef.rhs, ref, cls, cacheResult = true)
            Returns.popHandler(ctor)
          }
      else
        // no source code available
        Bottom

    case _ =>
      report.warning("[Internal error] unexpected constructor call, meth = " + ctor + ", this = " + value + Trace.show, Trace.position)
      Bottom
  }

  /** Handle selection `e.f`.
   *
   * @param value        The value for the receiver.
   * @param field        The symbol of the target field (could be virtual or abstract).
   * @param receiver     The type of the receiver.
   * @param needResolve  Whether the target of the selection needs resolution?
   */
  def select(value: Value, field: Symbol, receiver: Type, needResolve: Boolean = true): Contextual[Value] = log("select " + field.show + ", this = " + value.show, printer, (_: Value).show) {
    value match
    case Cold =>
      report.warning("Using cold alias", Trace.position)
      Bottom

    case ref: Ref =>
      val target = if needResolve then resolve(ref.klass, field) else field
      if target.is(Flags.Lazy) then
        given Env.Data = Env.emptyEnv(target.owner.asInstanceOf[ClassSymbol].primaryConstructor)
        if target.hasSource then
          val rhs = target.defTree.asInstanceOf[ValDef].rhs
          eval(rhs, ref, target.owner.asClass, cacheResult = true)
        else
          Bottom
      else if target.exists then
        if target.isOneOf(Flags.Mutable) then
          if ref.hasVar(target) then
            val addr = ref.varAddr(target)
            if addr.owner == State.currentObject then
              Heap.read(addr)
            else
              errorReadOtherStaticObject(State.currentObject, addr.owner)
              Bottom
          else if ref.isObjectRef && ref.klass.hasSource then
            report.warning("Access uninitialized field " + field.show + ". " + Trace.show, Trace.position)
            Bottom
          else
            // initialization error, reported by the initialization checker
            Bottom
        else if ref.hasVal(target) then
          ref.valValue(target)
        else if ref.isObjectRef && ref.klass.hasSource then
          report.warning("Access uninitialized field " + field.show + ". " + Trace.show, Trace.position)
          Bottom
        else
          // initialization error, reported by the initialization checker
          Bottom

      else
        if ref.klass.isSubClass(receiver.widenSingleton.classSymbol) then
          report.warning("[Internal error] Unexpected resolution failure: ref.klass = " + ref.klass.show + ", field = " + field.show + Trace.show, Trace.position)
          Bottom
        else
          // This is possible due to incorrect type cast.
          // See tests/init/pos/Type.scala
          Bottom

    case fun: Fun =>
      report.warning("[Internal error] unexpected tree in selecting a function, fun = " + fun.code.show + Trace.show, fun.code)
      Bottom

    case arr: OfArray =>
      report.warning("[Internal error] unexpected tree in selecting an array, array = " + arr.show + Trace.show, Trace.position)
      Bottom

    case Bottom =>
      if field.isStaticObject then ObjectRef(field.moduleClass.asClass)
      else Bottom

    case ValueSet(values) =>
      values.map(ref => select(ref, field, receiver)).join
  }

  /** Handle assignment `lhs.f = rhs`.
   *
   * @param lhs         The value of the object to be mutated.
   * @param field       The symbol of the target field.
   * @param rhs         The value to be assigned.
   * @param rhsTyp      The type of the right-hand side.
   */
  def assign(lhs: Value, field: Symbol, rhs: Value, rhsTyp: Type): Contextual[Value] = log("Assign" + field.show + " of " + lhs.show + ", rhs = " + rhs.show, printer, (_: Value).show) {
    lhs match
    case fun: Fun =>
      report.warning("[Internal error] unexpected tree in assignment, fun = " + fun.code.show + Trace.show, Trace.position)

    case arr: OfArray =>
      report.warning("[Internal error] unexpected tree in assignment, array = " + arr.show + Trace.show, Trace.position)

    case Cold =>
      report.warning("Assigning to cold aliases is forbidden. " + Trace.show, Trace.position)

    case Bottom =>

    case ValueSet(values) =>
      values.foreach(ref => assign(ref, field, rhs, rhsTyp))

    case ref: Ref =>
      if ref.hasVar(field) then
        val addr = ref.varAddr(field)
        if addr.owner != State.currentObject then
          errorMutateOtherStaticObject(State.currentObject, addr.owner)
        else
          Heap.writeJoin(addr, rhs)
      else
        report.warning("Mutating a field before its initialization: " + field.show + ". " + Trace.show, Trace.position)
    end match

    Bottom
  }

  /** Handle new expression `new p.C(args)`.
   *
   * @param outer       The value for `p`.
   * @param klass       The symbol of the class `C`.
   * @param ctor        The symbol of the target constructor.
   * @param args        The arguments passsed to the constructor.
   */
  def instantiate(outer: Value, klass: ClassSymbol, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = log("instantiating " + klass.show + ", outer = " + outer + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
    outer match

    case _ : Fun | _: OfArray  =>
      report.warning("[Internal error] unexpected outer in instantiating a class, outer = " + outer.show + ", class = " + klass.show + ", " + Trace.show, Trace.position)
      Bottom

    case outer: (Ref | Cold.type | Bottom.type) =>
      if klass == defn.ArrayClass then
        args.head.tree.tpe match
          case ConstantType(Constants.Constant(0)) =>
            // new Array(0)
            Bottom
          case _ =>
            val arr = OfArray(State.currentObject, summon[Regions.Data])
            Heap.writeJoin(arr.addr, Bottom)
            arr
      else
        // Widen the outer to finitize the domain. Arguments already widened in `evalArgs`.
        val (outerWidened, envWidened) =
          outer match
            case _ : Bottom.type => // For top-level classes
              (Bottom, Env.NoEnv)
            case thisV : (Ref | Cold.type) =>
              if klass.owner.isClass then
                if klass.owner.is(Flags.Package) then
                  report.warning("[Internal error] top-level class should have `Bottom` as outer, class = " + klass.show + ", outer = " + outer.show + ", " + Trace.show, Trace.position)
                  (Bottom, Env.NoEnv)
                else
                  (thisV.widenRefOrCold(1), Env.NoEnv)
              else
                // klass.enclosingMethod returns its primary constructor
                Env.resolveEnv(klass.owner.enclosingMethod, thisV, summon[Env.Data]).getOrElse(Cold -> Env.NoEnv)

        val instance = OfClass(klass, outerWidened, ctor, args.map(_.value), envWidened)
        callConstructor(instance, ctor, args)
        instance

    case ValueSet(values) =>
      values.map(ref => instantiate(ref, klass, ctor, args)).join
  }

  /** Handle local variable definition, `val x = e` or `var x = e`.
   *
   * @param sym          The symbol of the variable.
   * @param value        The value of the initializer.
   */
  def initLocal(sym: Symbol, value: Value): Contextual[Unit] = log("initialize local " + sym.show + " with " + value.show, printer) {
    if sym.is(Flags.Mutable) then
      val addr = Heap.localVarAddr(summon[Regions.Data], sym, State.currentObject)
      Env.setLocalVar(sym, addr)
      Heap.writeJoin(addr, value)
    else
      Env.setLocalVal(sym, value)
  }

  /** Read local variable `x`.
   *
   * @param thisV        The value for `this` where the variable is used.
   * @param sym          The symbol of the variable.
   */
  def readLocal(thisV: ThisValue, sym: Symbol): Contextual[Value] = log("reading local " + sym.show, printer, (_: Value).show) {
    def isByNameParam(sym: Symbol) = sym.is(Flags.Param) && sym.info.isInstanceOf[ExprType]
    Env.resolveEnv(sym.enclosingMethod, thisV, summon[Env.Data]) match
    case Some(thisV -> env) =>
      if sym.is(Flags.Mutable) then
        // Assume forward reference check is doing a good job
        given Env.Data = env
        Env.getVar(sym) match
        case Some(addr) =>
          if addr.owner == State.currentObject then
            Heap.read(addr)
          else
            errorReadOtherStaticObject(State.currentObject, addr.owner)
            Bottom
          end if
        case _ =>
          // Only vals can be lazy
          report.warning("[Internal error] Variable not found " + sym.show + "\nenv = " + env.show + ". " + Trace.show, Trace.position)
          Bottom
      else
        given Env.Data = env
        if sym.is(Flags.Lazy) then
          val rhs = sym.defTree.asInstanceOf[ValDef].rhs
          eval(rhs, thisV, sym.enclosingClass.asClass, cacheResult = true)
        else
          // Assume forward reference check is doing a good job
          val value = Env.valValue(sym)
          if isByNameParam(sym) then
            value match
            case fun: Fun =>
              given Env.Data = fun.env
              eval(fun.code, fun.thisV, fun.klass)
            case Cold =>
              report.warning("Calling cold by-name alias. " + Trace.show, Trace.position)
              Bottom
            case _: ValueSet | _: Ref | _: OfArray =>
              report.warning("[Internal error] Unexpected by-name value " + value.show  + ". " + Trace.show, Trace.position)
              Bottom
          else
            value

    case None =>
      if isByNameParam(sym) then
        report.warning("Calling cold by-name alias. " + Trace.show, Trace.position)
        Bottom
      else
        Cold
  }

  /** Handle local variable assignmenbt, `x = e`.
   *
   * @param thisV        The value for `this` where the assignment locates.
   * @param sym          The symbol of the variable.
   * @param value        The value of the rhs of the assignment.
   */
  def writeLocal(thisV: ThisValue, sym: Symbol, value: Value): Contextual[Value] = log("write local " + sym.show + " with " + value.show, printer, (_: Value).show) {
    assert(sym.is(Flags.Mutable), "Writing to immutable variable " + sym.show)

    Env.resolveEnv(sym.enclosingMethod, thisV, summon[Env.Data]) match
    case Some(thisV -> env) =>
      given Env.Data = env
      Env.getVar(sym) match
      case Some(addr) =>
        if addr.owner != State.currentObject then
          errorMutateOtherStaticObject(State.currentObject, addr.owner)
        else
          Heap.writeJoin(addr, value)
      case _ =>
        report.warning("[Internal error] Variable not found " + sym.show + "\nenv = " + env.show + ". " + Trace.show, Trace.position)

    case _ =>
      report.warning("Assigning to variables in outer scope. " + Trace.show, Trace.position)

    Bottom
  }

  // -------------------------------- algorithm --------------------------------

  /** Check an individual object */
  private def accessObject(classSym: ClassSymbol)(using Context, State.Data, Trace): ObjectRef = log("accessing " + classSym.show, printer, (_: Value).show) {
    if classSym.hasSource then
      State.checkObjectAccess(classSym)
    else
      ObjectRef(classSym)
  }


  def checkClasses(classes: List[ClassSymbol])(using Context): Unit =
    given State.Data = new State.Data
    given Trace = Trace.empty

    for
      classSym <- classes  if classSym.isStaticObject
    do
      accessObject(classSym)

  /** Evaluate an expression with the given value for `this` in a given class `klass`
   *
   *  Note that `klass` might be a super class of the object referred by `thisV`.
   *  The parameter `klass` is needed for `this` resolution. Consider the following code:
   *
   *  class A {
   *    A.this
   *    class B extends A { A.this }
   *  }
   *
   *  As can be seen above, the meaning of the expression `A.this` depends on where
   *  it is located.
   *
   *  This method only handles cache logic and delegates the work to `cases`.
   *
   * @param expr        The expression to be evaluated.
   * @param thisV       The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass       The enclosing class where the expression is located.
   * @param cacheResult It is used to reduce the size of the cache.
   */
  def eval(expr: Tree, thisV: ThisValue, klass: ClassSymbol, cacheResult: Boolean = false): Contextual[Value] = log("evaluating " + expr.show + ", this = " + thisV.show + ", regions = " + Regions.show + " in " + klass.show, printer, (_: Value).show) {
    cache.cachedEval(thisV, expr, cacheResult) { expr => cases(expr, thisV, klass) }
  }


  /** Evaluate a list of expressions */
  def evalExprs(exprs: List[Tree], thisV: ThisValue, klass: ClassSymbol): Contextual[List[Value]] =
    exprs.map { expr => eval(expr, thisV, klass) }

  /** Handles the evaluation of different expressions
   *
   *  Note: Recursive call should go to `eval` instead of `cases`.
   *
   * @param expr   The expression to be evaluated.
   * @param thisV  The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass  The enclosing class where the expression `expr` is located.
   */
  def cases(expr: Tree, thisV: ThisValue, klass: ClassSymbol): Contextual[Value] = log("evaluating " + expr.show + ", this = " + thisV.show + " in " + klass.show, printer, (_: Value).show) {
    val trace2 = trace.add(expr)

    expr match
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        Bottom

      case id @ Ident(name) if !id.symbol.is(Flags.Method)  =>
        assert(name.isTermName, "type trees should not reach here")
        withTrace(trace2) { evalType(expr.tpe, thisV, klass) }

      case NewExpr(tref, New(tpt), ctor, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        val cls = tref.classSymbol.asClass
        withTrace(trace2) {
          val outer = outerValue(tref, thisV, klass)
          instantiate(outer, cls, ctor, args)
        }

      case Apply(ref, arg :: Nil) if ref.symbol == defn.InitRegionMethod =>
        val regions2 = Regions.extend(expr.sourcePos)
        if Regions.exists(expr.sourcePos) then
          report.warning("Cyclic region detected. Trace:\n" + Trace.show, expr)
          Bottom
        else
          given Regions.Data = regions2
          eval(arg, thisV, klass)

      case Call(ref, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe: @unchecked
          val thisValue2 = extendTrace(ref) {
            thisTp match
              case thisTp: ThisType =>
                evalType(thisTp, thisV, klass)
              case AndType(thisTp: ThisType, _) =>
                evalType(thisTp, thisV, klass)
              case _ =>
                report.warning("[Internal error] Unexpected type " + thisTp.show + ", trace:\n" + Trace.show, ref)
                Bottom
          }
          withTrace(trace2) { call(thisValue2, ref.symbol, args, thisTp, superTp) }

        case Select(qual, _) =>
          val receiver = eval(qual, thisV, klass)
          if ref.symbol.isConstructor then
            withTrace(trace2) { callConstructor(receiver, ref.symbol, args) }
          else
            withTrace(trace2) { call(receiver, ref.symbol, args, receiver = qual.tpe, superType = NoType) }

        case id: Ident =>
          id.tpe match
          case TermRef(NoPrefix, _) =>
            // resolve this for the local method
            val enclosingClass = id.symbol.owner.enclosingClass.asClass
            val thisValue2 = extendTrace(ref) { resolveThis(enclosingClass, thisV, klass) }
            // local methods are not a member, but we can reuse the method `call`
            withTrace(trace2) { call(thisValue2, id.symbol, args, receiver = NoType, superType = NoType, needResolve = false) }
          case TermRef(prefix, _) =>
            val receiver = withTrace(trace2) { evalType(prefix, thisV, klass) }
            if id.symbol.isConstructor then
              withTrace(trace2) { callConstructor(receiver, id.symbol, args) }
            else
              withTrace(trace2) { call(receiver, id.symbol, args, receiver = prefix, superType = NoType) }

      case Select(qualifier, name) =>
        val qual = eval(qualifier, thisV, klass)

        name match
          case OuterSelectName(_, _) =>
            val current = qualifier.tpe.classSymbol
            val target = expr.tpe.widenSingleton.classSymbol.asClass
            withTrace(trace2) { resolveThis(target, qual, current.asClass) }
          case _ =>
            withTrace(trace2) { select(qual, expr.symbol, receiver = qualifier.tpe) }

      case _: This =>
        evalType(expr.tpe, thisV, klass)

      case Literal(_) =>
        Bottom

      case Typed(expr, tpt) =>
        if tpt.tpe.hasAnnotation(defn.UncheckedAnnot) then
          Bottom
        else
          eval(expr, thisV, klass)

      case NamedArg(name, arg) =>
        eval(arg, thisV, klass)

      case Assign(lhs, rhs) =>
        var isLocal = false
        val receiver =
          lhs match
          case Select(qual, _) =>
            eval(qual, thisV, klass)
          case id: Ident =>
            id.tpe match
            case TermRef(NoPrefix, _) =>
              isLocal = true
              thisV
            case TermRef(prefix, _) =>
              extendTrace(id) { evalType(prefix, thisV, klass) }

        val value = eval(rhs, thisV, klass)

        if isLocal then
          writeLocal(thisV, lhs.symbol, value)
        else
          withTrace(trace2) { assign(receiver, lhs.symbol, value, rhs.tpe) }

      case closureDef(ddef) =>
        Fun(ddef, thisV, klass, summon[Env.Data])

      case PolyFun(ddef) =>
        Fun(ddef, thisV, klass, summon[Env.Data])

      case Block(stats, expr) =>
        evalExprs(stats, thisV, klass)
        eval(expr, thisV, klass)

      case If(cond, thenp, elsep) =>
        eval(cond, thisV, klass)
        evalExprs(thenp :: elsep :: Nil, thisV, klass).join

      case Annotated(arg, annot) =>
        if expr.tpe.hasAnnotation(defn.UncheckedAnnot) then
          Bottom
        else
          eval(arg, thisV, klass)

      case Match(scrutinee, cases) =>
        val scrutineeValue = eval(scrutinee, thisV, klass)
        patternMatch(scrutineeValue, cases, thisV, klass)

      case Return(expr, from) =>
        Returns.handle(from.symbol, eval(expr, thisV, klass))
        Bottom

      case WhileDo(cond, body) =>
        evalExprs(cond :: body :: Nil, thisV, klass)
        Bottom

      case Labeled(_, expr) =>
        eval(expr, thisV, klass)

      case Try(block, cases, finalizer) =>
        val res = evalExprs(block :: cases.map(_.body), thisV, klass).join
        if !finalizer.isEmpty then
          eval(finalizer, thisV, klass)
        res

      case SeqLiteral(elems, elemtpt) =>
        evalExprs(elems, thisV, klass).join

      case Inlined(call, bindings, expansion) =>
        evalExprs(bindings, thisV, klass)
        eval(expansion, thisV, klass)

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Bottom

      case vdef : ValDef =>
        // local val definition
        val sym = vdef.symbol
        if !sym.is(Flags.Lazy) then
          val rhs = eval(vdef.rhs, thisV, klass)
          initLocal(sym, rhs)
        Bottom

      case ddef : DefDef =>
        // local method
        Bottom

      case tdef: TypeDef =>
        // local type definition
        Bottom

      case _: Import | _: Export =>
        Bottom

      case tpl: Template =>
        init(tpl, thisV.asInstanceOf[Ref], klass)

      case _ =>
        report.warning("[Internal error] unexpected tree: " + expr + "\n" + Trace.show, expr)
        Bottom
  }

  /** Evaluate the cases against the scrutinee value.
   *
   *  It returns the scrutinee in most cases. The main effect of the function is for its side effects of adding bindings
   *  to the environment.
   *
   *  See https://docs.scala-lang.org/scala3/reference/changed-features/pattern-matching.html
   *
   *  @param scrutinee   The abstract value of the scrutinee.
   *  @param cases       The cases to match.
   *  @param thisV       The value for `C.this` where `C` is represented by `klass`.
   *  @param klass       The enclosing class where the type `tp` is located.
   */
  def patternMatch(scrutinee: Value, cases: List[CaseDef], thisV: ThisValue, klass: ClassSymbol): Contextual[Value] =
    def evalCase(caseDef: CaseDef): Value =
      evalPattern(scrutinee, caseDef.pat)
      eval(caseDef.guard, thisV, klass)
      eval(caseDef.body, thisV, klass)

    /** Abstract evaluation of patterns.
     *
     *  It augments the local environment for bound pattern variables. As symbols are globally
     *  unique, we can put them in a single environment.
     *
     *  Currently, we assume all cases are reachable, thus all patterns are assumed to match.
     */
    def evalPattern(scrutinee: Value, pat: Tree): Value = log("match " + scrutinee.show + " against " + pat.show, printer, (_: Value).show):
      val trace2 = Trace.trace.add(pat)
      pat match
      case Alternative(pats) =>
        for pat <- pats do evalPattern(scrutinee, pat)
        scrutinee

      case bind @ Bind(_, pat) =>
        val value = evalPattern(scrutinee, pat)
        initLocal(bind.symbol, value)
        scrutinee

      case UnApply(fun, implicits, pats) =>
        given Trace = trace2

        val fun1 = funPart(fun)
        val funRef = fun1.tpe.asInstanceOf[TermRef]
        val unapplyResTp = funRef.widen.finalResultType

        val receiver = fun1 match
          case ident: Ident =>
            evalType(funRef.prefix, thisV, klass)
          case select: Select =>
            eval(select.qualifier, thisV, klass)

        def implicitArgsBeforeScrutinee(fun: Tree): Contextual[List[ArgInfo]] = fun match
          case Apply(f, implicitArgs) =>
            implicitArgsBeforeScrutinee(f) ++ evalArgs(implicitArgs.map(Arg.apply), thisV, klass)
          case _ => List()

        val implicitArgsAfterScrutinee = evalArgs(implicits.map(Arg.apply), thisV, klass)
        val args = implicitArgsBeforeScrutinee(fun) ++ (ArgInfo(scrutinee, summon[Trace], EmptyTree) :: implicitArgsAfterScrutinee)
        val unapplyRes = call(receiver, funRef.symbol, args, funRef.prefix, superType = NoType, needResolve = true)

        def callSelectors(selectors: List[Symbol], resToMatch: Value, resultTp: Type) =
          selectors.zip(pats).map { (sel, pat) =>
            val selectRes = call(resToMatch, sel, Nil, resultTp, superType = NoType, needResolve = true)
            evalPattern(selectRes, pat)
          }

        def maybeGet(getMatch: GetMatchInfo)(onValue: Value => Unit) =
          var resToMatch = unapplyRes

          if getMatch.isValid then
            // Get match
            call(unapplyRes, getMatch.isEmptyDenot.symbol, Nil, unapplyResTp, superType = NoType, needResolve = true)

            resToMatch = call(unapplyRes, getMatch.getDenot.symbol, Nil, unapplyResTp, superType = NoType, needResolve = true)
          end if

          onValue(resToMatch)

        val ext = Extractor(unapplyResTp, fun.symbol.name, pats.length)
        maybeGet(ext.getMatchInfo): resToMatch =>
          ext match
          case ext: SeqMatch =>
            // sequence match
            evalSeqPatterns(resToMatch, ext.unapplySeqInfo, ext.resType, pats)
          case ext: ProdSeqMatch =>
            // product sequence match
            val init :+ last = ext.productSelectors: @unchecked
            callSelectors(init, resToMatch, ext.resType)
            val seqPats = pats.drop(init.size)
            val toSeqRes = call(resToMatch, last, Nil, ext.resType, superType = NoType, needResolve = true)
            val toSeqResTp = ext.productSelectorTypes.last
            evalSeqPatterns(toSeqRes, ext.unapplySeqInfo, toSeqResTp, seqPats)

          // distribute unapply to patterns
          case ext: ProductMatch =>
            // product match
            callSelectors(ext.productSelectors, resToMatch, ext.resType)
          case ext: BooleanMatch =>
            // Boolean extractor, do nothing
          case ext: SingleMatch =>
            // single match
            evalPattern(resToMatch, pats.head)
          case ext: NameBasedMatch =>
            // name-based match
            callSelectors(ext.productSelectors, resToMatch, ext.resType)
          case ext: TupleMatch =>
            val sel = defn.RuntimeTuples_apply
            val args = ArgInfo(Bottom, summon[Trace], EmptyTree) :: Nil
            val recv = defn.RuntimeTuplesModule.termRef
            ext.tupleComponentTypes.indices.lazyZip(pats).foreach: (_, pat) =>
              val selectRes = call(resToMatch, sel, args, recv, superType = NoType, needResolve = true)
              evalPattern(selectRes, pat)

          case x @ NoExtractor => unreachable(x)

        scrutinee

      case Ident(nme.WILDCARD) | Ident(nme.WILDCARD_STAR) =>
        scrutinee

      case Typed(pat, _) =>
        evalPattern(scrutinee, pat)

      case tree =>
        // For all other trees, the semantics is normal.
        eval(tree, thisV, klass)

    end evalPattern

    /**
     * Evaluate a sequence value against sequence patterns.
     */
    def evalSeqPatterns(scrutinee: Value, unapp: UnapplySeqInfo, scrutineeType: Type, pats: List[Tree])(using Trace): Unit =
      // call .lengthCompare or .length
      if unapp.lengthCmpDenot.exists then
        call(scrutinee, unapp.lengthCmpDenot.symbol, ArgInfo(Bottom, summon[Trace], EmptyTree) :: Nil, scrutineeType, superType = NoType, needResolve = true)
      else
        call(scrutinee, unapp.lengthDenot.symbol, Nil, scrutineeType, superType = NoType, needResolve = true)
      end if

      // call .apply
      val applyRes = call(scrutinee, unapp.applyDenot.symbol, ArgInfo(Bottom, summon[Trace], EmptyTree) :: Nil, scrutineeType, superType = NoType, needResolve = true)

      if isWildcardStarArgList(pats) then
        if pats.size == 1 then
          // call .toSeq
          val toSeqRes = call(scrutinee, unapp.toSeqDenot.symbol, Nil, scrutineeType, superType = NoType, needResolve = true)
          evalPattern(toSeqRes, pats.head)
        else
          // call .drop
          val dropRes = call(scrutinee, unapp.dropDenot.symbol, ArgInfo(Bottom, summon[Trace], EmptyTree) :: Nil, scrutineeType, superType = NoType, needResolve = true)
          for pat <- pats.init do evalPattern(applyRes, pat)
          evalPattern(dropRes, pats.last)
        end if
      else
        // no patterns like `xs*`
        for pat <- pats do evalPattern(applyRes, pat)
      end if
    end evalSeqPatterns


    cases.map(evalCase).join
  end patternMatch

  /** Handle semantics of leaf nodes
   *
   * For leaf nodes, their semantics is determined by their types.
   *
   * @param tp      The type to be evaluated.
   * @param thisV   The value for `C.this` where `C` is represented by `klass`.
   * @param klass   The enclosing class where the type `tp` is located.
   * @param elideObjectAccess Whether object access should be omitted.
   *
   * Object access elission happens when the object access is used as a prefix
   * in `new o.C` and `C` does not need an outer.
   */
  def evalType(tp: Type, thisV: ThisValue, klass: ClassSymbol, elideObjectAccess: Boolean = false): Contextual[Value] = log("evaluating " + tp.show, printer, (_: Value).show) {
    tp match
      case _: ConstantType =>
        Bottom

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        val sym = tmref.symbol
        if sym.is(Flags.Package) then
          Bottom
        else if sym.owner.isClass then
          // The typer incorrectly assigns a TermRef with NoPrefix for `config`,
          // while the actual denotation points to the symbol of the class member
          // instead of the parameter symbol for the primary constructor.
          //
          // abstract class Base(implicit config: Int)
          // case class A(x: Int)(implicit config: Int) extends Base
          evalType(sym.termRef, thisV, klass, elideObjectAccess)
        else
          readLocal(thisV, sym)

      case tmref: TermRef =>
        val sym = tmref.symbol
        if sym.isStaticObject then
          if elideObjectAccess then
            ObjectRef(sym.moduleClass.asClass)
          else
            accessObject(sym.moduleClass.asClass)
        else
          val value = evalType(tmref.prefix, thisV, klass)
          select(value, tmref.symbol, tmref.prefix)

      case tp @ ThisType(tref) =>
        val sym = tref.symbol
        if sym.is(Flags.Package) then
          Bottom
        else if sym.isStaticObject && sym != klass then
          // The typer may use ThisType to refer to an object outside its definition.
          if elideObjectAccess then
            ObjectRef(sym.moduleClass.asClass)
          else
            accessObject(sym.moduleClass.asClass)

        else
          resolveThis(tref.classSymbol.asClass, thisV, klass)

      case _ =>
        throw new Exception("unexpected type: " + tp + ", Trace:\n" + Trace.show)
  }

  /** Evaluate arguments of methods and constructors */
  def evalArgs(args: List[Arg], thisV: ThisValue, klass: ClassSymbol): Contextual[List[ArgInfo]] =
    val argInfos = new mutable.ArrayBuffer[ArgInfo]
    args.foreach { arg =>
      val res =
        if arg.isByName then
          Fun(arg.tree, thisV, klass, summon[Env.Data])
        else
          eval(arg.tree, thisV, klass)

      val widened =
        arg.tree.tpe.getAnnotation(defn.InitWidenAnnot) match
        case Some(annot) =>
          annot.argument(0).get match
          case arg @ Literal(c: Constants.Constant) =>
            val height = c.intValue
            if height < 0 then
              report.warning("The argument should be positive", arg)
              res.widen(1)
            else
              res.widen(c.intValue)
          case arg =>
            report.warning("The argument should be a constant integer value", arg)
            res.widen(1)
        case _ =>
          res.widen(1)

      argInfos += ArgInfo(widened, trace.add(arg.tree), arg.tree)
    }
    argInfos.toList

  /** Initialize part of an abstract object in `klass` of the inheritance chain
   *
   * @param tpl       The class body to be evaluated.
   * @param thisV     The value of the current object to be initialized.
   * @param klass     The class to which the template belongs.
   */
  def init(tpl: Template, thisV: Ref, klass: ClassSymbol): Contextual[Ref] = log("init " + klass.show, printer, (_: Value).show) {
    val paramsMap = tpl.constr.termParamss.flatten.map { vdef =>
      vdef.name -> Env.valValue(vdef.symbol)
    }.toMap

    // init param fields
    klass.paramGetters.foreach { acc =>
      val value = paramsMap(acc.name.toTermName)
      if acc.is(Flags.Mutable) then
        val addr = Heap.fieldVarAddr(summon[Regions.Data], acc, State.currentObject)
        thisV.initVar(acc, addr)
        Heap.writeJoin(addr, value)
      else
        thisV.initVal(acc, value)
      printer.println(acc.show + " initialized with " + value)
    }

    // Tasks is used to schedule super constructor calls.
    // Super constructor calls are delayed until all outers are set.
    type Tasks = mutable.ArrayBuffer[() => Unit]
    def superCall(tref: TypeRef, ctor: Symbol, args: List[ArgInfo], tasks: Tasks): Unit =
      val cls = tref.classSymbol.asClass
      // update outer for super class
      val res = outerValue(tref, thisV, klass)
      thisV.initOuter(cls, res)

      // follow constructor
      if cls.hasSource then
        tasks.append { () =>
          printer.println("init super class " + cls.show)
          callConstructor(thisV, ctor, args)
          ()
        }

    // parents
    def initParent(parent: Tree, tasks: Tasks) =
      parent match
      case tree @ Block(stats, NewExpr(tref, New(tpt), ctor, argss)) =>  // can happen
        evalExprs(stats, thisV, klass)
        val args = evalArgs(argss.flatten, thisV, klass)
        superCall(tref, ctor, args, tasks)

      case tree @ NewExpr(tref, New(tpt), ctor, argss) =>       // extends A(args)
        val args = evalArgs(argss.flatten, thisV, klass)
        superCall(tref, ctor, args, tasks)

      case _ =>   // extends A or extends A[T]
        val tref = typeRefOf(parent.tpe)
        superCall(tref, tref.classSymbol.primaryConstructor, Nil, tasks)

    // see spec 5.1 about "Template Evaluation".
    // https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html
    if !klass.is(Flags.Trait) then
      // outers are set first
      val tasks = new mutable.ArrayBuffer[() => Unit]

      // 1. first init parent class recursively
      // 2. initialize traits according to linearization order
      val superParent = tpl.parents.head
      val superCls = superParent.tpe.classSymbol.asClass
      extendTrace(superParent) { initParent(superParent, tasks) }

      val parents = tpl.parents.tail
      val mixins = klass.baseClasses.tail.takeWhile(_ != superCls)

      // The interesting case is the outers for traits.  The compiler
      // synthesizes proxy accessors for the outers in the class that extends
      // the trait. As those outers must be stable values, they are initialized
      // immediately following class parameters and before super constructor
      // calls and user code in the class body.
      mixins.reverse.foreach { mixin =>
        parents.find(_.tpe.classSymbol == mixin) match
        case Some(parent) =>
          extendTrace(parent) { initParent(parent, tasks) }
        case None =>
          // According to the language spec, if the mixin trait requires
          // arguments, then the class must provide arguments to it explicitly
          // in the parent list. That means we will encounter it in the Some
          // branch.
          //
          // When a trait A extends a parameterized trait B, it cannot provide
          // term arguments to B. That can only be done in a concrete class.
          val tref = typeRefOf(klass.typeRef.baseType(mixin).typeConstructor)
          val ctor = tref.classSymbol.primaryConstructor
          if ctor.exists then
            // The parameter check of traits comes late in the mixin phase.
            // To avoid crash we supply hot values for erroneous parent calls.
            // See tests/neg/i16438.scala.
            val args: List[ArgInfo] = ctor.info.paramInfoss.flatten.map(_ => new ArgInfo(Bottom, Trace.empty, EmptyTree))
            extendTrace(superParent) {
              superCall(tref, ctor, args, tasks)
            }
      }

      // initialize super classes after outers are set
      tasks.foreach(task => task())
    end if

    // class body
    tpl.body.foreach {
      case vdef : ValDef if !vdef.symbol.is(Flags.Lazy) && !vdef.rhs.isEmpty =>
        val res = eval(vdef.rhs, thisV, klass)
        val sym = vdef.symbol
        if sym.is(Flags.Mutable) then
          val addr = Heap.fieldVarAddr(summon[Regions.Data], sym, State.currentObject)
          thisV.initVar(sym, addr)
          Heap.writeJoin(addr, res)
        else
          thisV.initVal(sym, res)

      case _: MemberDef =>

      case tree =>
        eval(tree, thisV, klass)
    }

    thisV
  }


  /** Resolve C.this that appear in `klass`
   *
   * @param target  The class symbol for `C` for which `C.this` is to be resolved.
   * @param thisV   The value for `D.this` where `D` is represented by the parameter `klass`.
   * @param klass   The enclosing class where the type `C.this` is located.
   * @param elideObjectAccess Whether object access should be omitted.
   *
   * Object access elision happens when the object access is used as a prefix
   * in `new o.C` and `C` does not need an outer.
   */
  def resolveThis(target: ClassSymbol, thisV: Value, klass: ClassSymbol, elideObjectAccess: Boolean = false): Contextual[Value] = log("resolveThis target = " + target.show + ", this = " + thisV.show, printer, (_: Value).show) {
    if target == klass then
      thisV
    else if target.is(Flags.Package) then
      Bottom
    else if target.isStaticObject then
      val res = ObjectRef(target.moduleClass.asClass)
      if elideObjectAccess then res
      else accessObject(target)
    else
      thisV match
        case Bottom => Bottom
        case Cold => Cold
        case ref: Ref =>
          val outerCls = klass.owner.lexicallyEnclosingClass.asClass
          if !ref.hasOuter(klass) then
            val error = "[Internal error] outer not yet initialized, target = " + target + ", klass = " + klass + Trace.show
            report.warning(error, Trace.position)
            Bottom
          else
            resolveThis(target, ref.outerValue(klass), outerCls)
        case ValueSet(values) =>
          values.map(ref => resolveThis(target, ref, klass)).join
        case _: Fun | _ : OfArray =>
          report.warning("[Internal error] unexpected thisV = " + thisV + ", target = " + target.show + ", klass = " + klass.show + Trace.show, Trace.position)
          Bottom
  }

  /** Compute the outer value that corresponds to `tref.prefix`
   *
   * @param tref    The type whose prefix is to be evaluated.
   * @param thisV   The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass   The enclosing class where the type `tref` is located.
   */
  def outerValue(tref: TypeRef, thisV: ThisValue, klass: ClassSymbol): Contextual[Value] =
    val cls = tref.classSymbol.asClass
    if tref.prefix == NoPrefix then
      val enclosing = cls.owner.lexicallyEnclosingClass.asClass
      resolveThis(enclosing, thisV, klass, elideObjectAccess = cls.isStatic)
    else
      if cls.isAllOf(Flags.JavaInterface) then Bottom
      else evalType(tref.prefix, thisV, klass, elideObjectAccess = cls.isStatic)

  def errorMutateOtherStaticObject(currentObj: ClassSymbol, otherObj: ClassSymbol)(using Trace, Context) =
    val msg =
      s"Mutating ${otherObj.show} during initialization of ${currentObj.show}.\n" +
      "Mutating other static objects during the initialization of one static object is forbidden. " + Trace.show

    report.warning(msg, Trace.position)

  def errorReadOtherStaticObject(currentObj: ClassSymbol, otherObj: ClassSymbol)(using Trace, Context) =
    val msg =
      "Reading mutable state of " + otherObj.show + " during initialization of " + currentObj.show + ".\n" +
      "Reading mutable state of other static objects is forbidden as it breaks initialization-time irrelevance. " + Trace.show

    report.warning(msg, Trace.position)
