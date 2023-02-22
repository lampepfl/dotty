package dotty.tools.dotc
package quoted

import scala.language.unsafeNulls

import scala.collection.mutable
import scala.reflect.ClassTag

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.{InvocationTargetException, Method => JLRMethod}

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.TreeMapWithImplicits
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Denotations.staticRef
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds.FlatName
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.transform.TreeMapWithStages._
import dotty.tools.dotc.typer.ImportInfo.withRootImports
import dotty.tools.dotc.util.SrcPos
import dotty.tools.dotc.reporting.Message
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.core.CyclicReference

/** Tree interpreter for metaprogramming constructs */
class Interpreter(pos: SrcPos, classLoader0: ClassLoader)(using Context):
  import Interpreter._
  import tpd._

  val classLoader =
    if ctx.owner.topLevelClass.name.startsWith(str.REPL_SESSION_LINE) then
        new AbstractFileClassLoader(ctx.settings.outputDir.value, classLoader0)
    else classLoader0

  /** Local variable environment */
  type Env = Map[Symbol, Object]
  def emptyEnv: Env = Map.empty
  inline def env(using e: Env): e.type = e

  /** Returns the result of interpreting the code in the tree.
   *  Return Some of the result or None if the result type is not consistent with the expected type.
   *  Throws a StopInterpretation if the tree could not be interpreted or a runtime exception ocurred.
   */
  final def interpret[T](tree: Tree)(using ct: ClassTag[T]): Option[T] =
    interpretTree(tree)(using emptyEnv) match {
      case obj: T => Some(obj)
      case obj =>
        // TODO upgrade to a full type tag check or something similar
        report.error(em"Interpreted tree returned a result of an unexpected type. Expected ${ct.runtimeClass} but was ${obj.getClass}", pos)
        None
    }

  /** Returns the result of interpreting the code in the tree.
   *  Throws a StopInterpretation if the tree could not be interpreted or a runtime exception ocurred.
   */
  protected def interpretTree(tree: Tree)(using Env): Object = tree match {
    case Literal(Constant(value)) =>
      interpretLiteral(value)

    case tree: Ident if tree.symbol.is(Inline, butNot = Method) =>
      tree.tpe.widenTermRefExpr match
        case ConstantType(c) => c.value.asInstanceOf[Object]
        case _ => throw new StopInterpretation(em"${tree.symbol} could not be inlined", tree.srcPos)

    // TODO disallow interpreted method calls as arguments
    case Call(fn, args) =>
      if (fn.symbol.isConstructor)
        interpretNew(fn.symbol, args.flatten.map(interpretTree))
      else if (fn.symbol.is(Module))
        interpretModuleAccess(fn.symbol)
      else if (fn.symbol.is(Method) && fn.symbol.isStatic) {
        interpretedStaticMethodCall(fn.symbol.owner, fn.symbol, interpretArgs(args, fn.symbol.info))
      }
      else if fn.symbol.isStatic then
        assert(args.isEmpty)
        interpretedStaticFieldAccess(fn.symbol)
      else if (fn.qualifier.symbol.is(Module) && fn.qualifier.symbol.isStatic)
        if (fn.name == nme.asInstanceOfPM)
          interpretModuleAccess(fn.qualifier.symbol)
        else {
          interpretedStaticMethodCall(fn.qualifier.symbol.moduleClass, fn.symbol, interpretArgs(args, fn.symbol.info))
        }
      else if (env.contains(fn.symbol))
        env(fn.symbol)
      else if (tree.symbol.is(InlineProxy))
        interpretTree(tree.symbol.defTree.asInstanceOf[ValOrDefDef].rhs)
      else
        unexpectedTree(tree)

    case closureDef((ddef @ DefDef(_, ValDefs(arg :: Nil) :: Nil, _, _))) =>
      (obj: AnyRef) => interpretTree(ddef.rhs)(using env.updated(arg.symbol, obj))

    // Interpret `foo(j = x, i = y)` which it is expanded to
    // `val j$1 = x; val i$1 = y; foo(i = i$1, j = j$1)`
    case Block(stats, expr) => interpretBlock(stats, expr)
    case NamedArg(_, arg) => interpretTree(arg)

    case Inlined(_, bindings, expansion) => interpretBlock(bindings, expansion)

    case Typed(expr, _) =>
      interpretTree(expr)

    case SeqLiteral(elems, _) =>
      interpretVarargs(elems.map(e => interpretTree(e)))

    case _ =>
      unexpectedTree(tree)
  }

  private def interpretArgs(argss: List[List[Tree]], fnType: Type)(using Env): List[Object] = {
    def interpretArgsGroup(args: List[Tree], argTypes: List[Type]): List[Object] =
      assert(args.size == argTypes.size)
      val view =
        for (arg, info) <- args.lazyZip(argTypes) yield
          info match
            case _: ExprType => () => interpretTree(arg) // by-name argument
            case _ => interpretTree(arg) // by-value argument
      view.toList

    fnType.dealias match
      case fnType: MethodType if fnType.isErasedMethod => interpretArgs(argss, fnType.resType)
      case fnType: MethodType =>
        val argTypes = fnType.paramInfos
        assert(argss.head.size == argTypes.size)
        interpretArgsGroup(argss.head, argTypes) ::: interpretArgs(argss.tail, fnType.resType)
      case fnType: AppliedType if fnType.isContextFunctionType =>
        val argTypes :+ resType = fnType.args: @unchecked
        interpretArgsGroup(argss.head, argTypes) ::: interpretArgs(argss.tail, resType)
      case fnType: PolyType => interpretArgs(argss, fnType.resType)
      case fnType: ExprType => interpretArgs(argss, fnType.resType)
      case _ =>
        assert(argss.isEmpty)
        Nil
  }

  private def interpretBlock(stats: List[Tree], expr: Tree)(using Env) = {
    var unexpected: Option[Object] = None
    val newEnv = stats.foldLeft(env)((accEnv, stat) => stat match
      case stat: ValDef =>
        accEnv.updated(stat.symbol, interpretTree(stat.rhs)(using accEnv))
      case stat =>
        if (unexpected.isEmpty)
          unexpected = Some(unexpectedTree(stat))
        accEnv
    )
    unexpected.getOrElse(interpretTree(expr)(using newEnv))
  }

  private def interpretLiteral(value: Any): Object =
    value.asInstanceOf[Object]

  private def interpretVarargs(args: List[Object]): Object =
    args.toSeq

  private def interpretedStaticMethodCall(moduleClass: Symbol, fn: Symbol, args: List[Object]): Object = {
    val inst =
      try loadModule(moduleClass)
      catch
        case MissingClassDefinedInCurrentRun(sym) =>
          suspendOnMissing(sym, pos)
    val clazz = inst.getClass
    val name = fn.name.asTermName
    val method = getMethod(clazz, name, paramsSig(fn))
    stopIfRuntimeException(method.invoke(inst, args: _*), method)
  }

  private def interpretedStaticFieldAccess(sym: Symbol): Object = {
    val clazz = loadClass(sym.owner.fullName.toString)
    val field = clazz.getField(sym.name.toString)
    field.get(null)
  }

  private def interpretModuleAccess(fn: Symbol): Object =
    loadModule(fn.moduleClass)

  private def interpretNew(fn: Symbol, args: List[Object]): Object = {
    val className = fn.owner.fullName.mangledString.replaceAll("\\$\\.", "\\$")
    val clazz = loadClass(className)
    val constr = clazz.getConstructor(paramsSig(fn): _*)
    constr.newInstance(args: _*).asInstanceOf[Object]
  }

  private def unexpectedTree(tree: Tree): Object =
    throw new StopInterpretation(em"Unexpected tree could not be interpreted: ${tree.toString}", tree.srcPos)

  private def loadModule(sym: Symbol): Object =
    if (sym.owner.is(Package)) {
      // is top level object
      val moduleClass = loadClass(sym.fullName.toString)
      moduleClass.getField(str.MODULE_INSTANCE_FIELD).get(null)
    }
    else {
      // nested object in an object
      val clazz = loadClass(sym.binaryClassName)
      clazz.getConstructor().newInstance().asInstanceOf[Object]
    }

  private def loadReplLineClass(moduleClass: Symbol): Class[?] = {
    val lineClassloader = new AbstractFileClassLoader(ctx.settings.outputDir.value, classLoader)
    lineClassloader.loadClass(moduleClass.name.firstPart.toString)
  }

  private def loadClass(name: String): Class[?] =
    try classLoader.loadClass(name)
    catch
      case MissingClassDefinedInCurrentRun(sym) =>
        suspendOnMissing(sym, pos)


  private def getMethod(clazz: Class[?], name: Name, paramClasses: List[Class[?]]): JLRMethod =
    try clazz.getMethod(name.toString, paramClasses: _*)
    catch {
      case _: NoSuchMethodException =>
        val msg = em"Could not find method ${clazz.getCanonicalName}.$name with parameters ($paramClasses%, %)"
        throw new StopInterpretation(msg, pos)
      case MissingClassDefinedInCurrentRun(sym) =>
        suspendOnMissing(sym, pos)
    }

  private def stopIfRuntimeException[T](thunk: => T, method: JLRMethod): T =
    try thunk
    catch {
      case ex: RuntimeException =>
        val sw = new StringWriter()
        sw.write("A runtime exception occurred while executing macro expansion\n")
        sw.write(ex.getMessage)
        sw.write("\n")
        ex.printStackTrace(new PrintWriter(sw))
        sw.write("\n")
        throw new StopInterpretation(sw.toString.toMessage, pos)
      case ex: InvocationTargetException =>
        ex.getTargetException match {
          case ex: scala.quoted.runtime.StopMacroExpansion =>
            throw ex
          case MissingClassDefinedInCurrentRun(sym) =>
            suspendOnMissing(sym, pos)
          case targetException =>
            val sw = new StringWriter()
            sw.write("Exception occurred while executing macro expansion.\n")
            if (!ctx.settings.Ydebug.value) {
              val end = targetException.getStackTrace.lastIndexWhere { x =>
                x.getClassName == method.getDeclaringClass.getCanonicalName && x.getMethodName == method.getName
              }
              val shortStackTrace = targetException.getStackTrace.take(end + 1)
              targetException.setStackTrace(shortStackTrace)
              targetException.printStackTrace(new PrintWriter(sw))

              targetException match
                case _: CyclicReference => sw.write("\nSee full stack trace using -Ydebug")
                case _ =>
            } else {
              targetException.printStackTrace(new PrintWriter(sw))
            }
            sw.write("\n")
            throw new StopInterpretation(sw.toString.toMessage, pos)
        }
    }

  /** List of classes of the parameters of the signature of `sym` */
  private def paramsSig(sym: Symbol): List[Class[?]] = {
    def paramClass(param: Type): Class[?] = {
      def arrayDepth(tpe: Type, depth: Int): (Type, Int) = tpe match {
        case JavaArrayType(elemType) => arrayDepth(elemType, depth + 1)
        case _ => (tpe, depth)
      }
      def javaArraySig(tpe: Type): String = {
        val (elemType, depth) = arrayDepth(tpe, 0)
        val sym = elemType.classSymbol
        val suffix =
          if (sym == defn.BooleanClass) "Z"
          else if (sym == defn.ByteClass) "B"
          else if (sym == defn.ShortClass) "S"
          else if (sym == defn.IntClass) "I"
          else if (sym == defn.LongClass) "J"
          else if (sym == defn.FloatClass) "F"
          else if (sym == defn.DoubleClass) "D"
          else if (sym == defn.CharClass) "C"
          else "L" + javaSig(elemType) + ";"
        ("[" * depth) + suffix
      }
      def javaSig(tpe: Type): String = tpe match {
        case tpe: JavaArrayType => javaArraySig(tpe)
        case _ =>
          // Take the flatten name of the class and the full package name
          val pack = tpe.classSymbol.topLevelClass.owner
          val packageName = if (pack == defn.EmptyPackageClass) "" else s"${pack.fullName}."
          packageName + tpe.classSymbol.fullNameSeparated(FlatName).toString
      }

      val sym = param.classSymbol
      if (sym == defn.BooleanClass) classOf[Boolean]
      else if (sym == defn.ByteClass) classOf[Byte]
      else if (sym == defn.CharClass) classOf[Char]
      else if (sym == defn.ShortClass) classOf[Short]
      else if (sym == defn.IntClass) classOf[Int]
      else if (sym == defn.LongClass) classOf[Long]
      else if (sym == defn.FloatClass) classOf[Float]
      else if (sym == defn.DoubleClass) classOf[Double]
      else java.lang.Class.forName(javaSig(param), false, classLoader)
    }
    def getExtraParams(tp: Type): List[Type] = tp.widenDealias match {
      case tp: AppliedType if tp.isContextFunctionType =>
        // Call context function type direct method
        tp.args.init.map(arg => TypeErasure.erasure(arg)) ::: getExtraParams(tp.args.last)
      case _ => Nil
    }
    val extraParams = getExtraParams(sym.info.finalResultType)
    val allParams = TypeErasure.erasure(sym.info) match {
      case meth: MethodType => meth.paramInfos ::: extraParams
      case _ => extraParams
    }
    allParams.map(paramClass)
  }
end Interpreter

object Interpreter:
  /** Exception that stops interpretation if some issue is found */
  class StopInterpretation(val msg: Message, val pos: SrcPos) extends Exception

  object Call:
    import tpd._
    /** Matches an expression that is either a field access or an application
    *  It retruns a TermRef containing field accessed or a method reference and the arguments passed to it.
    */
    def unapply(arg: Tree)(using Context): Option[(RefTree, List[List[Tree]])] =
      Call0.unapply(arg).map((fn, args) => (fn, args.reverse))

    private object Call0 {
      def unapply(arg: Tree)(using Context): Option[(RefTree, List[List[Tree]])] = arg match {
        case Select(Call0(fn, args), nme.apply) if fn.tpe.widenDealias.finalResultType.isContextFunctionType =>
          Some((fn, args))
        case fn: Ident => Some((tpd.desugarIdent(fn).withSpan(fn.span), Nil))
        case fn: Select => Some((fn, Nil))
        case Apply(f @ Call0(fn, args1), args2) =>
          if (f.tpe.widenDealias.isErasedMethod) Some((fn, args1))
          else Some((fn, args2 :: args1))
        case TypeApply(Call0(fn, args), _) => Some((fn, args))
        case _ => None
      }
    }
  end Call

  object MissingClassDefinedInCurrentRun {
    def unapply(targetException: Throwable)(using Context): Option[Symbol] = {
      if !ctx.compilationUnit.isSuspendable then None
      else targetException match
        case _: NoClassDefFoundError | _: ClassNotFoundException =>
          val className = targetException.getMessage
          if className eq null then None
          else
            val sym = staticRef(className.toTypeName).symbol
            if (sym.isDefinedInCurrentRun) Some(sym) else None
        case _ => None
    }
  }

  def suspendOnMissing(sym: Symbol, pos: SrcPos)(using Context): Nothing =
    if ctx.settings.XprintSuspension.value then
      report.echo(i"suspension triggered by a dependency on $sym", pos)
    ctx.compilationUnit.suspend() // this throws a SuspendException
