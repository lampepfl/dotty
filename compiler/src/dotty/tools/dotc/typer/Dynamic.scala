package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Names.{Name, TermName}
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.TypeErasure
import util.Spans.*
import core.Symbols.*
import ErrorReporting.*
import dotty.tools.dotc.transform.ValueClasses
import dotty.tools.dotc.transform.TypeUtils.isPrimitiveValueType
import reporting.*

object Dynamic {
  private def isDynamicMethod(name: Name): Boolean =
    name == nme.applyDynamic || name == nme.selectDynamic || name == nme.updateDynamic || name == nme.applyDynamicNamed

  /** Is `tree` a reference over `Dynamic` that should be expanded to a
   *  dyanmic `applyDynamic`, `selectDynamic`, `updateDynamic`, or `applyDynamicNamed` call?
   */
  def isDynamicExpansion(tree: untpd.RefTree)(using Context): Boolean =
    isDynamicMethod(tree.name)
    || tree.match
          case Select(Apply(fun: untpd.RefTree, _), nme.apply)
          if defn.isContextFunctionClass(fun.symbol.owner) =>
            isDynamicExpansion(fun)
          case Select(qual, nme.apply) =>
            isDynamicMethod(qual.symbol.name) && tree.span.isSynthetic
          case _ => false
}

object DynamicUnapply {
  def unapply(tree: tpd.Tree): Option[List[tpd.Tree]] = tree match
    case TypeApply(Select(qual, name), _) if name == nme.asInstanceOfPM =>
      unapply(qual)
    case Apply(Apply(Select(selectable, fname), Literal(Constant(name)) :: ctag :: Nil), _ :: implicits)
    if fname == nme.applyDynamic && (name == "unapply" || name == "unapplySeq") =>
      Some(selectable :: ctag :: implicits)
    case _ =>
      None
}

/** Handles programmable member selections of `Dynamic` instances and values
 *  with structural types. Two functionalities:
 *
 * 1. Translates selection that does not typecheck according to the scala.Dynamic rules:
 *    foo.bar(baz) = quux                   ~~> foo.selectDynamic(bar).update(baz, quux)
 *    foo.bar = baz                         ~~> foo.updateDynamic("bar")(baz)
 *    foo.bar(x = bazX, y = bazY, baz, ...) ~~> foo.applyDynamicNamed("bar")(("x", bazX), ("y", bazY), ("", baz), ...)
 *    foo.bar(baz0, baz1, ...)              ~~> foo.applyDynamic(bar)(baz0, baz1, ...)
 *    foo.bar                               ~~> foo.selectDynamic(bar)
 *
 *  The first matching rule of is applied.
 *
 * 2. Translates member selections on structural types to calls of `selectDynamic`
 *    or `applyDynamic` on a `Selectable` instance. @See handleStructural.
 *
 */
trait Dynamic {
  self: Typer & Applications =>

  import Dynamic._
  import tpd._

  /** Translate selection that does not typecheck according to the normal rules into a applyDynamic/applyDynamicNamed.
   *    foo.bar(baz0, baz1, ...)                       ~~> foo.applyDynamic(bar)(baz0, baz1, ...)
   *    foo.bar[T0, ...](baz0, baz1, ...)              ~~> foo.applyDynamic[T0, ...](bar)(baz0, baz1, ...)
   *    foo.bar(x = bazX, y = bazY, baz, ...)          ~~> foo.applyDynamicNamed("bar")(("x", bazX), ("y", bazY), ("", baz), ...)
   *    foo.bar[T0, ...](x = bazX, y = bazY, baz, ...) ~~> foo.applyDynamicNamed[T0, ...]("bar")(("x", bazX), ("y", bazY), ("", baz), ...)
   */
  def typedDynamicApply(tree: untpd.Apply, isInsertedApply: Boolean, pt: Type)(using Context): Tree = {
    def typedDynamicApply(qual: untpd.Tree, name: Name, selSpan: Span, targs: List[untpd.Tree]): Tree = {
      def isNamedArg(arg: untpd.Tree): Boolean = arg match { case NamedArg(_, _) => true; case _ => false }
      val args = tree.args
      val dynName = if (args.exists(isNamedArg)) nme.applyDynamicNamed else nme.applyDynamic
      if (dynName == nme.applyDynamicNamed && untpd.isWildcardStarArgList(args))
        errorTree(tree, em"applyDynamicNamed does not support passing a vararg parameter")
      else {
        def namedArgTuple(name: String, arg: untpd.Tree) = untpd.Tuple(List(Literal(Constant(name)), arg))
        def namedArgs = args.map {
          case NamedArg(argName, arg) => namedArgTuple(argName.toString, arg)
          case arg => namedArgTuple("", arg)
        }
        val args1 = if (dynName == nme.applyDynamic) args else namedArgs
        typedApply(untpd.Apply(coreDynamic(qual, dynName, name, selSpan, targs), args1), pt)
      }
    }

    if (isInsertedApply) {
      tree.fun match {
        case TypeApply(fun, targs) =>
          typedDynamicApply(fun, nme.apply, fun.span, targs)
        case fun =>
          typedDynamicApply(fun, nme.apply, fun.span, Nil)
      }
    } else {
      tree.fun match {
        case sel @ Select(qual, name) if !isDynamicMethod(name) =>
          typedDynamicApply(qual, name, sel.span, Nil)
        case TypeApply(sel @ Select(qual, name), targs) if !isDynamicMethod(name) =>
          typedDynamicApply(qual, name, sel.span, targs)
        case _ =>
          errorTree(tree, em"Dynamic insertion not applicable")
      }
    }
  }

  /** Translate selection that does not typecheck according to the normal rules into a selectDynamic.
   *    foo.bar          ~~> foo.selectDynamic(bar)
   *    foo.bar[T0, ...] ~~> foo.selectDynamic[T0, ...](bar)
   *
   *  Note: inner part of translation foo.bar(baz) = quux ~~> foo.selectDynamic(bar).update(baz, quux) is achieved
   *  through an existing transformation of in typedAssign [foo.bar(baz) = quux ~~> foo.bar.update(baz, quux)].
   */
  def typedDynamicSelect(tree: untpd.Select, targs: List[untpd.Tree], pt: Type)(using Context): Tree =
    typedApply(coreDynamic(tree.qualifier, nme.selectDynamic, tree.name, tree.span, targs), pt)

  /** Translate selection that does not typecheck according to the normal rules into a updateDynamic.
   *    foo.bar = baz ~~> foo.updateDynamic(bar)(baz)
   */
  def typedDynamicAssign(tree: untpd.Assign, pt: Type)(using Context): Tree = {
    def typedDynamicAssign(qual: untpd.Tree, name: Name, selSpan: Span, targs: List[untpd.Tree]): Tree =
      typedApply(untpd.Apply(coreDynamic(qual, nme.updateDynamic, name, selSpan, targs), tree.rhs), pt)
    tree.lhs match {
      case sel @ Select(qual, name) if !isDynamicMethod(name) =>
        typedDynamicAssign(qual, name, sel.span, Nil)
      case TypeApply(sel @ Select(qual, name), targs) if !isDynamicMethod(name) =>
        typedDynamicAssign(qual, name, sel.span, targs)
      case _ =>
        errorTree(tree, ReassignmentToVal(tree.lhs.symbol.name))
    }
  }

  private def coreDynamic(qual: untpd.Tree, dynName: Name, name: Name, selSpan: Span, targs: List[untpd.Tree])(using Context): untpd.Apply = {
    val select = untpd.Select(qual, dynName).withSpan(selSpan)
    val selectWithTypes =
      if (targs.isEmpty) select
      else untpd.TypeApply(select, targs)
    untpd.Apply(selectWithTypes, Literal(Constant(name.toString)))
  }

  /** Handle reflection-based dispatch for members of structural types.
   *
   *  Given `x.a`, where `x` is of (widened) type `T` (a value type or a nullary method type),
   *  and `x.a` is of type `U`, map `x.a` to the equivalent of:
   *
   *  ```scala
   *  x1.selectDynamic("a").asInstanceOf[U]
   *  ```
   *  where `x1` is `x` adapted to `Selectable`.
   *
   *  Given `x.a(a11, ..., a1n)...(aN1, ..., aNn)`, where `x.a` is of (widened) type
   *  `(T11, ..., T1n)...(TN1, ..., TNn): R`, it is desugared to:
   *
   *  ```scala
   *  x1.applyDynamic("a")(a11, ..., a1n, ..., aN1, ..., aNn)
   *    .asInstanceOf[R]
   *  ```
   *  If this call resolves to an `applyDynamic` method that takes a `Class[?]*` as second
   *  parameter, we further rewrite this call to
   *  scala```
   *  x1.applyDynamic("a", c11, ..., c1n, ..., cN1, ... cNn)
   *                  (a11, ..., a1n, ..., aN1, ..., aNn)
   *    .asInstanceOf[R]
   *  ```
   *  where c11, ..., cNn are the classOf constants representing the erasures of T11, ..., TNn.
   *
   *  It's an error if U is neither a value nor a method type, or a dependent method
   *  type
   */
  def handleStructural(tree: Tree)(using Context): Tree = {
    val fun @ Select(qual, name) = funPart(tree): @unchecked
    val vargss = termArgss(tree)
    val isRepeated = vargss.flatten.exists(_.tpe.widen.isRepeatedParam)

    def handleRepeated(args: List[List[Tree]]): List[Tree] =
      if isRepeated then List(untpd.TypedSplice(tpd.repeated(args.flatten, TypeTree(defn.AnyType))))
      else args.flatten.map { t =>
        val clzSym = t.tpe.resultType.classSymbol.asClass
        if ValueClasses.isDerivedValueClass(clzSym) then
          val underlying = ValueClasses.valueClassUnbox(clzSym).asTerm
          tpd.Select(t, underlying.name)
        else
          t
      }.map(untpd.TypedSplice(_))

    def structuralCall(selectorName: TermName, classOfs: => List[Tree]) = {
      val selectable = adapt(qual, defn.SelectableClass.typeRef | defn.DynamicClass.typeRef)

      // ($qual: Selectable).$selectorName("$name")
      val base =
        untpd.Apply(
          untpd.Select(untpd.TypedSplice(selectable), selectorName).withSpan(fun.span),
          (Literal(Constant(name.encode.toString)) :: Nil).map(untpd.TypedSplice(_)))

      val scall =
        if (vargss.isEmpty) base
        else untpd.Apply(base, handleRepeated(vargss))

      // If function is an `applyDynamic` that takes a Class* parameter,
      // add `classOfs`.
      def addClassOfs(tree: Tree): Tree = tree match
        case Apply(fn: Apply, args) =>
          cpy.Apply(tree)(addClassOfs(fn), args)
        case Apply(fn @ Select(_, nme.applyDynamic), nameArg :: _ :: Nil) =>
          fn.tpe.widen match
            case mt: MethodType => mt.paramInfos match
              case _ :: classOfsParam :: Nil
              if classOfsParam.isRepeatedParam
                 && classOfsParam.argInfos.head.isRef(defn.ClassClass) =>
                  val jlClassType = defn.ClassClass.typeRef.appliedTo(TypeBounds.empty)
                  cpy.Apply(tree)(fn,
                    nameArg :: seqToRepeated(SeqLiteral(classOfs, TypeTree(jlClassType))) :: Nil)
              case _ => tree
            case other => tree
        case _ => tree
      addClassOfs(typed(scall))
    }

    def fail(reason: String): Tree =
      errorTree(tree, em"Structural access not allowed on method $name because it $reason")

    extension (tree: Tree)
      /** The implementations of `selectDynamic` and `applyDynamic` in `scala.reflect.SelectDynamic` have no information about the expected return type of a value/method which was declared in the refinement,
       *  only the JVM type after erasure can be obtained through reflection, e.g.
       *
       *  class Foo(val i: Int) extends AnyVal
       *  class Reflective extends reflect.Selectable
       *  val reflective = new Reflective {
       *    def foo = Foo(1) // Foo at compile time, java.lang.Integer in reflection
       *  }
       *
       *  Because of that reflective access cannot be implemented properly in `scala.reflect.SelectDynamic` itself
       *  because it's not known there if the value should be wrapped in a value class constructor call or not.
       *  Hence the logic of wrapping is performed here, relying on the fact that the implementations of `selectDynamic` and `applyDynamic` in `scala.reflect.SelectDynamic` are final.
       */
      def maybeBoxingCast(tpe: Type) =
        val maybeBoxed =
          if ValueClasses.isDerivedValueClass(tpe.classSymbol) && qual.tpe <:< defn.ReflectSelectableTypeRef then
            val genericUnderlying = ValueClasses.valueClassUnbox(tpe.classSymbol.asClass)
            val underlying = tpe.select(genericUnderlying).widen.resultType
            New(tpe, tree.cast(underlying) :: Nil)
          else
            tree
        maybeBoxed.cast(tpe)

    fun.tpe.widen match {
      case tpe: ValueType =>
        structuralCall(nme.selectDynamic, Nil).maybeBoxingCast(tpe)

      case tpe: MethodType =>
        def isDependentMethod(tpe: Type): Boolean = tpe match {
          case tpe: MethodType =>
            tpe.isParamDependent ||
            tpe.isResultDependent ||
            isDependentMethod(tpe.resultType)
          case _ =>
            false
        }

        if (isDependentMethod(tpe))
          fail(i"has a method type with inter-parameter dependencies")
        else {
          def classOfs =
            if tpe.paramInfoss.nestedExists(!TypeErasure.hasStableErasure(_)) then
              fail(i"has a parameter type with an unstable erasure") :: Nil
            else
              TypeErasure.erasure(tpe).asInstanceOf[MethodType].paramInfos.map { tpe =>
                if ValueClasses.isDerivedValueClass(tpe.widen.classSymbol) then
                  clsOf(ValueClasses.valueClassUnbox(tpe.classSymbol.asClass).info)
                else
                  clsOf(tpe)
              }
          structuralCall(nme.applyDynamic, classOfs).maybeBoxingCast(tpe.finalResultType)
        }

      // (@allanrenucci) I think everything below is dead code
      case _: PolyType =>
        fail("is polymorphic")
      case tpe =>
        fail(i"has an unsupported type: $tpe")
    }
  }
}
