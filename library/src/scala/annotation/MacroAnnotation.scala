// TODO in which package should this class be located?
package scala
package annotation

import scala.quoted.*

/** Base trait for macro annotation implementation.
 *  Macro annotations can transform definitions and add new definitions.
 *
 *  See: `MacroAnnotation.transform`
 *
 *  @syntax markdown
 */
@experimental
trait MacroAnnotation extends StaticAnnotation:

  /** Transform the `tree` definition and add new definitions
   *
   *  This method takes as argument the annotated definition.
   *  It returns a non-empty list containing the modified version of the annotated definition.
   *  The new tree for the definition must use the original symbol.
   *  New definitions can be added to the list before or after the transformed definitions, this order
   *  will be retained. New definitions will not be visible from outside the macro expansion.
   *
   *  #### Restrictions
   *   - All definitions in the result must have the same owner. The owner can be recovered from `Symbol.spliceOwner`.
   *     - Special case: an annotated top-level `def`, `val`, `var`, `lazy val` can return a `class`/`object`
   *                     definition that is owned by the package or package object.
   *   - Can not return a `type`.
   *   - Annotated top-level `class`/`object` can not return top-level `def`, `val`, `var`, `lazy val`.
   *   - Can not see new definition in user written code.
   *
   *  #### Good practices
   *   - Make your new definitions private if you can.
   *   - New definitions added as class members should use a fresh name (`Symbol.freshName`) to avoid collisions.
   *   - New top-level definitions should use a fresh name (`Symbol.freshName`) that includes the name of the annotated
   *     member as a prefix to avoid collisions of definitions added in other files.
   *
   *  **IMPORTANT**: When developing and testing a macro annotation, you must enable `-Xcheck-macros` and `-Ycheck:all`.
   *
   *  #### Example 1
   *  This example shows how to modify a `def` and add a `val` next to it using a macro annotation.
   *  ```scala
   *  import scala.quoted.*
   *  import scala.collection.mutable
   *
   *  class memoize extends MacroAnnotation:
   *    def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
   *      import quotes.reflect.*
   *      definition match
   *        case DefDef(name, TermParamClause(param  :: Nil) :: Nil, tpt, Some(rhsTree)) =>
   *          (param.tpt.tpe.asType, tpt.tpe.asType) match
   *            case ('[t], '[u]) =>
   *              val cacheName = Symbol.freshName(name + "Cache")
   *              val cacheSymbol = Symbol.newVal(Symbol.spliceOwner, cacheName, TypeRepr.of[mutable.Map[t, u]], Flags.Private, Symbol.noSymbol)
   *              val cacheRhs =
   *                given Quotes = cacheSymbol.asQuotes
   *                '{ mutable.Map.empty[t, u] }.asTerm
   *              val cacheVal = ValDef(cacheSymbol, Some(cacheRhs))
   *              val newRhs =
   *                given Quotes = definition.symbol.asQuotes
   *                val cacheRefExpr = Ref(cacheSymbol).asExprOf[mutable.Map[t, u]]
   *                val paramRefExpr = Ref(param.symbol).asExprOf[t]
   *                val rhsExpr = rhsTree.asExprOf[u]
   *                '{ $cacheRefExpr.getOrElseUpdate($paramRefExpr, $rhsExpr) }.asTerm
   *              val newTree = DefDef.copy(definition)(name, TermParamClause(param :: Nil) :: Nil, tpt, Some(newRhs))
   *              List(cacheVal, newTree)
   *        case _ =>
   *          report.error("Annotation only supported on `def` with a single argument are supported")
   *          List(definition)
   *  ```
   *  with this macro annotation a user can write
   *  ```scala
   *  //{
   *  class memoize extends scala.annotation.StaticAnnotation
   *  //}
   *  @memoize
   *  def fib(n: Int): Int =
   *    println(s"compute fib of $n")
   *    if n <= 1 then n else fib(n - 1) + fib(n - 2)
   *  ```
   *  and the macro will modify the definition to create
   *  ```scala
   *   val fibCache$macro$1 =
   *     scala.collection.mutable.Map.empty[Int, Int]
   *   def fib(n: Int): Int =
   *     fibCache$macro$1.getOrElseUpdate(
   *       n,
   *       {
   *         println(s"compute fib of $n")
   *         if n <= 1 then n else fib(n - 1) + fib(n - 2)
   *       }
   *     )
   *  ```
   *
   *  #### Example 2
   *  This example shows how to modify a `class` using a macro annotation.
   *  It shows how to override inherited members and add new ones.
   *  ```scala
   *  import scala.annotation.{experimental, MacroAnnotation}
   *  import scala.quoted.*
   *
   *  @experimental
   *  class equals extends MacroAnnotation:
   *    def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
   *      import quotes.reflect.*
   *      definition match
   *        case ClassDef(className, ctr, parents, self, body) =>
   *          val cls = definition.symbol
   *
   *          val constructorParameters = ctr.paramss.collect { case clause: TermParamClause => clause }
   *          if constructorParameters.size != 1 || constructorParameters.head.params.isEmpty then
   *            report.errorAndAbort("@equals class must have a single argument list with at least one argument", ctr.pos)
   *          def checkNotOverridden(sym: Symbol): Unit =
   *            if sym.overridingSymbol(cls).exists then
   *              report.error(s"Cannot override ${sym.name} in a @equals class")
   *
   *          val fields = body.collect {
   *            case vdef: ValDef if vdef.symbol.flags.is(Flags.ParamAccessor) =>
   *              Select(This(cls), vdef.symbol).asExpr
   *          }
   *
   *          val equalsSym = Symbol.requiredMethod("java.lang.Object.equals")
   *          checkNotOverridden(equalsSym)
   *          val equalsOverrideSym = Symbol.newMethod(cls, "equals", equalsSym.info, Flags.Override, Symbol.noSymbol)
   *          def equalsOverrideDefBody(argss: List[List[Tree]]): Option[Term] =
   *            given Quotes = equalsOverrideSym.asQuotes
   *            cls.typeRef.asType match
   *              case '[c] =>
   *                Some(equalsExpr[c](argss.head.head.asExpr, fields).asTerm)
   *          val equalsOverrideDef = DefDef(equalsOverrideSym, equalsOverrideDefBody)
   *
   *          val hashSym = Symbol.newVal(cls, Symbol.freshName("hash"), TypeRepr.of[Int], Flags.Private | Flags.Lazy, Symbol.noSymbol)
   *          val hashVal = ValDef(hashSym, Some(hashCodeExpr(className, fields)(using hashSym.asQuotes).asTerm))
   *
   *          val hashCodeSym = Symbol.requiredMethod("java.lang.Object.hashCode")
   *          checkNotOverridden(hashCodeSym)
   *          val hashCodeOverrideSym = Symbol.newMethod(cls, "hashCode", hashCodeSym.info, Flags.Override, Symbol.noSymbol)
   *          val hashCodeOverrideDef = DefDef(hashCodeOverrideSym, _ => Some(Ref(hashSym)))
   *
   *          val newBody = equalsOverrideDef :: hashVal :: hashCodeOverrideDef :: body
   *          List(ClassDef.copy(definition)(className, ctr, parents, self, newBody))
   *        case _ =>
   *          report.error("Annotation only supports `class`")
   *          List(definition)
   *
   *    private def equalsExpr[T: Type](that: Expr[Any], thisFields: List[Expr[Any]])(using Quotes): Expr[Boolean] =
   *      '{
   *        $that match
   *          case that: T @unchecked =>
   *            ${
   *              val thatFields: List[Expr[Any]] =
   *                import quotes.reflect.*
   *                thisFields.map(field => Select('{that}.asTerm, field.asTerm.symbol).asExpr)
   *              thisFields.zip(thatFields)
   *                .map { case (thisField, thatField) => '{ $thisField == $thatField } }
   *                .reduce { case (pred1, pred2) => '{ $pred1 && $pred2 } }
   *            }
   *          case _ => false
   *      }
   *
   *    private def hashCodeExpr(className: String, thisFields: List[Expr[Any]])(using Quotes): Expr[Int] =
   *      '{
   *        var acc: Int = ${ Expr(scala.runtime.Statics.mix(-889275714, className.hashCode)) }
   *        ${
   *          Expr.block(
   *            thisFields.map {
   *              case '{ $field: Boolean } => '{ if $field then 1231 else 1237 }
   *              case '{ $field: Byte } => '{ $field.toInt }
   *              case '{ $field: Char } => '{ $field.toInt }
   *              case '{ $field: Short } => '{ $field.toInt }
   *              case '{ $field: Int } => field
   *              case '{ $field: Long } => '{ scala.runtime.Statics.longHash($field) }
   *              case '{ $field: Double } => '{ scala.runtime.Statics.doubleHash($field) }
   *              case '{ $field: Float } => '{ scala.runtime.Statics.floatHash($field) }
   *              case '{ $field: Null } => '{ 0 }
   *              case '{ $field: Unit } => '{ 0 }
   *              case field => '{ scala.runtime.Statics.anyHash($field) }
   *            }.map(hash => '{ acc = scala.runtime.Statics.mix(acc, $hash) }),
   *            '{ scala.runtime.Statics.finalizeHash(acc, ${Expr(thisFields.size)}) }
   *          )
   *        }
   *      }
   *  ```
   *  with this macro annotation a user can write
   *  ```scala
   *  //{
   *  class equals extends scala.annotation.StaticAnnotation
   *  //}
   *  @equals class User(val name: String, val id: Int)
   *  ```
   *  and the macro will modify the class definition to generate the following code
   *  ```scala
   *  class User(val name: String, val id: Int):
   *    override def equals(that: Any): Boolean =
   *      that match
   *        case that: User => this.name == that.name && this.id == that.id
   *        case _ => false
   *    private lazy val hash$macro$1: Int =
   *      var acc = 515782504 // scala.runtime.Statics.mix(-889275714, "User".hashCode)
   *      acc = scala.runtime.Statics.mix(acc, scala.runtime.Statics.anyHash(name))
   *      acc = scala.runtime.Statics.mix(acc, id)
   *      scala.runtime.Statics.finalizeHash(acc, 2)
   *    override def hashCode(): Int = hash$macro$1
   *  ```
   *
   *  @param Quotes     Implicit instance of Quotes used for tree reflection
   *  @param definition Tree that will be transformed
   *  @param companion  Tree for the companion class or module if the definition is respectively a module or a class
   *
   *  @syntax markdown
   */
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition]
