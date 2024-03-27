package scala
package compiletime

import annotation.compileTimeOnly

/** Use this method when you have a type, do not have a value for it but want to
 *  pattern match on it. For example, given a type `Tup <: Tuple`, one can
 *  pattern-match on it as follows:
 *  ```scala
 *  //{
 *  type Tup
 *  inline def f = {
 *  //}
 *  inline erasedValue[Tup] match {
 *    case _: EmptyTuple => ???
 *    case _: (h *: t) => ???
 *  }
 *  //{
 *  }
 *  //}
 *  ```
 *  This value can only be used in an inline match and the value cannot be used in
 *  the branches.
 *  @syntax markdown
 */
// TODO add `erased` once it is not an experimental feature anymore
def erasedValue[T]: T = erasedValue[T]

/** Used as the initializer of a mutable class or object field, like this:
 *
 *  ```scala
 *  //{
 *  type T
 *  //}
 *  var x: T = uninitialized
 *  ```
 *
 *  This signifies that the field is not initialized on its own. It is still initialized
 *  as part of the bulk initialization of the object it belongs to, which assigns zero
 *  values such as `null`, `0`, `0.0`, `false` to all object fields.
 */
@compileTimeOnly("`uninitialized` can only be used as the right hand side of a mutable field definition")
def uninitialized: Nothing = ???

/** Used as the right hand side of a given in a trait, like this
 *
 *  ```
 *  given T = deferred
 *  ```
 *
 *  This signifies that the given will get a synthesized definition in all classes
 *  that implement the enclosing trait and that do not contain an explicit overriding
 *  definition of that given.
 */
@compileTimeOnly("`deferred` can only be used as the right hand side of a given definition in a trait")
def deferred: Nothing = ???

/** The error method is used to produce user-defined compile errors during inline expansion.
 *  If an inline expansion results in a call error(msgStr) the compiler produces an error message containing the given msgStr.
 *
 *  ```scala sc:fail
 *  error("My error message")
 *  ```
 *  or
 *  ```scala
 *  inline def errorOnThisCode(inline x: Any) =
 *    error("My error of this code: " + codeOf(x))
 *  ```
 *  @syntax markdown
 */
inline def error(inline msg: String): Nothing = ???

/** Returns the string representation of argument code:
 *
 *  ```scala
 *  inline def logged(inline p1: Any) =
 *    ("code: " + codeOf(p1), p1)
 *
 *  logged(identity("foo"))
 *  // above is equivalent to:
 *  // ("code: scala.Predef.identity("foo")", identity("foo"))
 *  ```
 *
 *  The formatting of the code is not stable across version of the compiler.
 *
 *  @note only `inline` arguments will be displayed as "code".
 *        Other values may display unintutively.
 *
 *  @syntax markdown
 */
transparent inline def codeOf(arg: Any): String =
  // implemented in dotty.tools.dotc.typer.Inliner.Intrinsics
  error("Compiler bug: `codeOf` was not evaluated by the compiler")

/** Checks at compiletime that the provided values is a constant after
 *  inlining and constant folding.
 *
 *  Usage:
 *  ```scala sc:fail
 *  inline def twice(inline n: Int): Int =
 *    requireConst(n) // compile-time assertion that the parameter `n` is a constant
 *    n + n
 *
 *  twice(1)
 *  val m: Int = ???
 *  twice(m) // error: expected a constant value but found: m
 *  ```
 *  @syntax markdown
 */
inline def requireConst(inline x: Boolean | Byte | Short | Int | Long | Float | Double | Char | String): Unit =
  // implemented in dotty.tools.dotc.typer.Inliner
  error("Compiler bug: `requireConst` was not evaluated by the compiler")

/** Same as `constValue` but returns a `None` if a constant value
 *  cannot be constructed from the provided type. Otherwise returns
 *  that value wrapped in `Some`.
 */
transparent inline def constValueOpt[T]: Option[T] =
  // implemented in dotty.tools.dotc.typer.Inliner
  error("Compiler bug: `constValueOpt` was not evaluated by the compiler")

/** Given a constant, singleton type `T`, convert it to a value
 *  of the same singleton type. For example: `assert(constValue[1] == 1)`.
 */
transparent inline def constValue[T]: T =
  // implemented in dotty.tools.dotc.typer.Inliner
  error("Compiler bug: `constValue` was not evaluated by the compiler")

/** Given a tuple type `(X1, ..., Xn)`, returns a tuple value
 *  `(constValue[X1], ..., constValue[Xn])`.
 */
inline def constValueTuple[T <: Tuple]: T =
  // implemented in dotty.tools.dotc.typer.Inliner
  error("Compiler bug: `constValueTuple` was not evaluated by the compiler")


/** Summons first given matching one of the listed cases. E.g. in
 *
 *  ```scala
 *  //{
 *  type A
 *  trait B
 *  type C
 *  inline def f = {
 *  //}
 *  given B with { }
 *
 *  summonFrom {
 *    case given A => 1
 *    case given B => 2
 *    case given C => 3
 *    case _ => 4
 *  }
 *  //{
 *  }
 *  //}
 *  ```
 *  the returned value would be `2`.
 *  @syntax markdown
 */
transparent inline def summonFrom[T](f: Nothing => T): T =
  error("Compiler bug: `summonFrom` was not evaluated by the compiler")

/** Summon a given value of type `T`. Usually, the argument is not passed explicitly.
 *  The summoning is delayed until the call has been fully inlined.
 *
 *  @tparam T the type of the value to be summoned
 *  @return the given value typed as the provided type parameter
 */
transparent inline def summonInline[T]: T =
  error("Compiler bug: `summonInline` was not evaluated by the compiler")

/** Given a tuple T, summons each of its member types and returns them in
 *  a Tuple.
 *
 *  @tparam T the tuple containing the types of the values to be summoned
 *  @return the given values typed as elements of the tuple
 */
inline def summonAll[T <: Tuple]: T =
  // implemented in dotty.tools.dotc.typer.Inliner
  error("Compiler bug: `summonAll` was not evaluated by the compiler")

/** Assertion that an argument is by-name. Used for nullability checking. */
def byName[T](x: => T): T = x

/** Casts a value to be `Matchable`. This is needed if the value's type is an unconstrained
  *  type parameter and the value is the scrutinee of a match expression.
  *  This is normally disallowed since it violates parametricity and allows
  *  to uncover implementation details that were intended to be hidden.
  *  The `asMatchable` escape hatch should be used sparingly. It's usually
  *  better to constrain the scrutinee type to be `Matchable` in the first place.
  */
extension [T](x: T)
  transparent inline def asMatchable: x.type & Matchable = x.asInstanceOf[x.type & Matchable]
