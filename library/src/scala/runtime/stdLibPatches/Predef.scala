package scala.runtime.stdLibPatches

object Predef:
  import compiletime.summonFrom

  transparent inline def assert(inline assertion: Boolean, inline message: => Any): Unit =
    if !assertion then scala.runtime.Scala3RunTime.assertFailed(message)

  transparent inline def assert(inline assertion: Boolean): Unit =
    if !assertion then scala.runtime.Scala3RunTime.assertFailed()

  /**
   * Retrieve the single value of a type with a unique inhabitant.
   *
   * @example {{{
   * object Foo
   * val foo = valueOf[Foo.type]
   * // foo is Foo.type = Foo
   *
   * val bar = valueOf[23]
   * // bar is 23.type = 23
   * }}}
   * @group utilities
   */
  inline def valueOf[T]: T = summonFrom {
    case ev: ValueOf[T] => ev.value
  }

  /** Summon a given value of type `T`. Usually, the argument is not passed explicitly.
   *
   *  @tparam T the type of the value to be summoned
   *  @return the given value typed: the provided type parameter
   */
  transparent inline def summon[T](using inline x: T): x.type = x

  /** Apply a non-implicit conversion automatically.
   *
   *  @tparam T the type of the value to be converted
   *  @return the converted value
   */

  extension [T](x: T)
    inline def convertTo[A](using c: T => A) = c(x)

  // Extension methods for working with explicit nulls

  /** Strips away the nullability from a value.
   *  e.g.
   *    val s1: String|Null = "hello"
   *    val s: String = s1.nn
   *
   *  Note that `.nn` performs a checked cast, so if invoked on a null value it'll throw an NPE.
   */
  extension [T](x: T | Null) inline def nn: x.type & T =
    scala.runtime.Scala3RunTime.nn(x)
end Predef
