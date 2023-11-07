package scala.quoted.runtime

import scala.annotation.{Annotation, compileTimeOnly}
import scala.annotation.experimental

@compileTimeOnly("Illegal reference to `scala.quoted.runtime.Patterns`")
object Patterns {

  /** A splice in a quoted pattern is desugared by the compiler into a call to this method.
   *
   *  Calling this method in source has undefined behavior at compile-time
   */
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Patterns.patternHole`")
  def patternHole[T]: T = ???

  /** A higher order splice in a quoted pattern is desugared by the compiler into a call to this method.
   *
   *  Calling this method in source has undefined behavior at compile-time
   */
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Patterns.patternHigherOrderHole`")
  def patternHigherOrderHole[U](pat: Any, args: Any*): U = ???

  /** A higher order splice in a quoted pattern is desugared by the compiler into a call to this method.
   *
   *  Calling this method in source has undefined behavior at compile-time
   */
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Patterns.higherOrderHole`")
  def higherOrderHole[U](args: Any*): U = ???

  /** A higher order splice in a quoted pattern is desugared by the compiler into a call to this method.
   *
   *  Calling this method in source has undefined behavior at compile-time
   */
  @experimental
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Patterns.higherOrderHoleWithTypes`")
  def higherOrderHoleWithTypes[U, T](args: Any*): U = ???

  /** A splice of a name in a quoted pattern is that marks the definition of a type splice.
   *
   *  Adding this annotation in source has undefined behavior at compile-time
   */
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Patterns.patternType`")
  class patternType extends Annotation

  /** A type pattern that must be approximated from above
   *
   *  Adding this annotation in source has undefined behavior at compile-time
   */
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Patterns.fromAbove`")
  class fromAbove extends Annotation

}
