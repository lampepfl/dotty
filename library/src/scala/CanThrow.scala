package scala
import language.experimental.erasedDefinitions
import annotation.{implicitNotFound, experimental, capability}

/**
 * A capability class that allows to throw exception `E`. When used with the
 *  experimental.saferExceptions feature, a `throw Ex()` expression will require
 *  a given of class `CanThrow[Ex]` to be available.
 */
@experimental @capability
@implicitNotFound("The capability to throw exception of type ${E} is missing.\nThe capability can be provided by one of the following:\n - Adding `throws ${E}` clause after the parameter list of the enclosing method\n - Adding `throws ${E}` clause after the result type of the enclosing method\n - Wrapping this piece of code with a `try` block that catches ${E}")
erased class CanThrow[-E <: Exception]

@experimental
object CanThrow:
  def apply[A <: Exception, B <: Exception](ct1: CanThrow[A], ct2: CanThrow[B]): CanThrow[A | B] =
    compiletime.erasedValue

@experimental
object unsafeExceptions:
  given canThrowAny: CanThrow[Exception] = compiletime.erasedValue

