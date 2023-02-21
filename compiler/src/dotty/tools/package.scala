package dotty

import scala.compiletime.*
import dotty.tools.dotc.core.Contexts.*

package object tools {

  val ListOfNil: List[Nil.type] = Nil :: Nil

  /** Throws an `UnsupportedOperationException` with the given method name. */
  def unsupported(methodName: String): Nothing =
    throw new UnsupportedOperationException(methodName)

  /** Forward-ported from the explicit-nulls branch. */
  extension [T](x: T | Null)
    /** Should be used when we know from the context that `x` is not null.
     *  Flow-typing under explicit nulls will automatically insert many necessary
     *  occurrences of uncheckedNN.
     */
    transparent inline def uncheckedNN: T = x.asInstanceOf[T]

    inline def toOption: Option[T] =
      if x == null then None else Some(x.asInstanceOf[T])
  end extension

  /** Nullable eq and ne. */
  extension [T <: AnyRef](x: T | Null)
    inline def eqn (y: T | Null) =
      x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]

    inline def nen(y: T | Null): Boolean = !eqn(y)

  object resultWrapper {
    opaque type WrappedResult[T] = T
    private[tools] def unwrap[T](x: WrappedResult[T]): T = x
    private[tools] def wrap[T](x: T): WrappedResult[T] = x
  }
  type WrappedResult[T] = resultWrapper.WrappedResult[T]
  def WrappedResult[T](x: T) = resultWrapper.wrap(x)
  def result[T](using x: WrappedResult[T]): T = resultWrapper.unwrap(x)

  def unreachable(x: Any = "<< this case was declared unreachable >>"): Nothing =
    throw new MatchError(x)
  

  transparent inline def assertShort(inline assertion: Boolean, inline message: Any = null): Unit =
    if !assertion then
    summonFrom {
      case ctx: Context =>  throwAssertionError(message, ctx, truncateStack = true)
      case _ => throwAssertionError(message, null, truncateStack = true)
    }
    
  transparent inline def assert(inline assertion: Boolean): Unit =
    assert(assertion, null)
  
  transparent inline def assert(inline assertion: Boolean, inline message: Any): Unit =
    if !assertion then
    summonFrom {
      case ctx: Context =>  throwAssertionError(message, ctx)
      case _ => throwAssertionError(message, null)
    }

  // extracted from `assert` to make it as small (and inlineable) as possible
  private def throwAssertionError(message: Any | Null, ctx: Context | Null, truncateStack: Boolean = false): Nothing =

    val assertionError = AssertionError("assertion failed: " + String.valueOf(message).nn)
    if truncateStack then assertionError.setStackTrace(Array())
    if ctx == null then
      throw assertionError
    else 
      ctx.implode(assertionError)

  // Ensure this object is already classloaded, since it's only actually used
  // when handling stack overflows and every operation (including class loading)
  // risks failing.
  dotty.tools.dotc.core.handleRecursive
 }
