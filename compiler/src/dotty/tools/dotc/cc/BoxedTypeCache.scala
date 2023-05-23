package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*

/** A one-element cache for the boxed version of an unboxed capturing type */
class BoxedTypeCache:
  private var boxed: Type = compiletime.uninitialized
  private var unboxed: Type = NoType

  def apply(tp: AnnotatedType)(using Context): Type =
    if tp ne unboxed then
      unboxed = tp
      val CapturingType(parent, refs) = tp: @unchecked
      boxed = CapturingType(parent, refs, tp.separationSet, boxed = true)
    boxed
end BoxedTypeCache
