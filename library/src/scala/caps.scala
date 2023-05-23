package scala

import annotation.experimental
import annotation.retains

@experimental object caps:

  /** The universal capture reference (deprecated) */
  @deprecated("Use `cap` instead")
  val `*`: Any = ()

  /** The universal capture reference */
  val cap: Any = ()

  /** The reader capture reference */
  val rdr: Any @retains(cap) = ()

  object unsafe:

    extension [T](x: T)
      /** If argument is of type `cs T`, converts to type `box cs T`. This
      *  avoids the error that would be raised when boxing `*`.
      */
      @deprecated(since = "3.3")
      def unsafeBox: T = x

      /** If argument is of type `box cs T`, converts to type `cs T`. This
       *  avoids the error that would be raised when unboxing `*`.
       */
      @deprecated(since = "3.3")
      def unsafeUnbox: T = x

    extension [T, U](f: T => U)
      /** If argument is of type `box cs T`, converts to type `cs T`. This
       *  avoids the error that would be raised when unboxing `*`.
       */
      @deprecated(since = "3.3")
      def unsafeBoxFunArg: T => U = f

  end unsafe

  /** An annotation that expresses the sealed modifier on a type parameter
   *  Should not be directly referred to in source
   */
  @deprecated("The Sealed annotation should not be directly used in source code.\nUse the `sealed` modifier on type parameters instead.")
  class Sealed extends annotation.Annotation

  /** Mixing in this trait forces a trait or class to be pure, i.e.
   *  have no capabilities retained in its self type.
   */
  trait Pure:
    this: Pure =>

  /** Marker trait for reader classes. */
  trait ReadOnly
