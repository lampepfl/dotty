package hylo

/** A type whose instances can be described by a character string. */
trait StringConvertible:
  type Self

  /** Returns a textual description of `self`. */
  extension (self: Self)
    def description: String = self.toString
