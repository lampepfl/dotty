package hylo

/** A type whose instances can be described by a character string. */
trait StringConvertible[Self] {

  extension (self: Self) {

    /** Returns a textual description of `self`. */
    def description: String =
      self.toString

  }

}
