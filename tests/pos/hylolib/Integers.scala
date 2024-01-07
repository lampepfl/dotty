package hylo

given Boolean is Value:

  extension (self: Boolean)

    def copy(): Boolean =
      // Note: Scala's `Boolean` has value semantics already.
      self

    def eq(other: Boolean): Boolean =
      self == other

    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(if self then 1 else 0)

given Int is Value:

  extension (self: Int)

    def copy(): Int =
      // Note: Scala's `Int` has value semantics already.
      self

    def eq(other: Int): Boolean =
      self == other

    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(self)

given Int is Comparable:

  extension (self: Int)

    def copy(): Int =
      self

    def eq(other: Int): Boolean =
      self == other

    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(self)

    def lt(other: Int): Boolean = self < other

given Int is StringConvertible
