package hylo

given Boolean forms Value:

  extension (self: Boolean)

    def copy(): Boolean =
      // Note: Scala's `Boolean` has value semantics already.
      self

    def eq(other: Boolean): Boolean =
      self == other

    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(if self then 1 else 0)

given Int forms Value:

  extension (self: Int)

    def copy(): Int =
      // Note: Scala's `Int` has value semantics already.
      self

    def eq(other: Int): Boolean =
      self == other

    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(self)

given Int forms Comparable:

  extension (self: Int)

    def copy(): Int =
      self

    def eq(other: Int): Boolean =
      self == other

    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(self)

    def lt(other: Int): Boolean = self < other

given Int forms StringConvertible
