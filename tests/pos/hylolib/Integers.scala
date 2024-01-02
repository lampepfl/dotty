package hylo

given Boolean is Value with {

  extension (self: Boolean) {

    def copy(): Boolean =
      // Note: Scala's `Boolean` has value semantics already.
      self

    def eq(other: Boolean): Boolean =
      self == other

    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(if self then 1 else 0)

  }

}

given Int is Value with {

  extension (self: Int) {

    def copy(): Int =
      // Note: Scala's `Int` has value semantics already.
      self

    def eq(other: Int): Boolean =
      self == other

    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(self)

  }

}

given Int is Comparable with {

  extension (self: Int) {

    def copy(): Int =
      self

    def eq(other: Int): Boolean =
      self == other

    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(self)

    def lt(other: Int): Boolean = self < other

  }

}

given intIsStringConvertible: StringConvertible[Int] with {}
