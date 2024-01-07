package hylo

/** A wrapper around an object providing a reference API. */
private final class Ref[T](val value: T) {

  override def toString: String =
    s"Ref($value)"

}

/** A type-erased value.
  *
  * An `AnyValue` forwards its operations to a wrapped value, hiding its implementation.
  */
final class AnyValue private (
    private val wrapped: AnyRef,
    private val _copy: (AnyRef) => AnyValue,
    private val _eq: (AnyRef, AnyRef) => Boolean,
    private val _hashInto: (AnyRef, Hasher) => Hasher
) {

  /** Returns a copy of `this`. */
  def copy(): AnyValue =
    _copy(this.wrapped)

  /** Returns `true` iff `this` and `other` have an equivalent value. */
  def eq(other: AnyValue): Boolean =
    _eq(this.wrapped, other.wrapped)

  /** Hashes the salient parts of `this` into `hasher`. */
  def hashInto(hasher: Hasher): Hasher =
    _hashInto(this.wrapped, hasher)

  /** Returns the value wrapped in `this` as an instance of `T`. */
  def unsafelyUnwrappedAs[T]: T =
    wrapped.asInstanceOf[Ref[T]].value

  /** Returns a textual description of `this`. */
  override def toString: String =
    wrapped.toString

}

object AnyValue {

  /** Creates an instance wrapping `wrapped`. */
  def apply[T](using Value[T])(wrapped: T): AnyValue =
    def copy(a: AnyRef): AnyValue =
      AnyValue(a.asInstanceOf[Ref[T]].value.copy())

    def eq(a: AnyRef, b: AnyRef): Boolean =
      a.asInstanceOf[Ref[T]].value `eq` b.asInstanceOf[Ref[T]].value

    def hashInto(a: AnyRef, hasher: Hasher): Hasher =
      a.asInstanceOf[Ref[T]].value.hashInto(hasher)

    new AnyValue(Ref(wrapped), copy, eq, hashInto)

}

given anyValueIsValue: Value[AnyValue] with {

  extension (self: AnyValue) {

    def copy(): AnyValue =
      self.copy()

    def eq(other: AnyValue): Boolean =
      self `eq` other

    def hashInto(hasher: Hasher): Hasher =
      self.hashInto(hasher)

  }

}
