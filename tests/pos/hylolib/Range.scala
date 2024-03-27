package hylo

/** A half-open interval from a lower bound up to, but not including, an uppor bound. */
final class Range[Bound] private (val lowerBound: Bound, val upperBound: Bound) {

  /** Returns a textual description of `this`. */
  override def toString: String =
    s"[${lowerBound}, ${upperBound})"

}

object Range {

  /** Creates a half-open interval [`lowerBound`, `upperBound`), using `isLessThanOrEqual` to ensure
    * that the bounds are well-formed.
    *
    * @requires
    *   `lowerBound` is lesser than or equal to `upperBound`.
    */
  def apply[Bound](
      lowerBound: Bound,
      upperBound: Bound,
      isLessThanOrEqual: (Bound, Bound) => Boolean
  ) =
    require(isLessThanOrEqual(lowerBound, upperBound))
    new Range(lowerBound, upperBound)

  /** Creates a half-open interval [`lowerBound`, `upperBound`).
    *
    * @requires
    *   `lowerBound` is lesser than or equal to `upperBound`.
    */
  def apply[Bound: Comparable](lowerBound: Bound, upperBound: Bound) =
    require(lowerBound `le` upperBound)
    new Range(lowerBound, upperBound)

}
