package hylo

/** A type whose instance can be treated as independent values.
  *
  * The data structure of and algorithms of Hylo's standard library operate "notional values" rather
  * than arbitrary references. This trait defines the basis operations of all values.
  */
trait Value[Self] {

  extension (self: Self) {

    /** Returns a copy of `self`. */
    def copy(): Self

    /** Returns `true` iff `self` and `other` have an equivalent value. */
    def eq(other: Self): Boolean

    /** Hashes the salient parts of `self` into `hasher`. */
    def hashInto(hasher: Hasher): Hasher

  }

}

extension [Self: Value](self: Self) def neq(other: Self): Boolean = !self.eq(other)

// ----------------------------------------------------------------------------
// Comparable
// ----------------------------------------------------------------------------

trait Comparable[Self] extends Value[Self] {

  extension (self: Self) {

    /** Returns `true` iff `self` is ordered before `other`. */
    def lt(other: Self): Boolean

    /** Returns `true` iff `self` is ordered after `other`. */
    def gt(other: Self): Boolean = other.lt(self)

    /** Returns `true` iff `self` is equal to or ordered before `other`. */
    def le(other: Self): Boolean = !other.lt(self)

    /** Returns `true` iff `self` is equal to or ordered after `other`. */
    def ge(other: Self): Boolean = !self.lt(other)

  }

}

/** Returns the lesser of `x` and `y`. */
def min[T: Comparable](x: T, y: T): T =
  if y.lt(x) then y else x

/** Returns the greater of `x` and `y`. */
def max[T: Comparable](x: T, y: T): T =
  if x.lt(y) then y else x
