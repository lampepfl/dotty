//> using options -language:experimental.modularity -source future
package hylotest

trait Value:
  type Self
  extension (self: Self) def eq(other: Self): Boolean

/** A collection of elements accessible by their position. */
trait Collection:
  type Self

  /** The type of the elements in the collection. */
  type Element: Value

class BitArray

given Boolean is Value:
  extension (self: Self) def eq(other: Self): Boolean =
    self == other

given BitArray is Collection:
  type Element = Boolean

extension [Self: Value](self: Self)
  def neq(other: Self): Boolean = !self.eq(other)

extension [Self: Collection](self: Self)
  def elementsEqual[T: Collection { type Element = Self.Element } ](other: T): Boolean =
    ???
