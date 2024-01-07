//> using options -language:experimental.modularity -source future
package hylotest

trait Value[Self]

/** A collection of elements accessible by their position. */
trait Collection[Self]:

  /** The type of the elements in the collection. */
  type Element
  given elementIsValue: Value[Element] = deferredSummon

class BitArray

given Value[Boolean] {}

given Collection[BitArray] with
  type Element = Boolean
