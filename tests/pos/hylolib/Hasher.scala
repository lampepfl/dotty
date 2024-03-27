//> using options -language:experimental.modularity -source future
package hylo

import scala.util.Random

/** A universal hash function. */
final class Hasher private (private val hash: Int = Hasher.offsetBasis) {

  /** Returns the computed hash value. */
  def finalizeHash(): Int =
    hash

  /** Adds `n` to the computed hash value. */
  def combine(n: Int): Hasher =
    var h = hash
    h = h ^ n
    h = h * Hasher.prime
    new Hasher(h)
}

object Hasher {

  private val offsetBasis = 0x811c9dc5
  private val prime = 0x01000193

  /** A random seed ensuring different hashes across multiple runs. */
  private lazy val seed = scala.util.Random.nextInt()

  /** Creates an instance with the given `seed`. */
  def apply(): Hasher =
    val h = new Hasher()
    h.combine(seed)
    h

  /** Returns the hash of `v`. */
  def hash[T: Value](v: T): Int =
    v.hashInto(Hasher()).finalizeHash()

}
