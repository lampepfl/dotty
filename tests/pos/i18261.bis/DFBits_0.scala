trait DFBits[W <: Int]

trait Candidate[R]:
  type OutW <: Int
object Candidate:
  given [W <: Int, R <: Foo[DFBits[W], VAL]]: Candidate[R] with
    type OutW = W
