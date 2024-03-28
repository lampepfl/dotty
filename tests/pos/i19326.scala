
import OpaqueScope.Opaque

type MapSum[D1, D2] = D1 match
  case Opaque[l1] => D2 match
    case Opaque[l2] => Opaque[Sum[l1, l2]]

object OpaqueScope:
  opaque type Opaque[N <: NatT] = Double

  val foo: MapSum[Opaque[Succ[Zero]], Opaque[Zero]] = 1.0
end OpaqueScope

sealed trait NatT derives CanEqual
case class Zero() extends NatT
case class Succ[N <: NatT](n: N) extends NatT

type Sum[M <: NatT, N <: NatT] <: NatT = (M, N) match
  case (_, Zero) => M
  case (Zero, _) => N
  case (Succ[predM], Succ[predN]) => Succ[Succ[Sum[predM, predN]]]


object Minimized:

  object OpaqueScope:
    opaque type Opaque[N <: Any] = Double
    val foo: StripOpaque[Opaque[Int]] = 1

    // Note that match types within OpaqueScope are not limited by the absence of dealiasing
    type NeedsRhs[D] = D match
      case Double => String
    val bar: NeedsRhs[Opaque[Float]] = "hello"

  type StripOpaque[D] = D match
    case OpaqueScope.Opaque[x] => x

