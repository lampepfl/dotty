import scala.compiletime.ops.int.{S, +, <, <=, *}
import scala.compiletime.ops.boolean.&&

type Dimension = Int & Singleton

sealed trait Shape extends Product with Serializable

final case class #:[+H <: Dimension, +T <: Shape](head: H, tail: T) extends Shape:
  override def toString = (head: Any) match
    case _ #: _ => s"($head) #: $tail"
    case _      => s"$head #: $tail"

sealed trait SNil extends Shape
case object SNil extends SNil

object Shape:
  def scalar: SNil = SNil

  type Concat[X <: Shape, Y <: Shape] <: Shape = X match
    case SNil         => Y
    case head #: tail => head #: Concat[tail, Y]

  type Reverse[X <: Shape] <: Shape = X match
    case SNil         => SNil
    case head #: tail => Concat[Reverse[tail], head #: SNil]

  type NumElements[X <: Shape] <: Int = X match
    case SNil         => 1
    case head #: tail => head * NumElements[tail]

  type Rank[X <: Shape] <: Int = X match
    case SNil         => 0
    case head #: tail => Rank[tail] + 1

  type IsEmpty[X <: Shape] <: Boolean = X match
    case SNil   => true
    case _ #: _ => false

  type Head[X <: Shape] <: Dimension = X match { case head #: _ => head }
  type Tail[X <: Shape] <: Shape     = X match { case _ #: tail => tail }

  type Reduce[S <: Shape, Axes <: None.type | Indices] <: Shape = Axes match
    case None.type => SNil
    case Indices   => ReduceLoop[S, Axes, 0]

  protected type ReduceLoop[RemoveFrom <: Shape, ToRemove <: Indices, I <: Index] <: Shape = RemoveFrom match
    case head #: tail => Indices.Contains[ToRemove, I] match
      case true  => ReduceLoop[tail, Indices.RemoveValue[ToRemove, I], S[I]]
      case false => head #: ReduceLoop[tail, ToRemove, S[I]]
    case SNil => ToRemove match { case INil => SNil }

  type WithinBounds[I <: Index, S <: Shape] = (0 <= I && I < Rank[S])

  type RemoveIndex[RemoveFrom <: Shape, I <: Index] <: Shape = WithinBounds[I, RemoveFrom] match
    case true => RemoveIndexLoop[RemoveFrom, I, 0]

  protected type RemoveIndexLoop[RemoveFrom <: Shape, I <: Index, Current <: Index] <: Shape = RemoveFrom match
    case head #: tail => Current match
      case I => tail
      case _ => head #: RemoveIndexLoop[tail, I, S[Current]]

  type Map[X <: Shape, F[_ <: Dimension] <: Dimension] <: Shape = X match
    case SNil         => SNil
    case head #: tail => F[head] #: Map[tail, F]

  type FoldLeft[B, X <: Shape, Z <: B, F[_ <: B, _ <: Int] <: B] <: B = X match
    case SNil         => Z
    case head #: tail => FoldLeft[B, tail, F[Z, head], F]
