import scala.compiletime.ops.int.*

type Index     = Int & Singleton
type Dimension = Int & Singleton

sealed trait Indices extends Product with Serializable
sealed trait Shape   extends Product with Serializable
final case class :::[+H <: Index,     +T <: Indices](head: H, tail: T) extends Indices
final case class  #:[+H <: Dimension, +T <: Shape  ](head: H, tail: T) extends Shape
sealed trait INil extends Indices; case object INil extends INil
sealed trait SNil extends Shape;   case object SNil extends SNil

object Ts:
  type ReduceKeepDims[S <: Shape, AxisIndices <: None.type | Indices] <: Shape = AxisIndices match
    case None.type => SNil
    case Indices   => ReduceKeepDimsLoop[S, AxisIndices, 0]

  protected type ReduceKeepDimsLoop[ReplaceFrom <: Shape, ToReplace <: Indices, I <: Index] <: Shape = ReplaceFrom match
    case head #: tail => ReduceKeepDimsLoop[tail, ToReplace, S[I]]
    case SNil         => ToReplace match { case INil => SNil }
