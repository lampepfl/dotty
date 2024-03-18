import scala.compiletime.ops.int.S

type DimensionDenotation = String & Singleton

sealed trait TensorShapeDenotation extends Product with Serializable

final case class ##:[+H <: DimensionDenotation, +T <: TensorShapeDenotation](head: H, tail: T) extends TensorShapeDenotation:
  override def toString = (head: Any) match
    case _ ##: _ => s"($head) ##: $tail"
    case _       => s"$head ##: $tail"

sealed trait TSNil extends TensorShapeDenotation
case object TSNil extends TSNil

object TensorShapeDenotation:
  type Reduce[S <: TensorShapeDenotation, Axes <: None.type | Indices] <: TensorShapeDenotation = Axes match
    case None.type => TSNil
    case Indices   => ReduceLoop[S, Axes, 0]

  protected type ReduceLoop[RemoveFrom <: TensorShapeDenotation, ToRemove <: Indices, I <: Index] <: TensorShapeDenotation = RemoveFrom match
    case head ##: tail => Indices.Contains[ToRemove, I] match
      case true  => ReduceLoop[tail, Indices.RemoveValue[ToRemove, I], S[I]]
      case false => head ##: ReduceLoop[tail, ToRemove, S[I]]
    case TSNil => ToRemove match { case INil => TSNil }
