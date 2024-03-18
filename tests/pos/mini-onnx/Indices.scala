import scala.compiletime.ops.string.+
import scala.compiletime.ops.any

type Index = Int & Singleton

sealed trait Indices

final case class :::[+H <: Index, +T <: Indices](head: H, tail: T) extends Indices:
  override def toString = s"$head ::: $tail"

sealed trait INil extends Indices
case object INil extends INil

object Indices:
  type ToString[X <: Indices] <: String = X match
    case INil          => "INil"
    case head ::: tail => any.ToString[head] + " ::: " + ToString[tail]

  type Contains[Haystack <: Indices, Needle <: Index] <: Boolean = Haystack match
    case head ::: tail => head match
      case Needle => true
      case _      => Contains[tail, Needle]
    case INil => false

  type RemoveValue[RemoveFrom <: Indices, Value <: Index] <: Indices = RemoveFrom match
    case INil          => INil
    case head ::: tail => head match
      case Value => RemoveValue[tail, Value]
      case _     => head ::: RemoveValue[tail, Value]
