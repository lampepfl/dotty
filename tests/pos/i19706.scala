
import scala.compiletime.ops.string.{Length, Matches, Substring}

def emptyContext(): Unit =
  summon[Decoded["Tuple(0, EmptyTuple)"] =:= 0 *: EmptyTuple]

type Decoded[S <: String] = Matches[S, "Tuple(.+, .+)"] match
  case true => Parsed[Substring[S, 6, 19], 0, ""] match
    case (h, t) => Decoded["0"] *: EmptyTuple
  case false => 0

type Parsed[S <: String, I <: Int, A <: String] <: (String, String) = Matches[S, "other"] match
  case true => I match
    case 1 => ("", "")
    case _ => Parsed[Substring[S, 1, Length[S]], I, ""]
  case false => ("0", "EmptyTuple")


object Minimization:

  type Cond[B <: Boolean] <: Tuple2[String, String] = B match
    case true => ("", "")
    case false => ("a", "b")

  type Decoded[B <: Boolean] = Cond[B] match
    case (h1, _) => Int

  val _: Decoded[false] = 1

