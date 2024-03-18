import scala.compiletime.{constValueTuple, constValue}

object Sums extends App:

  println(constValueTuple[Plus[(true, true, true), (true, true)]]) // works
  println(constValueTuple[Plus1[(true, true, true), (true, true)]]) // fails
  println(constValueTuple[
    Reverse[PlusLoop[Reverse[(true, true, true)], Reverse[(true, true)], false]]]
    ) // also works despite it's just an unfold of `Plus1` application

  type Plus[A <: Tuple, B <: Tuple] <: Tuple = (A, B) match
    case (EmptyTuple, EmptyTuple) => EmptyTuple
    case (a, b)                   => Reverse[PlusLoop[Reverse[A], Reverse[B], false]]

  type Plus1[A <: Tuple, B <: Tuple]         = Reverse[PlusLoop[Reverse[A], Reverse[B], false]]

  type ReverseLoop[A, XS <: Tuple] <: Tuple = A match {
    case EmptyTuple => XS
    case x *: xs    => ReverseLoop[xs, x *: XS]
  }

  type Reverse[A] = ReverseLoop[A, EmptyTuple]

  type PlusTri[A, B, C] <: Tuple                    = (A, B, C) match
    case (false, false, false)                                              => (false, false)
    case (true, false, false) | (false, true, false) | (false, false, true) => (false, true)
    case (true, true, false) | (true, false, true) | (false, true, true)    => (true, false)
    case (true, true, true)                                                 => (true, true)

  type Inc[A <: Tuple] <: Tuple                     = A match
    case EmptyTuple => true *: EmptyTuple
    case t *: as    =>
      t match
        case false => true *: as
        case true  => false *: Inc[as]

  type IncT[A <: Tuple, O <: Boolean] <: Tuple      = O match
    case false => A
    case true  => Inc[A]

  type PlusLoop[A <: Tuple, B <: Tuple, O <: Boolean] <: Tuple = (A, B) match
    case (EmptyTuple, EmptyTuple) =>
      O match
        case true  => (true *: EmptyTuple)
        case false => EmptyTuple
    case (EmptyTuple, B)          => IncT[B, O]
    case (A, EmptyTuple)          => IncT[A, O]
    case (a *: as, b *: bs)       =>
      PlusTri[a, b, O] match
        case (x, y) => y *: PlusLoop[as, bs, x & Boolean]
