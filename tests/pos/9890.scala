object Test {
  import scala.compiletime.ops.int._

  trait x

  type Range[Min <: Int, Max <: Int] <: Tuple = Min match {
    case Max => EmptyTuple
    case _ => Min *: Range[Min + 1, Max]
  }

  type TupleMap[Tup <: Tuple, Bound, F[_ <: Bound]] <: Tuple = Tup match {
    case EmptyTuple => EmptyTuple
    case h *: t => F[h & Bound] *: TupleMap[t, Bound, F]
  }
  type TupleDedup[Tup <: Tuple, Mask] <: Tuple = Tup match {
    case EmptyTuple => EmptyTuple
    case h *: t => h match {
      case Mask => TupleDedup[t, Mask]
      case _ => h *: TupleDedup[t, h | Mask]
    }
  }

  type CoordToPos[r <: Int, c <: Int] = r * 9 + c
  type Cell[r <: Int, c <: Int, Board <: Tuple] = Tuple.Elem[Board, CoordToPos[r, c]]
  type Col[c <: Int, Board <: Tuple] = TupleMap[Range[0, 9], Int, [r <: Int] =>> Cell[r, c, Board]]

  type ColFromPos[Pos <: Int] = Pos % 9

  type Sudoku1 = (
    x, x, x,  x, 1, x,  4, x, 6,
    8, x, 1,  6, 2, x,  x, x, 9,
    x, 3, x,  x, x, 9,  x, 2, x,

    5, x, 9,  1, 3, x,  x, 6, x,
    x, 6, x,  9, x, 2,  x, 4, x,
    x, 2, x,  x, 6, 7,  8, x, 5,

    x, 9, x,  5, x, x,  x, 3, x,
    3, x, x,  x, 4, 6,  9, x, 7,
    6, x, 7,  x, 9, x,  x, x, x,
  )

  //compiles fine
  summon[Col[ColFromPos[0], Sudoku1] =:= (x, 8, x,  5, x, x,  x, 3, 6)]

  summon[TupleDedup[(x, 8, x,  5, x, x,  x, 3, 6), Nothing] =:= (x, 8, 5, 3, 6)]
  //but this doesn't
  summon[TupleDedup[Col[ColFromPos[0], Sudoku1], Nothing] =:= (x, 8, 5, 3, 6)]
}
