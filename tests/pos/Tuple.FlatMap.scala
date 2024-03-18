type FlatMap[Tup <: Tuple, F[_] <: Tuple] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t     => Tuple.Concat[F[h], FlatMap[t, F]]
