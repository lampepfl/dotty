object Test {
  // e.g inserts[z, (a, b)] =:= ((z, a, b), (a, z, b), (a, b, z))
  type inserts[a, as <: Tuple] <: Tuple =
    as match
      case EmptyTuple => (a *: EmptyTuple) *: EmptyTuple
      case y *: ys => (a *: y *: ys) *: Tuple.Map[inserts[a, ys], [t] =>> y *: (t & Tuple)]

  type inserts2[a] =
    [as <: Tuple] =>> inserts[a, as]

  type A = inserts [1, EmptyTuple]
  type B = inserts2[1][EmptyTuple]

  summon[A =:= ((1 *: EmptyTuple) *: EmptyTuple)]
  summon[B =:= ((1 *: EmptyTuple) *: EmptyTuple)]
  summon[A =:= B]

  type H[t <: Tuple] = Tuple.Concat[t, EmptyTuple]

  summon[H[A] =:= H[B]]

  summon[Tuple.Concat[A, EmptyTuple] =:= Tuple.Concat[B, EmptyTuple]]
}

object Minimized {
  type Concombre[X <: Tuple, +Y <: Tuple] <: Tuple = X match {
    case EmptyTuple => Y
    case x1 *: xs1 => X
  }

  type inserts[a, as <: Tuple] <: Tuple =
    as match
      case EmptyTuple => a *: EmptyTuple

  type inserts2[a] =
    [as <: Tuple] =>> inserts[a, as]

  type A = inserts [1, EmptyTuple]
  type B = inserts2[1][EmptyTuple]
  type C = 1 *: EmptyTuple

  summon[A =:= B]
  summon[A =:= C]
  summon[B =:= C]

  type H[t <: Tuple] = Concombre[t, EmptyTuple]

  summon[H[C] =:= H[A]]
  summon[H[C] =:= H[B]]
}
