type Fold0[Tup <: Tuple, Z, F[_, _]] = Tup match
  case EmptyTuple => Z
  case h *: t     => F[h, Fold0[t, Z, F]]


type Union0[T <: Tuple] = Fold0[T, Nothing, [x, y] =>> x | y]

type Union1[Tup <: Tuple] = Tup match
  case EmptyTuple => Nothing
  case h *: t     => h | Union1[t]


import Tuple.Map as Map0

type Map1[Tup <: Tuple, F[_ <: Union0[Tup]]] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t     => F[h & Union0[Tup]] *: Map1[t, [x <: Union0[t]] =>> F[x & Union0[Tup]]]

type Map2[Tup <: Tuple, F[_]] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t     => F[h] *: Map2[t, F]

//type Map3[Tup <: Tuple, F[_ <: Union1[Tup]]] <: Tuple = Tup match
//  case EmptyTuple => EmptyTuple
//  case h *: t     => F[h] *: Map3[t, F]

type Map4  [Tup <: Tuple, F[_ <: Union1[Tup]]] = Map4UB[Tup, F, Union1[Tup]]
type Map4UB[Tup <: Tuple, F[_ <: UB], UB] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t     => F[h & UB] *: Map4UB[t, F, UB]

type Map5  [Tup <: Tuple, F[_ <: Union1[Tup]]] = Map5UB[Tup, Union1[Tup], F, Tup]
type Map5UB[Tup <: Tuple, UB, F[_ <: UB], Tup1 <: Tuple] <: Tuple = Tup1 match
  case EmptyTuple => EmptyTuple
  case h *: t     => F[h & UB] *: Map5UB[Tup, UB, F, t]

trait Dt[T]
case class IBox[A <: Int](v: A)

class Test[H, T <: Tuple]:
//def t0 = { val x: Dt[H] *: Map0[T, Dt] = ???; val y: Map0[H *: T, Dt] = x }
  def t1 = { val x: Dt[H] *: Map1[T, Dt] = ???; val y: Map1[H *: T, Dt] = x }
  def t2 = { val x: Dt[H] *: Map2[T, Dt] = ???; val y: Map2[H *: T, Dt] = x }
//def t3 = { val x: Dt[H] *: Map3[T, Dt] = ???; val y: Map3[H *: T, Dt] = x }
//def t4 = { val x: Dt[H] *: Map4[T, Dt] = ???; val y: Map4[H *: T, Dt] = x }
//def t5 = { val x: Dt[H] *: Map5[T, Dt] = ???; val y: Map5[H *: T, Dt] = x }

  def i0 = { val x: Map0[(1, 2), IBox] = (IBox(1), IBox(2)) }
  def i1 = { val x: Map1[(1, 2), IBox] = (IBox(1), IBox(2)) }
//def i2 = { val x: Map2[(1, 2), IBox] = (IBox(1), IBox(2)) }
//def i3 = { val x: Map3[(1, 2), IBox] = (IBox(1), IBox(2)) }
  def i4 = { val x: Map4[(1, 2), IBox] = (IBox(1), IBox(2)) }
  def i5 = { val x: Map5[(1, 2), IBox] = (IBox(1), IBox(2)) }
