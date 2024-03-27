//> using options -experimental -Yno-experimental

import scala.Tuple.*

def test[T1, T2, T3, T4] =
  summon[ReverseOnto[(T1, T2), (T3, T4)] =:= ReverseOnto[(T1, T2), (T3, T4)]]
  summon[ReverseOnto[(T1, T2), (T3, T4)] =:= (T2, T1, T3, T4)]
  summon[ReverseOnto[(T1, T2), (T3, T4)] <:< (Any, Any, Any, Any)]
  summon[ReverseOnto[(T1, T2), (T3, T4)] <:< ReverseOnto[(Any, Any), (Any, Any)]]
  summon[ReverseOnto[(T1, T2), (T3, T4)] =:= Concat[Reverse[(T1, T2)], (T3, T4)]]

def test2[Tup1 <: Tuple, Tup2 <: Tuple] =
  summon[ReverseOnto[EmptyTuple, Tup1] =:= Tup1]
  summon[ReverseOnto[Tup1, EmptyTuple] =:= Reverse[Tup1]]

def test3[T1, T2, T3, T4](tup1: (T1, T2), tup2: (T3, T4)): Unit =
  val tup11: (T1, T2) = tup1
  summon[ReverseOnto[tup11.type, tup2.type] <:< (T2, T1, T3, T4)]
  summon[ReverseOnto[tup11.type, tup2.type] =:= T2 *: T1 *: tup2.type]
