import scala.Tuple.*
def test[T1, T2, T3, T4] =
  summon[Reverse[EmptyTuple] =:= EmptyTuple]
  summon[Reverse[T1 *: EmptyTuple] =:= T1 *: EmptyTuple]
  summon[Reverse[(T1, T2)] =:= (T2, T1)]
  summon[Reverse[(T1, T2, T3)] =:= (T3, T2, T1)]
  summon[Reverse[(T1, T2, T3, T4)] =:= (T4, T3, T2, T1)]

  summon[Reverse[(T1, T2, T3, T4)] =:= Reverse[(T1, T2, T3, T4)]]
  summon[Reverse[(T1, T2, T3, T4)] <:< Reverse[(Any, Any, Any, Any)]]

def test2[Tup <: Tuple] =
  summon[Reverse[Tup] =:= Reverse[Tup]]

def test3[T1, T2, T3, T4](tup1: (T1, T2, T3, T4)): Unit =
  val tup11: (T1, T2, T3, T4) = tup1
  summon[Reverse[tup11.type] =:= (T4, T3, T2, T1)]
