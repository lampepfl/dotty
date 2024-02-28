import language.experimental.namedTuples
import Tuple.*

def test =
  val x1: Conforms[(1, 2), (1, 2)] = ???
  val _: true = x1

  val x2: Conforms[(1, 2), (1, 3)] = ???
  val _: false = x2

  val x3: Conforms[(1, 2), (1, 2, 4)] = ???
  val _: false = x2

  val x4: Conforms[(1, 2, 4), (1, 2)] = ???
  val _: false = x2

  summon[Disjoint[(1, 2, 3), (4, 5)] =:= true]
  summon[Disjoint[(1, 2, 6), (4, 5)] =:= true]
  summon[Disjoint[(1, 2, 6), EmptyTuple] =:= true]
  summon[Disjoint[EmptyTuple, EmptyTuple] =:= true]

  summon[Contains[(1, 2, 3), Int] =:= true]
  summon[Contains[(1, 2, 3), 2] =:= true]
  summon[Contains[(1, 2, 3), 4] =:= false]

  summon[Conforms[(1, 2, 3), (1, 2, 3)] =:= true]
  summon[Conforms[(1, 2, 3), (1, 2)] =:= false]
  summon[Conforms[(1, 2, 3), (1, 2, 4)] =:= false]
  summon[Conforms[(1, 2, 3), (Int, 2, 3)] =:= true]
//  summon[Conforms[(Int, 2, 3), (1, 2, 3)] =:= true] // error, reduction gets stuck

  summon[Disjoint[(1, 2, 3), (4, 2)] =:= false]
  summon[Disjoint[("a", "b"), ("b", "c")] =:= false]
  summon[Disjoint[(1, 2, 6), Tuple1[2]] =:= false]
  summon[Disjoint[Tuple1[3], (4, 3, 6)] =:= false]

