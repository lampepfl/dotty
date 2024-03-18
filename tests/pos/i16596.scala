import scala.compiletime.ops.int, int.-

type Count[N <: Int, T] <: Tuple = (N, T) match
  case (0, T) => EmptyTuple
  case (N, T) => T *: Count[N - 1, T]

val a: Count[3, Int] = (1, 2, 3)
val b: Count[4, Int] = (1, 2, 3, 4)
val c: Count[5, Int] = (1, 2, 3, 4, 5)
val d: Count[6, Int] = (1, 2, 3, 4, 5, 6)
val z: Count[23, Int] = (
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  21, 22, 23)
