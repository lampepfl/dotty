import scala.compiletime.ops.*

object Test {
  infix type +[X <: Int | String, Y <: Int | String] = (X, Y) match {
    case (Int, Int) => int.+[X & Int, Y & Int]
    case (String, String) => string.+[X & String, Y & String]
    case (String, Int) => string.+[X & String, int.ToString[Y & Int]]
    case (Int, String) => string.+[int.ToString[X & Int], Y & String]
  }

  val t0: "a" + 1 = "a1"
  val t1: "a" + "b" = "ab"
  val t2: 1 + "b" = "1b"
  val t3: 1 + 1 = 2
}
