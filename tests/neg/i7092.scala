import scala.annotation.implicitNotFound
import scala.util.NotGiven

@implicitNotFound("Not found for ${A}")
type F[A]

@implicitNotFound("Not found for ${A}")
trait G[A]

@implicitNotFound("Not found for ${A}, ${B}")
type H = [A] =>> [B] =>> (A, B)

@implicitNotFound("Not found for ${A}")
type AAA = [A] =>> [A] =>> A

object op:
  @implicitNotFound("Could not find ${A}")
  opaque type F[A] = A

@implicitNotFound("Cannot proof type inequality because types are equal: ${A} =:= ${B}")
type =!:=[A, B] = NotGiven[A =:= B]

object Test:
  summon[F[String]] // error
  summon[G[String]] // error
  summon[H[Int]] // error
  summon[H[Int][Float]] // error
  summon[AAA[Int]] // error
  summon[AAA[Int][Float]] // error
  summon[op.F[Int]] // error
  summon[String =!:= String] // error
