package tests.hugetype

import compiletime._
import compiletime.ops.int._
import scala.annotation.experimental
import scala.annotation.nowarn

/**
 * a particular group of people or things that share similar characteristics and form a smaller division of a larger set:
 * - There were so many different types of bread that I didn't know which to buy.
 * - What type of clothes does she wear?
 * - It was dark so I didn't notice what type of car it was.
 * - He's the type of man you could take home to your mother.
 * - He's very attractive, if you like the blond, athletic type.
 * - They sell dried flowers and baskets and that type of thing.
 * - We have a range of moisturizers for all different skin types.
 * - She was young and she was wearing student-type clothes, so I assumed she was studying here.
 * - He took me to a bar full of actor types trying to get noticed.
 *
 */
type Take[T <: Tuple, N <: Int] <: Tuple = N match {
  case 0 => EmptyTuple
  case S[n1] => T match {
    case EmptyTuple => EmptyTuple
    case x *: xs => x *: Take[xs, n1]
  }
}

trait E:
  @experimental
  @deprecated
  protected implicit def same[A](a: A): A

@experimental trait XD extends E:
  /**
   * Some important information :o
   *
   * @param a any value of type forall a. a
   * @return Nothing, because I haven't implemented it yet ^^
   */
  @experimental
  @deprecated
  protected override final implicit transparent inline abstract infix def same[A](a: A): A = a: @nowarn

trait Parent[X, A[_], B[_, _]]

trait HugeTrait[X] extends Parent[X, HugeTrait, Map]

trait EvenBiggerTrait[X, Y] extends Parent[X, Seq, EvenBiggerTrait]
