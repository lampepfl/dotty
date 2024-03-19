enum A:
  case B
  case C
  case D

object A:
  type B_type = B.type
  type C_type = C.type
  type D_type = D.type

type Matcher[T] = T match
  case A.C.type | A.D.type => Int
  case A.B.type            => Float

class Test:
  def fn[U <: A](u: U)(value: Matcher[U]): Matcher[U] = value

  def t1: Unit =
    val a: A.C_type | A.D_type = A.C
    val x = fn(a)(5)

  def t2: Unit =
    val a: A.C.type | A.D.type = A.C
    val x = fn(a)(5) // was:
//                ^
//                Found:    (5 : Int)
//                Required: Matcher[A]
//
//                Note: a match type could not be fully reduced:
//
//                  trying to reduce  Matcher[A]
//                  failed since selector A
//                  does not match  case (A.C : A) | (A.D : A) => Int
//                  and cannot be shown to be disjoint from it either.
//                  Therefore, reduction cannot advance to the remaining case
//
//                    case (A.B : A) => Float
