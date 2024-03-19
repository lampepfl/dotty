sealed abstract class A

object A:
  case object B extends A
  case object C extends A
  case object D extends A

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
    val x = fn(a)(5)
