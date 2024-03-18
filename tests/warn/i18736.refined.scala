abstract sealed class ||[+A, +B]            extends Product with Serializable
final    case   class LHS[+A, +B](value: A) extends (A || B)
final    case   class RHS[+A, +B](value: B) extends (A || B)

abstract sealed class A { type Self }
abstract        class B extends A
final           class C extends B { type Self = C }

class Test:
  def t2[T <: A { type Self = T; type I = A }](x: String || T): Unit = x match
    case RHS(_) => ()
    case LHS(_) => ()

  def t3[T <: A { type I = A; type Self = T }](x: String || T): Unit = x match
    case RHS(_) => ()
    case LHS(_) => ()
