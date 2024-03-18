abstract sealed class ||[+A, +B]            extends Product with Serializable
final    case   class LHS[+A, +B](value: A) extends (A || B)
final    case   class RHS[+A, +B](value: B) extends (A || B)

abstract sealed class A { type Self }
object B extends A

class Test:
  def t1[T <: A { type Self = T }](x: String || T): Unit = x match
    case RHS(_) => () // warn: unreachable
    case LHS(_) => ()
