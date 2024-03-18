// like pos/i15926.scala
// but with the nested match type extracted
// which is a workaround that fixed the problem
sealed trait Nat
final case class Zero() extends Nat
final case class Succ[+N <: Nat]() extends Nat

final case class Neg[+N <: Succ[Nat]]()

type Sum[X <: Nat, Y] = Y match
  case Zero    => X
  case Succ[y] => Sum[Succ[X], y]

type IntSum[A, B] = B match
  case Neg[b] => IntSumNeg[A, b]

type IntSumNeg[A, B] = A match
  case Neg[a] => Negate[Sum[a, B]]

type Negate[A] = A match
  case Zero    => Zero
  case Succ[x] => Neg[A & Succ[x]]

type One = Succ[Zero]
type Two = Succ[One]

class Test:
  def test() = summon[IntSum[Neg[One], Neg[One]] =:= Neg[Two]]
