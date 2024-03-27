//> using options -language:experimental.modularity -source future

trait Nat:
  type N <: Nat

class _0 extends Nat:
  type N = _0

class NatOps[N <: Nat](tracked val n: N):
  def toInt(using toIntN: ToInt[n.N]): Int = toIntN()

// works
def toInt[N <: Nat](n: N)(using toIntN: ToInt[n.N]) = toIntN()

sealed abstract class ToInt[N <: Nat]:
  def apply(): Int

object ToInt:
  given ToInt[_0] {
    def apply() = 0
  }

@main def Test() =
  assert(toInt(new _0) == 0)
  assert(NatOps[_0](new _0).toInt == 0)
  assert:
    NatOps(new _0).toInt == 0 // did not work
