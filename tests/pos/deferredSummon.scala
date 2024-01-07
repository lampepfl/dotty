//> using options -language:experimental.modularity -source future
trait Ord:
  type Self
  def less(x: Self, y: Self): Boolean

trait A:
  type Elem
  given Elem is Ord = deferredSummon
  def foo = summon[Elem is Ord]

object Inst:
  given Int is Ord:
    def less(x: Int, y: Int) = x < y

object Test:
  import Inst.given
  class C extends A:
    type Elem = Int
  object E extends A:
    type Elem = Int
  given A:
    type Elem = Int

class D[T: Ord] extends A:
  type Elem = T




