//> using options -language:experimental.modularity -source future
trait Ord:
  type Self
  def less(x: Self, y: Self): Boolean

trait A:
  type Elem
  given Elem is Ord = deferredSummon
  def foo = summon[Elem is Ord]

trait B:
  type Elem: Ord
  def foo = summon[Elem is Ord]

object Inst:
  given Int is Ord:
    def less(x: Int, y: Int) = x < y

object Test1:
  import Inst.given
  class C extends A:
    type Elem = Int
  object E extends A:
    type Elem = Int
  given A:
    type Elem = Int

class D1[T: Ord] extends B:
  type Elem = T

object Test2:
  import Inst.given
  class C extends B:
    type Elem = Int
  object E extends B:
    type Elem = Int
  given B:
    type Elem = Int

class D2[T: Ord] extends B:
  type Elem = T





