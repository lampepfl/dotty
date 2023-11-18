//> using options -source future -language:experimental.modularity
trait Ordering {
  type T
  def compare(t1:T, t2: T): Int
}

class SetFunctor(tracked val ord: Ordering) {
  type Set = List[ord.T]
  def empty: Set = Nil

  implicit class helper(s: Set) {
    def add(x: ord.T): Set = x :: remove(x)
    def remove(x: ord.T): Set = s.filter(e => ord.compare(x, e) != 0)
    def member(x: ord.T): Boolean = s.exists(e => ord.compare(x, e) == 0)
  }
}

object Test {
  val orderInt = new Ordering {
    type T = Int
    def compare(t1: T, t2: T): Int = t1 - t2
  }

  val IntSet = new SetFunctor(orderInt)
  import IntSet.*

  def main(args: Array[String]) = {
    val set = IntSet.empty.add(6).add(8).add(23)
    assert(!set.member(7))
    assert(set.member(8))
  }
}