trait T

object A {

  class B extends T
  class C extends T
  class D[T]

  given b: B()
  given c: C()
  given t: T()
  given d: D[Int]()
}

object Test extends App {
  import A.*
  import A.{t, given B, given D[_]}

  val x1: B = b
  val x2: T = t
  val x3: D[Int] = d

  assert(summon[T].isInstanceOf[T])
  assert(summon[D[Int]].isInstanceOf[D[_]])
}

class Ordering[T]
class ExecutionContext
class Monoid[T]

object Instances {
  given intOrd: Ordering[Int]()

  given listOrd[T](using Ordering[T]): Ordering[List[T]]()
  given ec: ExecutionContext()
  given im: Monoid[Int]()
}

object Test2 {
  import Instances.{given Ordering[_], given ExecutionContext}
  val x = intOrd
  val y = listOrd[Int]
  val z = ec
  summon[Ordering[Int]]
  summon[Ordering[List[Int]]]
  summon[ExecutionContext]
}

object Test3 {
  import Instances.{im, given Ordering[_]}
  val x = intOrd
  val y = listOrd[Int]
  val z = im
  summon[Ordering[Int]]
  summon[Ordering[List[Int]]]
  summon[Monoid[Int]]
}

