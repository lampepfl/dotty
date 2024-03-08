//> using options -language:experimental.modularity -source future -explain

class C:
  type Self

class D:
  type Self

trait Test:

  def foo[A: {C, D}] = A // error

  type A: C

  val x = A  // error

  val y: A.type = ??? // error

