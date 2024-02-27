//> using options -language:experimental.modularity -source future
import compiletime.deferred

class Ctx
class Ctx2

trait A:
  given Ctx as ctx = deferred
  given Ctx2 = deferred

class B extends A // error

abstract class C extends A // error

class D extends A:
  given Ctx as ctx = Ctx() // ok, was implemented
  given Ctx2 = Ctx2()      // ok

class Ctx3[T]

trait A2:
  given [T] => Ctx3[T] = deferred

object O:
  given [T] => Ctx3[T] = Ctx3[T]()
  class E extends A2  // error, can't summon polymorphic given

class E extends A2:
  given [T] => Ctx3[T] = Ctx3[T]() // ok

