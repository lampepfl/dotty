//> using options -Xfatal-warnings -deprecation -feature

def foo: Unit =
  object O:
    sealed abstract class A
  class B extends O.A
  class C extends O.A

  val x: O.A = ???
  x match
    case x: B => ???
    case x: C => ???
