//> using options -source future -language:experimental.modularity

object test {
  class A { type T }
  type X = A { type T = Int }
  class B extends X                                   // was error, now OK
  type Y = A & B
  class C extends Y                                   // error
  type Z = A | B
  class D extends Z                                   // error
  abstract class E extends ({ val x: Int })           // was error, now OK
}
