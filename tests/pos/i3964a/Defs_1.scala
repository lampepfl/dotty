//> using options -source future -language:experimental.modularity
trait Animal
class Dog extends Animal
class Cat extends Animal

object Test2:
  class Bar(tracked val x: Animal)
  val b = new Bar(new Cat)
  val bar: Bar { val x: Cat } = new Bar(new Cat)  // ok

  trait Foo(tracked val x: Animal)
  val foo: Foo { val x: Cat } = new Foo(new Cat) {} // ok

package coll:
  trait Vec(tracked val size: Int)
  class Vec8 extends Vec(8)

  abstract class Lst(tracked val size: Int)