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

object Test3:
  trait Vec(tracked val size: Int)
  class Vec8 extends Vec(8)

  abstract class Lst(tracked val size: Int)
  class Lst8 extends Lst(8)

  val v8a: Vec { val size: 8 } = new Vec8
  val v8b: Vec { val size: 8 } = new Vec(8) {}

  val l8a: Lst { val size: 8 } = new Lst8
  val l8b: Lst { val size: 8 } = new Lst(8) {}

  class VecN(tracked val n: Int) extends Vec(n)
  class Vec9 extends VecN(9)
  val v9a = VecN(9)
  val _: Vec { val size: 9 } = v9a
  val v9b = Vec9()
  val _: Vec { val size: 9 } = v9b
