//> using options -source future -language:experimental.modularity
trait Animal
class Dog extends Animal
class Cat extends Animal

object Test1:

  abstract class Bar { val x: Animal }
  val bar: Bar { val x: Cat } = new Bar { val x = new Cat }  // error, but should work

  trait Foo { val x: Animal }
  val foo: Foo { val x: Cat } = new Foo { val x = new Cat }  // error, but should work
