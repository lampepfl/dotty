//> using options -source future -language:experimental.modularity

class A
class B extends A
class C extends B

trait Id { type Value }
type IdOf[T] = Id { type Value = T }
trait X { type Value }

case class Year(value: Int) extends IdOf[Int]:
  val x: Value = 2

type Between[Lo, Hi] = X { type Value >: Lo <: Hi }

class Foo() extends IdOf[B], Between[C, A]:
  val x: Value = B()

trait Bar extends IdOf[Int], (X { type Value = String })

class Baz extends IdOf[Int]:
  type Value = String
  val x: Value = ""

trait Gen:
  type T
  val x: T

type IntInst = Gen:
  type T = Int
  val x: 0

trait IntInstTrait extends IntInst

abstract class IntInstClass extends IntInstTrait, IntInst

object obj1 extends IntInstTrait:
  val x = 0

object obj2 extends IntInstClass:
  val x = 0

def main =
  val x: obj1.T = 2 - obj2.x
  val y: obj2.T = 2 - obj1.x



