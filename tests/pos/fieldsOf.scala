case class Person(name: String, age: Int)

type PF = NamedTuple.From[Person]

def foo[T]: NamedTuple.From[T] = ???

class Anon(name: String, age: Int)

def test =
  var x: NamedTuple.From[Person] = ???
  val y: (name: String, age: Int) = x
  x = y
  x = foo[Person]
  //x = foo[Anon] // error


