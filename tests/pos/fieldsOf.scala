import NamedTuple.FieldsOf

case class Person(name: String, age: Int)

type PF = FieldsOf[Person]

def foo[T]: FieldsOf[T] = ???

class Anon(name: String, age: Int)

def test =
  var x: FieldsOf[Person] = ???
  val y: (name: String, age: Int) = x
  x = y
  x = foo[Person]
  //x = foo[Anon] // error


