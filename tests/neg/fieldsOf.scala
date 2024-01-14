import NamedTuple.FieldsOf

case class Person(name: String, age: Int)
class Anon(name: String, age: Int)
def foo[T](): FieldsOf[T] = ???

def test =
  var x: FieldsOf[Person] = ???
  x = foo[Person]() // ok
  x = foo[Anon]() // error
  x = foo() // error


