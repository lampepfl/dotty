case class Person(name: String, age: Int)
class Anon(name: String, age: Int)
def foo[T](): NamedTuple.From[T] = ???

def test =
  var x: NamedTuple.From[Person] = ???
  x = foo[Person]() // ok
  x = foo[Anon]() // error
  x = foo() // error


