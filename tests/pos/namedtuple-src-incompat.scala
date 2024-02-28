import language.experimental.namedTuples
var age = 22
val x = (age = 1)
val _: (age: Int) = x
val x2 = {age = 1}
val _: Unit = x2

class C:
  infix def id[T](age: T): T = age

def test =
  val c: C = ???
  val y = c id (age = 1)
  val _: (age: Int) = y
  val y2 = c.id(age = 1)
  val _: Int = y2

