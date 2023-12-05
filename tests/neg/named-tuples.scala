import annotation.experimental
import language.experimental.namedTuples

@experimental object Test:

  type Person = (name: String, age: Int)
  val person = (name = "Bob", age = 33): (name: String, age: Int)

  val illformed = (_2 = 2) // error
  type Illformed = (_1: Int) // error
  val illformed2 = (name = "", age = 0, name = true)  // error
  type Illformed2 = (name: String, age: Int, name: Boolean) // error

  type NameOnly = (name: String)

  val nameOnly = (name = "Louis")

  val y: (String, Int) = person // error
  val _: (String, Int) = (name = "", age = 0) // error
  val _: NameOnly = person // error
  val _: Person = nameOnly // error

  val _: (age: Int, name: String) = person // error

  val (name = x, agee = y) = person // error

  ("Ives", 2) match
    case (name = n, age = a) => () // error // error

  val pp = person ++ (1, 2)  // error
  val qq = ("a", true) ++ (1, 2)

  person ++ (1, 2) match // error
    case _ =>

  val bad = ("", age = 10) // error

  person match
    case (name = n, age) => () // error
    case (name, age = a) => () // error

  (??? : Tuple) match
    case (age = x) => // error

  val p2 = person ++ person // error
  val p3 = person ++ (first = 11, age = 33) // error
  val p4 = person.zip(person) // ok
  val p5 = person.zip(first = 11, age = 33) // error




