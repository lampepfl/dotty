//> using options -Xfatal-warnings

class Foo:

  val s: String = ???

  s match
    case s: String => 100
    case _ => 200 // error: unreachable case except for null

  s match
    case s: String => 100
    case null => 200

  s match
    case null => 100
    case _ => 200

  val s2: String | Null = ???

  s2 match
    case s2: String => 100
    case _ => 200 // error: unreachable case except for null

  s2 match
    case s2: String => 100
    case null => 200

  s2 match
    case null => 200
    case s2: String => 100

  sealed trait Animal
  case class Dog(name: String) extends Animal
  case object Cat extends Animal

  val a: Animal = ???
  a match
    case Dog(name) => 100
    case Cat => 200
    case _ => 300 // error: unreachable case except for null

  val a2: Animal | Null = ???
  a2 match
    case Dog(_) => 100
    case Cat => 200
    case _ => 300 // error: unreachable case except for null

  a2 match
    case Dog(_) => 100
    case Cat => 200
    case null => 300 // ok
