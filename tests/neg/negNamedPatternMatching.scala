package patterns

type Age = Age.Age

object Age:
  opaque type Age = Int

  def apply(years: Int): Age = years

  def unapply(age: Age): Some[Int] =
    Some(age)

type City = City.City

object City:
  opaque type City = String

  class ExCity(city: City) extends Product:
    def _1: String = city

    private[City] type Names = "name" *: EmptyTuple

    // Members declared in scala.Equals
    def canEqual(that: Any): Boolean = ???

    // Members declared in scala.Product
    def productArity: Int = ???
    def productElement(n: Int): Any = ???

  City("test") match
    case City("test") => null
    case _ => null

  def apply(name: String): City = name

  def unapply(age: City): ExCity = ExCity(age)

case class User(name: String, age: Age, city: City)

val user = User(name = "Anna", age = Age(10), city = City("Berlin"))

val annasCity = user match
  case User(names = "Tom", city = city) => null // error
  case User(city = _, 10) => null // error
  case User(age = Age(years = 10)) => null // error
  case User(
    name = "Tom",
    name = "Tom 2",  // error
    name = "Tom 3" // error
    ) => null
  case User(_, name = "Anna") => null // error
  case User(city = City(name = "Berlin")) => null // error

// TODO: Don't show an error about recursive value
val User(names = notRecursive) = user // error // error


object MySeq:
  def unapplySeq[A](x: Seq[A]): Some[Seq[A]] & { type Names = ("first", "second") } =
    Some(x).asInstanceOf[Some[Seq[A]] & { type Names = ("first", "second") }]

val x = Seq(1, 2) match {
  case MySeq(head = 1, tail = 2) => ??? // error
  case _ => println("Also nope")
}
