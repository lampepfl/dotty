package patterns

class Age(val hidden: Int)

object Age:
  def apply(years: Int): Age = new Age(years)

  // TODO: Describe alternative encoding with tagged tuples
  def unapply(age: Age): Some[Int & { type Names = "years" *: EmptyTuple }] =
    Some(age.hidden.asInstanceOf)

object StringExample:
  def unapply(str: String): (Char, Char) & { type Names = "first" *: "\"" *: EmptyTuple }  =
    Some((str.head, str.last)).asInstanceOf

case class User(name: String, age: Age, city: String)

val user = User(name = "Anna", age = Age(10), city = "Berlin")

val annasCity = user match
  case User(name = "Tom", city = city) => ???
  case User(city = c, name = s"Ann$_") => c
  case User(name = guy @ ("Guy" | "guy")) => ???

// nested patterns
val User(name = name, age = Age(years = years)) = user

// partial function
val maybeTom = Some(user).collect {
  case u @ User(name = StringExample(`"` = 'm')) => u
}

val berlinerNames = for
  case User(city = "Berlin", name = name) <- List(user)
yield name
