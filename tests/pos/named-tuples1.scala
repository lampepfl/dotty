import annotation.experimental
import language.experimental.namedTuples

@main def Test =
  val bob = (name = "Bob", age = 33): (name: String, age: Int)
  val persons = List(
    bob,
    (name = "Bill", age = 40),
    (name = "Lucy", age = 45)
  )
  val ages = persons.map(_.age)
    // pickling failure: matchtype is reduced after pickling, unreduced before.
  assert(ages.sum == 118)
