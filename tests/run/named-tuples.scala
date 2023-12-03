import language.experimental.namedTuples
import NamedTuple.toTuple

type Person = (name: String, age: Int)
val bob = (name = "Bob", age = 33): (name: String, age: Int)
val person2: (name: String, age: Int) = bob

type Uni = (uni: Double)
val uni = (uni = 1.0)
val _: Uni = uni

type AddressInfo = (city: String, zip: Int)
val addr = (city = "Lausanne", zip = 1003)
val _: AddressInfo = addr

type CombinedInfo = NamedTuple.Concat[Person, AddressInfo]
val bobWithAddr = bob ++ addr
val _: CombinedInfo = bobWithAddr
val _: CombinedInfo = bob ++ addr

@main def Test =
  println(bob)
  println(bob.age)
  println(person2.name)
  println(bobWithAddr)
  bob match
    case p @ (name = "Bob", age = _) => println(p.age)
  bob match
    case p @ (name = "Bob", age = age) => assert(age == 33)
  bob match
    case p @ (name = "Peter", age = _) => println(p.age)
    case p @ (name = "Bob", age = 0) => println(p.age)
    case _ => println("no match")

  val x = bob.age
  assert(x == 33)

  val y: (String, Int) = bob.toTuple

  def ageOf(person: Person) = person.age

  assert(ageOf(bob) == 33)
  assert(ageOf((name = "anon", age = 22)) == 22)
  assert(ageOf(("anon", 11)) == 11)

  val persons = List(
    bob,
    (name = "Bill", age = 40),
    (name = "Lucy", age = 45)
  )
  for
    p <- persons
    q <- persons
    if p.age < q.age
  do
    println(s"${p.name} is younger than ${q.name}")

  //persons.select(_.age, _.name)
  //persons.join(addresses).withCommon(_.name)

  def minMax(elems: Int*): (min: Int, max: Int) =
    var min = elems(0)
    var max = elems(0)
    for elem <- elems do
      if elem < min then min = elem
      if elem > max then max = elem
    (min = min, max = max)

  val mm = minMax(1, 3, 400, -3, 10)
  assert(mm.min == -3)
  assert(mm.max == 400)

  val name1 = bob(0)
  val age1 = bob(1)
  val _: String = name1
  val _: Int = age1

  val bobS = bob.reverse
  val _: (age: Int, name: String) = bobS
  val _: NamedTuple.Reverse[Person] = bobS

  val silly = bob match
    case (name, age) => name.length + age

  assert(silly == 36)

  val minors = persons.filter:
    case (age = a) => a < 18
    case _ => false

  assert(minors.isEmpty)

  bob match
    case bob1 @ (age = 33, name = "Bob") =>
      val x: Person = bob1 // bob1 still has type Person with the unswapped elements
    case _ => assert(false)



