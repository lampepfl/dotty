import language.experimental.namedTuples
import NamedTuple.toTuple

type Person = (
  x0: Int, x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int,
  name: String, y1: Int, age: Int, y2: Int,
  z0: Int, z1: Int, z2: Int, z3: Int, z4: Int, z5: Int, z6: Int, z7: Int, z8: Int, z9: Int)

val bob = (
  x0 = 0, x1 = 0, x2 = 0, x3 = 0, x4 = 0, x5 = 0, x6 = 0, x7 = 0, x8 = 0, x9 = 0,
  name = "Bob", y1 = 0, age = 33, y2 = 0,
  z0 = 0, z1 = 0, z2 = 0, z3 = 0, z4 = 0, z5 = 0, z6 = 0, z7 = 0, z8 = 0, z9 = 0)

val person2: Person = bob


type AddressInfo = (city: String, zip: Int)
val addr = (city = "Lausanne", zip = 1003)

type CombinedInfo = NamedTuple.Concat[Person, AddressInfo]
val bobWithAddr = bob ++ addr
val _: CombinedInfo = bobWithAddr
val _: CombinedInfo = bob ++ addr

@main def Test =
  assert(bob.name == "Bob")
  assert(bob.age == 33)
  bob match
    case p @ (name = "Bob", age = a) =>
      val x = p
      println(x)
      assert(p.age == 33)
      assert(a == 33)
    case _ =>
      assert(false)

  bob match
    case p @ (name = "Peter", age = _) => assert(false)
    case p @ (name = "Bob", age = 0) => assert(false)
    case _ =>
  bob match
    case b @ (x0 = 0, x1 = 0, x2 = 0, x3 = 0, x4 = 0, x5 = 0, x6 = 0, x7 = 0, x8 = 0, x9 = 0,
          name = "Bob", y1 = 0, age = 33, y2 = 0,
          z0 = 0, z1 = 0, z2 = 0, z3 = 0, z4 = 0, z5 = 0, z6 = 0, z7 = 0, z8 = 0, z9 = 0)
     => // !!! spurious unreachable case warning
      println(bob)
      println(b)
    case _ => assert(false)

  val x = bob.age
  assert(x == 33)

  val y: (
    Int, Int, Int, Int, Int, Int, Int, Int, Int, Int,
    String, Int, Int, Int,
    Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
     = bob.toTuple

  def ageOf(person: Person) = person.age

  assert(ageOf(bob) == 33)

  val persons = List(
    bob,
    (x0 = 0, x1 = 0, x2 = 0, x3 = 0, x4 = 0, x5 = 0, x6 = 0, x7 = 0, x8 = 0, x9 = 0,
     name = "Bill", y1 = 0, age = 40, y2 = 0,
     z0 = 0, z1 = 0, z2 = 0, z3 = 0, z4 = 0, z5 = 0, z6 = 0, z7 = 0, z8 = 0, z9 = 0),
    (x0 = 0, x1 = 0, x2 = 0, x3 = 0, x4 = 0, x5 = 0, x6 = 0, x7 = 0, x8 = 0, x9 = 0,
     name = "Lucy", y1 = 0, age = 45, y2 = 0,
     z0 = 0, z1 = 0, z2 = 0, z3 = 0, z4 = 0, z5 = 0, z6 = 0, z7 = 0, z8 = 0, z9 = 0),
  )
  for
    p <- persons
    q <- persons
    if p.age < q.age
  do
    println(s"${p.name} is younger than ${q.name}")

  val name1 = bob(10)
  val age1 = bob(12)

  val minors = persons.filter:
    case (age = a) => a < 18
    case _ => false

  assert(minors.isEmpty)

  bob match
    case bob1 @ (age = 33, name = "Bob") =>
      val x: Person = bob1 // bob1 still has type Person with the unswapped elements
    case _ => assert(false)
