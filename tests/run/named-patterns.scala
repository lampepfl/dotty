import language.experimental.namedTuples

object Test1:
  class Person(val name: String, val age: Int)

  object Person:
    def unapply(p: Person): (name: String, age: Int) = (p.name, p.age)

  class Person2(val name: String, val age: Int)
  object Person2:
    def unapply(p: Person2): Option[(name: String, age: Int)] = Some((p.name, p.age))

  case class Address(city: String, zip: Int, street: String, number: Int)

  @main def Test =
    val bob = Person("Bob", 22)
    bob match
      case Person(name = n, age = a) => println(s"name $n, age $a")
    bob match
      case Person(name = n) => println(s"name $n")
    bob match
      case Person(age = a) => println(s"age $a")
    bob match
      case Person(age = a, name = n) => println(s"age $a, name $n")
    bob match
      case Person(age, name) => println(s"$age, $name")

    val bob2 = Person2("Bob", 22)
    bob2 match
      case Person2(name = n, age = a) => println(s"name $n, age $a")
    bob2 match
      case Person2(name = n) => println(s"name $n")
    bob2 match
      case Person2(age = a) => println(s"age $a")
    bob2 match
      case Person2(age = a, name = n) => println(s"age $a, name $n")
    bob2 match
      case Person2(age, name) => println(s"$age, $name")

    val addr = Address("Lausanne", 1003, "Rue de la Gare", 44)
    addr match
      case Address(city = c, zip = z, street = s, number = n) =>
        println(s"$z $c, $s $n")
    addr match
      case Address(zip = z, city = c) =>
        println(s"$z $c")
    addr match
      case Address(city = c, street = s) =>
        println(s"$s in $c")
    addr match
      case Address(number = n, street = s, zip = z, city = c) =>
        println(s"$z $c, $s $n")
    addr match
      case Address(c, z, s, number) =>
        println(s"$z $c, $s $number")

    type Person3 = (p: Person2, addr: Address)

    val p3 = (p = bob2, addr = addr)
    p3 match
      case (addr = Address(city = c, zip = z, street = s, number = n), p = Person2(name = nn, age = a)) =>
        println(s"$nn, aged $a, in $z $c, $s $n")
    p3 match
      case (p = Person2(name = nn), addr = Address(zip = z, city = c)) =>
        println(s"$nn in $z $c")
    p3 match
      case (p = Person2(age = a), addr = Address(city = c, street = s)) =>
        println(s"aged $a in $s in $c")
    p3 match
      case  (Person2(age = a, name = nn), Address(number = n, street = s, zip = z, city = c)) =>
        println(s"$nn, aged $a in $z $c, $s $n")
    p3 match
      case (Person2(nn, a), Address(c, z, s, number)) =>
        println(s"$nn, aged $a in $z $c, $s $number")
