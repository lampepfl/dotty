object Test:

  object Named:
    opaque type Named[name <: String & Singleton, A] >: A = A
    def apply[S <: String & Singleton, A](name: S, x: A): Named[name.type, A] = x
    extension [name <: String & Singleton, A](named: Named[name, A]) def value: A = named
  import Named.*

  type DropNames[T <: Tuple] = T match
    case Named[_, x] *: xs => x *: DropNames[xs]
    case _ => T

  extension [T <: Tuple](x: T) def toTuple: DropNames[T] =
    x.asInstanceOf // named and unnamed tuples have the same runtime representation

  val name = "hi"
  val named = Named(name, 33)  // ok, but should be rejectd

  inline val name2 = "hi"
  val named2 = Named(name2, 33)  // ok, but should be rejectd
  val _: Named["hi", Int] = named2

  var x = (Named("name", "Bob"), Named("age", 33))

  val y: (String, Int) = x.toTuple

  x = y

  val z = y.toTuple

  type PersonInfo = (Named["name", String], Named["age", Int])
  type AddressInfo = (Named["city", String], Named["zip", Int])

  val ok1: (Named["name", String], Named["age", Int]) = x
  val ok2: PersonInfo = y
  //val err1: (Named["bad", String], Named["age", Int]) = x // error
  val err2: (Named["bad", String], Named["age", Int]) = x.toTuple // ok
  val ok3: (Named["bad", String], Named["age", Int]) = y // ok

  val addr = (Named("city", "Lausanne"), Named("zip", 1003))
  val _: AddressInfo = addr

  type CombinedInfo = Tuple.Concat[PersonInfo, AddressInfo]

  val combined: CombinedInfo = x ++ addr

//  val person = (name = "Bob", age = 33): (name: String, age: Int)
//  person.age
