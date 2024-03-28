// Check Java calls have been cast to non-nullable.

val j: J = new J

val s1: String = j.f1() // error

val s1n: String | Null = j.f1()

val i1: Int = j.f2()

val s2: String = j.g1[String]()  // error

val s2n: String | Null = j.g1[String]()

// val s3: String = j.g1[String | Null]()  error

val s3n: String | Null = j.g1[String | Null]()

val i2: Int = j.g1[Int]()  // error

val a1: Any = j.g1[Any]()

val ar1: AnyRef = j.g1[AnyRef]()  // error

val n1: Null = j.g1[Null]()

// val ar2: AnyRef = j.g1[Null]() error

def clo1[T]: T = j.g1[T]()  // error

def clo2[T <: AnyRef]: T = j.g1[T]() // error

def clo3[T >: Null <: AnyRef | Null]: T = j.g1[T]()

def testJ2[T]: T =
  val j2: J2[T] = new J2
  j2.x // error
