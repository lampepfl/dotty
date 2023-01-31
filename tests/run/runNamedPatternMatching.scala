object Test:

  class User(val name: String, val age: Int, val city: String)

  object UserEx:
    type Reverse[A, B] = (B, A)
    class UserExtractor(user: User) extends Product:
      def _1: String =
        println("Got name")
        return user.name
      def _2: Int =
        println("Got age")
        user.age
      def _3: String =
        println("Got city")
        user.city

      type Names = "name" *: Reverse["city", "age"]

      // Members declared in scala.Equals
      def canEqual(that: Any): Boolean = ???

      // Members declared in scala.Product
      def productArity: Int = ???
      def productElement(n: Int): Any = ???

    def unapply(user: User) = UserExtractor(user)

  object Helper:
    val empty: EmptyTuple & { type Names = EmptyTuple } = EmptyTuple.asInstanceOf

    extension [N <: Singleton & String, V] (head: (N, V))
      def +::[Vs <: Tuple, Ns <: Tuple](tail: Vs & { type Names = Ns }) : V *: Vs & { type Names = N *: Ns } =
        (head._2 *: tail).asInstanceOf

  object UserEx2:
    import Helper._
    // TODO: The inferred type doesn't work and leads to a runtime exception
    def unapply(user: User): (String, Int, String) & { type Names = ("name", "age", "city") } =
      ("name", user.name) +:: ("age", user.age) +:: ("city", user.city) +:: empty

  def main(args: Array[String]): Unit =
    // TODO: following line leads to "Recursive value c needs type"
    // val UserEx(city = c, name = _) = User("Guy", 25, "Paris")
    // println("city = " + c)
    val UserEx2(city = _, name = n) = User("Guy", 25, "Paris")

    println(n)
