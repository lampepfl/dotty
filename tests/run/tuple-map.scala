
object Test extends App {
  val emptyTuple: Tuple = Tuple()
  val tuple: Tuple = ("1", "2", "3", "4", "5")
  val tupleXXL: Tuple = ("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35")

  type F[X] = String
  val f: (t: Any) => F[t.type] = x => {
    val str = x.asInstanceOf[String]
    str.updated(0, (str(0) + 1).toChar)
  }

  // Test all possible combinations of making
  println(emptyTuple.map(f))
  println(tuple.map(f))
  println(tupleXXL.map(f))

  // NOTE F is needed in ascription of f above for inference of .map
  def alternative: Unit =
    val f: (t: Any) => String = x => {
      val str = x.asInstanceOf[String]
      str.updated(0, (str(0) + 1).toChar)
    }
    println(emptyTuple.map[F](f))
    println(tuple.map[F](f))
    println(tupleXXL.map[F](f))

  // Given the body of f, the following is more likely to occur in practice:
  def withoutWidening: Unit =
    val emptyTuple = Tuple()
    val tuple = ("1", "2", "3", "4", "5")
    val tupleXXL = ("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35")
    val f: (s: String) => F[s.type] = str => str.updated(0, (str(0) + 1).toChar)
    println(emptyTuple.map(f))
    println(tuple.map(f))
    println(tupleXXL.map(f))

}
