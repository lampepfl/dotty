object Test {
  def main(args: Array[String]): Unit = {
    val v: Vector[String | Null] = Vector("a", "b")
    val v2: Vector[String] = Vector("a", "b")
    println(v)
    println(v2)
  }
}
