object Test {
  def main(args: Array[String]): Unit = {
    val seq: Seq[String] = List("one", "two")
    println(java.util.Arrays.asList(seq*))
    println(java.util.Arrays.asList(Seq(1,2,3)*))
  }
}
