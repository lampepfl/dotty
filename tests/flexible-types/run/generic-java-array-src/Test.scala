object Test {
  def main(args: Array[String]): Unit = {
    // This test shows that if we have a Java method that takes a generic array,
    // then on the Scala side we'll need to pass a nullable array.
    // i.e. with explicit nulls the previously-implicit cast becomes an explicit
    // type annotation.
    val x = new Array[Integer|Null](1)
    x(0) = 10
    println(JA.get(x))

    // However, if the Java method takes an array that's explicitly of a value type,
    // then we can pass a non-nullable array from the Scala side.
    val intArr = new Array[Int](1)
    intArr(0) = 20
    println(JA.getInt(intArr))

    val boolArr = new Array[Boolean](1)
    boolArr(0) = true
    println(JA.getBool(boolArr))
  }
}
