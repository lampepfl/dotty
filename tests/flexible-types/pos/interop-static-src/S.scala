class S {
  // Java static methods are also nullified
  val x: Int = J.foo(null)
  val y: String | Null = J.bar(0)
  val y2: String = J.bar(0)
}
