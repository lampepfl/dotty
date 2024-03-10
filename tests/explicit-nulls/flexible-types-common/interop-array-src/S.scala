class S {

  val j = new J()

  def f =  {
    val x1: Array[String] = ???
    j.foo1(x1) // error: expected Array[String | Null] but got Array[String]

    val x2: Array[String | Null] = ???
    j.foo1(x2) // ok
    j.foo1(null) // ok

    val y1: Array[String] = j.foo2() // error
    val y2: Array[String | Null] = j.foo2() // error: expected Array[String | Null] but got Array[String]
    val y3: Array[String | Null] | Null = j.foo2()
  }

  def g =  {
    val x1: Array[Int] = ???
    j.bar1(x1) // ok

    val y1: Array[Int] = j.bar2() // error
    val y2: Array[Int] | Null = j.bar2()
  }
}
