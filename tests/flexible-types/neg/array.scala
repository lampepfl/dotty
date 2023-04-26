// Test array with nulls.

class ArrayWithNulls {
  def test1 = {
    // A non-nullable array of non-nullable strings
    val a1: Array[String] = Array("hello")
    val s1: String = a1(0)
    val s2: String | Null = a1(0)
    val a2: Array[String] = Array()

    // Array type is non-nullable
    val b1: Array[String] = null // error
    val b2: Array[Int] = null // error
  }

  def test2 = {
    // A nullable array of non-nullable strings
    val a1: Array[String] | Null = null
    val a2: Array[String] | Null = Array()
    val a3: Array[String] | Null = Array("")
    val a4: Array[String] | Null = Array("", null) // error

    // A non-nullable array of nullable strings
    val b1: Array[String | Null] = Array()
    val b2: Array[String | Null] = Array(null)
    val b3: Array[String | Null] = Array("")
    val b4: Array[String | Null] = Array("", null)
    val b5: Array[String | Null] = null // error

    val s1: String = b1(0) // error
    val s2: String | Null = b1(0)

    // A nullable array of nullable strings
    val c1: Array[String | Null] | Null = Array()
  }

  def test3 = {
    val a1: Array[String] = Array()

    val a2: Array[String] | Null = a1

    val a3: Array[String | Null] = a1 // error
  }
}
