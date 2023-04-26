// Check that calling `asInstanceOf[Nothing]` throws a ClassCastException.
// In particular, the compiler needs access to the right method to throw
// the exception, and identifying the method uses some explicit nulls related
// logic (see ClassCastExceptionClass in Definitions.scala).

object Test {
  def main(args: Array[String]): Unit = {
    val x: String = "hello"
    try {
      val y: Nothing = x.asInstanceOf[Nothing]
      assert(false)
    } catch {
      case e: ClassCastException =>
        // ok
    }

    val n: Null = null
    try {
      val y: Nothing = n.asInstanceOf[Nothing]
      assert(false)
    } catch {
      case e: ClassCastException =>
        // ok
    }
  }
}
