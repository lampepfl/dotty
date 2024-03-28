//> using options -Yno-flexible-types

// Test that the return type of Java methods as well as the type of Java fields is marked as nullable.

class Foo {

  def foo = {
    import java.util.ArrayList

    val x = new ArrayList[String]()
    val r: String = x.get(0) // error: got String | Null instead of String

    val x2 = new ArrayList[Int]()
    val r2: Int = x2.get(0) // error: even though Int is non-nullable in Scala, its counterpart
    // (for purposes of generics) in Java (Integer) is. So we're missing `| Null`
  }
}
