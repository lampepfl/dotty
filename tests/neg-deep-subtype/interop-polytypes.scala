//> using options -Yexplicit-nulls -Yno-flexible-types

class Foo {
  import java.util.ArrayList
  // Test that return values in PolyTypes are marked as nullable.
  val lstring = new ArrayList[String]()
  val res: String = java.util.Collections.max(lstring) // error: missing |Null
  val res2: String|Null = java.util.Collections.max(lstring) // ok
}
