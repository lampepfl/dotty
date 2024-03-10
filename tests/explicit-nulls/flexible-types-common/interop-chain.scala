// With flexible types, we can select a member of its underlying type.

class Foo {
  import java.util.ArrayList
  import java.util.Iterator

  val x3 = new ArrayList[ArrayList[ArrayList[String]]]()
  val x4: Int = x3.get(0).get(0).get(0).length() // error
}
