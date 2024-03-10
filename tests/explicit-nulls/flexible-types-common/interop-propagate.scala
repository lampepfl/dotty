 class Foo {
  import java.util.ArrayList

  // Test that type mapping works with flexible types.
  val ll: ArrayList[ArrayList[ArrayList[String]]] = new ArrayList[ArrayList[ArrayList[String]]]
  val level1: ArrayList[ArrayList[String]] = ll.get(0) // error
  val level2: ArrayList[String] = ll.get(0).get(0) // error
  val level3: String = ll.get(0).get(0).get(0)  // error

  val lb = new ArrayList[ArrayList[ArrayList[String]]]
  val levelA = lb.get(0)
  val levelB = lb.get(0).get(0)  // error
  val levelC = lb.get(0).get(0).get(0)  // error

  val x = levelA.get(0)  // error
  val y = levelB.get(0)
  val z: String = levelA.get(0).get(0)  // error
}
