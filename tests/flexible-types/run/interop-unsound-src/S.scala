// An example that shows that the nullability transform is unsound.

class ScalaBox[T](init: T) {
  var contents: T = init

  def setContents(c: T): Unit = {
    contents = c
  }
}

object Test {

  def main(args: Array[String]): Unit = {
    val jb: JavaBox[String] = new JavaBox("hello")
    val sb: ScalaBox[String] = ScalaBox("world")

    Forwarder.putInJavaBox(jb, null) // not unsound, becase JavaBox is java-defined
                                     // so the contents fields will have a nullable
                                     // type
    
    Forwarder.putInScalaBox(sb, null) // this is unsound, because ScalaBox
                                      // should contain only Strings, but we added
                                      // a null
    
    try {
      sb.contents.length
      assert(false)
    } catch {
      case ex: NullPointerException =>
        // expected
    }
  }
}
