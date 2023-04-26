/*
class Foo {
  def err(msg: String): Nothing = {
    throw new RuntimeException("Hello")
  }
  def retTypeNothing(): String = {
    val y: String|Null = ???
    if (y == null) err("y is null!")
    y
  }
}
*/



@main def main() = {
  val i : Integer = new Integer(3) // Constructor with non-ref arg
  val s1 : String | Null = new String("abc") // Constructor with ref arg
  val s2 : String = new String("abc") // Constructor with ref arg, not null
  val s3 = s1.nn.substring(0,1).substring(0,1)
  val s4 = s2.substring(0,1).substring(0,1)
  val s5 = s4.startsWith(s4)
  // s1.substring(0,1) // error
  val j : J = new J("")
  println(s4)
  //val f : Foo = new Foo("x")
  //f.err("Hello")
  //val l : List[String] = Java.returnsNull();
  //val j : J = new J
}
