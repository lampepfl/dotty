import java.nio.file.Paths

def test1 = {
  Paths.get("")
  Paths.get("", null)
  Paths.get("", "")
  Paths.get("", "", null)

  val x1: String = ???
  val x2: String | Null = ???

  Paths.get("", x1)
  Paths.get("", x2)
}

def test2 = {
  val xs1: Seq[String] = ???
  val xs2: Seq[String | Null] = ???
  val xs3: Seq[String | Null] | Null = ???
  val xs4: Seq[String] | Null = ???

  val ys1: Array[String] = ???
  val ys2: Array[String | Null] = ???
  val ys3: Array[String | Null] | Null = ???
  val ys4: Array[String] | Null = ???

  Paths.get("", xs1*)
  Paths.get("", xs2*)
  Paths.get("", xs3*) // error
  Paths.get("", xs4*) // error

  Paths.get("", ys1*)
  Paths.get("", ys2*)
  Paths.get("", ys3*) // error
  Paths.get("", ys4*) // error

  Paths.get("", null*) // error
}