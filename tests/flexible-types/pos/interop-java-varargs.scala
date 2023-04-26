import java.nio.file.*
import java.nio.file.Paths

class S {

  // Paths.get is a Java method with two arguments, where the second one
  // is a varargs: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Paths.html
  // static Path get(String first, String... more)
  // The Scala compiler converts this signature into
  // def get(first: String | Null, more: (String | Null)*)

  // Test that we can avoid providing the varargs argument altogether.
  Paths.get("out")

  // Test with one argument in the varargs.
  Paths.get("home", "src")
  Paths.get("home", null)

  // Test multiple arguments in the varargs.
  Paths.get("home", "src", "compiler", "src")
  Paths.get("home", null, null, null)
}
