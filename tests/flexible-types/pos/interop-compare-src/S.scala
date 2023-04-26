@main def main() = {
  val j: J = new J
  if (j.foo(null) == null) {
    println("null")
  }
  val x = j.foo("a")
  if (x == null) {
    println("null")
  }
}
