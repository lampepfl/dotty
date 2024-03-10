// Test that nullification can handle Java varargs.
// For varargs, the element type is nullified, but the top level argument isn't.

class S {
  // Pass an empty array.
  Names.setNames()

  // Pass a singleton array with null as an element.
  Names.setNames(null)

  // Pass a singleton array.
  Names.setNames("name1")

  // Multiple arguments.
  Names.setNames("name1", "name2", "name3", "name4")

  // Multiple arguments, some null.
  Names.setNames(null, null, "hello", "world", null)

  val arg1: Array[String] = ???
  val arg2: Array[String | Null] = ???
  val arg3: Array[String] | Null = ???
  val arg4: Array[String | Null] | Null = ???

  Names.setNames(arg1*)
  Names.setNames(arg2*)
  Names.setNames(arg3*) // error
  Names.setNames(arg4*) // error
}
