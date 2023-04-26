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
}
