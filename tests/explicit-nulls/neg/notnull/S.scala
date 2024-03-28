//> using options -Yno-flexible-types

// Test that NotNull annotations not in the list are not working in Java files.

class S {
  def kk: String = J.k // error: k doesn't have a constant type and the NotNull annotation is not in the list

  def ll: String = J.l // error: the NotNull annotation is not in the list
}