//> using options -Xfatal-warnings -deprecation -feature

object Test {
  (List(1: @unchecked, 2, 3): @unchecked) match {
    case a :: as =>
  }
}