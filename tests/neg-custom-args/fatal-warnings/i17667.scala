// scalac: -Wunused:imports

object MyImplicits:
  extension (a: Int) def print: Unit = println(s"Hello, I am $a")

import MyImplicits.print // ok
object Foo:
  def printInt(a: Int): Unit = a.print
  import MyImplicits._ // error
