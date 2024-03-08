//> using options -experimental

/* This is a version of ../pos/phantom.scala that tests phantom clases with separate compilation */
object PhantomEq {
  import EqUtil.*

  "ghi" === "jkl"
  3 === 4
  2.0 === 3.1

  List(1, 2) === Nil
  List(1, 2) === Vector(1, 2)

  1.toByte === (1: Number)
  (1: Number) === 1.toByte
}
