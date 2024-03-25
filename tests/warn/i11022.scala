//> using options  -deprecation

@deprecated("no CaseClass")
case class CaseClass(rgb: Int):
  def magic(): Unit = ()

object CaseClass:
  def notDeprecated(): Unit = ()

val a: CaseClass = CaseClass(42)        // warn: deprecated type // warn: deprecated apply method
val b: CaseClass = new CaseClass(42)    // warn: deprecated type // warn: deprecated class
val c: Unit = CaseClass(42).magic()     // warn: deprecated apply method
val d: Unit = CaseClass.notDeprecated() // compiles
val CaseClass(rgb) = b                  // warn

case class K(@deprecated("don't use k, ok?","0.1") k: Int)

object K:
  def usage(k: K) = k.k   // warn

val s: String = CaseClass.toString
