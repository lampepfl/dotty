import scala.language.experimental.unsafeJavaReturn

def f = {
  val j: J2 = new J2()
  j.getJ1().getJ2().getJ1().getJ2().getJ1().getJ2()
}
