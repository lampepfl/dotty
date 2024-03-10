import java.util.ArrayList

def f[T]: ArrayList[T] = {
  val cz = Class.forName("java.util.ArrayList")
  val o = cz.newInstance() // error: T of Class[?] | Null
  o.asInstanceOf[ArrayList[T]]
}