import scala.compiletime.*

inline trait A:
  inline def f[T <: String] =
    inline if constValue[T] == "I consent" then "All is OK!"
    else error("You must consent!")

class B extends A:
  val x = f["I consent"]