import language.`3.5`

case class Show[T](val i: Int)
object Show {
  def apply[T](implicit st: Show[T]): Int = st.i

  given showInt: Show[Int] = new Show[Int](0)
  given fallback[T]: Show[T] = new Show[T](1)
}

class Generic
object Generic {
  given gen: Generic = new Generic
  given showGen[T](using Generic): Show[T] = new Show[T](2)
}

class Generic2
object Generic2 {
  opaque type HiPriority = AnyRef
  given showGen[T]: (Show[T] & HiPriority) = new Show[T](2).asInstanceOf
}

class SubGen extends Generic
object SubGen {
  given SubGen()
}
object Contextual {
  trait Context

  given ctx: Context()

  given showGen[T](using Generic): Show[T] = new Show[T](2)

  given showGen[T](using Generic, Context): Show[T] = new Show[T](3)

  given showGen[T](using SubGen): Show[T] = new Show[T](4)
}

object Test extends App {
  assert(Show[Int] == 0)
  assert(Show[String] == 1)
  assert(Show[Generic] == 1)   // showGen loses against fallback due to longer argument list
  assert(Show[Generic2] == 1)  // ... and the opaque type intersection trick no longer works with new resolution rules.
}
