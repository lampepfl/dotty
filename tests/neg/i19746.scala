trait V:
  type X = this.type match
    case W[x] => x

trait W[+Y] extends V

object Test:
  extension (self: Any) def as[T]: T =
    def asX(w: W[Any]): w.X = self // error: Type Mismatch
    asX(new W[T] {})

  def main(args: Array[String]): Unit =
    val b = 0.as[Boolean] // java.lang.ClassCastException if the code is allowed to compile
    println(b)
end Test
