trait Show[T]:
  extension (x: T) def show: String

given showAny[T]: Show[T] = _.toString
given showInt: Show[Int] = x => s"INT $x"

def print[T: Show](x: T) = println(x.show)

@main def Test =
  println("hello".show)
  println(1.show)
  print("hello".show)
  print(1)
