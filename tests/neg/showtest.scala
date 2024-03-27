trait Show[T]:
  extension (x: T) def show: String

given showAnx: Show[Any] = _.toString

def print[T: Show](x: T) = println(x.show)

@main def Test =
  println("hello".show) // ok
  print("hello".show) // error
