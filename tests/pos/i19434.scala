object Named:
  opaque type Named[name <: String & Singleton, A] >: A = A

type DropNames[T <: Tuple] = T match
  case Named.Named[_, x] *: xs => x *: DropNames[xs]
  case _                       => T

class Test:
  def f[T <: Tuple]: DropNames[T] = ???
