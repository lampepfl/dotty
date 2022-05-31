trait Iterable[T]

@deprecated type Traversable[T] = Iterable[T]

def test: Traversable[Int] = ??? // error
