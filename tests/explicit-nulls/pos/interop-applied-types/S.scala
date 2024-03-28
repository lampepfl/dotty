def test1[T](x: J[T]): J[T] =
  x match {
    case y: J[_] => y
  }

def test2[T](x: J[T]): J[T] =
  x match {
    case y: J[_] => y.j
  }

def test3[T](x: J[T]): J[T] =
  x.j match {
    case y: J[_] => y.j
  }