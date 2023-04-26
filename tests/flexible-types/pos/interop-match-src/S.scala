class S[T] {
  def bar(x: J[T]): J[T] = x.j match {
    case y: J[_] => y.j
  }
}

