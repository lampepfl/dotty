inline trait A[T]:
  trait InnerAType[T >: Int <: AnyVal]
  trait InnerATypes[T <: AnyVal, U <: T]
  trait InnerATerm(i: Int)  // error
  trait InnerATerms(i: Int, j: Double)  // error
  trait InnerATermsCurried(i: Int, j: Double)(k: String)  // error
  trait InnerAAllCurried[T, U](i: T, j: U)(k: (T, U))  // error