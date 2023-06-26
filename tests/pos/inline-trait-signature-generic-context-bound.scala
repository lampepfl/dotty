inline trait A[T: List]

given List[Int] = Nil
class B extends A