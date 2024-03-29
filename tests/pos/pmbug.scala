//> using options -source 3.2

object Test {

  def flatten[a](l: List[List[a]]): List[a] = l match {
    case Nil => Nil
    case head :: tail => head ::: flatten(tail)
  }

}
