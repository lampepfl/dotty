@main
def Test: Unit = {
//
//  def unreachableFunction(): Seq[Int] = {
//    for {
//      a <- List(1)
//      b <- List(2)
//    } yield {
//      println(a)
//      println(b)
//      a + b
//    }
//  }
//
//  def unreachableFunctionUnlessTrue(flag: Boolean): Option[Int] = {
//    if (flag) {
//      val foo: Seq[Int] = for {
//        a <- List(1)
//        b <- List(2)
//      } yield {
//        println(a)
//        println(b)
//        a + b
//      }
//      foo.headOption
//    } else {
//      None
//    }
//  }

  for {
    a <- List(1)
    b <- List(2)
  } yield  {
    println(a)
    println(b)
    (a, b)
  }

//  unreachableFunctionUnlessTrue(false)
}
