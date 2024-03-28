//> using options -experimental

object Test:
  def map2(xs: List[Int])(f: Int => Int): List[Int] = xs.map(f)
  val f1 = map2
  val fc1: List[Int] -> (Int => Int) -> List[Int] = f1

  def map3(f: Int => Int)(xs: List[Int]): List[Int] = xs.map(f)
  private val f2 = map3
  val fc2: (f: Int => Int) -> List[Int] ->{f} List[Int] = f2

  val f3 = (f: Int => Int) =>
    println(f(3))
    (xs: List[Int]) => xs.map(_ + 1)
  val f3c: (Int => Int) -> List[Int] -> List[Int] = f3

  class LL[A]:
    def drop(n: Int): LL[A]^{this} = ???

  def test(ct: CanThrow[Exception]) =
    def xs: LL[Int]^{ct} = ???
    val ys = xs.drop(_)
    val ysc: Int -> LL[Int]^{ct} = ys

import java.io.*
def Test4(g: OutputStream^) =
  val xs: List[Int] = ???
  val later = (f: OutputStream^) => (y: Int) => xs.foreach(x => f.write(x + y))
  val _: (f: OutputStream^) ->{} Int ->{f} Unit = later

  val later2 = () => (y: Int) => xs.foreach(x => g.write(x + y))
  val _: () ->{} Int ->{g} Unit = later2

