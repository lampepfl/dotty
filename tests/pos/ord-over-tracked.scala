import language.experimental.modularity

trait Ord[T]:
  def lt(x: T, y: T): Boolean

given Ord[Int] = ???

case class D(tracked val x: Int)
given [T <: D]: Ord[T] = (a, b) => a.x < b.x

def mySort[T: Ord](x: Array[T]): Array[T] = ???

def test =
  val arr = Array(D(1))
  val arr1 = mySort(arr) // error: no given instance of  type Ord[D{val x: (1 : Int)}]