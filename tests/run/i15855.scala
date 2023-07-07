class MyFunction1()

trait MyFun1[+R] extends MyFunction1 {
  def apply(i: Int): R
}

val myFun1: MyFun1[Int] = (i: Int) => 1

//

class MyFunction2(arg: String = "default") {
  println(arg)
}

trait MyFun2[+R] extends MyFunction2 {
  def apply(i: Int): R
}

val myFun2: MyFun2[Int] = (i: Int) => 2

//

class MyFunction3(arg: String*) {
  println(arg)
}

trait MyFun3[+R] extends MyFunction3 {
  def apply(i: Int): R
}

val myFun3: MyFun3[Int] = (i: Int) => 3

//

class MyFunction4(arg: String) {
  println(arg)
  def this() = {
    this("other")
  }
}

trait MyFun4[+R] extends MyFunction4 {
  def apply(i: Int): R
}

val myFun4: MyFun4[Int] = (i: Int) => 4

//

class MyFunction5(arg: String = "asdf")(arg1: Int = 420, arg2: Double = 21.37) {
  println(arg)
  println(arg1)
  println(arg2)
}

trait MyFun5[+R] extends MyFunction5 {
  def apply(i: Int): R
}

val myFun5: MyFun5[Int] = (i: Int) => 5

//

trait Equiv[T] extends Any with Serializable {
  def equiv(x: T, y: T): Boolean
}

def reference[T <: AnyRef]: Equiv[T] = { _ eq _ }
def universal[T]: Equiv[T] = { _ == _ }
def fromFunction[T](cmp: (T, T) => Boolean): Equiv[T] = {
  (x, y) => cmp(x, y)
}

//

object Test extends App {
  println(myFun1(1))
  println(myFun2(1))
  println(myFun3(1))
  println(myFun4(1))
  println(myFun5(1))
}
