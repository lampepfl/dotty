class MyFunction(arg: String)

trait MyFun[+R] extends MyFunction {
  def apply(i: Int): R
}

val myFun: MyFun[Int] = (i: Int) => 1 // error
