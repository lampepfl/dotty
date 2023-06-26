inline trait IT1:
  def i: Int = 1
  def f[T](x: T): T = x

inline trait IT2:
  def j: String = "inline"
  def g(x: Int): String = x.toString()

trait T:
  def k: List[Nothing]
  def h(x: Int, y: Double): Double = x + y

class C1 extends IT1, T:
  override def i: Int = 123456
  def k = Nil
  override def h(x: Int, y: Double): Double = 1.0

class C2 extends IT1, IT2:
  override def i: Int = 567890
  override def f[T](x: T): T = ???