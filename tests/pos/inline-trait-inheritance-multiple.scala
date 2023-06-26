object InlineTraits:
  inline trait IT1:
    def i: Int = 1
    def f[T](x: T): T = x

  inline trait IT2:
    def j: String = "inline"
    def g(x: Int): String = x.toString()

trait T:
  def k: List[Nothing]
  def h(x: Int, y: Double): Double = x + y

class C1 extends InlineTraits.IT1, T:
  def k = Nil

class C2 extends InlineTraits.IT1, InlineTraits.IT2