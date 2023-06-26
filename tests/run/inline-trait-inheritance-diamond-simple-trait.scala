trait TGP[T]:
  def i: T
  def f(x: T): T = x

inline trait IT1[T](x: T) extends TGP[T]:
  override def i: T = x

inline trait IT2 extends TGP[Int]:
  override def i: Int = 999
  def j: String = "inline"
  def g(x: Int): String = x.toString()

trait T extends TGP[Int]

class C1 extends T, IT1[Int](1)
class C2 extends IT1[Int](2), T
class C3 extends IT1[Int](3), IT2
class C4 extends IT2, IT1[Int](4)

@main def Test: Unit =
  for c <- List(C1(), C2(), C3(), C4())
  do println(c.i)