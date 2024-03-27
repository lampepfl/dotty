import language.experimental.modularity
import language.future

trait M:
  type Self
  extension (x: Self) def combine (y: Self): String
  def unit: Self

trait Num:
  type Self
  def zero: Self

trait A extends M
trait B extends M

def f[X: {M, A, B}](x: X) =
    summon[X forms M]
    x.combine(x)

trait AA:
  type XX: {M, A, B}
  val x = XX.unit
  val A: String = "hello"

trait AAA:
  type X: M
trait BBB:
  type X: Num
class CCC[X1: {M, Num}] extends AAA, BBB:
  type X = X1
  X.zero
  X.unit

@main def Test =
  class C

  given C forms M:
    extension (x: Self) def combine (y: Self) = "M"
    def unit = C()

  given C forms A:
    extension (x: Self) def combine (y: Self) = "A"
    def unit = C()

  given C forms B:
    extension (x: Self) def combine (y: Self) = "B"
    def unit = C()

  assert(f(C()) == "M")

  class CC extends AA:
    type XX = C
    assert(A.length == 5)
    assert(A.toString == "hello")

  CC()


