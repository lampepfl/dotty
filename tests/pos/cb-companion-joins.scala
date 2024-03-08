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

trait AA:
  type X: M
trait BB:
  type X: Num
class CC[X1: {M, Num}] extends AA, BB:
  type X = X1
  X.zero
  X.unit
