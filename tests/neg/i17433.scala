
class C:
  def f(i: Int) = i + 1
  def f(s: String) = s + "_1"

def f = 42

class D extends C:
  def g = f(42) // error

@main def test() = println:
  D().g
