inline trait A:
  class Inner(val x: Int)

class B extends A:
  def f = Inner(17).x