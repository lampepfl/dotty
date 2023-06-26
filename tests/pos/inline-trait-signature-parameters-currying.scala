inline trait A(i: Int)(j: Double):
  def f: Double = i + j

class B extends A(1)(1.0)
