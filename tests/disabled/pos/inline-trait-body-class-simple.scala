inline trait A:
  class Inner:
    val x = 1

class B extends A:
  def f = Inner().x