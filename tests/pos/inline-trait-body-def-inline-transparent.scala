inline trait A:
  transparent inline def x = 1

class B extends A:
  def f: 1 = x