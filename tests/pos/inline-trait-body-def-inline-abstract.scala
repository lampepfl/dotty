inline trait A:
  inline def x: Int

class B extends A:
  inline def x = 1
  def f = x