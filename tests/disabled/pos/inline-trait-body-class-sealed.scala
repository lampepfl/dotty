inline trait A:
  sealed class InnerA:
    val x = 1

class B extends A:
  class InnerB extends InnerA
  def f = InnerB().x