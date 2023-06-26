inline trait A:
  class InnerA:
    def foo(): Int
    def bar = foo() + 1

class B extends A:
  class InnerB extends InnerA:
    def foo(): Int = -23

  def f = InnerB().bar