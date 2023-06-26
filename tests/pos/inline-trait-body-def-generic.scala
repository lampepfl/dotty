inline trait A:
  def foo[T] = 1
  def bar: [U <: A] => U => Int = [U <: A] => (x: U) => x.foo

class B extends A:
  def f = foo[String] + bar(this)