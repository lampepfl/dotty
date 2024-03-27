import language.future

class A
class B extends A
class C extends A

given A = A()
given B = B()
given C = C()

def f(using a: A, b: B, c: C) =
  println(a.getClass)
  println(b.getClass)
  println(c.getClass)

@main def Test = f
