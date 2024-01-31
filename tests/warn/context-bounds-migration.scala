

class C[T]
def foo[X: C] = ()

given [T]: C[T] = C[T]()

def Test =
  foo(C[Int]())  // warning
  foo(using C[Int]()) // ok
