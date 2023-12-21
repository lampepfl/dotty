class A
class B
class C[-T]

def foo(using A): C[Any] = ???
def foo(using B): C[Int] = ???


@main def Test =
  given A = A()
  given B = B()
  val x = foo
  val _: C[Any] = x
