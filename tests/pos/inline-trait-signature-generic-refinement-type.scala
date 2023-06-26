import reflect.Selectable.reflectiveSelectable

class C[T](x: T):
  def foo(): T = x

inline trait A[T, U[T]](u: U[T]{ def foo(): T }):
  def f: T = u.foo()

class B extends A(C(1))
