inline trait A[T]:
  var x = 1
  def foo =
    if x == 1 then println("1")
    x = 1 - x
    println("foo")

  foo
  foo
  foo

class B extends A[Int]:
  def bar =
    if x == 1 then println("1")
    println("bar")

  bar
  bar
  bar

@main def Test =
  B()