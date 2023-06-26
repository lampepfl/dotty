inline trait A:
  def foo: Unit = throw Exception("I should not be run!")

class B extends A:
  override def foo: Unit = ()

@main def Test =
  val b = B()
  b.foo
