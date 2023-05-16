class Foo(val i: Int) extends AnyVal
class Argument(val x: String) extends AnyVal
class Reflective extends reflect.Selectable

@main def Test: Unit =
  val reflective = new Reflective {
    def bar(foo: Foo) = foo.i
    def fun(argument: Argument) = argument
    def manyArgs(argument: Argument, foo: Foo, someInt: Int) = foo.i + someInt + argument.x.length
  }
  
  val i = reflective.bar(Foo(1))
  println(i)
  
  val arg = Argument("check")
  val k = reflective.fun(arg).x
  println(k)
  
  val length4 = Argument("four")
  val foo = Foo(1)
  val x = 1
  val j = reflective.manyArgs(length4, foo, x)
  println(j)