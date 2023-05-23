class Foo(val i: Int) extends AnyVal
class Argument(val x: String) extends AnyVal
class Reflective extends reflect.Selectable

@main def Test: Unit =
  val reflective = new Reflective {
    def bar(foo: Foo) = foo.i
    def fun(argument: Argument) = argument
    def manyArgs(argument: Argument, foo: Foo, someInt: Int) = foo.i + someInt + argument.x.length
    def varargs(x: Int, foo: Foo*) = foo.map(_.i).sum + x
    def letsHaveSeq(args: Seq[Argument]) = args.map(_.x.length).sum
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

  val varargs = List(Foo(1), Foo(2), Foo(3))
  val m = reflective.varargs(1, varargs:_*)
  println(m)

  val foo1 = Foo(1)
  val foo2 = Foo(2)
  val foo3 = Foo(3)
  val n = reflective.varargs(2, foo1, foo2)
  println(n)

  val arg1 = Argument("1")
  val seq = Seq(arg1, arg1, arg1)
  val p = reflective.letsHaveSeq(seq)
  println(p)