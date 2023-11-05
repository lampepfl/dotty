class Foo(val i: Int) extends AnyVal
class Argument(val x: String) extends AnyVal
class Reflective extends reflect.Selectable

type ReflectiveType = {
  def reflectiveCall(arg1: Int)(arg2: Int): Int
}

class ClassWithReflectiveCall {
  def reflectiveCall(x: Int)(y: Int): Int = x + y
}

class ScalaSelectable(values: Map[String, Any], methods: Map[String, (Int, Seq[Foo]) => Int]) extends Selectable {
  def selectDynamic(name: String): Any = values(name)

  def applyDynamic(name: String)(i: Int, foos: Foo*): Int = methods(name)(i, foos)

  def applyDynamic(name: String)(foo: Foo)(argument: Argument)(someInt: Int): Int = foo.i + argument.x.length + someInt
}

@main def Test: Unit =
  val reflective = new Reflective {
    def bar(foo: Foo) = foo.i
    def fun(argument: Argument) = argument
    def manyArgs(argument: Argument, foo: Foo, someInt: Int) = foo.i + someInt + argument.x.length
    def varargs(x: Int, foo: Foo*) = foo.map(_.i).sum + x
    def letsHaveSeq(args: Seq[Argument]) = args.map(_.x.length).sum
    def curried(foo: Foo)(arg1: Argument)(someInt: Int): Int = foo.i + arg1.x.length + someInt
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

  println(reflective.curried(foo1)(arg1)(1))

  val cont2values = Map.empty[String, Any]

  val cont2methods = Map[String, (Int, Seq[Foo]) => Int](
    "varargs" -> { (i: Int, foos: Seq[Foo]) => foos.map(_.i).sum + i }
  )

  val cont2 = ScalaSelectable(cont2values, cont2methods).asInstanceOf[ScalaSelectable {
    def varargs(i: Int, foos: Foo*): Int
    def curried(foo: Foo)(argument: Argument)(someInt: Int): Int
  }]

  println(cont2.varargs(1, Foo(1), Foo(1)))

  println(cont2.curried(Foo(1))(Argument("123"))(3))

  {
    import scala.reflect.Selectable.reflectiveSelectable
    val obj = new ClassWithReflectiveCall()
    def instantiate(): ReflectiveType = obj

    val rtype = instantiate()
    println(rtype.reflectiveCall(1)(2))
  }