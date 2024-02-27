class Get[+A](val get: A) { def isEmpty: Boolean = get != null }

object Abc:
  def unapply(any: Any): Get[Seq[Int]] = Get(Seq(1, 2, 3))

object Foo:
  def unapply(any: Any): Get[List[Int]] = Get(List(1, 2, 3))

object Bar:
  def unapply(any: Any): Get[::[Int]] = Get(::(1, 2 :: 3 :: Nil))

class Test:
  def i(i: Int): Unit        = ()
  def is(is: Seq[Int]): Unit = ()

  def t0(x: Any): Unit =
    x match { case Abc(xs)        => is(xs) }

  def t1(x: Any): Unit =
    x match { case Foo(xs)           => is(xs) }
    x match { case Foo(x :: xs)      => i(x); is(xs) }
    x match { case Foo(x :: y :: zs) => i(x); i(y); is(zs) }

  def t2(x: Any): Unit =
    x match { case Bar(xs)           => is(xs) }
    x match { case Bar(x :: xs)      => i(x); is(xs) }
    x match { case Bar(x :: y :: zs) => i(x); i(y); is(zs) }

  def t2b(x: Any): Unit =
    x match { case Bar(xs)        => is(xs) }
