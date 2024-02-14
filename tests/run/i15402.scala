trait Named:
  def me: Named

trait Foo extends Named:
  def me: Foo = this
  def foo(x: Named): Named

trait Foo2 extends Foo

class Names(xs: List[Named]):
  def mkString = xs.map(_.me).mkString(",")

object Names:
  def single[T <: Named](t: T): Names = Names(List(t))

@main def Test() =
  Names.single[Foo](identity).mkString
  Names.single[Foo2](identity).mkString
