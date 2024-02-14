class Names(xs: List[NamedScala | NamedJava]):
  def mkString = xs.map{
    case n: NamedScala => n.self
    case n: NamedJava => n.self
  }.mkString(",")

object Names:
  def single[T <: NamedScala](t: T): Names = Names(List(t))
  def single[T <: NamedJava](t: T): Names = Names(List(t))


@main def Test() =
  Names.single[FooJavaFromJava](identity).mkString
  Names.single[FooJavaFromScala](identity).mkString
  Names(List(new BarJavaFromJava())).mkString
  Names(List(new BarJavaFromScala())).mkString
  Names.single[FooScalaFromJava](identity).mkString // failing in #15402
  Names.single[FooScalaFromScala](identity).mkString // failing in #15402