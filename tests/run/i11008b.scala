object Foo:
  def unapply(x: String): Int *: Seq[String] *: EmptyTuple = (x.length, x.toList.map(_.toString))

@main def Test =
  "foo" match
    case Foo(1, c) => println("One character " + c)
    case Foo(x, xs*) => println(s"Many $x characters $xs")
