object FailingTransparentInline:
  sealed trait Foo
  case class FooA() extends Foo
  case class FooB() extends Foo

  transparent inline def execute(): Foo = ${ executeImpl() }
  def executeImpl(using Quotes)() = {
    val a = 0
    a.asInstanceOf[String]
    FooB()
  }
