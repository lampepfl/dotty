import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 99, classesWithReachableMethods = 14, reachableMethods = 63)
  def main(args: Array[String]): Unit = {
    val classLoader = Test.getClass.getClassLoader()

    try {
      val mainClass = classLoader.loadClass("Test")
      val mainMethod = mainClass.getMethod("dceTest")
      mainMethod.invoke(null);
    } catch {
      case e: java.lang.Exception => e.getCause.printStackTrace()
    }
  }

  @internal.link.AssertNotReachable
  @internal.link.DoNotDeadCodeEliminate
  def shouldDCE(expr: => Any): Unit = try {
    expr
    throw new Exception("Expected DCE")
  } catch {
    case dce: dotty.runtime.DeadCodeEliminated =>
    case _: java.lang.NoSuchMethodError => // agressive DCE
  }

  @internal.link.AssertNotReachable
  @internal.link.DoNotDeadCodeEliminate
  def dceTest: Unit = {
    System.out.println("dceTest")
    Test.shouldDCE(Foo.bar())
    Foo.foo()
  }
}

object Foo {
  @scala.EntryPoint def foo(): Unit = System.out.println(42)
  @internal.link.AssertNotReachable def bar(): Unit = System.out.println(43)
}
