//> using options -experimental

object Test {
  def main(args: Array[String]): Unit = {
    assert(Macro.foo1(1) == 0)
    assert(Macro.foo2(1) == 0)
    assert(Macro.foo3(1) == 0)
    assert(Macro.foo4(1) == 0)
    assert(Macro.foo5(1) == 0)
    assert(Macro.foo6(1) == 0)
    assert(Macro.foo7(1) == 0)
    assert(Macro.foo8(1) == 0)
  }
}
