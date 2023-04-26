// scalajs: --skip
import scala.language.unsafeNulls

import java.lang.invoke._, MethodType.methodType

// A copy of tests/run/i11332.scala
// to test the bootstrap minimisation which failed
// (because bootstrap runs under explicit nulls)
class Foo:
  def neg(x: Int): Int = -x

object Test:
  def main(args: Array[String]): Unit =
    val l    = MethodHandles.lookup()
    val self = new Foo()

    val res4 = {
      l // explicit chain method call - previously derivedSelect broke the type
        .findVirtual(classOf[Foo], "neg", methodType(classOf[Int], classOf[Int]))
        .invokeExact(self, 4): Int
    }
    assert(-4 == res4)
