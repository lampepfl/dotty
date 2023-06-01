package org.scalajs.testsuite.compiler

import org.junit.Assert.*
import org.junit.Test

class CustomReflectSelectableTestScala3 {
  import CustomReflectSelectableTestScala3.*

  @Test def selectField(): Unit = {
    val obj: reflect.Selectable { val x: Int } = new CustomReflectSelectable(42)
    assertEquals(47, obj.x)
  }

  @Test def callMethod(): Unit = {
    val obj: reflect.Selectable { def foo(x: Int, y: String): String } = new CustomReflectSelectable(42)
    assertEquals("3 bar 42", obj.foo(3, "bar"))
  }

  @Test def callMethodWithValueClass(): Unit = {
    val obj: reflect.Selectable { def bar(bar: Bar): Int } = new CustomReflectSelectable(42)
    assertEquals(1, obj.bar(Bar(1)))
  }

  @Test def callMethodWithVarargs(): Unit = {
    val obj: reflect.Selectable { def varargs(x: Int, args: Bar*): Int } = new CustomReflectSelectable(42)
    assertEquals(4, obj.varargs(2, Bar(1), Bar(1)))
  }

  @Test def callSelectableWithVarargs(): Unit = {
    val cont2values = Map.empty[String, Any]
    val cont2methods = Map[String, (Int, Seq[Bar]) => Int](
      "varargs" -> { (i: Int, bars: Seq[Bar]) => bars.map(_.x).sum + i }
    )
    val cont = ScalaSelectable(cont2values, cont2methods).asInstanceOf[ScalaSelectable {
      def varargs(i: Int, foos: Bar*): Int
    }]
    assertEquals(3, cont.varargs(1, Bar(1), Bar(1)))
  }

  @Test def callSelectableWithVarargsExpansion(): Unit = {
    val cont2values = Map.empty[String, Any]
    val cont2methods = Map[String, (Int, Seq[Bar]) => Int](
      "varargs" -> { (i: Int, bars: Seq[Bar]) => bars.map(_.x).sum + i }
    )
    val cont = ScalaSelectable(cont2values, cont2methods).asInstanceOf[ScalaSelectable {
      def varargs(i: Int, foos: Bar*): Int
    }]
    val args = Seq(Bar(1), Bar(1))
    assertEquals(3, cont.varargs(1, args:_*))
  }
}

object CustomReflectSelectableTestScala3 {
  class Bar(val x: Int) extends AnyVal
  class CustomReflectSelectable(param: Int) extends reflect.Selectable {
    val x: Int = 5 + param

    def foo(x: Int, y: String): String = s"$x $y $param"

    def bar(bar: Bar) = bar.x

    def varargs(x: Int, args: Bar*) = args.map(_.x).sum + x
  }

  class ScalaSelectable(values: Map[String, Any], methods: Map[String, (Int, Seq[Bar]) => Int]) extends Selectable {
    def selectDynamic(name: String): Any = values(name)

    def applyDynamic(name: String)(i: Int, bars: Bar*): Int = methods(name)(i, bars)
  }
}
