package dotty.tools.repl

import scala.language.unsafeNulls

import org.junit.Assert._
import org.junit.Test

/** These tests test input that has proved problematic */
class TabcompleteTests extends ReplTest {

  @Test def tabCompleteList = initially {
    val comp = tabComplete("List.r")
    assertEquals(List("range"), comp.distinct)
  }

  @Test def tabCompleteListInstance = initially {
    val comp = tabComplete("(null: List[Int]).sli")
    assertEquals(List("slice", "sliding"), comp.distinct)
  }

  @Test def tabCompleteModule = initially {
    val comp = tabComplete("scala.Pred")
    assertEquals(List("Predef"), comp)
  }

  @Test def tabCompleteInClass = initially {
    val comp = tabComplete("class Foo { def bar: List[Int] = List.ap")
    assertEquals(List("apply"), comp)
  }

  @Test def tabCompleteTwiceIn = {
    val src1 = "class Foo { def bar(xs: List[Int]) = xs.map"
    val src2 = "class Foo { def bar(xs: List[Int]) = xs.mapC"

    initially {
      val comp1 = tabComplete(src1)
      assertEquals(List("map", "mapConserve"), comp1)
      val comp2 = tabComplete(src2)
      assertEquals(List("mapConserve"), comp2)
    }
  }

  @Test def i3309 = initially {
    // We make sure we do not crash
    List("\"", ")", "'", "¨", "£", ":", ",", ";", "@", "}", "[", "]", ".")
      .foreach(tabComplete(_))
  }

  @Test def completeFromPreviousState =
    initially {
      val src = "class Foo { def comp3 = 3; def comp1 = 1; def comp2 = 2 }"
      run(src)
    } andThen {
      val expected = List("comp1", "comp2", "comp3")
      assertEquals(expected, tabComplete("(new Foo).comp"))
    }

  @Test def completeFromPreviousState2 =
    initially {
      val src = "def hello = 1"
      run(src)
    } andThen {
      val expected = List("hello")
      assertEquals(expected, tabComplete("hel"))
    }

  @Test def tabCompleteFromPreviousImport =
    initially {
      val src = "import java.io.FileDescriptor"
      run(src)
    } andThen {
      val expected = List("FileDescriptor")
      assertEquals(expected, tabComplete("val foo: FileDesc"))
    }

  @Test def tabCompleteRenamedImport =
    initially {
      val src = "import java.io.{FileDescriptor => Renamed}"
      run(src)
    } andThen {
      val expected = List("Renamed")
      assertEquals(expected, tabComplete("val foo: Rena"))
    }

  @Test def tabClosureComplete = initially {
    assertEquals(List("map", "mapConserve"), tabComplete("Nil.map"))
    assertEquals(List("map", "mapConserve"), tabComplete("(x: Int => Int) => Nil.map"))
    assertEquals(List("apply"), tabComplete("(x: Int => Int) => x.ap"))
  }

  @Test def importScala = initially {
    val comp = tabComplete("import scala.")
    // check that there are no special symbols leaked: <byname>, <special-ops>, ...
    assertEquals(Some("<:<"), comp.find(_.startsWith("<")))
    assert(!comp.contains("package"))
  }

  @Test def `null` = initially {
    val comp = tabComplete("null.")
    assertEquals(
      List("!=", "##", "==", "asInstanceOf", "eq", "equals", "getClass", "hashCode",
          "isInstanceOf", "ne", "notify", "notifyAll", "synchronized", "toString", "wait"),
      comp.distinct.sorted)
  }

  @Test def anyRef = initially {
    val comp = tabComplete("(null: AnyRef).")
    assertEquals(
      List("!=", "##", "->", "==", "asInstanceOf", "ensuring", "eq", "equals", "formatted",
          "getClass", "hashCode", "isInstanceOf", "ne", "nn", "notify", "notifyAll", "synchronized", "toString", "wait", "→"),
      comp.distinct.sorted)
  }

  @Test def `???` = initially {
    val comp = tabComplete("???.")
    assertEquals(Nil, comp)
  }

  @Test def moduleCompletion = initially {
    assertEquals(List("Predef"), tabComplete("object Foo { type T = Pre"))
  }

  @Test def i6415 = initially {
    assertEquals(List("Predef"), tabComplete("object Foo { opaque type T = Pre"))
  }

  @Test def i6361 = initially {
    assertEquals(Nil, tabComplete("object foo { given bar: Int = 10 }; import foo.*; ba"))
  }

  @Test def i12600 = initially {
    assertEquals(List("select", "show", "simplified", "substituteTypes"),
      tabComplete("import quoted.* ; def fooImpl(using Quotes): Expr[Int] = { import quotes.reflect.* ; TypeRepr.of[Int].s"))
  }

  @Test def backticked = initially {
    assertEquals(
      List(
        "!=",
        "##",
        "->",
        "==",
        "__system",
        "`back-tick`",
        "`match`",
        "asInstanceOf",
        "dot_product_*",
        "ensuring",
        "eq",
        "equals",
        "foo",
        "formatted",
        "fromOrdinal",
        "getClass",
        "hashCode",
        "isInstanceOf",
        "ne",
        "nn",
        "notify",
        "notifyAll",
        "synchronized",
        "toString",
        "valueOf",
        "values",
        "wait",
        "→"
      ),
      tabComplete("""|enum Foo:
                     |  case `back-tick`
                     |  case `match`
                     |  case foo
                     |  case dot_product_*
                     |  case __system
                     |
                     |Foo.""".stripMargin))
  }


  @Test def backtickedAlready = initially {
    assertEquals(
      List(
        "`back-tick`"
      ),
      tabComplete("""|enum Foo:
                     |  case `back-tick`
                     |  case `match`
                     |  case foo
                     |  case dot_product_*
                     |  case __system
                     |
                     |Foo.`bac""".stripMargin))
  }

  @Test def backtickedImport = initially {
    assertEquals(
      List(
        "`scalaUtilChainingOps`",
        "`synchronized`"
      ),
      tabComplete("import scala.util.chaining.`s"))
  }

  @Test def commands = initially {
    assertEquals(
      List(
        ":doc",
        ":exit",
        ":help",
        ":imports",
        ":load",
        ":quit",
        ":reset",
        ":settings",
        ":type"
      ),
      tabComplete(":")
    )
  }

  @Test def commandPreface = initially {
    // This looks odd, but if we return :doc here it will result in ::doc in the REPL
    assertEquals(
      List(":doc"),
      tabComplete(":d")
    )
  }

  @Test def `i16551 typer phase for implicits` = initially {
    val comp = tabComplete("BigInt(1).")
    assertTrue(comp.distinct.nonEmpty)
  }

  @Test def i9334 = initially {
    assert(tabComplete("class Foo[T]; classOf[Foo].").contains("getName"))
  }
}
