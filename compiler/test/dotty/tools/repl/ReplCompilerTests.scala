package dotty.tools.repl

import scala.language.unsafeNulls

import java.util.regex.Pattern

import org.junit.Assert.{assertTrue => assert, _}
import org.junit.{Ignore, Test}
import dotty.tools.dotc.core.Contexts.Context

class ReplCompilerTests extends ReplTest:
  import ReplCompilerTests._

  private def lines() =
    storedOutput().trim.linesIterator.toList

  @Test def compileSingle = initially {
    run("def foo: 1 = 1")
    assertEquals("def foo: 1", storedOutput().trim)
  }

  @Test def compileTwo =
    initially {
      run("def foo: 1 = 1")
    } andThen {
      val s2 = run("def foo(i: Int): i.type = i")
      assertEquals(2, s2.objectIndex)
    }

  @Test def inspectWrapper =
    initially {
      run("def foo = 1")
    } andThen {
      storedOutput() // discard output
      run("val x = rs$line$1.foo")
      assertEquals("val x: Int = 1", storedOutput().trim)
    }

  @Test def testVar = initially {
    run("var x = 5")
    assertEquals("var x: Int = 5", storedOutput().trim)
  }

  @Test def testRes = initially {
    run {
      """|def foo = 1 + 1
         |val x = 5 + 5
         |1 + 1
         |var y = 5
         |10 + 10""".stripMargin
    }

    val expected = List(
      "def foo: Int",
      "val x: Int = 10",
      "val res0: Int = 2",
      "var y: Int = 5",
      "val res1: Int = 20"
    )

    assertEquals(expected, lines())
  }

  @Test def testImportMutable =
    initially {
      run("import scala.collection.mutable")
    } andThen {
      assertEquals(1, summon[State].imports.size)
      run("""mutable.Map("one" -> 1)""")
      assertEquals(
        "val res0: scala.collection.mutable.Map[String, Int] = HashMap(one -> 1)",
        storedOutput().trim
      )
    }

  @Test def rebindVariable =
    initially {
      val state = run("var x = 5")
      assertEquals("var x: Int = 5", storedOutput().trim)
      state
    } andThen {
      run("x = 10")
      assertEquals("x: Int = 10", storedOutput().trim)
    }

  @Test def defaultParameter = initially {
    run("def foo(a: Int = 1): 1 = 1")
    assertEquals("def foo(a: Int): 1", storedOutput().trim)
  }

  @Test def i8677 = initially {
    run {
      """|sealed trait T1
         |case class X() extends T1
         |case class Y() extends T1
         |case object O extends T1
         """.stripMargin
    }

    val expected = List(
     "// defined trait T1",
     "// defined case class X",
     "// defined case class Y",
     "// defined case object O"
    )

    assertEquals(expected, lines())
  }

  // FIXME: Tests are not run in isolation, the classloader is corrupted after the first exception
  @Ignore @Test def i3305: Unit = {
    initially {
      run("null.toString")
      assert(storedOutput().startsWith("java.lang.NullPointerException"))
    }

    initially {
      run("def foo: Int = 1 + foo; foo")
      assert(storedOutput().startsWith("def foo: Int\njava.lang.StackOverflowError"))
    }

    initially {
      run("""throw new IllegalArgumentException("Hello")""")
      assert(storedOutput().startsWith("java.lang.IllegalArgumentException: Hello"))
    }

    initially {
      run("val (x, y) = null")
      assert(storedOutput().startsWith("scala.MatchError: null"))
    }
  }

  @Test def i2789: Unit = initially {
    run("(x: Int) => println(x)")
    assert(storedOutput().startsWith("val res0: Int => Unit ="))
  }

  @Test def byNameParam: Unit = initially {
    run("def f(g: => Int): Int = g")
    assert(storedOutput().startsWith("def f(g: => Int): Int"))
  }

  @Test def i4051 = initially {
    val source =
      """val x: PartialFunction[Int, Int] = { case x => x }
        |val y = Map(("A", 1), ("B", 2), ("X", 3)).collect { case (k, v) => v }.toList""".stripMargin

    val expected = List(
      "val x: PartialFunction[Int, Int] = <function1>",
      "val y: List[Int] = List(1, 2, 3)"
    )

    run(source)
    assertEquals(expected, lines())
  }

  @Test def i5897 =
    initially {
      run("given Int = 10")
    } andThen {
      assertEquals(
        "lazy val given_Int: Int",
        storedOutput().trim
      )
      run("implicitly[Int]")
      assertEquals(
        "val res0: Int = 10",
        storedOutput().trim
      )
    }

  @Test def i6200 =
    initially {
      run("""
        |trait Ord[T] {
        |  def compare(x: T, y: T): Int
        |  extension (x: T) def < (y: T) = compare(x, y) < 0
        |  extension (x: T) def > (y: T) = compare(x, y) > 0
        |}
        |
        |given IntOrd: Ord[Int] with {
        |  def compare(x: Int, y: Int) =
        |  if (x < y) -1 else if (x > y) +1 else 0
        |}
      """.stripMargin)
    } andThen {
      assertMultiLineEquals(
        """// defined trait Ord
          |// defined object IntOrd""".stripMargin,
        storedOutput().trim
      )
      run("IntOrd")
      assert(storedOutput().startsWith("val res0: IntOrd.type ="))
    }

  @Test def i7934: Unit = contextually {
    assertFalse(ParseResult.isIncomplete("_ + 1"))  // was: assertThrows[NullPointerException]
  }

  @Test def `i9374 accept collective extensions`: Unit = contextually {
    assert(ParseResult.isIncomplete("extension (x: String)"))
    assert(ParseResult.isIncomplete("extension (x: String) {"))
  }

  @Test def testSingletonPrint = initially {
    run("""val a = "hello"; val x: a.type = a""")
    assertMultiLineEquals("val a: String = hello\nval x: a.type = hello", storedOutput().trim)
  }

  @Test def i6574 = initially {
    run("val a: 1 | 0 = 1")
    assertEquals("val a: 1 | 0 = 1", storedOutput().trim)
  }

  @Test def `i10214 must show classic MatchError` = initially {
    run("val 1 = 2: @unchecked")
    assertEquals("scala.MatchError: 2 (of class java.lang.Integer)", storedOutput().linesIterator.next())
  }
  @Test def `i10214 must show useful regex MatchError` =
    initially {
      run("""val r = raw"\d+".r""")
    } andThen {
      run("""val r() = "abc": @unchecked""")
      assertEquals("scala.MatchError: abc (of class java.lang.String)", storedOutput().linesIterator.drop(1).next())
    }
  @Test def `i10214 must show MatchError on literal type` = initially {
    run("val (x: 1) = 2: @unchecked")
    assertEquals("scala.MatchError: 2 (of class java.lang.Integer)", storedOutput().linesIterator.next())
  }
  @Test def `i12920 must truncate stack trace to user code` = initially {
    run("???")
    val all = lines()
    assertEquals(3, all.length)
    assertEquals("scala.NotImplementedError: an implementation is missing", all.head)
    /* avoid asserting much about line number or elided count
    scala.NotImplementedError: an implementation is missing
      at scala.Predef$.$qmark$qmark$qmark(Predef.scala:344)
      ... 28 elided
     */
  }
  @Test def `i14281 context class loader must be REPL class loader` = initially {
    run("class C ; assert(classOf[C].getClassLoader eq Thread.currentThread.getContextClassLoader)")
    assertEquals(List("// defined class C"), lines())
  }

  def assertNotFoundError(id: String): Unit =
    val lines = storedOutput().linesIterator
    assert(lines.next().startsWith("-- [E006] Not Found Error:"))
    assert(lines.drop(2).next().trim().endsWith(s"Not found: $id"))

  @Test def i4416 = initially {
    val state = run("val x = 1 / 0")
    val all = lines()
    assertEquals(2, all.length)
    assert(all.head.startsWith("java.lang.ArithmeticException:"))
    state
  } andThen {
    val state = run("def foo = x")
    assertNotFoundError("x")
    state
  } andThen {
    run("x")
    assertNotFoundError("x")
  }

  @Test def i4416b = initially {
    val state = run("val a = 1234")
    val _ = storedOutput() // discard output
    state
  } andThen {
    val state = run("val a = 1; val x = ???; val y = x")
    val all = lines()
    assertEquals(3, all.length)
    assertEquals("scala.NotImplementedError: an implementation is missing", all.head)
    state
  } andThen {
    val state = run("x")
    assertNotFoundError("x")
    state
  } andThen {
    val state = run("y")
    assertNotFoundError("y")
    state
  } andThen {
    run("a")   // `a` should retain its original binding
    assertEquals("val res0: Int = 1234", storedOutput().trim)
  }

  @Test def i4416_imports = initially {
    run("import scala.collection.mutable")
  } andThen {
    val state = run("import scala.util.Try; val x = ???")
    val _ = storedOutput() // discard output
    state
  } andThen {
    run(":imports")  // scala.util.Try should not be imported
    assertEquals("import scala.collection.mutable", storedOutput().trim)
  }

  @Test def i4416_types_defs_aliases = initially {
    val state =
      run("""|type Foo = String
             |trait Bar
             |def bar: Bar = ???
             |val x = ???
             |""".stripMargin)
    val all = lines()
    assertEquals(3, all.length)
    assertEquals("scala.NotImplementedError: an implementation is missing", all.head)
    assert("type alias in failed wrapper should not be rendered",
      !all.exists(_.startsWith("// defined alias type Foo = String")))
    assert("type definitions in failed wrapper should not be rendered",
      !all.exists(_.startsWith("// defined trait Bar")))
    assert("defs in failed wrapper should not be rendered",
      !all.exists(_.startsWith("def bar: Bar")))
    state
  } andThen {
    val state = run("def foo: Foo = ???")
    assertNotFoundError("type Foo")
    state
  } andThen {
    val state = run("type B = Bar")
    assertNotFoundError("type Bar")
    state
  } andThen {
    run("bar")
    assertNotFoundError("bar")
  }

  @Test def i14473 = initially {
    run("""val (x,y) = (if true then "hi" else (42,17)): @unchecked""")
    val all = lines()
    assertEquals(2, all.length)
    assertEquals("scala.MatchError: hi (of class java.lang.String)", all.head)
  }

  @Test def i14701 = initially {
    val state = run("val _ = ???")
    val all = lines()
    assertEquals(3, all.length)
    assertEquals("scala.NotImplementedError: an implementation is missing", all.head)
    state
  } andThen {
    run("val _ = assert(false)")
    val all = lines()
    assertEquals(3, all.length)
    assertEquals("java.lang.AssertionError: assertion failed", all.head)
  }

  @Test def `i13097 expect lambda after colon` = contextually:
    assert(ParseResult.isIncomplete("val x = List(42).foreach:"))

  @Test def `i13097 expect template after colon` = contextually:
    assert(ParseResult.isIncomplete("class C:"))

  @Test def i15562: Unit = initially {
    val s1 = run("List(1, 2).filter(_ % 2 == 0).foreach(println)")
    assertEquals("2", storedOutput().trim)
    s1
  } andThen { s1 ?=>
    val comp = tabComplete("List(1, 2).filter(_ % 2 == 0).fore")
    assertEquals(List("foreach"), comp.distinct)
    s1
  } andThen {
    val s2 = run("List(1, 2).filter(_ % 2 == 0).foreach(println)")
    assertEquals("2", storedOutput().trim)
    s2
  }

  @Test def i15562b: Unit = initially {
    val s1 = run("List(1, 2).filter(_ % 2 == 0).foreach(println)")
    assertEquals("2", storedOutput().trim)
    s1
  } andThen { s1 ?=>
    val comp = tabComplete("val x = false + true; List(1, 2).filter(_ % 2 == 0).fore")
    assertEquals(List("foreach"), comp.distinct)
    s1
  } andThen {
    val s2 = run("List(1, 2).filter(_ % 2 == 0).foreach(println)")
    assertEquals("2", storedOutput().trim)
    s2
  }

object ReplCompilerTests:

  private val pattern = Pattern.compile("\\r[\\n]?|\\n");

  // Ensure 'expected' and 'actual' contain the same line separator(s).
  def assertMultiLineEquals(expected: String, actual: String): Unit =
    val expected0 = pattern.matcher(expected).replaceAll(System.lineSeparator)
    val actual0 = pattern.matcher(actual).replaceAll(System.lineSeparator)
    assertEquals(expected0, actual0)

end ReplCompilerTests

class ReplXPrintTyperTests extends ReplTest(ReplTest.defaultOptions :+ "-Xprint:typer"):
  @Test def i9111 = initially {
    run("""|enum E {
           |  case A
           |}""".stripMargin)
    assert(storedOutput().trim().endsWith("// defined class E"))
  }

  @Test def i10883 = initially {
    run("val a = 42")
    assert(storedOutput().trim().endsWith("val a: Int = 42"))
  }
end ReplXPrintTyperTests

class ReplVerboseTests extends ReplTest(ReplTest.defaultOptions :+ "-verbose"):
  @Test def i9111 = initially {
    run("""|enum E {
           |  case A
           |}""".stripMargin)
    assert(storedOutput().trim().endsWith("// defined class E"))
  }

  @Test def i10883 = initially {
    run("val a = 42")
    assert(storedOutput().trim().endsWith("val a: Int = 42"))
  }

  @Test def `i4393-incomplete-catch`: Unit = contextually {
    assert(ParseResult.isIncomplete("""|try {
                                       |  ???
                                       |} catch""".stripMargin))
    assert(ParseResult.isIncomplete("""|try {
                                       |  ???
                                       |} catch {""".stripMargin))
  }

end ReplVerboseTests
