package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionExtensionSuite extends BaseCompletionSuite:

  @Test def `simple` =
    check(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |
         |def main = 100.inc@@
         |""".stripMargin,
      """|incr: Int (extension)
         |asInstanceOf[X0]: X0
         |isInstanceOf[X0]: Boolean
         |""".stripMargin
    )

  @Test def `simple-old-syntax` =
    check(
      """package example
        |
        |object Test:
        |  implicit class TestOps(a: Int):
        |    def testOps(b: Int): String = ???
        |
        |def main = 100.test@@
        |""".stripMargin,
      """testOps(b: Int): String (implicit)
        |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `simple2` =
    check(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |
         |def main = 100.i@@
         |""".stripMargin,
      """|incr: Int (extension)
         |""".stripMargin,
      filter = _.contains("(extension)")
    )

  @Test def `simple2-old-syntax` =
    check(
      """|package example
         |
         |object enrichments:
         |  implicit class TestOps(a: Int):
         |    def testOps(b: Int): String = ???
         |
         |def main = 100.t@@
         |""".stripMargin,
      """|testOps(b: Int): String (implicit)
         |""".stripMargin,
      filter = _.contains("(implicit)")
    )

  @Test def `filter-by-type` =
    check(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |  extension (str: String)
         |    def identity: String = str
         |
         |def main = "foo".i@@
         |""".stripMargin,
      """|identity: String (extension)
         |""".stripMargin, // incr won't be available
      filter = _.contains("(extension)")
    )

  @Test def `filter-by-type-old` =
    check(
      """|package example
        |
        |object enrichments:
        |  implicit class A(num: Int):
        |    def identity2: Int = num + 1
        |  implicit class B(str: String):
        |    def identity: String = str
        |
        |def main = "foo".iden@@
        |""".stripMargin,
     """|identity: String (implicit)
        |indent(x$0: Int): String
        |stripIndent(): String
        |isDefinedAt(idx: Int): Boolean
        |""".stripMargin // identity2 won't be available
    )

  @Test def `filter-by-type-subtype` =
    check(
      """|package example
         |
         |class A
         |class B extends A
         |
         |object enrichments:
         |  extension (a: A)
         |    def doSomething: A = a
         |
         |def main = (new B).do@@
         |""".stripMargin,
      """|doSomething: A (extension)
         |""".stripMargin,
      filter = _.contains("(extension)")
    )

  @Test def `filter-by-type-subtype-old` =
    check(
      """|package example
         |
         |class A
         |class B extends A
         |
         |object enrichments:
         |  implicit class Test(a: A):
         |    def doSomething: A = a
         |
         |def main = (new B).do@@
         |""".stripMargin,
      """|doSomething: A (implicit)
         |""".stripMargin,
      filter = _.contains("(implicit)")
    )

  @Test def `simple-edit` =
    checkEdit(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |
         |def main = 100.inc@@
         |""".stripMargin,
      """|package example
         |
         |import example.enrichments.incr
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |
         |def main = 100.incr
         |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `simple-edit-old` =
    checkEdit(
      """|package example
         |
         |object enrichments:
         |  implicit class A (num: Int):
         |    def incr: Int = num + 1
         |
         |def main = 100.inc@@
         |""".stripMargin,
      """|package example
         |
         |import example.enrichments.A
         |
         |object enrichments:
         |  implicit class A (num: Int):
         |    def incr: Int = num + 1
         |
         |def main = 100.incr
         |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `simple-edit-suffix` =
    checkEdit(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def plus(other: Int): Int = num + other
         |
         |def main = 100.pl@@
         |""".stripMargin,
      """|package example
         |
         |import example.enrichments.plus
         |
         |object enrichments:
         |  extension (num: Int)
         |    def plus(other: Int): Int = num + other
         |
         |def main = 100.plus($0)
         |""".stripMargin
    )

  @Test def `simple-edit-suffix-old` =
    checkEdit(
     """|package example
        |
        |object enrichments:
        |  implicit class A (val num: Int):
        |    def plus(other: Int): Int = num + other
        |
        |def main = 100.pl@@
        |""".stripMargin,
     """|package example
        |
        |import example.enrichments.A
        |
        |object enrichments:
        |  implicit class A (val num: Int):
        |    def plus(other: Int): Int = num + other
        |
        |def main = 100.plus($0)
        |""".stripMargin
    )

  @Test def `simple-empty` =
    check(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |
         |def main = 100.@@
         |""".stripMargin,
      """|incr: Int (extension)
         |""".stripMargin,
      filter = _.contains("(extension)")
    )

  @Test def `simple-empty-old` =
    check(
      """|package example
         |
         |object enrichments:
         |  implicit class TestOps(a: Int):
         |    def testOps(b: Int): String = ???
         |
         |def main = 100.@@
         |""".stripMargin,
      """|testOps(b: Int): String (implicit)
         |""".stripMargin,
      filter = _.contains("(implicit)")
    )

  @Test def `directly-in-pkg1` =
    check(
      """|
         |package example:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |
         |package example2:
         |  def main = 100.inc@@
         |""".stripMargin,
      """|incr: Int (extension)
         |asInstanceOf[X0]: X0
         |isInstanceOf[X0]: Boolean
         |""".stripMargin
    )

  @Test def `directly-in-pkg1-old` =
  check(
      """|
         |package examples:
         |  implicit class A(num: Int):
         |    def incr: Int = num + 1
         |
         |package examples2:
         |  def main = 100.inc@@
         |""".stripMargin,
      """|incr: Int (implicit)
         |asInstanceOf[X0]: X0
         |isInstanceOf[X0]: Boolean
         |""".stripMargin
    )

  @Test def `directly-in-pkg2` =
    check(
      """|package example:
         |  object X:
         |    def fooBar(num: Int) = num + 1
         |  extension (num: Int) def incr: Int = num + 1
         |
         |package example2:
         |  def main = 100.inc@@
         |""".stripMargin,
      """|incr: Int (extension)
         |asInstanceOf[X0]: X0
         |isInstanceOf[X0]: Boolean
         |""".stripMargin
    )

  @Test def `directly-in-pkg2-old` =
    check(
      """|package examples:
         |  object X:
         |    def fooBar(num: Int) = num + 1
         |  implicit class A (num: Int) { def incr: Int = num + 1 }
         |
         |package examples2:
         |  def main = 100.inc@@
         |""".stripMargin,
      """|incr: Int (implicit)
         |asInstanceOf[X0]: X0
         |isInstanceOf[X0]: Boolean
         |""".stripMargin
    )

  @Test def `nested-pkg` =
    check(
      """|package a:  // some comment
         |  package c:
         |    extension (num: Int)
         |        def increment2 = num + 2
         |  extension (num: Int)
         |    def increment = num + 1
         |
         |
         |package b:
         |  def main: Unit = 123.incre@@
         |""".stripMargin,
      """|increment: Int (extension)
         |increment2: Int (extension)
         |""".stripMargin
    )

  @Test def `nested-pkg-old` =
    check(
      """|package aa:  // some comment
         |  package cc:
         |    implicit class A (num: Int):
         |        def increment2 = num + 2
         |  implicit class A (num: Int):
         |    def increment = num + 1
         |
         |
         |package bb:
         |  def main: Unit = 123.incre@@
         |""".stripMargin,
      """|increment: Int (implicit)
         |increment2: Int (implicit)
         |""".stripMargin
    )

  @Test def `name-conflict` =
    checkEdit(
      """
        |package example
        |
        |import example.enrichments.*
        |
        |object enrichments:
        |  extension (num: Int)
        |    def plus(other: Int): Int = num + other
        |
        |def main = {
        |  val plus = 100.plus(19)
        |  val y = 19.pl@@
        |}
        |""".stripMargin,
      """
        |package example
        |
        |import example.enrichments.*
        |
        |object enrichments:
        |  extension (num: Int)
        |    def plus(other: Int): Int = num + other
        |
        |def main = {
        |  val plus = 100.plus(19)
        |  val y = 19.plus($0)
        |}
        |""".stripMargin
    )

  @Test def `implicit-val-var` =
    check(
      """|package example
         |
         |object Test:
         |  implicit class TestOps(val testArg: Int):
         |    var testVar: Int = 42
         |    val testVal: Int = 42
         |    def testOps(b: Int): String = ???
         |
         |def main = 100.test@@
         |""".stripMargin,
      """|testArg: Int (implicit)
         |testVal: Int (implicit)
         |testVar: Int (implicit)
         |testOps(b: Int): String (implicit)
         |""".stripMargin,
      topLines = Some(4)
    )

  @Test def `implicit-val-edit` =
    checkEdit(
      """|package example
         |
         |object Test:
         |  implicit class TestOps(a: Int):
         |    val testVal: Int = 42
         |
         |def main = 100.test@@
         |""".stripMargin,
      """|package example
         |
         |import example.Test.TestOps
         |
         |object Test:
         |  implicit class TestOps(a: Int):
         |    val testVal: Int = 42
         |
         |def main = 100.testVal
         |""".stripMargin,
      assertSingleItem = false
    )
