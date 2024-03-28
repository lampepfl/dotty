package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test
import org.junit.Ignore

class CompletionSnippetSuite extends BaseCompletionSuite:

  @Test def `member` =
    checkSnippet(
      """
        |object Main {
        |  List.appl@@
        |}
        |""".stripMargin,
      """|apply($0)
         |""".stripMargin
    )

  @Test def `scope` =
    checkSnippet(
      """
        |object Main {
        |  printl@@
        |
        |}
        |""".stripMargin,
      """|println()
         |println($0)
         |""".stripMargin
    )

  @Test def `nullary` =
    checkSnippet(
      """
        |object Main {
        |  List(1).hea@@
        |}
        |""".stripMargin,
      """|head
         |headOption
         |""".stripMargin
    )

  @Test def `nilary` =
    checkSnippet(
      s"""|class Hello{
          |  def now() = 25
          |}
          |object Main {
          |  val h = new Hello()
          |  h.no@@
          |}
          |""".stripMargin,
      """|now()
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `java-nullary` =
    checkSnippet(
      """
        |class Foo {
        |  override def toString = "Foo"
        |}
        |object Main {
        |  new Foo().toStrin@@
        |
        |}
        |""".stripMargin,
      // even if `Foo.toString` is nullary, it overrides `Object.toString()`
      // which is a Java non-nullary method with an empty parameter list.
      """|toString
         |""".stripMargin
    )

  // Dotty does not currently support fuzzy completions. Please take a look at
  // https://github.com/lampepfl/dotty-feature-requests/issues/314
  @Ignore("Fuzzy should be provided by dotty")
  @Test def `type-empty` =
    if (scala.util.Properties.isJavaAtLeast("9")) {
      checkSnippet(
        """
          |object Main {
          |  type MyType = List[Int]
          |  def list : MT@@
          |}
          |""".stripMargin,
        """|MyType
           |""".stripMargin
      )
    } else {
      checkSnippet(
        """
          |object Main {
          |  type MyType = List[Int]
          |  def list : MT@@
          |}
          |""".stripMargin,
        """|MyType
           |MTOM
           |MTOMFeature
           |""".stripMargin
      )
    }

    // Dotty does not currently support fuzzy completions. Please take a look at
    // https://github.com/lampepfl/dotty-feature-requests/issues/314
  @Ignore("Fuzzy should be provided by dotty")
  @Test def `type-new-empty` =
    if (scala.util.Properties.isJavaAtLeast("9")) {
      checkSnippet(
        """
          |object Main {
          |  class Gen[T]
          |  type MyType = Gen[Int]
          |  new MT@@
          |}
          |""".stripMargin,
        """|MyType
           |""".stripMargin
      )
    } else {
      checkSnippet(
        """
          |object Main {
          |  class Gen[T]
          |  type MyType = Gen[Int]
          |  new MT@@
          |}
          |""".stripMargin,
        """|MyType
           |MTOM
           |MTOMFeature
           |""".stripMargin
      )
    }

  @Test def `type` =
    checkSnippet(
      s"""|object Main {
          |  val x: scala.IndexedSe@@
          |}
          |""".stripMargin,
      // It's expected to have two separate results, one for `object IndexedSeq` (which should not
      // expand snipppet) and one for `type IndexedSeq[T]`.
      """|IndexedSeq[$0]
         |IndexedSeq
         |""".stripMargin
    )

  @Test def `empty-params-with-implicit` =
    checkSnippet(
      s"""|object Main {
          |  def doSomething()(implicit x: Int) = x
          |  val bar = doSomethi@@
          |}
          |""".stripMargin,
      "doSomething($0)"
    )

  @Test def `type3` =
    checkSnippet(
      s"""|object Main {
          |  def foo(param: ArrayDeque@@)
          |}
          |""".stripMargin,
      // ArrayDeque upper is for java, the lower for scala
      """|ArrayDeque[$0]
         |ArrayDeque[$0]
         |ArrayDequeOps[$0]
         |ArrayDeque
         |ArrayDeque
         |""".stripMargin
    )

  @Test def `type4` =
    checkSnippet(
      s"""|object Main {
          |  new SimpleFileVisitor@@
          |}
          |""".stripMargin,
      """|SimpleFileVisitor[$0]
         |SimpleFileVisitor
         |""".stripMargin
    )

  @Test def `type5` =
    checkSnippet(
      s"""|object Main {
          |  new scala.Iterabl@@
          |}
          |""".stripMargin,
      """|Iterable[$0] {}
         |IterableOnce[$0] {}
         |Iterable
         |""".stripMargin
    )

  @Test def `type6` =
    checkSnippet(
      s"""|object Main {
          |  def foo: scala.Iterable@@
          |}
          |""".stripMargin,
      """|Iterable[$0]
         |IterableOnce[$0]
         |Iterable
         |""".stripMargin
    )

  @Test def `type7` =
    checkSnippet(
      s"""|object Main {
          |  def foo(param: List[scala.Iterable@@])
          |}
          |""".stripMargin,
      """|Iterable[$0]
         |IterableOnce[$0]
         |Iterable
         |""".stripMargin
    )

  @Test def `type8` =
    checkSnippet(
      s"""|
          |class Base {
          |  class Inner
          |}
          |object Upper extends Base
          |object Main {
          |  def foo(param: Uppe@@)
          |}
          |""".stripMargin,
      """|Upper
         |""".stripMargin
    )

  @Test def `trailing-paren` =
    checkEditLine(
      s"""|object Main {
          |  def trailing(a: Int) = ()
          |  ___
          |}
          |""".stripMargin,
      "trailing@@()",
      "trailing()"
    )

  @Test def `trailing-brace` =
    checkEditLine(
      s"""|object Main {
          |  def trailing(a: Int) = ()
          |  ___
          |}
          |""".stripMargin,
      "trailing@@ { }",
      "trailing { }"
    )

  @Test def `trailing-brace1` =
    checkEditLine(
      s"""|object Main {
          |  def trailing(a: Int) = ()
          |  ___
          |}
          |""".stripMargin,
      "trailing@@{ }",
      "trailing{ }"
    )

  @Test def `implicit` =
    checkEditLine(
      s"""|object Main {
          |  ___
          |}
          |""".stripMargin,
      "List(1).flatte@@",
      "List(1).flatten"
    )

  // no completions are suggested if we already have full typ =
  @Test def `bug1` =
    checkEditLine(
      s"""|object Main {
          |  ___
          |}
          |""".stripMargin,
      "scala.util.Try@@(1)",
      "scala.util.Try(1)"
    )

  @Test def `case-class` =
    checkEditLine(
      s"""|object Main {
          |  ___
          |}
          |""".stripMargin,
      "scala.util.Tr@@(1)",
      "scala.util.Try(1)",
      filter = str => str.contains("Try")
    )

  @Test def `case-class2` =
    checkSnippet(
      s"""|object Main {
          |  scala.util.Tr@@
          |}
          |""".stripMargin,
      """|Try
         |Try($0)
         |new Try
         |""".stripMargin
    )

  @Test def `case-class3` =
    checkSnippet(
      s"""|object Main {
          |  Try@@
          |}
          |""".stripMargin,
      // Note: the class and trait items in here are invalid. So
      // they are filtered out.
      """|Try -  scala.util
         |Try($0) - [T](r: => T): Try[T]
         |new Try - [T]: Try[T]
         |""".stripMargin,
      includeDetail = true
    )

  @Test def `symbol` =
    checkEditLine(
      s"""|object Main {
          |  val out = new StringBuilder()
          |  ___
          |}
          |""".stripMargin,
      "out.+@@=('a')",
      "out.++==('a')",
      filter = _.contains("++=(s: String)")
    )

  @Test def `multiple-apply` =
    checkSnippet(
      s"""|package example
          |
          |case class Widget(name: String, age: Int)
          |object Widget{
          |  def apply(name: String): Widget = Widget(name, 0)
          |  def apply(age: Int): Widget = Widget("name", age)
          |}
          |object Main {
          |  Wi@@
          |}
          |""".stripMargin,
      """|Widget -  example
         |Widget($0) - (name: String): Widget
         |Widget($0) - (age: Int): Widget
         |Widget($0) - (name: String, age: Int): Widget
         |""".stripMargin,
      includeDetail = true,
      topLines = Some(4)
    )

  @Test def `no-apply` =
    checkSnippet(
      s"""|package example
          |
          |object TestObject {}
          |object Main {
          |  TestObjec@@
          |}
          |""".stripMargin,
      """|TestObject -  example
         |""".stripMargin,
      includeDetail = true,
    )

  @Test def `dont-enter-empty-paramlist` =
    checkSnippet(
      s"""|package example
          |
          |object Main {
          |  ListMa@@
          |}
          |""".stripMargin,
      """|ListMap -  scala.collection.immutable
         |ListMap -  scala.collection.mutable
         |ListMap($0) - [K, V](elems: (K, V)*): ListMap[K, V]
         |new ListMap - [K, V]: ListMap[K, V]
         |ListMapBuilder - [K, V]: ListMapBuilder[K, V]
         |new ListMap - [K, V]: ListMap[K, V]
         |ConcurrentSkipListMap -  java.util.concurrent
         |""".stripMargin,
      includeDetail = true,
    )

  // https://github.com/scalameta/metals/issues/4004
  @Test def `extension-param1` =
    checkEdit(
      s"""|package a
          |object Foo:
          |  extension (s: String)
          |    def bar = 0
          |  val bar = "abc".ba@@
      """.stripMargin,
      s"""|package a
          |object Foo:
          |  extension (s: String)
          |    def bar = 0
          |  val bar = "abc".bar
      """.stripMargin
    )

  // https://github.com/scalameta/metals/issues/4004
  @Test def `extension-param2` =
    checkEdit(
      s"""|package a
          |object Foo:
          |  extension (s: String)
          |    def bar() = 0
          |  val bar = "abc".ba@@
      """.stripMargin,
      s"""|package a
          |object Foo:
          |  extension (s: String)
          |    def bar() = 0
          |  val bar = "abc".bar()
      """.stripMargin
    )
