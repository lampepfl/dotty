package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionWorkspaceSuite extends BaseCompletionSuite:

  @Test def files =
    checkEdit(
      """package pkg
        |object Main {
        |  val x = Files@@
        |}
        |""".stripMargin,
      """|package pkg
         |
         |import java.nio.file.Files
         |object Main {
         |  val x = Files
         |}
         |""".stripMargin
    )

  @Test def `import` =
    checkEditLine(
      """package pkg
        |object Main {
        |  ___
        |}
        |""".stripMargin,
      "import Files@@",
      "import java.nio.file.Files",
      filterText = "Files"
    )

  @Test def `import-escape` =
    checkEditLine(
      """package pkg
        |
        |package app {
        |  object Main {
        |    ___
        |  }
        |}
        |package `type` {
        |  object Banana
        |}
        |""".stripMargin,
      "import Banana@@",
      "import pkg.`type`.Banana"
    )

  @Test def `conflict` =
    checkEdit(
      """package pkg
        |trait Serializable
        |object Main extends Serializable@@
        |""".stripMargin,
      """package pkg
        |trait Serializable
        |object Main extends java.io.Serializable
        |""".stripMargin,
      filter = _ == "Serializable - java.io"
    )

  @Test def `import-conflict` =
    checkEdit(
      """package `import-conflict`
        |object Main {
        |  val java = 42
        |  val x = Files@@
        |}
        |""".stripMargin,
      """|package `import-conflict`
         |
         |import java.nio.file.Files
         |object Main {
         |  val java = 42
         |  val x = Files
         |}
         |""".stripMargin,
      filter = _ == "Files - java.nio.file"
    )

  @Test def `import-conflict2` =
    checkEdit(
      """package `import-conflict2`
        |object java
        |object Main {
        |  val x = Files@@
        |}
        |""".stripMargin,
      """|package `import-conflict2`
         |
         |import _root_.java.nio.file.Files
         |object java
         |object Main {
         |  val x = Files
         |}
         |""".stripMargin,
      filter = _ == "Files - java.nio.file"
    )

  @Test def `import-conflict3` =
    checkEdit(
      """|package `import-conflict3`
         |import java.util.concurrent.Future
         |case class Foo(
         |  name: Future@@
         |)
         |""".stripMargin,
      """|package `import-conflict3`
         |import java.util.concurrent.Future
         |case class Foo(
         |  name: scala.concurrent.Future[$0]
         |)
         |""".stripMargin,
      filter = _ == "Future[T] - scala.concurrent"
    )

  @Test def `import-conflict4` =
    checkEdit(
      """|package `import-conflict4`
         |import java.util.concurrent._
         |case class Foo(
         |  name: Future@@
         |)
         |""".stripMargin,
      """|package `import-conflict4`
         |import java.util.concurrent._
         |case class Foo(
         |  name: scala.concurrent.Future[$0]
         |)
         |""".stripMargin,
      filter = _ == "Future[T] - scala.concurrent"
    )

  @Test def `import-no-conflict` =
    checkEdit(
      """|package `import-no-conflict`
         |import java.util.concurrent.{Future => _, _}
         |case class Foo(
         |  name: Future@@
         |)
         |""".stripMargin,
      """|package `import-no-conflict`
         |import java.util.concurrent.{Future => _, _}
         |import scala.concurrent.Future
         |case class Foo(
         |  name: Future[$0]
         |)
         |""".stripMargin,
      filter = _ == "Future[T] - scala.concurrent"
    )

  @Test def `imported-names-check1` =
    checkEdit(
      """|package `imported-names-check`
         |import scala.concurrent.Future
         |object A {
         |  Await@@
         |}
         |""".stripMargin,
      """|package `imported-names-check`
         |import scala.concurrent.Future
         |import scala.concurrent.Await
         |object A {
         |  Await
         |}
         |""".stripMargin,
      filter = _ == "Await - scala.concurrent"
    )

  @Test def `extends` =
    checkEdit(
      """package pkg
        |object Main extends CompletableFutur@@
        |""".stripMargin,
      """package pkg
        |
        |import java.util.concurrent.CompletableFuture
        |object Main extends CompletableFuture[$0]
        |""".stripMargin,
        assertSingleItem = false,
    )

  @Test def `replace` =
    checkEdit(
      """package pkg
        |object Main extends CompletableFu@@ture
        |""".stripMargin,
      """package pkg
        |
        |import java.util.concurrent.CompletableFuture
        |object Main extends CompletableFuture[$0]
        |""".stripMargin,
        assertSingleItem = false,
    )

  @Test def `block1` =
    checkEdit(
      """|object Main {
         |  def foo(): Unit = {
         |    Files@@
         |  }
         |}
         |""".stripMargin,
      """|import java.nio.file.Files
         |object Main {
         |  def foo(): Unit = {
         |    Files
         |  }
         |}
         |""".stripMargin
    )

  @Test def `block2` =
    checkEdit(
      """|object Main {
         |  def foo(): Unit = {
         |    val x = 1
         |    Files@@
         |  }
         |}
         |""".stripMargin,
      """|import java.nio.file.Files
         |object Main {
         |  def foo(): Unit = {
         |    val x = 1
         |    Files
         |  }
         |}
         |""".stripMargin
    )

  @Test def `block3` =
    checkEdit(
      """|object Main {
         |  def foo(): Unit = {
         |    val x = 1
         |    println("".substring(Files@@))
         |  }
         |}
         |""".stripMargin,
      """|import java.nio.file.Files
         |object Main {
         |  def foo(): Unit = {
         |    val x = 1
         |    println("".substring(Files))
         |  }
         |}
         |""".stripMargin
    )

  @Test def `match` =
    checkEdit(
      """|object Main {
         |  def foo(): Unit = 1 match {
         |    case 2 =>
         |      Files@@
         |  }
         |}
         |""".stripMargin,
      """|import java.nio.file.Files
         |object Main {
         |  def foo(): Unit = 1 match {
         |    case 2 =>
         |      Files
         |  }
         |}
         |""".stripMargin
    )

  @Test def `case-if` =
    checkEdit(
      """|object Main {
         |  def foo(): Unit = 1 match {
         |    case 2 if {
         |      Files@@
         |     } =>
         |  }
         |}
         |""".stripMargin,
      """|import java.nio.file.Files
         |object Main {
         |  def foo(): Unit = 1 match {
         |    case 2 if {
         |      Files
         |     } =>
         |  }
         |}
         |""".stripMargin
    )

  @Test def `match-typed` =
    checkEdit(
      """|object Main {
         |  def foo(): Unit = null match {
         |    case x: ArrayDeque@@ =>
         |  }
         |}
         |""".stripMargin,
      """|import java.util.ArrayDeque
         |object Main {
         |  def foo(): Unit = null match {
         |    case x: ArrayDeque[$0] =>
         |  }
         |}
         |""".stripMargin,
      filter = _.contains("java.util"),
      assertSingleItem = false,
    )

  @Test def `type` =
    checkEdit(
      """|object Main {
         |  def foo(): Unit = {
         |    val x: Failure@@
         |  }
         |}
         |""".stripMargin,
      """|import scala.util.Failure
         |object Main {
         |  def foo(): Unit = {
         |    val x: Failure[$0]
         |  }
         |}
         |""".stripMargin,
      filter = _.contains("scala.util"),
      assertSingleItem = false,
    )

  @Test def `partial-function` =
    checkEdit(
      """package pkg
        |object Main {
        |  List(1).collect {
        |    case 2 =>
        |      Files@@
        |  }
        |}
        |""".stripMargin,
      """|package pkg
         |
         |import java.nio.file.Files
         |object Main {
         |  List(1).collect {
         |    case 2 =>
         |      Files
         |  }
         |}
         |""".stripMargin
    )

  @Test def `for` =
    checkEdit(
      """package pkg
        |object Main {
        |  for {
        |    x <- List(1)
        |    y = x + 1
        |    if y > 2
        |    _ = Files@@
        |  } yield x
        |}
        |""".stripMargin,
      """|package pkg
         |
         |import java.nio.file.Files
         |object Main {
         |  for {
         |    x <- List(1)
         |    y = x + 1
         |    if y > 2
         |    _ = Files
         |  } yield x
         |}
         |""".stripMargin
    )

  @Test def `for2` =
    checkEdit(
      """package pkg
        |object Main {
        |  for {
        |    x <- List(1)
        |    _ = {
        |      println(1)
        |      Files@@
        |    }
        |  } yield x
        |}
        |""".stripMargin,
      """|package pkg
         |
         |import java.nio.file.Files
         |object Main {
         |  for {
         |    x <- List(1)
         |    _ = {
         |      println(1)
         |      Files
         |    }
         |  } yield x
         |}
         |""".stripMargin
    )

  @Test def `for3` =
    checkEdit(
      """package pkg
        |object Main {
        |  for {
        |    x <- List(1)
        |    if {
        |      println(x)
        |      Files@@
        |    }
        |  } yield x
        |}
        |""".stripMargin,
      """|package pkg
         |
         |import java.nio.file.Files
         |object Main {
         |  for {
         |    x <- List(1)
         |    if {
         |      println(x)
         |      Files
         |    }
         |  } yield x
         |}
         |""".stripMargin
    )

  @Test def `annotation-def-with-middle-space` =
    checkEdit(
      """|
         |object Main {
         |  @noinline
         |  def foo: ArrayBuffer@@ [Int] = ???
         |}
         |""".stripMargin,
      """|import scala.collection.mutable.ArrayBuffer
         |
         |object Main {
         |  @noinline
         |  def foo: ArrayBuffer [Int] = ???
         |}
         |""".stripMargin,
      filter = _ == "ArrayBuffer - scala.collection.mutable"
    )

  @Test def `annotation-class` =
    checkEdit(
      """|package annotationclass
         |object Main {
         |  @deprecated("", "")
         |  class Foo extends ArrayBuffer@@[Int]
         |}
         |""".stripMargin,
      """|package annotationclass
         |
         |import scala.collection.mutable.ArrayBuffer
         |object Main {
         |  @deprecated("", "")
         |  class Foo extends ArrayBuffer[Int]
         |}
         |""".stripMargin,
      filter = _ == "ArrayBuffer - scala.collection.mutable"
    )

  @Test def `annotation-trait` =
    checkEdit(
      """|package annotationtrait
         |object Main {
         |  @deprecated("", "")
         |  trait Foo extends ArrayBuffer@@[Int]
         |}
         |""".stripMargin,
      """|package annotationtrait
         |
         |import scala.collection.mutable.ArrayBuffer
         |object Main {
         |  @deprecated("", "")
         |  trait Foo extends ArrayBuffer[Int]
         |}
         |""".stripMargin,
      filter = _ == "ArrayBuffer - scala.collection.mutable"
    )

  @Test def `class-param` =
    checkEdit(
      """|package classparam
         |case class Foo(
         |  name: Future@@[String]
         |)
         |""".stripMargin,
      """|package classparam
         |
         |import scala.concurrent.Future
         |case class Foo(
         |  name: Future[String]
         |)
         |""".stripMargin,
      filter = _ == "Future - scala.concurrent"
    )

  @Test def `docstring` =
    checkEdit(
      """|package docstring
         |/**
         | * Hello
         | */
         |object Main {
         |  val x = Future@@
         |}
         |""".stripMargin,
      """|package docstring
         |
         |import scala.concurrent.Future
         |/**
         | * Hello
         | */
         |object Main {
         |  val x = Future
         |}
         |""".stripMargin,
      filter = _ == "Future - scala.concurrent"
    )

  @Test def `docstring-import` =
    checkEdit(
      """|package docstring
         |import scala.util._
         |/**
         | * Hello
         | */
         |object Main {
         |  val x = Future@@
         |}
         |""".stripMargin,
      """|package docstring
         |import scala.util._
         |import scala.concurrent.Future
         |/**
         | * Hello
         | */
         |object Main {
         |  val x = Future
         |}
         |""".stripMargin,
      filter = _ == "Future - scala.concurrent"
    )

  @Test def `empty-pkg` =
    checkEdit(
      """|import scala.util._
         |object Main {
         |  val x = Future@@
         |}
         |""".stripMargin,
      """|import scala.util._
         |import scala.concurrent.Future
         |object Main {
         |  val x = Future
         |}
         |""".stripMargin,
      filter = _ == "Future - scala.concurrent"
    )

  @Test def `parent-object` =
    checkEdit(
      """|object Main {
         |  Implicits@@
         |}
         |""".stripMargin,
      """|import scala.concurrent.ExecutionContext.Implicits
         |object Main {
         |  Implicits
         |}
         |""".stripMargin,
      filter = _ == "Implicits - scala.concurrent.ExecutionContext"
    )

  // this test was intended to check that import is rendered correctly - without `$` symbol
  // but it spotted the difference in scala2/scala3 `AutoImports` implementation
  // this one might be removed / joined with `parent-object-scala2` in future
  @Test def `parent-object-scala3` =
    checkEdit(
      """|object Main {
         |  Implicits@@
         |}
         |""".stripMargin,
      """|import scala.concurrent.ExecutionContext.Implicits
         |object Main {
         |  Implicits
         |}
         |""".stripMargin,
      filter = _ == "Implicits - scala.concurrent.ExecutionContext"
    )

  @Test def `specify-owner` =
    checkEdit(
      """|object Main {
         |  Map@@
         |}
         |""".stripMargin,
      """|import scala.collection.mutable
         |object Main {
         |  mutable.Map
         |}
         |""".stripMargin,
      filter = _ == "Map - scala.collection.mutable"
    )

  @Test def `renamed-mutable` =
    checkEdit(
      """|import scala.collection.{mutable => mut}
         |object Main {
         |  Map@@
         |}
         |""".stripMargin,
      """|import scala.collection.{mutable => mut}
         |object Main {
         |  mut.Map
         |}
         |""".stripMargin,
      filter = _ == "Map - scala.collection.mutable"
    )

  @Test def `ju-import` =
    checkEdit(
      """|object Main {
         |  Map@@
         |}
         |""".stripMargin,
      """|import java.{util => ju}
         |object Main {
         |  ju.Map
         |}
         |""".stripMargin,
      filter = _ == "Map - java.util"
    )

  @Test def `ju-import-dup` =
    checkEdit(
      """|import java.{util => ju}
         |object Main {
         |  Map@@
         |}
         |""".stripMargin,
      """|import java.{util => ju}
         |object Main {
         |  ju.Map
         |}
         |""".stripMargin,
      filter = _ == "Map - java.util"
    )

  @Test def `ordering-1` =
    check(
      """|import scala.concurrent.Future
         |object Main {
         |  def foo(
         |    x: Futu@@
         |  ): String = ???
         |}
         |""".stripMargin,
      """|Future[T] scala.concurrent
         |Future scala.concurrent
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `ordering-2` =
    check(
      """|import java.util.concurrent.Future
         |object Main {
         |  def foo(
         |    x: Futu@@
         |  ): String = ???
         |}
         |""".stripMargin,
      """|Future[V] java.util.concurrent
         |Future java.util.concurrent
         |Future[T] - scala.concurrent
         |""".stripMargin,
      topLines = Some(3)
    )

  @Test def `apply-method` =
    checkEdit(
      """|object Main {
         |  val a = ListBuf@@
         |}""".stripMargin,
      """|import scala.collection.mutable.ListBuffer
         |object Main {
         |  val a = ListBuffer($0)
         |}""".stripMargin,
      filter = _.startsWith("ListBuffer[A]")
    )

  @Test def `type-import` =
    checkEdit(
      """|package a {
         |  object A {
         |    type Beta = String
         |    def m(): Int = ???
         |  }
         |}
         |
         |package b {
         |  object B{
         |    val x: Bet@@
         |  }
         |}""".stripMargin,
      """|import a.A.Beta
         |package a {
         |  object A {
         |    type Beta = String
         |    def m(): Int = ???
         |  }
         |}
         |
         |package b {
         |  object B{
         |    val x: Beta
         |  }
         |}
         |""".stripMargin
    )

  @Test def `directly-in-pkg` =
    checkEdit(
      """|package a:
         |  object Y:
         |    val bar = 123
         |  val fooBar = 123
         |
         |package b:
         |  def main() = fooB@@
         |""".stripMargin,
      """|import a.fooBar
         |package a:
         |  object Y:
         |    val bar = 123
         |  val fooBar = 123
         |
         |package b:
         |  def main() = fooBar
         |""".stripMargin
    )

  @Test def `nested-pkg` =
    check(
      """|package a:
         |  package c: // some comment
         |    def increment2 = 2
         |  def increment = 1
         |
         |package d:
         |  val increment3 = 3
         |
         |package b:
         |  def main: Unit = incre@@
         |""".stripMargin,
      """|increment3: Int
         |increment - a: Int
         |increment2 - a.c: Int
         |""".stripMargin
    )

  @Test def `indent-method` =
    check(
      """|package a:
         |  val y = 123
         |  given intGiven: Int = 123
         |  type Alpha = String
         |  class Foo(x: Int)
         |  object X:
         |    val x = 123
         |  def fooBar(x: Int) = x + 1
         |  package b:
         |    def fooBar(x: String) = x.length
         |
         |package c:
         |  def main() = foo@@
         |""".stripMargin,
      """|fooBar - a(x: Int): Int
         |fooBar - a.b(x: String): Int
         |""".stripMargin
    )

  @Test def `case-class-param` =
    check(
      """|case class Foo(fooBar: Int, gooBar: Int)
         |class Bar(val fooBaz: Int, val fooBal: Int) {
         |  val fooBar: Option[Int] = Some(1)
         |}
         |object A {
         |  val fooBar: List[Int] = List(1)
         |}
         |
         |object Main {
         |  val fooBar = "Abc"
         |  val x = fooBa@@
         |}
         |""".stripMargin,
      """|fooBar: String
         |fooBar: List[Int]
         |fooBar(n: Int): Int - test.A
         |""".stripMargin,
    )

  @Test def `type-apply` =
    check(
      """|package demo
         |
         |package other:
         |  type MyType = Long
         |
         |  object MyType:
         |    def apply(m: Long): MyType = m
         |
         |val j = MyTy@@
         |""".stripMargin,
      """|MyType - demo.other
         |MyType(m: Long): MyType - demo.other
         """.stripMargin,
    )

  @Test def `type-apply2` =
    check(
      """|package demo
         |
         |package other:
         |  object MyType:
         |    def apply(m: Long): MyType = m
         |
         |  type MyType = Long
         |
         |val j = MyTy@@
         |""".stripMargin,
      """|MyType - demo.other
         |MyType(m: Long): MyType - demo.other
      """.stripMargin,
    )

  @Test def `method-name-conflict` =
    checkEdit(
      """|package demo
         |
         |object O {
         |  def mmmm(x: Int) = x + 3
         |  class Test {
         |    val mmmm = "abc"
         |    val foo = mmmm@@
         |  }
         |}
         |""".stripMargin,
      """|package demo
         |
         |object O {
         |  def mmmm(x: Int) = x + 3
         |  class Test {
         |    val mmmm = "abc"
         |    val foo = demo.O.mmmm($0)
         |  }
         |}
         |""".stripMargin,
      filter = _.contains("mmmm - demo.O")
    )

  @Test def `method-label` =
    check(
      """|package demo
         |
         |object O {
         | def method(i: Int): Int = i + 1
         |}
         |
         |object Main {
         |  val x = meth@@
         |}
         |""".stripMargin,
      """|method - demo.O(i: Int): Int
         |""".stripMargin
    )

  @Test def `implicit-class-val` =
    check(
      """|package demo
         |
         |object O {
         |  implicit class CursorOps(val bar: Int)
         |}
         |
         |object Main {
         |  val x = bar@@
         |}
         |""".stripMargin,
      ""
    )

  @Test def `implicit-class-def` =
    check(
      """|package demo
         |
         |object O {
         |  implicit class CursorOps(val bar: Int) {
         |    def fooBar = 42
         |  }
         |}
         |
         |object Main {
         |  val x = fooB@@
         |}
         |""".stripMargin,
      ""
    )

  @Test def `extension-method` =
    check(
      """|package demo
         |
         |object O {
         |  extension (bar: Int) {
         |    def fooBar = 42
         |  }
         |}
         |
         |object Main {
         |  val x = fooB@@
         |}
         |""".stripMargin,
      ""
    )
