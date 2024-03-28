package dotty.tools.pc.tests.completion

import scala.meta.pc.SymbolDocumentation

import dotty.tools.pc.base.BaseCompletionSuite
import dotty.tools.pc.utils.MockEntries

import org.junit.Test

class CompletionOverrideSuite extends BaseCompletionSuite:

  override protected def mockEntries: MockEntries = new MockEntries:
    override def documentations: Set[SymbolDocumentation] = Set(
      MockDocumentation(
        "java/nio/file/SimpleFileVisitor#visitFile().",
        "visitFile",
        Seq(),
        Seq("file", "attrs")
      )
    )

  @Test def `basic` =
    checkEdit(
      """
        |object Main extends AutoCloseable {
        |  def close@@
        |}
      """.stripMargin,
      """
        |object Main extends AutoCloseable {
        |  def close(): Unit = ${0:???}
        |}
        |""".stripMargin
    )

  @Test def `overload` =
    checkEdit(
      """
        |trait Interface {
        |  def foo(a: Int): Unit
        |  def foo(a: String): Unit
        |}
        |object Main extends Interface {
        |  override def foo(a: Int): Unit = ()
        |  override def foo@@
        |}
      """.stripMargin,
      """
        |trait Interface {
        |  def foo(a: Int): Unit
        |  def foo(a: String): Unit
        |}
        |object Main extends Interface {
        |  override def foo(a: Int): Unit = ()
        |  override def foo(a: String): Unit = ${0:???}
        |}
        |""".stripMargin
    )

  @Test def `seen-from` =
    checkEdit(
      """
        |object Main {
        |  new Iterable[Int] {
        |    def iterato@@
        |  }
        |}
      """.stripMargin,
      """
        |object Main {
        |  new Iterable[Int] {
        |    def iterator: Iterator[Int] = ${0:???}
        |  }
        |}
        |""".stripMargin,
      filter = _.contains("iterat")
    )

  @Test def `generic` =
    checkEdit(
      """
        |object Main {
        |  new scala.Traversable[Int] {
        |    def foreach@@
        |  }
        |}
      """.stripMargin,
      """
        |object Main {
        |  new scala.Traversable[Int] {
        |    override def foreach[U](f: Int => U): Unit = ${0:???}
        |  }
        |}
        |""".stripMargin
    )

  @Test def `context-bound` =
    checkEdit(
      """
        |trait Context {
        |   def add[T:Ordering]: T
        |}
        |object Main {
        |  new Context {
        |    override def add@@
        |  }
        |}
      """.stripMargin,
      """
        |trait Context {
        |   def add[T:Ordering]: T
        |}
        |object Main {
        |  new Context {
        |    override def add[T: Ordering]: T = ${0:???}
        |  }
        |}
        |""".stripMargin
    )

  @Test def `import` = // import position is flak
    checkEdit(
      """
        |object Main {
        |  new java.nio.file.SimpleFileVisitor[java.nio.file.Path] {
        |    def visitFil@@
        |  }
        |}
      """.stripMargin,
      """|import java.nio.file.attribute.BasicFileAttributes
         |import java.nio.file.FileVisitResult
         |import java.nio.file.Path
         |
         |object Main {
         |  new java.nio.file.SimpleFileVisitor[java.nio.file.Path] {
         |    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = ${0:???}
         |  }
         |}
         |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `empty` =
    check(
      """
        |trait SuperAbstract {
        |  def aaa: Int = 2
        |}
        |trait Abstract extends SuperAbstract {
        |  def bbb: Int = 2
        |  type TypeAlias = String // should be ignored
        |}
        |object Main {
        |  new Abstract {
        |    def @@
        |  }
        |}
      """.stripMargin,
      // assert that `isInstanceOf` and friends are not included
      """|override def aaa: Int
         |override def bbb: Int
         |override def equals(x$0: Any): Boolean
         |override def hashCode(): Int
         |override def toString(): String
         |""".stripMargin,
      includeDetail = false
    )

  def implement(completion: String): String =
    s"""
       |trait Abstract {
       |  def implementMe: Int
       |}
       |object Main {
       |  new Abstract {
       |    $completion
       |  }
       |}
    """.stripMargin

  @Test def `implement-test` =
    checkEdit(
      // assert that `override` is not inserted.
      implement("def implement@@"),
      implement("def implementMe: Int = ${0:???}")
    )

  @Test def `implement-override` =
    checkEdit(
      // assert that `override` is inserted.
      implement("override def implement@@"),
      implement("override def implementMe: Int = ${0:???}")
    )

  @Test def `error` =
    checkEdit(
      """
        |object Main {
        |  new scala.Iterable[Unknown] {
        |    def iterato@@
        |  }
        |}
      """.stripMargin,
      // Replace error types with type parameter name `A`. IntelliJ converts the unknown type
      // into `Any`, which is less helpful IMO.
      """
        |object Main {
        |  new scala.Iterable[Unknown] {
        |    def iterator: Iterator[A] = ${0:???}
        |  }
        |}
      """.stripMargin,
      filter = _.contains("iterat")
    )

  @Test def `sort` =
    check(
      """
        |trait Super {
        |  def a: Int = 2
        |  def b: Int
        |}
        |object Main {
        |  new Super {
        |    def @@
        |  }
        |}
      """.stripMargin,
      // assert that `isInstanceOf` and friends are not included
      """|def b: Int
         |override def a: Int
         |""".stripMargin,
      topLines = Some(2),
      includeDetail = false
    )

  @Test def `conflict` =
    checkEdit(
      s"""package a.b
         |abstract class Conflict {
         |  def self: Conflict
         |}
         |object Main {
         |  class Conflict
         |  new a.b.Conflict {
         |    def self@@
         |  }
         |}
         |""".stripMargin,
      """|package a.b
         |abstract class Conflict {
         |  def self: Conflict
         |}
         |object Main {
         |  class Conflict
         |  new a.b.Conflict {
         |    def self: a.b.Conflict = ${0:???}
         |  }
         |}
         |""".stripMargin
    )

  @Test def `conflict2` =
    check(
      s"""package a.c
         |abstract class Conflict {
         |  type Inner
         |  def self: Conflict
         |  def selfArg: Option[Conflict]
         |  def selfPath: Conflict#Inner
         |}
         |object Main {
         |  class Conflict
         |  val a = 2
         |  new _root_.a.c.Conflict {
         |    def self@@
         |  }
         |}
         |""".stripMargin,
      """|def self: a.c.Conflict
         |def selfArg: Option[a.c.Conflict]
         |def selfPath: Inner
         |""".stripMargin,
      includeDetail = false
    )

  @Test def `mutable` =
    checkEdit(
      """|abstract class Mutable {
         |  def foo: scala.collection.mutable.Set[Int]
         |}
         |object Main {
         |  new Mutable {
         |    def foo@@
         |  }
         |}
         |""".stripMargin,
      """|import scala.collection.mutable
         |abstract class Mutable {
         |  def foo: scala.collection.mutable.Set[Int]
         |}
         |object Main {
         |  new Mutable {
         |    def foo: mutable.Set[Int] = ${0:???}
         |  }
         |}
         |""".stripMargin
    )

  @Test def `mutable-conflict` =
    checkEditLine(
      s"""|abstract class Mutable {
          |  def foo: scala.collection.mutable.Set[Int]
          |}
          |object Main {
          |  new Mutable {
          |    val mutable = 42
          |___
          |  }
          |}
          |""".stripMargin,
      "    def foo@@",
      """    def foo: scala.collection.mutable.Set[Int] = ${0:???}"""
    )

  @Test def `jutil` =
    checkEdit(
      """|abstract class JUtil {
         |  def foo: java.util.List[Int]
         |}
         |class Main extends JUtil {
         |  def foo@@
         |}
         |""".stripMargin,
      """|import java.{util => ju}
         |abstract class JUtil {
         |  def foo: java.util.List[Int]
         |}
         |class Main extends JUtil {
         |  def foo: ju.List[Int] = ${0:???}
         |}
         |""".stripMargin
    )

  @Test def `jutil-package` =
    checkEdit(
      """|abstract class JUtil {
         |  def foo: java.util.concurrent.CompletableFuture[Int]
         |}
         |class Main extends JUtil {
         |  def foo@@
         |}
         |""".stripMargin,
      """|import java.util.concurrent.CompletableFuture
         |abstract class JUtil {
         |  def foo: java.util.concurrent.CompletableFuture[Int]
         |}
         |class Main extends JUtil {
         |  def foo: CompletableFuture[Int] = ${0:???}
         |}
         |""".stripMargin
    )

  @Test def `jutil-multiple-symbols` =
    checkEdit(
      """|abstract class JUtil {
         |  def foo(x: java.util.List[Int]): java.util.List[Int]
         |}
         |class Main extends JUtil {
         |  override def fo@@
         |}
         |""".stripMargin,
      """|import java.{util => ju}
         |abstract class JUtil {
         |  def foo(x: java.util.List[Int]): java.util.List[Int]
         |}
         |class Main extends JUtil {
         |  override def foo(x: ju.List[Int]): ju.List[Int] = ${0:???}
         |}
         |""".stripMargin
    )

  @Test def `jutil-conflict` =
    checkEdit(
      """|package jutil
         |abstract class JUtil {
         |  def foo: java.util.List[Int]
         |}
         |class Main extends JUtil {
         |  val java = 42
         |  def foo@@
         |}
         |""".stripMargin,
      // Ensure we don't insert `_root_` prefix for import because `val java = 42` is local.
      """|package jutil
         |
         |import java.{util => ju}
         |abstract class JUtil {
         |  def foo: java.util.List[Int]
         |}
         |class Main extends JUtil {
         |  val java = 42
         |  def foo: ju.List[Int] = ${0:???}
         |}
         |""".stripMargin
    )

  @Test def `jutil-conflict2` =
    checkEditLine(
      s"""|package jutil2
          |abstract class JUtil {
          |  def foo: java.util.List[Int]
          |}
          |class Main extends JUtil {
          |  val ju = 42
          |___
          |}
          |""".stripMargin,
      "  def foo@@",
      // Can't use `import java.{util => ju}` because `val ju = 42` is in scope.
      """  def foo: java.util.List[Int] = ${0:???}"""
    )

  @Test def `jlang` =
    checkEdit(
      """|abstract class Mutable {
         |  def foo: java.lang.StringBuilder
         |}
         |class Main extends Mutable {
         |  def foo@@
         |}
         |""".stripMargin,
      """|abstract class Mutable {
         |  def foo: java.lang.StringBuilder
         |}
         |class Main extends Mutable {
         |  def foo: java.lang.StringBuilder = ${0:???}
         |}
         |""".stripMargin
    )

  @Test def `alias` =
    checkEditLine(
      s"""|
          |abstract class Abstract {
          |  def foo: scala.collection.immutable.List[Int]
          |}
          |class Main extends Abstract {
          |  ___
          |}
          |
          |""".stripMargin,
      "  def foo@@",
      """  def foo: List[Int] = ${0:???}""".stripMargin
    )

  @Test def `alias2` =
    checkEditLine(
      s"""|package alias
          |abstract class Alias {
          |  type Foobar = List[Int]
          |  def foo: Foobar
          |}
          |class Main extends Alias {
          |  ___
          |}
          |
          |""".stripMargin,
      "  def foo@@",
      // NOTE(olafur) I am not sure this is desirable behavior, we might want to
      // consider not dealiasing here.
      """  def foo: Foobar = ${0:???}""".stripMargin
    )

  @Test def `rename` =
    checkEditLine(
      s"""|import java.lang.{Boolean => JBoolean}
          |abstract class Abstract {
          |  def foo: JBoolean
          |}
          |class Main extends Abstract {
          |___
          |}
          |
          |""".stripMargin,
      "  def foo@@",
      """  def foo: JBoolean = ${0:???}""".stripMargin
    )

  @Test def `path` =
    checkEditLine(
      s"""|package path
          |abstract class Path {
          |  type Out
          |  def foo: Out
          |}
          |class Main extends Path {
          |___
          |}
          |
          |""".stripMargin,
      "  def foo@@",
      """  def foo: Out = ${0:???}""".stripMargin
    )

  @Test def `path-alias` =
    checkEditLine(
      s"""|package paththis
          |abstract class Path {
          |  type Out
          |  def foo: Out
          |}
          |class Main extends Path {
          |  type Out = String
          |___
          |}
          |
          |""".stripMargin,
      "  def foo@@",
      """  def foo: Out = ${0:???}""".stripMargin
    )

  @Test def `path-this` =
    checkEditLine(
      """|package paththis
         |abstract class Path {
         |  type Out
         |}
         |class Main extends Path {
         |  trait Conflict {
         |    def conflict: Out
         |  }
         |  object Conflict extends Conflict {
         |    type Out = Int
         |___
         |  }
         |}
         |""".stripMargin,
      "    def conflict@@",
      """    def conflict: Main.this.Out = ${0:???}""".stripMargin
    )

  @Test def `final` =
    check(
      """|package f
         |abstract class Final {
         |  def hello1: Int = 42
         |  final def hello2: Int = 42
         |}
         |class Main extends Final {
         |  def hello@@
         |}
         |""".stripMargin,
      """override def hello1: Int
        |""".stripMargin,
      includeDetail = false
    )

  @Test def `default` =
    check(
      """|package g
         |abstract class Final {
         |  def hello1: Int
         |  final def hello2(hello2: Int = 42)
         |}
         |class Main extends Final {
         |  def hello@@
         |}
         |""".stripMargin,
      """def hello1: Int
        |""".stripMargin,
      includeDetail = false
    )

  @Test def `default2` =
    checkEditLine(
      """|package h
         |abstract class Final {
         |  def hello(arg: Int = 42): Unit
         |}
         |class Main extends Final {
         |  ___
         |}
         |""".stripMargin,
      "def hello@@",
      """def hello(arg: Int): Unit = ${0:???}""".stripMargin
    )

  @Test def `existential` =
    checkEditLine(
      """|package i
         |abstract class Exist {
         |  def exist: Set[_]
         |}
         |class Main extends Exist {
         |  ___
         |}
         |""".stripMargin,
      "def exist@@",
      """def exist: Set[?] = ${0:???}""".stripMargin
    )

  @Test def `cake` =
    checkEditLine(
      """|package i
         |trait Trees { this: Global =>
         |  case class Tree()
         |  def Apply(tree: Tree): Tree = ???
         |}
         |class Global extends Trees  {
         |  ___
         |}
         |""".stripMargin,
      "def Apply@@",
      """override def Apply(tree: Tree): Tree = ${0:???}""".stripMargin
    )

  @Test def `cake2` =
    checkEditLine(
      """|package i2
         |trait Trees { this: Global =>
         |  case class Tree()
         |  def Apply(tree: Tree): Tree = ???
         |}
         |class Global extends Trees  {
         |}
         |class MyGlobal extends Global  {
         |  ___
         |}
         |""".stripMargin,
      "def Apply@@",
      """override def Apply(tree: Tree): Tree = ${0:???}""".stripMargin
    )

  @Test def `cake-generic` =
    checkEditLine(
      """|package i
         |trait Trees[T] { this: Global =>
         |  case class Tree()
         |  def Apply(tree: T): Tree = ???
         |}
         |class Global extends Trees[Int] {
         |  ___
         |}
         |""".stripMargin,
      "def Apply@@",
      """override def Apply(tree: Int): Tree = ${0:???}""".stripMargin
    )

  @Test def `val-negative` =
    check(
      """|package j
         |abstract class Val {
         |  val hello1: Int = 42
         |  var hello2: Int = 42
         |}
         |class Main extends Val {
         |  def hello@@
         |}
         |""".stripMargin,
      ""
    )

  @Test def `val` =
    checkEditLine(
      """|package k
         |abstract class Val {
         |  val hello1: Int = 42
         |}
         |class Main extends Val {
         |  ___
         |}
         |""".stripMargin,
      "val hello@@",
      "override val hello1: Int = ${0:???}"
    )

  @Test def `var` =
    check(
      """|package l
         |abstract class Val {
         |  var hello1: Int = 42
         |}
         |class Main extends Val {
         |   override var hello@@
         |}
         |""".stripMargin,
      // NOTE(olafur) assert completion items are empty because it's not possible to
      // override concrete vars.
      ""
    )

  @Test def `val-var` =
    check(
      """|package m
         |abstract class Val {
         |  var hello1: Int = 42
         |}
         |class Main extends Val {
         |   override val hello@@
         |}
         |""".stripMargin,
      // NOTE(olafur) assert completion items are empty because it's not possible to
      // override concrete vars.
      ""
    )

  @Test def `abstract-var` =
    check(
      """|package m
         |abstract class Val {
         |  var hello1: Int
         |}
         |class Main extends Val {
         |  override var hello@@
         |}
         |""".stripMargin,
      "override var hello1: Int",
      includeDetail = false,
      filterText = "override var hello1: Int"
    )

  @Test def `abstract-var-val` =
    check(
      """|package m
         |abstract class Val {
         |  var hello1: Int
         |  val hello2: Int
         |  def hello3: Int
         |}
         |class Main extends Val {
         |  override val hello@@
         }
         |""".stripMargin,
      // NOTE(gabro): we inlcude also var and def completions despite the user typed a val
      """|override var hello1: Int
         |override val hello2: Int
         |override def hello3: Int
         |""".stripMargin,
      includeDetail = false
    )

  @Test def `abstract-var-def` =
    check(
      """|package m
         |abstract class Val {
         |  var hello1: Int
         |  val hello2: Int
         |  def hello3: Int
         |}
         |class Main extends Val {
         |  override def hello@@
         }
         |""".stripMargin,
      "override def hello3: Int"
    )

  @Test def `private` =
    check(
      """|package n
         |abstract class Val {
         |  private def hello: Int = 2
         |}
         |class Main extends Val {
         |   override val hello@@
         |}
         |""".stripMargin,
      ""
    )

  @Test def `protected` =
    check(
      """|package o
         |abstract class Val {
         |  protected def hello: Int = 2
         |}
         |class Main extends Val {
         |   override def hello@@
         |}
         |""".stripMargin,
      "override protected def hello: Int",
      includeDetail = false
    )

  @Test def `filter` =
    check(
      """|package p
         |abstract class Val {
         |  def hello: Int = 2
         |}
         |class Main extends Val {
         |   override def hel@@
         |}
         |""".stripMargin,
      "override def hello: Int",
      includeDetail = false,
      filterText = "override def hello: Int"
    )

  @Test def `lazy` =
    checkEditLine(
      """|package q
         |abstract class Val {
         |  lazy val hello: Int = 2
         |}
         |class Main extends Val {
         |   ___
         |}
         |""".stripMargin,
      "override val hel@@",
      "override lazy val hello: Int = ${0:???}"
    )

  @Test def `early-init` =
    checkEditLine(
      """|package r
         |abstract class Global {
         |  lazy val analyzer = new {
         |    val global: Global.this.type = Global.this
         |  }
         |}
         |class Main extends Global {
         |   ___
         |}
         |""".stripMargin,
      "val analyz@@",
      "override lazy val analyzer: Object = ${0:???}"
    )

  @Test def `val-trait` =
    checkEditLine(
      """|package s
         |trait Val {
         |  val hello1: Int = 42
         |}
         |class Main extends Val {
         |  ___
         |}
         |""".stripMargin,
      "val hello@@",
      "override val hello1: Int = ${0:???}"
    )

  @Test def `ident` =
    check(
      """|package t
         |abstract class Val {
         |  def hello: Int = 2
         |}
         |class Main extends Val {
         |   hello@@
         |}
         |""".stripMargin,
      """|hello: Int
         |override def hello: Int
         |""".stripMargin,
      includeDetail = false
    )

  @Test def `override-abstract` =
    check(
      """|package u
         |abstract class Val {
         |  def overTop: Int
         |}
         |class Main extends Val {
         |   over@@
         |}
         |""".stripMargin,
      /**
       * NOTE(tgodzik) observe that the "override" is not needed here but the completion
       * has "override" because the identifier name starts with "o". It's a known limitation
       * due to the feature that allows for easily auto-completing on writing o... .
       */
      """|override def overTop: Int
         |overTop: Int
         |""".stripMargin,
      includeDetail = false,
      topLines = Some(2)
    )

  @Test def `override-concrete` =
    check(
      """|package w
         |abstract class Val {
         |  def overTop: Int = 5
         |}
         |class Main extends Val {
         |   over@@
         |}
         |""".stripMargin,
      """|overTop: Int
         |override def overTop: Int
         |""".stripMargin,
      includeDetail = false,
      topLines = Some(2)
    )

  @Test def `override-word` =
    check(
      """|package y
         |abstract class Val {
         |  def hello1: Int = 2
         |  val hello2: Int = 2
         |}
         |class Main extends Val {
         |   overr@@
         |}
         |""".stripMargin,
      """|override def hello1: Int
         |override val hello2: Int
         |""".stripMargin,
      includeDetail = false,
      topLines = Some(2)
    )

  @Test def `def-word` =
    check(
      """|package z
         |abstract class Val {
         |  def hello1: Int
         |  val hello2: Int = 5
         |}
         |class Main extends Val {
         |   def@@
         |}
         |""".stripMargin,
      """|def hello1: Int
         |override def equals(x$0: Any): Boolean
         |override def hashCode(): Int
         |""".stripMargin,
      includeDetail = false,
      topLines = Some(3)
    )

  @Test def `path-dependent` =
    checkEdit(
      """|trait Over {
         |  object Outer {
         |    class Inner
         |  }
         |  def inner: Outer.Inner
         |}
         |class Under extends Over {
         |  def inner@@
         |}
         |""".stripMargin,
      """|trait Over {
         |  object Outer {
         |    class Inner
         |  }
         |  def inner: Outer.Inner
         |}
         |class Under extends Over {
         |  def inner: Outer.Inner = ${0:???}
         |}
         |""".stripMargin
    )

  @Test def `overriden-twice` =
    checkEdit(
      """
        |trait A {
        |  def close: Unit
        |}
        |trait B extends A{
        |  override def close : Unit = {}
        |}
        |class C extends B{
        |  close@@
        |}
      """.stripMargin,
      """|trait A {
         |  def close: Unit
         |}
         |trait B extends A{
         |  override def close : Unit = {}
         |}
         |class C extends B{
         |  override def close: Unit = ${0:???}
         |}
         |""".stripMargin,
      filter = (str) => str.contains("def")
    )

  @Test def `not-complete-ident` =
    checkEdit(
      """
        |trait A {
        |  def close: Unit
        |}
        |trait B extends A{
        |  override def close : Unit = {}
        |}
        |class C extends B{
        |  clos@@
        |}
        """.stripMargin,
      """|trait A {
         |  def close: Unit
         |}
         |trait B extends A{
         |  override def close : Unit = {}
         |}
         |class C extends B{
         |  override def close: Unit = ${0:???}
         |}
         |""".stripMargin,
      filter = (str) => str.contains("def")
    )

  @Test def `extension-override` =
    checkEdit(
      """|package a
         |
         |trait Base:
         |  extension (x: Int)
         |    def foo: Int
         |
         |class Concrete extends Base:
         |  over@@
         |""".stripMargin,
      """|package a
         |
         |trait Base:
         |  extension (x: Int)
         |    def foo: Int
         |
         |class Concrete extends Base:
         |  extension (x: Int) override def foo: Int = ${0:???}
         |""".stripMargin,
      filter = (str) => str.contains("foo")
    )

  @Test def `extension` =
    checkEdit(
      """|package a
         |
         |trait Base:
         |  extension (x: Int)
         |    def foo: Int
         |
         |class Concrete extends Base:
         |  def fo@@
         |""".stripMargin,
      """|package a
         |
         |trait Base:
         |  extension (x: Int)
         |    def foo: Int
         |
         |class Concrete extends Base:
         |  extension (x: Int) def foo: Int = ${0:???}
         |""".stripMargin
    )
