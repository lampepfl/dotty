package dotty.tools.pc.tests.completion

import scala.meta.pc.SymbolDocumentation
import scala.language.unsafeNulls

import dotty.tools.pc.base.BaseCompletionSuite
import dotty.tools.pc.utils.MockEntries

import org.junit.Test
import org.junit.Ignore

class CompletionSuite extends BaseCompletionSuite:

  override protected def mockEntries: MockEntries = new MockEntries:
    override def documentations: Set[SymbolDocumentation] = Set(
      MockDocumentation("java/lang/String#substring().", "substring", Seq(), Seq("beginIndex")),
      MockDocumentation("java/lang/String#substring(+1).", "substring", Seq(), Seq("beginIndex", "endIndex")),
      MockDocumentation("java/nio/file/Files#readAttributes().", "readAttributes", Seq("A"), Seq("path", "type", "options")),
      MockDocumentation("java/nio/file/Files#readAttributes(+1).", "readAttributes", Seq(), Seq("path", "attributes", "options"))
    )

  @Test def scope =
    check(
      """
        |object A {
        |  Lis@@
        |}""".stripMargin,
      """
        |List scala.collection.immutable
        |List[A](elems: A*): List[A]
        |List - java.awt
        |List - java.util
        |""".stripMargin,
      topLines = Some(4)
    )

  @Test def member =
    check(
      """
        |object A {
        |  List.emp@@
        |}""".stripMargin,
      """
        |empty[A]: List[A]
        |""".stripMargin
    )

  @Test def extension =
    check(
      """
        |object A {
        |  "".stripSu@@
        |}""".stripMargin,
      """
        |stripSuffix(suffix: String): String
        |""".stripMargin
    )

  @Test def tparam1 =
    check(
      """
        |class Foo[A] {
        |  def identity(a: A): A = a
        |}
        |object Foo {
        |  new Foo[Int].ident@@
        |}""".stripMargin,
      """|identity(a: Int): Int
         |""".stripMargin
    )

  @Test def tparam2 =
    check(
      """
        |object A {
        |  Map.empty[Int, String].getOrEl@@
        |}
        |""".stripMargin,
      """|getOrElse[V1 >: String](key: Int, default: => V1): V1
         |""".stripMargin
    )

  @Test def dot =
    check(
      """
        |object A {
        |  List.@@
        |}""".stripMargin,
      // ordinal(x$0: MirroredMonoType): Int completion is present because PC tests in dotty rely on bootstrapped stdlib,
      // thus mirrors were automatically genererated for List as they satisfy the requirements
      // https://docs.scala-lang.org/scala3/reference/contextual/derivation.html#
      """|empty[A]: List[A]
         |from[B](coll: IterableOnce[B]): List[B]
         |newBuilder[A]: Builder[A, List[A]]
         |apply[A](elems: A*): List[A]
         |concat[A](xss: Iterable[A]*): List[A]
         |fill[A](n1: Int, n2: Int)(elem: => A): List[List[A] @uncheckedVariance]
         |fill[A](n1: Int, n2: Int, n3: Int)(elem: => A): List[List[List[A]] @uncheckedVariance]
         |fill[A](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): List[List[List[List[A]]] @uncheckedVariance]
         |fill[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A): List[List[List[List[List[A]]]] @uncheckedVariance]
         |fill[A](n: Int)(elem: => A): List[A]
         |iterate[A](start: A, len: Int)(f: A => A): List[A]
         |range[A: Integral](start: A, end: A): List[A]
         |range[A: Integral](start: A, end: A, step: A): List[A]
         |tabulate[A](n1: Int, n2: Int)(f: (Int, Int) => A): List[List[A] @uncheckedVariance]
         |tabulate[A](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): List[List[List[A]] @uncheckedVariance]
         |tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A): List[List[List[List[A]]] @uncheckedVariance]
         |tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): List[List[List[List[List[A]]]] @uncheckedVariance]
         |tabulate[A](n: Int)(f: Int => A): List[A]
         |unapplySeq[A](x: List[A] @uncheckedVariance): UnapplySeqWrapper[A]
         |unfold[A, S](init: S)(f: S => Option[(A, S)]): List[A]
         |->[B](y: B): (List.type, B)
         |ensuring(cond: Boolean): List.type
         |ensuring(cond: List.type => Boolean): List.type
         |ensuring(cond: Boolean, msg: => Any): List.type
         |ensuring(cond: List.type => Boolean, msg: => Any): List.type
         |fromSpecific(from: Any)(it: IterableOnce[Nothing]): List[Nothing]
         |fromSpecific(it: IterableOnce[Nothing]): List[Nothing]
         |nn: List.type & List.type
         |toFactory(from: Any): Factory[Nothing, List[Nothing]]
         |formatted(fmtstr: String): String
         |→[B](y: B): (List.type, B)
         |iterableFactory[A]: Factory[A, List[A]]
         |asInstanceOf[X0]: X0
         |equals(x$0: Any): Boolean
         |getClass[X0 >: List.type](): Class[? <: X0]
         |hashCode(): Int
         |isInstanceOf[X0]: Boolean
         |synchronized[X0](x$0: X0): X0
         |toString(): String
         |wait(): Unit
         |wait(x$0: Long): Unit
         |wait(x$0: Long, x$1: Int): Unit
         |""".stripMargin
    )

  @Test def implicitClass =
    check(
      """
        |object A {
        |  implicit class XtensionMethod(a: Int) {
        |    def increment = a + 1
        |  }
        |  Xtension@@
        |}""".stripMargin,
      "XtensionMethod(a: Int): XtensionMethod"
    )

  @Ignore("This test should be handled by compiler fuzzy search")
  @Test def fuzzy =
    check(
      """
        |object A {
        |  def userService = 1
        |  uService@@
        |}""".stripMargin,
      """|userService: Int
         |""".stripMargin
    )

  @Ignore("This test should be handled by compiler fuzzy search")
  @Test def fuzzy1 =
    check(
      """
        |object A {
        |  new PBuilder@@
        |}""".stripMargin,
      """|ProcessBuilder - scala.sys.process
         |ProcessBuilderImpl - scala.sys.process
         |""".stripMargin,
      filter = _.contains("ProcessBuilder")
    )

  @Test def companion =
    check(
      """
        |import scala.collection.concurrent._
        |object A {
        |  TrieMap@@
        |}""".stripMargin,
      """|TrieMap scala.collection.concurrent
         |TrieMap[K, V](elems: (K, V)*): TrieMap[K, V]
         |new TrieMap[K, V]: TrieMap[K, V]
         |new TrieMap[K, V](hashf: Hashing[K], ef: Equiv[K]): TrieMap[K, V]
         |""".stripMargin
    )

  @Test def `no-companion-apply-in-new`=
    check(
      """
        |import scala.collection.concurrent._
        |object A {
        |  new TrieMap@@
        |}""".stripMargin,
      // TrieMap should be filtered if it doesn't contain any types that can be constructed in `new` keyword context.
      """|TrieMap[K, V]: TrieMap[K, V]
         |TrieMap[K, V](hashf: Hashing[K], ef: Equiv[K]): TrieMap[K, V]
         |TrieMap scala.collection.concurrent
         |""".stripMargin
    )

  @Test def pkg =
    check(
      """
        |import scala.collection.conc@@
        |""".stripMargin,
      """|concurrent scala.collection
         |""".stripMargin
    )

  @Test def `import-star-basic` =
    check(
      """
        |import scala.collection.immutable.List.*@@
        |""".stripMargin,
      """|*
         |""".stripMargin
    )

  @Test def `import-star-multi-import` =
    check(
      """
        |import scala.collection.immutable.List.{range => r, *@@}
        |""".stripMargin,
      """|*
         |""".stripMargin
    )

  @Test def `import` =
    check(
      """
        |import JavaCon@@
        |""".stripMargin,
      """|JavaConverters - scala.collection
         |JavaConversions - scala.concurrent
         |AsJavaConsumer - scala.jdk.FunctionWrappers
         |AsJavaConverters - scala.collection.convert
         |FromJavaConsumer - scala.jdk.FunctionWrappers
         |AsJavaBiConsumer - scala.jdk.FunctionWrappers
         |AsJavaIntConsumer - scala.jdk.FunctionWrappers
         |""".stripMargin
    )

  @Test def import1 =
    check(
      """
        |import Paths@@
        |""".stripMargin,
      """|Paths - java.nio.file
         |""".stripMargin
    )

  @Test def import2 =
    check(
      """
        |import Catch@@
        |""".stripMargin,
      """|Catch - scala.util.control.Exception
         |""".stripMargin
    )

  @Test def import3 =
    check(
      """
        |import Path@@
        |""".stripMargin,
      """|Path - java.nio.file
         |Paths - java.nio.file
         |PathMatcher - java.nio.file
         |""".stripMargin,
      filter = _.contains("java.nio.file")
    )

  @Test def import4 =
    check(
      """
        |import scala.collection.AbstractIterator@@
        |""".stripMargin,
      """|AbstractIterator scala.collection
         |""".stripMargin
    )

  @Test def accessible =
    check(
      """
        |package a
        |import MetaData@@
        |""".stripMargin,
      """|RowSetMetaData - javax.sql
         |DatabaseMetaData - java.sql
         |ParameterMetaData - java.sql
         |ResultSetMetaData - java.sql
         |RowSetMetaDataImpl - javax.sql.rowset
         |""".stripMargin
    )

  @Test def source =
    check(
      """
        |package a
        |object Main {
        |  import Inner@@
        |}
        |object Outer {
        |  class Inner
        |}
        |""".stripMargin,
      """|Inner - a.Outer
         |""".stripMargin
    )

  @Test def duplicate =
    check(
      """
        |package a
        |object Main {
        |  import a.Outer.Inner
        |  import Inner@@
        |}
        |object Outer {
        |  class Inner
        |}
        |""".stripMargin,
      "Inner a.Outer"
    )

  @Test def duplicate2 =
    check(
      """
        |package a
        |import java.nio.file.Files
        |
        |final class AbsolutePath private (val underlying: String) extends AnyVal {
        |  def syntax: String = Files@@
        |}
        |
        |object Outer {
        |  object Files
        |}
        |""".stripMargin,
      """Files java.nio.file
        |Files - a.Outer
        |""".stripMargin
    )

  @Test def commit =
    check(
      """
        |package a
        |
        |object Main{
        |  Map.emp@@
        |}
        |""".stripMargin,
      "empty[K, V]: Map[K, V] (commit: '')", // space between K V
      includeCommitCharacter = true
    )

  @Test def commit1 =
    check(
      """
        |package a
        |
        |object Main{
        |  identity@@
        |}
        |""".stripMargin,
      """|identity[A](x: A): A (commit: '')
         |""".stripMargin,
      includeCommitCharacter = true
    )

  @Test def `numeric-sort` =
    check(
      """
        |package a
        |
        |object Main{
        |  import scala.Function@@
        |}
        |""".stripMargin,
      // assert that we don't sort lexicographically: Function1, Function11, ..., Function2, ...
      """|Function scala
         |Function0 scala
         |Function1 scala
         |Function2 scala
         |Function3 scala
         |Function4 scala
         |Function5 scala
         |Function6 scala
         |Function7 scala
         |Function8 scala
         |Function9 scala
         |Function10 scala
         |Function11 scala
         |Function12 scala
         |Function13 scala
         |Function14 scala
         |Function15 scala
         |Function16 scala
         |Function17 scala
         |Function18 scala
         |Function19 scala
         |Function20 scala
         |Function21 scala
         |Function22 scala
         |""".stripMargin,
      topLines = Some(25)
    )

  @Test def sam =
    check(
      """
        |object A {
        |  new java.util.ArrayList[String]().forEach(p => p.toChar@@)
        |}
      """.stripMargin,
      """|toCharArray(): Array[Char]
         |""".stripMargin
    )

  @Test def `implicit` =
    check(
      """
        |object A {
        |  Array.concat@@
        |}
      """.stripMargin,
      """|concat[T: ClassTag](xss: Array[T]*): Array[T]
         |""".stripMargin
    )

  @Test def `implicit-evidence-many` =
    check(
      """
        |object A {
        |  object B {
        |    def test[T: Ordering: Numeric](x: T): T = ???
        |  }
        |  B.tes@@
        |}
      """.stripMargin,
      """|test[T: Ordering: Numeric](x: T): T
         |""".stripMargin
    )

  @Test def bounds =
    check(
      """
        |object A {
        |  java.nio.file.Files.readAttributes@@
        |}
      """.stripMargin,
      """|readAttributes(path: Path, attributes: String, options: LinkOption*): java.util.Map[String, Object]
         |readAttributes[A <: BasicFileAttributes](path: Path, type: Class[A], options: LinkOption*): A
         |""".stripMargin
    )

  @Test def local =
    check(
      """
        |object A {
        |  locally {
        |    val thisIsLocal = 1
        |    thisIsLoc@@
        |  }
        |}
      """.stripMargin,
      """|thisIsLocal: Int
         |""".stripMargin
    )

  @Test def local1 =
    check(
      """
        |import scala.concurrent.DelayedLazyVal
        |
        |object Main {
        |
        |  List(1).map { client =>
        |    val x = 2
        |    DelayedLazyVal@@
        |    val y = 1
        |  }
        |
        |}
      """.stripMargin,
      "DelayedLazyVal[T](f: () => T, body: => Unit)(implicit exec: ExecutionContext): DelayedLazyVal[T]"
    )

  @Test def local2 =
    check(
      """
        |object Main {
        |  def foo(): Unit = {
        |    val prefixaa = 1
        |    locally {
        |      val prefixbb = 2
        |      println(prefix@@)
        |      val prefixcc = 3
        |    }
        |    val prefixyy = 4
        |  }
        |}
      """.stripMargin,
      """|prefixbb: Int
         |prefixaa: Int
         |""".stripMargin
    )

  @Test def singleton =
    check(
      """
        |class A {
        |  def incrementThisType(): this.type = x
        |  incrementThisType@@
        |}
      """.stripMargin,
      "incrementThisType(): A.this.type"
    )

  @Test def deprecated =
    check(
      """
        |class A {
        |  1.until@@
        |}
      """.stripMargin,
      """|until(end: Int): Range
         |until(end: Int, step: Int): Range
         |until(end: Long): Exclusive[Long]
         |until(end: Long, step: Long): Exclusive[Long]
         |""".stripMargin,
      postProcessObtained = _.replace("Float", "Double"),
      stableOrder = false
    )

  def classFoo: String =
    """
      |import scala.language.dynamics
      |class Foo extends Dynamic {
      |  def banana: Int = 42
      |  def selectDynamic(field: String): Foo = this
      |  def applyDynamicNamed(name: String)(arg: (String, Int)): Foo = this
      |  def updateDynamic(name: String)(value: Int): Foo = this
      |}
      |""".stripMargin

  @Test def dynamic =
    check(
      s"""|$classFoo
          |object Main {
          |  new Foo().bana@@
          |}
          |""".stripMargin,
      "banana: Int"
    )

  @Test def dynamic2 =
    check(
      s"""|$classFoo
          |object Main {
          |  val x = new Foo().foo.bana@@
          |}
          |""".stripMargin,
      "banana: Int"
    )

  @Test def dynamic3 =
    check(
      s"""|$classFoo
          |object Main {
          |  val foo = new Foo()
          |  (foo.bar = 42).bana@@
          |}
          |""".stripMargin,
      "banana: Int"
    )

  @Test def dynamic4 =
    check(
      s"""|$classFoo
          |object Main {
          |  val foo = new Foo().foo(x = 42).bana@@
          |}
          |""".stripMargin,
      "banana: Int"
    )

  @Test def dynamic5 =
    check(
      s"""|$classFoo
          |object Main {
          |  val foo = new Foo()
          |  List(foo.selectDy@@)
          |}
          |""".stripMargin,
      """|selectDynamic(field: String): Foo
         |""".stripMargin
    )

  @Test def `type` =
    check(
      s"""|object Main {
          |  val foo: ListBuffe@@
          |}
          |""".stripMargin,
      """|ListBuffer[A] - scala.collection.mutable
         |ListBuffer - scala.collection.mutable
         |""".stripMargin
    )

  @Test def type1 =
    check(
      s"""|object Main {
          |  val foo: Map[Int, ListBuffe@@]
          |}
          |""".stripMargin,
      """|ListBuffer[A] - scala.collection.mutable
         |ListBuffer - scala.collection.mutable
         |""".stripMargin
    )

  @Test def pat =
    check(
      s"""|object Main {
          |  Option(1) match {
          |    case Som@@
          |}
          |""".stripMargin,
      """|Some(value) scala
         |Some scala
         |Some[A](value: A): Some[A]
         |""".stripMargin
    )

  @Test def pat1 =
    check(
      s"""|object Main {
          |  Option(1) match {
          |    case List(Som@@)
          |}
          |""".stripMargin,
      """|Some scala
         |Some[A](value: A): Some[A]
         |""".stripMargin
    )

  @Test def adt =
    check(
      s"""|object Main {
          |  Option(1) match {
          |    case No@@
          |}
          |""".stripMargin,
      """|None scala
         |NoManifest scala.reflect
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def adt1 =
    check(
      s"""|object Main {
          |  Option(1) match {
          |    case S@@
          |}
          |""".stripMargin,
      """|Some(value) scala
         |Seq scala.collection.immutable
         |Set scala.collection.immutable
         |""".stripMargin,
      topLines = Some(3)
    )

  @Test def adt2 =
    check(
      s"""|object Main {
          |  Option(1) match {
          |    case _: Som@@
          |}
          |""".stripMargin,
      """|Some[?] scala
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def adt3 =
    check(
      s"""|import Matches._
          |object Matches {
          |  val Number = "".r
          |}
          |object Main {
          |  locally {
          |    val NotString = 42
          |    "" match {
          |      case N@@
          |  }
          |}
          |""".stripMargin,
      """|NotString: Int
         |Number: Regex
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def adt4 =
    check(
      s"""|object Main {
          |  val Number = "".r
          |  "" match {
          |    case _: Numb@@
          |}
          |""".stripMargin,
      """|Number: Regex
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `no-methods-on-case-type` =
    check(
      s"""|object Main {
          |  val Number = "".r
          |  "" match {
          |    case _: NotImplementedErr@@
          |}
          |""".stripMargin,
      """|NotImplementedError scala
         |""".stripMargin,
    )

  @Test def underscore =
    check(
      s"""|object Main {
          |  List(1).exists(_@@)
          |}
          |""".stripMargin,
      // assert that `_root_` is not a completion item.
      ""
    )

  @Test def filterText =
    check(
      s"""|object Main {
          |  "".substring@@
          |}
          |""".stripMargin,
      """substring(beginIndex: Int): String
        |substring(beginIndex: Int, endIndex: Int): String
        |""".stripMargin,
      filterText = "substring"
    )

  @Test def error =
    check(
      s"""|object Main {
          |  def foo(myError: String): String = {
          |    println(myErr@@)
          |    // type mismatch: obtained Unit, expected String
          |  }
          |}
          |""".stripMargin,
      "myError: String"
    )

  @Test def sort =
    check(
      s"""|object Main {
          |  def printnnn = ""
          |  def printmmm = ""
          |  locally {
          |    val printxxx = ""
          |    print@@
          |  }
          |}
          |""".stripMargin,
      """|printxxx: String
         |printmmm: String
         |printnnn: String
         |print(x: Any): Unit
         |""".stripMargin,
      topLines = Some(4)
    )

  @Test def `fuzzy-member-sort` =
    check(
      s"""|class Foo {
          |  def toInt: Int = 0
          |  def instance: Int = 42
          |  def intNumber: Int = 42
          |}
          |object Main {
          |  new Foo().int@@
          |}
          |""".stripMargin,
      """|intNumber: Int
         |""".stripMargin
    )

  @Test def `fields-first` =
    check(
      s"""|class Foo {
          |  def yeti1: Int = 0
          |  def yeti2: Int = 42
          |  val yeti3 = ""
          |}
          |object Main {
          |  new Foo().ye@@
          |}
          |""".stripMargin,
      """|yeti3: String
         |yeti1: Int
         |yeti2: Int
         |""".stripMargin,
      topLines = Some(3)
    )

  @Test def `using` =
    check(
      s"""|class Foo {
          |  def max[T](x: T, y: T)(using ord: Ordered[T]): T =
          |    if ord.compare(x, y) < 0 then y else x
          |}
          |object Main {
          |  new Foo().max@@
          |}
          |""".stripMargin,
      """|max[T](x: T, y: T)(using ord: Ordered[T]): T
         |""".stripMargin
    )

  @Test def `annoncontext` =
    check(
      s"""|class Foo {
          |  def max[T](x: T, y: T)(using Ordered[T]): T = ???
          |}
          |object Main {
          |  new Foo().max@@
          |}
          |""".stripMargin,
      """|max[T](x: T, y: T)(using Ordered[T]): T
         |""".stripMargin
    )

  @Test def ordering1 =
    check(
      s"""|object Main {
          |  languageFeature.@@
          |}
          |""".stripMargin,
      """|dynamics scala.languageFeature
         |existentials scala.languageFeature
         |experimental scala.languageFeature
         |implicitConversions scala.languageFeature
         |postfixOps scala.languageFeature
         |""".stripMargin,
      topLines = Some(5)
    )

  @Test def ordering2 =
    check(
      s"""|object Main {
          |  1.@@
          |}
          |""".stripMargin,
      """|!=(x: Byte): Boolean
         |!=(x: Char): Boolean
         |!=(x: Double): Boolean
         |!=(x: Float): Boolean
         |!=(x: Int): Boolean
         |!=(x: Long): Boolean
         |!=(x: Short): Boolean
         |%(x: Byte): Int
         |%(x: Char): Int
         |%(x: Double): Double
         |""".stripMargin,
      topLines = Some(10)
    )

  @Test def ordering3 =
    check(
      s"""|class A {
          |  def fooA: String = ""
          |}
          |
          |class B extends A {
          |  def fooB: String = ""
          |}
          |
          |object Main {
          |    val x = new B()
          |    x.foo@@
          |}
          |""".stripMargin,
      """|fooB: String
         |fooA: String
         |""".stripMargin,
      topLines = Some(2)
    )

  // issues with scala 3 https://github.com/scala/scala3/pull/13515
  @Test def ordering4 =
    check(
      s"""|class Main {
          |  def main(fooC: Int): Unit = {
          |    val fooA = 1
          |    val fooB = 2
          |    println(foo@@)
          |  }
          |  def foo: String = ""
          |}
          |""".stripMargin,
      """|fooB: Int
         |fooA: Int
         |fooC: Int
         |foo: String
         |""".stripMargin,
      topLines = Some(4)
    )

  @Test def newlinedot =
    checkEdit(
      """|object O {
         |  val a = List(1, 2)
         |    .m@@
         |}""".stripMargin,
      """|object O {
         |  val a = List(1, 2)
         |    .map($0)
         |}""".stripMargin,
      filter = _.contains("map["),
      assertSingleItem = false
    )

  @Test def `dot-error-tree-edit` =
    checkEdit(
      """
        |case class A(x: Int) {
        |  "".@@
        |  def foo: Int = {
        |    val a = 42
        |     a
        |  }
        |}""".stripMargin,
      """
        |case class A(x: Int) {
        |  "".toInt
        |  def foo: Int = {
        |    val a = 42
        |     a
        |  }
        |}""".stripMargin,
      filter = _.startsWith("toInt:")
    )

  @Test def `select-ignores-next-line` =
    checkItems(
      s"""
         |object Main {
         |  def hello = {
         |    val name = Option("Bob")
         |    name.@@
         |    println(msg)
         |  }
         |}
         |""".stripMargin,
      _.nonEmpty
    )

  @Test def `empty-template-braces` =
    check(
      s"""|package x
          |object Foo {
          |  def bar: Int = 42
          |  @@
          |}""".stripMargin,
      """|Foo x
         |bar: Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `empty-template-optional-braces1` =
    check(
      s"""|package x
          |object Foo:
          |  def bar: Int = 42
          |  @@
          |""".stripMargin,
      """|Foo x
         |bar: Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `emptyline-optional-braces2` =
    check(
      s"""|package x
          |object Foo:
          |  def bar: Int = 42
          |  def baz: Int =
          |    val x = 1
          |    val y = 2
          |    @@
          |""".stripMargin,
      """|y: Int
         |x: Int
         |Foo x
         |bar: Int
         |""".stripMargin,
      topLines = Some(4)
    )

  @Test def `emptyline-optional-braces3` =
    check(
      s"""|package x
          |object Foo:
          |  def bar: Int = 42
          |  def baz: Int =
          |    val x = 1
          |    val y = 2
          |  @@
          |""".stripMargin,
      """|Foo x
         |bar: Int
         |baz: Int
         |""".stripMargin,
      topLines = Some(3)
    )

  @Test def `tab-indented` =
    checkEdit(
      s"""|package x
          |object Foo {
          |    def bar: Int =
          |            42.@@
          |}
          |""".stripMargin,
      s"""|package x
          |object Foo {
          |    def bar: Int =
          |            42.toInt
          |}
          |""".stripMargin,
      filter = _.startsWith("toInt")
    )

  @Test def nocompletions =
    check(
      s"""|package nocomp
          |object Foo {
          |  errored.@@
          |}
          |""".stripMargin,
      ""
    )

  @Test def pkg2 =
    check(
      s"""|object Foo {
          |  scala.coll@@
          |}
          |""".stripMargin,
      "collection scala"
    )

  @Test def pkgtyped =
    check(
      s"""|object Foo {
          |  val a : scala.coll@@
          |}
          |""".stripMargin,
      "collection scala"
    )

  @Test def pkgnew =
    check(
      s"""|object Foo {
          |  new scala.coll@@
          |}
          |""".stripMargin,
      "collection scala"
    )

  @Test def pkgscala =
    check(
      s"""|object Foo {
          |  scala@@
          |}
          |""".stripMargin,
      """|scala <root>
         |""".stripMargin
    )

  @Test def `class-members-trait-issue` =
    check(
      s"""|package x
          |class Foo(
          |  first: java.util.List[Int],
          |  second: String,
          |) {
          |  fir@@
          |  def abc: Int = 23
          |}
          |""".stripMargin,
      """|first: java.util.List[Int]
         |""".stripMargin
    )

  @Test def `object-at-type-pos` =
    check(
      s"""|object Foo {
          |  class FFF
          |}
          |object Main {
          |  def f1(a: Fo@@)
          |}
          |""".stripMargin,
      """|Foo test
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `wildcard-param` =
    check(
      """
        |object A {
        |  List(1, 3, 4).map { _ =>
        |    @@
        |  }
        |}""".stripMargin,
      "",
      filter = _.startsWith("_")
    )

  @Test def currentTest =
    check(
      """
        |class Test ext@@""".stripMargin,
      "extends"
    )

  @Test def `issue-3625` =
    check(
      """|package a
         |
         |object Test:
         |  case class Widget(name: String, other: Int)
         |  val otxxx: Int = 1
         |  Widget(name = "foo", @@
         |""".stripMargin,
      """|other = : Int
         |other = otxxx : Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `constructor-empty` =
    check(
      """|package a
         |
         |class Testing
         |
         |def main =
         |  Testin@@
         |""".stripMargin,
      """|Testing(): Testing
         |""".stripMargin
    )

  @Test def `constructor-params` =
    check(
      """|package a
         |
         |class Testing(a: Int, b: String)
         |
         |def main =
         |  Testin@@
         |""".stripMargin,
      """|Testing(a: Int, b: String): Testing
         |""".stripMargin
    )

  // https://github.com/scalameta/metals/issues/2810
  @Test def `higher-kinded-match-type` =
    check(
      """|package a
         |
         |trait Foo[A] {
         |  def map[B](f: A => B): Foo[B] = ???
         |}
         |case class Bar[F[_]](bar: F[Int])
         |type M[T] = T match {
         |  case Int => Foo[Int]
         |}
         |object Test:
         |  val x = Bar[M](new Foo[Int]{})
         |  x.bar.m@@
         |""".stripMargin,
      """|map[B](f: Int => B): Foo[B]
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `issue-3421` =
    check(
      """|object Main{
         |  def thing() = thi@@
         |  def run() = {
         |    val thing = ???
         |  }
         |}
         |""".stripMargin,
      "thing(): Any",
      topLines = Some(1)
    )

  @Test def `issue-3421-match` =
    check(
      """|
         |trait Dependency
         |object InvalidDependency extends Dependency
         |object Main{
         |  def exists(dep: Dependency) = {
         |    deps match {
         |      case invalid @ InvalidDependency => println(inv@@)
         |    }
         |  }
         |}
         |""".stripMargin,
      "invalid: Any",
      topLines = Some(1)
    )

  @Test def `trait-member` =
    checkEdit(
      """|trait Foo:
         |  def foo: String
         |
         |object Bar extends Foo:
         |  def@@
         |""".stripMargin,
      """|trait Foo:
         |  def foo: String
         |
         |object Bar extends Foo:
         |  def foo: String = ${0:???}
         |""".stripMargin,
      assertSingleItem = false,
    )

  @Test def `type-with-params` =
    check(
      s"""|object O {
          | type TTT[A <: Int] = List[A]
          | val t: TT@@
          |}
          |""".stripMargin,
      "TTT[A <: Int]",
      includeDetail = false,
    )

  @Test def `type-with-params-with-detail` =
    check(
      s"""|object O {
          | type TTT[A <: Int] = List[A]
          | val t: TT@@
          |}
          |""".stripMargin,
    "TTT[A <: Int] = List[A]"
    )

  @Test def `type-lambda` =
    check(
      s"""|object O {
          | type TTT = [A <: Int] =>> List[A]
          | val t: TT@@
          |}
          |""".stripMargin,
      "TTT[A <: Int]",
      includeDetail = false,
    )

  @Test def `type-lambda2` =
    check(
      s"""|object O {
          | type TTT[K <: Int] = [V] =>> Map[K, V]
          | val t: TT@@
          |}
          |""".stripMargin,
      "TTT[K <: Int]",
      includeDetail = false,
    )

  @Test def `type-lambda2-with-detail` =
    check(
      s"""|object O {
          | type TTT[K <: Int] = [V] =>> Map[K, V]
          | val t: TT@@
          |}
          |""".stripMargin,
      "TTT[K <: Int] = [V] =>> Map[K, V]",
    )

  @Test def `type-bound` =
    check(
      s"""|trait O {
          | type TTT <: Int
          | val t: TT@@
          |}
          |""".stripMargin,
      "TTT <: Int"
    )

  @Test def `class-with-params` =
    check(
      s"""|object O {
          | class AClass[A <: Int]
          | object AClass
          | val v: ACla@@
          |}
          |""".stripMargin,
      """|AClass[A <: Int] test.O
         |AClass test.O
         """.stripMargin
    )

  val extensionResult =
    """|Foo test
       |Found - scala.collection.Searching
       |Font - java.awt
       |Form - java.text.Normalizer
       |Format - java.text
       |FontPeer - java.awt.peer
       |FormView - javax.swing.text.html
       |Formatter - java.util
       |Formatter - java.util.logging
       |FocusEvent - java.awt.event""".stripMargin

  @Test def `extension-definition-scope` =
    check(
      """|trait Foo
         |object T:
         |  extension (x: Fo@@)
         |""".stripMargin,
      extensionResult
    )

  @Test def `extension-definition-symbol-search` =
    check(
      """|object T:
         |  extension (x: ListBuffe@@)
         |""".stripMargin,
      """|ListBuffer[A] - scala.collection.mutable
         |ListBuffer - scala.collection.mutable
         |""".stripMargin,
    )

  @Test def `extension-definition-type-parameter` =
    check(
      """|trait Foo
         |object T:
         |  extension [A <: Fo@@]
         |""".stripMargin,
      extensionResult
    )

  @Test def `extension-definition-type-parameter-symbol-search` =
    check(
      """|object T:
         |  extension [A <: ListBuffe@@]
         |""".stripMargin,
      """|ListBuffer[A] - scala.collection.mutable
         |ListBuffer - scala.collection.mutable
         |""".stripMargin
    )

  @Test def `extension-definition-using-param-clause` =
    check(
      """|trait Foo
         |object T:
         |  extension (using Fo@@)
         |""".stripMargin,
      extensionResult
    )


  @Test def `extension-definition-mix-1` =
    check(
      """|trait Foo
         |object T:
         |  extension (x: Int)(using Fo@@)
         |""".stripMargin,
      extensionResult
    )

  @Test def `extension-definition-mix-2` =
    check(
      """|trait Foo
         |object T:
         |  extension (using Fo@@)(x: Int)(using Foo)
         |""".stripMargin,
      extensionResult
    )

  @Test def `extension-definition-mix-3` =
    check(
      """|trait Foo
         |object T:
         |  extension (using Foo)(x: Int)(using Fo@@)
         |""".stripMargin,
      extensionResult
    )

  @Test def `extension-definition-mix-4` =
    check(
      """|trait Foo
         |object T:
         |  extension [A](x: Fo@@)
         |""".stripMargin,
      extensionResult
    )

  @Test def `extension-definition-mix-5` =
    check(
      """|trait Foo
         |object T:
         |  extension [A](using Fo@@)(x: Int)
         |""".stripMargin,
      extensionResult
    )

  @Test def `extension-definition-mix-6` =
    check(
      """|trait Foo
         |object T:
         |  extension [A](using Foo)(x: Fo@@)
         |""".stripMargin,
      extensionResult
    )

  @Test def `extension-definition-mix-7` =
    check(
      """|trait Foo
         |object T:
         |  extension [A](using Foo)(x: Fo@@)(using Foo)
         |""".stripMargin,
      extensionResult
    )

  @Test def `extension-definition-select` =
    check(
      """|object Test:
         |  class TestSelect()
         |object T:
         |  extension (x: Test.TestSel@@)
         |""".stripMargin,
      """|TestSelect test.Test
         |""".stripMargin
    )

  @Test def `extension-definition-select-mix-1` =
    check(
      """|object Test:
         |  class TestSelect()
         |object T:
         |  extension (using Int)(x: Test.TestSel@@)
         |""".stripMargin,
      """|TestSelect test.Test
         |""".stripMargin
    )

  @Test def `extension-definition-select-mix-2` =
    check(
      """|object Test:
         |  class TestSelect[T]()
         |object T:
         |  extension [T](x: Test.TestSel@@)
         |""".stripMargin,
      """|TestSelect[T] test.Test
         |""".stripMargin
    )

  @Test def `no-square-brackets` =
    checkEdit(
      """|object O:
         |  val a = List.appl@@
         |""".stripMargin,
      """|object O:
         |  val a = List.apply($0)
         |""".stripMargin,
    )

  @Test def `multiline-comment` =
   checkEdit(
     """|package a
        |object O:
        |  /*@@
        |  def f = 1
        |""".stripMargin,
     """|package a
        |object O:
        |  /* $0 */
        |  def f = 1
        |""".stripMargin,
   )

  @Test def `prepend-instead-of-replace` =
    checkEdit(
      """|object O:
         |  printl@@println()
         |""".stripMargin,
      """|object O:
         |  printlnprintln()
         |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `prepend-duplicate-word` =
    checkEdit(
      """|object O:
         |  println@@println()
         |""".stripMargin,
      """|object O:
         |  printlnprintln()
         |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `replace-when-inside` =
    checkEdit(
      """|object O:
         |  pri@@nt()
         |""".stripMargin,
      """|object O:
         |  print()
         |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `replace-exact-same` =
    checkEdit(
      """|object O:
         |  println@@()
         |""".stripMargin,
      """|object O:
         |  println()
         |""".stripMargin,
      assertSingleItem = false
    )


  @Test def `multi-export` =
    check(
      """export scala.collection.{AbstractMap, Set@@}
        |""".stripMargin,
      """Set scala.collection
        |SetOps scala.collection
        |""".stripMargin
    )

  @Test def `multi-imports` =
    check(
      """import scala.collection.{AbstractMap, Set@@}
        |""".stripMargin,
      """Set scala.collection
        |SetOps scala.collection
        |""".stripMargin,
    )


  @Test def `multi-imports-empty-query` =
    check(
      """import scala.collection.{AbstractMap, @@}
        |""".stripMargin,
      """+: scala.collection
        |:+ scala.collection
        |AbstractIndexedSeqView scala.collection
        |AbstractIterable scala.collection
        |AbstractIterator scala.collection
        |""".stripMargin,
      topLines = Some(5)
    )

  @Test def `import-rename` =
    check(
      """import scala.collection.{AbstractMap => Set@@}
        |""".stripMargin,
      ""
    )

  @Ignore
  @Test def `dont-crash-implicit-search` =
    check(
      """object M:
        |  Array[Int].fi@@
        |""".stripMargin,
      ""
    )

  @Test def `extension-definition-type-variable-inference` =
    check(
      """|object M:
         |  extension [T](xs: List[T]) def test(p: T => Boolean): List[T] = ???
         |  List(1,2,3).tes@@
         |""".stripMargin,
      """|test(p: Int => Boolean): List[Int]
         |""".stripMargin
    )

  @Test def `old-style-extension-type-variable-inference` =
    check(
      """|object M:
         |  implicit class ListUtils[T](xs: List[T]) {
         |    def test(p: T => Boolean): List[T] = ???
         |  }
         |  List(1,2,3).tes@@
         |""".stripMargin,
      """|test(p: Int => Boolean): List[Int]
         |""".stripMargin
    )

  @Test def `instantiate-type-vars-in-extra-apply-completions` =
    check(
      """|object M:
         |  val fooBar = List(123)
         |  foo@@
         |""".stripMargin,
      """|fooBar: List[Int]
         |fooBar(n: Int): Int
         |""".stripMargin
    )

  @Test def `show-underlying-type-instead-of-CC` =
    check(
      """|object M:
         |  List@@
         |""".stripMargin,
      """|List[A](elems: A*): List[A]
         |ListMap[K, V](elems: (K, V)*): ListMap[K, V] - scala.collection.immutable
         |new ListMap[K, V]: ListMap[K, V] - scala.collection.immutable
         |new ListSet[A]: ListSet[A] - scala.collection.immutable
         |new ListMap[K, V]: ListMap[K, V] - scala.collection.mutable
         |""".stripMargin,
      filter = _.contains("[")
    )

  @Test def `empty-import` =
    check(
      """|import @@
         |""".stripMargin,
      """|java <root>
         |javax <root>
         |""".stripMargin,
      filter = _.startsWith("java")
    )

  @Test def `empty-import-selector` =
    check(
      """|import java.@@
         |""".stripMargin,
      """|util java
         |""".stripMargin,
      filter = _.startsWith("util")
    )

  @Test def `empty-export` =
    check(
      """|export @@
         |""".stripMargin,
      """|java <root>
         |javax <root>
         |""".stripMargin,
      filter = _.startsWith("java")
    )

  @Test def `empty-export-selector` =
    check(
      """|export java.@@
         |""".stripMargin,
      """|util java
         |""".stripMargin,
      filter = _.startsWith("util")
    )

  @Test def `annotation` =
    check(
      """|@Over@@
         |object M {}
         |""".stripMargin,
      """|Override java.lang
         |""".stripMargin,
      filter = _ == "Override java.lang"
    )

  @Test def `no-annotation` =
    check(
      """|
         |object M {
         |  Overr@@
         |}
         |""".stripMargin,
      """|Override java.lang
         |""".stripMargin,
      filter = _ == "Override java.lang"
    )

  @Test def `no-annotation-param-first-pos` =
    check(
      """|
         |object M {
         |  def hello(Overr@@)
         |}
         |""".stripMargin,
      ""
    )

  @Test def `no-annotation-param-second-pos` =
    check(
      """|
         |object M {
         |  def hello(x: Int, Overr@@)
         |}
         |""".stripMargin,
      ""
    )

  @Test def `no-annotation-param-second-list` =
    check(
      """|
         |object M {
         |  def hello(x: Int)(Overr@@)
         |}
         |""".stripMargin,
      ""
    )


  @Test def `annotation-param-first-pos` =
    check(
      """|
         |object M {
         |  def hello(@Overr@@)
         |}
         |""".stripMargin,
      """|Override java.lang
         |""".stripMargin,
      filter = _ == "Override java.lang"
    )

  @Test def `annotation-param-second-pos` =
    check(
      """|
         |object M {
         |  def hello(x: Int, @Overr@@)
         |}
         |""".stripMargin,
      """|Override java.lang
         |""".stripMargin,
      filter = _ == "Override java.lang"
    )

  @Test def `annotation-param-second-list` =
    check(
      """|
         |object M {
         |  def hello(x: Int)( @Overr@@)
         |}
         |""".stripMargin,
      """|Override java.lang
         |""".stripMargin,
      filter = _ == "Override java.lang"
    )

