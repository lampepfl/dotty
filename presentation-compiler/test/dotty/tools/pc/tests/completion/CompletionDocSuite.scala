package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test
import dotty.tools.pc.utils.MockEntries
import scala.meta.pc.SymbolDocumentation

class CompletionDocSuite extends BaseCompletionSuite:

  override protected def mockEntries: MockEntries = new MockEntries:
    override def documentations: Set[SymbolDocumentation] = Set(
      MockDocumentation("java/util/Collections#singletonList().", "singletonList", Seq(), Seq("o")),
      MockDocumentation("java/util/Map#Entry#setValue().", "setValue", Seq(), Seq("value")),
      MockDocumentation("java/util/Map#Entry#setValue().", "setValue", Seq(), Seq("value")),
      MockDocumentation("java/lang/String#join().", "join", Seq(), Seq("delimiter", "elements")),
      MockDocumentation("java/lang/String#substring().", "substring", Seq(), Seq("beginIndex")),
      MockDocumentation("java/lang/String#substring(+1).", "substring", Seq(), Seq("beginIndex", "endIndex")),
      ScalaMockDocumentation("scala/collection/Iterator#sliding().", "sliding", List(), List(MockParam("size"), MockParam("step", "1"))),
      ScalaMockDocumentation("scala/collection/immutable/TreeMap#insert().", "insert", List(), List(MockParam("key"), MockParam("value"))),
      ScalaMockDocumentation("scala/Option#isDefined().", "isDefined"),
      ScalaMockDocumentation("scala/util/DynamicVariable#", "DynamicVariable"),
      ScalaMockDocumentation("scala/util/DynamicVariable.", "DynamicVariable"),
      ScalaMockDocumentation("scala/io/Source#reportWarning().", "reportWarning", List(), List(MockParam("pos"), MockParam("msg"), MockParam("out", "Console.out"))),
      ScalaMockDocumentation("scala/Predef.println().", "println"),
      ScalaMockDocumentation("scala/Predef.println(+1).", "println", List(), List(MockParam("x"))),
      ScalaMockDocumentation("scala/Predef.", "Predef"),
      ScalaMockDocumentation("scala/runtime/stdLibPatches/Predef.", "Predef"),
      ScalaMockDocumentation("scala/util/control/Exception.Catch#", "Catch"),
      ScalaMockDocumentation("scala/util/control/Exception.Catch.", "Catch"),
      ScalaMockDocumentation("scala/package.Vector.", "Vector"),
      ScalaMockDocumentation("scala/package.Vector.", "Vector"),
      ScalaMockDocumentation("scala/collection/mutable/StringBuilder.", "StringBuilder"),
      ScalaMockDocumentation("scala/util/Try.", "Try"),
      ScalaMockDocumentation("scala/util/Try*", "Try"),
      ScalaMockDocumentation("scala/util/Try.apply().", "apply"),
      ScalaMockDocumentation("scala/concurrent/ExecutionContext.Implicits.global().", "global"),
      ScalaMockDocumentation("scala/collection/Iterator.", "Iterator"),
      ScalaMockDocumentation("scala/collection/Iterator#", "Iterator"),
    )

  @Test def `java` =
    check(
      """
        |object A {
        |  "".substrin@@
        |}
      """.stripMargin,
      """|substring(beginIndex: Int): String
         |substring(beginIndex: Int, endIndex: Int): String
         |""".stripMargin
    )

  @Test def `java2` =
    check(
      """
        |object A {
        |  String.join@@
        |}
      """.stripMargin,
      """|join(delimiter: CharSequence, elements: CharSequence*): String
         |""".stripMargin,
         topLines = Some(1)
    )

  @Test def `java3` =
    check(
      """
        |import scala.collection.JavaConverters._
        |object A {
        |  new java.util.HashMap[String, Int]().entrySet.asScala.foreach { entry =>
        |    entry.setV@@
        |  }
        |}
      """.stripMargin,
      """|setValue(value: Int): Int
         |""".stripMargin
    )

  @Test def `java4` =
    check(
      """
        |object A {
        |  java.util.Collections.singletonLis@@
        |}
      """.stripMargin,
      """|singletonList[T](o: T): java.util.List[T]
         |""".stripMargin
    )

  @Test def `scala` =
    check(
      """
        |object A {
        |  val source: io.Source = ???
        |  source.reportWarn@@
        |}
      """.stripMargin,
      """|reportWarning(pos: Int, msg: String, out: PrintStream = Console.out): Unit
         |""".stripMargin
    )

  @Test def `scala1` =
    check(
      """
        |object A {
        |  List(1).iterator.sliding@@
        |}
      """.stripMargin,
      """|sliding[B >: Int](size: Int, step: Int = 1): List[Int]#iterator.GroupedIterator[B]
         |""".stripMargin
    )

  @Test def `scala2` =
    check(
      """
        |object A {
        |  println@@
        |}
      """.stripMargin,
      """|> Found documentation for scala/Predef.println().
         |println(): Unit
         |> Found documentation for scala/Predef.println(+1).
         |println(x: Any): Unit
         |""".stripMargin,
      includeDocs = true
    )

  @Test def `scala3` =
    check(
      """
        |object A {
        |  Predef@@
        |}
      """.stripMargin,
      s"""
         |> Found documentation for scala/Predef.
         |Predef scala
         |> Found documentation for scala/runtime/stdLibPatches/Predef.
         |Predef - scala.runtime.stdLibPatches
         |""".stripMargin,
      includeDocs = true
    )

  @Test def `scala4` =
    check(
      """
        |object A {
        |  import scala.collection.Iterator@@
        |}
      """.stripMargin,
      """
        |> ### class Iterator
        |Found documentation for scala/collection/Iterator#
        |### object Iterator
        |Found documentation for scala/collection/Iterator.
        |Iterator scala.collection
        |""".stripMargin,

      includeDocs = true
    )

  @Test def `scala5` =
    check(
      """
        |object A {
        |  scala.concurrent.ExecutionContext.Implicits.global@@
        |}
      """.stripMargin,
      s"""|> Found documentation for scala/concurrent/ExecutionContext.Implicits.global().
          |global: ExecutionContext
          |""".stripMargin,
      includeDocs = true
    )


  @Test def `scala6` =
    check(
      """
        |object A {
        |  scala.util.Try@@
        |}
      """.stripMargin,
      """
        |> Found documentation for scala/util/Try.
        |Try scala.util
        |> Found documentation for scala/util/Try.apply().
        |Try[T](r: => T): Try[T]
        |new Try[T]: Try[T]
        |""".stripMargin,
      includeDocs = true
    )

  @Test def `scala7` =
    check(
      """
        |object A {
        |  scala.collection.mutable.StringBuilder@@
        |}
      """.stripMargin,
      """
        |> Found documentation for scala/collection/mutable/StringBuilder.
        |StringBuilder scala.collection.mutable
        |""".stripMargin,
      includeDocs = true,
      topLines = Some(1)
    )

  @Test def `scala8` =
    check(
      """
        |object A {
        |  scala.Vector@@
        |}
      """.stripMargin,
      """
        |> Found documentation for scala/package.Vector.
        |Vector scala.collection.immutable
        |Vector[A](elems: A*): Vector[A]
        |""".stripMargin,
      includeDocs = true
    )

  @Test def `scala9` =
    check(
      """
        |object A {
        |  new Catch@@
        |}
      """.stripMargin,
      """
        |> Found documentation for scala/util/control/Exception.Catch#
        |Catch[T](pf: Catcher[T], fin: Option[Finally] = ..., rethrow: Throwable => Boolean = ...): Catch[T] - scala.util.control.Exception
        |> ### class Catch
        |Found documentation for scala/util/control/Exception.Catch#
        |### object Catch
        |Found documentation for scala/util/control/Exception.Catch.
        |Catch - scala.util.control.Exception
        |""".stripMargin,
      includeDocs = true
    )

  @Test def `scala10` =
    check(
      """
        |object A {
        |  scala.util.Failure@@
        |}
      """.stripMargin,
      """|Failure scala.util
         |Failure[T](exception: Throwable): Failure[T]
         |""".stripMargin,
      includeDocs = true
    )

  // New completions not yet implemented for Scala 3
  @Test def `scala11` =
    check(
      """
        |object A {
        |  new scala.util.DynamicVariable@@
        |}
      """.stripMargin,
      """
        |> Found documentation for scala/util/DynamicVariable#
        |DynamicVariable[T](init: T): DynamicVariable[T]
        |""".stripMargin,
      includeDocs = true
    )

  @Test def `scala12` =
    check(
      """
        |object A {
        |  Option(1).isDefined@@
        |}
      """.stripMargin,
      """
        |> Found documentation for scala/Option#isDefined().
        |isDefined: Boolean
        |""".stripMargin,
      includeDocs = true
    )

  @Test def `scala13` =
    check(
      """
        |object A {
        |  scala.collection.immutable.TreeMap.empty[Int, Int].insert@@
        |}
      """.stripMargin,
      // tests both @define and HTML expansion
      """|> Found documentation for scala/collection/immutable/TreeMap#insert().
         |insert[V1 >: Int](key: Int, value: V1): TreeMap[Int, V1]
         |""".stripMargin,
      includeDocs = true
    )

  @Test def `local` =
    check(
      """
        |object A {
        |  locally {
        |    val myNumbers = Vector(1)
        |    myNumbers@@
        |  }
        |}
      """.stripMargin,
      """|myNumbers: Vector[Int]
         |myNumbers(i: Int): Int
         |""".stripMargin
    )
