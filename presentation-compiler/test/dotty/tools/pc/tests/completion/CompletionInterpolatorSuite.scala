package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.runners.MethodSorters
import org.junit.{FixMethodOrder, Test}

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class CompletionInterpolatorSuite extends BaseCompletionSuite:

  @Test def `string` =
    checkEdit(
      """|object Main {
         |  val myName = ""
         |  def message = "Hello $myNam@@, you are welcome"
         |}
         |""".stripMargin,
      """|object Main {
         |  val myName = ""
         |  def message = s"Hello $myName$0, you are welcome"
         |}
         |""".stripMargin,
      filterText = "myName"
    )

  @Test def `string1` =
    checkEdit(
      """|object Main {
         |  val myName = ""
         |  def message = "$myNam@@"
         |}
         |""".stripMargin,
      """|object Main {
         |  val myName = ""
         |  def message = s"$myName$0"
         |}
         |""".stripMargin,
      filterText = "myName"
    )

  @Test def `string2` =
    checkEdit(
      """|object Main {
         |  val myName = ""
         |  def message = "$myNa@@me"
         |}
         |""".stripMargin,
      """|object Main {
         |  val myName = ""
         |  def message = s"${myName$0}me"
         |}
         |""".stripMargin,
      filterText = "myName"
    )

  @Test def `multiline` =
    checkEdit(
      """|object Main {
         |  val myName = ""
         |  def message = '''$myNa@@me'''
         |}
         |""".stripMargin.triplequoted,
      """|object Main {
         |  val myName = ""
         |  def message = s'''${myName$0}me'''
         |}
         |""".stripMargin.triplequoted,
      filterText = "myName"
    )

  @Test def `multiline1` =
    checkEdit(
      """|object Main {
         |  val myName = ""
         |  def message = '''
         |    |$myNa@@me
         |    |'''.stripMargin
         |}
         |""".stripMargin.triplequoted,
      """|object Main {
         |  val myName = ""
         |  def message = s'''
         |    |${myName$0}me
         |    |'''.stripMargin
         |}
         |""".stripMargin.triplequoted,
      filterText = "myName".triplequoted
    )

  @Test def `escape` =
    checkEdit(
      """|object Main {
         |  val myName = ""
         |  "$myNam@@ $"
         |}
         |""".stripMargin.triplequoted,
      """|object Main {
         |  val myName = ""
         |  s"$myName$0 $$"
         |}
         |""".stripMargin.triplequoted,
      filterText = "myName"
    )

  @Test def `not-escape-twice` =
    checkEdit(
      """|object Main {
         |  val myName = ""
         |  s"$myNam@@ $$"
         |}
         |""".stripMargin.triplequoted,
      """|object Main {
         |  val myName = ""
         |  s"$myName $$"
         |}
         |""".stripMargin.triplequoted,
      filterText = "myName"
    )

  @Test def `escape-ident` =
    checkEdit(
      """|object Main {
         |  val myName = ""
         |  "Say $myName is $myNam@@"
         |}
         |""".stripMargin.triplequoted,
      """|object Main {
         |  val myName = ""
         |  s"Say $$myName is $myName$0"
         |}
         |""".stripMargin.triplequoted
    )

  @Test def `interpolator` =
    check(
      """|object Main {
         |  val myName = ""
         |  def message = s"Hello $myNam@@, you are welcome"
         |}
         |""".stripMargin,
      """|myName: String
         |""".stripMargin
    )

  @Test def `interpolator-in-object` =
    checkEdit(
      """|object Outer {
         |  private def method = {
         |    object Test {
         |      val hello: String = "1"
         |      s"$hello.toStri@@  $$"
         |    }
         |  }
         |}
         |""".stripMargin,
      """|object Outer {
         |  private def method = {
         |    object Test {
         |      val hello: String = "1"
         |      s"${hello.toString()$0}  $$"
         |    }
         |  }
         |}
         |""".stripMargin
    )

  @Test def `negative` =
    check(
      """|object Main {
         |  "$1@@"
         |}
         |""".stripMargin,
      ""
    )

  @Test def `negative1` =
    check(
      """|object Main {
         |  "$ @@"
         |}
         |""".stripMargin,
      ""
    )

  @Test def `negative2` =
    check(
      """|object Main {
         |  "$-@@"
         |}
         |""".stripMargin,
      ""
    )

  @Test def `negative3` =
    check(
      """|object Main {
         |  "$-@@"
         |}
         |""".stripMargin,
      ""
    )

  @Test def `negative4` =
    check(
      """|object Main {
         |  "$hello-@@"
         |}
         |""".stripMargin,
      ""
    )

  @Test def `negative5` =
    check(
      """|object Main {
         |  "$-hello@@"
         |}
         |""".stripMargin,
      ""
    )

  @Test def `negative6` =
    check(
      """|object Main {
         |  "$he-llo@@"
         |}
         |""".stripMargin,
      ""
    )

  @Test def `positive` =
    check(
      """|object Main {
         |  val he11o = "hello"
         |  "$he11o@@"
         |}
         |""".stripMargin,
      "he11o: String"
    )

  @Test def `positive1` =
    checkEdit(
      """|object Main {
         |  val myName = "name"
         |  "$$myNam@@"
         |}
         |""".stripMargin,
      """|object Main {
         |  val myName = "name"
         |  s"$$$myName$0"
         |}
         |""".stripMargin
    )

  @Test def `snippet` =
    checkEdit(
      """|object Main {
         |  "$identity@@"
         |}
         |""".stripMargin,
      """|object Main {
         |  s"${identity($0)}"
         |}
         |""".stripMargin
    )

  @Test def `snippet2` =
    checkEdit(
      """|object Main {
         |  "$toStrin@@"
         |}
         |""".stripMargin,
      """|object Main {
         |  s"${toString()$0}"
         |}
         |""".stripMargin
    )

  @Test def `snippet3` =
    checkEdit(
      """|object Main {
         |  def empty: Boolean = true
         |  "$empty@@"
         |}
         |""".stripMargin,
      """|object Main {
         |  def empty: Boolean = true
         |  s"$empty$0"
         |}
         |""".stripMargin
    )

  @Test def `brace` =
    checkEdit(
      """|object Main {
         |  val myName = ""
         |  "${myNa@@"
         |}
         |""".stripMargin,
      """|object Main {
         |  val myName = ""
         |  s"${myName$0}"
         |}
         |""".stripMargin
    )

  @Test def `empty` =
    check(
      """|object Main {
         |  locally {
         |    val a = ""
         |    val b = 42
         |    "$@@"
         |  }
         |}
         |""".stripMargin,
      """|b: Int
         |a: String
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `empty-brace` =
    check(
      """|object Main {
         |  locally {
         |    val a = ""
         |    val b = 42
         |    "${@@"
         |  }
         |}
         |""".stripMargin,
      """|b: Int
         |a: String
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `member` =
    checkEdit(
      """|object Main {
         |  def member = 42
         |  s"Hello $Main.membe@@!"
         |}
         |""".stripMargin,
      """|object Main {
         |  def member = 42
         |  s"Hello ${Main.member$0}!"
         |}
         |""".stripMargin
    )

  @Test def `member-label` =
    check(
      """|object Main {
         |
         |  s"Hello $List.e@@ "
         |}
         |""".stripMargin,
      """|empty[A]: List[A]
         |equals(x$0: Any): Boolean
         |""".stripMargin,
      topLines = Some(6),
      includeDetail = false
    )

  @Test def `member1` =
    checkEdit(
      """|object Main {
         |  def method(arg: Int) = 42
         |  s"Hello $Main.meth@@!"
         |}
         |""".stripMargin,
      """|object Main {
         |  def method(arg: Int) = 42
         |  s"Hello ${Main.method($0)}!"
         |}
         |""".stripMargin
    )

  @Test def `member2` =
    checkEdit(
      """|object Main {
         |  s"Hello $Main.toStr@@!"
         |}
         |""".stripMargin,
      """|object Main {
         |  s"Hello ${Main.toString()$0}!"
         |}
         |""".stripMargin
    )

  @Test def `member3` =
    check(
      """|object Main {
         |  val a = ""
         |  val b = 42
         |  s"Hello $Main.@@!"
         |}
         |""".stripMargin,
      """|a: String
         |b: Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `member-backtick` =
    checkEdit(
      """|object Main {
         |  val `type` = ""
         |  s"Hello $Main.type@@!"
         |}
         |""".stripMargin,
      """|object Main {
         |  val `type` = ""
         |  s"Hello ${Main.`type`$0}!"
         |}
         |""".stripMargin
    )

  @Test def `member-multiple` =
    checkEdit(
      """|object Main {
         |  val abc = ""
         |  val dfg = ""
         |  s"Hello $abc.toStrin@@ from ${dfg.toString()}!"
         |}
         |""".stripMargin,
      """|object Main {
         |  val abc = ""
         |  val dfg = ""
         |  s"Hello ${abc.toString()$0} from ${dfg.toString()}!"
         |}
         |""".stripMargin
    )

  @Test def `member-multiple2` =
    checkEdit(
      """|object Main {
         |  val abc = ""
         |  val dfg = ""
         |  s"Hello $dfg $abc.toStrin@@ from ${dfg.toString()}!"
         |}
         |""".stripMargin,
      """|object Main {
         |  val abc = ""
         |  val dfg = ""
         |  s"Hello $dfg ${abc.toString()$0} from ${dfg.toString()}!"
         |}
         |""".stripMargin
    )

  @Test def `member-f` =
    check(
      """|object Main {
         |  val a = ""
         |  val b = 42
         |  f"Hello $Main.@@!"
         |}
         |""".stripMargin,
      """|a: String
         |b: Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `member-raw` =
    check(
      """|object Main {
         |  val a = ""
         |  val b = 42
         |  raw"Hello $Main.@@!"
         |}
         |""".stripMargin,
      """|a: String
         |b: Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `member-unknown` =
    check(
      """|object Main {
         |  val a = ""
         |  val b = 42
         |  implicit class XtensionStringContext(c: StringContext) {
         |    def unknown(args: Any*): String = ""
         |  }
         |  unknown"Hello $Main.@@!"
         |}
         |""".stripMargin,
      """|a: String
         |b: Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `member-multiline` =
    check(
      """|object Main {
         |  val member = ""
         |  s'''
         |    Hello $Main.memb@@!
         |  '''
         |}
         |""".stripMargin.triplequoted,
      """|member: String
         |""".stripMargin,
      filter = s => s.contains("member")
    )

  @Test def `closing-brace` =
    checkEditLine(
      """|object Main {
         |  val hello = ""
         |  ___
         |}
         |""".stripMargin,
      """"Hello ${hell@@}"""".stripMargin,
      """s"Hello ${hello$0}"""".stripMargin
    )

  @Test def `closing-brace-negative` =
    checkEditLine(
      """|object Main {
         |  val hello = ""
         |  ___
         |}
         |""".stripMargin,
      """"Hello ${hell@@o}"""".stripMargin,
      """s"Hello ${hello$0}o}"""".stripMargin
    )

  // See https://github.com/scalameta/metals/issues/608
  // Turns out this bug was accidentally fixed by limiting snippets to only
  // when creating new expressions, inside existing code we don't insert ($0) snippets.
  @Test def `existing-interpolator-snippet` =
    checkEditLine(
      """|object Main {
         |  val hello = ""
         |  def helloMethod(a: Int) = ""
         |  ___
         |}
         |""".stripMargin,
      """s"Hello $hello@@"""".stripMargin,
      """s"Hello ${helloMethod($0)}"""".stripMargin,
      filter = _.contains("a: Int")
    )

  @Test def `token-error` =
    checkEditLine(
      """|object Main {
         |  val hello = ""
         |  ___
         |}
         |""".stripMargin,
      """s"Hello $@@"""".stripMargin,
      """s"Hello $hello"""".stripMargin,
      filter = _.contains("hello")
    )

  @Test def `brace-token-error` =
    checkEditLine(
      """|object Main {
         |  val hello = ""
         |  ___
         |}
         |""".stripMargin,
      """s"Hello ${@@}"""".stripMargin,
      """s"Hello ${hello}"""".stripMargin,
      filter = _.contains("hello")
    )

  @Test def `backtick` =
    checkEdit(
      """|object Main {
         |  val `type` = 42
         |  "Hello $type@@"
         |}
         |""".stripMargin,
      """|object Main {
         |  val `type` = 42
         |  s"Hello ${`type`$0}"
         |}
         |""".stripMargin,
      filterText = "type"
    )

  @Test def `backtick2` =
    checkEdit(
      """|object Main {
         |  val `hello world` = 42
         |  "Hello $hello@@"
         |}
         |""".stripMargin,
      """|object Main {
         |  val `hello world` = 42
         |  s"Hello ${`hello world`$0}"
         |}
         |""".stripMargin,
      filterText = "hello world"
    )

  @Test def `auto-imports` =
    checkEdit(
      """|object Main {
         |  "this is an interesting $Paths@@"
         |}
         |""".stripMargin,
      """|import java.nio.file.Paths
         |object Main {
         |  s"this is an interesting $Paths$0"
         |}
         |""".stripMargin
    )

  @Test def `auto-imports-prefix` =
    checkEdit(
      """|
         |class Paths
         |object Main {
         |  "this is an interesting $Paths@@"
         |}
         |""".stripMargin,
      """|class Paths
         |object Main {
         |  s"this is an interesting ${java.nio.file.Paths}"
         |}
         |""".stripMargin,
      itemIndex = 0,
      assertSingleItem = false,
    )


  @Test def `auto-imports-prefix-with-interpolator` =
    checkEdit(
      """|
         |class Paths
         |object Main {
         |  s"this is an interesting $Paths@@"
         |}
         |""".stripMargin,
      """|class Paths
         |object Main {
         |  s"this is an interesting ${java.nio.file.Paths}"
         |}
         |""".stripMargin,
      itemIndex = 1,
      assertSingleItem = false
    )

  @Test def `extension` =
    checkEdit(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |def aaa = 123
         |def main = s" $aaa.inc@@"
         |""".stripMargin,
      """|package example
         |
         |import example.enrichments.incr
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |def aaa = 123
         |def main = s" ${aaa.incr$0}"
         |""".stripMargin,
      // simulate issues with VS Code
      filterText = "aaa.incr"
    )

  @Test def `extension2` =
    checkEdit(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def plus(other: Int): Int = num + other
         |
         |def aaa = 123
         |def main = s"  $aaa.pl@@"
         |""".stripMargin,
      """|package example
         |
         |import example.enrichments.plus
         |
         |object enrichments:
         |  extension (num: Int)
         |    def plus(other: Int): Int = num + other
         |
         |def aaa = 123
         |def main = s"  ${aaa.plus($0)}"
         |""".stripMargin,
      filterText = "aaa.plus"
    )


  @Test def `extension3` =
    checkEdit(
      """|trait Cursor
         |
         |extension (c: Cursor) def spelling: String = "hello"
         |object Main {
         |  val c = new Cursor {}
         |  val x = s"$c.spelli@@"
         |}
         |""".stripMargin,
      """|trait Cursor
         |
         |extension (c: Cursor) def spelling: String = "hello"
         |object Main {
         |  val c = new Cursor {}
         |  val x = s"${c.spelling$0}"
         |}""".stripMargin
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
         |val foo = "foo"
         |def main = s" $foo.i@@"
         |""".stripMargin,
      """|identity: String (extension)
         |""".stripMargin, // incr won't be available
      filter = _.contains("(extension)")
    )

  @Test def `apply-method` =
    checkEdit(
      """|object Main {
         |  val a = "$ListBuf@@""
         |}""".stripMargin,
      """|import scala.collection.mutable.ListBuffer
         |object Main {
         |  val a = s"${ListBuffer($0)}""
         |}""".stripMargin,
      assertSingleItem = false,
      itemIndex = 1
    )

  @Test def `dont-show-when-writing-before-dollar` =
    check(
      """|object M:
         |  val host = ""
         |  val path = ""
         |
         |  println(s"host@@$path")}
         |""".stripMargin,
      ""
    )

  @Test def `show-when-writing-between-dollars` =
    check(
      """|object M:
         |  val host = ""
         |  val path = ""
         |
         |  println(s"$host@@$path")}
         |""".stripMargin,
      "host: String"
    )

  @Test def `show-when-writing-between-dollars-2` =
    check(
      """|object M:
         |  val host = ""
         |  val path = ""
         |
         |  println(s"$ho@@$path")}
         |""".stripMargin,
      "host: String"
    )
