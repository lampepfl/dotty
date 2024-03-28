package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionKeywordSuite extends BaseCompletionSuite:

  @Test def `super-template` =
    check(
      """
        |package foo
        |
        |trait A {
        |  final def superVisorStrategy = 1
        |}
        |
        |object B extends A {
        |  supe@@
        |}
        |""".stripMargin,
      """|superVisorStrategy: Int (commit: '')
         |super (commit: '')
         |""".stripMargin,
      includeCommitCharacter = true
    )

  @Test def `comment` =
    check(
      """
        |package foo
        |
        |trait A {
        |  final def superVisorStrategy = 1
        |}
        |
        |object B extends A {
        |  // tr@@
        |}
        |""".stripMargin,
      "",
      includeCommitCharacter = true
    )

  @Test def `scala-doc` =
    check(
      """
        |package foo
        |
        |trait A {
        |  final def superVisorStrategy = 1
        |}
        |
        |object B extends A {
        |  /** tr@@
        |  *
        |  **/
        |}
        |""".stripMargin,
     "",
      includeCommitCharacter = true
    )

  @Test def `super-def` =
    check(
      """
        |package foo
        |
        |trait A {
        |  final def superVisorStrategy = 1
        |}
        |
        |object B extends A {
        |  def someMethod = {
        |    supe@@
        |  }
        |}
        |""".stripMargin,
      """|superVisorStrategy: Int
         |super
         |""".stripMargin
    )

  @Test def `super-val` =
    check(
      """
        |package foo
        |
        |trait A {
        |  final def superVisorStrategy = 1
        |}
        |
        |object B extends A {
        |  val someVal = {
        |    supe@@
        |  }
        |}
        |""".stripMargin,
      """|superVisorStrategy: Int
         |super
         |""".stripMargin
    )

  @Test def `super-var` =
    check(
      """
        |package foo
        |
        |trait A {
        |  final def superVisorStrategy = 1
        |}
        |
        |object B extends A {
        |  var someVal = {
        |    supe@@
        |  }
        |}
        |""".stripMargin,
      """|superVisorStrategy: Int
         |super
         |""".stripMargin
    )

  @Test def `super-arg` =
    check(
      """
        |package foo
        |
        |trait A {
        |  final def someMethod(x: Int) = x
        |}
        |
        |object B extends A {
        |  val x = someMethod(supe@@)
        |}
        |""".stripMargin,
      """|super
         |""".stripMargin
    )

  @Test def `val-template` =
    check(
      """
        |package foo
        |
        |object A {
        |  val value = 42
        |  va@@
        |}
        |""".stripMargin,
      """|value: Int
         |val
         |var
         |varargs(): varargs
         |varargs - scala.annotation
         |""".stripMargin
    )

  @Test def `val-def` =
    check(
      """
        |package foo
        |
        |object A {
        |  def someMethod = {
        |    va@@
        |  }
        |}
        |""".stripMargin,
      """|val
         |var
         |varargs(): varargs
         |varargs - scala.annotation
         |""".stripMargin
    )

  @Test def `given-def` =
    check(
      """
        |package foo
        |
        |object A {
        |  def someMethod = {
        |    gi@@
        |  }
        |}
        |""".stripMargin,
      """given (commit: '')
        |""".stripMargin,
      includeCommitCharacter = true,
      topLines = Some(5)
    )

  @Test def `val-arg` =
    check(
      """
        |package foo
        |
        |object A {
        |  val value = 42
        |  def someMethod(x: Int) = x
        |  someMethod(va@@)
        |}
        |""".stripMargin,
      """|value: Int
         |varargs(): varargs
         |varargs - scala.annotation""".stripMargin
    )

  @Test def `val-trailing-space` =
    checkEditLine(
      """
        |package foo
        |
        |object A {
        |___
        |}
        |""".stripMargin,
      "  va@@",
      "  val ",
      filter = _ == "val"
    )

  @Test def `return-method` =
    check(
      """
        |package foo
        |
        |object A {
        |  def someMethod(x: Int) = ret@@
        |}
        |""".stripMargin,
      """|return
         |""".stripMargin,
      // methods add in 3.2.1
      filter = item => !item.contains("retains")
    )

  @Test def `return-val` =
    check(
      """
        |package foo
        |
        |object A {
        |  val someVal = ret@@
        |}
        |""".stripMargin,
      "",
      // methods add in 3.2.1
      filter = item => !item.contains("retains")
    )

  @Test def `return-template` =
    check(
      """
        |package foo
        |
        |object A {
        |  ret@@
        |}
        |""".stripMargin,
      "",
      // methods add in 3.2.1
      filter = item => !item.contains("retains")
    )

  @Test def `return-toplevel` =
    check(
      """
        |package foo
        |
        |ret@@
        |""".stripMargin,
      ""
      // methods add in 3.2.1
      // filter = item => !item.contains("retains"),
    )

  @Test def `import-top` =
    check(
      """
        |package foo
        |
        |impo@@
        |
        |class Foo {
        |}
        |""".stripMargin,
      """|import (commit: '')
         |""".stripMargin,
      includeCommitCharacter = true
    )

  @Test def `import-empty` =
    check(
      """
        |impo@@
        |
        |class Foo {
        |}
        |""".stripMargin,
      """|import (commit: '')
         |""".stripMargin,
      includeCommitCharacter = true,
      enablePackageWrap = false
    )

  @Test def `abstract-class` =
    check(
      """
        |package foo
        |
        |abstract cla@@
        |""".stripMargin,
      """|class
         |""".stripMargin
    )

  @Test def `type-toplevel` =
    check(
      """
        |package foo
        |
        |typ@@
      """.stripMargin,
      "type"
    )

  @Test def `type-template` =
    check(
      """
        |package foo
        |
        |class Foo {
        |  typ@@
        |}
      """.stripMargin,
      "type"
    )

  @Test def `type-block` =
    check(
      """
        |package foo
        |
        |class Foo {
        |  val x = {
        |    val y = {}
        |    typ@@
        |  }
        |}
      """.stripMargin,
      // NOTE(olafur) `type` is technically valid in blocks but they're not completed
      // to reduce noise (we do the same for class, object, trait).
      ""
    )

  @Test def `new-type` =
    check(
      """
        |package foo
        |
        |trait Foo {
        |  val x: Map[Int, new@@]
        |}
      """.stripMargin,
      ""
    )
  // TODO: Should provide empty completions
  // The issue is that the tree looks the same as for `case @@` (it doesn't see `new`)
  // Issue: https://github.com/scalameta/metals/issues/4367
  @Test def `new-pattern` =
    check(
      """
        |package foo
        |
        |trait Foo {
        |  List(1) match {
        |    case new@@
        |  }
        |}
      """.stripMargin,
      ""
    )

  @Test def `super-typeapply` =
    check(
      """
        |package foo
        |
        |class Foo {
        |  def supervisorStrategy: Int
        |  def callObject = supe@@[Int]
        |}
      """.stripMargin,
      """|supervisorStrategy: Int
         |super
         |""".stripMargin
    )

  @Test def `protected-def` =
    check(
      """
        |package foo
        |
        |class Foo {
        |  protected de@@
        |}
      """.stripMargin,
      """|def
         |""".stripMargin,
    )

  @Test def `protected-val` =
    check(
      """
        |package foo
        |
        |class Foo {
        |  protected va@@
        |}
      """.stripMargin,
      """|val
         |var
         |""".stripMargin
    )

  @Test def `topLevel` =
    check(
      "@@",
      """|def
         |val
         |lazy val
         |inline
         |var
         |given
         |extension
         |type
         |class
         |enum
         |case class
         |trait
         |object
         |package
         |import
         |final
         |private
         |protected
         |abstract class
         |sealed trait
         |sealed abstract class
         |sealed class
         |implicit
         |""".stripMargin
    )

  @Test def `using` =
    check(
      """|object A{
         |  def hello(u@@)
         |}""".stripMargin,
      """|using (commit: '')
         |""".stripMargin,
      includeCommitCharacter = true,
    )

  @Test def `not-using` =
    check(
      """|object A{
         |  def hello(a: String, u@@)
         |}""".stripMargin,
      "",
    )

  @Test def `extends-class` =
    check(
      """
        |package foo
        |
        |class Foo ext@@
      """.stripMargin,
      """|extends
         |""".stripMargin
    )

  @Test def `extends-with-class` =
    check(
      """
        |package foo
        |
        |class Foo extends Any wi@@
      """.stripMargin,
      """|with
         |""".stripMargin
    )

  @Test def `extends-class-nested` =
    check(
      """
        |package foo
        |
        |class Foo {
        |  class Boo ext@@
        |}
      """.stripMargin,
      """|extends
         |""".stripMargin
    )

  @Test def `extends-class-nested-in-object` =
    check(
      """
        |package foo
        |
        |object Foo {
        |  class Boo ext@@
        |}
      """.stripMargin,
      """|extends
         |""".stripMargin
    )

  @Test def `extends-class-nested-with-body` =
    check(
      """
        |package foo
        |
        |class Foo {
        |  class Boo ext@@ {
        |    def test: Int = ???
        |  }
        |}
      """.stripMargin,
      """|extends
         |""".stripMargin
    )

  @Test def `extends-obj` =
    check(
      """
        |package foo
        |
        |object Foo ext@@
      """.stripMargin,
      """|extends
         |""".stripMargin
    )

  @Test def `extends-trait` =
    check(
      """
        |package foo
        |
        |trait Foo ext@@ {}
      """.stripMargin,
      """|extends
         |""".stripMargin
    )

  @Test def `extends-with-constructor` =
    check(
      """
        |package foo
        |
        |class Foo(x: Int) ext@@
      """.stripMargin,
      """|extends
         |""".stripMargin
    )

  @Test def `extends-with-type-param` =
    check(
      """
        |package foo
        |
        |class Foo[A] ext@@
        """.stripMargin,
      """|extends
         |""".stripMargin
    )

  @Test def `no-extends` =
    check(
      """
        |package foo
        |
        |object Main {
        |  def main = {
        |    foo.ext@@
        |  }
        |}
      """.stripMargin,
      ""
    )

  @Test def `no-extends-paren` =
    check(
      """
        |package foo
        |
        |object Main {
        |  def main = {
        |    foo(i) ex@@
        |  }
        |}
      """.stripMargin,
      ""
    )

  @Test def `extends-limitation` =
    check(
      """
        |package foo
        |
        |// can't provide extends keyword completion if there's newline between class
        |// because the completion engine tokenize only the line
        |class Main
        |  exten@@
      """.stripMargin,
      """|extends
         |""".stripMargin
    )

  @Test def `extends-enum` =
    check(
      """
        |package foo
        |
        |enum Foo(x: Int) ext@@
          """.stripMargin,
      """|extends
         |""".stripMargin
    )

  @Test def `derives-object` =
    check(
      """
        |package foo
        |
        |object Foo der@@
        """.stripMargin,
      """|derives
         |""".stripMargin
    )

  @Test def `derives-with-constructor` =
    check(
      """
        |package foo
        |
        |class Foo(x: Int) der@@
        """.stripMargin,
      """|derives
         |""".stripMargin
    )

  @Test def `derives-comma-extends` =
    check(
      """
        |package foo
        |
        |trait Bar {}
        |trait Baz {}
        |
        |class Foo(x: Int) extends Bar, Baz der@@
          """.stripMargin,
      """|derives
         |""".stripMargin
    )

  @Test def `derives-extends` =
    check(
      """
        |package foo
        |
        |trait Bar {}
        |class Foo(x: Int) extends Bar der@@
            """.stripMargin,
      """|derives
         |""".stripMargin
    )

  @Test def `derives-extends-type-param` =
    check(
      """
        |package foo
        |
        |trait Bar[B] {}
        |class Foo(x: Int) extends Bar[Int] der@@
              """.stripMargin,
      """|derives
         |""".stripMargin
    )

  @Test def `derives-with-extends` =
    check(
      """
        |package foo
        |
        |trait Bar {}
        |trait Baz {}
        |
        |class Foo(x: Int) extends Bar with Baz der@@
                """.stripMargin,
      """|derives
         |""".stripMargin
    )

  @Test def `derives-with-constructor-extends` =
    check(
      """
        |package foo
        |
        |trait Bar {}
        |class Baz(b: Int) {}
        |
        |class Foo(x: Int) extends Bar with Baz(1) der@@
                  """.stripMargin,
      """|derives
         |""".stripMargin
    )

  @Test def `no-derives` =
    check(
      """
        |package foo
        |
        |object Main {
        |  def main = {
        |    foo.der@@
        |  }
        |}
        """.stripMargin,
      ""
    )
