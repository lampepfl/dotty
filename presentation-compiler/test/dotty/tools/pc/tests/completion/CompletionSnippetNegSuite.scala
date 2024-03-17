package dotty.tools.pc.tests.completion

import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.PresentationCompilerConfig

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionSnippetNegSuite extends BaseCompletionSuite:

  override def config: PresentationCompilerConfig =
    PresentationCompilerConfigImpl(
      isCompletionSnippetsEnabled = false
    )

  @Test def `member` =
    checkSnippet(
      """|object Main {
         |  List.appl@@
         |}
         |""".stripMargin,
      """|apply
         |unapplySeq""".stripMargin
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
         |println
         |""".stripMargin
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
      "toString"
    )
