package dotty.tools.pc.utils

import scala.language.unsafeNulls

import dotty.tools.pc.completions.CompletionSource
import dotty.tools.dotc.util.DiffUtil
import dotty.tools.pc.utils.MtagsEnrichments.*

import org.hamcrest
import org.hamcrest.*

trait PcAssertions:

  def assertWithDiff(
      expected: String,
      actual: String,
      includeSources: Boolean,
      snippet: Option[String] = None,
      completionSources: List[CompletionSource] = Nil,
  ): Unit =
    val longestExpeceted =
      expected.linesIterator.map(_.length).maxOption.getOrElse(0)
    val longestActual =
      actual.linesIterator.map(_.length).maxOption.getOrElse(0)

    val actualMatcher =
      if longestActual >= 40 || longestExpeceted >= 40 then
        lineByLineDiffMatcher(expected, completionSources, includeSources)
      else sideBySideDiffMatcher(expected, completionSources, includeSources)

    assertThat(actual, actualMatcher, snippet)

  def assertNoDiff(
      expected: String,
      actual: String,
      snippet: Option[String] = None
  ): Unit =
    assertThat(actual, lineByLineDiffMatcher(expected), snippet)

  def assertNonEmpty(
      actual: Seq[?],
      message: String,
      snippet: Option[String] = None
  ): Unit =
    assertWithoutStacktrace(true, actual.nonEmpty, message, snippet)

  def assertEquals[T](expected: T, actual: T, message: String) =
    assertWithoutStacktrace(expected, actual, message, None)

  def fail(message: String, snippet: Option[String] = None): Nothing =
    val description = new StringDescription

    description.appendText(System.lineSeparator)
    description.appendText(message)
    description.appendText(System.lineSeparator)

    snippet.map(addSnippet(description))

    val error = new AssertionError(description.toString)
    error.setStackTrace(Array.empty)
    throw error

  private def unifyNewlines(str: String): String =
    str.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n").trim

  private def addSnippet(description: StringDescription)(snippet: String) =
    description.appendText(System.lineSeparator)
    description.appendText("Code snippet:")
    description.appendText(System.lineSeparator)
    description.appendText(System.lineSeparator)
    description.appendText(unifyNewlines(snippet))
    description.appendText(System.lineSeparator)
    description.appendText(System.lineSeparator)

  private def assertWithoutStacktrace[T](
      expected: T,
      obtained: T,
      message: String,
      snippet: Option[String] = None
  ): Unit =
    if (expected != obtained) then
      val description = new StringDescription

      description.appendText(System.lineSeparator)
      description.appendText(message)
      description.appendText(System.lineSeparator)

      snippet.map(addSnippet(description))

      val error = new AssertionError(description.toString)
      error.setStackTrace(Array.empty)
      throw error

  private def assertThat[T](
      actual: T,
      matcher: Matcher[T],
      snippet: Option[String] = None
  ): Unit =
    val _actual = actual.asInstanceOf[AnyRef]
    if (!matcher.matches(_actual)) then
      val description = new StringDescription

      snippet.map(addSnippet(description))

      description.appendText(System.lineSeparator)
      description.appendText(
        " (" + Console.GREEN + "+++ Expected" + Console.RESET + ", "
      )
      description.appendText(
        Console.RED + "--- Obtained" + Console.RESET + ", "
      )
      description.appendText("NO CHANGES" + ")")
      description.appendText(System.lineSeparator)

      matcher.describeMismatch(_actual, description)

      val error = new AssertionError(description.toString)
      error.setStackTrace(Array.empty)
      throw error


  private def lineByLineDiffMatcher(
    expected: String,
    completionSources: List[CompletionSource] = Nil,
    isCompletion: Boolean = false
  ): TypeSafeMatcher[String] =
    def getDetailedMessage(diff: String): String =
      val lines = diff.linesIterator.toList
      val sources = completionSources.padTo(lines.size, CompletionSource.Empty)
      var completionIndex = 0
      lines.map: line =>
        if line.startsWith(Console.BOLD + Console.RED) || line.startsWith("  ") then
          completionIndex += 1
          s"$line | [${sources(completionIndex - 1)}]"
        else
          line
      .mkString("\n")

    new TypeSafeMatcher[String]:

      override def describeMismatchSafely(
          item: String,
          mismatchDescription: org.hamcrest.Description
      ): Unit =
        val diff = DiffUtil.mkColoredHorizontalLineDiff(unifyNewlines(expected), unifyNewlines(item))
        val maybeEnhancedDiff = if isCompletion then getDetailedMessage(diff) else diff
        mismatchDescription.appendText(System.lineSeparator)
        mismatchDescription.appendText(maybeEnhancedDiff)
        mismatchDescription.appendText(System.lineSeparator)

      override def describeTo(description: org.hamcrest.Description): Unit = ()
      override def matchesSafely(item: String): Boolean =
        unifyNewlines(expected) == unifyNewlines(item)

  private def sideBySideDiffMatcher(
    expected: String,
    completionSources: List[CompletionSource] = Nil,
    isCompletion: Boolean = false
  ): TypeSafeMatcher[String] =
    def getDetailedMessage(diff: String): String =
      val lines = diff.linesIterator.toList
      val sources = completionSources.padTo(lines.size, CompletionSource.Empty)
      (lines zip sources).map((line, source) => s"$line | [$source]").mkString("\n")

    new TypeSafeMatcher[String]:

      override def describeMismatchSafely(
          item: String,
          mismatchDescription: org.hamcrest.Description
      ): Unit =
        val cleanedExpected = unifyNewlines(expected)
        val cleanedActual = unifyNewlines(item)

        val expectedLines = cleanedExpected.linesIterator.toSeq
        val actualLines = cleanedActual.linesIterator.toSeq
        val diff = DiffUtil.mkColoredLineDiff(expectedLines, actualLines)
        val maybeEnhancedDiff = if isCompletion then getDetailedMessage(diff) else diff

        mismatchDescription.appendText(maybeEnhancedDiff)
        mismatchDescription.appendText(System.lineSeparator)

      override def describeTo(description: org.hamcrest.Description): Unit = ()
      override def matchesSafely(item: String): Boolean =
        unifyNewlines(expected) == unifyNewlines(item)
