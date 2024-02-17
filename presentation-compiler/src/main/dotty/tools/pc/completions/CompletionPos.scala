package dotty.tools.pc
package completions

import java.net.URI

import scala.meta.pc.OffsetParams

import dotty.tools.dotc.ast.untpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.util.Chars
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans
import dotty.tools.dotc.interactive.Completion
import dotty.tools.pc.utils.MtagsEnrichments.*

import org.eclipse.lsp4j as l

case class CompletionPos(
    start: Int,
    end: Int,
    query: String,
    cursorPos: SourcePosition,
    sourceUri: URI
):

  def sourcePos: SourcePosition = cursorPos.withSpan(Spans.Span(start, end))
  def stripSuffixEditRange: l.Range = new l.Range(cursorPos.offsetToPos(start), cursorPos.offsetToPos(end))
  def toEditRange: l.Range = cursorPos.withStart(start).withEnd(cursorPos.point).toLsp

end CompletionPos

object CompletionPos:

  def infer(
      cursorPos: SourcePosition,
      offsetParams: OffsetParams,
      adjustedPath: List[Tree]
  )(using Context): CompletionPos =
    infer(cursorPos, offsetParams.uri().nn, offsetParams.text().nn, adjustedPath)

  def infer(
      cursorPos: SourcePosition,
      uri: URI,
      text: String,
      adjustedPath: List[Tree]
  )(using Context): CompletionPos =
    val identEnd = inferIdentEnd(cursorPos, text)
    val query = Completion.completionPrefix(adjustedPath, cursorPos)
    val start = cursorPos.point - query.length()

    CompletionPos(start, identEnd, query.nn, cursorPos, uri)
  end infer

  /**
   * Infer the indentation by counting the number of spaces in the given line.
   *
   * @param lineOffset the offset position of the beginning of the line
   */
  private[completions] def inferIndent(
      lineOffset: Int,
      text: String
  ): (Int, Boolean) =
    var i = 0
    var tabIndented = false
    while lineOffset + i < text.length() && {
        val char = text.charAt(lineOffset + i)
        if char == '\t' then
          tabIndented = true
          true
        else char == ' '
      }
    do i += 1
    (i, tabIndented)
  end inferIndent

  /**
   * Returns the end offset of the identifier starting as the given offset position.
   */
  private def inferIdentEnd(pos: SourcePosition, text: String): Int =
    var i = pos.point
    while i < text.length() && Chars.isIdentifierPart(text.charAt(i)) do i += 1
    i

end CompletionPos
