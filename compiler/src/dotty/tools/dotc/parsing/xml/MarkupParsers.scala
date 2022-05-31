package dotty.tools
package dotc
package parsing
package xml

import scala.language.unsafeNulls

import scala.collection.mutable
import scala.collection.BufferedIterator
import mutable.{ Buffer, ArrayBuffer, ListBuffer }
import scala.util.control.ControlThrowable
import util.Chars.SU
import Parsers._
import util.Spans._
import core._
import Constants._
import util.SourceFile
import Utility._


// XXX/Note: many/most of the functions in here are almost direct cut and pastes
// from another file - scala.xml.parsing.MarkupParser, it looks like.
// (It was like that when I got here.) They used to be commented "[Duplicate]" butx
// since approximately all of them were, I snipped it as noise.  As far as I can
// tell this wasn't for any particularly good reason, but slightly different
// compiler and library parser interfaces meant it would take some setup.
//
// I rewrote most of these, but not as yet the library versions: so if you are
// tempted to touch any of these, please be aware of that situation and try not
// to let it get any worse.  -- paulp

/** This trait ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
object MarkupParsers {

  import ast.untpd._

  case object MissingEndTagControl extends ControlThrowable {
    override def getMessage: String = "start tag was here: "
  }

  case object ConfusedAboutBracesControl extends ControlThrowable {
    override def getMessage: String = " I encountered a '}' where I didn't expect one, maybe this tag isn't closed <"
  }

  case object TruncatedXMLControl extends ControlThrowable {
    override def getMessage: String = "input ended while parsing XML"
  }

  class MarkupParser(parser: Parser, final val preserveWS: Boolean)(implicit src: SourceFile) extends MarkupParserCommon {

    import Tokens.{ LBRACE, RBRACE }

    type PositionType = Span
    type InputType    = CharArrayReader
    type ElementType  = Tree
    type AttributesType = mutable.Map[String, Tree]
    type NamespaceType = Any  // namespaces ignored

    def mkAttributes(name: String, other: NamespaceType): AttributesType = xAttributes

    def eof: Boolean = false

    def truncatedError(msg: String): Nothing = throw TruncatedXMLControl
    def xHandleError(that: Char, msg: String): Unit =
      if (ch == SU) throw TruncatedXMLControl
      else reportSyntaxError(msg)

    var input : CharArrayReader = _
    def lookahead(): BufferedIterator[Char] =
      (input.buf drop input.charOffset).iterator.buffered

    import parser.{ symbXMLBuilder => handle }

    def curOffset : Int = input.charOffset - 1
    var tmppos : Span = NoSpan
    def ch: Char = input.ch
    /** this method assign the next character to ch and advances in input */
    def nextch(): Unit = { input.nextChar() }

    protected def ch_returning_nextch: Char = {
      val result = ch; input.nextChar(); result
    }

    def mkProcInstr(position: Span, name: String, text: String): ElementType =
      parser.symbXMLBuilder.procInstr(position, name, text)

    var xEmbeddedBlock: Boolean = false

    private var debugLastStartElement = List.empty[(Int, String)]
    private def debugLastPos = debugLastStartElement.head._1
    private def debugLastElem = debugLastStartElement.head._2

    private def errorBraces() = {
      reportSyntaxError("in XML content, please use '}}' to express '}'")
      throw ConfusedAboutBracesControl
    }
    def errorNoEnd(tag: String): Nothing = {
      reportSyntaxError("expected closing tag of " + tag)
      throw MissingEndTagControl
    }

    /** checks whether next character starts a Scala block, if yes, skip it.
     * @return true if next character starts a scala block
     */
    def xCheckEmbeddedBlock: Boolean = {
      // attentions, side-effect, used in xText
      xEmbeddedBlock = (ch == '{') && { nextch(); (ch != '{') }
      xEmbeddedBlock
    }

    /** parse attribute and add it to listmap
     *  [41] Attributes   ::= { S Name Eq AttValue }
     *       AttValue     ::= `'` { _  } `'`
     *                      | `"` { _ } `"`
     *                      | `{` scalablock `}`
     */
    def xAttributes: mutable.LinkedHashMap[String, Tree] = {
      val aMap = mutable.LinkedHashMap[String, Tree]()

      while (isNameStart(ch)) {
        val start = curOffset
        val key = xName
        xEQ()
        val delim = ch
        val mid = curOffset
        val value: Tree = ch match {
          case '"' | '\'' =>
            val tmp = xAttributeValue(ch_returning_nextch)

            try handle.parseAttribute(Span(start, curOffset, mid), tmp)
            catch {
              case e: RuntimeException =>
                errorAndResult("error parsing attribute value", parser.errorTermTree(parser.in.offset))
            }

          case '{'  =>
            nextch()
            xEmbeddedExpr
          case SU =>
            throw TruncatedXMLControl
          case _ =>
            errorAndResult("' or \" delimited attribute value or '{' scala-expr '}' expected", Literal(Constant("<syntax-error>")))
        }
        // well-formedness constraint: unique attribute names
        if (aMap contains key)
          reportSyntaxError("attribute %s may only be defined once" format key)

        aMap(key) = value
        if (ch != '/' && ch != '>')
          xSpace()
      }
      aMap
    }

    /** '<! CharData ::= [CDATA[ ( {char} - {char}"]]>"{char} ) ']]>'
     *
     * see [15]
     */
    def xCharData: Tree = {
      val start = curOffset
      xToken("[CDATA[")
      val mid = curOffset
      xTakeUntil(handle.charData, () => Span(start, curOffset, mid), "]]>")
    }

    def xUnparsed: Tree = {
      val start = curOffset
      xTakeUntil(handle.unparsed, () => Span(start, curOffset, start), "</xml:unparsed>")
    }

    /** Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
     *
     * see [15]
     */
    def xComment: Tree = {
      val start = curOffset - 2   // Rewinding to include "<!"
      xToken("--")
      xTakeUntil(handle.comment, () => Span(start, curOffset, start), "-->")
    }

    def appendText(span: Span, ts: Buffer[Tree], txt: String): Unit = {
      def append(t: String) = ts append handle.text(span, t)

      if (preserveWS) append(txt)
      else {
        val sb = new StringBuilder()

        txt foreach { c =>
          if (!isSpace(c)) sb append c
          else if (sb.isEmpty || !isSpace(sb.last)) sb append ' '
        }

        val trimmed = sb.toString.trim
        if (!trimmed.isEmpty) append(trimmed)
      }
    }

    /** adds entity/character to ts as side-effect
     *  @precond ch == '&'
     */
    def content_AMP(ts: ArrayBuffer[Tree]): Unit = {
      nextch()
      val toAppend = ch match {
        case '#' => // CharacterRef
          nextch()
          val theChar = handle.text(tmppos, xCharRef)
          xToken(';')
          theChar
        case _ =>   // EntityRef
          val n = xName
          xToken(';')
          handle.entityRef(tmppos, n)
      }

      ts append toAppend
    }

    /**
     *  @precond ch == '{'
     *  @postcond: xEmbeddedBlock == false!
     */
    def content_BRACE(p: Span, ts: ArrayBuffer[Tree]): Unit =
      if (xCheckEmbeddedBlock) ts append xEmbeddedExpr
      else appendText(p, ts, xText)

    /** Returns true if it encounters an end tag (without consuming it),
     *  appends trees to ts as side-effect.
     *
     *  @param ts ...
     *  @return   ...
     */
    private def content_LT(ts: ArrayBuffer[Tree]): Boolean = {
      if (ch == '/')
        return true   // end tag

      val toAppend = ch match {
        case '!'    => nextch() ; if (ch =='[') xCharData else xComment // CDATA or Comment
        case '?'    => nextch() ; xProcInstr                            // PI
        case _      => element                                          // child node
      }

      ts append toAppend
      false
    }

    def content: Buffer[Tree] = {
      val ts = new ArrayBuffer[Tree]
      while (true) {
        if (xEmbeddedBlock)
          ts append xEmbeddedExpr
        else {
          tmppos = Span(curOffset)
          ch match {
            // end tag, cdata, comment, pi or child node
            case '<'  => nextch() ; if (content_LT(ts)) return ts
            // either the character '{' or an embedded scala block }
            case '{'  => content_BRACE(tmppos, ts)  // }
            // EntityRef or CharRef
            case '&'  => content_AMP(ts)
            case SU   => return ts
            // text content - here xEmbeddedBlock might be true
            case _    => appendText(tmppos, ts, xText)
          }
        }
      }
      unreachable
    }

    /** '<' element ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
     *                | xmlTag1 '/' '>'
     */
    def element: Tree = {
      val start = curOffset
      val (qname, attrMap) = xTag(())
      if (ch == '/') { // empty element
        xToken("/>")
        handle.element(Span(start, curOffset, start), qname, attrMap, true, new ListBuffer[Tree])
      }
      else { // handle content
        xToken('>')
        if (qname == "xml:unparsed")
          return xUnparsed

        debugLastStartElement = (start, qname) :: debugLastStartElement
        val ts = content
        xEndTag(qname)
        debugLastStartElement = debugLastStartElement.tail
        val span = Span(start, curOffset, start)
        qname match {
          case "xml:group" => handle.group(span, ts)
          case _ => handle.element(span, qname, attrMap, false, ts)
        }
      }
    }

    /** parse character data.
     *  precondition: xEmbeddedBlock == false (we are not in a scala block)
     */
    private def xText: String = {
      assert(!xEmbeddedBlock, "internal error: encountered embedded block")
      val buf = new StringBuilder
      def done = buf.toString

      while (ch != SU) {
        if (ch == '}') {
          if (charComingAfter(nextch()) == '}') nextch()
          else errorBraces()
        }

        buf append ch
        nextch()
        if (xCheckEmbeddedBlock || ch == '<' ||  ch == '&')
          return done
      }
      done
    }

    /** Some try/catch/finally logic used by xLiteral and xLiteralPattern.  */
    inline private def xLiteralCommon(f: () => Tree, ifTruncated: String => Unit): Tree = {
      assert(parser.in.token == Tokens.XMLSTART)
      val saved = parser.in.newTokenData
      saved.copyFrom(parser.in)
      var output: Tree = null.asInstanceOf[Tree]
      try output = f()
      catch {
        case c @ TruncatedXMLControl  =>
          ifTruncated(c.getMessage)
        case c @ (MissingEndTagControl | ConfusedAboutBracesControl) =>
          parser.syntaxError(c.getMessage + debugLastElem + ">", debugLastPos)
        case _: ArrayIndexOutOfBoundsException =>
          parser.syntaxError("missing end tag in XML literal for <%s>" format debugLastElem, debugLastPos)
      }
      finally parser.in.resume(saved)

      if (output == null)
        parser.errorTermTree(parser.in.offset)
      else
        output
    }

    /** Use a lookahead parser to run speculative body, and return the first char afterward. */
    private def charComingAfter(body: => Unit): Char = {
      try {
        input = input.lookaheadReader()
        body
        ch
      }
      finally input = parser.in
    }

    /** xLiteral = element { element }
     *  @return Scala representation of this xml literal
     */
    def xLiteral: Tree = xLiteralCommon(
      () => {
        input = parser.in
        handle.isPattern = false

        val ts = new ArrayBuffer[Tree]
        val start = curOffset
        tmppos = Span(curOffset)    // Iuli: added this line, as it seems content_LT uses tmppos when creating trees
        content_LT(ts)

        // parse more XML?
        if (charComingAfter(xSpaceOpt()) == '<') {
          while {
            xSpaceOpt()
            nextch()
            ts.append(element)
            charComingAfter(xSpaceOpt()) == '<'
          } do ()
          handle.makeXMLseq(Span(start, curOffset, start), ts)
        }
        else {
          assert(ts.length == 1, "Require one tree")
          ts(0)
        }
      },
      msg => parser.incompleteInputError(msg)
    )

    /** @see xmlPattern. resynchronizes after successful parse
     *  @return this xml pattern
     */
    def xLiteralPattern: Tree = xLiteralCommon(
      () => {
        input = parser.in
        saving[Boolean, Tree](handle.isPattern, handle.isPattern = _) {
          handle.isPattern = true
          val tree = xPattern
          xSpaceOpt()
          tree
        }
      },
      msg => parser.syntaxError(msg, curOffset)
    )

    def escapeToScala[A](op: => A, kind: String): A = {
      xEmbeddedBlock = false
      val res = saving(parser.in.currentRegion, parser.in.currentRegion = _) {
        val lbrace = parser.in.newTokenData
        lbrace.token = LBRACE
        lbrace.offset = parser.in.charOffset - 1
        lbrace.lastOffset = parser.in.lastOffset
        lbrace.lineOffset = parser.in.lineOffset
        parser.in.resume(lbrace)
        op
      }
      if (parser.in.token != RBRACE)
        reportSyntaxError(" expected end of Scala " + kind)

      res
    }

    def xEmbeddedExpr: Tree = escapeToScala(parser.block(), "block")

    /** xScalaPatterns  ::= patterns
     */
    def xScalaPatterns: List[Tree] = escapeToScala(parser.patterns(), "pattern")

    def reportSyntaxError(offset: Int, str: String): Unit = parser.syntaxError(str, offset)
    def reportSyntaxError(str: String): Unit = {
      reportSyntaxError(curOffset, "in XML literal: " + str)
      nextch()
    }

    /** '<' xPattern  ::= Name [S] { xmlPattern | '{' pattern3 '}' } ETag
     *                  | Name [S] '/' '>'
     */
    def xPattern: Tree = {
      var start = curOffset
      val qname = xName
      debugLastStartElement = (start, qname) :: debugLastStartElement
      xSpaceOpt()

      val ts = new ArrayBuffer[Tree]

      val isEmptyTag = ch == '/'
      if (isEmptyTag) nextch()
      xToken('>')

      if (!isEmptyTag) {
        // recurses until it hits a termination condition, then returns
        def doPattern: Boolean = {
          val start1 = curOffset
          if (xEmbeddedBlock) ts ++= xScalaPatterns
          else ch match {
            case '<'  => // tag
              nextch()
              if (ch != '/') ts append xPattern   // child
              else return false                   // terminate

            case '{'  => // embedded Scala patterns
              while (ch == '{') {
                nextch()
                ts ++= xScalaPatterns
              }
              assert(!xEmbeddedBlock, "problem with embedded block")

            case SU   =>
              throw TruncatedXMLControl

            case _    => // text
              appendText(Span(start1, curOffset, start1), ts, xText)
              // here xEmbeddedBlock might be true:
              // if (xEmbeddedBlock) throw new ApplicationError("after:" + text); // assert
          }
          true
        }

        while (doPattern) { }  // call until false
        xEndTag(qname)
        debugLastStartElement = debugLastStartElement.tail
      }

      handle.makeXMLpat(Span(start, curOffset, start), qname, ts)
    }
  } /* class MarkupParser */
}
