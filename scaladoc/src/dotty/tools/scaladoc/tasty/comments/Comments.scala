package dotty.tools.scaladoc
package tasty.comments

import scala.collection.immutable.SortedMap
import scala.util.Try

import com.vladsch.flexmark.util.{ast => mdu, sequence}
import com.vladsch.flexmark.{ast => mda}
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.util.sequence.BasedSequence

import scala.quoted._
import dotty.tools.scaladoc.tasty.comments.markdown.ExtendedFencedCodeBlock
import dotty.tools.scaladoc.tasty.comments.wiki.Paragraph
import dotty.tools.scaladoc.DocPart
import dotty.tools.scaladoc.tasty.{ SymOpsWithLinkCache, SymOps }
import scala.jdk.CollectionConverters._
import dotty.tools.scaladoc.snippets._

class Repr(val qctx: Quotes)(val sym: qctx.reflect.Symbol)

case class Comment (
  body:                    DocPart,
  short:                   Option[DocPart],
  authors:                 List[DocPart],
  see:                     List[DocPart],
  result:                  Option[DocPart],
  throws:                  List[DocPart],
  valueParams:             SortedMap[String, DocPart],
  typeParams:              SortedMap[String, DocPart],
  version:                 Option[DocPart],
  since:                   Option[DocPart],
  todo:                    List[DocPart],
  deprecated:              Option[DocPart],
  note:                    List[DocPart],
  example:                 List[DocPart],
  constructor:             Option[DocPart],
  group:                   Option[String],
  // see comment in PreparsedComment below regarding these
  groupDesc:               SortedMap[String, DocPart],
  groupNames:              SortedMap[String, DocPart],
  groupPrio:               SortedMap[String, Int],
  /** List of conversions to hide - containing e.g: `scala.Predef.FloatArrayOps` */
  hideImplicitConversions: List[DocPart]
)

case class PreparsedComment(
  body:                    String,
  authors:                 List[String],
  see:                     List[String],
  result:                  List[String],
  throws:                  SortedMap[String, String],
  valueParams:             SortedMap[String, String],
  typeParams:              SortedMap[String, String],
  version:                 List[String],
  since:                   List[String],
  todo:                    List[String],
  deprecated:              List[String],
  note:                    List[String],
  example:                 List[String],
  constructor:             List[String],
  group:                   List[String],
  // NOTE these don't need to be sorted in principle, but code is nicer if they are
  groupDesc:               SortedMap[String, String],
  groupNames:              SortedMap[String, String],
  groupPrio:               SortedMap[String, Int],
  hideImplicitConversions: List[String],
  shortDescription:        List[String],
  syntax:                  List[String],
  strippedLinesBeforeNo:   Int,
)

case class DokkaCommentBody(summary: Option[DocPart], body: DocPart)

abstract class MarkupConversion[T](val repr: Repr)(using dctx: DocContext) {
  protected def stringToMarkup(str: String): T
  protected def markupToDokka(t: T): DocPart
  protected def markupToString(t: T): String
  protected def markupToDokkaCommentBody(t: T): DokkaCommentBody
  protected def filterEmpty(xs: List[String]): List[T]
  protected def filterEmpty(xs: SortedMap[String, String]): SortedMap[String, T]
  protected def processSnippets(t: T, preparsed: PreparsedComment): T

  lazy val snippetChecker = dctx.snippetChecker

  val qctx: repr.qctx.type = if repr == null then null else repr.qctx // TODO why we do need null?
  val owner: qctx.reflect.Symbol =
    if repr == null then null.asInstanceOf[qctx.reflect.Symbol] else repr.sym
  private given qctx.type = qctx

  lazy val srcPos = if owner == qctx.reflect.defn.RootClass then {
    val sourceFile = dctx.args.rootDocPath.map(p => dotty.tools.dotc.util.SourceFile(dotty.tools.io.AbstractFile.getFile(p), scala.io.Codec.UTF8))
    sourceFile.fold(dotty.tools.dotc.util.NoSourcePosition)(sf => dotty.tools.dotc.util.SourcePosition(sf, dotty.tools.dotc.util.Spans.NoSpan))
  } else owner.pos.get.asInstanceOf[dotty.tools.dotc.util.SrcPos]

  object SymOpsWithLinkCache extends SymOpsWithLinkCache
  export SymOpsWithLinkCache._
  import SymOps._

  def resolveLink(queryStr: String): DocLink =
    if SchemeUri.matches(queryStr) then DocLink.ToURL(queryStr)
    else QueryParser(queryStr).tryReadQuery() match
      case Left(err) =>
        report.warning(s"Unable to parse query `$queryStr`: ${err.getMessage}", srcPos)
        val msg = s"Unable to parse query: ${err.getMessage}"
        DocLink.UnresolvedDRI(queryStr, msg)
      case Right(query) =>
        MemberLookup.lookup(using qctx)(query, owner) match
          case Some((sym, targetText, inheritingParent)) =>
            var dri = inheritingParent match
              case Some(parent) => sym.driInContextOfInheritingParent(parent)
              case None => sym.dri
            DocLink.ToDRI(dri, targetText)
          case None =>
            val txt = s"No DRI found for query, the link seems to be wrong or the file may not exist."
            val msg = s"$txt: $queryStr"

            if (!summon[DocContext].args.noLinkWarnings) then

              report.warning(msg, srcPos)

            DocLink.UnresolvedDRI(queryStr, txt)

  private val SchemeUri = """[a-z]+:.*""".r

  private def single(annot: String, xs: List[String], filter: Boolean = true): Option[T] =
    (if (filter) filterEmpty(xs) else xs.map(stringToMarkup)) match {
      case x :: xs =>
        Some(x)
      case _ => None
    }

  def snippetCheckingFunc: qctx.reflect.Symbol => SnippetChecker.SnippetCheckingFunc =
    (s: qctx.reflect.Symbol) => {
      val path = s.source.map(_.path)
      val pathBasedArg = dctx.snippetCompilerArgs.get(path)
      val scDataCollector = SnippetCompilerDataCollector[qctx.type](qctx)
      val data = scDataCollector.getSnippetCompilerData(s, s)
      val sourceFile = scDataCollector.getSourceFile(s)
      (str: String, lineOffset: SnippetChecker.LineOffset, argOverride: Option[SCFlags]) => {
          val arg = argOverride.fold(pathBasedArg)(pathBasedArg.overrideFlag(_))
          val res = snippetChecker.checkSnippet(str, Some(data), arg, lineOffset, sourceFile)
          res.filter(r => !r.isSuccessful).foreach(_.reportMessages()(using compilerContext))
          res
      }
    }

  def linkedExceptions(exceptions: SortedMap[String, T]): List[DocPart] = {
    exceptions.map { (name, body) =>
      val link: DocLink = resolveLink(name)
      val merged = mergeLinkWithBody(link, body)
      markupToDokka(merged)
    }.toList
  }

  def mergeLinkWithBody(link: DocLink, body: T): T

  final def parse(preparsed: PreparsedComment): Comment =
    val markup = stringToMarkup(preparsed.body)
    val body = markupToDokkaCommentBody(processSnippets(markup, preparsed))
    Comment(
      body                    = body.body,
      short                   = body.summary,
      authors                 = filterEmpty(preparsed.authors).map(markupToDokka),
      see                     = filterEmpty(preparsed.see).map(markupToDokka),
      result                  = single("@result", preparsed.result).map(markupToDokka),
      throws                  = linkedExceptions(filterEmpty(preparsed.throws)),
      valueParams             = filterEmpty(preparsed.valueParams).view.mapValues(markupToDokka).to(SortedMap),
      typeParams              = filterEmpty(preparsed.typeParams).view.mapValues(markupToDokka).to(SortedMap),
      version                 = single("@version", preparsed.version).map(markupToDokka),
      since                   = single("@since", preparsed.since).map(markupToDokka),
      todo                    = filterEmpty(preparsed.todo).map(markupToDokka),
      deprecated              = single("@deprecated", preparsed.deprecated, filter = false).map(markupToDokka),
      note                    = filterEmpty(preparsed.note).map(markupToDokka),
      example                 = filterEmpty(preparsed.example).map(markupToDokka),
      constructor             = single("@constructor", preparsed.constructor).map(markupToDokka),
      group                   = single("@group", preparsed.group).map(markupToString),
      groupDesc               = filterEmpty(preparsed.groupDesc).view.mapValues(markupToDokka).to(SortedMap),
      groupNames              = filterEmpty(preparsed.groupNames).view.mapValues(markupToDokka).to(SortedMap),
      groupPrio               = preparsed.groupPrio,
      hideImplicitConversions = filterEmpty(preparsed.hideImplicitConversions).map(markupToDokka)
    )
}

class MarkdownCommentParser(repr: Repr)(using dctx: DocContext)
    extends MarkupConversion[mdu.Node](repr) {

  def stringToMarkup(str: String) =
    MarkdownParser.parseToMarkdown(str, markdown.DocFlexmarkParser(resolveLink))

  def markupToString(t: mdu.Node): String = t.getChars().toString()

  def markupToDokka(md: mdu.Node): DocPart = md

  def markupToDokkaCommentBody(md: mdu.Node) =
    val summary =
      md.getChildIterator.asScala.collectFirst { case p: mda.Paragraph => p }

    DokkaCommentBody(summary, md)

  def filterEmpty(xs: List[String]) = {
    xs.map(_.trim)
      .filterNot(_.isEmpty)
      .map(stringToMarkup)
  }

  def filterEmpty(xs: SortedMap[String,String]) =
    xs.view.mapValues(_.trim)
      .filterNot { case (_, v) => v.isEmpty }
      .mapValues(stringToMarkup).to(SortedMap)

  def processSnippets(root: mdu.Node, preparsed: PreparsedComment): mdu.Node =
    FlexmarkSnippetProcessor.processSnippets(root, Some(preparsed), snippetCheckingFunc(owner))

  def mergeLinkWithBody(link: DocLink, body: mdu.Node): mdu.Node = {
    import dotty.tools.scaladoc.tasty.comments.markdown._
    val str = link match
      case DocLink.ToURL(url) => url
      case DocLink.ToDRI(dri, name) => name
      case DocLink.UnresolvedDRI(query, msg) => query
    val sequence = BasedSequence.EmptyBasedSequence().append(str)
    val node = new DocLinkNode(link, "", sequence)
    body.prependChild(node)
    body
  }
}

class WikiCommentParser(repr: Repr)(using DocContext)
    extends MarkupConversion[wiki.Body](repr):

  def stringToMarkup(str: String) = wiki.Parser(str, resolverLink).document()

  def resolverLink(queryStr: String, bodyOpt: Option[wiki.Inline]): wiki.Inline =
    val link = resolveLink(queryStr)
    wiki.Link(link, bodyOpt)

  // Do we need those?
  private def flatten(b: wiki.Inline): String = b match
    case wiki.Text(t) => t
    case wiki.Italic(t) => flatten(t)
    case wiki.Bold(t) => flatten(t)
    case wiki.Underline(t) => flatten(t)
    case wiki.Superscript(t) => flatten(t)
    case wiki.Subscript(t) => flatten(t)
    case wiki.Link(_, t) => t.fold("")(flatten)
    case wiki.Monospace(t) => flatten(t)
    case wiki.RepresentationLink(t, _) => flatten(t)
    case wiki.Chain(elems) => elems.headOption.fold("")(flatten)
    case wiki.HtmlTag(t) => t
    case wiki.Summary(t) => flatten(t)

  private def flatten(b: wiki.Block): String = b match
    case wiki.Paragraph(text) => flatten(text)
    case wiki.Title(text, _) => flatten(text)
    case wiki.Code(text) => text
    case wiki.UnorderedList(elems) => elems.headOption.fold("")(flatten)
    case wiki.OrderedList(elems, _) => elems.headOption.fold("")(flatten)
    case wiki.DefinitionList(items) => items.headOption.fold("")(e => flatten(e._1))
    case wiki.HorizontalRule => ""
    case wiki.Table(header, columns, rows) => (header +: rows).flatMap(_.cells).flatMap(_.blocks).map(flatten).mkString

  def markupToString(str: wiki.Body) = str.blocks.headOption.fold("")(flatten)

  def markupToDokka(body: wiki.Body) = parseBlocks(body.blocks) // TODO

  def parseBlocks(blocks: Seq[wiki.WikiDocElement]) = blocks

  def markupToDokkaCommentBody(body: wiki.Body) =
     DokkaCommentBody(
      summary = body.summary.map(s => parseBlocks(s.blocks)),
      body = parseBlocks(body.blocks),
    )

  def filterEmpty(xs: List[String]) =
    xs.map(stringToMarkup)

  def filterEmpty(xs: SortedMap[String,String]) =
    xs.view.mapValues(stringToMarkup).to(SortedMap)
      .filterNot { case (_, v) => v.blocks.isEmpty }

  def processSnippets(root: wiki.Body, preparsed: PreparsedComment): wiki.Body =
    // Currently not supported
    root

  def mergeLinkWithBody(link: DocLink, body: wiki.Body): wiki.Body =
    val linkNode = wiki.Link(link, None)
    val newBody = body match {
      case wiki.Body(List(wiki.Paragraph(wiki.Chain(content)))) =>
        val descr = wiki.Text(" ") +: content
        wiki.Body(List(wiki.Paragraph(wiki.Chain(linkNode +: descr))))
      case _ => body
    }
    newBody
