package dotty.tools.scaladoc
package signatures

import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex
import dotty.tools.scaladoc.test.BuildInfo
import java.nio.file.Path;
import org.jsoup.Jsoup
import util.IO

private enum SignatureRes:
  case Expected(name: String, signature: String)
  case Unexpected(name: String)
import SignatureRes._

abstract class SignatureTest(
  testName: String,
  signatureKinds: Seq[String],
  sourceFiles: List[String] = Nil,
  ignoreMissingSignatures: Boolean = false,
  filterFunc: (Path) => Boolean = _ => true
) extends ScaladocTest(testName):

  def runTest = { org.junit.Assume.assumeTrue("Running on Windows", java.io.File.separatorChar == '/'); afterRendering {
    val sources = sourceFiles match
      case Nil => testName :: Nil
      case s => s

    val allSignaturesFromSources = sources
      .map { file => Source.fromFile(s"${BuildInfo.test_testcasesSourceRoot}/tests/$file.scala") }
      .flatMap(signaturesFromSources(_, signatureKinds))
      .toList

    val expectedFromSources: Map[String, List[String]] = allSignaturesFromSources
      .collect { case e: Expected => e }
      .groupMap(_.name)(_.signature)

    val unexpectedFromSources: Set[String] = allSignaturesFromSources.collect { case Unexpected(name) => name }.toSet

    val actualSignatures: Map[String, Seq[String]] =
      signaturesFromDocumentation().flatMap { signature =>
        findName(signature, signatureKinds).map(_ -> signature)
      }.groupMap(_._1)(_._2)

    val unexpected = unexpectedFromSources.flatMap(actualSignatures.get).flatten
    val expectedButNotFound = expectedFromSources.flatMap {
      case (k, v) => findMissingSignatures(v, actualSignatures.getOrElse(k, Nil))
    }

    val missingReport = Option.when(!ignoreMissingSignatures && !expectedButNotFound.isEmpty)
      (s"Not documented signatures:\n${expectedButNotFound.mkString("\n")}")
    val unexpectedReport = Option.when(!unexpected.isEmpty)
      (s"Unexpectedly documented signatures:\n${unexpected.mkString("\n")}")

    val reports = missingReport ++ unexpectedReport

    if !reports.isEmpty then
      val allSignaturesMessage =
        s"""
        |All documented signatures:
        |${actualSignatures.flatMap(_._2).mkString("\n")}
        |
        |All expected signatures from source:
        |${expectedFromSources.flatMap(_._2).mkString("\n")}
        """.stripMargin
      val errorMessage = (reports ++ Some(allSignaturesMessage)).mkString(start = "\n", sep = "\n\n", end = "\n")
      reportError(errorMessage)
    end if

  } :: Nil }

  // e.g. to remove '(0)' from object IAmACaseObject extends CaseImplementThis/*<-*/(0)/*->*/
  private val commentRegex = raw"\/\*<-\*\/[^\/]+\/\*->\*\/".r
  private val whitespaceRegex = raw"\s+".r
  private val expectedRegex = raw".*//expected: (.+)".r
  private val unexpectedRegex = raw"(.+)//unexpected".r
  private val identifierRegex = raw"^\s*(`.*`|(?:\w+)(?:_[^\[\(\s]+)|\w+|[^\[\(\s]+)".r

  private def findMissingSignatures(expected: Seq[String], actual: Seq[String]): Set[String] =
    expected.toSet &~ actual.toSet

  extension (s: String)
    private def startWithAnyOfThese(c: String*) = c.exists(s.startsWith)
    private def compactWhitespaces = whitespaceRegex.replaceAllIn(s, " ")

  private def findName(signature: String, kinds: Seq[String]): Option[String] =
    for
      kindMatch <- kinds.flatMap(k => s"\\b$k\\b".r.findFirstMatchIn(signature)).minByOption(_.start)
      kind <- Option(kindMatch.group(0)) // to filter out nulls
      afterKind <- Option(kindMatch.after(0)) // to filter out nulls
      name <- if kind.contains("extension") then Some(signature) // The name of an extension will always be the signature itself
                    else identifierRegex.findFirstMatchIn(afterKind).map(_.group(1))
    yield name

  private def signaturesFromSources(source: Source, kinds: Seq[String]): Seq[SignatureRes] =
    source.getLines.map(_.trim)
      .filterNot(_.isEmpty)
      .filterNot(l => l.startWithAnyOfThese("=",":","{","}", "//") && !l.startsWith("//expected:"))
      .toSeq
      .flatMap {
        case unexpectedRegex(signature) => findName(signature, kinds).map(Unexpected(_))
        case expectedRegex(signature) =>
          findName(signature, kinds).map(Expected(_, signature))
        case signature =>
          findName(signature, kinds).map(
            Expected(_, commentRegex.replaceAllIn(signature, "")
              .compactWhitespaces.reverse.dropWhile(List('{', ':').contains(_)).reverse)
          )
      }

  private def signaturesFromDocumentation()(using DocContext): Seq[String] =
    val output = summon[DocContext].args.output.toPath
    val signatures = List.newBuilder[String]

    def processFile(path: Path): Unit = if filterFunc(path) then
      val document = Jsoup.parse(IO.read(path))
      val documentable = document.select(".groupHeader").forEach { element =>
        signatures += element.text
      }
      val content = document.select(".documentableElement").forEach { elem =>
        val annotations = elem.select(".annotations").eachText.asScala.mkString("")
        val other = elem.select(".header .other-modifiers").eachText.asScala.mkString("")
        val kind = elem.select(".header .kind").eachText.asScala.mkString("")
        val name = elem.select(".header .documentableName").eachText.asScala.mkString("")
        val signature = elem.select(".header .signature").eachText.asScala.mkString("")
        val sigPrefix = elem.select(".header .signature").textNodes match
          case list if list.size > 0 && list.get(0).getWholeText().startsWith(" ") => " "
          case _ => ""
        val all = s"$annotations$other $sigPrefix$signature".trim()
        signatures += all
      }

    IO.foreachFileIn(output, processFile)
    signatures.result

object SignatureTest {
  val classlikeKinds = Seq("class",  "object", "trait", "enum") // TODO add docs for packages
  val members = Seq("type", "def", "val", "var", "given", "extension")
  val all = classlikeKinds ++ members
}
