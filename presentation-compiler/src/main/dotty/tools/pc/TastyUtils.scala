package dotty.tools.pc

import java.net.URI
import java.nio.file.{Files, Paths}

import scala.meta.internal.metals.HtmlBuilder

import dotty.tools.dotc.core.tasty.TastyHTMLPrinter
import dotty.tools.dotc.core.tasty.TastyPrinter

object TastyUtils:
  def getTasty(
      tastyURI: URI,
      isHttpEnabled: Boolean
  ): String =
    if isHttpEnabled then getStandaloneHtmlTasty(tastyURI)
    else normalTasty(tastyURI)

  def getStandaloneHtmlTasty(tastyURI: URI): String =
    htmlTasty(tastyURI, List(standaloneHtmlStyles))

  private def normalTasty(tastyURI: URI): String =
    val tastyBytes = Files.readAllBytes(Paths.get(tastyURI))
    new TastyPrinter(tastyBytes.nn, isBestEffortTasty = false, testPickler = false).showContents()

  private def htmlTasty(
      tastyURI: URI,
      headElems: List[String] = Nil,
      bodyAttributes: String = ""
  ): String =
    val title = tastyHtmlPageTitle(tastyURI)
    val tastyBytes = Files.readAllBytes(Paths.get(tastyURI))
    val tastyHtml = new TastyHTMLPrinter(tastyBytes.nn).showContents()
    HtmlBuilder()
      .page(title, htmlStyles :: headElems, bodyAttributes) { builder =>
        builder
          .element("pre", "class='container is-dark'")(_.raw(tastyHtml))
      }
      .render
  end htmlTasty

  private def tastyHtmlPageTitle(file: URI) =
    val filename = Paths.get(file).nn.getFileName().toString
    s"TASTy for $filename"

  private val standaloneHtmlStyles =
    """|<style>
       |  body {
       |    background-color: #212529;
       |    color: wheat;
       |    padding: 1em;
       |    margin: 0;
       |    font-size: 14px;
       |  }
       |</style>
       |""".stripMargin

  private val htmlStyles =
    """|<style>
       |  span.name {
       |    color: magenta;
       |  }
       |  span.tree {
       |    color: yellow;
       |  }
       |  span.length {
       |    color: cyan;
       |  }
       |</style>
       |""".stripMargin

end TastyUtils
