package dotty.tools.scaladoc
package site

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import org.junit.Test
import org.junit.Assert._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import java.nio.charset.Charset
import dotty.tools.scaladoc.test.BuildInfo

class SiteGeneratationTest extends BaseHtmlTest:

  def indexLinks(content: DocumentContext) =
    content.assertAttr("p a","href", "_docs/index.html")

  def checkFile(
    path: String)(
    title: String,
    header: String,
    parents: Seq[String] = Nil,
    checks: DocumentContext => Unit = _ => ())(using ProjectContext) =
      withHtmlFile(path){ content  =>
        content.assertTextsIn(".project-name", projectName, projectName) // There are two elements with project name: one for desktop, one for mobile
        content.assertTextsIn(".projectVersion", projectVersion)
        content.assertTextsIn("h1", header)
        content.assertTextsIn("title", title)
        content.assertTextsIn(".breadcrumbs a", (parents :+ title)*)
        checks(content)
      }

  def testDocPages()(using ProjectContext) =
    checkFile("docs/Adoc.html")(title = "Adoc", header = "Header in Adoc", parents = Seq(projectName))
    checkFile("docs/dir/index.html")(title = "A directory", header = "A directory", parents = Seq(projectName))
    checkFile("docs/dir/nested.html")(
      title = "Nested in a directory", header = "Nested in a directory", parents = Seq(projectName, "A directory"))

  def testDocIndexPage()(using ProjectContext) =
    checkFile("docs/index.html")(title = projectName, header = s"$projectName in header")

  def testApiPages(
    mainTitle: String = projectName,
    parents: Seq[String] = Seq.empty,
    hasToplevelIndexIndex: Boolean = false)(using ProjectContext) =
      checkFile((if hasToplevelIndexIndex then "api/" else "" )+ "index.html")(
        title = mainTitle,
        header = projectName,
        parents = parents
      )
      checkFile("tests/site.html")(
        title = "tests.site",
        header = "tests.site",
        parents = parents :+ mainTitle
      )
      checkFile("tests/site/SomeClass.html")(
        title = "SomeClass",
        header = "SomeClass",
        parents = parents ++ Seq(mainTitle, "tests.site")
      )

  @Test
  def basicTest() = withGeneratedSite(testDocPath.resolve("basic")){
    testDocPages()
    testDocIndexPage()
    testApiPages()

    withHtmlFile("docs/Adoc.html"){ content  =>
        content.assertAttr("p a","href", "../tests/site/SomeClass.html")
    }

    withHtmlFile("tests/site/SomeClass.html"){ content  =>
      content.assertAttr(".breadcrumbs a","href",
        "../../index.html", "../site.html", "SomeClass.html"
      )
    }
  }

  @Test
  def noGlobalIndexTest() = withGeneratedSite(testDocPath.resolve("noGlobalIndex")){
    testDocPages()
    testDocIndexPage()
    testApiPages(hasToplevelIndexIndex = false)
  }

  @Test
  def noIndexesTest() = withGeneratedSite(testDocPath.resolve("noIndexes")){
    testDocPages()
    testApiPages(hasToplevelIndexIndex = false)
  }

  @Test
  def noExistingDocs() = withGeneratedSite(testDocPath.resolve("noExisting")){
    testApiPages(mainTitle = projectName, parents = Nil, hasToplevelIndexIndex = false)
  }

  @Test
  def emptyPage() = withGeneratedSite(testDocPath.resolve("emptyPage")){
    withHtmlFile("docs/hello.html") { content =>
      // There should be no content as the page body is empty.
      content.assertTextsIn("#content", Nil*)
    }
  }

  @Test
  def noConfigEnd() = withGeneratedSite(testDocPath.resolve("noConfigEnd")){
    withHtmlFile("docs/hello.html") { content =>
      // There should be no content as the page body is empty.
      content.assertTextsIn("#content", Nil*)
    }
  }

  @Test
  def staticLinking() = withGeneratedSite(testDocPath.resolve("static-links")){

    withHtmlFile("docs/Adoc.html"){ content  =>
        content.assertAttr("p a","href",
        "dir/html.html",
        "dir/name...with..dots..html",
        "dir/name.with.md.and.html.html",
        "dir/nested.html",
        "dir/nested.svg"
        )
    }
  }

  @Test
  def sidebarFile() = withGeneratedSite(testDocPath.resolve("sidebar")) {
    withHtmlFile("docs/index.html")(
      _.assertTextsIn("#leftColumn #docs-nav > div:nth-child(1) > span > a > span", "A Directory Subsection"))
    withHtmlFile("docs/directory/index.html")(_.assertTextsIn("title", "A Directory Subsection"))
  }
