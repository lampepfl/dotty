import scala.quoted._
import scala.tasty.inspector._

import dotty.tools.io.Directory

import java.io.File.pathSeparator
import java.io.File.separator

@main def Test: Unit =
  blacklistsOnlyContainsClassesThatExist()
  testTastyInspector()

/** Test that we can load trees from TASTy */
def testTastyInspector(): Unit =
  loadWithTastyInspector(loadBlacklisted)

def blacklistsOnlyContainsClassesThatExist() =
  val scalaLibTastyPathsSet = scalaLibTastyPaths.toSet
  assert(loadBlacklisted.diff(scalaLibTastyPathsSet).isEmpty,
    loadBlacklisted.diff(scalaLibTastyPathsSet).mkString(
      "`loadBlacklisted` contains names that are not in `scalaLibTastyPaths`: \n  ", "\n  ", "\n\n"))

def dottyVersion =
  System.getProperty("java.class.path").nn.split(pathSeparator).collectFirst {
    case path if path.endsWith(".jar") && path.contains("scala3-library_3-") =>
      path.split("scala3-library_3-").last.stripSuffix(".jar")
  }.get

def scalaLibClassesPath =
  java.nio.file.Paths.get(
    s"out/bootstrap/scala2-library-bootstrapped/scala-$dottyVersion-nonbootstrapped/classes".replace("/", separator))

lazy val scalaLibTastyPaths =
  new Directory(scalaLibClassesPath).deepFiles
    .filter(_.ext.isTasty)
    .map(_.normalize.path.stripPrefix(scalaLibClassesPath.toString + separator))
    .toList

def loadWithTastyInspector(blacklisted: Set[String]): Unit =
  val inspector = new scala.tasty.inspector.Inspector {
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      for tasty <- tastys do
        tasty.ast.show(using quotes.reflect.Printer.TreeStructure) // Check that we can traverse the full tree
      ()
  }
  val tastyFiles = scalaLibTastyPaths.filterNot(blacklisted)
  val isSuccess = TastyInspector.inspectTastyFiles(tastyFiles.map(x => scalaLibClassesPath.resolve(x).toString))(inspector)
  assert(isSuccess, "Errors reported while loading from TASTy")

/** Set of tasty files that cannot be loaded from TASTy */
def loadBlacklisted = Set[String](
  // No issues :)
)
